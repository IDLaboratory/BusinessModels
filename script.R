library(dplyr)
library(tidyr)
library(rvest)
library(readxl)
library(stringr)
library(readr)
library(R.utils)
library(stringi)
library(quanteda)
library(edgarWebR)

###################### DATA ############################

# Vocabulary of the keywords
vocabulary <- read.csv("vocabulary.csv")

# Creating a dictionary
words <- list()
for (i in 1:nrow(vocabulary)) {
  words[[i]] <- vocabulary$words[i]
}
names(words) <- vocabulary$words
myDict <- dictionary(words)

# Sample
sample <- read.csv("sample.csv")


#################### PART I 10-k #########################

year <- 2019

data <- data.frame()

for (i in 1:nrow(sample)) {
  
  CIK = sample$CIK[i]
  
  #link to company reports at EDGAR
  href <- paste0("https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany",
                 "&CIK=", URLencode(as.character(CIK), reserved = TRUE),
                 "&owner=", ifelse(FALSE, "include", "exclude"),
                 "&type=", URLencode("10-K", reserved = TRUE),
                 "&dateb=", paste0(year+1, "1231"),
                 "&start=", (1 - 1) * 40,
                 "&count=", 40,
                 "&output=atom")
  # parse link
  res <- httr::GET(href, httr::user_agent("NRU HSE IDLab nnvotinseva@edu.hse.ru"))
  
  try_result <- try(xml2::read_xml(res, base_url = href, options = "HUGE"), silent = TRUE)
  
  doc <- xml2::read_xml(res, base_url = href, options = "HUGE")
  # info about company at EDGAR
  info <- company_information(doc)
  # company filings at EDGAR
  filings <- company_filings(doc)
  filings$filing_date <- as.Date(filings$filing_date)
  # filter filings by input year
  filings_need <- filings %>%
    filter(filing_date > as.Date(paste0(as.numeric(year), "0501"), format = "%Y%m%d")) %>%
    filter(filing_date < as.Date(paste0(as.numeric(year)+1, "0701"), format = "%Y%m%d"))
  
  # all company details
  company.details <- list("information" = info,
                          "filings" = filings_need)
  # functions to retrieve filing
  filing_documents_mine <- function(x) {
    res_raw <- httr::GET(x, httr::user_agent("NRU HSE IDLab nnvotinseva@edu.hse.ru"))
    res_xml <- xml2::read_html(res_raw, base_url = x, options = "HUGE")
    
    entries_xpath <- paste0(
      "//table[@summary='Document Format Files']/tr[not(descendant::th)]|",
      "//table[@summary='Data Files']/tr[not(descendant::th)]")
    
    info_pieces <- list(
      "seq" = "td[1]",
      "description" = "td[2]",
      "document" = "td[3]/a/text()",
      "href" = "td[3]/a/@href",
      "type" = "td[4]",
      "size" = "td[5]"
    )
    
    map_xml <- function(doc,
                        entries_xpath,
                        parts,
                        trim = c(),
                        integers = c(),
                        date_format = "") {
      xml2::xml_ns_strip(doc)
      entries <- xml2::xml_find_all(doc, entries_xpath)
      
      res <- sapply(parts, function(path) {
        sapply(entries, function(entry) {
          node <- xml2::xml_find_first(entry, path)
          if (typeof(node) == "character") {
            return(node)
          } else {
            return(xml2::xml_text(node))
          }
        })
      })
      if (length(entries) == 1) {
        res <- t(res)
      }
      
      res <- data.frame(res, stringsAsFactors = FALSE)
      
      for (col in trim) {
        res[[col]] <- trimws(res[[col]])
      }
      
      for (col in integers) {
        # To suppress warnings that don't matter, first convert spaces to NA
        empties <- grepl("(*UCP)^\\s*$", res[[col]], perl = TRUE)
        res[empties, col] <- NA
        res[[col]] <- as.integer(res[[col]])
      }
      
      link_cols <- colnames(res)[grepl("href$", colnames(res))]
      for (ref in link_cols) {
        res[[ref]] <- ifelse(startsWith(res[[ref]], "javascript"),
                             regmatches(res[[ref]],
                                        regexpr("http[^('|\")]+", res[[ref]])),
                             res[[ref]])
        res[[ref]] <- ifelse(is.na(res[[ref]]), NA,
                             xml2::url_absolute(res[[ref]], xml2::xml_url(doc)))
        
        # We need to do this because the rss gives http url's - this saves the
        # redirect
        res[[ref]] <- gsub("http:", "https:", res[[ref]])
      }
      
      date_cols <- colnames(res)[grepl("date$", colnames(res))]
      for (ref in date_cols) {
        res[[ref]] <- if (date_format == "") {
          as.POSIXct(res[[ref]])
        } else {
          as.POSIXct(res[[ref]], format = date_format)
        }
      }
      
      return(res)
    }
    res <- map_xml(res_xml, entries_xpath, info_pieces, integers = c("seq", "size"))
    
    # Fix links for iXBRL documents that lead to interactive viewers.
    res$href <- sub("ix?doc=/", "", res$href, fixed = TRUE)
    return(res)
  }
  filing_doc <- function(href) {
    sapply(href, function(x) {
      filing_documents_mine(x) %>%
        filter(type == "10-K") %>% select(href) }) %>%
      unlist(recursive = TRUE, use.names = FALSE)
  }
  
  # get link to report
  href_to_doc <- filing_doc(company.details$filings$href)[1]
  company.reports <- company.details$filings %>%
    filter(type == "10-K") %>%
    slice(1:1) %>% 
    dplyr::mutate(doc.href = href_to_doc,
                  mdlink = paste0("[Filing Link](", href, ")"),
                  reportLink = paste0("[10-K Link](", doc.href, ")")) %>%
    dplyr::select(filing_date, accession_number, mdlink, reportLink, href, doc.href)
  # link
  uri = company.reports$doc.href
  # get report
  resu <- httr::GET(uri, httr::user_agent("NRU HSE IDLab nnvotinseva@edu.hse.ru"))
  result <- rvest::read_html(resu$content)
  Text <- result %>% html_text()
  # turn to lower
  Text <- tolower(Text)
  Text <- gsub("\r?\n|\r", " ", Text)
  Text <- gsub(":", "", Text)
  Text <- gsub("\\.", "", Text) #remove dots
  Text <- gsub("\\—", "", Text) #remove —
  Text <- gsub("\u200b", "", Text)
  Text <- gsub("\u0093", "", Text)
  Text <- gsub("see item", "", Text)
  Text <- gsub("herein in “part ii item 5", "", Text)
  Text <- gsub("bellring brands", "", Text)
  Text <- str_split_1(Text, "securities\\s{0,}and\\s{0,}exchange\\s{0,}commission\\s{0,}washington")[2]
  # extract Part I
  if (nchar(str_split_1(Text, "item\\s{0,}5\\s{0,}market")[1]) < 20000)
  {
    text_Part_1 <- paste0(str_split_1(Text, "item\\s{0,}5\\s{0,}market")[1],
                          str_split_1(Text, "item\\s{0,}5\\s{0,}market")[2])
    
  } else {
    
    text_Part_1 <- str_split_1(Text, "item\\s{0,}5\\s{0,}market")[1]
    
  }
  
  
  ###################### CONTENT ANALYSIS ###################
  
  fulltext <- corpus(text_Part_1)
  
  # turn to tokens and remove common English words
  res_dfm <- tokens(fulltext, remove_punct = TRUE) %>%
    tokens_select(pattern = stopwords("en"), selection = "remove") %>%
    tokens_lookup(myDict, valuetype = "glob", exclusive = FALSE) %>% 
    dfm() %>%
    convert(to = "data.frame")
  
  result_dfm <- res_dfm[,-1] %>% gather("words", "frequency")
  result_dfm_1 <- left_join(vocabulary, result_dfm, by = "words") %>%
    as.data.frame()
  result_dfm_1[is.na(result_dfm_1)] <- 0
  Result_dfm <- result_dfm_1 %>% group_by(block, feature) %>%
    summarize(freq = sum(frequency), .groups = 'keep')  %>%
    as.data.frame()
  # add CIK number
  Result_dfm$CIK <- CIK

  data <- rbind(data, Result_dfm)
  
  print(i)
  
  Sys.sleep(5)
}

################### NORMALIZATION  ###################


#calculate normalized frequencies on the elements level
data %>%
  group_by(block, feature) %>%
  mutate(norm_freq = (freq-min(freq))/(max(freq)-min(freq))) %>%
  unique() %>%
  ungroup -> elements_level_normalization

elements_level_normalization[is.na(elements_level_normalization)] <- 0

#calculate blocks level
data %>%
  group_by(CIK, block) %>%
  mutate(block_freq = sum(freq)) %>%
  select(-feature, -freq) %>%
  unique() -> block_level

#calculate normalized frequencies on the blocks level
block_level %>%
  group_by(block) %>%
  mutate(norm_freq = (block_freq-min(block_freq))/(max(block_freq)-min(block_freq))) %>%
  ungroup -> blocks_level_normalization


################### CLUSTER ANALYSIS  ###################

n_clusters = 5

elements_level_normalization <- elements_level_normalization %>% left_join(sample)
elements_level_normalization %>%
  select(company, feature, norm_freq) %>%
  tidyr::pivot_wider(names_from = feature, values_from = norm_freq) -> new_data
size = nrow(new_data)

#convert to matrix
m <- as.matrix(new_data[c(1:size),-1])
#companies as row names
rownames(m) <- new_data$company[1:size]


# distance between document vectors
d <- dist(m)

# hierarchical clustering using Ward's method
groups <- hclust(d, method="ward.D")

# dendrogram
plot(groups, hang=-1)

#clusters
clustering <- cutree(groups, n_clusters)


#companies
companies_clusters <- clustering %>% as.data.frame()
colnames(companies_clusters)[1] <- "cluster"
companies_clusters$company <- rownames(companies_clusters)
rownames(companies_clusters) <- NULL

