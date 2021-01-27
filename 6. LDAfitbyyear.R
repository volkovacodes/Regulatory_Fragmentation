require(data.table)
require(dplyr)
require(lubridate)
require(tm)
require(topicmodels)

prepare_prosp <- function(text){
  require(qdapDictionaries)
  require(stringr)
  text <- tolower(text)
  text <- gsub("\\d+", "", text)
  text <- unlist(str_extract_all(text, "\\w+"))
  
  text <- text[text %in% GradyAugmented]
  text <- text[str_length(text) > 2]
  text <- text[!text %in% c(stopwords("english"))]
  text <- paste0(text, collapse = " ")
  return(text)
}

lda.trained <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/LDA_Data/lda_random_sample_topics100.rds" %>%
  readRDS


outfolder <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/LDA_Data/"


for(yr in 1995:2000)
{
  print(yr)
  print(Sys.time())
  
  dtm.name <- yr %>%
    paste0(outfolder, "dtm_random_sample_topics100_",.,".rds")
  
  lda.name <- yr %>%
    paste0(outfolder, "lda_random_sample_topics100_",.,".rds")
  
  sample <- yr %>%
    paste0("/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/Master and Texts/",.,".rds") %>%
    readRDS
  
  
  alltexts <- lapply(sample$texts, prepare_prosp)
  
  #create corpus from vector
  docs <- alltexts %>% tolower %>% VectorSource %>% Corpus
  #remove stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  #remove whitespace
  docs <- tm_map(docs, stripWhitespace)
  #Stem document
  require(SnowballC)
  docs <- tm_map(docs, stemDocument)
  
  #Create document-term matrix
  dtm <- DocumentTermMatrix(docs)
  #convert rownames to filenames
  rownames(dtm) <- sample$document_number
  
  ui = unique(dtm$i)
  dtm = dtm[ui,]
  
  saveRDS(dtm, dtm.name)
  
  lda <- posterior(lda.trained, dtm)
  
  saveRDS(lda, lda.name)
}