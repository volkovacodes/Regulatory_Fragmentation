require(data.table)
require(dplyr)
require(lubridate)
require(tm)
require(topicmodels)
require(DBI)
require(RSQLite)
require(parallel)

prepare_prosp <- function(text, dict = ldadict){
  require(qdapDictionaries)
  require(stringr)
  require(SnowballC)
  text <- tolower(text)
  text <- gsub("\\d+", "", text)
  text <- unlist(str_extract_all(text, "\\w+"))
  
  text <- text[text %in% GradyAugmented]
  text <- text[str_length(text) > 2]
  text <- text[!text %in% c(stopwords("english"))]
  text <- wordStem(text)
  text <- text[text %in% dict]
  text <- paste0(text, collapse = " ")
  return(text)
}

lda.trained <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/LDA_Data/lda_random_sample_topics100.rds" %>%
  readRDS

ldadict <- lda.trained@terms

outfolder <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/LDA_Data/LDA10K/"

### years to fix: 2019
for(yr in 1994:2019)
{
  print(yr)
  print(Sys.time())
  
  dtm.name <- yr %>%
    paste0(outfolder, "dtm_random_sample_topics100_10K",.,".rds")

  if(file.exists(dtm.name)) next
  tenk <- NULL
  for(qtr in 1:4)
  {
    dbname <- paste0("/Users/evolkova/Dropbox/SEC Filings/10-K/Clean Forms/10K_",yr,qtr,".sqlite")
    con = dbConnect(SQLite(), dbname=dbname)
    
    line <- paste0('SELECT * FROM filings')
    x = dbGetQuery(con, line)
    x <- x %>% 
      data.table
    
    tenk <- tenk %>%
      rbind(x)
    
    dbDisconnect(con)
  }
  
  
  alltexts <- mclapply(tenk$FILING, prepare_prosp)
  
  #create corpus from vector
  docs <- alltexts %>% tolower %>% VectorSource %>% Corpus
  #remove stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  #remove whitespace
  docs <- tm_map(docs, stripWhitespace)
  #Stem document
  docs <- tm_map(docs, stemDocument)
  
  #Create document-term matrix
  dtm <- DocumentTermMatrix(docs)
  #convert rownames to filenames
  rownames(dtm) <- tenk$FILENAME
  
  ui = unique(dtm$i)
  dtm = dtm[ui,]
  
  saveRDS(dtm, dtm.name)
}