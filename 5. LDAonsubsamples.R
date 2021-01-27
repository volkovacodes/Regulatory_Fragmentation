require(data.table)
require(dplyr)
require(lubridate)
require(tm)



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

get.dict <- function(texts, cutoff = 5)
{
  library(tidyverse)
  x <- data_frame(text = texts) %>% 
    mutate(text = tolower(text)) %>% 
    mutate(text = str_remove_all(text, '[[:punct:]]')) %>% 
    mutate(tokens = str_split(text, "\\s+")) %>%
    unnest() %>% 
    count(tokens) %>% 
    #filter(!tokens %in% stop_words) %>% 
    mutate(freq = n / sum(n)) %>% 
    arrange(desc(n))
    x <- x[x$n >= 50,]
    return(x$tokens)
}

exclude_rare <- function(text, dict){
  require(stringr)
  text <- text %>%
    strsplit(.," ") %>%
    unlist
  
  text <- text[text %in% dict]
  text <- paste0(text, collapse = " ")
  return(text)
}


run.lda <- function(sample, n.topics, outname, dtm.name)
{
  alltexts <- lapply(sample$texts, prepare_prosp)
  
  dict <- alltexts %>%
    get.dict
  
  alltexts <- lapply(alltexts, function(x) exclude_rare(x,dict))
  alltexts <- unlist(alltexts)
  
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
  require(topicmodels)
  #Set parameters for Gibbs sampling
  k <- n.topics
  burnin <- 100
  iter <- 600
  thin <- 200
  seed <-list(2003,5,63,100001,765)
  nstart <- 5
  best <- TRUE
  
  start <- Sys.time()
  print(start)
  ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, 
                                                   burnin = burnin, iter = iter, thin=thin))
  end <- Sys.time()
  print(end - start)
  
  saveRDS(ldaOut, outname)
}





outfolder <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/Sandbox/20200820/LDA.raw.output/"


yr <- 2010
subsample <- yr %>%
  paste0("/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/Master and Texts/",.,".rds") %>%
  readRDS
run.lda(subsample, 25, "lda_year2010_topics25.rds" %>% paste0(outfolder,.), "dtm_year2010_topics25.rds" %>% paste0(outfolder,.))
run.lda(subsample, 50, "lda_year2010_topics50.rds" %>% paste0(outfolder,.), "dtm_year2010_topics50.rds" %>% paste0(outfolder,.))



yr <- 2017
subsample <- yr %>%
  paste0("/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/Master and Texts/",.,".rds") %>%
  readRDS
run.lda(subsample, 25, "lda_year2017_topics25.rds" %>% paste0(outfolder,.), "dtm_year2017_topics25.rds" %>% paste0(outfolder,.))
run.lda(subsample, 50, "lda_year2017_topics50.rds" %>% paste0(outfolder,.), "dtm_year2017_topics50.rds" %>% paste0(outfolder,.))


subsample <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/Master and Texts/random_sample.rds" %>%
  readRDS
run.lda(subsample, 25, "lda_random_sample_topics25.rds" %>% paste0(outfolder,.), "dtm_random_sample_topics25.rds" %>% paste0(outfolder,.))
run.lda(subsample, 50, "lda_random_sample_topics50.rds" %>% paste0(outfolder,.), "dtm_random_sample_topics50.rds" %>% paste0(outfolder,.))
run.lda(subsample, 100, "lda_random_sample_topics100.rds" %>% paste0(outfolder,.), "dtm_random_sample_topics100.rds" %>% paste0(outfolder,.))







