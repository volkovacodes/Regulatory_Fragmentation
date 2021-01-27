memory.limit(size = 17500000000000)

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
  text <- text[text %in% lda.trained@terms]
  text <- paste0(text, collapse = " ")
  return(text)
}

require(parallel)

mclapply.hack <- function(...) {
  ## Create a cluster
  ## ... How many workers do you need?
  ## ... N.B. list(...)[[1]] returns the first 
  ##          argument passed to the function. In
  ##          this case it is the list to iterate over
  size.of.list <- length(list(...)[[1]])
  cl <- makeCluster( min(size.of.list, detectCores()) )
  
  ## Find out the names of the loaded packages 
  loaded.package.names <- c(
    ## Base packages
    sessionInfo()$basePkgs,
    ## Additional packages
    names( sessionInfo()$otherPkgs ))
  
  ## N.B. tryCatch() allows us to properly shut down the 
  ##      cluster if an error in our code halts execution
  ##      of the function. For details see: help(tryCatch)
  tryCatch( {
    
    ## Copy over all of the objects within scope to
    ## all clusters. 
    ## 
    ## The approach is as follows: Beginning with the 
    ## current environment, copy over all objects within
    ## the environment to all clusters, and then repeat
    ## the process with the parent environment. 
    ##
    this.env <- environment()
    while( identical( this.env, globalenv() ) == FALSE ) {
      clusterExport(cl,
                    ls(all.names=TRUE, env=this.env),
                    envir=this.env)
      this.env <- parent.env(environment())
    }
    ## repeat for the global environment
    clusterExport(cl,
                  ls(all.names=TRUE, env=globalenv()),
                  envir=globalenv())
    
    ## Load the libraries on all the clusters
    ## N.B. length(cl) returns the number of clusters
    parLapply( cl, 1:length(cl), function(xx){
      lapply(loaded.package.names, function(yy) {
        ## N.B. the character.only option of 
        ##      require() allows you to give the 
        ##      name of a package as a string. 
        require(yy , character.only=TRUE)})
    })
    
    ## Run the lapply in parallel 
    return( parLapply( cl, ...) )
  }, finally = {        
    ## Stop the cluster
    stopCluster(cl)
  })
}

ldafit <- function(i) 
{
  out <- posterior(lda.trained, dtm[i,])$topic
  return(out)
}

lda.trained <- "M:/evolkova/Dropbox/Projects/Govt Agenda/Data/LDA_Data/lda_random_sample_topics100.rds" %>%
  readRDS


outfolder <- "M:/evolkova/Dropbox/Projects/Govt Agenda/Data/LDA_Data/"


for(yr in 2000)
{
  print(yr)
  print(Sys.time())
  
  dtm.name <- yr %>%
    paste0(outfolder, "dtm_random_sample_topics100_",.,"_server.rds")
  
  lda.name <- yr %>%
    paste0(outfolder, "lda_random_sample_topics100_",.,".csv")
  
  sample <- yr %>%
    paste0("M:/evolkova/Dropbox/Projects/Govt Agenda/Data/Master and Texts/",.,".rds") %>%
    readRDS
  
  
  alltexts <- mclapply.hack(sample$texts, prepare_prosp)
  
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
  
  Ndocs <- dim(dtm)[1]
  
  print(yr)
  print(Sys.time())
  print(Ndocs)
  
  res <- mclapply.hack(1:Ndocs,ldafit)
  
  out <- NULL
  for(i in 1:length(res)) out <- rbind(out, data.frame(res[[i]], filename = rownames(res[[i]])))
  
  colnames(out)[1:100] <- paste0("Topic", 1:100)
  rownames(out) <- NULL
  fwrite(out, lda.name)
}
