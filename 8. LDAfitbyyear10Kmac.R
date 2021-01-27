require(tm)
require(topicmodels)
require(dplyr)
require(parallel)

lda.trained <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/LDA_Data/lda_random_sample_topics100.rds" %>%
  readRDS

outfolder <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/LDA_Data/LDA10K/"

fitlda <- function(x) posterior(lda.trained, dtm[x,])$topic

for(yr in 1994:1999)
{
  dtm.name <- yr %>%
    paste0(outfolder, "dtm_random_sample_topics100_10K",.,".rds")
  
  lda.name <- yr %>%
    paste0(outfolder, "lda_random_sample_topics100_10K",.,".csv")
  
  dtm <- dtm.name %>%
    readRDS
  
  print(yr)
  print(Sys.time())
  
  Ndocs <- dim(dtm)[1]
  res <- mclapply(1:Ndocs,fitlda,mc.cores = 8)
  
  out <- NULL
  for(i in 1:length(res)) out <- rbind(out, data.frame(res[[i]], filename = rownames(res[[i]])))
  
  colnames(out)[1:100] <- paste0("Topic", 1:100)
  rownames(out) <- NULL
  fwrite(out, lda.name)
}



