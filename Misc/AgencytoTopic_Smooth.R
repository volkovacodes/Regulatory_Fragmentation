require(data.table)
require(lubridate)
require(dplyr)
require(tm)
require(topicmodels)
require(parallel)
require(stringr)

path <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/"
setwd(path)
############################################################################
###################### LOADING AGENCIES FILE  ##############################
############################################################################

agencies <-  "./Data/Agencies.csv" |> fread(select = c("document_number", "clean.parent_agency"))

agencies[clean.parent_agency == "", clean.parent_agency := "-"]
agencies <- agencies[clean.parent_agency != "" & clean.parent_agency != "-"]
agencies[, id := paste(document_number, clean.parent_agency)]
agencies <- agencies[!duplicated(id)]
agencies$id <- NULL
agencies[, all_agencies := paste0(clean.parent_agency, collapse = "|"), by = document_number]

############################################################################
######################## LOADING MASTER FILE  ##############################
############################################################################

master <- fread("/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/fedreg_rin.csv")

############################################################################
############################# LOADING TOPICS ###############################
############################################################################

topics.prob <- NULL
for(yr in 1995:2019) {
  tmp <- yr %>%
    paste0(path, "/Data/LDA_Data/LDAFedReg/lda_random_sample_topics100_",.,".csv") %>%
    fread
  
  tmp[, document_number := filename]
  tmp$filename <- NULL
  
  dtm <- yr %>%
    paste0(path, "/Data/LDA_Data/LDAFedReg/dtm_random_sample_topics100_",.,"_server.rds") %>%
    readRDS
  
  m <- match(tmp$document_number, rownames(dtm))
  nwords <- rowSums(as.matrix(dtm))
  
  tmp$nwords <- nwords
  topics.prob <- rbind(topics.prob, tmp)
}

rm(dtm)
colnames(topics.prob)[1:100] <- 1:100

topics.prob <- topics.prob[!duplicated(document_number)]

m <- match(topics.prob$document_number, master$document_number)

tmp <- master[m] %>% select("type", "type2", "publication_date")
topics.prob <- cbind(topics.prob[,1:102, with = F], tmp)
rm(tmp)
rm(master)

topics.prob[, date := publication_date]
topics.prob[, year := year(date)]
topics.prob[, quarter := paste0(year, "Q",quarter(date))]
topics.prob[, month := paste0(year, sprintf("%02d", month(date)))]
topics.prob[, publication_date := NULL]

m <- match(topics.prob$document_number, agencies$document_number)
topics.prob$all_agencies <- agencies$all_agencies[m]
rm(m)
rm(agencies)

vars <- paste0(colnames(topics.prob)[colnames(topics.prob) != "all_agencies"], collapse = ",")
topics.prob <- topics.prob[,list(agency = all_agencies %>% strsplit("\\|") %>% unlist), by = vars]
topics.prob[, Nagencies := .N, by = document_number]

#######################################################################################
########################## Long files starts here #####################################
#######################################################################################

### start here:
topicstoagencies.yr  <- NULL
for(yr in 1999:2019)
{
  print(yr)
  
  long.topics.prob <- melt(topics.prob[year %in% yr:(yr-4)], id.vars = c("document_number", "nwords", "type", "type2" ,"date", "month", 
                                                                "quarter","year", "agency", "Nagencies"))
  long.topics.prob[, wt := 1/(yr - year + 1)]
  long.topics.prob[, TopicWords := wt*value*nwords/Nagencies]
  colnames(long.topics.prob)[colnames(long.topics.prob) == "variable"] <- "TopicNumber"
  colnames(long.topics.prob)[colnames(long.topics.prob) == "value"] <- "TopicProb"
  
  long.topics.prob[, minusProb := -TopicProb]
  setkey(long.topics.prob, TopicNumber, minusProb)
  
  long.topics.prob <- long.topics.prob[!is.na(agency)]
  
  long.topics.prob[, AllWords := sum(TopicWords), by = "TopicNumber"]
  tmp <- long.topics.prob[, list(TopicWords = sum(TopicWords),
                                 AllWords = AllWords[1]),by = "TopicNumber,agency"]
  
  tmp[, AgencyPercent := TopicWords/AllWords]
  tmp$year <- yr
  topicstoagencies.yr <- topicstoagencies.yr %>% rbind(tmp)
}

fwrite(topicstoagencies.yr, "/Users/evolkova/Dropbox/Projects/Govt Agenda/Sandbox/20230401/topictoagency5.csv")


topicstoagencies.yr  <- NULL
for(yr in 2004:2019)
{
  print(yr)
  
  long.topics.prob <- melt(topics.prob[year %in% yr:(yr-9)], id.vars = c("document_number", "nwords", "type", "type2" ,"date", "month", 
                                                                         "quarter","year", "agency", "Nagencies"))
  long.topics.prob[, wt := 1/(yr - year + 1)]
  long.topics.prob[, TopicWords := wt*value*nwords/Nagencies]
  colnames(long.topics.prob)[colnames(long.topics.prob) == "variable"] <- "TopicNumber"
  colnames(long.topics.prob)[colnames(long.topics.prob) == "value"] <- "TopicProb"
  
  long.topics.prob[, minusProb := -TopicProb]
  setkey(long.topics.prob, TopicNumber, minusProb)
  
  long.topics.prob <- long.topics.prob[!is.na(agency)]
  
  long.topics.prob[, AllWords := sum(TopicWords), by = "TopicNumber"]
  tmp <- long.topics.prob[, list(TopicWords = sum(TopicWords),
                                 AllWords = AllWords[1]),by = "TopicNumber,agency"]
  
  tmp[, AgencyPercent := TopicWords/AllWords]
  tmp$year <- yr
  topicstoagencies.yr <- topicstoagencies.yr %>% rbind(tmp)
}

fwrite(topicstoagencies.yr, "/Users/evolkova/Dropbox/Projects/Govt Agenda/Sandbox/20230401/topictoagency10.csv")
