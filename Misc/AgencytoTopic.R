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

master <- NULL
for(yr in 1995:2019) {
  print(yr)
  #/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/Master and Texts/1995.rds
  fedregmaster.yr <- readRDS(paste0("./Data/Master and Texts/",yr, ".rds")) %>% 
    select("document_number", "html_url", "publication_date" ,"Info", "type", "TYPE")
  fedregmaster.yr$year <- yr
  #fedredmaster.yr <- fedredmaster.yr[grep("\\bRIN\\b",Info, ignore.case = T)]
  fedregmaster.yr[, RIN := gsub("\\s+", " ", Info)]
  fedregmaster.yr[, RIN := gsub(".*RIN: ", "", RIN)]
  fedregmaster.yr[, RIN := gsub(" Document Number:.*", "", RIN)]
  
  fedregmaster.yr[, publication_date := mdy(publication_date)]
  fedregmaster.yr[, n := 1:.N, by = RIN]
  fedregmaster.yr[!grepl("\\bRIN\\b",Info, ignore.case = T), RIN := NA]
  #fedregmaster.yr <- fedregmaster[n == 1]
  
  master <- rbind(master, fedregmaster.yr |> select(document_number, publication_date, type, TYPE, RIN, year))
}

setkey(master, RIN, publication_date)
master[, n := 1:.N, by = RIN]
master[, type2  := ""]
master[!is.na(RIN) & n == 1, type2 := "RIN"]
master[!is.na(RIN) & n > 1, type2 := "old_RIN"]
setkey(master, publication_date)

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
for(yr in 1995:2019)
{
  print(yr)
  
  long.topics.prob <- melt(topics.prob[year == yr], id.vars = c("document_number", "nwords", "type", "type2" ,"date", "month", 
                                                    "quarter","year", "agency", "Nagencies"))
  
  long.topics.prob[, TopicWords := value*nwords/Nagencies]
  colnames(long.topics.prob)[colnames(long.topics.prob) == "variable"] <- "TopicNumber"
  colnames(long.topics.prob)[colnames(long.topics.prob) == "value"] <- "TopicProb"
  
  long.topics.prob[, minusProb := -TopicProb]
  setkey(long.topics.prob, TopicNumber, minusProb)
  
  long.topics.prob <- long.topics.prob[!is.na(agency)]
  
  long.topics.prob[, AllWords := sum(TopicWords), by = "TopicNumber"]
  tmp <- long.topics.prob[, list(TopicWords = sum(TopicWords),
                                               AllWords = AllWords[1]),by = "TopicNumber,agency,year"]
  
  tmp[, AgencyPercent := TopicWords/AllWords]
  topicstoagencies.yr <- topicstoagencies.yr %>%
    rbind(tmp)
}
  
fwrite(topicstoagencies.yr, "./Data/topicagencyyear.csv")


get.specific.type <- function(TP)
{
  topicstoagencies.yr  <- NULL
  for(yr in 1995:2019)
  {
    print(yr)
    
    if(TP %in% c("RIN", "old_RIN")) topics.prob[, type := type2]
    long.topics.prob <- melt(topics.prob[year == yr & type == TP], id.vars = c("document_number", "nwords", "type", "type2" ,"date", "month", 
                                                                  "quarter","year", "agency", "Nagencies"))
    
    long.topics.prob[, TopicWords := value*nwords/Nagencies]
    colnames(long.topics.prob)[colnames(long.topics.prob) == "variable"] <- "TopicNumber"
    colnames(long.topics.prob)[colnames(long.topics.prob) == "value"] <- "TopicProb"
    
    long.topics.prob[, minusProb := -TopicProb]
    setkey(long.topics.prob, TopicNumber, minusProb)
    
    long.topics.prob <- long.topics.prob[!is.na(agency)]
    
    long.topics.prob[, AllWords := sum(TopicWords), by = "TopicNumber"]
    tmp <- long.topics.prob[, list(TopicWords = sum(TopicWords),
                                   AllWords = AllWords[1]),by = "TopicNumber,agency,year"]
    
    tmp[, AgencyPercent := TopicWords/AllWords]
    topicstoagencies.yr <- topicstoagencies.yr %>%
      rbind(tmp)
  }
  
  fwrite(topicstoagencies.yr, paste0("/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/topicagencyyear_",TP,".csv"))
  
}

get.specific.type("Notice")
get.specific.type("Proposed Rule")
get.specific.type("Rule")
get.specific.type("RIN")
get.specific.type("old_RIN")
