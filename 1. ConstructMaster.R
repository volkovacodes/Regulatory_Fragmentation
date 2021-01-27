require(data.table)
require(dplyr)
require(stringr)
require(lubridate)

master <- NULL
for(yr in 1995:2019)
{
  print(yr)
  tmp <- yr %>%
    paste0("/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/Master and Texts/",.,".rds") %>%
    readRDS
  
  tmp$texts <- NULL
  
  master <- rbind(master, tmp)
}


x <- master[,strsplit(agency_names, split = "; ") %>% unlist, by = document_number]
x <- x[,.N, by = V1]
colnames(x) <- c("Agency", "N")

x <- x[order(N, decreasing = T)]
fwrite(x, "/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/list_of_agencies.csv")
