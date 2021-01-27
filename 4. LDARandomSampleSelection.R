require(data.table)
require(httr)
require(rvest)
require(dplyr)
require(lubridate)

set.seed(838)
random.sample <- NULL
for(yr in 1995:2019)
{
  print(yr)
  tmp <- readRDS(paste0("/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/Master and Texts/",yr,".rds"))
  
  tmp <- tmp[sample(1:length(tmp$title), 1000)]  
  random.sample <- rbind(random.sample, tmp)
}

saveRDS(random.sample, "/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/Master and Texts/random_sample.rds")
