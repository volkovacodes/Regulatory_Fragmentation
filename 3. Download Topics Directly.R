require(dplyr)
require(data.table)
require(rvest)


x <- "https://www.federalregister.gov/topics" %>%
  read_html  %>% 
  html_nodes('[id="topic-list"]') %>%
  html_nodes("li") 


count <- x %>%
  html_nodes("span") %>% 
  html_text

links <- x %>% 
  html_nodes("a") %>%
  as.character

  
cfr.topics <- data.table(names = links, count = count)

cfr.topics[, count := gsub("[[:space:]]", "", count) %>% as.numeric]
cfr.topics[, links := gsub('<a href="', "" ,names)]
cfr.topics[, links := gsub('".*', "", links)]

cfr.topics[, names := gsub('.*">', "", names)]
cfr.topics[, names := gsub('<.*', "", names)]



short.cfr.topics <- cfr.topics[count %in% 1:1000]

for(i in 1:length(short.cfr.topics$names))
{

  tmp <- gsub("/topics/", "", short.cfr.topics$links[i])
  
  name.out <- paste0("/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/Raw Data/CFR topics/", tmp, ".csv")
  if(file.exists(name.out)) next
  print(tmp)
  link <- paste0("https://www.federalregister.gov/documents/search?conditions%5Btopics%5D%5B%5D=",tmp, "&format=csv")
  
  res <- try(download.file(link, name.out, quiet = T))
  if(class(res) == "try-error") next
  x <- fread(name.out)
  x$CFR.topic <- short.cfr.topics$names[i]
  fwrite(x, name.out)
}


long.cfr.topics <- cfr.topics[count >= 1000]
long.cfr.topics <- long.cfr.topics[!grepl("^Reporting ", names)]
long.cfr.topics <- long.cfr.topics[names != "Administrative practice and procedure"]


dim(long.cfr.topics)
#dim(cfr.topics[count <= 50])
require(lubridate)

origin <- ymd("20000101")
lastday <- ymd("20191231")

temp.name <- "/Users/evolkova/Downloads/tempfile.csv"
for(i in 1:length(long.cfr.topics$names))
{
  
  tmp <- gsub("/topics/", "", long.cfr.topics$links[i])
  name.out <- paste0("/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/Raw Data/CFR topics/", tmp, ".csv")
  out <- NULL
  count <- long.cfr.topics$count[i] %>% as.numeric
  width <- 500
  blocks <- round(count/width) + 1
  Ndays <- (lastday - origin)/blocks
  
  if(file.exists(name.out)) next
  
  for(block in 1:blocks)
  {
    start.date <- origin + (block - 1)*Ndays
    end.date <- start.date + Ndays
    
    cond1 <- paste0("https://www.federalregister.gov/documents/search?conditions%5Bpublication_date%5D%5Bgte%5D=",
                    month(start.date) %>% sprintf("%02d", .),
                    "%2F", day(start.date) %>% sprintf("%02d", .),
                    "%2F", year(start.date))
    
    cond2 <- paste0("&conditions%5Bpublication_date%5D%5Blte%5D=",
                    month(end.date) %>% sprintf("%02d", .),
                    "%2F", day(end.date) %>% sprintf("%02d", .),
                    "%2F", year(end.date), 
                    "&conditions%5Btopics%5D%5B%5D=",tmp,"&format=csv")
    
    
    link <- paste0(cond1, cond2)
    
    download.file(link, temp.name, quiet = T)
    temp.file <- fread(temp.name)
    temp.file$CFR.topic <- long.cfr.topics$names[i]
    
    if(dim(temp.file)[1] >= 1000) print(tmp)
    
    out <- out %>%
      rbind(temp.file)
    
  }
  
  out <- out[!duplicated(document_number)]
  fwrite(out, name.out)
  
  
}

setwd("/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/Raw Data/CFR topics/")
files <- list.files()

out <- NULL

for(fl in files)
{
  tmp <- fread(fl)
  tmp$CFR.topic.short <- fl
  out <- rbind(tmp, out)
}
  
fwrite(out, "/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/CFR topics.csv")  

  