require(data.table)
require(dplyr)
require(RCurl)
require(parallel)
require(httr)
require(rvest)


dwnld.files <- function(i) {
  url <- year.masterfile$html_url[i] %>%
    read_html() %>%
    html_nodes(xpath = '//*[@class="metadata_list"]') %>%
    html_text() %>%
    try()

  return(url[1])
}

for(yr in 1998:2004){
  
  year.masterfile <- yr %>%
    paste0("/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/Master and Texts/", ., ".rds") %>%
    readRDS(.)
  
  
  print(yr)
  print(dim(year.masterfile))
  print(Sys.time())
  year.masterfile$Info <- ""
  
  for (i in 1:length(year.masterfile$pdf_url)) {
    if (i %% 1000 == 0) print(Sys.time())
    
    try(year.masterfile$Info[i] <- dwnld.files(i))
  }
  saveRDS(year.masterfile, paste0("/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/Master and Texts/",yr, ".rds"))
}


