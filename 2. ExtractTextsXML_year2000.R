require(data.table)
require(httr)
require(rvest)
require(dplyr)
require(lubridate)

fedreg.master <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/Raw Data/masterfile_fedreg.csv" %>%
  fread

correction <- function(x) {
  if (length(x) > 0) {
    return(x)
  }
  if (length(x) == 0) {
    return(NA)
  }
}

get_node <- function(nd, nodetype) {
  nd %>%
    html_nodes(nodetype) %>%
    html_text()
  
}

### cleaning html files
### * drop first 13 lines and last two lines
### * drop [[Page XXX]] and [Page XXX]
### * drop [GRAPHIC] and [TIFF OMITTED]

clean.earlier.files <- function(txt)
{
  
  txt <- txt %>%
    strsplit("\n") %>%
    unlist
  
  txt <- try(txt <- txt[13:(length(txt) - 3)])

  if(class(txt) == "try-error") return("")
  txt <- gsub("(<a href=).*?(</a>)", "",txt, perl = T)

  txt <- gsub("<SUP>.*?</SUP>", "", txt, ignore.case = T)
  txt <- gsub("<span>.*?</span>", "", txt, ignore.case = T)
  #txt <- gsub("</SUP>", "", txt, ignore.case = T)
  
  txt <- gsub("\\[\\[Page \\d+\\]\\]", "" ,txt)
  txt <- gsub("\\[Page \\d+\\]", "" ,txt)
  txt <- gsub("\\[GRAPHIC\\]", "" ,txt)
  txt <- gsub("\\[TIFF OMITTED\\]", "" ,txt)
  txt <- paste0(txt, collapse = "\n")
  return(txt)
}


for(yr in 1999:1995)
{
  print(yr)
  require(DBI)
  require(RSQLite)
  year.masterfile <- fedreg.master[publication_date %>% mdy %>% year == yr]
  dbname <- paste0("/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/Raw Data/ScriptDownload/FedReg_",yr,".sqlite")
  con = dbConnect(SQLite(), dbname = dbname)
  
  add.data <- dbGetQuery(con, 'SELECT * FROM docs')
  
  
  m <- match(year.masterfile$document_number, add.data$DOCUMENTNUMBER)
  year.masterfile$TYPE <- ""
  year.masterfile$rin <- ""
  year.masterfile$texts <- add.data$HTMLTEXT[m]
  year.masterfile[, ind := 1:.N]
  year.masterfile[,texts := clean.earlier.files(texts), by = ind]
  year.masterfile[, ind := NULL]
  
  saveRDS(year.masterfile, paste0("/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/Master and Texts/",yr,".rds"))
  
}

###########################################################################
######################### YEAR 2000 CORREXCTION ###########################
###########################################################################

folder <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/Raw Data/BulkFedReg/"
daily.files <- list.files(folder,pattern = "xml$", recursive = TRUE)


types <- c("rule", "prorule", "notice", "presdocu")#, "correct")


for(yr in 2000)
{
  print(yr)
  print(Sys.time())
  year.daily.files <- daily.files[grepl(paste0("FR-",yr,"-"), daily.files)]
  year.masterfile <- fedreg.master[publication_date %>% mdy %>% year == yr]
  
  texts <- NULL
  
  for(fl in year.daily.files)
  {
    print(fl)
    one.day <- fl %>%
      paste0(folder,.) %>%
      read_html
    
    for(TYPE in types)
    {
      split.docs <- html_nodes(one.day, TYPE)
      frtext <- lapply(split.docs, html_text) %>%
        sapply(., correction)
      
      frdocs <- lapply(split.docs, get_node, nodetype = "frdoc") %>%
        sapply(., correction)
      
      rin <- lapply(split.docs, get_node, nodetype = "rin") %>%
        sapply(., correction)
      
      
      if(length(frtext) == 0 | length(frdocs) == 0)  next
      
      if(length(frtext) != length(frdocs))
      {
        next
      }
      
      res <- try(tmp <- data.table(document_number = frdocs, text = frtext, rin = unlist(rin)))
      if(class(res)[1] == "try-error") next
      tmp$type <- TYPE
      texts <- rbind(texts, tmp)
      
    }
  }
  
  texts[, docs := gsub(".*FR Doc. ", "", document_number)]
  texts[, docs := gsub("\\s+Filed\\s+.*", "" ,docs)]
  
  m <- match(year.masterfile$document_number, texts$docs)
  year.masterfile$texts <- texts$text[m]
  year.masterfile$rin <- texts$rin[m]
  year.masterfile$TYPE <- texts$type[m]
}


dbname <- paste0("//Users/evolkova/Dropbox/Projects/Govt Agenda/Data/Raw Data/ScriptDownload/FedReg_2000_jan118.sqlite")
con = dbConnect(SQLite(), dbname = dbname)

add.data <- dbGetQuery(con, 'SELECT * FROM docs')

ind <- which(is.na(year.masterfile$texts))
m <- match(year.masterfile[ind]$document_number, add.data$DOCUMENTNUMBER)
year.masterfile[ind]$TYPE <- ""
year.masterfile[ind]$rin <- ""
year.masterfile[ind]$texts <- add.data$HTMLTEXT[m]
year.masterfile[ind, texts := clean.earlier.files(texts), by = document_number]

saveRDS(year.masterfile, "/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/Master and Texts/2000.rds")
