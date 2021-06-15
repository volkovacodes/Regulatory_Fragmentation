require(data.table)
require(httr)
require(rvest)
require(dplyr)
require(lubridate)

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

folder <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/Raw Data/BulkFedReg/"
daily.files <- list.files(folder, pattern = "xml$", recursive = TRUE)
fedreg.master <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/Raw Data/masterfile_fedreg.csv" %>%
  fread()

types <- c("rule", "prorule", "notice", "presdocu")#, "correct")


for (yr in 2001:2019)
{
  print(yr)
  print(Sys.time())
  year.daily.files <- daily.files[grepl(paste0("FR-", yr, "-"), daily.files)]
  year.masterfile <- fedreg.master[publication_date %>%
    mdy() %>%
    year() == yr]

  texts <- NULL

  for (fl in year.daily.files)
  {
    #print(fl)
    one.day <- fl %>%
      paste0(folder, .) %>%
      read_html()

    for (TYPE in types)
    {
      split.docs <- html_nodes(one.day, TYPE)
      frtext <- lapply(split.docs, html_text) %>%
        sapply(., correction)

      frdocs <- lapply(split.docs, get_node, nodetype = "frdoc") %>%
        sapply(., correction)

      rin <- lapply(split.docs, get_node, nodetype = "rin") %>%
        sapply(., correction)


      if (length(frtext) == 0 | length(frdocs) == 0) next

      if (length(frtext) != length(frdocs)) {
        # print(paste("BBB: ", fl, " ", TYPE))
        #  errors <- errors + length(frtext)
        next
      }

      res <- try(tmp <- data.table(document_number = frdocs, rin = unlist(rin), text = frtext))
      if (class(res)[1] == "try-error") next
      tmp$type <- TYPE
      texts <- rbind(texts, tmp)
    }
  }

  texts[, docs := gsub(".*FR Doc. ", "", document_number)]
  texts[, docs := gsub("\\s+Filed\\s+.*", "", docs)]

  m <- match(year.masterfile$document_number, texts$docs)
  year.masterfile$texts <- texts$text[m]
  year.masterfile$rin <- texts$rin[m]
  year.masterfile$TYPE <- texts$type[m]

  saveRDS(year.masterfile, paste0("/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/Master and Texts/", yr, ".rds"))
}
