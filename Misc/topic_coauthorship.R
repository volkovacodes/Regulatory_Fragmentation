require(pacman)
p_load(data.table, lubridate, dplyr, modelsummary, lfe,tm)

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
agencies_num <- agencies[,list(N = length(unique(clean.parent_agency)), all_agencies = all_agencies[1]),by = document_number]


### load data topic*document*agency
out <- NULL
for(yr in 1995:2019) {
  print(yr)
  tmp <- yr %>%
    paste0(path, "/Data/LDA_Data/LDAFedReg/lda_random_sample_topics100_",.,".csv") %>%
    fread
  
  tmp[, document_number := filename]
  tmp <- tmp[!duplicated(document_number)]
  tmp$filename <- NULL
  
  dtm <- yr %>%
    paste0(path, "/Data/LDA_Data/LDAFedReg/dtm_random_sample_topics100_",.,"_server.rds") %>%
    readRDS
  
  m <- match(tmp$document_number, rownames(dtm))
  nwords <- rowSums(as.matrix(dtm))
  
  tmp$nwords <- nwords[m]
  
  m <- match(tmp$document_number, agencies_num$document_number)
  tmp$n_agencies <- agencies_num$N[m]
  tmp$all_agencies <- agencies_num$all_agencies[m]
  
  cols <- paste0("Topic", 1:100)
  x <- tmp[,which.max(.SD),.SDcols = cols, by = document_number]
  tmp$max_topic <- x$V1[match(tmp$document_number, x$document_number)]
  tmp[, max_topic := paste0("Topic",max_topic)]
  
  tmp_long <- melt(tmp, id.vars = c("document_number", "nwords", "n_agencies", "max_topic", "all_agencies"))
  tmp_long[, Solo := "Yes"]
  tmp_long[n_agencies > 1, Solo := "No" ]
  tmp_long[, topic_words := nwords*value]
  
  out1 <- tmp_long[,list(Solo_words = sum(topic_words[Solo == "Yes"])/sum(topic_words)), by = "variable"]
  
  x <- tmp_long[max_topic == variable]
  out2 <- x[,list(Solo_documents = sum(n_agencies == 1, na.rm = T)/.N),by = variable]

  m <- match(out1$variable, out2$variable)
  out1$Solo_documents <- out2$Solo_documents[m]
  out1$year <- yr
  rm(dtm)
  
  out <- rbind(out, out1)
}

fwrite(out, "/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/topic_coauthorship.csv")

