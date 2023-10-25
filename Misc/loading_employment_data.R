
############################################################################
###################### LOADING EMPLOYMENT FILE  ############################
############################################################################

emp_data <- fread("/Users/evolkova/Dropbox/Projects/Govt Agenda/Sandbox/20230401/emp_data.csv")
cols <- c("N_hired","total_adjbase", "increase_adjbase", "average_increase_adjbase", "N_emp", 
          "median_increase_adjbase", "max_increase_adjbase", "N_promotions", "unexpected_promotion", "exit_private")

for(cl in cols) emp_data[, (paste0("lead.", cl)) := shift(.SD, n = 1, type = "lead"), .SDcols = cl, by = agysub_full]

############################################################################
###################### LOADING AGENCIES FILE  ##############################
############################################################################

agencies <-  "/Users/evolkova/Dropbox/Projects/Govt Agenda//Data/Agencies.csv" |> fread()
agencies[ clean.sub_agency == "-",  clean.sub_agency := clean.parent_agency ]
fedreg_master <- fread("/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/masterfile_fedreg.csv", select = c("type", "document_number"))

agencies <- left_join(agencies, fedreg_master)
agencies[, agysub_full := clean.sub_agency]
agency_data <- agencies[!is.na(year),list(N_Rule = length(unique(document_number[type == "Rule"])),
                                          N_Notice = length(unique(document_number[type == "Notice"])),
                                          Words_Rule = sum(nwords[type == "Rule"], na.rm = T),
                                          Words_PRule = sum(nwords[type == "Proposed Rule"], na.rm = T),
                                          Words_Notice = sum(nwords[type == "Notice"], na.rm = T)),by = "agysub_full,year"]

emp_data <- emp_data %>% 
  left_join(agency_data)


for(i in 3:dim(emp_data)[2]) emp_data[[i]] <- nm(win(emp_data[[i]], 0.01))


topicsubagencyyear <- fread("/Users/evolkova/Dropbox/Projects/Govt Agenda/Sandbox/20230401/topicsubagencyyear.csv",
                            select = c("agency", "year", "TopicWords", "TopicNumber")) %>% 
  rename(agysub_full = agency) 

topicsubagencyyear <- topicsubagencyyear[order(TopicWords)]
topicsubagencyyear[, Rank := 1:100, by = "agysub_full,year"]
topicsubagencyyear[, AgencyWords := sum(TopicWords), by = "agysub_full,year"]
setkey(topicsubagencyyear, agysub_full, TopicNumber, year)
topicsubagencyyear[, lag.Rank := shift(Rank, n = 1, type = "lag"), by = "agysub_full,TopicNumber"]
topic_data <- topicsubagencyyear[, list(Words_hightopic = sum(TopicWords[lag.Rank %in% 90:100])/AgencyWords[1],
                                        AgencyWords = AgencyWords[1]), by = "agysub_full,year"]

emp_data <- emp_data %>% 
  left_join(topic_data)


for(i in 3:dim(emp_data)[2]) emp_data[[i]] <- nm(win(emp_data[[i]], 0.01))
emp_data <- emp_data %>% select("lead.unexpected_promotion", "Words_Notice",
                                "Words_hightopic", "Words_Rule", "year", "agysub_full")

saveRDS(emp_data, "/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/emp_data.rds")