### sup code: /Users/evolkova/Dropbox/Projects/Govt Agenda/Code/Gov_Agenda/Misc/AgencytoTopic.R
require(data.table)
require(lubridate)
require(dplyr)
require(tm)

na0 <- function(x) {
  x[is.na(x)] <- 0
  return(x)
}

win <- function(x, eps = 0.005){
  up <- quantile(x, na.rm = T, 1 - eps)
  down <- quantile(x, na.rm = T, eps)
  x[x>up] <- up
  x[x<down] <- down
  return(x)
}

cnt <- function(x){
  if(length(x) < 3) return(0)
  y <- str_count(x, ";") + 1
  return(y)
}

dsp <- function(x){
  if(length(x) == 0) return(0)
  y <- strsplit(x, split = ";") %>% unlist %>% as.numeric()
  fracs <- y/sum(y)
  hhi <- sum(fracs^2)
  frag <- 1 - hhi
  return(frag)
}

sm <- function(x){
  x <- strsplit(x, ";") %>% unlist %>% as.numeric
  return(sum(x,na.rm = T))
}

data_path <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/"
outpath <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/Sandbox/20201029/temp_files/"

##################################################################################
#################### LOAD LDA COMPANY DATA #######################################
##################################################################################

companyyear <- NULL
for (yr in 1994:2019) {
  master <- NULL
  
  for (i in 1:4) {
    master <- master %>%
      rbind(fread(paste0("/Users/evolkova/Dropbox/SEC Filings/10-K/Master with Compustat/master_", yr, i, ".csv")))
  }

  master <- master %>%
    select("cik", "name", "file", "GVKEY", "conm", "at", "sic")

  master$year <- yr
  ldacompanies <- paste0("/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/LDA_Data/LDA10K/lda_random_sample_topics100_10K", yr, ".csv") %>%
    fread()

  dtmcompanies <- paste0("/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/LDA_Data/LDA10K/dtm_random_sample_topics100_10K", yr, ".rds") %>%
    readRDS()

  words <- rowSums(as.matrix(dtmcompanies))

  m <- match(paste0(master$file, ".htm"), ldacompanies$filename)
  master <- cbind(master, ldacompanies[m])
  m <- match(paste0(master$file, ".htm"), dtmcompanies$dimnames$Docs)
  master$Words10K <- words[m]
  master <- master[!is.na(at) & !is.na(Topic1) & !is.na(sic)]

  companyyear <- rbind(companyyear, master)
  
  rm(master)
  rm(ldacompanies)
  rm(dtmcompanies)
}

setkey(companyyear, cik, year)
companyyear <- companyyear[!duplicated(paste0(cik, year))]

##################################################################################
############## LOAD COMPUSTAT/CRSP COMPANY DATA ##################################
##################################################################################

crsp <- "/Users/evolkova/Dropbox/DataY/CRSP/MSF/CRSP_MSF.csv" %>%
  fread()

#crsp <- crsp[date > 19900101]
crsp[, date := ymd(date)]
crsp <- crsp[month(date) == 12]
crsp[, year := year(date)]
crsp[, size := abs(PRC) * SHROUT]
crsp[, ipo_year := min(year,na.rm = T), by = PERMNO]
crsp <- crsp[year >= 1990]


comp <- "/Users/evolkova/Dropbox/DataY/Compustat/crsp_compustat_1990_2020.csv" %>%
  fread()

comp <- comp[!is.na(LPERMNO)]
comp <- comp[fyr > 0 & at > 0]
comp[, year := fyear]
comp[fyr %in% 1:5, year := fyear + 1]

### matching ipo year
comp$ipo_year <- crsp$ipo_year[match(comp$LPERMNO, crsp$PERMNO)]

### matching size
m <- match(paste(comp$LPERMNO, comp$year), paste(crsp$PERMNO, crsp$year - 1))
comp$size <- crsp$size[m]


### calculating book
comp[, pstock := pstkl]
comp[is.na(pstock), pstock := pstkrv]
comp[is.na(pstock), pstock := pstk]
comp[is.na(pstock), pstock := 0] ### is it right?
comp[, se := seq]
comp[is.na(se) & !is.na(ceq) & !is.na(pstk), se := ceq + pstk]
comp[is.na(se), se := at - lt]
comp[is.na(txditc), txditc := 0]
comp[is.na(dcvt), dcvt := 0]
comp[, be := se - pstock + txditc + dcvt]

### market-to-book
comp[, MB := (size / 10^3) / (be)]

### ppe to assets
comp[, ppe_at := ppent / at]

### ebitda to assets
comp[, ebitda_at := ebitda / at]

### sga to assets
comp[, sga := xsga]
comp[, sga_at := xsga / at]

### employment to sales
comp[, emp_sale := emp / sale]
comp[, emp_at := emp / at]

### sales growth
setkey(comp, GVKEY, year)

comp[, lag.sale := shift(sale, 1, type = "lag"), by = "GVKEY"]
comp[, growth := 100 * (sale / lag.sale - 1)]

### R&D
comp[is.na(xrd), xrd := 0]

### capx_at
comp[, capx_at := capx / at]
comp[is.na(capx_at), capx_at := 0]
comp[, inv_at := (capx + xrd) / at]
comp[is.na(inv_at), inv_at := 0]
comp[, rd_at := (xrd) / at]
comp[is.na(rd_at), rd_at := 0]

### tobin's q
comp[, tobin := (at - ceq + size / 10^3) / at]
# comp[, tobin := (at - ceq - n0(txdb) + marcap)/at] alternative definition in some other papers

### free cash flows
comp[, fcf := (ib + na0(dp)) / at]

### leverage
comp[, leverage := (na0(dltt) + na0(dlc)) / at]

### return on assets
comp[, roa := ib / at]

### cash
comp[, cash_at := che / at]
comp[, cash := che]

### assets
comp[, lag.at := shift(at, 1, type = "lag"), by = "GVKEY"]
comp[, growth.at := 100 * (at / lag.at - 1)]

vars <- c(
  "fyear", "sale", "growth", "MB", "ppe_at", "ebitda_at", "sga", "sga_at", "emp" ,"emp_sale", "emp_at", "xrd",
  "capx_at", "inv_at", "fcf", "tobin", "leverage", "roa", "cash", "cash_at", "growth.at", "ipo_year"
)

##################################################################################
############################# matching data  #####################################
##################################################################################

m <- match(paste(companyyear$cik, companyyear$year), paste(comp$cik, comp$year))
tmp <- comp[m] %>%
  select(vars)

companyyear <- companyyear %>%
  cbind(tmp)

m <- match(paste0(companyyear$cik, companyyear$year), paste0(comp$cik, comp$year + 1))
companyyear$lead.sga_at <- comp$sga_at[m]
companyyear$lead.emp_sale <- comp$emp_sale[m]
companyyear$lead.emp_at <- comp$emp_at[m]
companyyear$lead.capx_at <- comp$capx_at[m]
companyyear$lead.inv_at <- comp$inv_at[m]
companyyear$lead.rd_at <- comp$rd_at[m]
companyyear$lead.leverage <- comp$leverage[m]
companyyear$lead.roa <- comp$roa[m]
companyyear$lead.growth <- comp$growth[m]
companyyear$lead.growth.at <- comp$growth.at[m]
companyyear$lead.cash_at <- comp$cash_at[m]
companyyear$lead.cash <- comp$cash[m]


m <- match(paste0(companyyear$cik, companyyear$year), paste0(comp$cik, comp$year + 2))
companyyear$lead2.sga_at <- comp$sga_at[m]
companyyear$lead2.growth <- comp$growth[m]
companyyear$lead2.growth.at <- comp$growth.at[m]
companyyear$lead2.emp_at <- comp$emp_at[m]
companyyear$lead2.roa <- comp$roa[m]

m <- match(paste0(companyyear$cik, companyyear$year), paste0(comp$cik, comp$year + 3))
companyyear$lead3.sga_at <- comp$sga_at[m]
companyyear$lead3.growth <- comp$growth[m]
companyyear$lead3.growth.at <- comp$growth.at[m]
companyyear$lead3.emp_at <- comp$emp_at[m]
companyyear$lead3.roa <- comp$roa[m]


rm(comp)
rm(crsp)
##################################################################################
################### LOAD COMPUSTAT SEGMENT #######################################
##################################################################################

compseg <- "/Users/evolkova/Dropbox/DataY/Compustat/compustat_segments_1990.csv" %>%
  fread(select = c("stype", "gvkey", "sid", "srcdate"))
compseg <- compseg[stype == "BUSSEG"]
compseg[, year := year(ymd(srcdate))]
setkey(compseg, gvkey, year, sid)

compseg <- compseg[, list(Nseg = length(unique(sid))), by = "gvkey,year"]

m <- match(paste(companyyear$GVKEY, companyyear$year), paste(compseg$gvkey, compseg$year))
companyyear$Nsegments <- compseg$Nseg[m]
companyyear$Nsegments[is.na(companyyear$Nsegments)] <- 1

m <- match(paste(companyyear$GVKEY, companyyear$year), paste(compseg$gvkey, compseg$year + 1))
companyyear$lead.Nsegments <- compseg$Nseg[m]
companyyear$lead.Nsegments[is.na(companyyear$lead.Nsegments)] <- 1

rm(compseg)
##################################################################################
################# @@@## ADD INDUSTRIES INFO #######################################
##################################################################################

ffmap <- "/Users/evolkova/Dropbox/DataY/SIC_to_Fama_French_industry.csv" %>% fread()
m <- match(companyyear$sic, ffmap$SIC0)
companyyear$FF12 <- ffmap$FF_12[m]
companyyear$FF48 <- ffmap$FF_48[m]
companyyear[, FF48_year := paste(FF48, year)]


industries <- fread("/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/fic_data.txt", sep = "\t")

m <- match(paste0(companyyear$GVKEY, companyyear$year), paste0(industries$gvkey, industries$year))
companyyear$icode500 <- industries$icode500[m]

m <- match(paste0(companyyear$GVKEY, companyyear$year), paste0(industries$gvkey, industries$year + 1))
companyyear$lead.icode500 <- industries$icode500[m]

rm(ffmap)
rm(industries)
##################################################################################
###################### ADD Productivity ##########################################
##################################################################################

tfp <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/TFPData_updated.csv" %>%
  fread()

m <- match(paste(companyyear$GVKEY, companyyear$year), paste(tfp$gvkey, tfp$fyear))
companyyear$tfp <- tfp$TFP[m]

m <- match(paste(companyyear$GVKEY, companyyear$year + 1), paste(tfp$gvkey, tfp$fyear))
companyyear$lead.tfp <- tfp$TFP[m]

m <- match(paste(companyyear$GVKEY, companyyear$year + 2), paste(tfp$gvkey, tfp$fyear))
companyyear$lead2.tfp <- tfp$TFP[m]

m <- match(paste(companyyear$GVKEY, companyyear$year + 3), paste(tfp$gvkey, tfp$fyear))
companyyear$lead3.tfp <- tfp$TFP[m]

##################################################################################
################## Three measures generation #####################################
########## main variables, docs types, new/old RINS ##############################
##################################################################################

### this function creates measures of frag/disp/complexity using the same company_year_topic.melt datatable

company_year_topic.melt <- companyyear %>%
  select("cik", "year", paste0("Topic", 1:100)) %>%
  melt(id.vars = c("cik", "year")) %>% 
  mutate(topic_year = paste(variable, year))

getdata_upd <- function(filename, tp) {
  topicstoagencies <- data_path %>%
    paste0(filename) %>%
    fread()

  data <- topicstoagencies[, list(HHI = sum(AgencyPercent^2), Words = AllWords[1]),
    by = c("year,TopicNumber")
  ]
  
  data[, topic_year := paste(paste0("Topic", TopicNumber), year)]
  m <- match(company_year_topic.melt$topic_year, data$topic_year)
  
  company_year_topic.melt$hhi_topic_agency <- data$HHI[m]
  company_year_topic.melt$fedreg_topic_words <- data$Words[m]
  
  out <- company_year_topic.melt[, list(topic.disp = 1 - sum(value^2),
                                        regul.disp = 1 - sum(value * hhi_topic_agency),
                                        regul.complex.log = sum(value * log(fedreg_topic_words))), by = 'cik,year']
  m <- match(paste0(companyyear$cik, companyyear$year), paste0(out$cik, out$year))
  
  out <- out[m] %>% select(regul.disp, topic.disp, regul.complex.log)
  colnames(out) <- colnames(out) %>% paste0(tp)
  return(out)
}

### get files from this code ./Misc/AgencytoTopic.R 
companyyear <- companyyear %>% 
  cbind(getdata_upd("topicagencyyear.csv", "")) %>% 
  cbind(getdata_upd("topicagencyyear_Notice.csv", "_Notice")) %>% 
  cbind(getdata_upd("topicagencyyear_Proposed Rule.csv", "_PRule")) %>% 
  cbind(getdata_upd("topicagencyyear_Rule.csv", "_Rule")) %>% 
  cbind(getdata_upd("topicagencyyear_RIN.csv", "_RIN")) %>% 
  cbind(getdata_upd("topicagencyyear_old_RIN.csv", "_old_RIN")) %>% 
  cbind(getdata_upd("topictoagency5.csv", "_rolling5")) %>% 
  cbind(getdata_upd("topictoagency10.csv", "_rolling10")) 


#### here we generate top1,2,3 topics 
companyyear.melt.av <- company_year_topic.melt[,list(value = mean(value,na.rm = T)), by = "cik,variable"]
companyyear.melt.av[,`:=`(topicmax1 = paste0("Topic", order(value, decreasing = T)[1]),
                          topicmax2 = paste0("Topic", order(value, decreasing = T)[2]),
                          topicmax3 = paste0("Topic", order(value, decreasing = T)[3])), by = "cik"]

m <- match(company_year_topic.melt$cik, companyyear.melt.av$cik)
company_year_topic.melt <- company_year_topic.melt %>% cbind(select(companyyear.melt.av[m], paste0("topicmax", 1:3)))

topictoagency <- fread(paste0(data_path,"topicagencyyear.csv"))
data <- topictoagency[,list(HHI = sum(AgencyPercent^2), Words = AllWords[1]), by = "TopicNumber,year"]
data[, disp := 1 - HHI]
data[, topic_year := paste(paste0("Topic", TopicNumber), year)]

### main topic
main_topic <- company_year_topic.melt[,list(topic_main_year = which.max(value)), by = "cik,year"]
companyyear <- companyyear %>% inner_join(main_topic)
companyyear$regul.disp_main <- data$disp[match(paste0(companyyear$topic_main_year, companyyear$year), 
                                                            paste0(data$TopicNumber, data$year))]
companyyear[, topic.disp_main := topic.disp]
companyyear[, regul.complex.log_main := regul.complex.log]


m <- match(company_year_topic.melt$topic_year, data$topic_year)

company_year_topic.melt$hhi_topic_agency <- data$HHI[m]
company_year_topic.melt$fedreg_topic_words <- data$Words[m]

### michelle asked for top3 across all companies 
out1 <- company_year_topic.melt[!variable %in% paste0("Topic", c(31,2,59)) , list(topic.disp = 1 - sum(value^2),
                                                                                                     regul.disp = 1 - sum(value * hhi_topic_agency),
                                                                                                     regul.complex.log = sum(value * log(fedreg_topic_words))), by = 'cik,year']
m <- match(paste(companyyear$cik, companyyear$year), paste(out1$cik, out1$year))
companyyear$regul.disp_no3 <- out1$regul.disp[m] 
companyyear$topic.disp_no3 <- out1$topic.disp[m] 
companyyear$regul.complex.log_no3 <- out1$regul.complex.log[m] 

##################################################################################
################## Three measures generation #####################################
############### for 100 topics grouped into 57/70  ###############################
##################################################################################

### this function creates measures of frag/disp/complexity using the merged company_year_topic.melt datatable
group_measures <- function(gr.col, tp) {

  ### read manually coded groups 
  groups <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/Sandbox/20210128/topic_labels_ML_KV_JK.csv" %>%
    fread(select = c("Topic", "Group_KV", "Group_JK2"))
  groups[, TopicNumber := paste0("Topic", Topic)]
  

  ### then we re-estimate topic HHI
  topicstoagencies <- data_path %>%
    paste0("topicagencyyear.csv") %>%
    fread()
  
  ### aggregate group information
  m <- match(topicstoagencies$TopicNumber, groups$Topic)
  topicstoagencies$group <- groups[[gr.col]][m]
  topicstoagencies <- topicstoagencies[, list(GroupWords = sum(TopicWords, na.rm = T)), by = "group,year,agency"]
  topicstoagencies[, AllWords := sum(GroupWords, na.rm = T), by = "group,year"]
  topicstoagencies[, GroupPercent := GroupWords / AllWords]

  data <- topicstoagencies[, list(HHI = sum(GroupPercent^2, na.rm = T), Words = AllWords[1]), by = c("year,group")]

  

  m <- match(company_year_topic.melt$variable, groups$TopicNumber)
  company_year_topic.melt$group <- groups[[gr.col]][m]

  company_year_group.melt <- company_year_topic.melt[, list(value = sum(value, na.rm = T)),
    by = "cik,year,group"
  ]

  m <- match(
    paste(company_year_group.melt$year, company_year_group.melt$group),
    paste(data$year, data$group)
  )

  company_year_group.melt$HHI <- data$HHI[m]
  company_year_group.melt$fedreg_group_words <- data$Words[m]

  measures <- company_year_group.melt[, list(
    regul.disp = 1 - sum(value * HHI, na.rm = T),
    regul.complex.log = sum(value * log(fedreg_group_words), na.rm = T),
    topic.disp = 1 - sum(value^2, na.rm = T)
  ), by = "cik,year"]
  ### return properly matched data

  m <- match(
    paste(companyyear$cik, companyyear$year),
    paste(measures$cik, measures$year)
  )

  measures <- measures[m] %>% select("regul.disp", "topic.disp", "regul.complex.log")
  colnames(measures) <- paste0(colnames(measures), tp)
  return(measures)
}


companyyear <- companyyear %>% 
  cbind(group_measures("Group_KV", "_57"))%>% 
  cbind(group_measures("Group_JK2", "_70"))


##################################################################################
################## Three measures generation #####################################
############### for new LDAs with 200/300/1000 topics, item7  ####################
##################################################################################

### here we use all new topicstoagencies file and company.melt files
get_measures <- function(topicfiles, agencyfile, tp, frm = ".csv"){
  ### reading 10K info
  companyyear.melt <- NULL
  for(yr in 1995:2019){
    nm <- paste0(topicfiles, yr, frm)
    if(frm == '.csv') tmp <- fread(nm)
    if(frm == '.rds') tmp <- readRDS(nm)
    tmp$year <- yr
    companyyear.melt <- rbind(companyyear.melt, tmp)
  }
  colnames(companyyear.melt)[length(colnames(companyyear.melt)) - 1] <- "filename"
  companyyear.melt <- melt(companyyear.melt, id.vars = c("filename","year"))
  
  ### reading fr info
  topicstoagencies <- fread(agencyfile)
  data <- topicstoagencies[, list(HHI = sum(AgencyPercent^2), Words = AllWords[1]), by = c("year,TopicNumber")]
  m <- match(paste0(companyyear.melt$variable, companyyear.melt$year), paste0("Topic",data$TopicNumber, data$year))
  companyyear.melt$hhi_topic_agency <- data$HHI[m]
  companyyear.melt$fedreg_topic_words <- data$Words[m]
  companyyear.melt <- companyyear.melt %>% data.table
  
  measures <- companyyear.melt[,list(regul.disp = 1 - sum(value * hhi_topic_agency, na.rm = T), 
                                     topic.disp = 1 - sum(value^2,na.rm = T),
                                     regul.complex.log = sum(value * log(fedreg_topic_words), na.rm = T)), by = "filename"]
  
  measures[, file := gsub("(\\.txt)|(\\.htm)", "", filename)]
  
  m <- match(companyyear$file, measures$file)
  measures <- measures[m] %>% select("regul.disp", "topic.disp", "regul.complex.log")
  colnames(measures) <- colnames(measures) %>% paste0(tp)
  return(measures)
}

topicfiles <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/Sandbox/JF_1_Revision_R1_3b_and_R2_3.3/LDA_Data_more_topics/lda_random_sample_topics200_10K"
agencyfile <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/Sandbox/JF_1_Revision_R1_3b_and_R2_3.3/LDA_Data_more_topics/agency_to_topic_200.csv"
companyyear <- companyyear %>% cbind(get_measures(topicfiles, agencyfile, "_200"))

topicfiles <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/Sandbox/JF_1_Revision_R1_3b_and_R2_3.3/LDA_Data_more_topics/lda_random_sample_topics300_10K"
agencyfile <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/Sandbox/JF_1_Revision_R1_3b_and_R2_3.3/LDA_Data_more_topics/agency_to_topic_300.csv"
companyyear <- companyyear %>% cbind(get_measures(topicfiles, agencyfile, "_300"))

topicfiles <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/Sandbox/JF_1_Revision_R1_3b_and_R2_3.3/LDA_Data_more_topics/lda_random_sample_topics1000_10K"
agencyfile <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/Sandbox/JF_1_Revision_R1_3b_and_R2_3.3/LDA_Data_more_topics/agency_to_topic_1000.csv"
companyyear <- companyyear %>% cbind(get_measures(topicfiles, agencyfile, "_1000"))

topicfiles <- "/Users/evolkova/Dropbox/Projects/Govt Agenda//Sandbox/JF_1_Revision_R1_5_and_R2_2.3/LDA_files/lda_item_7_100_"
agencyfile <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/topicagencyyear.csv"
companyyear <- companyyear %>% cbind(get_measures(topicfiles, agencyfile, "_item7", frm = ".rds"))

topicfiles <- "/Users/evolkova/Dropbox/Projects/Govt Agenda//Sandbox/JF_1_Revision_R1_5_and_R2_2.3/LDA_files_item1/lda_item_1_"
agencyfile <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/topicagencyyear.csv"
companyyear <- companyyear %>% cbind(get_measures(topicfiles, agencyfile, "_item1", frm = ".rds"))

##################################################################
###################### Lead lobby ################################
##################################################################

lobby <- readRDS("/Users/evolkova/Dropbox/Projects/Govt Agenda/Sandbox/20210406/lobby.rds")

get_lobby <- function(m, names){
  
  companyyear$x <- lobby$amount[m] 
  companyyear[, lobbied := max(!is.na(x)), by = GVKEY]
  companyyear[, min_year := max(year)]
  companyyear[lobbied == 1, min_year := min(year[!is.na(x)]), by = GVKEY]
  companyyear[lobbied == 1 & year >= min_year & is.na(x), x := 0]
  
  companyyear[, logx := log(1 + x) %>% win]
  companyyear[, x_sale := x/(10^6*sale) %>% win]
  companyyear[, x_at := x/(10^6*at) %>% win]
  companyyear[, x := x %>% win]
  
  out <- companyyear %>% select("x", "logx", "x_sale")
  colnames(out) <- names
  return(out)
}
m <- match(paste(companyyear$GVKEY, companyyear$year + 1), paste(lobby$gvkey, lobby$report_year))
companyyear <- companyyear %>%
  cbind(get_lobby(m, c("lead.lobby", "lead.log_lobby", "lead.lobby_sale")))

m <- match(paste(companyyear$GVKEY, companyyear$year), paste(lobby$gvkey, lobby$report_year))
companyyear <- companyyear %>%
  cbind(get_lobby(m, c("lobby", "log_lobby", "lobby_sale")))


##################################################################
###################### Updates after revision1  ##################
##################################################################


### 1. Hoberg-Phillips Peers 
### source("./Misc/Hoberg_Philips_peers.R")
peers <- fread(paste0(data_path,"/HP_peers.csv"))
vars_need <- c("n_peers", "n_new_peers", "n_ipo" ,"n_leaving_peers", "n_delist", "n_poor_perf","n_peers_left_trading")

create_vars <- function(companyyear, vec, title){
  m <- match(vec, paste(peers$gvkey1, peers$year))
  add <- peers[m] %>% select(vars_need)
  
  colnames(add) <- paste0(title, colnames(add))
  for(i in 1:length(colnames(add))){
    add[[i]][is.na(add[[i]])] <- 0
    add[[i]] <- add[[i]] %>% win
  }
  companyyear <- cbind(companyyear, add)
  return(companyyear)
}


companyyear <- companyyear %>% create_vars(paste(companyyear$GVKEY, companyyear$year + 1), "lead.")  %>% 
  create_vars(paste(companyyear$GVKEY, companyyear$year), "")  

companyyear$lead.n_join <- (companyyear$lead.n_ipo - companyyear$lead.n_delist)
companyyear$lead.n_left <- 100*(companyyear$lead.n_peers_left_trading/companyyear$n_peers)

### 2  Coauthored topics 
### source("./Misc/topic_coauthorship.R")
coauthored <- fread(paste0(data_path, "/topic_coauthorship.csv"))
companyyeartopic <- companyyear %>% 
  select("cik", "year", "Words10K", paste0("Topic", 1:100)) %>% 
  melt(id.vars = c("cik", "year", "Words10K")) %>% 
  mutate(words = Words10K*value) %>% 
  left_join(coauthored) %>% 
  mutate(solo_topic = 0)

companyyeartopic[Solo_words > quantile(Solo_words, 0.5, na.rm = T), solo_topic := 1]
companyyeartopic[, solo_topic_words := sum(words*solo_topic), by = "cik,year"]
companyyeartopic[, coauthored_topic_words := sum(words*(1 - solo_topic)), by = "cik,year"]
companyyeartopic[, coauthored := 1]
companyyeartopic[solo_topic_words >= coauthored_topic_words, coauthored := 0]

indicator <- companyyeartopic[,list(coauthored = coauthored[1]), by = "cik,year"]
companyyear <- companyyear %>% inner_join(indicator)

# 3. Measure verification
agencies_in_notices.melt <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/Sandbox/JF_1_Revision_R1_4_and_R2_2.1/agencies_in_notices.csv" %>% fread
agencies_in_notices <- agencies_in_notices.melt[,list(Ndocuments = length(unique(document_number))),by = "gvkey,year,parent_agency_notice"]


colnames(agencies_in_notices) <- c("GVKEY","year", "parent_agency", "Ndocuments")
agencies_in_notices[, all_docs := sum(Ndocuments), by = "GVKEY,year"]
agencies_in_notices[, frac := (Ndocuments/all_docs)^2]
agencies_in_notices <- agencies_in_notices[,list(notice_dispersion = 1 - sum(frac)),by = "GVKEY,year"]


m <- match(paste(companyyear$GVKEY, companyyear$year), paste(agencies_in_notices$GVKEY, agencies_in_notices$year))
companyyear$company_FR.frag <- agencies_in_notices$notice_dispersion[m]


### getting 10K data
agencies_in_10K <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/Sandbox/JF_1_Revision_R1_4_and_R2_2.1/agencies_in_10Ks.csv" %>% fread
agencies_in_10K <- agencies_in_10K[, list(mentions = sum(mentions)), by = "GVKEY,year,parent_agency"]

agencies_in_10K <- agencies_in_10K[, list(parent_agency = paste0(unique(parent_agency), collapse = ";"),
                                          parent_agency_mentions = paste0(mentions, collapse = ";")), 
                                   by = "GVKEY,year"]
setkey(agencies_in_10K, GVKEY, year)

m <- match(paste(companyyear$GVKEY, companyyear$year), paste(agencies_in_10K$GVKEY, agencies_in_10K$year))
companyyear$parent_agencies_10K_mention <- agencies_in_10K$parent_agency_mentions[m] 

companyyear[, agencies_10K.frag := dsp(parent_agencies_10K_mention), by = "GVKEY,year"]
companyyear$agencies_10K.frag[is.na(companyyear$agencies_10K.frag)] <- 0

##################################################################
###################### Write results  ############################
##################################################################

fwrite(companyyear, paste0(data_path, "companyyear.csv"))
saveRDS(companyyear, paste0(data_path, "companyyear.rds"))
