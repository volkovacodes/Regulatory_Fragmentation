require(data.table)
require(lubridate)
require(dplyr)
require(tm)

na0 <- function(x) {
  x[is.na(x)] <- 0
  return(x)
}

data_path <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/"
outpath <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/Sandbox/20201029/temp_files/"

##################################################################################
#################### LOAD LDA COMPANY DATA #######################################
##################################################################################

companyyear <- NULL
for(yr in 1994:2019)
{
  master <- NULL
  for(i in 1:4)
  {
    master <- master %>%
      rbind(fread(paste0("/Users/evolkova/Dropbox/SEC Filings/10-K/Master with Compustat/master_",yr,i,".csv")))
  }
  
  master <- master %>%
    select("cik", "name", "file", "GVKEY", "conm", "at", "sic")
  
  master$year <- yr
  ldacompanies <- paste0("/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/LDA_Data/LDA10K/lda_random_sample_topics100_10K",yr,".csv") %>%
    fread
  
  dtmcompanies <- paste0("/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/LDA_Data/LDA10K/dtm_random_sample_topics100_10K",yr,".rds") %>%
    readRDS
  
  #dtmcompanies <- Docum
  words <- rowSums(as.matrix(dtmcompanies))
  
  m <- match(paste0(master$file, ".htm"), ldacompanies$filename)
  master <- cbind(master, ldacompanies[m])
  m <- match(paste0(master$file, ".htm"), dtmcompanies$dimnames$Docs)
  master$Words10K <- words[m]
  master <- master[!is.na(at) & !is.na(Topic1)& !is.na(sic)]
  
  companyyear <- rbind(companyyear, master)
}

setkey(companyyear, cik, year)
companyyear <- companyyear[!duplicated(paste0(cik, year))]

##################################################################################
############## LOAD COMPUSTAT/CRSP COMPANY DATA ##################################
##################################################################################

crsp <- "/Users/evolkova/Dropbox/DataY/CRSP/MSF/CRSP_MSF.csv" %>%
  fread

crsp <- crsp[date > 19900101]
crsp[, date := ymd(date)]
crsp <- crsp[month(date) == 12]
crsp[, year := year(date)] 
crsp[, size := abs(PRC)*SHROUT]


comp <- "/Users/evolkova/Dropbox/DataY/Compustat/crsp_compustat_1990_2019.csv" %>%
  fread

comp <- comp[!is.na(LPERMNO)]
comp <- comp[ fyr > 0 & at > 0]
comp[, year := fyear]
comp[fyr %in% 1:5, year := fyear + 1]

### matching size
m <- match(paste(comp$LPERMNO, comp$year), paste(crsp$PERMNO, crsp$year - 1))
comp$size <- crsp$size[m]

### calculating book
comp[, pstock := pstkl]
comp[is.na(pstock), pstock := pstkrv]
comp[is.na(pstock), pstock := pstk]
comp[is.na(pstock), pstock := 0] ### is it right?
comp[, se := seq]
comp[is.na(se)&!is.na(ceq)&!is.na(pstk), se := ceq + pstk]
comp[is.na(se), se := at - lt]
comp[is.na(txditc), txditc := 0]
comp[is.na(dcvt), dcvt := 0]
comp[, be := se - pstock + txditc + dcvt]

### market-to-book
comp[, MB := (size/10^3)/(be)]

### ppe to assets
comp[, ppe_at := ppent/at]

### ebitda to assets
comp[, ebitda_at := ebitda/at]

### sga to assets
comp[, sga := xsga]
comp[, sga_at := xsga/at]

### employment to sales 
comp[, emp_sale := emp/sale]
comp[, emp_at := emp/at]

### sales growth
setkey(comp, GVKEY, year)

comp[, lag.sale := shift(sale, 1, type = "lag"), by = "GVKEY"]
comp[, growth := 100*(sale/lag.sale - 1)]

### R&D
comp[is.na(xrd), xrd := 0]

### capx_at
comp[, capx_at := capx/at]
comp[, inv_at := (capx + xrd)/at]
comp[, rd_at := (xrd)/at]

### tobin's q
comp[, tobin := (at - ceq + size/10^3)/at]
#comp[, tobin := (at - ceq - n0(txdb) + marcap)/at] alternative definition in some other papers

### free cash flows
comp[, fcf := (ib + na0(dp))/at]

### leverage
comp[, leverage := (na0(dltt) + na0(dlc))/at]

### return on assets
comp[, roa := ib/at]

### cash
comp[, cash_at := che/at]
comp[, cash := che]

### assets 
comp[, lag.at := shift(at, 1, type = "lag"), by = "GVKEY"]
comp[, growth.at := 100*(at/lag.at - 1)]

vars <- c("sale", "growth", "MB", "ppe_at", "ebitda_at", "sga", "sga_at" ,"emp_sale","emp_at" ,"xrd", 
          "capx_at", "inv_at", "fcf", "tobin", "leverage", "roa", "cash" ,"cash_at", "growth.at")

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

##################################################################################
################### LOAD COMPUSTAT SEGMENT #######################################
##################################################################################

compseg <- "/Users/evolkova/Dropbox/DataY/Compustat/compustat_segments_1990.csv" %>% 
  fread(select = c("stype", "gvkey", "sid", "datadate"))
compseg <- compseg[stype == "BUSSEG"]
compseg[, year := year(ymd(datadate))]
setkey(compseg, gvkey, year, sid)

compseg <- compseg[, list(Nseg = length(unique(sid))), by = "gvkey,year"]

m <- match(paste(companyyear$GVKEY, companyyear$year), paste(compseg$gvkey, compseg$year))
companyyear$Nsegments <- compseg$Nseg[m]
companyyear$Nsegments[is.na(companyyear$Nsegments)] <- 1

##################################################################################
#################@@@## ADD INDUSTRIES INFO #######################################
##################################################################################

ffmap <- "/Users/evolkova/Dropbox/DataY/SIC_to_Fama_French_industry.csv" %>% fread
m <- match(companyyear$sic, ffmap$SIC0)
companyyear$FF12 <- ffmap$FF_12[m]
companyyear$FF48 <- ffmap$FF_48[m]

##################################################################################
###################### ADD Productivity ##########################################
##################################################################################

tfp <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/TFPData_updated_ImrohorogluTuzel.csv"  %>%
  fread

m <- match(paste(companyyear$GVKEY, companyyear$year), paste(tfp$gvkey, tfp$fyear))
companyyear$tfp <- tfp$TFP[m]

m <- match(paste(companyyear$GVKEY, companyyear$year + 1), paste(tfp$gvkey, tfp$fyear))
companyyear$lead.tfp <- tfp$TFP[m]
##################################################################################
################# ADD HHI and Other Measures #####################################
##################################################################################
company_year_topic.melt <- companyyear %>% 
  select("cik", "year", paste0("Topic", 1:100)) %>%
  melt(id.vars = c("cik", "year"))

company_year_topic.melt[, topic_year := paste(variable, year)]

### create topic-firm topic dispersion
data <- company_year_topic.melt[,list(topic.disp = 1- sum(value^2)), by = c("cik,year")]

m <- match(paste(companyyear$cik, companyyear$year),
           paste(data$cik, data$year))
companyyear$topic.disp <- data[m]$topic.disp

### create firmagencyA and firmagencyB
getdata <- function(filename){
  topicstoagencies <- data_path %>%
    paste0(filename) %>%
    fread
  
  data <- topicstoagencies[, list(HHI = sum(AgencyPercent^2), Words = AllWords[1]), 
                           by = c("year,TopicNumber")]
  data[, topic_year := paste(paste0("Topic",TopicNumber), year)]
  m <- match(company_year_topic.melt$topic_year, data$topic_year)
  return(data[m])
}

### one
data <- getdata("topicagencyyear.csv")
company_year_topic.melt$hhi_topic_agency <- data$HHI
company_year_topic.melt$fedreg_topic_words <- data$Words

### two 
data <- getdata("topicagencyyear_Notice.csv")
company_year_topic.melt$hhi_topic_agency_Notice <- data$HHI
company_year_topic.melt$fedreg_topic_words_Notice <- data$Words

### three
data <- getdata("topicagencyyear_Proposed Rule.csv")
company_year_topic.melt$hhi_topic_agency_PRule <- data$HHI
company_year_topic.melt$fedreg_topic_words_PRule <- data$Words

### four
data <- getdata("topicagencyyear_Rule.csv")
company_year_topic.melt$hhi_topic_agency_Rule <- data$HHI
company_year_topic.melt$fedreg_topic_words_Rule <- data$Words

x <- company_year_topic.melt %>% select("hhi_topic_agency", "hhi_topic_agency_Notice", 
                                        "hhi_topic_agency_PRule", "hhi_topic_agency_Rule")
View(cor(x, use = "complete.obs"))

measures <- company_year_topic.melt[, list(regul.disp = 1 - sum(value*hhi_topic_agency),
                                           #regul.disp2 = 1 - sum(value^2*hhi_topic_agency),
                                           regul.complex = sum(value*fedreg_topic_words),
                                           regul.complex.log = sum(value*log(fedreg_topic_words)),
                                           regul.disp_Notice = 1 - sum(value*hhi_topic_agency_Notice),
                                           regul.complex_Notice = sum(value*fedreg_topic_words_Notice),
                                           regul.complex_Notice.log = sum(value*log(fedreg_topic_words_Notice)),
                                           regul.disp_PRule = 1 - sum(value*hhi_topic_agency_PRule),
                                           regul.complex_PRule = sum(value*fedreg_topic_words_PRule),
                                           regul.complex_PRule.log = sum(value*log(fedreg_topic_words_PRule)),
                                           regul.disp_Rule = 1 - sum(value*hhi_topic_agency_Rule),
                                           regul.complex_Rule = sum(value*fedreg_topic_words_Rule),
                                           regul.complex_Rule.log = sum(value*log(fedreg_topic_words_Rule))), by = "cik,year"]


m <- match(paste(companyyear$cik, companyyear$year), 
           paste(measures$cik, measures$year))

measures <- measures[m] %>% select("regul.disp", "regul.complex", "regul.complex.log",
                                   "regul.disp_Notice", "regul.complex_Notice", "regul.complex_Notice.log",
                                   "regul.disp_PRule", "regul.complex_PRule", "regul.complex_PRule.log",
                                   "regul.disp_Rule", "regul.complex_Rule", "regul.complex_Rule.log")

companyyear <- companyyear %>%
  cbind(measures)

##################################################################
###################### Adding Groups #############################
##################################################################
require(xlsx)

groups <- read.xlsx2("/Users/evolkova/Dropbox/Projects/Govt Agenda/Sandbox/20210128/topic_labels_ML_KV_JK.xlsx", 1) %>%
  as.data.table
groups[, TopicNumber := paste0("Topic", Topic)]



group_measures <- function(gr.col)
{
  
  ### first we find a group column
  group.col <- which(colnames(groups) == gr.col)
  
  
  ### then we re-estimate topic HHI
  topicstoagencies <- data_path %>%
    paste0("topicagencyyear.csv") %>%
    fread
  
  m <- match(topicstoagencies$TopicNumber, groups$Topic)
  topicstoagencies$group <- groups[[group.col]][m]
  topicstoagencies <- topicstoagencies[, list(GroupWords = sum(TopicWords, na.rm = T)), by = "group,year,agency"]
  topicstoagencies[, AllWords := sum(GroupWords, na.rm = T), by = "group,year"]
  topicstoagencies[, GroupPercent := GroupWords/AllWords]
  
  data <- topicstoagencies[, list(HHI = sum(GroupPercent^2, na.rm = T), Words = AllWords[1]), by = c("year,group")]


  
  m <- match(company_year_topic.melt$variable, groups$TopicNumber)
  company_year_topic.melt$group <- groups[[which(colnames(groups) == gr.col)]][m]
  
  company_year_group.melt <- company_year_topic.melt[, list(value = sum(value, na.rm = T)), 
                                                     by = "cik,year,group"] 
  
  m <- match(paste(company_year_group.melt$year, company_year_group.melt$group),
             paste(data$year,data$group))

  company_year_group.melt$HHI <- data$HHI[m]
  company_year_group.melt$fedreg_group_words <- data$Words[m]
  
  measures <- company_year_group.melt[, list(regul.disp = 1 - sum(value*HHI, na.rm = T),
                                             regul.complex.log = sum(value*log(fedreg_group_words), na.rm = T)), by = "cik,year"]
  ### return properly matched data
  
  m <- match(paste(companyyear$cik, companyyear$year), 
             paste(measures$cik, measures$year))
  
  return(measures[m])
}

### my groups
measures <- "Group_KV" %>%
  group_measures

companyyear$regul.disp.group.KV <- measures$regul.disp
companyyear$regul.complex.log.group.KV <- measures$regul.complex.log

### Joseph groups2
measures <- "Group_JK2" %>%
  group_measures

companyyear$regul.disp.group.JK <- measures$regul.disp
companyyear$regul.complex.log.group.JK <- measures$regul.complex.log

fwrite(companyyear, paste0(data_path,"companyyear.csv"))
saveRDS(companyyear, paste0(data_path,"companyyear.rds"))


