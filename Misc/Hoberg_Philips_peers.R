require(pacman)
p_load(data.table, lubridate, dplyr, modelsummary, lfe, stringr, tm,fixest, ggplot2, ggthemr)
path <- "/Users/evolkova/Dropbox/Projects/Govt Agenda/"

companyyear <- readRDS(paste0(path, "./Data/companyyear.rds"))
comp <- fread("/Users/evolkova/Dropbox/DataY/Compustat/crsp_compustat_1990_2021.csv", 
              select = c("GVKEY", "LPERMNO", "fyear")) %>% filter(!is.na(LPERMNO))

crsp <- fread("/Users/evolkova/Dropbox/DataY/CRSP/MSF/CRSP_MSF.csv", 
              select = c("PERMNO", "date", "RETX", "DLSTCD")) %>% 
  mutate(year = as.numeric(substr(date, 1, 4))) %>% 
  filter(year >= 1991)

crsp[, first_year := min(year), by = PERMNO]
crsp[, last_year := max(year), by = PERMNO]
crsp[, delist := DLSTCD[.N], by = PERMNO]
crsp[is.na(delist), delist := DLSTCD[which(!is.na(DLSTCD))[1]], by = PERMNO]
crsp$gvkey <- comp$GVKEY[match(crsp$PERMNO, comp$LPERMNO)]

crsp[, gvkey_year := paste(gvkey, year)]

traiding_gvkeys <- crsp[month(ymd(date)) == 1 & !is.na(as.numeric(RETX)) & !is.na(gvkey)]$gvkey_year
crsp <- crsp[,list(first_year = first_year[1], last_year = last_year[1], delist = delist[1]),by = PERMNO]
crsp$gvkey <- comp$GVKEY[match(crsp$PERMNO, comp$LPERMNO)]

tnic3 <- fread("/Users/evolkova/Dropbox/Projects/Govt Agenda/Sandbox/20221027/tnic3_data.txt", sep = "\t") %>% 
  filter(year %in% 1994:2022) %>% 
  filter(!is.na(score)) %>% 
  filter(gvkey1 %in% companyyear$GVKEY) 

setkey(tnic3, gvkey1, year, gvkey2)
peers <- tnic3[,list(all_peers = paste0(gvkey2, collapse = ","), n_peers = length(unique(gvkey2))), by = "gvkey1,year"]
setkey(peers, gvkey1, year)

peers[, lag.all_peers := shift(all_peers, n = 1, type = "lag"), by = "gvkey1"]
peers[, lead.all_peers := shift(all_peers, n = 1, type = "lead"), by = "gvkey1"]

find.new <- function(x, y){
  x <- strsplit(x, split = ",") %>% unlist
  y <- strsplit(y, split = ",") %>% unlist
  out <- x[!(x %in% y)]
  out <- paste0(out, collapse = ",")
  return(out)
}

### detecting new peers and ipos
peers[, new_peers := find.new(all_peers, lag.all_peers), by = "gvkey1,year"]
peers[, left_peers := find.new(all_peers, lead.all_peers), by = "gvkey1,year"]
peers[, gvkey_year := paste(gvkey1,year)]

tmp <- peers %>% select("gvkey1", "year", "left_peers")
tmp <- tmp[, list(left_peers = unlist(str_split(left_peers, ","))), by = "gvkey1,year"]
tmp[, gvkey_left_peers := paste(left_peers, year)]
tmp[, trading := 0]
tmp[gvkey_left_peers %in% traiding_gvkeys, trading := 1]
tmp <- tmp[,list(n_peers_left_trading = sum(trading)), by = "gvkey1,year"]

peers$n_peers_left_trading <- tmp$n_peers_left_trading[match(paste(tmp$gvkey1, tmp$year), peers$gvkey_year)]

peers[, n_new_peers := str_count(new_peers, ",")]
peers[str_length(new_peers) > 0, n_new_peers := n_new_peers + 1]

find_new <- peers[year > 1993] %>% select("gvkey1","year","new_peers") 
find_new <- find_new[,list(gvkey2 = unlist(strsplit(new_peers,","))), by = "gvkey1,year"]
m <- match(find_new$gvkey2, crsp$gvkey)
find_new$first_year <- crsp$first_year[m]
find_new$ipo <- 0
find_new[(year == first_year)|(year == first_year + 1), ipo := 1]
find_new[, all_ipo := sum(ipo), by = "gvkey1,year"]
peers$n_ipo <- find_new$all_ipo[match(paste(peers$gvkey1,peers$year),paste(find_new$gvkey1, find_new$year))]

### detecting leaving peers and delisting
peers[, leaving_peers := find.new(lag.all_peers, all_peers), by = "gvkey1,year"]
peers[, n_leaving_peers := str_count(leaving_peers, ",")]
peers[str_length(leaving_peers) > 0, n_leaving_peers := n_leaving_peers + 1]

find_last <- peers %>% select("gvkey1", "year", "leaving_peers")
find_last <- find_last[,list(gvkey2 = unlist(strsplit(leaving_peers,","))), by = "gvkey1,year"]
m <- match(find_last$gvkey2, crsp$gvkey)
find_last$last_year <- crsp$last_year[m]
find_last$delist <- crsp$delist[m]
find_last <- find_last[(year - 1) == last_year|(year) == last_year]
find_last[, n_delist := sum(delist > 199), by = "gvkey1,year"]
find_last[, `:=`(poor_perf = 0, acq = 0)]
find_last[delist %in% 400:599, poor_perf := 1]
find_last[delist %in% 200:399, acq := 1]
find_last <- find_last[, list(n_poor_perf = sum(poor_perf), n_acq = sum(acq),n_delist = n_delist[1]), by = "gvkey1,year"]

m <- match(paste(peers$gvkey1,peers$year),paste(find_last$gvkey1, find_last$year))
peers$n_poor_perf <- find_last$n_poor_perf[m]
peers$n_acq <- find_last$n_acq[m]
peers$n_delist <- find_last$n_delist[m]

vars_need <- c("n_peers", "n_new_peers", "n_ipo" ,"n_leaving_peers", "n_poor_perf", "n_acq","n_delist", "n_peers_left_trading")
out <- peers %>% select("gvkey1", "year", vars_need)
fwrite(out, "/Users/evolkova/Dropbox/Projects/Govt Agenda/Data/HP_peers.csv")
