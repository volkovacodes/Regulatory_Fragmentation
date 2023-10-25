win <- function(x, eps = 0.005){
  up <- quantile(x, na.rm = T, 1 - eps)
  down <- quantile(x, na.rm = T, eps)
  x[x>up] <- up
  x[x<down] <- down
  return(x)
}

ms <- function(x){
  out <- modelsummary(x, stars = c('*' = .1, '**' = 0.05 ,'***' = .01),
               gof_omit = 'DF|Deviance|AIC|BIC|Log.Lik|R2 Within|R2 Pseudo',
               coef_omit = "(Intercept)|topic|Log|ppe|ebitda|sale|complex|tobin|segm")  
  return(out)
}

ms_all <- function(x){
  out <- modelsummary(x, stars = c('*' = .1, '**' = 0.05 ,'***' = .01),
                      gof_omit = 'DF|Deviance|AIC|BIC|Log.Lik|R2 Within|R2 Pseudo')  
  return(out)
}

nanna <- function(x) {
  x[is.infinite(x)|is.nan(x)] <- NA
  return(x)
}

nm <- function(x){
  x[is.infinite(x)|is.nan(x)] <- NA
  x <- (x - mean(x, na.rm = T))/sd(x, na.rm = T)
  return(x)
}

prep_cy <- function(data_path){
  ### load company year data
  companyyear <- "companyyear.rds" %>% 
    paste0(data_path,.) %>%
    readRDS

  companyyear[, `:=`(log_sale = log(1 + sale), 
                   fcf_at = fcf/at,
                   LogWords10K = log(Words10K),
                   lead.n_join = lead.n_ipo - lead.n_delist)]

  vars <- c("log_sale", "ppe_at", "ebitda_at", "sale",
          "growth", "tobin", 
          "regul.disp", "regul.complex.log", "topic.disp",
          "topic.disp", "LogWords10K", 
          "lead.sga_at", "lead.capx_at", "lead.inv_at", "lead.emp_at", "lead.n_join",
          "lead.leverage", "lead.growth", "lead.roa", "lead.cash_at", "lead.growth.at") 



  col <- which(colnames(companyyear) %in% vars)
  ind <- companyyear$sale > 0

  for(c in col) {
    ind <- ind & !is.na(companyyear[[c]]) & !is.nan(companyyear[[c]]) & !is.infinite(companyyear[[c]])
  }


  companyyear <- companyyear[ind]
  
  companyyear[, lead.roa := 100*lead.roa]
  companyyear[, roa := 100*roa]
  
  ### we use these variables only in summary table
  ### but we still need to winsorize them
  companyyear$Nsegments <- win(companyyear$Nsegments)
  companyyear$MB <- win(companyyear$MB)
  companyyear$growth <- win(companyyear$growth)
  companyyear$growth.at <- win(companyyear$growth.at)
  companyyear$sga_at <- win(companyyear$sga_at)
  companyyear$emp_at <- win(companyyear$emp_at)
  companyyear$inv_at <- win(companyyear$inv_at)
  companyyear$roa <- win(companyyear$roa)

  mains <- c("regul.disp", "topic.disp", "regul.complex.log")
  
  cols <- c(vars, "lead.tfp", "lead.n_left", "agencies_10K.frag", "company_FR.frag",
            "Nsegments", "lead.log_lobby", "lead.lobby",
            "lead.n_ipo","lead.n_peers", #"lead.n_join", 
            "lead.n_leaving_large_at_peers", 
            "lead.n_leaving_small_at_peers")
  
  for(c in col) companyyear[[c]] <- win(companyyear[[c]])

  companyyear[is.nan(lead.n_left) | is.infinite(lead.n_left), lead.n_left := NA]
  

  cols <- c(cols,
            paste0(mains, "_Notice"), paste0(mains, "_PRule"), paste0(mains, "_Rule"),
            paste0(mains, "_RIN"), paste0(mains, "_old_RIN"), paste0(mains, "_57"),
            paste0(mains, "_57"), paste0(mains, "_70"), paste0(mains, "_200"), 
            paste0(mains, "_300"), paste0(mains, "_1000"), paste0(mains, "_item7"), paste0(mains, "_item1"),
            paste0(mains, "_main"), paste0(mains, "_no3"), paste0(mains, "_rolling5"), paste0(mains, "_rolling10"))
  for(cl in cols) {
    x <- companyyear[[cl]]
    x <- (x - mean(x, na.rm = T))/sd(x, na.rm = T)
    nm <- paste0(cl, "_norm")
    companyyear[, (nm) := x]
  }
  
  companyyear[, FF48_year := paste(FF48, year)]


  return(companyyear)
}
 