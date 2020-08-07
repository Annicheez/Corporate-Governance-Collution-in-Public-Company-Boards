library('tidyverse')
library('data.table')
library('lubridate')
library('janitor')
library('moderndive')
library('stargazer')
library('plm')
library('lfe')
library('lmtest')
library('rmarkdown')

board_ex <- read_csv("C:/Users/Anas Khan/OneDrive - The University of Melbourne/
                     Desktop/Honours Materials/Research Methods/Corporate Governance_FloraKuang/Data/BoardEx. 05-19/a23d9cb34c224f97.csv") %>%
filter(!is.na(CIKCode), RowType == "Board Member") %>% mutate(fyear = year(as.Date(as.character(AnnualReportDate), format = '%Y%m%d')), CIKCode = as.numeric(CIKCode))

classified_board <- read_csv("C:/Users/Anas Khan/OneDrive - The University of Melbourne
                             /Desktop/Honours Materials/Research Methods/Corporate Governance_FloraKuang/Data/BoardEx. 05-19/129aad06f095e612.csv") %>%
mutate(fyear = year, staggered = as.factor(case_when(CBOARD == 'YES' ~ 1, TRUE ~ 0 ))) %>%
  select(cusip, fyear, staggered) %>%
  filter(!is.na(cusip))

## Create Total Annual Board Compensation Variable ##
tcomp <- board_ex %>%
group_by(fyear, CIKCode) %>%
  count(wt = TotalCompensation) %>%
  ungroup() %>%
  mutate(CIKCode = as.numeric(CIKCode))

## Update Board_ex data ##
board_ex <- board_ex %>%
  inner_join(tcomp, by = c('fyear', 'CIKCode'))


comp_crsp <- read_csv("C:/Users/Anas Khan/OneDrive - The University of Melbourne/
                      Desktop/Honours Materials/Research Methods/Corporate Governance_FloraKuang/Data/Compustat_CRSP. 05-19/a9efcf6b1dc5468e.csv") %>%
  mutate(cik = as.numeric(cik)) %>% filter(!is.na(cusip)) %>%
  inner_join(classified_board, by = c('fyear', 'cusip')) %>%
  mutate(cusip = substr(cusip, 1, 8))

execucomp <- read_csv("C:/Users/Anas Khan/OneDrive - The University of Melbourne
                      /Desktop/Honours Materials/Research Methods/Corporate Governance_FloraKuang/Data/Execucomp. 05-19/15de9ba1aa936b62.csv") 
execucomp_director <- read_csv("C:/Users/Anas Khan/OneDrive - The University of Melbourne/Desktop/
                               Honours Materials/Research Methods/Corporate Governance_FloraKuang/Data/Execucomp. 05-19/11891d6dd99d2d3a.csv") %>%
  group_by(YEAR, CUSIP) %>% mutate(annual_comp_total_sec = sum(TOTAL_SEC), 
                                   annual_comp_total_cash = sum(CASH_FEES), 
                                   annual_comp_total_rep = sum(CASH_FEES,OPTION_AWARDS,NONEQ_INCENT, OTHCOMP)) %>% ungroup() %>%
  select(YEAR, CUSIP, annual_comp_total_sec, annual_comp_total_cash, annual_comp_total_rep) %>% unique()

execucomp <- inner_join(execucomp, execucomp_director, by = c('YEAR','CUSIP')) %>%
  clean_names("snake") %>%
  filter(!is.na(becameceo), pceo == "CEO", !is.na(cusip)) %>%
  mutate(becameceo = as.Date(as.character(becameceo), format = "%Y%m%d"), 
         leftofc = as.Date(as.character(leftofc), format = "%Y%m%d"))
  

## EXCESS CEO COMPENSATION REGRESSISON ## 
  ceo_reg_dt <- inner_join(comp_crsp, execucomp, by = c("cusip", "fyear" = "year")) 
  
  ceo_reg_dt <- ceo_reg_dt %>%
  filter(!is.na(cusip) | !is.na(execid))%>%
    mutate(duality = as.character(str_detect(ceo_reg_dt$title, "Chair|chair")),
           fyear = year(as.Date(as.character(fyear), format = "%Y")),
           sic = as.factor(sic))
  ceo_reg_dt <- ceo_reg_dt %>% group_by(cusip) %>%
    mutate(prcc_ff = dplyr::lag(prcc_f), annual_ret = (prcc_f - prcc_ff)/prcc_ff, 
           sd_annual_ret = sd(annual_ret, na.rm = T)) %>%
    ungroup() %>%
    mutate(duality = case_when(
      duality == "TRUE" ~ "1",
      TRUE ~ "0"
    ), market_cap = prcc_f*csho , book_value = bkvlps*csho, tbq = market_cap/book_value,
    roa = ni/at, ocfta = oancf/at, debtr = lt/at)  
  
  ceo_reg_dt1 <- ceo_reg_dt %>%
    filter(!is.na(total_sec) | !is.na(tdc1)) %>%
    select(cusip, cik, fyear, execid, total_sec, tdc1, market_cap, tbq, annual_ret, roa, 
           sd_annual_ret, sic, sicdesc, gender, age, duality, ocfta, debtr, staggered, annual_comp_total_cash, 
           annual_comp_total_sec, annual_comp_total_rep)
  
    ceo_model_1 <- lm(total_sec ~ market_cap + tbq + annual_ret + roa + sd_annual_ret 
                      + as.factor(gender)+ age + as.factor(sic), data = ceo_reg_dt1, na.action = na.exclude) 
    ceo_model_2 <- lm(tdc1 ~ market_cap + tbq + annual_ret + roa + sd_annual_ret 
                      + as.factor(gender)+ age + as.factor(sic), data = ceo_reg_dt1, na.action = na.exclude)
    
    
    excess_ceo_comp1 <- as.vector(t(data.frame(resid(ceo_model_1))))
    excess_ceo_comp2 <- as.vector(t(data.frame(resid(ceo_model_2))))
    
    ceo_reg_dt11 <- cbind(ceo_reg_dt1, excess_ceo_comp = resid(ceo_model_1))
    ceo_reg_dt12 <- cbind(ceo_reg_dt1, excess_ceo_comp = resid(ceo_model_2))
    
    ## Create Group Variable ##
    
    board_ex_fil <- board_ex %>%
      mutate(AnnualReportDate =  as.Date(as.character(AnnualReportDate), format = "%Y%m%d"), 
             fyear = as.character(year(AnnualReportDate)), fmonth = month(AnnualReportDate), 
             cik = as.numeric(CIKCode), NED = case_when( NED == "No" ~ 0, NED == "Yes" ~ 1)) %>%
      semi_join(ceo_reg_dt, by = "cik") %>%
      clean_names("snake") %>%
      distinct(director_id, annual_report_date, cik, .keep_all = TRUE) %>%
      select(director_id, annual_report_date, fyear, cik, number_directors)
    
    nested_board_ex_fil <- board_ex_fil %>%
      group_by(cik,fyear) %>%
      nest() %>%
      arrange(fyear) %>%
      group_by(cik) %>%
      nest() %>%
      ungroup()
    
  ## Checking for data accuracy ##
    
      tf <- c(data = NA)
      count = 0
      for (i in 1:nrow(nested_board_ex_fil)) {
        for (k in 1:nrow(nested_board_ex_fil[[2]][[i]])) {
          if (c(nrow(nested_board_ex_fil[[2]][[i]][[2]][[k]]) != 
            distinct(nested_board_ex_fil[[2]][[i]][[2]][[k]]['number_directors']))){
            
            count = count + 1
            tf[count] <- c(nested_board_ex_fil[i,1])
          }
          
        }
      }
      
       inaccurate_co <- as.character(map_dbl(tf, ~.x)) %>%
         data.frame() %>%
         distinct()

## New data generation with Group variable ##
       
       majority_data = NA
       f = 0
       for (i in 1:nrow(nested_board_ex_fil)) {
         f = f + 1
         w = nrow(nested_board_ex_fil[[2]][[i]]) - 1
         prop_i = matrix(data = NA, nrow = w , 
                         ncol = w)
         if (nrow(nested_board_ex_fil[[2]][[i]]) > 1) {
           
           for (j in 1:(nrow(nested_board_ex_fil[[2]][[i]]) - 1) ) {
             
             if (j < (nrow(nested_board_ex_fil[[2]][[i]]) - 1)) {
               
               for (k in 1:(nrow(nested_board_ex_fil[[2]][[i]]) - j) ) {
                 
                 if (k == 1) {
                   
                   b = j + 1
                   directors_y0 = nested_board_ex_fil[[2]][[i]][[2]][[j]]['director_id']
                   directors_y1 = nested_board_ex_fil[[2]][[i]][[2]][[b]]['director_id']
                   common_directors = semi_join(directors_y0, 
                                                directors_y1, 
                                                by = 'director_id')
                   prop_i[j,j] = nrow(semi_join(common_directors, directors_y1, by = 'director_id')) / nrow(directors_y1)
                   
                   c = b
                   q = j
                   
                 } else if (k > 1 & k < (nrow(nested_board_ex_fil[[2]][[i]]) - j))  {
                   c = c + 1
                   q = q + 1
                   directors_y1 = nested_board_ex_fil[[2]][[i]][[2]][[c]]['director_id']
                   common_directors = semi_join(common_directors, 
                                                directors_y1, 
                                                by = 'director_id')
                   prop_i[q,j] = nrow(semi_join(common_directors, directors_y1, by = 'director_id')) / nrow(directors_y1)
                   
                   
                 } else {
                   c = c + 1
                   q = q + 1
                   directors_y1 = nested_board_ex_fil[[2]][[i]][[2]][[c]]['director_id']
                   common_directors = semi_join(common_directors, 
                                                directors_y1, 
                                                by = 'director_id')
                   prop_i[q,j] = nrow(semi_join(common_directors, directors_y1, by = 'director_id')) / nrow(directors_y1)
                   ## Mutate here ##
                   
                 }
                 ## Last j iteration ## 
                 
               } }  else {
                 
                 b = j + 1
                 directors_y0 = nested_board_ex_fil[[2]][[i]][[2]][[j]]['director_id']
                 directors_y1 = nested_board_ex_fil[[2]][[i]][[2]][[b]]['director_id']
                 common_directors = semi_join(directors_y0, 
                                              directors_y1, 
                                              by = 'director_id')
                 prop_i[j,j] = nrow(semi_join(common_directors, directors_y1, by = 'director_id')) / nrow(directors_y1)
                 
                 vec <- matrix(data = NA, nrow = (nrow(prop_i) + 1), ncol = 1)
                 
                 no_years  = 2
                 
                 if (nrow(prop_i) >= no_years) {
                   d = no_years - 1
                   for (p in 1: (nrow(prop_i) - (no_years - 2))) {
                     tt <- t(prop_i)
                     max <- max(tt[1:p, d])
                     if (max > 0.5) {
                       vec[(d + 1),1] <- 1
                     } else {
                       vec[(d + 1),1] <- 0
                     }
                     d = d + 1
                     }
                     }
                     }
                 
               }
             
           
           cc <- nested_board_ex_fil[[2]][[f]] %>% mutate(majority = vec, CIKCode = nested_board_ex_fil[f,1]$cik) %>%
             unnest()
           
           majority_data <- rbind(majority_data, cc)
         }}
         
       
       majority_data <- majority_data %>%
         filter(!is.na(majority)) %>%
         mutate(fyear = as.numeric(fyear))
       count(majority_data, majority)
       
    ## Creating Final Regression Dataframe ## 
    
       reg_dt <- inner_join(ceo_reg_dt11, majority_data, by = c('fyear', 'cik' = 'CIKCode'))
       
       bb <- board_ex %>%
         select(TotalCompensation, CIKCode, fyear)
       
       reg_dt <- inner_join(reg_dt, bb, by = c('fyear', 'cik' = 'CIKCode')) %>% mutate(duality = as.factor(duality),
                                                                                       majority = as.factor(majority)) %>%
        group_by(fyear) %>% mutate(avg_compensation_annual = mean(TotalCompensation, na.rm = T),
                                   avg_compensation_annual_sec = mean(annual_comp_total_sec, na.rm = T), 
                                   avg_compensation_annual_cash = mean(annual_comp_total_cash, na.rm = T),
                                   avg_compensation_annual_rep = mean(annual_comp_total_rep, na.rm = T)) %>% 
         ungroup() %>% distinct(fyear, cusip, .keep_all = T) %>% ungroup() %>% 
         arrange(cusip, fyear) %>% group_by(cusip) %>% mutate(roa_1 = dplyr::lag(roa), ocfta_1 = dplyr::lag(ocfta),
                                                              sd_annual_ret_1 = dplyr::lag(sd_annual_ret),
                                                              debtr_1 = dplyr::lag(debtr), excess_ceo_comp_1 = dplyr::lag(excess_ceo_comp),
                                                              avg_compensation_co_sec = mean(annual_comp_total_sec),
                                                              majority_1 = dplyr::lag(majority, 1)) %>% ungroup()
       
    ## Final Regression Models ##
       
    reg_model_boardex <- felm(TotalCompensation ~ excess_ceo_comp + staggered + roa_1 + ocfta_1 + 
                                sd_annual_ret_1 + debtr_1 + number_directors + majority + duality | sic, data = reg_dt)
    reg_model_execucomp_sec <- felm(annual_comp_total_sec ~ excess_ceo_comp + staggered + roa_1 + ocfta_1 + sd_annual_ret_1 + 
                                      debtr_1 + number_directors + majority + duality | sic, data = reg_dt)
    reg_model_execucomp_cash <- felm(annual_comp_total_cash ~excess_ceo_comp + staggered + roa_1 + ocfta_1 + sd_annual_ret_1 + debtr_1 + number_directors + majority + duality | sic, 
                              data = reg_dt)
    reg_model_execucomp_rep <- felm(annual_comp_total_rep ~excess_ceo_comp + staggered + roa_1 + ocfta_1 + sd_annual_ret_1 + debtr_1 + number_directors + majority + duality | sic, 
                           data = reg_dt)
    reg_model_execucomp_tdc <- felm(tdc1 ~excess_ceo_comp + staggered + roa_1 + ocfta_1 + sd_annual_ret_1 + debtr_1 + number_directors + majority + duality + excess_ceo_comp:majority + 
                                      staggered:majority + duality:majority| sic, 
                                    data = reg_dt)
    
summary(reg_model_execucomp_tdc)       
    ggplot(reg_dt, aes(x = fyear, y = avg_compensation_annual)) +
         geom_line()
       
    ggplot(reg_dt, aes(x = fyear, y = avg_compensation_annual_sec)) +
      geom_line()
    
    ggplot(reg_dt, aes(x = fyear, y = avg_compensation_annual_cash)) +
      geom_line()
    
    ggplot(reg_dt, aes(x = fyear, y = avg_compensation_annual_rep)) +
      geom_line()
    
    ggplot(reg_dt) + geom_line(aes(x = fyear, y = avg_compensation_co_sec, color = majority), outlier.size =NA) + facet_wrap(~majority) + 
      coord_cartesian(ylim = quantile(reg_dt$avg_compensation_co_sec, c(0.1, 0.9)))
