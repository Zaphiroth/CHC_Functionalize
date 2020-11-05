# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  CHC Functionalize
# Purpose:      Review
# programmer:   Zhe Liu
# Date:         2020-10-22
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


ReviewFunc <- function(raw.total, 
                       proj.sample.total, 
                       proj.nation, 
                       market.def, 
                       target.city) {
  
  ##---- Summary ----
  raw.sum <- raw.total %>% 
    group_by(date, province, city, packid) %>% 
    summarise(raw_sales = sum(sales, na.rm = TRUE), 
              raw_units = sum(units, na.rm = TRUE)) %>% 
    ungroup()
  
  proj.sample.sum <- proj.sample.total %>% 
    group_by(date, province, city, packid) %>% 
    summarise(proj_sample_sales = sum(sales, na.rm = TRUE), 
              proj_sample_units = sum(units, na.rm = TRUE)) %>% 
    ungroup()
  
  proj.nation.sum <- proj.nation %>% 
    group_by(date, province, city, packid) %>% 
    summarise(proj_nation_sales = sum(sales, na.rm = TRUE), 
              proj_nation_units = sum(units, na.rm = TRUE)) %>% 
    ungroup()
  
  
  ##---- Projectoin rate ----
  proj.rate <- raw.sum %>% 
    full_join(proj.sample.sum, by = c('date', 'province', 'city', 'packid')) %>% 
    full_join(proj.nation.sum, by = c('date', 'province', 'city', 'packid')) %>% 
    mutate(quarter = stri_sub(date, 5, 6), 
           quarter = case_when(
             quarter %in% c('01', '02', '03') ~ 'Q1', 
             quarter %in% c('04', '05', '06') ~ 'Q2', 
             quarter %in% c('07', '08', '09') ~ 'Q3', 
             quarter %in% c('10', '11', '12') ~ 'Q4', 
             TRUE ~ NA_character_
           ), 
           quarter = stri_paste(stri_sub(date, 1, 4), quarter))
  
  return(proj.rate)
}

