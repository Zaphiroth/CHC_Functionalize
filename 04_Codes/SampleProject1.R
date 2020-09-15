# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  CHC Functionalize
# Purpose:      Sample Projection
# programmer:   Zhe Liu
# Date:         2020-07-30
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


SampleProject1 <- function(total.raw, 
                           pchc.universe.m) {
  
  ##---- Universe info ----
  # projection data
  proj.data <- total.raw %>% 
    arrange(province, city, district, pchc, packid, date) %>% 
    group_by(province, city, district, pchc, packid, date) %>% 
    summarise(sales = sum(sales, na.rm = TRUE), 
              units = sum(units, na.rm = TRUE)) %>% 
    ungroup()
  
  # PCHC universe
  pchc.universe <- bind_rows(proj.data, pchc.universe.m) %>% 
    filter(province %in% c("安徽", "北京", "江苏", "山东", "浙江") | city %in% c("广州")) %>% 
    group_by(pchc) %>% 
    summarise(province = first(province),
              city = first(na.omit(city)),
              district = first(na.omit(district))) %>% 
    ungroup() %>% 
    left_join(pchc.universe.m[c('pchc', 'est')], by = 'pchc') %>% 
    filter(!is.na(est)) %>% 
    mutate(flag_sample = if_else(pchc %in% unique(proj.data$pchc), 1, 0)) %>% 
    left_join(proj.data, by = c('province', 'city', 'district', 'pchc'))
  
  
  ##---- Sample province projection ----
  # district mapping
  district.mapping <- pchc.universe %>% 
    group_by(province, city, district, pchc, flag_sample) %>% 
    summarise(est = first(est)) %>% 
    ungroup() %>% 
    group_by(province, city, district) %>% 
    summarise(est = mean(est, na.rm = TRUE),
              sample = sum(flag_sample)) %>% 
    ungroup() %>% 
    arrange(province, city, -est)
  
  district.sample <- district.mapping %>% 
    filter(sample > 0) %>% 
    select(province, sample_dist = district, sample_est = est)
  
  # pchc mapping
  pchc.mapping <- pchc.universe %>% 
    distinct(province, district, pchc, flag_sample, est)
  
  # sample pack
  sample.pack <- pchc.universe %>% 
    filter(!is.na(packid)) %>% 
    distinct(district, date, packid)
  
  # result
  proj.sample <- district.mapping %>% 
    left_join(district.sample, by = c('province')) %>% 
    mutate(est_diff = abs(est - sample_est)) %>% 
    group_by(province, city, district) %>% 
    filter(est_diff == min(est_diff)) %>% 
    ungroup() %>% 
    select(province, city, district, sample_dist) %>% 
    left_join(pchc.mapping, by = c('province', 'district')) %>% 
    left_join(sample.pack, by = c('sample_dist' = 'district')) %>% 
    left_join(pchc.universe[, c('pchc', 'packid', 'date', 'sales', 'units')], 
              by = c('pchc', 'packid', 'date')) %>% 
    mutate(sales = if_else(flag_sample == 1 & is.na(sales), 0, sales), 
           units = if_else(flag_sample == 1 & is.na(units), 0, units)) %>% 
    group_by(sample_dist, flag_sample, packid, date) %>% 
    mutate(sales_est_ratio = mean(sales, na.rm = TRUE) / mean(est, na.rm = TRUE), 
           units_est_ratio = mean(units, na.rm = TRUE) / mean(est, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(sample_dist, packid, date) %>% 
    mutate(sample_ratio = sum(flag_sample) / n(), 
           sales_est_ratio_m = first(na.omit(sales_est_ratio)), 
           units_est_ratio_m = first(na.omit(units_est_ratio))) %>% 
    ungroup() %>% 
    mutate(sales = if_else(flag_sample == 1 | sample_ratio >= 0.8, 
                           sales, 
                           est * sales_est_ratio_m), 
           units = if_else(flag_sample == 1 | sample_ratio >= 0.8, 
                           units, 
                           est * units_est_ratio_m)) %>% 
    group_by(date, province, city, district, packid, flag_sample) %>% 
    summarise(sales = sum(sales, na.rm = TRUE), 
              units = sum(units, na.rm = TRUE)) %>% 
    ungroup()
  
  
  return(proj.sample)
}


