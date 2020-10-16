# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  CHC Functionalize
# Purpose:      Sample Projection
# programmer:   Zhe Liu
# Date:         2020-07-16
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


SampleProject <- function(total.raw, 
                          pchc.universe.m, 
                          target.city, 
                          market.def) {
  
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
    filter(city != '上海') %>% 
    group_by(pchc) %>% 
    summarise(province = first(province),
              city = first(na.omit(city)),
              district = first(na.omit(district))) %>% 
    ungroup() %>% 
    left_join(pchc.universe.m[c('pchc', 'est')], by = 'pchc') %>% 
    mutate(flag_sample = if_else(pchc %in% unique(proj.data$pchc), 1, 0)) %>% 
    left_join(proj.data, by = c('province', 'city', 'district', 'pchc'))
  
  
  ##---- Projectoin ----
  # pchc est ratio
  pchc.ratio <- pchc.universe %>% 
    filter(flag_sample == 1) %>% 
    filter(!is.na(packid)) %>% 
    filter(!is.na(est)) %>% 
    group_by(city, district, pchc, packid, date) %>% 
    summarise(sales_est_ratio = sum(sales, na.rm = TRUE) / 
                sum(est, na.rm = TRUE), 
              units_est_ratio = sum(units, na.rm = TRUE) / 
                sum(est, na.rm = TRUE)) %>% 
    ungroup()
  
  # pchc mapping
  pchc.mapping <- pchc.universe %>% 
    filter(flag_sample == 1) %>% 
    group_by(province, city, district, sample_pchc = pchc) %>% 
    summarise(sample_est = first(na.omit(est))) %>% 
    ungroup() %>% 
    filter(!is.na(sample_est)) %>% 
    arrange(province, city, district, -sample_est)
  
  # district est ratio
  district.ratio <- pchc.universe %>% 
    filter(!is.na(packid)) %>% 
    filter(!is.na(est)) %>% 
    group_by(city, district, packid, date) %>% 
    summarise(sales_est_ratio = sum(sales, na.rm = TRUE) / 
                sum(est, na.rm = TRUE), 
              units_est_ratio = sum(units, na.rm = TRUE) / 
                sum(est, na.rm = TRUE)) %>% 
    ungroup()
  
  # district mapping
  district.mapping <- pchc.universe %>% 
    group_by(province, city, district, pchc, flag_sample) %>% 
    summarise(est = first(na.omit(est))) %>% 
    ungroup() %>% 
    filter(!is.na(est)) %>% 
    group_by(province, city, district) %>% 
    summarise(est = mean(est, na.rm = TRUE), 
              sample = sum(flag_sample)) %>% 
    ungroup() %>% 
    arrange(province, city, -est)
  
  district.sample <- district.mapping %>% 
    filter(sample > 0) %>% 
    select(province, city, sample_dist = district, sample_est = est)

  district.nonsample <- district.mapping %>% 
    left_join(district.sample, by = c('province', 'city')) %>% 
    mutate(est_diff = abs(est - sample_est)) %>% 
    group_by(province, city, district) %>% 
    filter(est_diff == min(est_diff)) %>% 
    ungroup() %>% 
    select(province, city, district, sample_dist)
  
  # projection
  proj.nonsample <- pchc.universe %>% 
    filter(flag_sample == 0) %>% 
    group_by(province, city, district, pchc, flag_sample) %>% 
    summarise(est = first(na.omit(est))) %>% 
    ungroup() %>% 
    left_join(district.nonsample, by = c('province', 'city', 'district')) %>% 
    filter(!is.na(sample_dist)) %>% 
    left_join(pchc.mapping, by = c('province', 'city', 
                                   'sample_dist' = 'district')) %>% 
    mutate(est_diff = abs(est - sample_est)) %>% 
    group_by(province, city, district, pchc, flag_sample) %>% 
    filter(est_diff == min(est_diff)) %>% 
    ungroup() %>% 
    select(-sample_est, -est_diff) %>% 
    left_join(pchc.ratio, by = c('city', 'sample_dist' = 'district', 
                                 'sample_pchc' = 'pchc'))
  
  proj.result <- proj.data %>% 
    mutate(flag_sample = 1, 
           sample_dist = district) %>% 
    bind_rows(proj.nonsample) %>% 
    group_by(sample_dist, packid, date) %>% 
    mutate(sample_ratio = sum(flag_sample) / n()) %>% 
    ungroup() %>% 
    mutate(sales = if_else(flag_sample == 1 | sample_ratio >= 0.8, 
                           sales, 
                           est * sales_est_ratio), 
           units = if_else(flag_sample == 1 | sample_ratio >= 0.8, 
                           units, 
                           est * units_est_ratio)) %>% 
    filter(sales > 0, units > 0) %>% 
    filter(city != '上海') %>% 
    group_by(date, province, city, district, packid, flag_sample) %>% 
    summarise(sales = sum(sales, na.rm = TRUE), 
              units = sum(units, na.rm = TRUE)) %>% 
    ungroup()
  
  
  ##---- Projection rate ----
  proj.chk <- proj.data %>% 
    mutate(flag_sample = 1, 
           sample_dist = district) %>% 
    bind_rows(proj.nonsample) %>% 
    inner_join(market.def, by = 'packid') %>% 
    group_by(sample_dist, packid, date) %>% 
    mutate(sample_ratio = sum(flag_sample) / n()) %>% 
    ungroup() %>% 
    mutate(sales = if_else(flag_sample == 1 | sample_ratio >= 0.8, 
                           sales, 
                           est * sales_est_ratio), 
           units = if_else(flag_sample == 1 | sample_ratio >= 0.8, 
                           units, 
                           est * units_est_ratio)) %>% 
    filter(sales > 0, units > 0) %>% 
    filter(city != '上海') %>% 
    group_by(city, district, market) %>% 
    summarise(proj = sum(sales, na.rm = TRUE)) %>% 
    ungroup()
  
  raw.chk <- proj.data %>% 
    inner_join(market.def, by = 'packid') %>% 
    group_by(city, district, market) %>% 
    summarise(raw = sum(sales, na.rm = TRUE)) %>% 
    ungroup()
  
  proj.rate <- proj.data %>% 
    mutate(flag_sample = 1, 
           sample_dist = district) %>% 
    bind_rows(proj.nonsample) %>% 
    distinct(city, district, pchc, flag_sample) %>% 
    group_by(city, district) %>% 
    summarise(sample_n = sum(flag_sample), 
              total_n = n()) %>% 
    ungroup() %>% 
    mutate(ratio_hosp = sample_n / total_n) %>% 
    merge(data.frame(market = c('HTN', 'IHD', 'OAD'))) %>% 
    left_join(raw.chk, by = c('city', 'district', 'market')) %>% 
    left_join(proj.chk, by = c('city', 'district', 'market')) %>% 
    mutate(raw = if_else(is.na(raw), 0, raw), 
           proj = if_else(is.na(proj), raw, proj), 
           ratio_sales = proj / raw, 
           mean_raw = round(raw / sample_n / 1000000, 2), 
           mean_proj = round((proj - raw) / (total_n - sample_n) / 1000000, 2))
  
  
  return(
    list(proj.result = proj.result, 
         proj.rate = proj.rate)
  )
}
















