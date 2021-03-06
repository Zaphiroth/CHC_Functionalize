# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  CHC Functionalize
# Purpose:      Sample Projection
# programmer:   Zhe Liu
# Date:         2020-07-30
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


ProjectSample <- function(raw.total, 
                          pchc.universe) {
  
  ##---- Universe info ----
  # projection data
  proj.data <- raw.total %>% 
    arrange(province, city, district, pchc, packid, date) %>% 
    group_by(province, city, district, pchc, packid, date) %>% 
    summarise(sales = sum(sales, na.rm = TRUE), 
              units = sum(units, na.rm = TRUE)) %>% 
    ungroup()
  
  out.data <- proj.data %>% 
    filter(!(pchc %in% pchc.universe$pchc)) %>% 
    mutate(flag_sample = 1)
  
  # PCHC universe
  # pchc.universe <- bind_rows(proj.data, pchc.universe.m) %>% 
  #   filter(city %in% unique(proj.data$city)) %>% 
  #   group_by(pchc) %>% 
  #   summarise(province = first(province),
  #             city = first(na.omit(city)),
  #             district = first(na.omit(district))) %>% 
  #   ungroup() %>% 
  #   left_join(pchc.universe.m[c('pchc', 'est')], by = 'pchc') %>% 
  #   filter(!is.na(est)) %>% 
  #   mutate(flag_sample = if_else(pchc %in% unique(proj.data$pchc), 1, 0)) %>% 
  #   left_join(proj.data, by = c('province', 'city', 'district', 'pchc'))
  
  
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
  # pchc.mapping <- pchc.universe %>% 
  #   distinct(province, district, pchc, flag_sample, est)
  
  # sample pack
  sample.pack <- proj.data %>% 
    filter(!is.na(packid)) %>% 
    distinct(district, date, packid)
  
  # result
  proj.sample <- district.mapping %>% 
    # sample district
    left_join(district.sample, by = c('province')) %>% 
    mutate(est_diff = abs(est - sample_est)) %>% 
    group_by(province, city, district) %>% 
    filter(est_diff == min(est_diff)) %>% 
    ungroup() %>% 
    select(province, city, district, sample_dist) %>% 
    # sample flag
    left_join(pchc.universe, by = c('province', 'city', 'district')) %>% 
    # sample pack
    left_join(sample.pack, by = c('sample_dist' = 'district')) %>% 
    # sample data
    left_join(proj.data, by = c('province', 'city', 'district', 'pchc', 'packid', 'date')) %>% 
    mutate(sales = if_else(flag_sample == 1 & is.na(sales), 0, sales), 
           units = if_else(flag_sample == 1 & is.na(units), 0, units)) %>% 
    # est ratio
    group_by(sample_dist, flag_sample, packid, date) %>% 
    mutate(sales_est_ratio = mean(sales, na.rm = TRUE) / mean(est, na.rm = TRUE), 
           units_est_ratio = mean(units, na.rm = TRUE) / mean(est, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(sample_dist, packid, date) %>% 
    mutate(sample_ratio = sum(flag_sample) / n(), 
           sales_est_ratio_m = first(na.omit(sales_est_ratio)), 
           units_est_ratio_m = first(na.omit(units_est_ratio))) %>% 
    ungroup() %>% 
    # result
    mutate(sales = if_else(flag_sample == 1 | sample_ratio >= 0.8, 
                           sales, 
                           est * sales_est_ratio_m), 
           units = if_else(flag_sample == 1 | sample_ratio >= 0.8, 
                           units, 
                           est * units_est_ratio_m)) %>% 
    # summary
    bind_rows(out.data) %>% 
    group_by(date, province, city, district, packid, flag_sample) %>% 
    summarise(sales = sum(sales, na.rm = TRUE), 
              units = sum(units, na.rm = TRUE)) %>% 
    ungroup()
  
  
  return(proj.sample)
}


