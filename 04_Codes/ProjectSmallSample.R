# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  CHC Functionalize
# Purpose:      Shanghai Projection
# programmer:   Zhe Liu
# Date:         2020-08-04
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


ProjectSmallSample <- function(raw.total, 
                               pchc.info, 
                               small = '上海', 
                               sample = c('上海', '北京')) {
  
  ##---- Shanghai info ----
  # Shanghai PCHC
  # pchc.sh.m <- pchc.universe %>% 
  #   filter(`地级市` %in% c('上海')) %>% 
  #   group_by(pchc = PCHC_Code) %>% 
  #   summarise(province = first(na.omit(`省`)),
  #             city = first(na.omit(`地级市`)),
  #             district = first(na.omit(`区[县/县级市】`)),
  #             pop = first(na.omit(`人口`)),
  #             pop1 = first(na.omit(`其中：0-14岁人口数`)),
  #             pop2 = first(na.omit(`15-64岁人口数`)),
  #             pop3 = first(na.omit(`65岁及以上人口数`)),
  #             doc = first(na.omit(`2016年执业医师（助理）人数`)),
  #             pat = first(na.omit(`2016年总诊疗人次数`)),
  #             inc = first(na.omit(`2016年药品收入（千元）`)),
  #             est = first(na.omit(`其中：西药药品收入（千元）`))) %>% 
  #   ungroup() %>% 
  #   filter(!is.na(pop), !is.na(pop1), !is.na(pop2), !is.na(pop3), !is.na(doc), 
  #          !is.na(pat), !is.na(inc), !is.na(est))
  
  # pchc.sh <- bind_rows(raw.total, pchc.sh.m) %>% 
  #   filter(city %in% c('上海')) %>% 
  #   group_by(pchc) %>% 
  #   summarise(province = first(na.omit(province)), 
  #             city = first(na.omit(city)), 
  #             district = first(na.omit(district)), 
  #             pop = sum(pop, na.rm = TRUE), 
  #             pop1 = sum(pop1, na.rm = TRUE),  
  #             pop2 = sum(pop2, na.rm = TRUE), 
  #             pop3 = sum(pop3, na.rm = TRUE), 
  #             doc = sum(doc, na.rm = TRUE), 
  #             pat = sum(pat, na.rm = TRUE), 
  #             inc = sum(inc, na.rm = TRUE), 
  #             est = sum(est, na.rm = TRUE)) %>% 
  #   ungroup() %>% 
  #   filter(pop > 0, est > 0) %>% 
  #   mutate(flag_sample = if_else(pchc %in% unique(raw.total$pchc), 1, 0))
  
  pchc.sh <- pchc.info %>% 
    filter(province %in% sample)
  
  # partition
  pchc.sh.sample <- pchc.sh %>% 
    filter(flag_sample == 1)
  
  pchc.sh.nonsample <- pchc.sh %>% 
    filter(flag_sample == 0, province %in% small)
  
  
  ##---- K-NN model ----
  # set
  train.set <- pchc.sh.sample[, 5:13]
  test.set <- pchc.sh.nonsample[, 5:13]
  
  # model
  knn.model <- kknn(flag_sample ~ ., train = train.set, test = test.set, 
                    k = 3, scale = TRUE)
  
  # weighting
  knn.indice <- as.data.frame(knn.model$C) %>% 
    lapply(function(x) {
      pchc.sh.sample$pchc[x]
    }) %>% 
    as.data.frame(col.names = c("pchc_1", "pchc_2", "pchc_3")) %>% 
    bind_cols(pchc.sh.nonsample[, c("pchc")]) %>% 
    pivot_longer(cols = c(pchc_1, pchc_2, pchc_3), 
                 names_to = 'knn_level', 
                 values_to = 'knn_pchc', 
                 values_drop_na = FALSE)
  
  knn.weight <- as.data.frame(knn.model$D) %>% 
    lapply(function(x) {
      1 / (x + 1)
    }) %>% 
    as.data.frame(col.names = c("pchc_1", "pchc_2", "pchc_3")) %>% 
    mutate(weight_sum = pchc_1 + pchc_2 + pchc_3,
           pchc_1 = pchc_1 / weight_sum,
           pchc_2 = pchc_2 / weight_sum,
           pchc_3 = pchc_3 / weight_sum) %>% 
    bind_cols(pchc.sh.nonsample[, c("pchc")]) %>% 
    select(-weight_sum) %>% 
    pivot_longer(cols = c(pchc_1, pchc_2, pchc_3), 
                 names_to = 'knn_level', 
                 values_to = 'knn_weight', 
                 values_drop_na = FALSE)
  
  
  ##---- Projection ----
  # projection data
  proj.small.data <- raw.total %>% 
    filter(province %in% small) %>% 
    arrange(province, city, district, pchc, packid, date) %>% 
    group_by(pchc, packid, date) %>% 
    summarise(sales = sum(sales, na.rm = TRUE), 
              units = sum(units, na.rm = TRUE)) %>% 
    ungroup()
  
  # est ratio factor
  est.sh <- pchc.sh %>% 
    distinct(pchc, est)
  
  est.ratio <- pchc.sh.nonsample %>% 
    distinct(pchc, province, city, district, flag_sample) %>% 
    left_join(knn.indice, by = 'pchc') %>% 
    left_join(knn.weight, by = c('pchc', 'knn_level')) %>% 
    left_join(est.sh, by = c('knn_pchc' = 'pchc')) %>% 
    rename(knn_est = est) %>% 
    left_join(est.sh, by = 'pchc') %>% 
    group_by(province, city, district, pchc) %>% 
    summarise(knn_est = sum(knn_est * knn_weight, na.rm = TRUE), 
              est = sum(est * knn_weight, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(est_ratio = est / knn_est)
  
  # result
  proj.small.nonsample <- pchc.sh.nonsample %>% 
    distinct(pchc, province, city, district, flag_sample) %>% 
    left_join(knn.indice, by = 'pchc') %>% 
    left_join(knn.weight, by = c('pchc', 'knn_level')) %>% 
    left_join(proj.small.data, by = c('knn_pchc' = 'pchc')) %>% 
    group_by(province, city, district, pchc, packid, date, flag_sample) %>% 
    summarise(knn_sales = sum(sales * knn_weight, na.rm = TRUE), 
              knn_units = sum(units * knn_weight, na.rm = TRUE)) %>% 
    ungroup() %>% 
    left_join(est.ratio, by = c('province', 'city', 'district', 'pchc')) %>% 
    mutate(sales = knn_sales * est_ratio, 
           units = knn_units * est_ratio)
  
  proj.small <- raw.total %>% 
    filter(province %in% small) %>% 
    mutate(flag_sample = 1) %>% 
    bind_rows(proj.small.nonsample) %>% 
    group_by(date, province, city, district, packid, flag_sample) %>% 
    summarise(sales = sum(sales, na.rm = TRUE), 
              units = sum(units, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(sales > 0, units > 0)
  
  
  return(proj.small)
}







