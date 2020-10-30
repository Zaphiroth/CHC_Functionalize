# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  CHC Functionalize
# Purpose:      Review
# programmer:   Zhe Liu
# Date:         2020-10-22
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- CHC universe ----
pchc.universe.raw <- read.xlsx("02_Inputs/Universe_PCHCCode_20201019.xlsx", sheet = "PCHC")

chk <- pchc.universe.raw %>% 
  distinct(pchc = PCHC_Code, est = `其中：西药药品收入（千元）`) %>% 
  add_count(pchc) %>% 
  filter(n > 1)


##---- Review ----
raw.dist <- raw.total %>% 
  left_join(market.def, by = 'packid') %>% 
  filter(!is.na(market)) %>% 
  filter(city %in% target.city) %>% 
  group_by(quarter, province, city, packid) %>% 
  summarise(raw_sales = sum(sales, na.rm = TRUE), 
            raw_units = sum(units, na.rm = TRUE)) %>% 
  ungroup()

result.dist <- servier.result %>% 
  group_by(quarter = Date, province = Province, city = City, 
           product = Prod_Desc, packid = Pack_ID) %>% 
  summarise(result_sales = sum(Sales, na.rm = TRUE), 
            result_units = sum(Units, na.rm = TRUE)) %>% 
  ungroup()

proj.rate <- left_join(raw.dist, result.dist, 
                       by = c("quarter", "province", "city", "packid")) %>% 
  mutate(rate = result_sales / raw_sales - 1)

write.xlsx(proj.rate, '05_Internal_Review/Seriver_Proj_Rate.xlsx')


# chk <- raw.dist %>% 
#   filter(!(prodid %in% stri_sub(total.proj$packid, 1, 5))) %>% 
#   distinct(prodid)
