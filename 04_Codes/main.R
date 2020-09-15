options(java.parameters = "-Xmx2048m",
        stringsAsFactors = FALSE, 
        encoding = 'UTF-8')


##---- Universe info ----
# target city
target.prov <- c("安徽", "北京", "福建", "广东", "江苏", "山东", "上海", "浙江")
target.city <- c("北京", "常州", "福州", "广州", "杭州", "合肥", "济南", "南京", 
                 "宁波", "青岛", "泉州", "厦门", "上海", "绍兴", "苏州", "温州", 
                 "无锡", "徐州")

# PCHC info
pchc.universe.raw <- read.xlsx("02_Inputs/Universe_PCHCCode_20200507.xlsx", sheet = "PCHC")

pchc.universe.m <- pchc.universe.raw %>% 
  group_by(pchc = PCHC_Code) %>% 
  summarise(province = first(na.omit(`省`)),
            city = first(na.omit(`地级市`)),
            district = first(na.omit(`区[县/县级市】`)),
            est = first(na.omit(`其中：西药药品收入（千元）`))) %>% 
  ungroup() %>% 
  filter(!is.na(est))

# city tier
city.tier <- read.xlsx("02_Inputs/pchc_city_tier.xlsx") %>% 
  group_by(city) %>% 
  mutate(tier = ifelse(is.na(city_tier), first(na.omit(city_tier)), city_tier)) %>% 
  ungroup() %>% 
  mutate(tier = ifelse(is.na(tier), 5, tier)) %>% 
  distinct(city, tier)

# IMS pack
ims.pack <- fread("02_Inputs/pfc与ims数据对应_20200708.csv") %>% 
  mutate(packid = stri_pad_left(Pack_Id, 7, 0),
         atc3 = stri_sub(ATC4_Code, 1, 4),
         atc2 = stri_sub(ATC4_Code, 1, 3)) %>% 
  distinct(packid, atc3, atc2, molecule_desc = Molecule_Desc, 
           prod_desc = Prod_Desc)

# market definition
market.def <- read_xlsx("02_Inputs/Market_Definition_20200708.xlsx") %>% 
  distinct(molecule_desc = Molecule_Desc, market = TA) %>% 
  right_join(ims.pack, by = "molecule_desc") %>% 
  filter(!is.na(market))


##---- Raw data ----
# Shanghai
servier.sh.raw <- read.xlsx("02_Inputs/raw data/shanghai_201805_202004_packid_moleinfo_PCHC.xlsx")

servier.sh <- servier.sh.raw %>% 
  mutate(year = as.character(Year),
         date = as.character(Month),
         quarter = ifelse(stri_sub(date, 5, 6) %in% c("01", "02", "03"), paste0(year, "Q1"), 
                          ifelse(stri_sub(date, 5, 6) %in% c("04", "05", "06"), paste0(year, "Q2"), 
                                 ifelse(stri_sub(date, 5, 6) %in% c("07", "08", "09"), paste0(year, "Q3"), 
                                        ifelse(stri_sub(date, 5, 6) %in% c("10", "11", "12"), paste0(year, "Q4"), 
                                               NA_character_)))),
         province = "上海",
         city = "上海",
         district = `区县`,
         pchc = PCHC_Code,
         packid = packcode) %>% 
  distinct() %>% 
  select(year, date, quarter, province, city, district, pchc, packid, units = `数量`, sales = `金额`)

# history
servier.history <- read_feather("02_Inputs/raw data/Servier_CHC_Total_Raw.feather")

# total
total.sales <- bind_rows(servier.history, servier.sh) %>% 
  filter(pchc != "#N/A", units > 0, sales > 0, year == '2019') %>% 
  mutate(packid = if_else(stri_sub(packid, 1, 5) == '47775', 
                          stri_paste('58906', stri_sub(packid, 6, 7)), 
                          packid), 
         packid = if_else(stri_sub(packid, 1, 5) == '06470', 
                          stri_paste('64895', stri_sub(packid, 6, 7)), 
                          packid)) %>% 
  group_by(year, date, quarter, pchc, packid) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

total.raw <- bind_rows(servier.history, servier.sh) %>% 
  filter(pchc != "#N/A", units > 0, sales > 0, year == '2019') %>% 
  group_by(pchc) %>% 
  summarise(province = first(na.omit(province)),
            city = first(na.omit(city)),
            district = first(na.omit(district))) %>% 
  ungroup() %>% 
  left_join(total.sales, by = 'pchc')


##---- Run Project ----
source('04_Codes/SampleProject.R', encoding = 'UTF-8')
source('04_Codes/SampleProject1.R', encoding = 'UTF-8')
source('04_Codes/ShanghaiProject.R', encoding = 'UTF-8')
source('04_Codes/ShanghaiProject1.R', encoding = 'UTF-8')
source('04_Codes/NationProject.R', encoding = 'UTF-8')

proj.sample <- SampleProject(total.raw, pchc.universe.m, 
                             target.city, market.def)

proj.sample <- SampleProject1(total.raw, pchc.universe.m)

proj.sh <- ShanghaiProject(total.raw, pchc.universe.raw)

proj.sh <- ShanghaiProject1(total.raw, pchc.universe.raw)

proj.result <- bind_rows(proj.sample, proj.sh)

proj.nation <- NationProject(proj.result, pchc.universe.m, city.tier)


##---- Bind ----
total.proj <- bind_rows(proj.result, proj.nation) %>% 
  mutate(year = stri_sub(date, 1, 4), 
         quarter = stri_sub(date, 5, 6), 
         quarter = ifelse(quarter %in% c("01", "02", "03"), "Q1",
                          ifelse(quarter %in% c("04", "05", "06"), "Q2",
                                 ifelse(quarter %in% c("07", "08", "09"), "Q3",
                                        ifelse(quarter %in% c("10", "11", "12"), "Q4",
                                               NA_character_)))), 
         quarter = stri_paste(year, quarter)) %>% 
  filter(city %in% target.city) %>% 
  group_by(year, quarter, date, province, city, packid, flag_sample) %>% 
  summarise(sales = sum(sales, na.rm = TRUE), 
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup()


##---- Format info ----
# zs flag
zs.flag.raw <- read.xlsx("02_Inputs/13城市的招标flag_zs_flag.xlsx")

zs.flag <- zs.flag.raw %>% 
  filter(!is.na(`是否是13城市`)) %>% 
  distinct(province = `省`, city = `地级市`, pchc = PCHC_Code, zs_flag)

# corporation
corp.ref <- fread("02_Inputs/cn_corp_ref_201903_1.txt", 
                  stringsAsFactors = FALSE, sep = "|") %>% 
  distinct()

# pack
pack.ref <- fread("02_Inputs/cn_prod_ref_201903_1.txt", 
                  stringsAsFactors = FALSE, sep = "|") %>% 
  distinct() %>% 
  mutate(Pack_Id = stri_pad_left(Pack_Id, 7, 0))

corp.pack <- pack.ref %>% 
  select("Pack_Id", "Pck_Desc", "Corp_ID") %>% 
  distinct() %>% 
  left_join(corp.ref, by = "Corp_ID") %>% 
  mutate(Corp_Desc = if_else(Corp_Desc == "LUYE GROUP", "LVYE GROUP", Corp_Desc)) %>% 
  select(packid = Pack_Id, pack_desc = Pck_Desc, corp_desc = Corp_Desc)

# pack size
pack.size <- pack.ref %>% 
  distinct(packid = Pack_Id, pack_size = PckSize_Desc)

# 4+7 flag
capital.47 <- read_xlsx("02_Inputs/4+7+省会名单.xlsx") %>% 
  filter(`类别` %in% "4+7城市") %>% 
  mutate(city = gsub("市", "", `城市`)) %>% 
  select(city, `是否是4+7城市` = `类别`)

# bid name
prod.bid <- read_xlsx("02_Inputs/Displayname Mapping.xlsx", sheet = 1) %>% 
  mutate(Pack_ID = stri_pad_left(Pack_ID, 7, 0),
         prodid = stri_sub(Pack_ID, 1, 5),
         `Display Name3 CN` = if_else(`Display Name3 CN` %in% c("中标品规", "非中标品规"), 
                                      "仿制", 
                                      `Display Name3 CN`),
         `Display Name3 CN` = gsub("-2|-1", "", `Display Name3 CN`),
         `Display Name2 CN` = gsub("-2|-1", "", `Display Name2 CN`)) %>% 
  distinct(name1 = `Display Name1 CN`,
           name2 = `Display Name2 CN`,
           name3 = `Display Name3 CN`,
           prodid)

# corporation, ATC3
corp.atc3 <- read_xlsx("02_Inputs/产品性质_chpa 08.23(1).xlsx", sheet = 1)

corp.type <- distinct(corp.atc3, corp_desc = Corp_Desc, Mnf_Type = `厂家性质`)

atc3.cn <- distinct(corp.atc3, atc3 = ATC3_Code, `ATC3中文分类` = `类别`)

molecule.cn <- distinct(corp.atc3, molecule_desc = Molecule_Desc, Molecule_CN = `分子`)

corp.add <- read_xlsx("02_Inputs/Corp_Info_20200720.xlsx") %>% 
  group_by(corp_desc = Corp_Desc) %>% 
  arrange(MnfType_Desc) %>% 
  summarise(Mnf_Type1 = first(MnfType_Desc)) %>% 
  ungroup() %>% 
  mutate(Mnf_Type1 = if_else(is.na(Mnf_Type1), "Local", Mnf_Type1),
         Mnf_Type1 = if_else(Mnf_Type1 %in% c("Imported", "Joint Venture"), "MNC", Mnf_Type1))

# new profile
packid.profile.raw <- read_xlsx("02_Inputs/packid_prod_20181112.xlsx")

packid.profile <- packid.profile.raw %>% 
  mutate(packid = stri_pad_left(Pack_Id, 7, 0)) %>% 
  distinct(packid, ATC4_Code, ims_product_cn)

prod.profile <- packid.profile %>%
  mutate(prodid = substr(packid, 1, 5)) %>% 
  select(prodid, ATC4_Code1 = ATC4_Code, ims_product_cn1 = ims_product_cn) %>%
  distinct()

# city EN
city.en <- read.xlsx("02_Inputs/CityEN.xlsx")


##---- Run format ----
source('04_Codes/FormatServier.R')

servier.result <- FormatServier(total.proj, market.def, corp.pack, pack.size, 
                                capital.47, prod.bid, corp.type, atc3.cn, 
                                molecule.cn, corp.add, packid.profile, 
                                prod.profile, city.en)

write.xlsx(servier.result, '03_Outputs/Servier_CHC_Result2.xlsx')
