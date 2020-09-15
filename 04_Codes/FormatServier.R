# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  CHC Functionalize
# Purpose:      Format Servier
# programmer:   Zhe Liu
# Date:         2020-07-16
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


FormatServier <- function(total.proj, market.def, corp.pack, pack.size, 
                          capital.47, prod.bid, corp.type, atc3.cn, 
                          molecule.cn, corp.add, packid.profile, prod.profile, 
                          city.en) {
  
  ##---- Adjustment ----
  adj.raw <- total.proj %>% 
    inner_join(market.def, by = 'packid') %>% 
    mutate(market = if_else(atc2 %in% c("C07", "C08"), "IHD", market)) %>% 
    filter(!(market == "IHD" & molecule_desc == "IVABRADINE")) %>% 
    filter(!(market == "OAD" & molecule_desc == "EPALRESTAT"))
  
  adj.ihd <- adj.raw %>% 
    filter(atc2 %in% c('C07', 'C08')) %>% 
    mutate(factor = if_else(atc2 == 'C07', 0.25, 0.1), 
           sales = sales * factor, 
           units = units * factor, 
           market = 'IHD') %>% 
    select(-factor)
  
  adj.htn <- adj.raw %>% 
    filter(atc2 %in% c('C07', 'C08')) %>% 
    mutate(factor = if_else(atc2 == "C07", 0.75, 0.9), 
           sales = sales * factor,
           units = units * factor,
           market = 'HTN') %>% 
    select(-factor)
  
  adj.nation <- adj.raw %>% 
    filter(!(atc2 %in% c('C07', 'C08'))) %>% 
    bind_rows(adj.ihd, adj.htn)
  
  
  ##---- Summary ----
  servier.result <- adj.nation %>% 
    left_join(corp.pack, by = 'packid') %>% 
    left_join(pack.size, by = 'packid') %>% 
    filter(stri_sub(pack_desc, 1, 4) %in% c("CAP ", "TAB ", "PILL")) %>% 
    mutate(dosage_units = pack_size * units,
           channel = "CHC") %>% 
    group_by(province, city, year, quarter, market, atc3, molecule_desc, packid, 
             pack_desc, prod_desc, corp_desc, channel) %>% 
    summarise(units = sum(units, na.rm = TRUE),
              dosage_units = sum(dosage_units, na.rm = TRUE),
              sales = sum(sales, na.rm = TRUE)) %>% 
    ungroup() %>% 
    left_join(capital.47, by = 'city') %>% 
    mutate(prodid = stri_sub(packid, 1, 5)) %>% 
    left_join(prod.bid, by = 'prodid') %>% 
    left_join(molecule.cn, by = 'molecule_desc') %>% 
    mutate(name1 = if_else(Molecule_CN %in% c("赖诺普利", "卡托普利"), 
                            "4+7分子", 
                            name1), 
           name1 = if_else(name1 == '非4+7分子', NA_character_, name1)) %>% 
    left_join(atc3.cn, by = 'atc3') %>% 
    left_join(corp.type, by = 'corp_desc') %>% 
    left_join(corp.add, by = 'corp_desc') %>% 
    mutate(name3 = trimws(name3),
           Mnf_Type = if_else(is.na(Mnf_Type), Mnf_Type1, Mnf_Type),
           Mnf_Type = trimws(Mnf_Type),
           name3 = ifelse(is.na(name3) & Mnf_Type == "MNC", "原研", 
                          ifelse(is.na(name3) & Mnf_Type == "Local", "仿制", 
                                 name3)),
           name2 = ifelse(name2 == "非中标产品", NA_character_, name2)) %>% 
    rename(`是否进入带量采购` = name1,
           `是否是中标品种` = name2,
           `是否是原研` = name3,
           `是否是MNC` = Mnf_Type) %>% 
    filter(!grepl('AMP', pack_desc)) %>% 
    mutate(first_num_position = stri_locate_first(pack_desc, regex = "\\d")[,1],
           last_space_position = stri_locate_last(pack_desc, regex = "\\s")[,1],
           Package = str_squish(substr(pack_desc, 1, first_num_position - 1)),
           Dosage = str_squish(substr(pack_desc, first_num_position, 
                                      last_space_position - 1)),
           Quantity = as.integer(str_squish(substr(pack_desc, last_space_position, 
                                                   nchar(pack_desc))))) %>% 
    left_join(packid.profile, by = 'packid') %>% 
    left_join(prod.profile, by = 'prodid') %>% 
    mutate(atc4 = ifelse(is.na(ATC4_Code) & !is.na(packid), 
                         ATC4_Code1, 
                         ATC4_Code),
           ims_product_cn = ifelse(is.na(ims_product_cn) & !is.na(packid), 
                                   ims_product_cn1, 
                                   ims_product_cn)) %>% 
    mutate(
      `TherapeuticClsII` = case_when(
        atc3 == "A10H" ~ "SULPHONYLUREA",
        atc3 == "A10J" ~      "BIGUANIDE",
        atc3 == "A10K" ~     "GLITAZONE",
        atc3 == "A10L" ~     "AGIs",
        atc3 == "A10M" ~    "METAGLINIDE",
        atc3 == "A10N" ~    "DPP-IV",
        atc3 == "A10P" ~     "SGLT2",
        atc3 == "A10S" ~     "GLP-1",
        atc3 == "A10X" ~     "OTHERS",
        atc3 == "C02A" ~     "ANTI-HTN",
        atc3 == "C02B" ~     "ANTI-HTN",
        atc3 == "C02C" ~     "ANTI-HTN",
        atc3 == "C03A" ~     "DIURETICS",
        atc3 == "C07A" ~     "BB",
        atc3 == "C08A" ~     "CCB",
        atc3 == "C08B" ~     "CCB",
        atc3 == "C09A" ~     "ACEi PLAIN",
        atc3 == "C09C" ~     "ARB PLAIN",
        atc3 == "C07A" ~     "BB",
        atc3 == "C08A" ~     "CCB",
        atc3 == "C01E" ~     "NITRITES",
        molecule_desc == "TRIMETAZIDINE" ~   "TMZ",
        atc3 == "C01D" & molecule_desc != "TRIMETAZIDINE" ~  "OTHERS",
        atc4 %in% c("C09B3", "C09D3") ~ "A+C FDC",
        atc4 %in% c("C09B1", "C09D1") ~ "A+D FDC",
        !is.na(atc4) ~ "OTHERS",
        TRUE ~ NA_character_
      )
    ) %>% 
    left_join(city.en, by = c('city' = 'City')) %>% 
    mutate(
      `Period-MAT` = case_when(
        quarter %in% c("2020Q1", "2020Q2", "2020Q3", "2020Q4") ~ "MAT20Q4",
        quarter %in% c("2019Q1", "2019Q2", "2019Q3", "2019Q4") ~ "MAT19Q4",
        quarter %in% c("2018Q1", "2018Q2", "2018Q3", "2018Q4") ~ "MAT18Q4",
        quarter %in% c("2017Q1", "2017Q2", "2017Q3", "2017Q4") ~ "MAT17Q4",
        TRUE ~ NA_character_
      ), 
      TherapeuticClsII = case_when(
        market == "HTN" & atc3 == "C09A" ~ "RAASi Plain",
        market == "HTN" & atc3 == "C09C" ~ "RAASi Plain",
        market == "HTN" & atc3 == "C09B" ~ "RAASi FDC",
        market == "HTN" & atc3 == "C09D" ~ "RAASi FDC",
        market == "HTN" & atc3 == "C02A" ~ "ANTI-HTN",
        market == "HTN" & atc3 == "C02B" ~ "ANTI-HTN",
        market == "HTN" & atc3 == "C02C" ~ "ANTI-HTN",
        market == "HTN" & atc3 == "C03A" ~ "DIURETICS",
        market == "HTN" & atc3 == "C07A" ~ "BB",
        market == "HTN" & atc3 == "C08A" ~ "CCB",
        market == "HTN" & atc3 == "C08B" ~ "CCB",
        market == "OAD" & atc3 == "A10H" ~ "SULPHONYLUREA",
        market == "OAD" & atc3 == "A10J" ~ "BIGUANIDE",
        market == "OAD" & atc3 == "A10K" ~ "GLITAZONE",
        market == "OAD" & atc3 == "A10L" ~ "AG Is",
        market == "OAD" & atc3 == "A10M" ~ "METAGLINIDE",
        market == "OAD" & atc3 == "A10N" ~ "DPP-IV",
        market == "OAD" & atc3 == "A10P" ~ "SGLT2",
        market == "OAD" & atc3 == "A10S" ~ "GLP-1",
        market == "OAD" & atc3 == "A10X" ~ "OTHERS",
        market == "IHD" & atc3 == "C07A" ~ "BB",
        market == "IHD" & atc3 == "C08A" ~ "CCB",
        market == "IHD" & atc3 == "C01E" ~ "NITRITES",
        market == "IHD" & atc3 == "C01D" & molecule_desc == "TRIMETAZIDINE" ~ "TMZ",
        market == "IHD" & atc3 == "C01D" & molecule_desc != "TRIMETAZIDINE"~ "OTHERS",
        TRUE ~ NA_character_
      ), 
      sales = round(sales, 2), 
      units = round(units), 
      dosage_units = round(dosage_units)
    ) %>% 
    filter(molecule_desc != 'ENALAPRIL+FOLIC ACID') %>% 
    select(Pack_ID = packid, 
           Channel = channel, 
           Province = province, 
           City = city, 
           Date = quarter, 
           ATC3 = atc3, 
           MKT = market, 
           Molecule_Desc = molecule_desc, 
           Prod_Desc = prod_desc, 
           Pck_Desc = pack_desc, 
           Corp_Desc = corp_desc, 
           Sales = sales, 
           Units = units, 
           DosageUnits = dosage_units, 
           `Period-MAT`, 
           `CITY-EN`, 
           TherapeuticClsII, 
           Prod_CN_Name = ims_product_cn, 
           Package, 
           Dosage, 
           Quantity, 
           `是否是4+7城市`, 
           `是否进入带量采购`, 
           `是否是原研`, 
           `是否是中标品种`, 
           `是否是MNC`, 
           `ATC3中文分类`) %>% 
    arrange(Channel, Date, Province, City, MKT, Pack_ID) %>% 
    filter(City %in% target.city)
  
  
  return(servier.result)
}












