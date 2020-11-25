Data Exploration for Philadelphia and surrounding Suburbs
================

# Load Libraries

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.4     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.1     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(ggplot2)
library(readr)
```

# Import Data

``` r
df <- read_csv('/Users/TyPainter1/Desktop/Masters/Fall\ 2020/DS-5610/eda20-team5-project/Data\ Basics/data.csv') 
```

# Clean Data

Most of the columns are hard to understand tried to make it easier

``` r
df <- df %>% 
  mutate(ED_PRXECE_NM = exp(ED_PRXECE)) %>%
  mutate(ED_PRXHQECE_NM = exp(ED_PRXHQECE)) %>%
  mutate(HE_SUPRFND_NM = exp(HE_SUPRFND)) %>%
  mutate(HE_RSEI_NM = exp(HE_RSEI)) %>%
  select(-ED_PRXECE, -ED_PRXHQECE, -HE_SUPRFND, -HE_RSEI)
```

Most of the columns are hard to understand. We tried to make it easier
and obvious even without the data dictionary.

``` r
df <- df %>% 
  rename(id = `_id`) %>% 
  rename(geo_id = geoid) %>% 
  rename(metro_areas = in100) %>%
  rename(area_code = msaid15) %>% 
  rename(county_code = countyfips) %>%
  rename(num_under_18 = pop) %>% 
  rename(ratio_students_AP_enrolled = ED_APENR) %>%
  rename(perc_over24_college_degree = ED_ATTAIN) %>%
  rename(perc_18to24_nearby_college_enrolled = ED_COLLEGE) %>%
  rename(perc_3to4_school_enrolled = ED_ECENROL) %>%
  rename(perc_high_grad = ED_HSGRAD) %>%
  rename(score_third_grade_math = ED_MATH) %>%
  rename(score_third_grade_read = ED_READING) %>%
  rename(perc_elementary_school_poverty = ED_SCHPOV) %>% 
  rename(perc_teacher_1and2_years=ED_TEACHXP) %>% 
  rename(num_ECE_nearby = ED_PRXECE_NM) %>% 
  rename(num_high_qual_ECE_nearby = ED_PRXHQECE_NM) %>% 
  rename(perc_supermarket_nearby = HE_FOOD) %>% 
  rename(perc_green_space_access = HE_GREEN) %>%
  rename(days_temp_above90 = HE_HEAT) %>%
  rename(perc_0to64_health_insurance = HE_HLTHINS) %>%
  rename(mean_ozone_amount = HE_OZONE) %>%
  rename(mean_microparticle = HE_PM25) %>%
  rename(perc_housing_vacancy = HE_VACANCY) %>%
  rename(index_walkability = HE_WALK) %>% 
  rename(num_waste_dump_sites = HE_SUPRFND_NM) %>%
  rename(index_air_pollutants = HE_RSEI_NM) %>%
  rename(perc_below100_poverty = SE_POVRATE) %>%
  rename(perc_household_public_assistance = SE_PUBLIC) %>%
  rename(perc_home_ownership = SE_HOME) %>%
  rename(perc_over15_high_skill = SE_OCC) %>%
  rename(median_income=SE_MHE) %>%
  rename(perc_adults_employed = SE_EMPRAT) %>%
  rename(perc_worker_commute_over1hour = SE_JOBPROX) %>%
  rename(perc_single_parent = SE_SINGLE)
```

# Basic Exporlation

``` r
philly <- df %>% 
  filter(msaname15 == "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD Metro Area") %>% # filter Philly area
  mutate(type = ifelse(county_code == 42101, "Philadelphia", "Suburb")) %>%  # mutate a city, suburb column;City of Phila. is 42101
  mutate(county_code = replace(county_code, county_code == '10003', 'New Castle (DE)')) %>%
  mutate(county_code = replace(county_code, county_code == '24015', 'Cecil (MD)')) %>%
  mutate(county_code = replace(county_code, county_code == '34005', 'Burlington (NJ)')) %>%
  mutate(county_code = replace(county_code, county_code == '34007', 'Camden (NJ)')) %>%
  mutate(county_code = replace(county_code, county_code == '34015', 'Gloucester (NJ)'))%>%
  mutate(county_code = replace(county_code, county_code == '34033', 'Salem (NJ)')) %>%
  mutate(county_code = replace(county_code, county_code == '42017', 'Bucks (PA)')) %>%
  mutate(county_code = replace(county_code, county_code == '42029', 'Chester (PA)')) %>%
  mutate(county_code = replace(county_code, county_code == '42045', 'Delaware (PA)')) %>%
  mutate(county_code = replace(county_code, county_code == '42091', 'Montgomery (PA)'))%>%
  mutate(county_code = replace(county_code, county_code == '42101', 'Philadelphia'))%>%
   mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))

factor(philly$county_code, levels = c("New Castle (DE)", "Cecil (MD)", "Burlington (NJ)", "Camden (NJ)", "Gloucester (NJ)", "Salem (NJ)", "Bucks (PA)", "Chester (PA)", "Delaware (PA)", "Montgomery (PA)"))
```

    ##    [1] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##    [5] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##    [9] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##   [13] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##   [17] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##   [21] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##   [25] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##   [29] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##   [33] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##   [37] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##   [41] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##   [45] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##   [49] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##   [53] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##   [57] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##   [61] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##   [65] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##   [69] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##   [73] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##   [77] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##   [81] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##   [85] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##   [89] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##   [93] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##   [97] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [101] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [105] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [109] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [113] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [117] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [121] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [125] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [129] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [133] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [137] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [141] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [145] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [149] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [153] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [157] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [161] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [165] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [169] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [173] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [177] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [181] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [185] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [189] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [193] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [197] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [201] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [205] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [209] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [213] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [217] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [221] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [225] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [229] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [233] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [237] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [241] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [245] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [249] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [253] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [257] New Castle (DE) New Castle (DE) New Castle (DE) New Castle (DE)
    ##  [261] New Castle (DE) New Castle (DE) Cecil (MD)      Cecil (MD)     
    ##  [265] Cecil (MD)      Cecil (MD)      Cecil (MD)      Cecil (MD)     
    ##  [269] Cecil (MD)      Cecil (MD)      Cecil (MD)      Cecil (MD)     
    ##  [273] Cecil (MD)      Cecil (MD)      Cecil (MD)      Cecil (MD)     
    ##  [277] Cecil (MD)      Cecil (MD)      Cecil (MD)      Cecil (MD)     
    ##  [281] Cecil (MD)      Cecil (MD)      Cecil (MD)      Cecil (MD)     
    ##  [285] Cecil (MD)      Cecil (MD)      Cecil (MD)      Cecil (MD)     
    ##  [289] Cecil (MD)      Cecil (MD)      Cecil (MD)      Cecil (MD)     
    ##  [293] Cecil (MD)      Cecil (MD)      Cecil (MD)      Cecil (MD)     
    ##  [297] Cecil (MD)      Cecil (MD)      Cecil (MD)      Cecil (MD)     
    ##  [301] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [305] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [309] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [313] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [317] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [321] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [325] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [329] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [333] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [337] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [341] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [345] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [349] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [353] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [357] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [361] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [365] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [369] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [373] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [377] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [381] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [385] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [389] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [393] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [397] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [401] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [405] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [409] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [413] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [417] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [421] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [425] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [429] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [433] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [437] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [441] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [445] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [449] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [453] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [457] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [461] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [465] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [469] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [473] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [477] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [481] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [485] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [489] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [493] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [497] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [501] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [505] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [509] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [513] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [517] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [521] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [525] Burlington (NJ) Burlington (NJ) Burlington (NJ) Burlington (NJ)
    ##  [529] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [533] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [537] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [541] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [545] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [549] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [553] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [557] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [561] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [565] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [569] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [573] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [577] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [581] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [585] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [589] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [593] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [597] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [601] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [605] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [609] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [613] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [617] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [621] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [625] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [629] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [633] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [637] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [641] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [645] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [649] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [653] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [657] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [661] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [665] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [669] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [673] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [677] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [681] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [685] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [689] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [693] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [697] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [701] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [705] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [709] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [713] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [717] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [721] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [725] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [729] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [733] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [737] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [741] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [745] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [749] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [753] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [757] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [761] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [765] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [769] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [773] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [777] Camden (NJ)     Camden (NJ)     Camden (NJ)     Camden (NJ)    
    ##  [781] Camden (NJ)     Camden (NJ)     Gloucester (NJ) Gloucester (NJ)
    ##  [785] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [789] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [793] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [797] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [801] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [805] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [809] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [813] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [817] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [821] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [825] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [829] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [833] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [837] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [841] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [845] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [849] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [853] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [857] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [861] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [865] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [869] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [873] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [877] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [881] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [885] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [889] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [893] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [897] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [901] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [905] Gloucester (NJ) Gloucester (NJ) Gloucester (NJ) Gloucester (NJ)
    ##  [909] Salem (NJ)      Salem (NJ)      Salem (NJ)      Salem (NJ)     
    ##  [913] Salem (NJ)      Salem (NJ)      Salem (NJ)      Salem (NJ)     
    ##  [917] Salem (NJ)      Salem (NJ)      Salem (NJ)      Salem (NJ)     
    ##  [921] Salem (NJ)      Salem (NJ)      Salem (NJ)      Salem (NJ)     
    ##  [925] Salem (NJ)      Salem (NJ)      Salem (NJ)      Salem (NJ)     
    ##  [929] Salem (NJ)      Salem (NJ)      Salem (NJ)      Salem (NJ)     
    ##  [933] Salem (NJ)      Salem (NJ)      Salem (NJ)      Salem (NJ)     
    ##  [937] Salem (NJ)      Salem (NJ)      Salem (NJ)      Salem (NJ)     
    ##  [941] Salem (NJ)      Salem (NJ)      Salem (NJ)      Salem (NJ)     
    ##  [945] Salem (NJ)      Salem (NJ)      Salem (NJ)      Salem (NJ)     
    ##  [949] Salem (NJ)      Salem (NJ)      Salem (NJ)      Salem (NJ)     
    ##  [953] Salem (NJ)      Salem (NJ)      Salem (NJ)      Salem (NJ)     
    ##  [957] Salem (NJ)      Salem (NJ)      Bucks (PA)      Bucks (PA)     
    ##  [961] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ##  [965] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ##  [969] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ##  [973] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ##  [977] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ##  [981] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ##  [985] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ##  [989] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ##  [993] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ##  [997] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1001] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1005] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1009] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1013] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1017] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1021] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1025] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1029] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1033] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1037] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1041] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1045] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1049] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1053] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1057] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1061] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1065] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1069] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1073] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1077] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1081] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1085] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1089] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1093] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1097] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1101] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1105] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1109] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1113] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1117] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1121] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1125] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1129] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1133] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1137] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1141] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1145] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1149] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1153] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1157] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1161] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1165] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1169] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1173] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1177] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1181] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1185] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1189] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1193] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1197] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1201] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1205] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1209] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1213] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1217] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1221] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1225] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1229] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1233] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1237] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1241] Bucks (PA)      Bucks (PA)      Bucks (PA)      Bucks (PA)     
    ## [1245] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1249] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1253] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1257] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1261] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1265] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1269] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1273] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1277] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1281] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1285] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1289] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1293] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1297] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1301] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1305] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1309] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1313] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1317] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1321] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1325] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1329] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1333] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1337] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1341] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1345] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1349] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1353] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1357] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1361] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1365] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1369] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1373] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1377] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1381] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1385] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1389] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1393] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1397] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1401] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1405] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1409] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1413] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1417] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1421] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1425] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1429] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1433] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1437] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1441] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1445] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1449] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1453] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1457] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1461] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1465] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1469] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1473] Chester (PA)    Chester (PA)    Chester (PA)    Chester (PA)   
    ## [1477] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1481] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1485] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1489] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1493] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1497] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1501] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1505] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1509] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1513] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1517] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1521] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1525] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1529] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1533] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1537] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1541] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1545] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1549] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1553] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1557] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1561] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1565] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1569] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1573] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1577] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1581] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1585] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1589] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1593] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1597] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1601] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1605] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1609] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1613] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1617] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1621] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1625] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1629] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1633] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1637] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1641] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1645] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1649] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1653] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1657] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1661] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1665] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1669] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1673] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1677] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1681] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1685] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1689] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1693] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1697] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1701] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1705] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1709] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1713] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1717] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1721] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1725] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1729] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1733] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1737] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1741] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1745] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1749] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1753] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1757] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1761] Delaware (PA)   Delaware (PA)   Delaware (PA)   Delaware (PA)  
    ## [1765] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1769] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1773] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1777] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1781] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1785] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1789] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1793] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1797] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1801] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1805] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1809] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1813] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1817] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1821] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1825] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1829] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1833] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1837] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1841] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1845] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1849] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1853] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1857] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1861] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1865] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1869] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1873] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1877] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1881] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1885] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1889] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1893] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1897] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1901] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1905] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1909] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1913] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1917] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1921] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1925] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1929] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1933] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1937] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1941] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1945] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1949] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1953] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1957] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1961] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1965] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1969] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1973] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1977] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1981] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1985] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1989] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1993] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [1997] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2001] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2005] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2009] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2013] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2017] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2021] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2025] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2029] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2033] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2037] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2041] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2045] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2049] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2053] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2057] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2061] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2065] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2069] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2073] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2077] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2081] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2085] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2089] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2093] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2097] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2101] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2105] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2109] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2113] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2117] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2121] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2125] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2129] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2133] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2137] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2141] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2145] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2149] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2153] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2157] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2161] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2165] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2169] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2173] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2177] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2181] Montgomery (PA) Montgomery (PA) Montgomery (PA) Montgomery (PA)
    ## [2185] Montgomery (PA) Montgomery (PA) <NA>            <NA>           
    ## [2189] <NA>            <NA>            <NA>            <NA>           
    ## [2193] <NA>            <NA>            <NA>            <NA>           
    ## [2197] <NA>            <NA>            <NA>            <NA>           
    ## [2201] <NA>            <NA>            <NA>            <NA>           
    ## [2205] <NA>            <NA>            <NA>            <NA>           
    ## [2209] <NA>            <NA>            <NA>            <NA>           
    ## [2213] <NA>            <NA>            <NA>            <NA>           
    ## [2217] <NA>            <NA>            <NA>            <NA>           
    ## [2221] <NA>            <NA>            <NA>            <NA>           
    ## [2225] <NA>            <NA>            <NA>            <NA>           
    ## [2229] <NA>            <NA>            <NA>            <NA>           
    ## [2233] <NA>            <NA>            <NA>            <NA>           
    ## [2237] <NA>            <NA>            <NA>            <NA>           
    ## [2241] <NA>            <NA>            <NA>            <NA>           
    ## [2245] <NA>            <NA>            <NA>            <NA>           
    ## [2249] <NA>            <NA>            <NA>            <NA>           
    ## [2253] <NA>            <NA>            <NA>            <NA>           
    ## [2257] <NA>            <NA>            <NA>            <NA>           
    ## [2261] <NA>            <NA>            <NA>            <NA>           
    ## [2265] <NA>            <NA>            <NA>            <NA>           
    ## [2269] <NA>            <NA>            <NA>            <NA>           
    ## [2273] <NA>            <NA>            <NA>            <NA>           
    ## [2277] <NA>            <NA>            <NA>            <NA>           
    ## [2281] <NA>            <NA>            <NA>            <NA>           
    ## [2285] <NA>            <NA>            <NA>            <NA>           
    ## [2289] <NA>            <NA>            <NA>            <NA>           
    ## [2293] <NA>            <NA>            <NA>            <NA>           
    ## [2297] <NA>            <NA>            <NA>            <NA>           
    ## [2301] <NA>            <NA>            <NA>            <NA>           
    ## [2305] <NA>            <NA>            <NA>            <NA>           
    ## [2309] <NA>            <NA>            <NA>            <NA>           
    ## [2313] <NA>            <NA>            <NA>            <NA>           
    ## [2317] <NA>            <NA>            <NA>            <NA>           
    ## [2321] <NA>            <NA>            <NA>            <NA>           
    ## [2325] <NA>            <NA>            <NA>            <NA>           
    ## [2329] <NA>            <NA>            <NA>            <NA>           
    ## [2333] <NA>            <NA>            <NA>            <NA>           
    ## [2337] <NA>            <NA>            <NA>            <NA>           
    ## [2341] <NA>            <NA>            <NA>            <NA>           
    ## [2345] <NA>            <NA>            <NA>            <NA>           
    ## [2349] <NA>            <NA>            <NA>            <NA>           
    ## [2353] <NA>            <NA>            <NA>            <NA>           
    ## [2357] <NA>            <NA>            <NA>            <NA>           
    ## [2361] <NA>            <NA>            <NA>            <NA>           
    ## [2365] <NA>            <NA>            <NA>            <NA>           
    ## [2369] <NA>            <NA>            <NA>            <NA>           
    ## [2373] <NA>            <NA>            <NA>            <NA>           
    ## [2377] <NA>            <NA>            <NA>            <NA>           
    ## [2381] <NA>            <NA>            <NA>            <NA>           
    ## [2385] <NA>            <NA>            <NA>            <NA>           
    ## [2389] <NA>            <NA>            <NA>            <NA>           
    ## [2393] <NA>            <NA>            <NA>            <NA>           
    ## [2397] <NA>            <NA>            <NA>            <NA>           
    ## [2401] <NA>            <NA>            <NA>            <NA>           
    ## [2405] <NA>            <NA>            <NA>            <NA>           
    ## [2409] <NA>            <NA>            <NA>            <NA>           
    ## [2413] <NA>            <NA>            <NA>            <NA>           
    ## [2417] <NA>            <NA>            <NA>            <NA>           
    ## [2421] <NA>            <NA>            <NA>            <NA>           
    ## [2425] <NA>            <NA>            <NA>            <NA>           
    ## [2429] <NA>            <NA>            <NA>            <NA>           
    ## [2433] <NA>            <NA>            <NA>            <NA>           
    ## [2437] <NA>            <NA>            <NA>            <NA>           
    ## [2441] <NA>            <NA>            <NA>            <NA>           
    ## [2445] <NA>            <NA>            <NA>            <NA>           
    ## [2449] <NA>            <NA>            <NA>            <NA>           
    ## [2453] <NA>            <NA>            <NA>            <NA>           
    ## [2457] <NA>            <NA>            <NA>            <NA>           
    ## [2461] <NA>            <NA>            <NA>            <NA>           
    ## [2465] <NA>            <NA>            <NA>            <NA>           
    ## [2469] <NA>            <NA>            <NA>            <NA>           
    ## [2473] <NA>            <NA>            <NA>            <NA>           
    ## [2477] <NA>            <NA>            <NA>            <NA>           
    ## [2481] <NA>            <NA>            <NA>            <NA>           
    ## [2485] <NA>            <NA>            <NA>            <NA>           
    ## [2489] <NA>            <NA>            <NA>            <NA>           
    ## [2493] <NA>            <NA>            <NA>            <NA>           
    ## [2497] <NA>            <NA>            <NA>            <NA>           
    ## [2501] <NA>            <NA>            <NA>            <NA>           
    ## [2505] <NA>            <NA>            <NA>            <NA>           
    ## [2509] <NA>            <NA>            <NA>            <NA>           
    ## [2513] <NA>            <NA>            <NA>            <NA>           
    ## [2517] <NA>            <NA>            <NA>            <NA>           
    ## [2521] <NA>            <NA>            <NA>            <NA>           
    ## [2525] <NA>            <NA>            <NA>            <NA>           
    ## [2529] <NA>            <NA>            <NA>            <NA>           
    ## [2533] <NA>            <NA>            <NA>            <NA>           
    ## [2537] <NA>            <NA>            <NA>            <NA>           
    ## [2541] <NA>            <NA>            <NA>            <NA>           
    ## [2545] <NA>            <NA>            <NA>            <NA>           
    ## [2549] <NA>            <NA>            <NA>            <NA>           
    ## [2553] <NA>            <NA>            <NA>            <NA>           
    ## [2557] <NA>            <NA>            <NA>            <NA>           
    ## [2561] <NA>            <NA>            <NA>            <NA>           
    ## [2565] <NA>            <NA>            <NA>            <NA>           
    ## [2569] <NA>            <NA>            <NA>            <NA>           
    ## [2573] <NA>            <NA>            <NA>            <NA>           
    ## [2577] <NA>            <NA>            <NA>            <NA>           
    ## [2581] <NA>            <NA>            <NA>            <NA>           
    ## [2585] <NA>            <NA>            <NA>            <NA>           
    ## [2589] <NA>            <NA>            <NA>            <NA>           
    ## [2593] <NA>            <NA>            <NA>            <NA>           
    ## [2597] <NA>            <NA>            <NA>            <NA>           
    ## [2601] <NA>            <NA>            <NA>            <NA>           
    ## [2605] <NA>            <NA>            <NA>            <NA>           
    ## [2609] <NA>            <NA>            <NA>            <NA>           
    ## [2613] <NA>            <NA>            <NA>            <NA>           
    ## [2617] <NA>            <NA>            <NA>            <NA>           
    ## [2621] <NA>            <NA>            <NA>            <NA>           
    ## [2625] <NA>            <NA>            <NA>            <NA>           
    ## [2629] <NA>            <NA>            <NA>            <NA>           
    ## [2633] <NA>            <NA>            <NA>            <NA>           
    ## [2637] <NA>            <NA>            <NA>            <NA>           
    ## [2641] <NA>            <NA>            <NA>            <NA>           
    ## [2645] <NA>            <NA>            <NA>            <NA>           
    ## [2649] <NA>            <NA>            <NA>            <NA>           
    ## [2653] <NA>            <NA>            <NA>            <NA>           
    ## [2657] <NA>            <NA>            <NA>            <NA>           
    ## [2661] <NA>            <NA>            <NA>            <NA>           
    ## [2665] <NA>            <NA>            <NA>            <NA>           
    ## [2669] <NA>            <NA>            <NA>            <NA>           
    ## [2673] <NA>            <NA>            <NA>            <NA>           
    ## [2677] <NA>            <NA>            <NA>            <NA>           
    ## [2681] <NA>            <NA>            <NA>            <NA>           
    ## [2685] <NA>            <NA>            <NA>            <NA>           
    ## [2689] <NA>            <NA>            <NA>            <NA>           
    ## [2693] <NA>            <NA>            <NA>            <NA>           
    ## [2697] <NA>            <NA>            <NA>            <NA>           
    ## [2701] <NA>            <NA>            <NA>            <NA>           
    ## [2705] <NA>            <NA>            <NA>            <NA>           
    ## [2709] <NA>            <NA>            <NA>            <NA>           
    ## [2713] <NA>            <NA>            <NA>            <NA>           
    ## [2717] <NA>            <NA>            <NA>            <NA>           
    ## [2721] <NA>            <NA>            <NA>            <NA>           
    ## [2725] <NA>            <NA>            <NA>            <NA>           
    ## [2729] <NA>            <NA>            <NA>            <NA>           
    ## [2733] <NA>            <NA>            <NA>            <NA>           
    ## [2737] <NA>            <NA>            <NA>            <NA>           
    ## [2741] <NA>            <NA>            <NA>            <NA>           
    ## [2745] <NA>            <NA>            <NA>            <NA>           
    ## [2749] <NA>            <NA>            <NA>            <NA>           
    ## [2753] <NA>            <NA>            <NA>            <NA>           
    ## [2757] <NA>            <NA>            <NA>            <NA>           
    ## [2761] <NA>            <NA>            <NA>            <NA>           
    ## [2765] <NA>            <NA>            <NA>            <NA>           
    ## [2769] <NA>            <NA>            <NA>            <NA>           
    ## [2773] <NA>            <NA>            <NA>            <NA>           
    ## [2777] <NA>            <NA>            <NA>            <NA>           
    ## [2781] <NA>            <NA>            <NA>            <NA>           
    ## [2785] <NA>            <NA>            <NA>            <NA>           
    ## [2789] <NA>            <NA>            <NA>            <NA>           
    ## [2793] <NA>            <NA>            <NA>            <NA>           
    ## [2797] <NA>            <NA>            <NA>            <NA>           
    ## [2801] <NA>            <NA>            <NA>            <NA>           
    ## [2805] <NA>            <NA>            <NA>            <NA>           
    ## [2809] <NA>            <NA>            <NA>            <NA>           
    ## [2813] <NA>            <NA>            <NA>            <NA>           
    ## [2817] <NA>            <NA>            <NA>            <NA>           
    ## [2821] <NA>            <NA>            <NA>            <NA>           
    ## [2825] <NA>            <NA>            <NA>            <NA>           
    ## [2829] <NA>            <NA>            <NA>            <NA>           
    ## [2833] <NA>            <NA>            <NA>            <NA>           
    ## [2837] <NA>            <NA>            <NA>            <NA>           
    ## [2841] <NA>            <NA>            <NA>            <NA>           
    ## [2845] <NA>            <NA>            <NA>            <NA>           
    ## [2849] <NA>            <NA>            <NA>            <NA>           
    ## [2853] <NA>            <NA>            <NA>            <NA>           
    ## [2857] <NA>            <NA>            <NA>            <NA>           
    ## [2861] <NA>            <NA>            <NA>            <NA>           
    ## [2865] <NA>            <NA>            <NA>            <NA>           
    ## [2869] <NA>            <NA>            <NA>            <NA>           
    ## [2873] <NA>            <NA>            <NA>            <NA>           
    ## [2877] <NA>            <NA>            <NA>            <NA>           
    ## [2881] <NA>            <NA>            <NA>            <NA>           
    ## [2885] <NA>            <NA>            <NA>            <NA>           
    ## [2889] <NA>            <NA>            <NA>            <NA>           
    ## [2893] <NA>            <NA>            <NA>            <NA>           
    ## [2897] <NA>            <NA>            <NA>            <NA>           
    ## [2901] <NA>            <NA>            <NA>            <NA>           
    ## [2905] <NA>            <NA>            <NA>            <NA>           
    ## [2909] <NA>            <NA>            <NA>            <NA>           
    ## [2913] <NA>            <NA>            <NA>            <NA>           
    ## [2917] <NA>            <NA>            <NA>            <NA>           
    ## [2921] <NA>            <NA>            <NA>            <NA>           
    ## [2925] <NA>            <NA>            <NA>            <NA>           
    ## [2929] <NA>            <NA>            <NA>            <NA>           
    ## [2933] <NA>            <NA>            <NA>            <NA>           
    ## [2937] <NA>            <NA>            <NA>            <NA>           
    ## [2941] <NA>            <NA>            <NA>            <NA>           
    ## [2945] <NA>            <NA>            <NA>            <NA>           
    ## [2949] <NA>            <NA>            <NA>            <NA>           
    ## [2953] <NA>            <NA>           
    ## 10 Levels: New Castle (DE) Cecil (MD) Burlington (NJ) ... Montgomery (PA)

``` r
dim(philly)
```

    ## [1] 2954   40

``` r
head(philly)
```

    ## # A tibble: 6 x 40
    ##      id geo_id  year metro_areas area_code msaname15 county_code statefips
    ##   <dbl> <chr>  <dbl>       <dbl>     <dbl> <chr>     <chr>       <chr>    
    ## 1 27465 10003…  2010           1     37980 Philadel… New Castle… 10       
    ## 2 27466 10003…  2015           1     37980 Philadel… New Castle… 10       
    ## 3 27467 10003…  2010           1     37980 Philadel… New Castle… 10       
    ## 4 27468 10003…  2015           1     37980 Philadel… New Castle… 10       
    ## 5 27469 10003…  2010           1     37980 Philadel… New Castle… 10       
    ## 6 27470 10003…  2015           1     37980 Philadel… New Castle… 10       
    ## # … with 32 more variables: stateusps <chr>, num_under_18 <dbl>,
    ## #   ratio_students_AP_enrolled <dbl>, perc_over24_college_degree <dbl>,
    ## #   perc_18to24_nearby_college_enrolled <dbl>, perc_3to4_school_enrolled <dbl>,
    ## #   perc_high_grad <dbl>, score_third_grade_math <dbl>,
    ## #   score_third_grade_read <dbl>, perc_elementary_school_poverty <dbl>,
    ## #   perc_teacher_1and2_years <dbl>, perc_supermarket_nearby <dbl>,
    ## #   perc_green_space_access <dbl>, days_temp_above90 <dbl>,
    ## #   perc_0to64_health_insurance <dbl>, mean_ozone_amount <dbl>,
    ## #   mean_microparticle <dbl>, perc_housing_vacancy <dbl>,
    ## #   index_walkability <dbl>, perc_below100_poverty <dbl>,
    ## #   perc_household_public_assistance <dbl>, perc_home_ownership <dbl>,
    ## #   perc_over15_high_skill <dbl>, median_income <dbl>,
    ## #   perc_adults_employed <dbl>, perc_worker_commute_over1hour <dbl>,
    ## #   perc_single_parent <dbl>, num_ECE_nearby <dbl>,
    ## #   num_high_qual_ECE_nearby <dbl>, num_waste_dump_sites <dbl>,
    ## #   index_air_pollutants <dbl>, type <chr>

``` r
summary(philly)
```

    ##        id            geo_id               year       metro_areas
    ##  Min.   : 27465   Length:2954        Min.   :2010   Min.   :1   
    ##  1st Qu.: 82309   Class :character   1st Qu.:2010   1st Qu.:1   
    ##  Median :113142   Mode  :character   Median :2012   Median :1   
    ##  Mean   : 98918                      Mean   :2012   Mean   :1   
    ##  3rd Qu.:115540                      3rd Qu.:2015   3rd Qu.:1   
    ##  Max.   :116278                      Max.   :2015   Max.   :1   
    ##    area_code      msaname15         county_code         statefips        
    ##  Min.   :37980   Length:2954        Length:2954        Length:2954       
    ##  1st Qu.:37980   Class :character   Class :character   Class :character  
    ##  Median :37980   Mode  :character   Mode  :character   Mode  :character  
    ##  Mean   :37980                                                           
    ##  3rd Qu.:37980                                                           
    ##  Max.   :37980                                                           
    ##   stateusps          num_under_18    ratio_students_AP_enrolled
    ##  Length:2954        Min.   :   0.0   Min.   :0.0000            
    ##  Class :character   1st Qu.: 563.0   1st Qu.:0.1690            
    ##  Mode  :character   Median : 850.0   Median :0.2757            
    ##                     Mean   : 924.4   Mean   :0.3083            
    ##                     3rd Qu.:1189.0   3rd Qu.:0.4091            
    ##                     Max.   :4097.0   Max.   :1.8218            
    ##  perc_over24_college_degree perc_18to24_nearby_college_enrolled
    ##  Min.   : 0.00              Min.   :36.83                      
    ##  1st Qu.:17.90              1st Qu.:43.95                      
    ##  Median :30.26              Median :45.97                      
    ##  Mean   :34.02              Mean   :46.33                      
    ##  3rd Qu.:48.80              3rd Qu.:48.13                      
    ##  Max.   :97.37              Max.   :68.70                      
    ##  perc_3to4_school_enrolled perc_high_grad  score_third_grade_math
    ##  Min.   :  0.00            Min.   :20.71   Min.   : 18.16        
    ##  1st Qu.: 40.20            1st Qu.:76.35   1st Qu.:172.78        
    ##  Median : 58.30            Median :87.40   Median :221.98        
    ##  Mean   : 58.15            Mean   :83.70   Mean   :213.59        
    ##  3rd Qu.: 77.60            3rd Qu.:93.22   3rd Qu.:257.87        
    ##  Max.   :100.00            Max.   :99.63   Max.   :388.53        
    ##  score_third_grade_read perc_elementary_school_poverty perc_teacher_1and2_years
    ##  Min.   : 26.15         Min.   :  0.00                 Min.   : 0.000          
    ##  1st Qu.:158.43         1st Qu.: 19.53                 1st Qu.: 4.931          
    ##  Median :202.67         Median : 42.10                 Median : 8.706          
    ##  Mean   :198.44         Mean   : 48.07                 Mean   :10.373          
    ##  3rd Qu.:242.75         3rd Qu.: 79.08                 3rd Qu.:13.370          
    ##  Max.   :354.10         Max.   :100.00                 Max.   :96.166          
    ##  perc_supermarket_nearby perc_green_space_access days_temp_above90
    ##  Min.   : 0.0000         Min.   : 0.50           Min.   : 0.6667  
    ##  1st Qu.: 0.6635         1st Qu.:14.40           1st Qu.:16.3333  
    ##  Median : 2.4104         Median :28.30           Median :19.0000  
    ##  Mean   : 4.9748         Mean   :33.86           Mean   :19.1113  
    ##  3rd Qu.: 5.8024         3rd Qu.:49.90           3rd Qu.:23.0000  
    ##  Max.   :84.0464         Max.   :95.40           Max.   :28.3333  
    ##  perc_0to64_health_insurance mean_ozone_amount mean_microparticle
    ##  Min.   : 45.68              Min.   :34.86     Min.   : 8.773    
    ##  1st Qu.: 86.84              1st Qu.:36.94     1st Qu.:10.168    
    ##  Median : 91.66              Median :38.52     Median :10.591    
    ##  Mean   : 90.41              Mean   :38.81     Mean   :10.566    
    ##  3rd Qu.: 95.33              3rd Qu.:40.81     3rd Qu.:10.994    
    ##  Max.   :100.00              Max.   :44.30     Max.   :12.001    
    ##  perc_housing_vacancy index_walkability perc_below100_poverty
    ##  Min.   : 0.000       Min.   : 2.742    Min.   : 0.000       
    ##  1st Qu.: 3.572       1st Qu.: 9.167    1st Qu.: 4.167       
    ##  Median : 6.558       Median :13.153    Median : 8.203       
    ##  Mean   : 8.135       Mean   :12.155    Mean   :13.484       
    ##  3rd Qu.:10.776       3rd Qu.:14.776    3rd Qu.:17.980       
    ##  Max.   :65.674       Max.   :19.667    Max.   :92.453       
    ##  perc_household_public_assistance perc_home_ownership perc_over15_high_skill
    ##  Min.   : 0.000                   Min.   :  0.00      Min.   :  0.00        
    ##  1st Qu.: 3.271                   1st Qu.: 53.51      1st Qu.: 28.54        
    ##  Median : 7.357                   Median : 70.03      Median : 39.61        
    ##  Mean   :12.806                   Mean   : 67.40      Mean   : 40.54        
    ##  3rd Qu.:16.949                   3rd Qu.: 85.66      3rd Qu.: 51.77        
    ##  Max.   :72.750                   Max.   :100.00      Max.   :100.00        
    ##  median_income    perc_adults_employed perc_worker_commute_over1hour
    ##  Min.   :  9587   Min.   :  0.00       Min.   :  0.000              
    ##  1st Qu.: 47457   1st Qu.: 72.66       1st Qu.:  6.723              
    ##  Median : 69130   Median : 79.90       Median :  9.844              
    ##  Mean   : 71605   Mean   : 76.45       Mean   : 10.704              
    ##  3rd Qu.: 91099   3rd Qu.: 84.21       3rd Qu.: 13.598              
    ##  Max.   :243418   Max.   :100.00       Max.   :100.000              
    ##  perc_single_parent num_ECE_nearby    num_high_qual_ECE_nearby
    ##  Min.   :  0.00     Min.   :  2.489   Min.   : 0.000          
    ##  1st Qu.: 17.80     1st Qu.: 55.765   1st Qu.: 3.000          
    ##  Median : 30.62     Median : 97.315   Median : 6.288          
    ##  Mean   : 36.17     Mean   :214.199   Mean   : 8.808          
    ##  3rd Qu.: 50.94     3rd Qu.:353.779   3rd Qu.:11.000          
    ##  Max.   :100.00     Max.   :877.306   Max.   :34.474          
    ##  num_waste_dump_sites index_air_pollutants     type          
    ##  Min.   :0.000001     Min.   :     28      Length:2954       
    ##  1st Qu.:0.000001     1st Qu.:   2998      Class :character  
    ##  Median :0.000001     Median :   4819      Mode  :character  
    ##  Mean   :0.177713     Mean   :  11562                        
    ##  3rd Qu.:0.000001     3rd Qu.:   7634                        
    ##  Max.   :2.897079     Max.   :4838489

``` r
philly %>%  group_by(stateusps, type) %>%
  tally(sort = T) %>%
  ungroup() %>%
  arrange(desc(n)) # see the distribution among states                 
```

    ## # A tibble: 5 x 3
    ##   stateusps type             n
    ##   <chr>     <chr>        <int>
    ## 1 PA        Suburb        1228
    ## 2 PA        Philadelphia   768
    ## 3 NJ        Suburb         658
    ## 4 DE        Suburb         262
    ## 5 MD        Suburb          38

``` r
philly %>% group_by(county_code, type, stateusps) %>%
  tally(sort = T) %>%
  ungroup() %>%
  arrange(desc(n)) # see the distribution among counties
```

    ## # A tibble: 11 x 4
    ##    county_code     type         stateusps     n
    ##    <chr>           <chr>        <chr>     <int>
    ##  1 Philadelphia    Philadelphia PA          768
    ##  2 Montgomery (PA) Suburb       PA          422
    ##  3 Delaware (PA)   Suburb       PA          288
    ##  4 Bucks (PA)      Suburb       PA          286
    ##  5 New Castle (DE) Suburb       DE          262
    ##  6 Camden (NJ)     Suburb       NJ          254
    ##  7 Chester (PA)    Suburb       PA          232
    ##  8 Burlington (NJ) Suburb       NJ          228
    ##  9 Gloucester (NJ) Suburb       NJ          126
    ## 10 Salem (NJ)      Suburb       NJ           50
    ## 11 Cecil (MD)      Suburb       MD           38

``` r
philly %>% group_by(type) %>%
  tally(sort = T) %>%
  ungroup() %>%
  arrange(desc(n)) # see the distribution among Philly and suburbs
```

    ## # A tibble: 2 x 2
    ##   type             n
    ##   <chr>        <int>
    ## 1 Suburb        2186
    ## 2 Philadelphia   768

``` r
philly %>% # find amount of NAs
  select_if(function(x) any(is.na(x))) %>% 
  summarise_each(funs(sum(is.na(.))))
```

    ## # A tibble: 1 x 0

# Exploratory Graphics

``` r
philly %>% group_by(type, stateusps) %>% 
  summarize(child_population = sum(num_under_18)) %>% 
  ggplot() +
  geom_col(aes(x = type, y = child_population, fill = reorder(stateusps, child_population)))
```

    ## `summarise()` regrouping output by 'type' (override with `.groups` argument)

![](eda-philly_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
philly %>% 
  group_by(stateusps, county_code) %>% 
  summarize(child_population = sum(num_under_18)) %>% 
  ggplot(aes(x = reorder(stateusps, child_population), y = child_population, fill = county_code, label = county_code)) +
  geom_col() +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = function(x){ifelse(x>=1000000,paste0(x/1000000, "M"), paste0(x/1000, "K"))})
```

    ## `summarise()` regrouping output by 'stateusps' (override with `.groups` argument)

![](eda-philly_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
philly %>% group_by(county_code, stateusps) %>% 
  summarize(child_population = sum(num_under_18)) %>% 
  ggplot() +
  geom_col(aes(x = reorder(county_code, child_population), y = child_population, fill = stateusps)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
```

    ## `summarise()` regrouping output by 'county_code' (override with `.groups` argument)

![](eda-philly_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->

``` r
ggplot(philly, aes(x = type, y = perc_home_ownership)) +
  geom_violin() +
  geom_point(position = position_jitter(width = 0.4), alpha = .35, aes(color = stateusps))
```

![](eda-philly_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
ggplot(philly, aes(x = stateusps, y = perc_home_ownership)) +
  geom_violin() +
  geom_point(position = position_jitter(width = 0.4), alpha = .35, aes(color = type)) +
  geom_hline(yintercept = 77.06333, colour = "magenta", linetype = "longdash", size = .4)
```

![](eda-philly_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
ggplot(philly, aes(x = county_code, y = perc_home_ownership)) +
  geom_violin() +
  geom_point(position = position_jitter(width = 0.4), alpha = .35, aes(color = stateusps, shape = type)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
```

![](eda-philly_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

``` r
ggplot(philly, aes(x = reorder(county_code, perc_home_ownership), y = perc_home_ownership)) +
  geom_violin() +
  geom_point(position = position_jitter(width = 0.4), alpha = .35, aes(color = stateusps)) +
  facet_wrap(~year, nrow = 2) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
```

![](eda-philly_files/figure-gfm/unnamed-chunk-8-4.png)<!-- -->

``` r
ggplot(philly, aes(x = perc_over15_high_skill, y = perc_home_ownership)) +
  geom_point(aes(col = type)) +
  scale_y_continuous(labels = function(x){paste0(x, "%")}) +
  scale_x_continuous(labels = function(x){paste0(x, "%")})
```

![](eda-philly_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
ggplot(philly, aes(x = median_income, y = perc_home_ownership)) +
  geom_point(aes(col = type)) +
  scale_y_continuous(labels = function(x){paste0(x, "%")}) +
  scale_x_continuous(labels = function(x){paste0("$", x/1000, "K")})
```

![](eda-philly_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
ggplot(philly, aes(x = type, y = perc_home_ownership)) +
  geom_jitter(alpha=.4) +
  scale_y_continuous(labels = function(x){paste0(x, "%")})
```

![](eda-philly_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
ggplot(philly, aes(x = type, y = median_income)) +
  geom_jitter(alpha=.4, aes(col = stateusps)) +
  scale_y_continuous(labels = function(x){paste0("$", x/1000, "K")}) 
```

![](eda-philly_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
ggplot(philly, aes(x = type, y = perc_worker_commute_over1hour)) +
  geom_jitter(alpha=.4, aes(col = stateusps)) +
  labs(y = "Commute > 1 Hour") +
  scale_y_continuous(labels = function(x){paste0(x, "%")}, limits = c(0, 35)) +
  theme_classic() # violin?
```

    ## Warning: Removed 8 rows containing missing values (geom_point).

![](eda-philly_files/figure-gfm/unnamed-chunk-10-3.png)<!-- -->
