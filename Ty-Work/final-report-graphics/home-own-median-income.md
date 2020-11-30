Home Ownership and Median Income
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
library(viridis)
```

    ## Loading required package: viridisLite

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

# Create Data Frames

``` r
# Philly
philly <- df %>% 
  filter(msaname15 == "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD Metro Area") %>% 
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

philly$county_code = factor(philly$county_code, levels = c("New Castle (DE)", "Cecil (MD)", "Burlington (NJ)", "Camden (NJ)", "Gloucester (NJ)", "Salem (NJ)", "Bucks (PA)", "Chester (PA)", "Delaware (PA)", "Montgomery (PA)", "Philadelphia"))

# NYC
nyc <- df %>% filter(
    county_code == '36061' |
    county_code ==  '36047' |
    county_code ==  '36081' |
    county_code ==  '36005' |
    county_code ==  '36085' 
)
nyc <- nyc %>%
  mutate(county_code = replace(county_code, county_code == '36061', 'Manhattan')) %>% 
  mutate(county_code = replace(county_code, county_code == '36047', 'Brooklyn')) %>%
  mutate(county_code =  replace(county_code, county_code == '36081', 'Queens')) %>%
  mutate(county_code =  replace(county_code, county_code == '36005', 'Bronx')) %>%
  mutate(county_code =  replace(county_code, county_code == '36085', 'Staten Island'))%>%
  mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))

nyc$county_code = factor(nyc$county_code, levels = c("Manhattan", "Brooklyn", "Queens", "Bronx", "Staten Island"))

# Houston
houston <- df %>% filter((county_code == "48201") | (county_code == "48157")| (county_code == "48339")|(county_code == "48167")|(county_code == "48039")|(county_code == "48291")|(county_code == "48473")|(county_code == "48071")) %>% 
  mutate(group = ifelse(county_code == "48201", "city", "suburb")) %>%
  mutate(county_name = case_when(county_code == "48201" ~ "Harris", 
                                 county_code == "48157" ~ "Fort Bend", 
                                 county_code == "48339" ~ "Montgomery", 
                                 county_code == "48167" ~ "Galveston", 
                                 county_code == "48039" ~ "Brazoria", 
                                 county_code == "48291" ~ "Liberty", 
                                 county_code == "48473" ~ "Waller", 
                                 county_code == "48071" ~ "Chambers")) %>% 
  mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))

houston$county_name = factor(houston$county_name, levels = c("Harris", "Fort Bend", "Galveston", "Montgomery", "Brazoria", "Chambers", "Waller", "Liberty"))

# Los Angeles
la <- df %>%
  mutate(county_name = case_when(county_code == "06111" ~ "Ventura",
                                county_code == "06059" ~ "Orange",
                                county_code == "06071" ~ "San Bernardino",
                                county_code == "06037" ~ "Los Angeles"))%>%
  drop_na(county_name ) %>% 
  mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))

la$county_name = factor(la$county_name, levels = c("Ventura", "Orange", "San Bernardino", "Los Angeles"))
```

# Final Graphics

## Philadelphia

``` r
ggplot(philly, aes(x = reorder(county_code, median_income), y = median_income)) +
  geom_violin() +
  geom_point(position = position_jitter(width = 0.3), alpha = .3, aes(color = perc_home_ownership)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_color_viridis(name = "Home Ownership",
                       labels = function(x){paste0(x, "%")})+
  scale_y_continuous(labels = function(x){paste0("$", x/1000, "K")},
                     breaks = seq(0,300000,50000)) +
    labs(
    x = "County",
    y = "Median Income",
    title = "Home Ownership in Philadelphia and Suburbs"
  ) +
  geom_hline(yintercept = median(philly$median_income), color = "red", linetype = "dashed")
```

![](home-own-median-income_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## New York City

``` r
ggplot(nyc, aes(x = reorder(county_code, median_income), y = median_income)) +
  geom_violin() +
  geom_point(position = position_jitter(width = 0.4), alpha = .3, aes(color = perc_home_ownership)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_color_viridis(name = "Home Ownership",
                       labels = function(x){paste0(x, "%")})+
  scale_y_continuous(labels = function(x){paste0("$", x/1000, "K")}, 
                     breaks = seq(0,300000,50000)) +
    labs(
    x = "County",
    y = "Median Income",
    title = "Home Ownership in NYC and Boroughs"
  ) +
  geom_hline(yintercept = median(nyc$median_income), color = "red", linetype = "dashed")
```

![](home-own-median-income_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Houston

``` r
ggplot(houston, aes(x = reorder(county_name, median_income), y = median_income)) +
  geom_violin() +
  geom_point(position = position_jitter(width = 0.4), alpha = .3, aes(color = perc_home_ownership)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_color_viridis(name = "Home Ownership",
                       labels = function(x){paste0(x, "%")})+
  scale_y_continuous(labels = function(x){paste0("$", x/1000, "K")}, 
                     breaks = seq(0,300000,50000)) +
    labs(
    x = "County",
    y = "Median Income",
    title = "Home Ownership in Houston and Suburbs"
  ) +
  geom_hline(yintercept = median(houston$median_income), color = "red", linetype = "dashed")
```

![](home-own-median-income_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

## Los Angeles

``` r
ggplot(la, aes(x = reorder(county_name, median_income), y = median_income)) +
  geom_violin() +
  geom_point(position = position_jitter(width = 0.4), alpha = .3, aes(color = perc_home_ownership)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_color_viridis(name = "Home Ownership",
                       labels = function(x){paste0(x, "%")})+
  scale_y_continuous(labels = function(x){paste0("$", x/1000, "K")}, 
                     breaks = seq(0,300000,50000)) +
    labs(
    x = "County",
    y = "Median Income",
    title = "Home Ownership in Los Angeles and Suburbs"
  ) +
  geom_hline(yintercept = median(la$median_income), color = "red", linetype = "dashed")
```

![](home-own-median-income_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
