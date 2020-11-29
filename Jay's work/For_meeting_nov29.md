For meeting Nov 29
================
Jay Kim

# Necessary Libraries

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.4     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(viridis)
```

    ## Loading required package: viridisLite

``` r
# Import data
library(readr)
df <- read_csv("/Users/jaykim/Documents/EDA/eda20-team5-project/Jay's work/data.csv")# Separate the column "msaname15" into "city", "state", "size" and "no_mean"
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   geoid = col_character(),
    ##   msaname15 = col_character(),
    ##   countyfips = col_character(),
    ##   statefips = col_character(),
    ##   stateusps = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
df <- df %>% 
  separate(msaname15, c("city", "state", "size", "no_mean"), sep = " ")
```

    ## Warning: Expected 4 pieces. Additional pieces discarded in 61334 rows [337, 338,
    ## 339, 340, 341, 342, 343, 344, 345, 346, 347, 348, 349, 350, 351, 352, 353, 354,
    ## 355, 356, ...].

``` r
df<- df %>% 
  mutate(num_edu_center = exp(ED_PRXECE)) %>%
  mutate(num_good_edu_center = exp(ED_PRXHQECE)) %>%
  mutate(num_waste_dump_site = exp(HE_SUPRFND)) %>%
  mutate(population = exp(HE_RSEI)) %>%
  select(-ED_PRXECE, -ED_PRXHQECE, -HE_SUPRFND, -HE_RSEI)

df <- df %>% 
  rename(id = `_id`) %>% 
  rename(geo_id = geoid) %>% 
  rename(metro_areas = in100) %>%
  rename(area_code = msaid15) %>% 
  rename(county_code = countyfips) %>%
  rename(num_under_18 = pop) %>% 
  rename(AP_students = ED_APENR) %>%
  rename(college_deg = ED_ATTAIN) %>%
  rename(num_college_enrolled = ED_COLLEGE) %>%
  rename(preschoolers = ED_ECENROL) %>%
  rename(hs_grads = ED_HSGRAD) %>%
  rename(third_g_math = ED_MATH) %>%
  rename(third_g_read = ED_READING) %>%
  rename(elementary_school_poverty = ED_SCHPOV) %>% 
  rename(firstsecond_y_teachers =ED_TEACHXP) %>% 
  rename(supermarket_nearby = HE_FOOD) %>% 
  rename(green_spaces = HE_GREEN) %>%
  rename(days_temp_above90 = HE_HEAT) %>%
  rename(phealth_insurance = HE_HLTHINS) %>%
  rename(mean_ozone_amount = HE_OZONE) %>%
  rename(mean_microparticle = HE_PM25) %>%
  rename(housing_vacancy = HE_VACANCY) %>%
  rename(walkability = HE_WALK) %>% 
  rename(below100_poverty = SE_POVRATE) %>%
  rename(public_assistance = SE_PUBLIC) %>%
  rename(home_ownership = SE_HOME) %>%
  rename(perc_over15_high_skill = SE_OCC) %>%
  rename(median_income=SE_MHE) %>%
  rename(employment_rate = SE_EMPRAT) %>%
  rename(commute_over1hour = SE_JOBPROX) %>%
  rename(single_parents = SE_SINGLE)

df <- df %>% select(-state, -no_mean, -statefips, -num_waste_dump_site)

extra_NA<- df %>% 
  select_if(function(x) any(is.na(x))) %>% 
  summarise_each(funs(sum(is.na(.))))
```

    ## Warning: `summarise_each_()` is deprecated as of dplyr 0.7.0.
    ## Please use `across()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

    ## Warning: `funs()` is deprecated as of dplyr 0.8.0.
    ## Please use a list of either functions or lambdas: 
    ## 
    ##   # Simple named list: 
    ##   list(mean = mean, median = median)
    ## 
    ##   # Auto named with `tibble::lst()`: 
    ##   tibble::lst(mean, median)
    ## 
    ##   # Using lambdas
    ##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

``` r
NA_subset <- df[rowSums(is.na(df)) > 0, ]

# Delete some rows having more NAs than meaningful values.
df <- df %>% filter((geo_id != 47155980100) & (geo_id != 47145980100) & (geo_id != 47031980100) & (geo_id != 47029980100) & (geo_id != 47009980200) & (geo_id != 47009980100) & (geo_id != 47001980100) & (geo_id != 47037980100) & (geo_id != 47037980200) & (geo_id != 47037013000))
```

``` r
library(tidyverse)
library(ggplot2)
library(readr)
library(ggalt)
```

    ## Registered S3 methods overwritten by 'ggalt':
    ##   method                  from   
    ##   grid.draw.absoluteGrob  ggplot2
    ##   grobHeight.absoluteGrob ggplot2
    ##   grobWidth.absoluteGrob  ggplot2
    ##   grobX.absoluteGrob      ggplot2
    ##   grobY.absoluteGrob      ggplot2

1.  Median Income bar
2.  AP course - family income
3.  Grad Rates

County Fips Codes:

``` r
la <- df%>%
  mutate(county_name = case_when(county_code == "06111" ~ "Ventura",
                                county_code == "06059" ~ "Orange",
                                county_code == "06071" ~ "San Bernardino",
                                county_code == "06037" ~ "Los Angeles"))%>%
  drop_na(county_name )


tx <- df%>%
  mutate(county_name = case_when(county_code == "48201" ~ "Harris (Houston)", 
                                 county_code == "48157" ~ "Fort Bend", 
                                 county_code == "48339" ~ "Montgomery", 
                                 county_code == "48167" ~ "Galveston"))%>%
  drop_na(county_name)


philly <- df%>%
  mutate(county_name = case_when(county_code == "42101" ~ "Philadelphia", 
                                 county_code == "42045" ~ "Delaware", 
                                 county_code == "42029" ~ "Chester", 
                                 county_code == "42091" ~ "Montomery",
                                 county_code == "42017" ~ "Bucks"))%>%
  drop_na(county_name)

ny <- df%>%
  mutate(county_name = case_when(county_code == "36061" ~ "Manhattan", 
                                 county_code == "36047" ~ "Brooklyn", 
                                 county_code == "36081" ~ "Queens", 
                                 county_code == "36005" ~ "Bronx",
                                 county_code == "36085" ~ "Staten Island"))%>%
  drop_na(county_name)
```

# 1\. Median Income bar

Data manip

``` r
A <- philly %>%
  drop_na(median_income)
income_Philly <- A %>%
  group_by(county_name)%>%
  summarise(mean_inc = mean(median_income))%>%
  mutate(deviation= mean_inc - 78030.84 )%>%
  arrange(desc(mean_inc))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
B <- tx %>%
  drop_na(median_income)
income_tx <- B %>%
  group_by(county_name)%>%
  summarise(mean_inc = mean(median_income))%>%
  mutate(deviation= mean_inc - 72739.39)%>%
  arrange(desc(mean_inc))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
C <- la %>%
  drop_na(median_income)
income_la <- C %>%
  group_by(county_name)%>%
  summarise(mean_inc = mean(median_income))%>%
  mutate(deviation= mean_inc - 74260.12)%>%
  arrange(desc(mean_inc))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
D <- ny %>%
  drop_na(median_income)
income_ny <- D %>%
  group_by(county_name)%>%
  summarise(mean_inc = mean(median_income))%>%
  mutate(deviation= mean_inc - 65819.36)%>%
  arrange(desc(mean_inc))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
ggplot(income_Philly, aes(x = reorder(county_name, deviation), y = deviation,
           fill = deviation >0))+
  geom_bar(stat = "identity")+
  coord_flip()+
  scale_fill_discrete(name = "", labels = c("Below Average", "Above Average"))+
  labs(
    title="Household income",
    subtitle="We can see how each city is above or below the state average, $78,030.84",
    x = "City",
    y = "Amount Difference"
  )+
  theme_bw()
```

![](For_meeting_nov29_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
ggplot(income_tx, aes(x = reorder(county_name, deviation), y = deviation,
           fill = deviation >0))+
  geom_bar(stat = "identity")+
  coord_flip()+
  scale_fill_discrete(name = "", labels = c("Below Average", "Above Average"))+
  labs(
    title="Household income",
    subtitle="We can see how each city is above or below the state average, $72,739.39",
    x = "City",
    y = "Amount Difference"
  )+
  theme_bw()
```

![](For_meeting_nov29_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
ggplot(income_la, aes(x = reorder(county_name, deviation), y = deviation,
           fill = deviation >0))+
  geom_bar(stat = "identity")+
  coord_flip()+
  scale_fill_discrete(name = "", labels = c("Below Average", "Above Average"))+
  labs(
    title="Household income",
    subtitle="We can see how each city is above or below the state average, $74,260.12",
    x = "City",
    y = "Amount Difference"
  )+
  theme_bw()
```

![](For_meeting_nov29_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

``` r
ggplot(income_ny, aes(x = reorder(county_name, deviation), y = deviation,
           fill = deviation >0))+
  geom_bar(stat = "identity")+
  coord_flip()+
  scale_fill_discrete(name = "", labels = c("Below Average", "Above Average"))+
  labs(
    title="Household income",
    subtitle="We can see how each city is above or below the state average, $65,819.36",
    x = "City",
    y = "Amount Difference"
  )+
  theme_bw()
```

![](For_meeting_nov29_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->

# 2\. AP course - family income

data manip: phill

``` r
ap_philly<- philly%>%
  filter(county_name== "Philadelphia")
philly_wealthy <- ap_philly%>%
  filter(median_income>100000)

ap_sub_philly<- philly%>%
  filter(county_name != "Philadelphia")
sub_philly_wealthy <- ap_philly%>%
  filter(median_income>100000)
```

la

``` r
ap_la<- la%>%
  filter(county_name== "Los Angeles")
la_wealthy <- ap_la%>%
  filter(median_income>100000)

ap_sub_la<- la%>%
  filter(county_name != "Los Angeles")
sub_la_wealthy <- ap_la%>%
  filter(median_income>100000)
```

houston

``` r
ap_hou<- tx%>%
  filter(county_name== "Harris (Houston)")
hou_wealthy <- ap_hou%>%
  filter(median_income>90000)

ap_sub_hou<- tx%>%
  filter(county_name != "Harris (Houston)")
sub_hou_wealthy <- ap_hou%>%
  filter(median_income>90000)
```

NY

``` r
ap_ny<- ny%>%
  filter(county_name== "Manhattan")
ny_wealthy <- ap_ny%>%
  filter(median_income>100000)

ap_b_ny<- ny%>%
  filter(county_name == "Bronx")
b_ny_wealthy <- ap_ny%>%
  filter(median_income>100000)

ap_k_ny<- ny%>%
  filter(county_name == "Brooklyn")
k_ny_wealthy <- ap_ny%>%
  filter(median_income>100000)

ap_s_ny<- ny%>%
  filter(county_name == "Staten Island")
s_ny_wealthy <- ap_ny%>%
  filter(median_income>100000)

ap_q_ny<- ny%>%
  filter(county_name == "Queens")
q_ny_wealthy <- ap_ny%>%
  filter(median_income>100000)
```

Graphs: Houston:

``` r
ggplot()+
  geom_density(aes(AP_students), alpha = .2, fill="pink", data = ap_hou)+
  geom_density(aes(AP_students), alpha = .2, fill="lightblue",  data = hou_wealthy)+
  labs(
    title="Ratio of students enrolled in at least one AP course to the number of 11th and 12th graders.",
    subtitle="Philadelphia",
    x="% in AP courses",
    y="Frequency"
  )+
  theme_classic()+
  theme(panel.grid.major.x=element_line())
```

![](For_meeting_nov29_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
ggplot()+
  geom_density(aes(AP_students), alpha = .2, fill="pink", data = ap_sub_hou)+
  geom_density(aes(AP_students), alpha = .2, fill="lightblue",  data = sub_hou_wealthy)+
  labs(
    title="Ratio of students enrolled in at least one AP course to the number of 11th and 12th graders.",
    subtitle="Suburbs of Philadelphia",
    x="% in AP courses",
    y="Frequency"
  )+
  theme_classic()+
  theme(panel.grid.major.x=element_line())
```

    ## Warning: Removed 2 rows containing non-finite values (stat_density).

![](For_meeting_nov29_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->
Philadelphia :

``` r
ggplot()+
  geom_density(aes(AP_students), alpha = .2, fill="pink", data = ap_philly)+
  geom_density(aes(AP_students), alpha = .2, fill="lightblue",  data = philly_wealthy)+
  labs(
    title="Ratio of students enrolled in at least one AP course to the number of 11th and 12th graders.",
    subtitle="Philadelphia",
    x="% in AP courses",
    y="Frequency"
  )+
  theme_classic()+
  theme(panel.grid.major.x=element_line())
```

    ## Warning: Removed 12 rows containing non-finite values (stat_density).

![](For_meeting_nov29_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
ggplot()+
  geom_density(aes(AP_students), alpha = .2, fill="pink", data = ap_sub_philly)+
  geom_density(aes(AP_students), alpha = .2, fill="lightblue",  data = sub_philly_wealthy)+
  labs(
    title="Ratio of students enrolled in at least one AP course to the number of 11th and 12th graders.",
    subtitle="Suburbs of Philadelphia",
    x="% in AP courses",
    y="Frequency"
  )+
  theme_classic()+
  theme(panel.grid.major.x=element_line())
```

    ## Warning: Removed 4 rows containing non-finite values (stat_density).

![](For_meeting_nov29_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->
LA:

``` r
ggplot()+
  geom_density(aes(AP_students), alpha = .2, fill="pink", data = ap_la)+
  geom_density(aes(AP_students), alpha = .2, fill="lightblue",  data = la_wealthy)+
  labs(
    title="Ratio of students enrolled in at least one AP course to the number of 11th and 12th graders.",
    subtitle="Los Angeles",
    x="% in AP courses",
    y="Frequency"
  )+
  theme_classic()+
  theme(panel.grid.major.x=element_line())
```

    ## Warning: Removed 46 rows containing non-finite values (stat_density).

    ## Warning: Removed 7 rows containing non-finite values (stat_density).

![](For_meeting_nov29_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
ggplot()+
  geom_density(aes(AP_students), alpha = .2, fill="pink", data = ap_sub_la)+
  geom_density(aes(AP_students), alpha = .2, fill="lightblue",  data = sub_la_wealthy)+
  labs(
    title="Ratio of students enrolled in at least one AP course to the number of 11th and 12th graders.",
    subtitle="Suburbs of Los Angeles",
    x="% in AP courses",
    y="Frequency"
  )+
  theme_classic()+
  theme(panel.grid.major.x=element_line())
```

    ## Warning: Removed 14 rows containing non-finite values (stat_density).
    
    ## Warning: Removed 7 rows containing non-finite values (stat_density).

![](For_meeting_nov29_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->
NYC:

``` r
ggplot()+
  geom_density(aes(AP_students), alpha = .2, fill="pink", data = ap_q_ny)+
  geom_density(aes(AP_students), alpha = .2, fill="lightblue",  data = q_ny_wealthy)+
  labs(
    title="Ratio of students enrolled in at least one AP course to the number of 11th and 12th graders.",
    subtitle="Queens",
    x="% in AP courses",
    y="Frequency"
  )+
  theme_classic()+
  theme(panel.grid.major.x=element_line())
```

    ## Warning: Removed 36 rows containing non-finite values (stat_density).

    ## Warning: Removed 2 rows containing non-finite values (stat_density).

![](For_meeting_nov29_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
ggplot()+
  geom_density(aes(AP_students), alpha = .2, fill="pink", data = ap_s_ny)+
  geom_density(aes(AP_students), alpha = .2, fill="lightblue",  data = s_ny_wealthy)+
  labs(
    title="Ratio of students enrolled in at least one AP course to the number of 11th and 12th graders.",
    subtitle="Staten Island",
    x="% in AP courses",
    y="Frequency"
  )+
  theme_classic()+
  theme(panel.grid.major.x=element_line())
```

    ## Warning: Removed 4 rows containing non-finite values (stat_density).
    
    ## Warning: Removed 2 rows containing non-finite values (stat_density).

![](For_meeting_nov29_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

``` r
ggplot()+
  geom_density(aes(AP_students), alpha = .2, fill="pink", data = ap_ny)+
  geom_density(aes(AP_students), alpha = .2, fill="lightblue",  data = ny_wealthy)+
  labs(
    title="Ratio of students enrolled in at least one AP course to the number of 11th and 12th graders.",
    subtitle="Manhattan",
    x="% in AP courses",
    y="Frequency"
  )+
  theme_classic()+
  theme(panel.grid.major.x=element_line())
```

    ## Warning: Removed 12 rows containing non-finite values (stat_density).
    
    ## Warning: Removed 2 rows containing non-finite values (stat_density).

![](For_meeting_nov29_files/figure-gfm/unnamed-chunk-13-3.png)<!-- -->

``` r
ggplot()+
  geom_density(aes(AP_students), alpha = .2, fill="pink", data = ap_b_ny)+
  geom_density(aes(AP_students), alpha = .2, fill="lightblue",  data = b_ny_wealthy)+
  labs(
    title="Ratio of students enrolled in at least one AP course to the number of 11th and 12th graders.",
    subtitle="Bronx",
    x="% in AP courses",
    y="Frequency"
  )+
  theme_classic()+
  theme(panel.grid.major.x=element_line())
```

    ## Warning: Removed 8 rows containing non-finite values (stat_density).
    
    ## Warning: Removed 2 rows containing non-finite values (stat_density).

![](For_meeting_nov29_files/figure-gfm/unnamed-chunk-13-4.png)<!-- -->

``` r
ggplot()+
  geom_density(aes(AP_students), alpha = .2, fill="pink", data = ap_k_ny)+
  geom_density(aes(AP_students), alpha = .2, fill="lightblue",  data = k_ny_wealthy)+
  labs(
    title="Ratio of students enrolled in at least one AP course to the number of 11th and 12th graders.",
    subtitle="Brookyn",
    x="% in AP courses",
    y="Frequency"
  )+
  theme_classic()+
  theme(panel.grid.major.x=element_line())
```

    ## Warning: Removed 16 rows containing non-finite values (stat_density).
    
    ## Warning: Removed 2 rows containing non-finite values (stat_density).

![](For_meeting_nov29_files/figure-gfm/unnamed-chunk-13-5.png)<!-- -->

# 3\. Grad Rates

``` r
data1 <- df%>%
  mutate(county_name = case_when(county_code == "06111" ~ "Ventura",
                                county_code == "06059" ~ "Orange",
                                county_code == "06071" ~ "San Bernardino",
                                county_code == "06037" ~ "Los Angeles",
                                county_code == "48201" ~ "Harris (Houston)", 
                                county_code == "48157" ~ "Fort Bend", 
                                county_code == "48339" ~ "Montgomery", 
                                county_code == "48167" ~ "Galveston",
                                county_code == "42101" ~ "Philadelphia", 
                                county_code == "42045" ~ "Delaware", 
                                county_code == "42029" ~ "Chester", 
                                county_code == "42091" ~ "Montomery",
                                county_code == "42017" ~ "Bucks",
                                county_code == "36061" ~ "Manhattan", 
                                county_code == "36047" ~ "Brooklyn", 
                                county_code == "36081" ~ "Queens", 
                                county_code == "36005" ~ "Bronx",
                                county_code == "36085" ~ "Staten Island"))%>%
  drop_na(county_name)
data1
```

    ## # A tibble: 15,250 x 39
    ##       id geo_id  year metro_areas area_code city  size  county_code stateusps
    ##    <dbl> <chr>  <dbl>       <dbl>     <dbl> <chr> <chr> <chr>       <chr>    
    ##  1  9463 06037…  2010           1     31080 Los   Beac… 06037       CA       
    ##  2  9464 06037…  2015           1     31080 Los   Beac… 06037       CA       
    ##  3  9465 06037…  2010           1     31080 Los   Beac… 06037       CA       
    ##  4  9466 06037…  2015           1     31080 Los   Beac… 06037       CA       
    ##  5  9467 06037…  2010           1     31080 Los   Beac… 06037       CA       
    ##  6  9468 06037…  2015           1     31080 Los   Beac… 06037       CA       
    ##  7  9469 06037…  2010           1     31080 Los   Beac… 06037       CA       
    ##  8  9470 06037…  2015           1     31080 Los   Beac… 06037       CA       
    ##  9  9471 06037…  2010           1     31080 Los   Beac… 06037       CA       
    ## 10  9472 06037…  2015           1     31080 Los   Beac… 06037       CA       
    ## # … with 15,240 more rows, and 30 more variables: num_under_18 <dbl>,
    ## #   AP_students <dbl>, college_deg <dbl>, num_college_enrolled <dbl>,
    ## #   preschoolers <dbl>, hs_grads <dbl>, third_g_math <dbl>, third_g_read <dbl>,
    ## #   elementary_school_poverty <dbl>, firstsecond_y_teachers <dbl>,
    ## #   supermarket_nearby <dbl>, green_spaces <dbl>, days_temp_above90 <dbl>,
    ## #   phealth_insurance <dbl>, mean_ozone_amount <dbl>, mean_microparticle <dbl>,
    ## #   housing_vacancy <dbl>, walkability <dbl>, below100_poverty <dbl>,
    ## #   public_assistance <dbl>, home_ownership <dbl>,
    ## #   perc_over15_high_skill <dbl>, median_income <dbl>, employment_rate <dbl>,
    ## #   commute_over1hour <dbl>, single_parents <dbl>, num_edu_center <dbl>,
    ## #   num_good_edu_center <dbl>, population <dbl>, county_name <chr>

``` r
data <-data1 %>% 
  drop_na(hs_grads)

#average grad rates in 2015
grad_rates_15<- data %>%
  filter(county_name %in% c("Ventura", "Orange", "Los Angeles", "San Bernardino", "Harris", "Fort Bend" , "Montgomery", "Galveston", "Philadelphia", "Delaware", "Chester",  "Montomery", "Bucks" ,"Manhattan", "Brooklyn",  "Queens", "Bronx", "Staten Island"))%>%
  filter(year == '2015')%>%
  group_by(county_name,year)%>%
  summarise(mean_grad_2015 = mean(hs_grads))
```

    ## `summarise()` regrouping output by 'county_name' (override with `.groups` argument)

``` r
#average grad rates in 2010
grad_rates_10<- data %>%
  filter(county_name %in% c("Ventura", "Orange", "Los Angeles", "San Bernardino", "Harris", "Fort Bend" , "Montgomery", "Galveston", "Philadelphia", "Delaware", "Chester",  "Montomery", "Bucks" ,"Manhattan", "Brooklyn",  "Queens", "Bronx", "Staten Island"))%>%
  filter(year == '2010')%>%
  group_by(county_name,year)%>%
  summarise(mean_grad_2010 = mean(hs_grads))
```

    ## `summarise()` regrouping output by 'county_name' (override with `.groups` argument)

``` r
grad_rate <- inner_join(grad_rates_10, grad_rates_15, by= "county_name")%>%
  select(county_name, year.x, year.y, mean_grad_2010, mean_grad_2015)
```

``` r
ggplot(grad_rate, aes(y=county_name, x = mean_grad_2015, xend = mean_grad_2010)) + 
        geom_dumbbell(color= "olivedrab", 
                      size=0.75, 
                      point.colour.l="black") +
  labs(title = "Change in high school graduation rate", 
       x = "Percent Graduated",
       y = "Region")+
  theme_classic()+
  theme(panel.grid.major.x=element_line())+
   scale_x_continuous(
            breaks=seq(50, 100, 2),
            labels = function(x){paste0(x*1, '%')}
            )
```

    ## Warning: Ignoring unknown parameters: point.colour.l

![](For_meeting_nov29_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->
