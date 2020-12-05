For meeting Dec 6
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
df <- read_csv("/Users/jaykim/Documents/EDA/eda20-team5-project/Jaywork/data.csv")# Separate the column "msaname15" into "city", "state", "size" and "no_mean"
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
                                county_code == "06037" ~ "Los Angeles",
                                county_code == "06029" ~ "Kern"))%>%
  drop_na(county_name )


tx <- df%>%
  mutate(county_name = case_when(county_code == "48201" ~ "Harris (Houston)", 
                                 county_code == "48157" ~ "Fort Bend", 
                                 county_code == "48339" ~ "Montgomery", 
                                 county_code == "48291" ~ "Liberty", 
                                 county_code == "48473" ~ "Waller"))%>%
  drop_na(county_name)



philly <- df%>%
  mutate(county_name = case_when(county_code == "42101" ~ "Philadelphia", 
                                 county_code == "42045" ~ "Delaware", 
                                 county_code == "42029" ~ "Chester", 
                                 county_code == "42091" ~ "Montgomery",
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
    title="Household income in Philadelphia",
    subtitle="We can see how each city is above or below the state average, $78,030.84",
    x = "City",
    y = "Amount Difference"
  )+
  theme_bw()
```

![](Dec_6_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
ggplot(income_tx, aes(x = reorder(county_name, deviation), y = deviation,
           fill = deviation >0))+
  geom_bar(stat = "identity")+
  coord_flip()+
  scale_fill_discrete(name = "", labels = c("Below Average", "Above Average"))+
  labs(
    title="Household income in Houston",
    subtitle="We can see how each city is above or below the state average, $72,739.39",
    x = "City",
    y = "Amount Difference"
  )+
  theme_bw()
```

![](Dec_6_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
ggplot(income_la, aes(x = reorder(county_name, deviation), y = deviation,
           fill = deviation >0))+
  geom_bar(stat = "identity")+
  coord_flip()+
  scale_fill_discrete(name = "", labels = c("Below Average", "Above Average"))+
  labs(
    title="Household income in Los Angeles",
    subtitle="We can see how each city is above or below the state average, $74,260.12",
    x = "City",
    y = "Amount Difference"
  )+
  theme_bw()
```

![](Dec_6_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

``` r
ggplot(income_ny, aes(x = reorder(county_name, deviation), y = deviation,
           fill = deviation >0))+
  geom_bar(stat = "identity")+
  coord_flip()+
  scale_fill_discrete(name = "", labels = c("Below Average", "Above Average"))+
  labs(
    title="Household income in New York",
    subtitle="We can see how each city is above or below the state average, $65,819.36",
    x = "City",
    y = "Amount Difference"
  )+
  theme_bw()
```

![](Dec_6_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->

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

ap_sub_ny<- ny%>%
  filter(county_name != "Manhattan")
sub_ap_wealthy <- ap_ny%>%
  filter(median_income>100000)
```

Graphs: Houston:

``` r
ggplot()+
  geom_density(aes(AP_students), alpha = .2, fill="pink", data = ap_hou)+
  geom_density(aes(AP_students), alpha = .2, fill="lightblue",  data = hou_wealthy)+
  labs(
    title="Ratio of students in at least one AP course to the number of 11th/12th graders",
    subtitle="Houston",
    x="Ratio",
    y="Frequency"
  )+
 scale_x_continuous(
            breaks=seq(0, 2, 0.2)
            )+ 
  theme_classic()+
  theme(panel.grid.major.x=element_line())
```

![](Dec_6_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
ggplot()+
  geom_density(aes(AP_students), alpha = .2, fill="pink", data = ap_sub_hou)+
  geom_density(aes(AP_students), alpha = .2, fill="lightblue",  data = sub_hou_wealthy)+
  labs(
    title="Ratio of students in at least one AP course to the number of 11th/12th graders",
    subtitle="Suburbs of Houston",
    x="Ratio",
    y="Frequency"
  )+
  scale_x_continuous(
            breaks=seq(0, 2, 0.2)
            )+ 
  theme_classic()+
  theme(panel.grid.major.x=element_line())
```

![](Dec_6_files/figure-gfm/unnamed-chunk-10-2.png)<!-- --> Philadelphia
:

``` r
ggplot()+
  geom_density(aes(AP_students), alpha = .2, fill="pink", data = ap_philly)+
  geom_density(aes(AP_students), alpha = .2, fill="lightblue",  data = philly_wealthy)+
  labs(
    title="Ratio of students in at least one AP course to the number of 11th/12th graders",
    subtitle="Philadelphia",
    x="Ratio",
    y="Frequency"
  )+
 scale_x_continuous(
            breaks=seq(0, 2, 0.1)
            )+ 
  theme_classic()+
  theme(panel.grid.major.x=element_line())
```

    ## Warning: Removed 12 rows containing non-finite values (stat_density).

![](Dec_6_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
ggplot()+
  geom_density(aes(AP_students), alpha = .2, fill="pink", data = ap_sub_philly)+
  geom_density(aes(AP_students), alpha = .2, fill="lightblue",  data = sub_philly_wealthy)+
  labs(
    title="Ratio of students in at least one AP course to the number of 11th/12th graders",
    subtitle="Suburbs of Philadelphia",
    x="Ratio",
    y="Frequency"
  )+
 scale_x_continuous(
            breaks=seq(0, 2, 0.2)
            )+ 
  theme_classic()+
  theme(panel.grid.major.x=element_line())
```

    ## Warning: Removed 4 rows containing non-finite values (stat_density).

![](Dec_6_files/figure-gfm/unnamed-chunk-11-2.png)<!-- --> LA:

``` r
ggplot()+
  geom_density(aes(AP_students), alpha = .2, fill="pink", data = ap_la)+
  geom_density(aes(AP_students), alpha = .2, fill="lightblue",  data = la_wealthy)+
  labs(
    title="Ratio of students in at least one AP course to the number of 11th/12th graders.",
    subtitle="Los Angeles",
    x="Ratio",
    y="Frequency"
  )+
 scale_x_continuous(
            breaks=seq(0, 2, 0.1)
            )+ 
  theme_classic()+
  theme(panel.grid.major.x=element_line())
```

    ## Warning: Removed 46 rows containing non-finite values (stat_density).

    ## Warning: Removed 7 rows containing non-finite values (stat_density).

![](Dec_6_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
ggplot()+
  geom_density(aes(AP_students), alpha = .2, fill="pink", data = ap_sub_la)+
  geom_density(aes(AP_students), alpha = .2, fill="lightblue",  data = sub_la_wealthy)+
  labs(
    title="Ratio of students in at least one AP course to the number of 11th/12th graders",
    subtitle="Suburbs of Los Angeles",
    x="Ratio",
    y="Frequency"
  )+
 scale_x_continuous(
            breaks=seq(0, 2, 0.1)
            )+ 
  theme_classic()+
  theme(panel.grid.major.x=element_line())
```

    ## Warning: Removed 20 rows containing non-finite values (stat_density).
    
    ## Warning: Removed 7 rows containing non-finite values (stat_density).

![](Dec_6_files/figure-gfm/unnamed-chunk-12-2.png)<!-- --> NYC:

``` r
ggplot()+
  geom_density(aes(AP_students), alpha = .2, fill="pink", data = ap_sub_ny)+
  geom_density(aes(AP_students), alpha = .2, fill="lightblue",  data = sub_ap_wealthy)+
  labs(
    title="Ratio of students in at least one AP course to the number of 11th/12th graders.",
    subtitle="Suburbs of New York",
    x="Ratio",
    y="Frequency"
  )+
 scale_x_continuous(
            breaks=seq(0, 2, 0.1)
            )+ 
  theme_classic()+
  theme(panel.grid.major.x=element_line())
```

    ## Warning: Removed 64 rows containing non-finite values (stat_density).

    ## Warning: Removed 2 rows containing non-finite values (stat_density).

![](Dec_6_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
ggplot()+
  geom_density(aes(AP_students), alpha = .2, fill="pink", data = ap_ny)+
  geom_density(aes(AP_students), alpha = .2, fill="lightblue",  data = ny_wealthy)+
  labs(
    title="Ratio of students in at least one AP course to the number of 11th/12th graders.",
    subtitle="Manhattan",
    x="Ratio",
    y="Frequency"
  )+
 scale_x_continuous(
            breaks=seq(0, 2, 0.1)
            )+ 
  theme_classic()+
  theme(panel.grid.major.x=element_line())
```

    ## Warning: Removed 12 rows containing non-finite values (stat_density).
    
    ## Warning: Removed 2 rows containing non-finite values (stat_density).

![](Dec_6_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

# 3\. Grad Rates

``` r
data1 <- df%>%
  mutate(county_name = case_when(county_code == "06111" ~ "Ventura, CA",
                                county_code == "06059" ~ "Orange, CA",
                                county_code == "06071" ~ "San Bernardino, CA",
                                county_code == "06037" ~ "Los Angeles, CA",
                                county_code == "06029" ~ "Kern, CA",
                                county_code == "48201" ~ "Harris (Houston), TX", 
                                county_code == "48157" ~ "Fort Bend, TX", 
                                county_code == "48339" ~ "Montgomery, TX", 
                                county_code == "48291" ~ "Liberty, TX", 
                                county_code == "48473" ~ "Waller, TX", 
                                county_code == "42101" ~ "Philadelphia, PA", 
                                county_code == "42045" ~ "Delaware, PA", 
                                county_code == "42029" ~ "Chester, PA", 
                                county_code == "42091" ~ "Montomery, PA",
                                county_code == "42017" ~ "Bucks, PA",
                                county_code == "36061" ~ "Manhattan, NY", 
                                county_code == "36047" ~ "Brooklyn, NY", 
                                county_code == "36081" ~ "Queens, NY", 
                                county_code == "36005" ~ "Bronx, NY",
                                county_code == "36085" ~ "Staten Island, NY"))%>%
  drop_na(county_name)
```

``` r
data <- data1 %>% 
  drop_na(hs_grads)

#NYC
grad_rates_nyc<- data %>%
  filter(county_name %in% c("Manhattan, NY", "Brooklyn, NY",  "Queens, NY", "Bronx, NY", "Staten Island, NY"))%>%
  group_by(county_name,year)%>%
  summarise(mean_grad = mean(hs_grads))
```

    ## `summarise()` regrouping output by 'county_name' (override with `.groups` argument)

``` r
ny_rates_good <- grad_rates_nyc %>% 
  filter(county_name %in% c("Manhattan, NY", "Brooklyn, NY",  "Queens, NY", "Bronx, NY"))
ny_rates_bad <-  grad_rates_nyc %>% 
  filter(county_name %in% c("Staten Island, NY"))

#LA
grad_rates_la<- data %>%
  filter(county_name %in% c("Ventura, CA", "Orange, CA", "Los Angeles, CA", "San Bernardino, CA", "Kern, CA"))%>%
  group_by(county_name,year)%>%
  summarise(mean_grad = mean(hs_grads))
```

    ## `summarise()` regrouping output by 'county_name' (override with `.groups` argument)

``` r
la_rates_good <- grad_rates_la %>% 
  filter(county_name %in% c("San Bernardino, CA", "Kern, CA"))
la_rates_bad <-  grad_rates_la %>% 
  filter(county_name %in% c("Los Angeles, CA", "Orange, CA","Ventura, CA" ))

#Hou
grad_rates_hou<- data %>%
  filter(county_name %in% c("Harris (Houston), TX", "Fort Bend, TX" , "Montgomery, TX", "Liberty, TX", "Waller, TX"))%>%
  group_by(county_name,year)%>%
  summarise(mean_grad = mean(hs_grads))
```

    ## `summarise()` regrouping output by 'county_name' (override with `.groups` argument)

``` r
hou_rates_good <- grad_rates_hou %>% 
  filter(county_name %in% c("Fort Bend, TX", "Liberty, TX" ,"Waller, TX", "Harris (Houston), TX"))
hou_rates_bad <-  grad_rates_hou %>% 
  filter(county_name %in% c("Montgomery, TX" ))

#Philly
grad_rates_philly<- data %>%
  filter(county_name %in% c("Philadelphia, PA", "Delaware, PA", "Chester, PA",  "Montomery, PA", "Bucks, PA"))%>%
  group_by(county_name,year)%>%
  summarise(mean_grad = mean(hs_grads))
```

    ## `summarise()` regrouping output by 'county_name' (override with `.groups` argument)

``` r
philly_rates_good <- grad_rates_philly %>% 
  filter(county_name %in% c("Chester, PA", "Delaware, PA", "Philadelphia, PA"))
philly_rates_bad <-  grad_rates_philly %>% 
  filter(county_name %in% c("Bucks, PA", "Montomery, PA"))
```

``` r
ggplot() + 
  geom_point(data = grad_rates_nyc,aes(x = mean_grad, y = county_name, color = factor(year)), size = 4, alpha = .8)+
  geom_line(data = ny_rates_good, aes(x = mean_grad, y = county_name), arrow = arrow(length=unit(0.20,"cm"), ends="last", type = "closed"))+
  geom_line(data = ny_rates_bad, aes(x = mean_grad, y = county_name), arrow = arrow(length=unit(0.30,"cm"), ends="first", type = "closed"))+
  labs(title = "Change in high school graduation rate", 
       subtitle = "New York",
       x = "Percent Graduated",
       y = "Region")+
  theme_classic()+
  theme(panel.grid.major.x=element_line())+
   scale_x_continuous(
            breaks=seq(50, 100, 2),
            labels = function(x){paste0(x*1, '%')}
   )
```

![](Dec_6_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
ggplot() + 
  geom_point(data = grad_rates_la,aes(x = mean_grad, y = county_name, color = factor(year)), size = 4, alpha = .8)+
  geom_line(data = la_rates_good, aes(x = mean_grad, y = county_name), arrow = arrow(length=unit(0.20,"cm"), ends="last", type = "closed"))+
  geom_line(data = la_rates_bad, aes(x = mean_grad, y = county_name), arrow = arrow(length=unit(0.30,"cm"), ends="first", type = "closed"))+
  labs(title = "Change in high school graduation rate", 
       subtitle = "Los Angeles",
       x = "Percent Graduated",
       y = "Region")+
  theme_classic()+
  theme(panel.grid.major.x=element_line())+
   scale_x_continuous(
            breaks=seq(50, 100, 2),
            labels = function(x){paste0(x*1, '%')}
   )
```

![](Dec_6_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

``` r
ggplot() + 
  geom_point(data = grad_rates_hou,aes(x = mean_grad, y = county_name, color = factor(year)), size = 4, alpha = .8)+
  geom_line(data = hou_rates_good, aes(x = mean_grad, y = county_name), arrow = arrow(length=unit(0.20,"cm"), ends="last", type = "closed"))+
  geom_line(data = hou_rates_bad, aes(x = mean_grad, y = county_name), arrow = arrow(length=unit(0.30,"cm"), ends="first", type = "closed"))+
  labs(title = "Change in high school graduation rate", 
       subtitle = "Houston",
       x = "Percent Graduated",
       y = "Region")+
  theme_classic()+
  theme(panel.grid.major.x=element_line())+
   scale_x_continuous(
            breaks=seq(50, 100, 2),
            labels = function(x){paste0(x*1, '%')}
   )
```

![](Dec_6_files/figure-gfm/unnamed-chunk-16-3.png)<!-- -->

``` r
ggplot() + 
  geom_point(data = grad_rates_philly,aes(x = mean_grad, y = county_name, color = factor(year)), size = 4, alpha = .8)+
  geom_line(data = philly_rates_good, aes(x = mean_grad, y = county_name), arrow = arrow(length=unit(0.20,"cm"), ends="last", type = "closed"))+
  geom_line(data = philly_rates_bad, aes(x = mean_grad, y = county_name), arrow = arrow(length=unit(0.30,"cm"), ends="first", type = "closed"))+
  labs(title = "Change in high school graduation rate", 
       subtitle = "Philadelphia",
       x = "Percent Graduated",
       y = "Region")+
  theme_classic()+
  theme(panel.grid.major.x=element_line())+
   scale_x_continuous(
            breaks=seq(50, 100, 2),
            labels = function(x){paste0(x*1, '%')}
   )
```

![](Dec_6_files/figure-gfm/unnamed-chunk-16-4.png)<!-- -->