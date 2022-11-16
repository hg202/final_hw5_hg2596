final_hw5_hg2596
================
2022-11-16

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(readxl)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(tidyr)
library(dplyr)
library(plotly)
```

    ## 
    ## Attaching package: 'plotly'
    ## 
    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot
    ## 
    ## The following object is masked from 'package:stats':
    ## 
    ##     filter
    ## 
    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

# Problem 2

``` r
homicide_1= read_csv("./data/homicide_data.csv", show_col_types = FALSE)
```

# Create a city_state variable

``` r
homicide_1 = homicide_1 %>%
  unite('city_state', city:state, remove = FALSE) %>% 
  apply(., 2, function(city_state) as.character(gsub("_", ",", city_state))) 
```

The homicide raw data has 52179observations and 13 variables. Key
variables

``` r
homicide_2 = as.tibble(homicide_1) %>%
  janitor::clean_names() %>%
  mutate(victim_age = as.numeric(victim_age)) %>%
  mutate(lat = as.numeric(lat)) %>%
  mutate(lon = as.numeric(lon)) %>%
  mutate(status = ifelse(disposition%in%c("Closed without arrest","Open/No arrest"), 1, 0))
```

    ## Warning: `as.tibble()` was deprecated in tibble 2.0.0.
    ## Please use `as_tibble()` instead.
    ## The signature and semantics have changed, see `?as_tibble`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

``` r
sum_hc = homicide_2 %>%
  group_by(city) %>%
  summarize(n_obs = n(), 
            n_unsolved = sum(status)) 
```

``` r
baltimore_hc = sum_hc %>% 
  filter(city == "Baltimore") 
```

# Prop test Baltimore

``` r
first_prop = prop.test(x = pull(baltimore_hc,n_unsolved), n = pull(baltimore_hc,n_obs)) %>%
  broom::tidy()
```

For Baltimore, the probability of having an unsolved case is .
