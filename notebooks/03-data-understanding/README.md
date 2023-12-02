Exploratory Data Analysis (EDA) of 2022 High Volume For-Hire Vehicles
================

- <a href="#setting-the-environment-up"
  id="toc-setting-the-environment-up">Setting the environment up</a>
- <a href="#exploring-distribution-of-each-individual-variable"
  id="toc-exploring-distribution-of-each-individual-variable">Exploring
  distribution of each individual variable</a>
  - <a href="#categorical-variables"
    id="toc-categorical-variables">Categorical variables</a>

After completing the [business
understanding](https://github.com/AngelFelizR/nyc-taxi-project/tree/master/notebooks/02-business-understanding)
step, we have a clear objective in mind and an initial description for
each column the [raw
data](https://github.com/AngelFelizR/nyc-taxi-project/tree/master/data),
we are ready to perform the *data understanding* by performing an EDA
with the following steps:

1.  Examining the distribution of each individual variable by counting
    the categorical variables and creating histograms or box plots for
    numerical variables
2.  Confirming domain knowledge relations by creating visualization with
    2 or more variables
3.  Taking a subset of the data to fit in RAM
4.  Defining the target variable and confirming its distribution
5.  Exploring correlations between predictors by using a correlation
    matrix or running a PCA
6.  Removing high correlated predictors
7.  Exploring correlations between the target and predictors creating a
    correlation funnel and some scatter plots.

After completing this process, we will have the following outcomes:

- Confirming the meaning of each variable
- Ensuring data quality by finding missing values and
- Identifying the best models to train
- Creating new features that can enhance the predictive power of the
  machine learning model

## Setting the environment up

To setting the `R` environment up we just need to apply the following 4
steps:

1.  Loading the packages to use.

``` r
library(here)
library(data.table)
library(ggplot2)
library(scales)
library(forcats)
library(dplyr)
library(arrow)
```

2.  Sourcing some custom functions created to avoid repeating myself.

``` r
source(here("R/01-custom-functions.R"))
```

3.  Creating an Arrow connection object to perform some manipulations in
    disk before taking the data into the RAM memory.

``` r
NycTrips2022 <- 
  here("data/trip-data/year=2022") |>
  open_dataset() |>
  mutate(company = case_when(
    hvfhs_license_num == "HV0002" ~ "Juno",
    hvfhs_license_num == "HV0003" ~ "Uber",
    hvfhs_license_num == "HV0004" ~ "Via",
    hvfhs_license_num == "HV0005" ~ "Lyft"
  )) |>
  select(-hvfhs_license_num)
```

4.  Importing the zone code description.

``` r
ZoneCodes <- fread(
  here("data/taxi_zone_lookup.csv"),
  colClasses = c("integer", "character", "character", "character")
)
```

## Exploring distribution of each individual variable

### Categorical variables

- `company`: The majority number of trips are done by *Uber* (HV003) and
  the rest for *Lyft*.

``` r
NycTrips2022 |> count_pct(company)
```

    # A tibble: 2 × 3
      company         n   pct
      <chr>       <int> <dbl>
    1 Uber    153847310 0.724
    2 Lyft     58568773 0.276

- `dispatching_base_num`: This column doesn’t show much information, so
  we will **erase** this column as it doesn’t show any useful
  information.

``` r
NycTrips2022 |> count_pct(dispatching_base_num)
```

    # A tibble: 29 × 3
       dispatching_base_num         n       pct
       <chr>                    <int>     <dbl>
     1 B03404               153732577 0.724    
     2 B03406                58568773 0.276    
     3 B02764                   54512 0.000257 
     4 B02872                    6078 0.0000286
     5 B02395                    4789 0.0000225
     6 B02882                    4097 0.0000193
     7 B02835                    3936 0.0000185
     8 B02870                    3887 0.0000183
     9 B02887                    3684 0.0000173
    10 B02876                    3607 0.0000170
    # ℹ 19 more rows

- `originating_base_num`: This column doesn’t show much information, so
  we will **erase** this column as it doesn’t show any useful
  information.

``` r
NycTrips2022 |> count_pct(originating_base_num)
```

    # A tibble: 37 × 3
       originating_base_num         n       pct
       <chr>                    <int>     <dbl>
     1 B03404               153730161 0.724    
     2 <NA>                  58498724 0.275    
     3 B03406                   71481 0.000337 
     4 B02764                   54511 0.000257 
     5 B02872                    6078 0.0000286
     6 B02395                    4789 0.0000225
     7 B02882                    4097 0.0000193
     8 B02835                    3936 0.0000185
     9 B02870                    3887 0.0000183
    10 B02887                    3684 0.0000173
    # ℹ 27 more rows

- `shared_request_flag`: Most of passengers don’t agree to a
  shared/pooled ride.

``` r
NycTrips2022 |> count_pct(shared_request_flag)
```

    # A tibble: 2 × 3
      shared_request_flag         n     pct
      <chr>                   <int>   <dbl>
    1 N                   210564721 0.991  
    2 Y                     1851362 0.00872

- `shared_match_flag`: Shows that actually fewer trips were shared.

``` r
NycTrips2022 |> count_pct(shared_match_flag)
```

    # A tibble: 2 × 3
      shared_match_flag         n     pct
      <chr>                 <int>   <dbl>
    1 N                 211916075 0.998  
    2 Y                    500008 0.00235

- `access_a_ride_flag`: *Uber* isn’t reporting whether their trips were
  administered on behalf of the Metropolitan Transportation Authority
  and for *Lyft* the answer is always “N”, so we will **erase** this
  column as it doesn’t show any useful information.

``` r
NycTrips2022 |> count_pct(company, access_a_ride_flag)
```

    # A tibble: 2 × 4
      company access_a_ride_flag         n   pct
      <chr>   <chr>                  <int> <dbl>
    1 Uber    " "                153847310 0.724
    2 Lyft    "N"                 58568773 0.276

- `wav_request_flag`: It’s really unusual for a passager to request a
  wheelchair-accessible vehicle.

``` r
NycTrips2022 |> count_pct(wav_request_flag)
```

    # A tibble: 2 × 3
      wav_request_flag         n     pct
      <chr>                <int>   <dbl>
    1 N                212142808 0.999  
    2 Y                   273275 0.00129

- `wav_match_flag`: 7% of trips took place in wheelchair-accessible
  vehicle which implies that there is more offers than demand.

``` r
NycTrips2022 |> count_pct(wav_match_flag)
```

    # A tibble: 2 × 3
      wav_match_flag         n    pct
      <chr>              <int>  <dbl>
    1 N              199779404 0.941 
    2 Y               12636679 0.0595

To explore the distribution of trips based start and end locations let’s
define a table and then explain the analysis into more plots.

``` r
TripsLocationSummary <-
  NycTrips2022 |>
  count(PULocationID, DOLocationID) |>
  collect() |>
  join_zones(zone_tb = ZoneCodes)
```

- `start_borough` and `end_borough`:

``` r
TripsLocationSummary[, .(n = sum(n)),
                     by = c("start_borough", "end_borough")
  ][order(n)
  ][, c("start_borough", "end_borough") := 
      lapply(.SD, \(x) factor(x, levels = unique(x, fromLast = TRUE)) ),
    .SDcols = c("start_borough", "end_borough")
  ][, end_borough := fct_rev(end_borough)] |>
  ggplot(aes(end_borough, start_borough))+
  geom_tile(aes(fill = n)) +
  geom_text(aes(label = percent(n/sum(n), accuracy = 0.01))) +
  scale_fill_gradient(low = "white", 
                      high = "red",
                      labels= comma_format())+
  scale_x_discrete(position = "top") +
  labs(title = "Distribution of Trips by Borough in NYC 2022",
       x = "Trip End", 
       y = "Trip Start", 
       fill = "Number of Trips") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(color = "black"),
        axis.title = element_text(face = "italic"))
```

![](README_files/figure-gfm/unnamed-chunk-14-1.png)
