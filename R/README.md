# R

This folder contains the R scripts that define custom functions and values for the nyc-taxi-project.

## 01-custom-functions.R

This script defines four functions that are used to decode, join, plot, and transform the trip and zone tables.

- `decode_business(trip_dt)` takes a trip table as input and replaces the company codes with the company names. It also removes the `hvfhs_license_num` column from the trip table.
- `decode_zones(trip_dt, zone_dt)` takes a trip table and a zone table as input and joins them based on the pickup and dropoff location IDs. It also removes the `PULocationID` and `DOLocationID` columns from the trip table.
- `count_dt(dt, x)` takes a data table and a variable name as input and returns a summary table with the count and percentage of each level of the variable. It also groups the levels with low frequency into an "Other" category and orders them by frequency.
- `plot_chr_count(dt, count_var, ...)` takes a data table and one or more variable names as input and returns a ggplot object with a horizontal bar chart for each variable. It uses the `count_dt` function to calculate the counts and percentages and accepts additional arguments for customization.
- `hist_low_tail(dt, var_name, ...)` takes a data table and a variable name as input and returns a ggplot object with a histogram of the low tail values of the variable. It accepts additional arguments for customization.
- `trans_after_eda(DT)` takes a data table as input and performs some feature engineering steps after exploratory data analysis. It removes unnecessary columns, recodes some categorical variables, and groups some levels into "Other".

## 02-custom-values.R

This script defines some custom values that are used in the data analysis and visualization.

- `ZoneCodesColTypes` is a named vector that specifies the column types for the zone table.
- `custom_theme` is a ggplot2 theme that uses a light background and bold plot titles.
- `plus1_log2_trans` is a custom transformation function that applies log2(x+1) to the data and its inverse to the labels.
