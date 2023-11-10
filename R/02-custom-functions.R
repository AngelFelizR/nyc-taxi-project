
# 1. Importing -----

decode_business <- function(trip_dt){

  # To avoid side effects
  trip_dt <- copy(trip_dt)

  # Define company codes
  company_code <- c(
    "HV0002" = "Juno",
    "HV0003" = "Uber",
    "HV0004" = "Via",
    "HV0005" = "Lyft"
  )

  # Replace company codes in trip table with company names
  trip_dt[, `:=`(company = company_code[hvfhs_license_num],
                 hvfhs_license_num = NULL)]

  return(trip_dt)

}


# Description
## It decodes columns in a trip table using a company code and zone table.
## It also joins the trip table with the zone table based on
## pickup and dropoff locations.

decode_zones <- function(trip_dt,
                         zone_dt){

  # We don't want to edit original variables
  zone_dt <- copy(zone_dt)
  zone_names <- copy(names(zone_dt))

  # Rename columns in zone table for pickup locations
  setnames(
    zone_dt,
    zone_names[-1],
    paste0("start_",zone_names[-1])
  )

  # Join trip table with zone table on pickup location
  trip_dt <-
    trip_dt[zone_dt,
            on = c("PULocationID" = "LocationID"),
            nomatch = 0]

  # Rename columns in zone table for dropoff locations
  setnames(
    zone_dt,
    names(zone_dt)[-1],
    paste0("end_",zone_names[-1])
  )

  # Join trip table with zone table on dropoff location
  trip_dt <-
    trip_dt[zone_dt,
            on = c("DOLocationID" = "LocationID"),
            nomatch = 0]

  # Remove position ids from trip table
  trip_dt[, c("PULocationID", "DOLocationID") := NULL]

  return(trip_dt)

}



# 2. Plotting ----

plot_chr_count <- function(dt,
                           count_var,
                           breaks_width = NULL,
                           wrap_scales = "fixed",
                           point_color = "forestgreen",
                           alpha = 0.85,
                           accuracy = 1){

  dt_count <-
    lapply(count_var, function(x){
      dt[, .(count_var = x,
             count = .N),
         by = .(var_x =
                  fct_infreq(get(x)) |>
                  fct_lump(n = 15) |>
                  fct_rev())
      ][, pct_count := count/sum(count)]
    }) |>
    rbindlist()

  report_plot <-
  ggplot(dt_count,
         aes(count, var_x))+
    geom_blank(aes(x = count *1.10))+
    geom_segment(linewidth = 1,
                 x = 0,
                 aes(xend = count,
                     y = var_x,
                     yend = var_x))+
    geom_point(size = 4,
               color = point_color,
               alpha = alpha)+
    geom_text(aes(label = percent(pct_count,
                                  accuracy = accuracy)),
              hjust = -0.75,
              size = 4)+
    scale_x_continuous(labels = comma_format(accuracy = accuracy),
                       breaks = if(!is.null(breaks_width)){breaks_width(breaks_width)}else{waiver()} )+
    expand_limits(x = 0)+
    labs(title = paste0(count_var, collapse = ", "),
         y = "") +
    theme(panel.grid.major.y = element_blank())


  if(length(count_var) > 1L){
    report_plot <-
      report_plot +
      facet_wrap(~count_var, scales = wrap_scales)+
      theme(plot.title = element_blank())
  }

  return(report_plot)

}

hist_low_tail <- function(dt,
                          var_name,
                          low_value = 1,
                          bins = 50){

  ggplot(dt[get(var_name) <= low_value],
         aes(get(var_name))) +
    geom_histogram(bins = bins,
                   fill = "forestgreen",
                   alpha = 0.75)+
    labs(title = paste0(var_name," <= ", low_value),
         x = "")

}


# 3. Feature Engineering ----

# Define function
add_date_features <- function(DT,
                              date_name = "date",
                              year = 2023,
                              prefix){

  var_names <- copy(names(DT))

  # Adding a temp date variable
  DT[, `.date` := get(date_name)]

  # Calculate dates for holidays that are not on fixed dates
  mlk_day <- timeDate::timeNthNdayInMonth(paste0(year,"-01-01"), nday = 1L, nth = 3L) |> as.Date()
  presidents_day <- timeDate::timeNthNdayInMonth(paste0(year,"-02-01"), nday = 1L, nth = 3L) |> as.Date()
  mothers_day <- timeDate::timeNthNdayInMonth(paste0(year,"-05-01"), nday = 0L, nth = 2L) |> as.Date()
  fathers_day <- timeDate::timeNthNdayInMonth(paste0(year,"-06-01"), nday = 0L, nth = 3L) |> as.Date()
  thanksgiving <- timeDate::timeNthNdayInMonth(paste0(year,"-11-01"), nday = 4L, nth = 4L) |> as.Date()

  # Define NYC holidays
  holidays <- c(
    "New Year" = make_date(year, 1L, 1L),
    "Day After New Year’s Day" = make_date(year, 1L, 2L),
    "Martin Luther King Jr. Day" = mlk_day,
    "Lincoln's Birthday Observed" = make_date(year, 2L, 12L),
    "Lincoln's Birthday" = make_date(year, 2L, 13L),
    "Presidents' Day" = presidents_day,
    "Mother's Day" = mothers_day,
    "Memorial Day" = make_date(year, 5L, 29L),
    "Father's Day" = fathers_day,
    "Juneteenth" = make_date(year, 6L, 19L),
    "Independence Day" = make_date(year, 7L, 4L),
    "Labor Day" = make_date(year, 9L, 4L),
    "Columbus Day" = make_date(year, 10L, 9L),
    "Election Day" = make_date(year, 11L, 7L),
    "Veterans Day Observed" = make_date(year, 11L, 10L),
    "Veterans Day" = make_date(year, 11L, 11L),
    "Thanksgiving Day" = thanksgiving,
    "Christmas Day" = make_date(year, 12L, 25L)
  )

  DT[, `:=`(
    # Time-based features
    hour = hour(`.date`),
    day = day(`.date`),
    month = month(`.date`),
    year = year(`.date`),
    wday = wday(`.date`),
    weekend = wday(`.date`) %in% c(6, 7),
    day_of_year = yday(`.date`),
    week_of_year = week(`.date`),
    quarter_of_year = quarter(`.date`),
    is_leap_year = leap_year(`.date`),

    # Calculate days to next holiday and days from previous holiday
    special_day = as.Date(`.date`) %in% holidays,
    days_to_next_holiday = as.numeric(difftime(
      holidays[findInterval(`.date`, holidays) + 1],
      `.date`,
      units = "days"
    )),
    days_from_prev_holiday = as.numeric(difftime(
      `.date`,
      holidays[findInterval(`.date`, holidays)],
      units = "days"
    )),

    # Periodicity features
    sin_month = sin(2 * pi * (month(`.date`) / 12)),
    cos_month = cos(2 * pi * (month(`.date`) / 12)),
    sin_hour = sin(2 * pi * (hour(`.date`) / 24)),
    cos_hour = cos(2 * pi * (hour(`.date`) / 24)),
    sin_day_of_week = sin(2 * pi * (wday(`.date`) / 7)),
    cos_day_of_week = cos(2 * pi * (wday(`.date`) / 7)),

    # Season of the year
    season = fcase(
      month(`.date`) %in% c(12, 1, 2), "Winter",
      month(`.date`) %in% c(3, 4, 5), "Spring",
      month(`.date`) %in% c(6, 7, 8), "Summer",
      month(`.date`) %in% c(9, 10, 11), "Fall"
    )
  )]

  # Removing the temp date variable
  DT[, `.date`:= NULL]

  # Adding prefix
  if(!missing(prefix)){
    new_vars <- setdiff(names(DT), var_names)
    setnames(DT, new_vars, paste0(prefix,"_",new_vars))
  }

  return(DT)
}



# This function transforms data after Exploratory Data Analysis (EDA).
# It handles categorical variables and performs some specific transformations based on certain conditions.
trans_after_eda <- function(DT){

  # Remove unnecessary columns
  DT[, c("dispatching_base_num",
         "originating_base_num",
         "PU_Zone",
         "DO_Zone") := NULL]

  # If access_a_ride_flag is not 'N', set it to 'Y'
  DT[access_a_ride_flag != "N",
     access_a_ride_flag := "Y"]

  # Define boroughs to be grouped as 'Other'
  borough_other <- c("Staten Island", "Unknown", "EWR")

  # Group certain pickup boroughs as 'Other'
  DT[PU_Borough %chin% borough_other,
     PU_Borough := "Other"]

  # Group certain dropoff boroughs as 'Other'
  DT[DO_Borough %chin% borough_other,
     DO_Borough := "Other"]

  # Group EWR service zones as 'Airports' for dropoff locations
  DT[PU_service_zone == "EWR",
     PU_service_zone := "Airports"]
  DT[DO_service_zone == "EWR",
     DO_service_zone := "Airports"]

  return(DT)
}
