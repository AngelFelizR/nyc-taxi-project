

simulate_trips <- function(arrow_con,
                           current_time,
                           current_zone,
                           minutes_next_trip,
                           waiting_time,
                           end_time,
                           valid_zones,
                           closest_zone,
                           borough_zones,
                           seed_num = 1234){

  # Empty table to fill
  done_trips <- data.table(
    s_PULocationID = vector("integer"),
    s_DOLocationID = vector("integer"),
    s_request_datetime = vector("double") |> as_datetime(),
    s_dropoff_datetime = vector("double") |> as_datetime(),
    s_trip_miles = vector("double"),
    s_trip_time = vector("integer"),
    s_driver_pay = vector("double"),
    s_tips = vector("double")
  )

  # We don't know how many trips
  # we will need to complete the day
  while(current_time < end_time){

    simulated_trip <-
      arrow_con |>
      select(s_PULocationID = PULocationID,
             s_DOLocationID = DOLocationID,
             s_request_datetime = request_datetime,
             s_dropoff_datetime = dropoff_datetime,
             s_trip_miles = trip_miles,
             s_trip_time = trip_time,
             s_driver_pay = driver_pay,
             s_tips = tips) |>
      filter(s_PULocationID %in% current_zone &
               s_DOLocationID %in% valid_zones &
               s_request_datetime >= current_time &
               s_request_datetime <= waiting_time) |>
      collect()


    # Go to closest zone
    if(length(current_zone) == 1L && nrow(simulated_trip) == 0L){

      waiting_time <- current_time + minutes_next_trip
      current_zone <- c(current_zone, closest_zone[as.character(current_zone)])

      # Find a trip in the Borough
    }else if(length(current_zone) > 1L && nrow(simulated_trip) == 0L){

      waiting_time <- current_time + minutes_next_trip
      current_zone <- c(
        current_zone,
        borough_zones[LocationID == current_zone[1L], id_list][[1L]]
      )

    }else{

      # Sample a trip
      set.seed(seed_num)
      simulated_trip <- sample_n(simulated_trip, size = 1L)
      set.seed(NULL)

      # Adding the trip to final result
      done_trips <- rbind(done_trips, simulated_trip)

      # Updating starting point
      current_time <- simulated_trip$s_dropoff_datetime
      current_zone <- simulated_trip$s_DOLocationID
      waiting_time <- current_time + minutes_next_trip
    }

  }

  # Returning all trips
  return(done_trips)

}


# 1. EDA -----

glimpse <- function(...){

  UseMethod("glimpse")

}

glimpse.default <- function(...){
  pillar::glimpse(...)
}


glimpse.arrow_dplyr_query <- function(x){

  schm <- x$.data$schema


  col_types <- sapply(

    x$selected_columns,

    FUN = function(expr) {
      name <- expr$field_name
      if (nzchar(name)) {
        schm$GetFieldByName(name)$type$ToString()
      }
      else {
        expr$type(schm)$ToString()
      }

    })


  fields <- paste(
    names(col_types),
    col_types,
    sep = ": ",
    collapse = "\n"
  )

  cat(
    paste0("FileSystemDataset (query)\n",
           scales::comma(nrow(x)), " rows x ",
           scales::comma(ncol(x)), " columns\n\n",
           fields)
  )
}

count_pct <- function(x, ..., sort = TRUE){

  count(x, ..., sort = sort) |>
    collect() |>
    mutate(pct = n / sum(n))

}


join_zones <- function(x, zone_tb){

  as.data.table(x) |>
    (\(dt) zone_tb[, .(end_id = LocationID,
                       end_borough = Borough,
                       end_zone = Zone,
                       end_service_zone = service_zone)
    ][dt, on = c("end_id" = "DOLocationID")])() |>
    (\(dt) zone_tb[, .(start_id = LocationID,
                       start_borough = Borough,
                       start_zone = Zone,
                       start_service_zone = service_zone)
    ][dt, on = c("start_id" = "PULocationID")
    ][, !c("start_id", "end_id")])()

}

factor_weekday <- function(x){

  weekdays_name <- c("Mo", "Tu", "We", "Th", "Fr", "Sa", "Su")

  factor(weekdays_name[x], levels = weekdays_name)

}

compute_boxplot <- function(x, value){

  filter(x,
         !is.na({{value}})) |>
    summarize(min_value = min({{value}}),
              q1 = quantile({{value}}, 0.25),
              q2 = median({{value}}),
              q3 = quantile({{value}}, 0.75),
              max_value = max({{value}})) |>
    mutate(lower_whisker = q1 - 1.5*(q3 - q1),
           higher_whisker = q3 + 1.5*(q3 - q1))

}


# 2. Feature Engineering ----

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
