
# This function decodes columns in a trip table using a company code and zone table.
# It also joins the trip table with the zone table based on pickup and dropoff locations.
decode_cols <- function(trip_table, zone_path){
  # Load zone table
  zone_table <- fread(zone_path)
  zone_names <- copy(names(zone_table))

  # Rename columns in zone table for pickup locations
  setnames(
    zone_table,
    zone_names[-1],
    paste0("PU_",zone_names[-1])
  )

  # Join trip table with zone table on pickup location
  trip_table <-
    trip_table[zone_table,
               on = c("PULocationID" = "LocationID"),
               nomatch = 0]

  # Rename columns in zone table for dropoff locations
  setnames(
    zone_table,
    names(zone_table)[-1],
    paste0("DO_",zone_names[-1])
  )

  # Join trip table with zone table on dropoff location
  trip_table <-
    trip_table[zone_table,
               on = c("DOLocationID" = "LocationID"),
               nomatch = 0]

  # Remove position ids from trip table
  trip_table[, c("PULocationID", "DOLocationID") := NULL]

  # Define company codes
  company_code <- c(
    "HV0002" = "Juno",
    "HV0003" = "Uber",
    "HV0004" = "Via",
    "HV0005" = "Lyft"
  )

  # Replace company codes in trip table with company names
  trip_table[, hvfhs_license_num := company_code[hvfhs_license_num]]

  return(trip_table)
}


# Define a custom function for creating histograms
custom_histogram <- function(DT,
                             var_name,
                             n_breaks = 7L){

  # Summarize the variable
  var_summary <- summary(DT[[var_name]]) |> round(2)

  # Create a text string of the summary
  summary_text <-
    paste(names(var_summary), var_summary) |>
    paste0(collapse = " | ")

  # Create the title for the plot
  plot_title <- paste0(
    var_name," Distribution\n",
    summary_text, "\n",
    DT[get(var_name) < 0,
       paste0("Neg: ", percent(.N/TripDataDim[1L], accuracy = 0.01))]
  )

  # Create the original plot
  original_plot <-
    ggplot(DT, aes(get(var_name)))+
    geom_histogram(fill = "blue",
                   color = "black",
                   alpha = 0.8,
                   bins = 30)+
    labs(title = plot_title,
         x = var_name)

  # Check if the first element of var_range is zero
  if(var_summary[1L] == 0){

    # Create a log plot with log2 transformation and an offset of +1
    log_plot <-
      original_plot+
      scale_x_continuous(trans = trans_new("log2+1",
                                           \(x) log2(x+1),
                                           \(x) 2^x - 1),
                         breaks = breaks_extended(n_breaks))+
      labs(title = "",
           x = paste0("log2(",var_name,"+1)"))

  }else{

    # Create a log plot with log2 transformation
    log_plot <-
      original_plot+
      scale_x_continuous(trans = "log2",
                         breaks = breaks_extended(n_breaks))+
      labs(title = "",
           x = paste0("log2(",var_name,")"))

  }

  # Combine the original and log plots
  gg_final <- original_plot/log_plot

  # Return the final plot
  return(gg_final)

}



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
