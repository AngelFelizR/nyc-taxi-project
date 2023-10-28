
# 1. Scraping document's links from official website ----

SourcePage <-
  rvest::read_html("https://www.nyc.gov/site/tlc/about/tlc-trip-record-data.page")

TripLinks <-
  SourcePage |>
  rvest::html_elements(xpath = '//div[@class="faq-answers"]//li/a') |>
  rvest::html_attr("href") |>
  grep(pattern = "fhvhv_[a-z]+_2023-0[1-6]\\.parquet", value = TRUE) |>
  trimws()

TaxiZoneLink <-
  SourcePage |>
  rvest::html_elements(xpath = '//ul/li/a[text()="Taxi Zone Lookup Table"]')  |>
  rvest::html_attr("href") |>
  trimws()


# 2. Downloading each file ----

# Creating data dir if missing
if(!"data" %in% dir()) dir.create("data")

## Defining the path to save files
TripLocalPath <- file.path("data", basename(TripLinks))

## This will make sure that R won't stop before
## downloading each parquet file
options(timeout = 800)

## Saving Trip Parquet files
## using he wb mode to download binaries
for(link_i in seq_along(TripLinks)){
  download.file(TripLinks[link_i],
                destfile = TripLocalPath[link_i],
                mode = "wb")
}

## Saving Taxi Zone CSV
download.file(TaxiZoneLink,
              destfile = file.path("data","taxi_zone_lookup.csv"),
              mode = "wb")


# 3. Splitting training and testing data ----

## As we a lot of data we can use 3 moths for training
## the rest of the months for testing the results
FilePathTrain <- head(TripLocalPath, 3L)
FilePathTest <- setdiff(TripLocalPath, FilePathTrain)

## Down sampling training and testing data to 10 million rows
## To mitigate the current computational limitations
DownSampleRows <- 1e7

## Saving training set
set.seed(202301)
FilePathTrain |>
  lapply(\(x) data.table::as.data.table(arrow::read_parquet(x))) |>
  data.table::rbindlist() |>
  (\(dt) dt[sample.int(n = nrow(dt), size = DownSampleRows)])() |>
  fst::write_fst("data/TripDataTrain.fst")
gc()

## Saving testing set
set.seed(202302)
FilePathTest |>
  lapply(\(x) data.table::as.data.table(arrow::read_parquet(x))) |>
  data.table::rbindlist() |>
  (\(dt) dt[sample.int(n = nrow(dt), size = DownSampleRows)])() |>
  fst::write_fst("data/TripDataTest.fst")
gc()

set.seed(NULL)
