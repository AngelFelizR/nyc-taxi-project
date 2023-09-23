
# 1. Install packages listed in the renv.lock file ----
if(!"renv" %in% installed.packages()[, 1L]) install.packages("renv")
renv::restore()

# 2. Scraping document's links from official website ----
SourcePage <-
  rvest::read_html("https://www.nyc.gov/site/tlc/about/tlc-trip-record-data.page")

ParquetLinks <-
  rvest::html_elements(SourcePage,
                       xpath = '//div[@class="faq-answers"]//li/a') |>
  rvest::html_attr("href") |>
  grep(pattern = "fhvhv_[a-z]+_2023-\\d{2}\\.parquet", value = TRUE) |>
  trimws()

# 3. Creating a folder to store the data ----
dir.create("00-data")

# 4. Downloading each file ----

## Defining the path to save files
LocalPath <- file.path("00-data", basename(ParquetLinks))

## This will make sure that R won't stop before
## downloading each parquet file
options(timeout = 800)

for(link_i in seq_along(ParquetLinks)){
  download.file(ParquetLinks[link_i],
                destfile = LocalPath[link_i],
                mode = "wb")
}


# 5. Downloading zone file ----
SourcePage |>
  rvest::html_elements(xpath = '//ul/li/a[text()="Taxi Zone Lookup Table"]')  |>
  rvest::html_attr("href") |>
  trimws() |>
  download.file(destfile = "00-data/taxi_zone_lookup.csv",
                mode = "wb")


# 6. Splitting training and test data

## As we a lot of data we can use 3 moths for training
## the rest of the months for testing the results
FilePathTrain <- head(LocalPath, 3L)
FilePathTest <- setdiff(LocalPath, FileSeqTrain)

## Saving training set
FilePathTrain |>
  lapply(\(x) data.table::as.data.table(arrow::read_parquet(x))) |>
  data.table::rbindlist() |>
  fst::write_fst("00-data/TripDataTrain.fst")
gc()

## Saving testing set
FilePathTest |>
  lapply(\(x) data.table::as.data.table(arrow::read_parquet(x))) |>
  data.table::rbindlist() |>
  fst::write_fst("00-data/TripDataTest.fst")
gc()
