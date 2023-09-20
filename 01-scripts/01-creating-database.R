
# 1. Scraping document's links from official website
ParquetLinks <-
  rvest::read_html("https://www.nyc.gov/site/tlc/about/tlc-trip-record-data.page") |>
  rvest::html_elements(xpath = '//div[@class="faq-answers"]//li/a') |>
  rvest::html_attr("href") |>
  grep(pattern = "fhvhv_[a-z]+_2023-\\d{2}\\.parquet", value = TRUE) |>
  trimws()

# 2. Extracting file name
FileSeq <- seq_along(ParquetLinks)
FileNames <- basename(ParquetLinks)
LocalPath <- file.path("00-data", FileNames)
ReportMonth <-
  gsub("[a-z]|_|\\.", "", FileNames) |>
  paste0("-01") |>
  lubridate::ymd()

# 3. Downloading each file

## It takes some time to download the files
options(timeout = 800)

for(link_i in FileSeq){
  download.file(ParquetLinks[link_i],
                LocalPath[link_i],
                mode = "wb")
}


# 4. Splitting training and test data

## As we a lot of data we can use 3 moths for training
## the other 3 months for testing the results
FileSeqTrain <- head(FileSeq, 3L)
FileSeqTest <- setdiff(FileSeq, FileSeqTrain)

## Saving training set
TripDataTrain <-
  lapply(FileSeqTrain,
         \(table_i) data.table::as.data.table(arrow::read_parquet(LocalPath[table_i])
                    )[, report_month := ReportMonth[table_i]]) |>
  data.table::rbindlist()

## Saving testing set
fst::write_fst(TripDataTrain, "00-data/TripDataTrain.fst")
rm(TripDataTrain)

TripDataTest <-
  lapply(FileSeqTest,
         \(table_i) data.table::as.data.table(arrow::read_parquet(LocalPath[table_i])
         )[, report_month := ReportMonth[table_i]]) |>
  data.table::rbindlist()

fst::write_fst(TripDataTest, "00-data/TripDataTest.fst")
rm(TripDataTest)

