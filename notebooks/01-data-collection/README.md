Data Collection Process
================

- <a href="#web-scraping" id="toc-web-scraping">Web Scraping</a>
- <a href="#saving-files" id="toc-saving-files">Saving files</a>
- <a href="#final-result" id="toc-final-result">Final result</a>

For most projects the data collection process can be done manually and
later attache the file in the `data` folder but that isn’t a option when
we are working with big data.

To solve this problem, we have created the next script to automate the
data collection process so the project could be reproduced easily just
by running the code below.

## Web Scraping

To always have a updated list of 2022 and 2023 links of **High Volume
For-Hire Vehicles** documents let’s scrape the [TLC Trip Record
Data](https://www.nyc.gov/site/tlc/about/tlc-trip-record-data.page) by
using the `rvest` library.

``` r
SourcePage <-
  rvest::read_html("https://www.nyc.gov/site/tlc/about/tlc-trip-record-data.page")

TripLinks <-
  SourcePage |>
  rvest::html_elements(xpath = '//div[@class="faq-answers"]//li/a') |>
  rvest::html_attr("href") |>
  grep(pattern = "fhvhv_[a-z]+_202[23]-\\d{2}\\.parquet", value = TRUE) |>
  trimws() |>
  sort()

FileNames <- basename(TripLinks)

FileNames
```

     [1] "fhvhv_tripdata_2022-01.parquet" "fhvhv_tripdata_2022-02.parquet"
     [3] "fhvhv_tripdata_2022-03.parquet" "fhvhv_tripdata_2022-04.parquet"
     [5] "fhvhv_tripdata_2022-05.parquet" "fhvhv_tripdata_2022-06.parquet"
     [7] "fhvhv_tripdata_2022-07.parquet" "fhvhv_tripdata_2022-08.parquet"
     [9] "fhvhv_tripdata_2022-09.parquet" "fhvhv_tripdata_2022-10.parquet"
    [11] "fhvhv_tripdata_2022-11.parquet" "fhvhv_tripdata_2022-12.parquet"
    [13] "fhvhv_tripdata_2023-01.parquet" "fhvhv_tripdata_2023-02.parquet"
    [15] "fhvhv_tripdata_2023-03.parquet" "fhvhv_tripdata_2023-04.parquet"
    [17] "fhvhv_tripdata_2023-05.parquet" "fhvhv_tripdata_2023-06.parquet"
    [19] "fhvhv_tripdata_2023-07.parquet" "fhvhv_tripdata_2023-08.parquet"

From the same page we also can find the link to download the codes
related to each Zone.

``` r
TaxiZoneLink <-
  SourcePage |>
  rvest::html_elements(xpath = '//ul/li/a[text()="Taxi Zone Lookup Table"]')  |>
  rvest::html_attr("href") |>
  trimws()
```

## Saving files

To take advantage of the best capacities of `arrow` we need save a each
parquet file in folder with useful information to filter later, that why
we will have one folder level related to years the next sub-folders
related to a month with each parquet with the name `part-0.parquet`

Let’s start creating a folder to store all the parquet files.

``` r
ParquetFolderPath <- file.path("data", "trip-data")

dir.create(ParquetFolderPath, showWarnings  = FALSE)

ParquetFolderPath
```

    [1] "data/trip-data"

Then we need to define the sub-folders to split the files based on year.

``` r
YearFolders <- gsub(
  x = FileNames,
  pattern = "^fhvhv_tripdata_|-\\d{2}\\.parquet$",
  replacement = ""
) |>
  paste0("year=", a = _)

YearFoldersUnique <- unique(YearFolders)
YearFoldersPath <- file.path(ParquetFolderPath, YearFoldersUnique)

for(year_i in YearFoldersPath) dir.create(year_i)

YearFoldersPath
```

    [1] "data/trip-data/year=2022" "data/trip-data/year=2023"

Once created we just need to create the de month folder.

``` r
MonthFolders <- gsub(
  x = FileNames,
  pattern = "^fhvhv_tripdata_\\d{4}-|\\.parquet$",
  replacement = ""
) |>
  paste0("month=", a = _)

MonthFoldersPath <- file.path(ParquetFolderPath, YearFolders, MonthFolders)

for(month_i in MonthFoldersPath) dir.create(month_i, showWarnings = FALSE)

MonthFoldersPath
```

     [1] "data/trip-data/year=2022/month=01" "data/trip-data/year=2022/month=02"
     [3] "data/trip-data/year=2022/month=03" "data/trip-data/year=2022/month=04"
     [5] "data/trip-data/year=2022/month=05" "data/trip-data/year=2022/month=06"
     [7] "data/trip-data/year=2022/month=07" "data/trip-data/year=2022/month=08"
     [9] "data/trip-data/year=2022/month=09" "data/trip-data/year=2022/month=10"
    [11] "data/trip-data/year=2022/month=11" "data/trip-data/year=2022/month=12"
    [13] "data/trip-data/year=2023/month=01" "data/trip-data/year=2023/month=02"
    [15] "data/trip-data/year=2023/month=03" "data/trip-data/year=2023/month=04"
    [17] "data/trip-data/year=2023/month=05" "data/trip-data/year=2023/month=06"
    [19] "data/trip-data/year=2023/month=07" "data/trip-data/year=2023/month=08"

Finally, we just need to download each file on each folder.

``` r
# Parquet files might time a longer time to be downloaded
options(timeout = 1800)


# Parquet trip data
for(link_i in seq_along(TripLinks)){
  
  download.file(TripLinks[link_i],
                destfile = file.path(MonthFoldersPath[link_i],"part-0.parquet"),
                mode = "wb")
  
}


# Taxi Zone CSV
download.file(TaxiZoneLink,
              destfile = file.path("data","taxi_zone_lookup.csv"),
              mode = "wb")
```

## Final result

After getting all the files you should end with the next structure for
the `data` folder.

``` r
fs::dir_tree(here::here("data"))
```
    data
    ├── 01-source-page.png
    ├── README.md
    ├── taxi_zone_lookup.csv
    └── trip-data
        ├── year=2022
        │   ├── month=01
        │   │   └── part-0.parquet
        │   ├── month=02
        │   │   └── part-0.parquet
        │   ├── month=03
        │   │   └── part-0.parquet
        │   ├── month=04
        │   │   └── part-0.parquet
        │   ├── month=05
        │   │   └── part-0.parquet
        │   ├── month=06
        │   │   └── part-0.parquet
        │   ├── month=07
        │   │   └── part-0.parquet
        │   ├── month=08
        │   │   └── part-0.parquet
        │   ├── month=09
        │   │   └── part-0.parquet
        │   ├── month=10
        │   │   └── part-0.parquet
        │   ├── month=11
        │   │   └── part-0.parquet
        │   └── month=12
        │       └── part-0.parquet
        └── year=2023
            ├── month=01
            │   └── part-0.parquet
            ├── month=02
            │   └── part-0.parquet
            ├── month=03
            │   └── part-0.parquet
            ├── month=04
            │   └── part-0.parquet
            ├── month=05
            │   └── part-0.parquet
            ├── month=06
            │   └── part-0.parquet
            ├── month=07
            │   └── part-0.parquet
            └── month=08
                └── part-0.parquet
