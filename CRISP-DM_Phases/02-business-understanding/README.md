Defining the project
================

- <a href="#project-name" id="toc-project-name">Project Name</a>
- <a href="#problem-statement" id="toc-problem-statement">Problem
  Statement</a>
- <a href="#project-scope" id="toc-project-scope">Project Scope</a>
- <a href="#stakeholders" id="toc-stakeholders">Stakeholders</a>
- <a href="#top-process-definition" id="toc-top-process-definition">Top
  Process Definition</a>
- <a href="#project-objetive" id="toc-project-objetive">Project
  Objetive</a>
- <a href="#defining-metric" id="toc-defining-metric">Defining Metric</a>
- <a href="#business-case" id="toc-business-case">Business Case</a>
- <a href="#data-requirements" id="toc-data-requirements">Data
  Requirements</a>
- <a href="#deliverables" id="toc-deliverables">Deliverables</a>

## Project Name

**Taxi Drivers Net Earnings**.

## Problem Statement

The problem is that taxi drivers’ net earnings are not as high as they
could be due to a lack of strategy for increasing tips.

## Project Scope

This project will be limited to Juno, Uber, Via and Lyft taxi drivers
who work in New York City.

## Stakeholders

- Taxi drivers
- Taxi companies
- Customers
- NYC Taxi and Limousine Commission

## Top Process Definition

Top define the elements of the process, we use a **SIPOC** diagram.

``` r
DiagrammeR::grViz('
digraph SIPOC {
    rankdir=LR;
    node [shape=box];
    subgraph cluster_S {
        label="Suppliers";
        S1 [label="Gas Station"];
        S2 [label="Car Manufacturer"];
        S3 [label="Taxi Application"];
        S4 [label="Telecomuncation\nCompany"];
        S5 [label="Smartphone Supplier"];
        S6 [label="Maintenance\nService Providers"];
    }
    subgraph cluster_I {
        label="Inputs";
        I1 [label="Gas"];
        I2 [label="Car"];
        I3 [label="Start\nLocation"];
        I4 [label="End\nLocation"];
        I5 [label="Internet"];
        I6 [label="Smartphone"];
        I7 [label="Customer\nRequests"];
    }
    subgraph cluster_P {
        label="Process";
        P1 [label="The customer request a taxi"];
        P2 [label="The driver arrived at\nthe pick-up location"];
        P3 [label="Drivers pick the customer up"];
        P4 [label="Drivers drive to destination"];
        P5 [label="Drivers leave the customer\nat the end point"];
    }
    subgraph cluster_O {
        label="Outputs";
        O1 [label="The customer is picked up\n at start location"];
        O2 [label="The customer recives a\ntravel experience"];
        O3 [label="The Customer gets\nat end location"];
        O4 [label="Payment Received"]
    }
    subgraph cluster_C {
        label="Customers";
        C1 [label="Taxi User"];
    }
    S1 -> I1 [style=invis];
    I1 -> P1 [style=invis];
    P1 -> O1 [style=invis];
    O1 -> C1 [style=invis];
    P1 -> P2 [constraint=false];
    P2 -> P3 [constraint=false];
    P3 -> P4 [constraint=false];
    P4 -> P5 [constraint=false];
}

')
```

![](img/01-SIPOC.png)

## Project Objetive

The objective of this project is to develop a strategy to select the
best payed trips possible and to increase their tips and thereby their
net earnings.

## Defining Metric

$$
\text{Hourly Wage} = \frac{\text{Total Driver Pay} + \text{Total Tips}}{\text{Total Hours Worked}}
$$

| **Metric**  |                                    **Baseline**                                     | **Goal** |
|:-----------:|:-----------------------------------------------------------------------------------:|:--------:|
| Hourly Wage | [55](https://www.ziprecruiter.com/Salaries/UBER-Taxi-Driver-Salary-in-Manhattan,NY) |    66    |

## Business Case

As the based driver’s pay increase with costs like gas, time and car’s
maintenance the best way to increase total earning is by increasing the
amount of **tips** that drivers receive from customers.

Based on *212,416,083* trips recorded 2022, drivers received
*\$229,936,965* in tips which is only 5% of the total earnings for that
year, for example if a driver improve his strategy to increase his tips
to **20%** of his current earning he could be earning **\$1,760** extra
monthly if he works 8 hours a day, 5 days each week and earns *\$55*
hourly (based on
[ziprecruiter](https://www.ziprecruiter.com/Salaries/UBER-Taxi-Driver-Salary-in-Manhattan,NY),
2023-11-22).

``` r
# Loading libraries to use
library(scales)
library(data.table)
library(dplyr)
library(arrow)

# Loading data to arrow
NycTrips <- open_dataset(here::here("data/trip-data"))

# 2022 Earning Summary
NycTrips |>
  filter(year == 2022) |>
  summarize(number_of_trips = sum(driver_pay > -1e6),
            trips_with_tips = sum(tips > 0, na.rm = TRUE),
            driver_net_earning = sum(driver_pay + tips, na.rm = TRUE),
            tips = sum(tips, na.rm = TRUE)) |>
  collect() |>
  as.data.table() |>
  (\(dt)  dt[, .(number_of_trips = comma(number_of_trips),
                 trips_with_tips = comma(trips_with_tips),
                 trips_with_tips_pct = percent(trips_with_tips / number_of_trips),
                 driver_net_earning = dollar(driver_net_earning),
                 tips = dollar(tips),
                 tips_pct = percent(tips/driver_net_earning))
          ][, melt(.SD, 
                   measure.vars = names(.SD),
                   variable.name = "Summary Variable",
                   value.name = "Total",
                   variable.factor = FALSE)]
   )()
```

          Summary Variable          Total
    1:     number_of_trips    212,416,083
    2:     trips_with_tips     42,358,143
    3: trips_with_tips_pct            20%
    4:  driver_net_earning $4,196,894,683
    5:                tips   $229,936,965
    6:            tips_pct             5%

It’s also important to consider that Taxi companies and customers can
both benefit from drivers earning more tips in several ways:

1.  **Taxi Companies**:
    - **Employee Satisfaction**: Higher tips can lead to increased job
      satisfaction among drivers, which can improve their performance
      and reduce turnover rates.
    - **Company Reputation**: If drivers are earning more tips, it could
      indicate that they are providing excellent service, which can
      enhance the company’s reputation.
    - **Customer Retention**: Satisfied drivers are more likely to
      provide better customer service, which can lead to higher customer
      retention rates.
2.  **Customers**:
    - **Better Service**: Drivers who earn more tips are often those who
      provide better service. This could mean cleaner vehicles, more
      courteous behavior, and a more enjoyable ride overall.
    - **Driver Availability**: If the tip earnings are high, it could
      attract more drivers to work, potentially reducing wait times for
      customers.
    - **Safety**: Drivers who are not worried about their earnings might
      be less likely to engage in risky behaviors (like speeding or
      working overly long shifts) to earn more.

## Data Requirements

In this project will use a subset of the data available in the [TLC Trip
Record
Data](https://www.nyc.gov/site/tlc/about/tlc-trip-record-data.page) from
2022 to 2023 for **High Volume For-Hire Vehicle** with the columns
described in the README.md file located at the
[data](https://github.com/AngelFelizR/nyc-taxi-project/tree/master/data)
folder.

## Deliverables

A Shiny app which assist the drivers focus their attention to the better
trips.
