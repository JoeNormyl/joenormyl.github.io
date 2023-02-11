---
modal-id: 1
date: 2022-02-05
img: unnamed-chunk-19-1.png
alt: Sample Bar Chart from the Cyclistic Project
project-date: January 2023
client: Google Data Analytics Course
category: Data Analysis
title: Finding Differences in Ridership for Cyclistic Bike Share Company
description: Cyclistic is a made-up company but the data in this project comes from real bike-share usage in Chicago. The premise of the project as well as all characters are fictional, but the analysis is very real.
---

# Differences in Ridership for Cyclistic

## Scenario

You are a junior data analyst working in the marketing analyst team at
Cyclistic, a bike-share company in Chicago. The director of marketing
believes the company’s future success depends on maximizing the number
of annual memberships. Therefore, your team wants to understand how
casual riders and annual members use Cyclistic bikes differently. From
these insights, your team will design a new marketing strategy to
convert casual riders into annual members. But first, Cyclistic
executives must approve your recommendations, so they must be backed up
with compelling data insights and professional data visualizations.

## Characters

- Cyclistic: A bike-share program that features more than 5,800
  bicycles and 600 docking stations. Cyclistic sets itself apart by
  also offering reclining bikes, hand tricycles, and cargo bikes,
  making bike-share more inclusive to people with disabilities and
  riders who can’t use a standard two-wheeled bike. The majority of
  riders opt for traditional bikes; about 8% of riders use the
  assistive options. Cyclistic users are more likely to ride for
  leisure, but about 30% use them to commute to work each day.
- Lily Moreno: The director of marketing and your manager. Moreno is
  responsible for the development of campaigns and initiatives to
  promote the bike-share program. These may include email, social
  media, and other channels.
- Cyclistic marketing analytics team: A team of data analysts who are
  responsible for collecting, analyzing, and reporting data that helps
  guide Cyclistic marketing strategy. You joined this team six months
  ago and have been busy learning about Cyclistic’s mission and
  business goals — as well as how you, as a junior data analyst, can
  help Cyclistic achieve them.
- Cyclistic executive team: The notoriously detail-oriented executive
  team will decide whether to approve the recommended marketing
  program.

## About the company

In 2016, Cyclistic launched a successful bike-share offering. Since
then, the program has grown to a fleet of 5,824 bicycles that are
geotracked and locked into a network of 692 stations across Chicago. The
bikes can be unlocked from one station and returned to any other station
in the system anytime. Until now, Cyclistic’s marketing strategy relied
on building general awareness and appealing to broad consumer segments.
One approach that helped make these things possible was the flexibility
of its pricing plans: single-ride passes, full-day passes, and annual
memberships. Customers who purchase single-ride or full-day passes are
referred to as casual riders. Customers who purchase annual memberships
are Cyclistic members. Cyclistic’s finance analysts have concluded that
annual members are much more profitable than casual riders. Although the
pricing flexibility helps Cyclistic attract more customers, Moreno
believes that maximizing the number of annual members will be key to
future growth. Rather than creating a marketing campaign that targets
all-new customers, Moreno believes there is a very good chance to
convert casual riders into members. She notes that casual riders are
already aware of the Cyclistic program and have chosen Cyclistic for
their mobility needs. Moreno has set a clear goal: Design marketing
strategies aimed at converting casual riders into annual members. In
order to do that, however, the marketing analyst team needs to better
understand how annual members and casual riders differ, why casual
riders would buy a membership, and how digital media could affect their
marketing tactics. Moreno and her team are interested in analyzing the
Cyclistic historical bike trip data to identify trends.

## Step 1: Ask

Three questions will guide the future marketing program:

1.  How do annual members and casual riders use Cyclistic bikes
    differently?
2.  Why would casual riders buy Cyclistic annual memberships?
3.  How can Cyclistic use digital media to influence casual riders to
    become members?

Moreno has assigned you the first question to answer: How do annual
members and casual riders use Cyclistic bikes differently?

You will produce a report with the following deliverables:

1.  A clear statement of the business task
2.  A description of all data sources used
3.  Documentation of any cleaning or manipulation of data
4.  A summary of your analysis
5.  Supporting visualizations and key findings
6.  Your top three recommendations based on your analysis

## Step 2: Prepare

First I will load all needed libraries

```r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0      ✔ purrr   0.3.5
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.5.0
    ## ✔ readr   2.1.3      ✔ forcats 0.5.2
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

```r
library(skimr)
library(janitor)
```

    ##
    ## Attaching package: 'janitor'
    ##
    ## The following objects are masked from 'package:stats':
    ##
    ##     chisq.test, fisher.test

```r
library(lubridate)
```

    ## Loading required package: timechange
    ##
    ## Attaching package: 'lubridate'
    ##
    ## The following objects are masked from 'package:base':
    ##
    ##     date, intersect, setdiff, union

```r
library(geosphere)
library(ggplot2)
library(readr)
library(scales)
```

    ##
    ## Attaching package: 'scales'
    ##
    ## The following object is masked from 'package:purrr':
    ##
    ##     discard
    ##
    ## The following object is masked from 'package:readr':
    ##
    ##     col_factor

```r
hour_v <- c("12am","1am","2am","3am","4am","5am","6am","7am","8am","9am","10am","11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm","7pm","8pm","9pm","10pm","11pm")

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
```

Next I will import the data from Cyclistic for the last 12 months

```r
df <-
  files <- list.files(path = "/Users/joe/R-Projects/Capstone_Project/", pattern = "*.csv") %>%
    map_df(~read_csv(.))
```

    ## Rows: 103770 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): ride_id, rideable_type, start_station_name, start_station_id, end_...
    ## dbl  (4): start_lat, start_lng, end_lat, end_lng
    ## dttm (2): started_at, ended_at
    ##
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 115609 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): ride_id, rideable_type, start_station_name, start_station_id, end_...
    ## dbl  (4): start_lat, start_lng, end_lat, end_lng
    ## dttm (2): started_at, ended_at
    ##
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 284042 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): ride_id, rideable_type, start_station_name, start_station_id, end_...
    ## dbl  (4): start_lat, start_lng, end_lat, end_lng
    ## dttm (2): started_at, ended_at
    ##
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 371249 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): ride_id, rideable_type, start_station_name, start_station_id, end_...
    ## dbl  (4): start_lat, start_lng, end_lat, end_lng
    ## dttm (2): started_at, ended_at
    ##
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 634858 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): ride_id, rideable_type, start_station_name, start_station_id, end_...
    ## dbl  (4): start_lat, start_lng, end_lat, end_lng
    ## dttm (2): started_at, ended_at
    ##
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 769204 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): ride_id, rideable_type, start_station_name, start_station_id, end_...
    ## dbl  (4): start_lat, start_lng, end_lat, end_lng
    ## dttm (2): started_at, ended_at
    ##
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 823488 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): ride_id, rideable_type, start_station_name, start_station_id, end_...
    ## dbl  (4): start_lat, start_lng, end_lat, end_lng
    ## dttm (2): started_at, ended_at
    ##
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 785932 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): ride_id, rideable_type, start_station_name, start_station_id, end_...
    ## dbl  (4): start_lat, start_lng, end_lat, end_lng
    ## dttm (2): started_at, ended_at
    ##
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 701339 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): ride_id, rideable_type, start_station_name, start_station_id, end_...
    ## dbl  (4): start_lat, start_lng, end_lat, end_lng
    ## dttm (2): started_at, ended_at
    ##
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 558685 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): ride_id, rideable_type, start_station_name, start_station_id, end_...
    ## dbl  (4): start_lat, start_lng, end_lat, end_lng
    ## dttm (2): started_at, ended_at
    ##
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 337735 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): ride_id, rideable_type, start_station_name, start_station_id, end_...
    ## dbl  (4): start_lat, start_lng, end_lat, end_lng
    ## dttm (2): started_at, ended_at
    ##
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 181806 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): ride_id, rideable_type, start_station_name, start_station_id, end_...
    ## dbl  (4): start_lat, start_lng, end_lat, end_lng
    ## dttm (2): started_at, ended_at
    ##
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 5667717 Columns: 22
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (10): ride_id, rideable_type, start_station_name, start_station_id, end...
    ## dbl  (10): start_lat, start_lng, end_lat, end_lng, hour_start, hour_end, day...
    ## dttm  (2): start_time, end_time
    ##
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

After dataframe is made, I get a feel for the data with some preview and
summary tools

```r
#Shows the first 10 rows
head(df)
```

    ## # A tibble: 6 × 24
    ##   ride_id        ridea…¹ started_at          ended_at            start…² start…³
    ##   <chr>          <chr>   <dttm>              <dttm>              <chr>   <chr>
    ## 1 C2F7DD78E82EC… electr… 2022-01-13 11:59:47 2022-01-13 12:02:44 Glenwo… 525
    ## 2 A6CF8980A652D… electr… 2022-01-10 08:41:56 2022-01-10 08:46:17 Glenwo… 525
    ## 3 BD0F91DFF741C… classi… 2022-01-25 04:53:40 2022-01-25 04:58:01 Sheffi… TA1306…
    ## 4 CBB80ED419105… classi… 2022-01-04 00:18:04 2022-01-04 00:33:00 Clark … KA1504…
    ## 5 DDC963BFDDA51… classi… 2022-01-20 01:31:10 2022-01-20 01:37:12 Michig… TA1309…
    ## 6 A39C6F6CC0586… classi… 2022-01-11 18:48:09 2022-01-11 18:51:31 Wood S… 637
    ## # … with 18 more variables: end_station_name <chr>, end_station_id <chr>,
    ## #   start_lat <dbl>, start_lng <dbl>, end_lat <dbl>, end_lng <dbl>,
    ## #   member_casual <chr>, start_time <dttm>, end_time <dttm>, hour_start <dbl>,
    ## #   hour_end <dbl>, weekday <chr>, month <chr>, day <dbl>, week <chr>,
    ## #   trip_time <dbl>, trip_dist <dbl>, avg_speed <dbl>, and abbreviated variable
    ## #   names ¹​rideable_type, ²​start_station_name, ³​start_station_id

```r
#Shows the last 10 rows
tail(df)
```

    ## # A tibble: 6 × 24
    ##   ride_id        ridea…¹ started_at ended_at start_station_name          start…²
    ##   <chr>          <chr>   <dttm>     <dttm>   <chr>                       <chr>
    ## 1 7BDEDE9860418… classi… NA         NA       Sangamon St & Washington B… 13409
    ## 2 43ABEE85B6E15… classi… NA         NA       Sangamon St & Washington B… 13409
    ## 3 F041C89A3D1F0… electr… NA         NA       Bernard St & Elston Ave     18016
    ## 4 A2BECB88430BE… classi… NA         NA       Wacker Dr & Washington St   KA1503…
    ## 5 37B392960E566… classi… NA         NA       Sangamon St & Washington B… 13409
    ## 6 2DD1587210BA4… classi… NA         NA       Southport Ave & Waveland A… 13235
    ## # … with 18 more variables: end_station_name <chr>, end_station_id <chr>,
    ## #   start_lat <dbl>, start_lng <dbl>, end_lat <dbl>, end_lng <dbl>,
    ## #   member_casual <chr>, start_time <dttm>, end_time <dttm>, hour_start <dbl>,
    ## #   hour_end <dbl>, weekday <chr>, month <chr>, day <dbl>, week <chr>,
    ## #   trip_time <dbl>, trip_dist <dbl>, avg_speed <dbl>, and abbreviated variable
    ## #   names ¹​rideable_type, ²​start_station_id

```r
#Shows structural info about data frame
str(df)
```

    ## spc_tbl_ [11,335,434 × 24] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ ride_id           : chr [1:11335434] "C2F7DD78E82EC875" "A6CF8980A652D272" "BD0F91DFF741C66D" "CBB80ED419105406" ...
    ##  $ rideable_type     : chr [1:11335434] "electric_bike" "electric_bike" "classic_bike" "classic_bike" ...
    ##  $ started_at        : POSIXct[1:11335434], format: "2022-01-13 11:59:47" "2022-01-10 08:41:56" ...
    ##  $ ended_at          : POSIXct[1:11335434], format: "2022-01-13 12:02:44" "2022-01-10 08:46:17" ...
    ##  $ start_station_name: chr [1:11335434] "Glenwood Ave & Touhy Ave" "Glenwood Ave & Touhy Ave" "Sheffield Ave & Fullerton Ave" "Clark St & Bryn Mawr Ave" ...
    ##  $ start_station_id  : chr [1:11335434] "525" "525" "TA1306000016" "KA1504000151" ...
    ##  $ end_station_name  : chr [1:11335434] "Clark St & Touhy Ave" "Clark St & Touhy Ave" "Greenview Ave & Fullerton Ave" "Paulina St & Montrose Ave" ...
    ##  $ end_station_id    : chr [1:11335434] "RP-007" "RP-007" "TA1307000001" "TA1309000021" ...
    ##  $ start_lat         : num [1:11335434] 42 42 41.9 42 41.9 ...
    ##  $ start_lng         : num [1:11335434] -87.7 -87.7 -87.7 -87.7 -87.6 ...
    ##  $ end_lat           : num [1:11335434] 42 42 41.9 42 41.9 ...
    ##  $ end_lng           : num [1:11335434] -87.7 -87.7 -87.7 -87.7 -87.6 ...
    ##  $ member_casual     : chr [1:11335434] "casual" "casual" "member" "casual" ...
    ##  $ start_time        : POSIXct[1:11335434], format: NA NA ...
    ##  $ end_time          : POSIXct[1:11335434], format: NA NA ...
    ##  $ hour_start        : num [1:11335434] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ hour_end          : num [1:11335434] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ weekday           : chr [1:11335434] NA NA NA NA ...
    ##  $ month             : chr [1:11335434] NA NA NA NA ...
    ##  $ day               : num [1:11335434] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ week              : chr [1:11335434] NA NA NA NA ...
    ##  $ trip_time         : num [1:11335434] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ trip_dist         : num [1:11335434] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ avg_speed         : num [1:11335434] NA NA NA NA NA NA NA NA NA NA ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   ride_id = col_character(),
    ##   ..   rideable_type = col_character(),
    ##   ..   started_at = col_datetime(format = ""),
    ##   ..   ended_at = col_datetime(format = ""),
    ##   ..   start_station_name = col_character(),
    ##   ..   start_station_id = col_character(),
    ##   ..   end_station_name = col_character(),
    ##   ..   end_station_id = col_character(),
    ##   ..   start_lat = col_double(),
    ##   ..   start_lng = col_double(),
    ##   ..   end_lat = col_double(),
    ##   ..   end_lng = col_double(),
    ##   ..   member_casual = col_character()
    ##   .. )
    ##  - attr(*, "problems")=<externalptr>

```r
#Provides a preview of data without rendering all
glimpse(df)
```

    ## Rows: 11,335,434
    ## Columns: 24
    ## $ ride_id            <chr> "C2F7DD78E82EC875", "A6CF8980A652D272", "BD0F91DFF7…
    ## $ rideable_type      <chr> "electric_bike", "electric_bike", "classic_bike", "…
    ## $ started_at         <dttm> 2022-01-13 11:59:47, 2022-01-10 08:41:56, 2022-01-…
    ## $ ended_at           <dttm> 2022-01-13 12:02:44, 2022-01-10 08:46:17, 2022-01-…
    ## $ start_station_name <chr> "Glenwood Ave & Touhy Ave", "Glenwood Ave & Touhy A…
    ## $ start_station_id   <chr> "525", "525", "TA1306000016", "KA1504000151", "TA13…
    ## $ end_station_name   <chr> "Clark St & Touhy Ave", "Clark St & Touhy Ave", "Gr…
    ## $ end_station_id     <chr> "RP-007", "RP-007", "TA1307000001", "TA1309000021",…
    ## $ start_lat          <dbl> 42.01280, 42.01276, 41.92560, 41.98359, 41.87785, 4…
    ## $ start_lng          <dbl> -87.66591, -87.66597, -87.65371, -87.66915, -87.624…
    ## $ end_lat            <dbl> 42.01256, 42.01256, 41.92533, 41.96151, 41.88462, 4…
    ## $ end_lng            <dbl> -87.67437, -87.67437, -87.66580, -87.67139, -87.627…
    ## $ member_casual      <chr> "casual", "casual", "member", "casual", "member", "…
    ## $ start_time         <dttm> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ end_time           <dttm> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ hour_start         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ hour_end           <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ weekday            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ month              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ day                <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ week               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ trip_time          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ trip_dist          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ avg_speed          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…

```r
#Lists all column names
colnames(df)
```

    ##  [1] "ride_id"            "rideable_type"      "started_at"
    ##  [4] "ended_at"           "start_station_name" "start_station_id"
    ##  [7] "end_station_name"   "end_station_id"     "start_lat"
    ## [10] "start_lng"          "end_lat"            "end_lng"
    ## [13] "member_casual"      "start_time"         "end_time"
    ## [16] "hour_start"         "hour_end"           "weekday"
    ## [19] "month"              "day"                "week"
    ## [22] "trip_time"          "trip_dist"          "avg_speed"

```r
#Simple Checks for Issues
# Check to make sure no odd values for member/casual
  n_distinct(df$member_casual)
```

    ## [1] 2

```r
#Provides a more detailed summary but greatly increases time to process
skim_without_charts(df)
```

|                                                  |          |
| :----------------------------------------------- | :------- |
| Name                                             | df       |
| Number of rows                                   | 11335434 |
| Number of columns                                | 24       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |          |
| Column type frequency:                           |          |
| character                                        | 10       |
| numeric                                          | 10       |
| POSIXct                                          | 4        |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |          |
| Group variables                                  | None     |

Data summary

**Variable type: character**

| skim_variable      | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
| :----------------- | --------: | ------------: | --: | --: | ----: | -------: | ---------: |
| ride_id            |         0 |          1.00 |  16 |  16 |     0 |  5667717 |          0 |
| rideable_type      |         0 |          1.00 |  11 |  13 |     0 |        3 |          0 |
| start_station_name |   1666128 |          0.85 |   7 |  64 |     0 |     1674 |          0 |
| start_station_id   |   1666128 |          0.85 |   3 |  44 |     0 |     1313 |          0 |
| end_station_name   |   1785484 |          0.84 |   9 |  64 |     0 |     1692 |          0 |
| end_station_id     |   1785484 |          0.84 |   3 |  44 |     0 |     1317 |          0 |
| member_casual      |         0 |          1.00 |   6 |   6 |     0 |        2 |          0 |
| weekday            |   5667717 |          0.50 |   6 |   9 |     0 |        7 |          0 |
| month              |   5667717 |          0.50 |   3 |   9 |     0 |       12 |          0 |
| week               |   5667717 |          0.50 |   2 |   2 |     0 |       52 |          0 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |    mean |       sd |        p0 |    p25 |     p50 |     p75 |       p100 |
| :------------ | --------: | ------------: | ------: | -------: | --------: | -----: | ------: | ------: | ---------: |
| start_lat     |         0 |           1.0 |   41.90 |     0.05 |     41.64 |  41.88 |   41.90 |   41.93 |      45.64 |
| start_lng     |         0 |           1.0 |  -87.65 |     0.03 |    -87.84 | -87.66 |  -87.64 |  -87.63 |     -73.80 |
| end_lat       |     11716 |           1.0 |   41.90 |     0.07 |      0.00 |  41.88 |   41.90 |   41.93 |      42.37 |
| end_lng       |     11716 |           1.0 |  -87.65 |     0.11 |    -88.14 | -87.66 |  -87.64 |  -87.63 |       0.00 |
| hour_start    |   5667717 |           0.5 |   14.22 |     5.03 |      0.00 |  11.00 |   15.00 |   18.00 |      23.00 |
| hour_end      |   5667717 |           0.5 |   14.37 |     5.10 |      0.00 |  11.00 |   15.00 |   18.00 |      23.00 |
| day           |   5667717 |           0.5 |   15.77 |     8.81 |      1.00 |   8.00 |   16.00 |   23.00 |      31.00 |
| trip_time     |   5667717 |           0.5 |   19.44 |   176.13 | -10353.35 |   5.82 |   10.28 |   18.47 |   41387.25 |
| trip_dist     |   5673575 |           0.5 | 2141.84 | 11843.49 |      0.00 | 873.63 | 1576.28 | 2782.55 | 9825063.44 |
| avg_speed     |   5673679 |           0.5 |     NaN |      NaN |      -Inf |   0.00 |    0.01 |    0.01 |        Inf |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
| :------------ | --------: | ------------: | :------------------ | :------------------ | :------------------ | -------: |
| started_at    |   5667717 |           0.5 | 2022-01-01 00:00:05 | 2022-12-31 23:59:26 | 2022-07-22 15:03:59 |  4745862 |
| ended_at      |   5667717 |           0.5 | 2022-01-01 00:01:48 | 2023-01-02 04:56:45 | 2022-07-22 15:24:44 |  4758633 |
| start_time    |   5667717 |           0.5 | 2022-01-01 00:00:05 | 2022-12-31 23:59:26 | 2022-07-22 15:03:59 |  4745862 |
| end_time      |   5667717 |           0.5 | 2022-01-01 00:01:48 | 2023-01-02 04:56:45 | 2022-07-22 15:24:44 |  4758633 |

## Step 3. Process

Having previewed the data it’s time to give it a good scrub. Due to the
volume of data, I chose to remove any rows with missing info as well as
any duplicates. Next I will mutate the dataframe to coerce the types I
will need for future calculations.

```r
#creating new df to preserve original raw data
#Removing empty values and ensuring names are clean
#Convert started_at/ended_at to datetime type and Long/Lat to numeric
df_clean  <- df %>%
  mutate(
    start_time = ymd_hms(as_datetime(started_at)),
    end_time = ymd_hms(as_datetime(ended_at)),
    start_lat = as.numeric(start_lat),
    start_lng = as.numeric(start_lng),
    end_lat = as.numeric(end_lat),
    end_lng = as.numeric(end_lng),
  )

glimpse(df_clean)
```

    ## Rows: 11,335,434
    ## Columns: 24
    ## $ ride_id            <chr> "C2F7DD78E82EC875", "A6CF8980A652D272", "BD0F91DFF7…
    ## $ rideable_type      <chr> "electric_bike", "electric_bike", "classic_bike", "…
    ## $ started_at         <dttm> 2022-01-13 11:59:47, 2022-01-10 08:41:56, 2022-01-…
    ## $ ended_at           <dttm> 2022-01-13 12:02:44, 2022-01-10 08:46:17, 2022-01-…
    ## $ start_station_name <chr> "Glenwood Ave & Touhy Ave", "Glenwood Ave & Touhy A…
    ## $ start_station_id   <chr> "525", "525", "TA1306000016", "KA1504000151", "TA13…
    ## $ end_station_name   <chr> "Clark St & Touhy Ave", "Clark St & Touhy Ave", "Gr…
    ## $ end_station_id     <chr> "RP-007", "RP-007", "TA1307000001", "TA1309000021",…
    ## $ start_lat          <dbl> 42.01280, 42.01276, 41.92560, 41.98359, 41.87785, 4…
    ## $ start_lng          <dbl> -87.66591, -87.66597, -87.65371, -87.66915, -87.624…
    ## $ end_lat            <dbl> 42.01256, 42.01256, 41.92533, 41.96151, 41.88462, 4…
    ## $ end_lng            <dbl> -87.67437, -87.67437, -87.66580, -87.67139, -87.627…
    ## $ member_casual      <chr> "casual", "casual", "member", "casual", "member", "…
    ## $ start_time         <dttm> 2022-01-13 11:59:47, 2022-01-10 08:41:56, 2022-01-…
    ## $ end_time           <dttm> 2022-01-13 12:02:44, 2022-01-10 08:46:17, 2022-01-…
    ## $ hour_start         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ hour_end           <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ weekday            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ month              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ day                <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ week               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ trip_time          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ trip_dist          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    ## $ avg_speed          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…

And while we’re at it, lets elaborate on the date and time to create
some more vectors for analysis. This way we can look at the data by day
of the week, month, week of the year, and starting hour. We will also
calculate trip time, trip distance, and average speed.

```r
df_clean <- df_clean %>%
  mutate(
    hour_start = hour(start_time),
    hour_end = hour(end_time),
    weekday = weekdays(start_time),
    month = month(start_time, label = T, abbr =F),
    day = day(start_time),
    week = strftime(start_time, format = "%V"),
    trip_time = as.numeric(difftime(end_time, start_time, units = "mins")),
  )
```

```r
##Using row-wise to run the distance calculation on the latitude/longitude one pair at a time, calculate trip distance and add it to the df
##I won't run this when knitting due to the time and resources it requires. I will instead import my saved clean data from the initial run

# df_clean <- df_clean %>%
#   rowwise() %>%
#   mutate(
#     trip_dist = distm(c(start_lng, start_lat), c(end_lng, end_lat), distHaversine)[,1],
#     avg_speed = trip_time/trip_dist
#   )
#

#Wrote the dataframe to a CSV after this step as the rowwise distance calculation took a long time to run
#write_csv(df_clean,"cyclistic_trips_clean_geo.csv")

# To save time running the geo calculations again for distance when knitting the rmd - I created a new csv to use as a shortcut. I will load this dataframe which is simply the result of al above code operations
df_clean <- read_csv("cyclistic_trips_clean_geo.csv")
```

    ## Rows: 5667717 Columns: 22
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (10): ride_id, rideable_type, start_station_name, start_station_id, end...
    ## dbl  (10): start_lat, start_lng, end_lat, end_lng, hour_start, hour_end, day...
    ## dttm  (2): start_time, end_time
    ##
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

```r
#Preview data one more time
glimpse(df_clean)
```

    ## Rows: 5,667,717
    ## Columns: 22
    ## $ ride_id            <chr> "C2F7DD78E82EC875", "A6CF8980A652D272", "BD0F91DFF7…
    ## $ rideable_type      <chr> "electric_bike", "electric_bike", "classic_bike", "…
    ## $ start_time         <dttm> 2022-01-13 11:59:47, 2022-01-10 08:41:56, 2022-01-…
    ## $ end_time           <dttm> 2022-01-13 12:02:44, 2022-01-10 08:46:17, 2022-01-…
    ## $ start_station_name <chr> "Glenwood Ave & Touhy Ave", "Glenwood Ave & Touhy A…
    ## $ start_station_id   <chr> "525", "525", "TA1306000016", "KA1504000151", "TA13…
    ## $ end_station_name   <chr> "Clark St & Touhy Ave", "Clark St & Touhy Ave", "Gr…
    ## $ end_station_id     <chr> "RP-007", "RP-007", "TA1307000001", "TA1309000021",…
    ## $ start_lat          <dbl> 42.01280, 42.01276, 41.92560, 41.98359, 41.87785, 4…
    ## $ start_lng          <dbl> -87.66591, -87.66597, -87.65371, -87.66915, -87.624…
    ## $ end_lat            <dbl> 42.01256, 42.01256, 41.92533, 41.96151, 41.88462, 4…
    ## $ end_lng            <dbl> -87.67437, -87.67437, -87.66580, -87.67139, -87.627…
    ## $ member_casual      <chr> "casual", "casual", "member", "casual", "member", "…
    ## $ hour_start         <dbl> 11, 8, 4, 0, 1, 18, 18, 12, 7, 15, 18, 12, 17, 22, …
    ## $ hour_end           <dbl> 12, 8, 4, 0, 1, 18, 18, 12, 8, 15, 18, 12, 18, 22, …
    ## $ weekday            <chr> "Thursday", "Monday", "Tuesday", "Tuesday", "Thursd…
    ## $ month              <chr> "January", "January", "January", "January", "Januar…
    ## $ day                <dbl> 13, 10, 25, 4, 20, 11, 30, 22, 17, 28, 11, 29, 2, 2…
    ## $ week               <chr> "02", "02", "04", "01", "03", "02", "04", "03", "03…
    ## $ trip_time          <dbl> 2.9500000, 4.3500000, 4.3500000, 14.9333333, 6.0333…
    ## $ trip_dist          <dbl> 700.3304, 695.0995, 1001.9534, 2465.5383, 815.4502,…
    ## $ avg_speed          <dbl> 0.004212297, 0.006258097, 0.004341519, 0.006056825,…

We still have some oddities such as 0 or negative trip time. Also
noticed that the days of the week are out of order. Let’s fix that.
Let’s get rid of those too

```r
# remove maintenance rides and 0 trip_time rides
df_clean <- df_clean[!(df_clean$start_station_name == "HQ QR" | df_clean$trip_time<0.001),] %>%
  #Remove any empty rows
  drop_na() %>%
  #remove any rows with empty fields
  remove_empty(which = c("cols", "rows")) %>%
  #make sure there are no issues with column names
  clean_names()

# Reorder the week days so they make sense in context
df_clean$weekday <- ordered(df_clean$weekday, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Also the months need to be arranged
df_clean$month <- ordered(df_clean$month, levels=c("January", "February", "March", "April", "June", "July", "August", "September", "October", "November", "December"))
```

Looking much better. Now on to the analysis!

## Step 4. Analyze

Time to look a little deeper into the data now that we have processed
and cleaned the original dataframe. I will run through some aggregations
and summaries to provide a solid base from which to build insights and
visuals for presentation.

#### Statistical Summaries of Interest

```r
#Summarize to get some basic statistical details about the distance and time
summary(df_clean$trip_dist)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
    ##       0     879    1567    2108    2740 9825063

```r
summary(df_clean$trip_time)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
    ##     0.02     6.05    10.60    17.10    19.02 34354.07

Now I will create some summary dataframes to use, aggregating the data
in various ways to investigate possible differences between member and
casual user bike usage.

#### Summaries:

```r
# analyze ridership data by type and weekday
df_analysis_weekday <- df_clean %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), average_trip_time = mean(trip_time), average_distance = mean(trip_dist, )) %>%
  arrange(weekday, member_casual)
```

    ## `summarise()` has grouped output by 'member_casual'. You can override using the
    ## `.groups` argument.

```r
#Preview new DF
print(df_analysis_weekday)
```

    ## # A tibble: 14 × 5
    ## # Groups:   member_casual [2]
    ##    member_casual weekday   number_of_rides average_trip_time average_distance
    ##    <chr>         <ord>               <int>             <dbl>            <dbl>
    ##  1 casual        Sunday             301278              27.2            2210.
    ##  2 member        Sunday             297707              13.9            2102.
    ##  3 casual        Monday             210746              24.8            2069.
    ##  4 member        Monday             375151              12.0            2003.
    ##  5 casual        Tuesday            196367              21.4            2073.
    ##  6 member        Tuesday            411226              11.8            2024.
    ##  7 casual        Wednesday          203568              20.7            2174.
    ##  8 member        Wednesday          412775              11.8            2180.
    ##  9 casual        Thursday           229993              21.4            2110.
    ## 10 member        Thursday           415862              12.0            2044.
    ## 11 casual        Friday             248785              22.4            2125.
    ## 12 member        Friday             360029              12.2            2008.
    ## 13 casual        Saturday           367310              26.8            2280.
    ## 14 member        Saturday           338255              14.0            2144.

```r
# analyze ridership data by type and weekday
df_analysis_month <- df_clean %>%
  group_by(member_casual, month) %>%  #groups by usertype and month
  summarise(number_of_rides = n(), average_trip_time = mean(trip_time), average_distance = mean(trip_dist, )) %>%
  arrange(month, member_casual)
```

    ## `summarise()` has grouped output by 'member_casual'. You can override using the
    ## `.groups` argument.

```r
print(df_analysis_month)
```

    ## # A tibble: 24 × 5
    ## # Groups:   member_casual [2]
    ##    member_casual month    number_of_rides average_trip_time average_distance
    ##    <chr>         <ord>              <int>             <dbl>            <dbl>
    ##  1 casual        January            12605              27.4            1838.
    ##  2 member        January            67523              10.3            1694.
    ##  3 casual        February           15143              24.8            1894.
    ##  4 member        February           74031              10.6            1739.
    ##  5 casual        March              67150              28.4            2123.
    ##  6 member        March             148821              11.8            1946.
    ##  7 casual        April              91889              25.9            2164.
    ##  8 member        April             180657              11.6            1940.
    ##  9 casual        June              292053              25.0            2204.
    ## 10 member        June              328258              13.7            2192.
    ## # … with 14 more rows

```r
df_analysis_week <- df_clean %>%
  group_by(member_casual, week) %>%  #groups by usertype and week of year
  summarise(number_of_rides = n(), average_trip_time = mean(trip_time), average_distance = mean(trip_dist, )) %>%
  arrange(week, member_casual)
```

    ## `summarise()` has grouped output by 'member_casual'. You can override using the
    ## `.groups` argument.

```r
print(df_analysis_week)
```

    ## # A tibble: 104 × 5
    ## # Groups:   member_casual [2]
    ##    member_casual week  number_of_rides average_trip_time average_distance
    ##    <chr>         <chr>           <int>             <dbl>            <dbl>
    ##  1 casual        01               2184             30.4             1670.
    ##  2 member        01              13158             10.2             1645.
    ##  3 casual        02               3750             31.0             2130.
    ##  4 member        02              18905             10.2             1751.
    ##  5 casual        03               3335             17.8             1686.
    ##  6 member        03              18519              9.97            1703.
    ##  7 casual        04               1893             21.2             1681.
    ##  8 member        04              11646             10.9             1626.
    ##  9 casual        05               1985             24.8             1745.
    ## 10 member        05              12745             11.0             1699.
    ## # … with 94 more rows

```r
df_analysis_hour <- df_clean %>%
  group_by(member_casual, hour_start) %>%  #groups by usertype and hour
  summarise(number_of_rides = n(), average_trip_time = mean(trip_time), average_distance = mean(trip_dist, )) %>%
  arrange(hour_start, member_casual)
```

    ## `summarise()` has grouped output by 'member_casual'. You can override using the
    ## `.groups` argument.

```r
print(df_analysis_hour)
```

    ## # A tibble: 48 × 5
    ## # Groups:   member_casual [2]
    ##    member_casual hour_start number_of_rides average_trip_time average_distance
    ##    <chr>              <dbl>           <int>             <dbl>            <dbl>
    ##  1 casual                 0           33310              21.2            1929.
    ##  2 member                 0           25221              12.4            2007.
    ##  3 casual                 1           21320              24.0            1966.
    ##  4 member                 1           15554              12.3            1965.
    ##  5 casual                 2           12672              22.4            2041.
    ##  6 member                 2            8593              12.4            1936.
    ##  7 casual                 3            7199              22.0            2120.
    ##  8 member                 3            5243              12.3            2002.
    ##  9 casual                 4            4660              19.7            2209.
    ## 10 member                 4            6160              12.5            2322.
    ## # … with 38 more rows

```r
df_analysis_bike_type <- df_clean %>%
  group_by(member_casual, rideable_type) %>%  #groups by usertype and bike type
  summarise(number_of_rides = n(), average_trip_time = mean(trip_time), average_distance = mean(trip_dist, )) %>%
  arrange(rideable_type, member_casual)
```

    ## `summarise()` has grouped output by 'member_casual'. You can override using the
    ## `.groups` argument.

```r
print(df_analysis_bike_type)
```

    ## # A tibble: 5 × 5
    ## # Groups:   member_casual [2]
    ##   member_casual rideable_type number_of_rides average_trip_time average_distance
    ##   <chr>         <chr>                   <int>             <dbl>            <dbl>
    ## 1 casual        classic_bike           888728              24.4            2087.
    ## 2 member        classic_bike          1708573              13.2            1966.
    ## 3 casual        docked_bike            174852              50.7            2176.
    ## 4 casual        electric_bike          694467              16.7            2257.
    ## 5 member        electric_bike          902432              11.0            2271.

Looking through the results, I can already start to see some of the
patterns of difference between the two types of users. Next I will
create some visuals to help highlight the insights and use for
presentation of my analysis.

## Step 5. Share

When we plot member vs casual usage by weekday, we should be able to
easily tell if there is any difference in usage by each group.

```r
df_analysis_weekday %>%
  #plots weekday to x, number of rides to y, and fill color (good for discrete values) to membership level
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  #Plots the 2 categories side by side instead of stacked
  geom_col(position = "dodge") +
  labs(title = "Membership and Casual Usage by Day of Week", x = "Day of Week", y = "Total Rides") +
  #Formatting look and feel
  theme(
    plot.title = element_text(family='Montserrat', face="bold", size=16, hjust = 0.5, margin = margin(t = 10, r = 00, b = 20, l = 0)),
    axis.title = element_text(family='Montserrat', face="bold", size = 10, hjust = 0.5),
    legend.title = element_text()
  ) +
  #Labels Legend with Discrete Values
  scale_fill_discrete(name = "Rider Type", labels = c("Casual", "Member")) +
  #Replaces Sci notation with whole number scale on y axis (continuous)
  scale_y_continuous(labels = comma)
```

![](/img/post_images/unnamed-chunk-15-1.png)

Based on the initial visual, I can see that members tend to ride more
often than casual riders during the week, while casual riders use
Cyclistic bikes more often on the weekends. This could suggest that
there is opportunity to either increase casual use price on the weekends
or offer a special promotion for weekend riders to upgrade their
membership.

After seeing this, I am also curious what the total usage looks like
throughout the week for both members and non-members just to compare.
For this I will rerun the plot with a stacked column instead of
side-by-side

```r
df_analysis_weekday %>%
  #plots weekday to x, number of rides to y, and fill color (good for discrete values) to membership level
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  #Plots the 2 categories stacked (default behavior)
  geom_col() +
  labs(title = "Membership and Casual Usage by Day of Week", x = "Day of Week", y = "Total Rides") +
  #Formatting look and feel
  theme(
    plot.title = element_text(family='Montserrat', face="bold", size=16, hjust = 0.5, margin = margin(t = 10, r = 00, b = 20, l = 0)),
    axis.title = element_text(family='Montserrat', face="bold", size = 10, hjust = 0.5),
    legend.title = element_text()
  ) +
  #Labels Legend with Discrete Values
  scale_fill_discrete(name = "Rider Type", labels = c("Casual", "Member")) +
  #Replaces Sci notation with whole number scale on y axis (continuous)
  scale_y_continuous(labels = comma)
```

![](/img/post_images/unnamed-chunk-16-1.png)

Now we can see that Saturday is our most popular day by a significant
margin. The high overall levels of use on Saturday could be something to
keep in mind as we move forward toward our recommendations. We can use
the same dataframe to visualize the differences in both average trip
time and average distance as well.

```r
df_analysis_weekday %>%
  ggplot(aes(x = weekday, y = average_trip_time, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Membership and Casual Average Trip Time by Day of Week", x = "Day of Week", y = "Average Trip Time") +
  theme(
    plot.title = element_text(family='Montserrat', face="bold", size=14, hjust = 0.5, margin = margin(t = 10, r = 00, b = 20, l = 0)),
    axis.title = element_text(family='Montserrat', face="bold", size = 10, hjust = 0.5),
    legend.title = element_text()
  ) +
  scale_fill_discrete(name = "Rider Type", labels = c("Casual", "Member")) +
  scale_y_continuous(labels = comma)
```

![](/img/post_images/unnamed-chunk-17-1.png)

```r
df_analysis_weekday %>%
  ggplot(aes(x = weekday, y = average_distance, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Membership and Casual Average Trip Distance by Day of Week", x = "Day of Week", y = "Average Trip Distance") +
  theme(
    plot.title = element_text(family='Montserrat', face="bold", size=14, hjust = 0.5, margin = margin(t = 10, r = 00, b = 20, l = 0)),
    axis.title = element_text(family='Montserrat', face="bold", size = 10, hjust = 0.5),
    legend.title = element_text()
  ) +
  scale_fill_discrete(name = "Rider Type", labels = c("Casual", "Member")) +
  scale_y_continuous(labels = comma)
```

![](/img/post_images/unnamed-chunk-18-1.png)

We can see that casual riders tend to have their bikes for a longer
period of time accross the board but the average distance traveled is
about the same for both members and non-members. This might indicate
that casual riders are using the bikes more for leisure or that they are
just not as experienced cyclists. Perhaps a time based incentive for
casual riders to upgrade could convince some people to purchase a
membership.

Now let’s see if there are any trends when looking at the rides by month

```r
df_analysis_month %>%
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Membership and Casual Usage by Month", x = "Month", y = "Total Rides") +
  theme(
    plot.title = element_text(family='Montserrat', face="bold", size=16, hjust = 0.5, margin = margin(t = 10, r = 00, b = 20, l = 0)),
    axis.title = element_text(family='Montserrat', face="bold", size = 10, hjust = 0.5),
    axis.text.x = element_text(angle = 40),
    legend.title = element_text()
  ) +
  scale_fill_discrete(name = "Rider Type", labels = c("Casual", "Member")) +
  scale_y_continuous(labels = comma)
```

![](/img/post_images/unnamed-chunk-19-1.png)

With this visualization we can see that all users take more rides during
the summer months, and the fewest during the winter. But we can also see
that the difference in seasonal usage by casual members is much larger
than with members. This could indicate that some casual members may be
more likely to purchase a season pass rather than a yearly one.

Even without plotting the stacked chart, I can tell that July will have
the highest usage overall. This may indicate further opportunities for
improving our winter usage rates (perhaps marketing the exercise
benefit), or providing more incentive for casual users to get a
membership during the busiest times.

As we did with the day of week visualizations, we can look at average
trip time and average trip distance by month as well

```r
df_analysis_month %>%
  ggplot(aes(x = month, y = average_trip_time, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Membership and Casual Average Trip Time by Month", x = "Month", y = "Average Trip Time") +
  theme(
    plot.title = element_text(family='Montserrat', face="bold", size=14, hjust = 0.5, margin = margin(t = 10, r = 00, b = 20, l = 0)),
    axis.title = element_text(family='Montserrat', face="bold", size = 10, hjust = 0.5),
    axis.text.x = element_text(angle = 40),
    legend.title = element_text()
  ) +
  scale_fill_discrete(name = "Rider Type", labels = c("Casual", "Member")) +
  scale_y_continuous(labels = comma)
```

![](/img/post_images/unnamed-chunk-20-1.png)

```r
df_analysis_month %>%
  ggplot(aes(x = month, y = average_distance, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Membership and Casual Average Trip Distance by Month", x = "Month", y = "Average Trip Distance") +
  theme(
    plot.title = element_text(family='Montserrat', face="bold", size=14, hjust = 0.5, margin = margin(t = 10, r = 00, b = 20, l = 0)),
    axis.title = element_text(family='Montserrat', face="bold", size = 10, hjust = 0.5),
    axis.text.x = element_text(angle = 40),
    legend.title = element_text()
  ) +
  scale_fill_discrete(name = "Rider Type", labels = c("Casual", "Member")) +
  scale_y_continuous(labels = comma)
```

![](/img/post_images/unnamed-chunk-21-1.png)

We can see again that members have significantly lower trip duration on
average and it stays relatively consistent through the whole year. The
casual riders have higher average trip times in every month, with an
interesting trend of shorter average duration in November and December,
but longer average duration in January, March and May. Further analysis
would be required to investigate possible reasons or correlations there.

As with the day of week visualizations, we can see that the average trip
distance is pretty consistent throughout the year and between the
different rider types.

Another factor I’d like to check would be the week of the year and if we
can see any spikes during odd months that might suggest further
opportunities.

```r
df_analysis_week %>%
  #plots week of year to x, number of rides to y, and fill color (good for discrete values) to membership level
  ggplot() +
  #Plots the 2 categories side by side instead of stacked
  geom_col(mapping = aes(x = week, y = number_of_rides, fill = member_casual),position = "dodge") +
  labs(title = "Membership and Casual Usage by Week of Year", x = "Week Number", y = "Total Rides") +
  #Formatting look and feel
  theme(
    plot.title = element_text(family='Montserrat', face="bold", size=16, hjust = 0.5, margin = margin(t = 10, r = 00, b = 20, l = 0)),
    axis.title = element_text(family='Montserrat', face="bold", size = 10, hjust = 0.5),
    axis.text.x = element_text(angle = 90),
    legend.title = element_text()
  ) +
  #Labels Legend with Discrete Values
  scale_fill_discrete(name = "Rider Type", labels = c("Casual", "Member")) +
  #Replaces Sci notation with whole number scale on y axis (continuous)
  scale_y_continuous(labels = comma)
```

![](/img/post_images/unnamed-chunk-22-1.png)

This one is a bit messy but it does show a couple of interesting surges
throughout the year both for members and casual users. I will leave it
here to investigate later, but will probably not use it for now. I will
forego the week of year visualizations for average trip duration and
average trip distance for now also.

After the analysis of usage by different date factors, I am curious to
see what the usage by time for members and non members looks like. I
will refer to the time by which hour the trip was started for this
portion of the analysis.

```r
df_analysis_hour %>%
  #plots hour to x, number of rides to y, and fill color (good for discrete values) to membership level
  ggplot() +
  #Plots the 2 categories side by side instead of stacked
  geom_col(mapping = aes(x = hour_start, y = number_of_rides, fill = member_casual),position = "dodge") +
  labs(title = "Membership and Casual Usage by Hour", x = "Hour", y = "Total Rides") +
  #Formatting look and feel
  theme(
    plot.title = element_text(family='Montserrat', face="bold", size=16, hjust = 0.5, margin = margin(t = 10, r = 00, b = 20, l = 0)),
    axis.title = element_text(family='Montserrat', face="bold", size = 10, hjust = 0.5),
    axis.text.x = element_text(angle = 90),
    legend.title = element_text()
  ) +
  #Labels Legend with Discrete Values
  scale_fill_discrete(name = "Rider Type", labels = c("Casual", "Member")) +
  #Replaces Sci notation with whole number scale on y axis (continuous)
  scale_y_continuous(labels = comma) +
  scale_x_continuous(
    breaks = c(0:23),
    labels = hour_v)
```

![](/img/post_images/unnamed-chunk-23-1.png)

Members use the bikes at a much higher rate during morning and evening
hours. Specifically around 8am and 5pm indicating a possible commuter
subscriber base. Casual users, on the other hand, use the bikes
progressively more throughout the day, peaking at 5pm. The casual riders
seem to prefer both evening use, as well as summertime use, indicating
they may be more sensitive to temperature or weather. It would be
interesting to follow this up with the local weather data to test that
in future analyses.

Lets see what the duration and distance averages look like by hour

```r
df_analysis_hour %>%
  #plots weekday to x, average trip time to y, and fill color (good for 2 discrete values) to membership level
  ggplot() +
  #Plots the 2 categories side by side instead of stacked
  geom_col(mapping = aes(x = hour_start, y = average_trip_time, fill = member_casual),position = "dodge") +
  labs(title = "Membership and Casual Average Trip Time by Hour", x = "Hour", y = "Average Trip Time") +
  #Formatting look and feel
  theme(
    plot.title = element_text(family='Montserrat', face="bold", size=14, hjust = 0.5, margin = margin(t = 10, r = 00, b = 20, l = 0)),
    axis.title = element_text(family='Montserrat', face="bold", size = 10, hjust = 0.5),
    axis.text.x = element_text(angle = 90),
    legend.title = element_text()
  ) +
  #Labels Legend with Discrete Values
  scale_fill_discrete(name = "Rider Type", labels = c("Casual", "Member")) +
  #Replaces Sci notation with whole number scale on y axis (continuous)
  scale_y_continuous(labels = comma) +
  scale_x_continuous(
    breaks = c(0:23),
    labels = hour_v)
```

![](/img/post_images/unnamed-chunk-24-1.png)

```r
df_analysis_hour %>%
  #plots weekday to x, average trip time to y, and fill color (good for 2 discrete values) to membership level
  ggplot() +
  #Plots the 2 categories side by side instead of stacked
  geom_col(mapping = aes(x = hour_start, y = average_distance, fill = member_casual),position = "dodge") +
  labs(title = "Membership and Casual Average Trip Distance by Hour", x = "Hour", y = "Average Trip Distance") +
  #Formatting look and feel
  theme(
    plot.title = element_text(family='Montserrat', face="bold", size=14, hjust = 0.5, margin = margin(t = 10, r = 00, b = 20, l = 0)),
    axis.title = element_text(family='Montserrat', face="bold", size = 10, hjust = 0.5),
    axis.text.x = element_text(angle = 90),
    legend.title = element_text()
  ) +
  #Labels Legend with Discrete Values
  scale_fill_discrete(name = "Rider Type", labels = c("Casual", "Member")) +
  #Replaces Sci notation with whole number scale on y axis (continuous)
  scale_y_continuous(labels = comma) +
  scale_x_continuous(
    breaks = c(0:23),
    labels = hour_v)
```

![](/img/post_images/unnamed-chunk-25-1.png)

Here we can see that distance stays consistent as seems to be the usual,
but there is a big dip in average trip duration during morning commute
hours. Both groups have this dip, but the casual riders average trip
duration decreases far more than the members between 4am and 8am. There
could be some value in that information for converting bicycle
commuters.

Finally I want to look at the use of different bike types by the two
groups.

```r
df_analysis_bike_type %>%
  #plots hour to x, number of rides to y, and fill color (good for discrete values) to membership level
  ggplot() +
  #Plots the 2 categories side by side instead of stacked
  geom_col(mapping = aes(x = rideable_type, y = number_of_rides, fill = member_casual),position = "dodge") +
  labs(title = "Membership and Casual Usage by Bike Type", x = "Rideable", y = "Total Rides") +
  #Formatting look and feel
  theme(
    plot.title = element_text(family='Montserrat', face="bold", size=16, hjust = 0.5, margin = margin(t = 10, r = 00, b = 20, l = 0)),
    axis.title = element_text(family='Montserrat', face="bold", size = 10, hjust = 0.5),
    legend.title = element_text()
  ) +
  #Labels Legend with Discrete Values
  scale_fill_discrete(name = "Rider Type", labels = c("Casual", "Member")) +
  #Replaces Sci notation with whole number scale on y axis (continuous)
  scale_y_continuous(labels = comma) +
  scale_x_discrete(labels = c("Classic Bike", "Docked Bike", "Electric Bike"))
```

![](/img/post_images/unnamed-chunk-26-1.png)

The first and immediately noticeable trend to me here is the percentage
of electric bike trips by casual riders compared to the percentage of
classic bike trips. Our annual members use the classic bikes more often
by far. With the casual users however, they seem much more interested in
the electric bike option relative to the total number of each. The other
thing I notice is that members do not use the docked bikes at all. This
could be significant but more investigation is needed

## Step 6. Act

### Based on the results of my analysis, I believe the main differences between the members and casual riders are:

#### Members use the bicycles for transportation while casual riders motivation is liesure

#### Members use the bicycles more often and for less time per ride

#### Members ride more during the week and casual riders ride on the weekends

#### Members ride more in the winter than casual riders

### Based on these findings my top three recommendations are:

#### 1. Charge non-members for ride time while members enjoy unlimited ride time

Average trip time for non-members is often more than double that of our
annual members. Casual riders seem to value the ability to spend more
time using their bike on each ride, so offering them unlimited time for
becoming a member could provide an avenue to convince them to sign up.
While further analysis is required, I believe casual riders might sign
up to avoid the hassle of watching their time and being able to enjoy
their rides without worry.

#### 2. Create an aditional membership tier

The casual riders use our bikes far more frequently during the summer
months. They may be willing to pay for a season pass that gives them a
middle ground for the time they will be using our bikes more frequently.
I believe that the casual riders might not see the benefit of paying and
annual fee when there are 4-5 months that they will rarely use the
service. The best rate should go to our annual members, but a seasonal
or monthly rate could provide an extra incentive to try and get used to
using the bikes

#### 3. Offer more electric bikes

Relative to the number of trips, our casual riders seem to be more
interested in electric bikes than our current members. This gives us a
huge potential to offer incentives or marketing specifically toward the
riders who prefer the electric bike experience. Perhaps focusing on
electric could also increase our morning ridership as riders would not
have to work up a sweat on their way in to work each morning.

## Further analysis

I believe we have a lot of room to expand and refine these findings. We
should investigate the weather aspects, the potential to expand our
electric fleet, and the costs and benefits of creating additional
membership tiers. As we continue to grow, some of these changes could
help even out revenue throughout the year, attract new customers, and
help us to expand into new markets.
