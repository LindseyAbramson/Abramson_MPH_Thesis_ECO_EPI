MPH Thesis: Lindsey Abramson
================
2025-10-01

## Import and Tidy Data

Import dataset

``` r
ticknyc_df=
read_csv("data/Tick_nyc_clean_23_24.csv") |>
  janitor::clean_names() |>
    mutate(date = mdy(date))
```

    ## Rows: 2070 Columns: 48
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (15): Transect No., County, Site, Site function, Transect type, Date, Ha...
    ## dbl (33): Drag round, Year, Longitude Start, Latitude Start, Amblyomma ameri...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Standardize by transect

## Descriptive Summaries

Overview

``` r
ticknyc_df |>
  summarise(
      Total_Transects = n(),
    Total_Ticks = sum(total, na.rm = TRUE),
    Mean_Ticks_Per_Transect = round(mean(total, na.rm = TRUE), 2),
    Max_Ticks_Single_Transect = max(total, na.rm = TRUE)
  ) |>
   knitr::kable()
```

| Total_Transects | Total_Ticks | Mean_Ticks_Per_Transect | Max_Ticks_Single_Transect |
|---:|---:|---:|---:|
| 2070 | 24643 | 12 | 386 |

County summary

``` r
ticknyc_df |>
  group_by(county) |>
  summarise(
    Transects = n(),
    Total_Ticks = sum(total, na.rm = TRUE),
    Mean_ticks_per_transect = mean(total, na.rm = TRUE),
    Median_ticks_per_transect = median(total, na.rm = TRUE),
    Transects_with_ticks = sum(total > 0, na.rm = TRUE),
    Percent_transects_with_ticks = round(mean(total > 0, na.rm = TRUE) * 100, 1),
  ) |>
  arrange(desc(Total_Ticks)) |>
   knitr::kable()
```

| county | Transects | Total_Ticks | Mean_ticks_per_transect | Median_ticks_per_transect | Transects_with_ticks | Percent_transects_with_ticks |
|:---|---:|---:|---:|---:|---:|---:|
| Suffolk | 694 | 17626 | 25.3976945 | 12 | 591 | 85.2 |
| Nassau | 638 | 3899 | 6.1401575 | 1 | 367 | 57.8 |
| Staten Island | 364 | 2981 | 8.3501401 | 3 | 271 | 75.9 |
| Queens | 298 | 96 | 0.3298969 | 0 | 54 | 18.6 |
| Brooklyn | 76 | 41 | 0.5394737 | 0 | 16 | 21.1 |

Transect type summary

``` r
ticknyc_df |>
  group_by(transect_type) |>
  summarise(
   Transects = n(),
    Total_Ticks = sum(total, na.rm = TRUE),
    Mean_ticks_per_transect = mean(total, na.rm = TRUE),
    Median_ticks_per_transect = median(total, na.rm = TRUE),
    Transects_with_ticks = sum(total > 0, na.rm = TRUE),
    Percent_transects_with_ticks = round(mean(total > 0, na.rm = TRUE) * 100, 1)) |> 
  arrange(desc(Total_Ticks)) |>
   knitr::kable()
```

| transect_type | Transects | Total_Ticks | Mean_ticks_per_transect | Median_ticks_per_transect | Transects_with_ticks | Percent_transects_with_ticks |
|:---|---:|---:|---:|---:|---:|---:|
| Trail | 828 | 10950 | 13.304982 | 2 | 538 | 65.4 |
| Interior | 803 | 10493 | 13.215365 | 3 | 517 | 65.1 |
| Edge | 421 | 3087 | 7.385167 | 1 | 234 | 56.0 |
| NA | 18 | 113 | 6.277778 | 1 | 10 | 55.6 |

Species counts

``` r
ticknyc_df |>
  summarise(
    ixodes_total = sum(across(starts_with("ixodes")), na.rm = TRUE),
    amblyomma_total = sum(across(starts_with("amblyomma")), na.rm = TRUE),
    dermacentor_total = sum(across(starts_with("dermacentor")), na.rm = TRUE),
    haemaphysalis_total = sum(across(starts_with("haemaphysalis")), na.rm = TRUE),
    rhipicephalus_total = sum(across(starts_with("rhipicephalus")), na.rm = TRUE),
    unknown_total = sum(across(starts_with("unknown")), na.rm = TRUE)
) |>
   knitr::kable()
```

| ixodes_total | amblyomma_total | dermacentor_total | haemaphysalis_total | rhipicephalus_total | unknown_total |
|---:|---:|---:|---:|---:|---:|
| 9923 | 12435 | 270 | 2043 | 2 | 8 |

Species + life stage count

``` r
ticknyc_df |>
  summarise(across(amblyomma_americanum_adult:unknown_unknown, ~sum(., na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "Species_LifeStage", values_to = "Count") |>
  arrange(desc(Count)) |>
  print() |>
   knitr::kable()
```

    ## # A tibble: 28 × 2
    ##    Species_LifeStage               Count
    ##    <chr>                           <dbl>
    ##  1 amblyomma_americanum_nymph      10026
    ##  2 ixodes_scapularis_nymph          9301
    ##  3 amblyomma_americanum_adult       1949
    ##  4 haemaphysalis_longicornis_nymph  1472
    ##  5 ixodes_scapularis_larva           520
    ##  6 amblyomma_americanum_larva        448
    ##  7 haemaphysalis_longicornis_larva   414
    ##  8 dermacentor_variabilis_adult      261
    ##  9 haemaphysalis_longicornis_adult   136
    ## 10 ixodes_scapularis_adult           102
    ## # ℹ 18 more rows

| Species_LifeStage                    | Count |
|:-------------------------------------|------:|
| amblyomma_americanum_nymph           | 10026 |
| ixodes_scapularis_nymph              |  9301 |
| amblyomma_americanum_adult           |  1949 |
| haemaphysalis_longicornis_nymph      |  1472 |
| ixodes_scapularis_larva              |   520 |
| amblyomma_americanum_larva           |   448 |
| haemaphysalis_longicornis_larva      |   414 |
| dermacentor_variabilis_adult         |   261 |
| haemaphysalis_longicornis_adult      |   136 |
| ixodes_scapularis_adult              |   102 |
| haemaphysalis_leporispalustris_nymph |    19 |
| amblyomma_maculatum_nymph            |     7 |
| dermacentor_variabilis_nymph         |     6 |
| amblyomma_maculatum_adult            |     4 |
| unknown_nymph                        |     3 |
| dermacentor_variabilis_larva         |     2 |
| haemaphysalis_leporispalustris_adult |     2 |
| rhipicephalus_sanguineus_nymph       |     2 |
| unknown_adult                        |     2 |
| unknown_unknown                      |     2 |
| amblyomma_maculatum_larva            |     1 |
| dermacentor_albipictus_adult         |     1 |
| unknown_larva                        |     1 |
| dermacentor_albipictus_nymph         |     0 |
| dermacentor_albipictus_larva         |     0 |
| haemaphysalis_leporispalustris_larva |     0 |
| rhipicephalus_sanguineus_adult       |     0 |
| rhipicephalus_sanguineus_larva       |     0 |

Species by transect type

``` r
ticknyc_df |>
  group_by(transect_type) |>
  summarise(
      ixodes_total = sum(across(starts_with("ixodes")), na.rm = TRUE),
    amblyomma_total = sum(across(starts_with("amblyomma")), na.rm = TRUE),
    dermacentor_total = sum(across(starts_with("dermacentor")), na.rm = TRUE),
    haemaphysalis_total = sum(across(starts_with("haemaphysalis")), na.rm = TRUE),
    rhipicephalus_total = sum(across(starts_with("rhipicephalus")), na.rm = TRUE),
    unknown_total = sum(across(starts_with("unknown")), na.rm = TRUE)
  ) |>
  pivot_longer(-transect_type, names_to = "species", values_to = "count") %>%
  pivot_wider(names_from = transect_type, values_from = count, values_fill = 0) |>
   knitr::kable()
```

| species             | Edge | Interior | Trail |  NA |
|:--------------------|-----:|---------:|------:|----:|
| ixodes_total        |  874 |     4661 |  4355 |  33 |
| amblyomma_total     | 1678 |     4865 |  5880 |  12 |
| dermacentor_total   |   99 |       41 |   124 |   6 |
| haemaphysalis_total |  432 |      935 |   614 |  62 |
| rhipicephalus_total |    2 |        0 |     0 |   0 |
| unknown_total       |    2 |        3 |     3 |   0 |

Species count in each county

``` r
ticknyc_df |>
  group_by(county) |>
  summarise(
    ixodes_total = sum(across(starts_with("ixodes")), na.rm = TRUE),
    amblyomma_total = sum(across(starts_with("amblyomma")), na.rm = TRUE),
    dermacentor_total = sum(across(starts_with("dermacentor")), na.rm = TRUE),
    haemaphysalis_total = sum(across(starts_with("haemaphysalis")), na.rm = TRUE),
    rhipicephalus_total = sum(across(starts_with("rhipicephalus")), na.rm = TRUE),
    unknown_total = sum(across(starts_with("unknown")), na.rm = TRUE)
  ) |>
  pivot_longer(-county, names_to = "species", values_to = "count") |>
  pivot_wider(names_from = county, values_from = count, values_fill = 0) |>
    knitr::kable()
```

| species             | Brooklyn | Nassau | Queens | Staten Island | Suffolk |
|:--------------------|---------:|-------:|-------:|--------------:|--------:|
| ixodes_total        |       10 |   3721 |     42 |           615 |    5535 |
| amblyomma_total     |        6 |     23 |      7 |           786 |   11613 |
| dermacentor_total   |       24 |    114 |     26 |             1 |     105 |
| haemaphysalis_total |        1 |     47 |     21 |          1607 |     367 |
| rhipicephalus_total |        0 |      0 |      0 |             0 |       2 |
| unknown_total       |        0 |      2 |      1 |             1 |       4 |

Life stages count

``` r
ticknyc_df |>
  summarise(
    Adults = sum(across(ends_with("_adult")), na.rm = TRUE),
    Nymphs = sum(across(ends_with("_nymph")), na.rm = TRUE),
    Larvae = sum(across(ends_with("_larva")), na.rm = TRUE),
    Total = Adults + Nymphs + Larvae
  ) |>
    knitr::kable()
```

| Adults | Nymphs | Larvae | Total |
|-------:|-------:|-------:|------:|
|   2457 |  20836 |   1386 | 24679 |

Life staage count by county

``` r
ticknyc_df |>
  group_by(county) |>
  summarise(
    Adults = sum(across(ends_with("_adult")), na.rm = TRUE),
    Nymphs = sum(across(ends_with("_nymph")), na.rm = TRUE),
    Larvae = sum(across(ends_with("_larva")), na.rm = TRUE),
    Total = Adults + Nymphs + Larvae
  ) |>
  knitr::kable()
```

| county        | Adults | Nymphs | Larvae | Total |
|:--------------|-------:|-------:|-------:|------:|
| Brooklyn      |     29 |     12 |      0 |    41 |
| Nassau        |    183 |   3292 |    432 |  3907 |
| Queens        |     34 |     63 |      0 |    97 |
| Staten Island |    281 |   2334 |    394 |  3009 |
| Suffolk       |   1930 |  15135 |    560 | 17625 |

Life stage count by transect type

``` r
ticknyc_df |>
  group_by(transect_type) |>
  summarise(
    Adults = sum(across(ends_with("_adult")), na.rm = TRUE),
    Nymphs = sum(across(ends_with("_nymph")), na.rm = TRUE),
    Larvae = sum(across(ends_with("_larva")), na.rm = TRUE),
    Total = Adults + Nymphs + Larvae
  ) |>
  knitr::kable()
```

| transect_type | Adults | Nymphs | Larvae | Total |
|:--------------|-------:|-------:|-------:|------:|
| Edge          |    460 |   2627 |      0 |  3087 |
| Interior      |    712 |   8665 |   1127 | 10504 |
| Trail         |   1276 |   9482 |    217 | 10975 |
| NA            |      9 |     62 |     42 |   113 |
