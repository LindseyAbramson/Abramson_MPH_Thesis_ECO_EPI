Abramson_MPHThesis_ECOEPI
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
overview <- ticknyc_df %>%
  summarise(
    Total_Transects = n(),
    Total_Ticks = sum(total, na.rm = TRUE),
    Mean_Ticks_Per_Transect = round(mean(total, na.rm = TRUE), 2),
    Max_Ticks_Single_Transect = max(total, na.rm = TRUE)
  )

cat("\n\n### Overall Summary\n")
```

    ## 
    ## 
    ## ### Overall Summary

``` r
kable(overview, format = "simple", caption = "Overall sampling statistics")
```

| Total_Transects | Total_Ticks | Mean_Ticks_Per_Transect | Max_Ticks_Single_Transect |
|---:|---:|---:|---:|
| 2070 | 24643 | 12 | 386 |

Overall sampling statistics

County summary

``` r
county_totals <- ticknyc_df %>%
  group_by(county) %>%
  summarise(
    Transects = n(),
    Total_Ticks = sum(total, na.rm = TRUE),
    Mean_Ticks_Per_Transect = round(mean(total, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(Total_Ticks))

cat("\n\n### County Totals\n")
```

    ## 
    ## 
    ## ### County Totals

``` r
kable(county_totals, format = "simple", caption = "Tick counts by county")
```

| county        | Transects | Total_Ticks | Mean_Ticks_Per_Transect |
|:--------------|----------:|------------:|------------------------:|
| Suffolk       |       694 |       17626 |                   25.40 |
| Nassau        |       638 |        3899 |                    6.14 |
| Staten Island |       364 |        2981 |                    8.35 |
| Queens        |       298 |          96 |                    0.33 |
| Brooklyn      |        76 |          41 |                    0.54 |

Tick counts by county

Transect type summary

``` r
transect_totals <- ticknyc_df %>%
  group_by(transect_type) %>%
  summarise(
    Transects = n(),
    Total_Ticks = sum(total, na.rm = TRUE),
    Mean_Ticks_Per_Transect = round(mean(total, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(Total_Ticks))

cat("\n\n### Transect Type Totals\n")
```

    ## 
    ## 
    ## ### Transect Type Totals

``` r
kable(transect_totals, format = "simple", caption = "Tick counts by transect type")
```

| transect_type | Transects | Total_Ticks | Mean_Ticks_Per_Transect |
|:--------------|----------:|------------:|------------------------:|
| Trail         |       828 |       10950 |                   13.30 |
| Interior      |       803 |       10493 |                   13.22 |
| Edge          |       421 |        3087 |                    7.39 |
| NA            |        18 |         113 |                    6.28 |

Tick counts by transect type

Species by transect type

``` r
species_transect <- ticknyc_df %>%
  group_by(transect_type) %>%
  summarise(
    ixodes_scapularis = sum(ixodes_scapularis_adult + ixodes_scapularis_nymph + ixodes_scapularis_larva, na.rm = TRUE),
    haemaphysalis_longicornis = sum(haemaphysalis_longicornis_adult + haemaphysalis_longicornis_nymph + haemaphysalis_longicornis_larva, na.rm = TRUE),
    dermacentor_variabilis = sum(dermacentor_variabilis_adult + dermacentor_variabilis_nymph + dermacentor_variabilis_larva, na.rm = TRUE),
    amblyomma_americanum = sum(amblyomma_americanum_adult + amblyomma_americanum_nymph + amblyomma_americanum_larva, na.rm = TRUE),
    unknown = sum(unknown_adult + unknown_nymph + unknown_larva + unknown_unknown, na.rm = TRUE)
  ) %>%
  pivot_longer(-transect_type, names_to = "species", values_to = "count") %>%
  pivot_wider(names_from = transect_type, values_from = count, values_fill = 0)

cat("\n\n### Species by Transect Type\n")
```

    ## 
    ## 
    ## ### Species by Transect Type

``` r
kable(species_transect, format = "simple", caption = "Total tick counts by species and transect type")
```

| species                   | Edge | Interior | Trail |  NA |
|:--------------------------|-----:|---------:|------:|----:|
| ixodes_scapularis         |  874 |     4661 |  4355 |  33 |
| haemaphysalis_longicornis |  423 |      931 |   606 |  62 |
| dermacentor_variabilis    |   99 |       40 |   124 |   6 |
| amblyomma_americanum      | 1676 |     4865 |  5870 |  12 |
| unknown                   |    2 |        3 |     3 |   0 |

Total tick counts by species and transect type

Species + life stage count

``` r
ticknyc_df |>
  summarise(across(amblyomma_americanum_adult:unknown_unknown, ~sum(., na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "Species_LifeStage", values_to = "Count") |>
  arrange(desc(Count)) |>
  print()
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

Species count in each county

``` r
species_county <- ticknyc_df %>%
  group_by(county) %>%
  summarise(
    ixodes_scapularis = sum(ixodes_scapularis_adult + ixodes_scapularis_nymph + ixodes_scapularis_larva, na.rm = TRUE),
    haemaphysalis_longicornis = sum(haemaphysalis_longicornis_adult + haemaphysalis_longicornis_nymph + haemaphysalis_longicornis_larva, na.rm = TRUE),
    dermacentor_variabilis = sum(dermacentor_variabilis_adult + dermacentor_variabilis_nymph + dermacentor_variabilis_larva, na.rm = TRUE),
    amblyomma_americanum = sum(amblyomma_americanum_adult + amblyomma_americanum_nymph + amblyomma_americanum_larva, na.rm = TRUE),
    unknown = sum(unknown_adult + unknown_nymph + unknown_larva + unknown_unknown, na.rm = TRUE)
  ) %>%
  pivot_longer(-county, names_to = "species", values_to = "count") %>%
  pivot_wider(names_from = county, values_from = count, values_fill = 0)

cat("### Species by County\n")
```

    ## ### Species by County

``` r
kable(species_county, format = "simple", caption = "Total tick counts by species and county")
```

| species                   | Brooklyn | Nassau | Queens | Staten Island | Suffolk |
|:--------------------------|---------:|-------:|-------:|--------------:|--------:|
| ixodes_scapularis         |       10 |   3721 |     42 |           615 |    5535 |
| haemaphysalis_longicornis |        1 |     44 |     21 |          1601 |     355 |
| dermacentor_variabilis    |       24 |    114 |     25 |             1 |     105 |
| amblyomma_americanum      |        5 |     23 |      7 |           781 |   11607 |
| unknown                   |        0 |      2 |      1 |             1 |       4 |

Total tick counts by species and county

Life stages count

``` r
# LIFE STAGES SUMMARY
ticknyc_df |>
  summarise(
    Adults = sum(across(ends_with("_adult")), na.rm = TRUE),
    Nymphs = sum(across(ends_with("_nymph")), na.rm = TRUE),
    Larvae = sum(across(ends_with("_larva")), na.rm = TRUE),
    Total = Adults + Nymphs + Larvae
  ) |>
  print()
```

    ## # A tibble: 1 × 4
    ##   Adults Nymphs Larvae Total
    ##    <dbl>  <dbl>  <dbl> <dbl>
    ## 1   2457  20836   1386 24679

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
  print()
```

    ## # A tibble: 5 × 5
    ##   county        Adults Nymphs Larvae Total
    ##   <chr>          <dbl>  <dbl>  <dbl> <dbl>
    ## 1 Brooklyn          29     12      0    41
    ## 2 Nassau           183   3292    432  3907
    ## 3 Queens            34     63      0    97
    ## 4 Staten Island    281   2334    394  3009
    ## 5 Suffolk         1930  15135    560 17625

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
  print()
```

    ## # A tibble: 4 × 5
    ##   transect_type Adults Nymphs Larvae Total
    ##   <chr>          <dbl>  <dbl>  <dbl> <dbl>
    ## 1 Edge             460   2627      0  3087
    ## 2 Interior         712   8665   1127 10504
    ## 3 Trail           1276   9482    217 10975
    ## 4 <NA>               9     62     42   113

Species (lfiestages combined) in each transect

``` r
species_transect_wide <- ticknyc_df %>%
  group_by(transect_type) %>%
  summarise(
    ixodes_scapularis = sum(ixodes_scapularis_adult + ixodes_scapularis_nymph + ixodes_scapularis_larva, na.rm = TRUE),
    haemaphysalis_longicornis = sum(haemaphysalis_longicornis_adult + haemaphysalis_longicornis_nymph + haemaphysalis_longicornis_larva, na.rm = TRUE),
    dermacentor_variabilis = sum(dermacentor_variabilis_adult + dermacentor_variabilis_nymph + dermacentor_variabilis_larva, na.rm = TRUE),
    amblyomma_americanum = sum(amblyomma_americanum_adult + amblyomma_americanum_nymph + amblyomma_americanum_larva, na.rm = TRUE),
    unknown = sum(unknown_adult + unknown_nymph + unknown_larva + unknown_unknown, na.rm = TRUE)
  ) %>%
  pivot_longer(-transect_type, names_to = "species", values_to = "count") %>%
  pivot_wider(names_from = transect_type, values_from = count, values_fill = 0)

print(species_transect_wide)
```

    ## # A tibble: 5 × 5
    ##   species                    Edge Interior Trail  `NA`
    ##   <chr>                     <dbl>    <dbl> <dbl> <dbl>
    ## 1 ixodes_scapularis           874     4661  4355    33
    ## 2 haemaphysalis_longicornis   423      931   606    62
    ## 3 dermacentor_variabilis       99       40   124     6
    ## 4 amblyomma_americanum       1676     4865  5870    12
    ## 5 unknown                       2        3     3     0

``` r
library(tidyverse)

# Simple table: species as rows, counties as columns
species_county <- ticknyc_df %>%
  group_by(county) %>%
  summarise(
    ixodes_scapularis = sum(ixodes_scapularis_adult + ixodes_scapularis_nymph + ixodes_scapularis_larva, na.rm = TRUE),
    haemaphysalis_longicornis = sum(haemaphysalis_longicornis_adult + haemaphysalis_longicornis_nymph + haemaphysalis_longicornis_larva, na.rm = TRUE),
    dermacentor_variabilis = sum(dermacentor_variabilis_adult + dermacentor_variabilis_nymph + dermacentor_variabilis_larva, na.rm = TRUE),
    amblyomma_americanum = sum(amblyomma_americanum_adult + amblyomma_americanum_nymph + amblyomma_americanum_larva, na.rm = TRUE),
    unknown = sum(unknown_adult + unknown_nymph + unknown_larva + unknown_unknown, na.rm = TRUE)
  ) %>%
  pivot_longer(-county, names_to = "species", values_to = "count") %>%
  pivot_wider(names_from = county, values_from = count, values_fill = 0)

print(species_county)
```

    ## # A tibble: 5 × 6
    ##   species                   Brooklyn Nassau Queens `Staten Island` Suffolk
    ##   <chr>                        <dbl>  <dbl>  <dbl>           <dbl>   <dbl>
    ## 1 ixodes_scapularis               10   3721     42             615    5535
    ## 2 haemaphysalis_longicornis        1     44     21            1601     355
    ## 3 dermacentor_variabilis          24    114     25               1     105
    ## 4 amblyomma_americanum             5     23      7             781   11607
    ## 5 unknown                          0      2      1               1       4

``` r
species_county_transect <- ticknyc_df %>%
  group_by(county, transect_type) %>%
  summarise(
    ixodes_scapularis = sum(ixodes_scapularis_adult + ixodes_scapularis_nymph + ixodes_scapularis_larva, na.rm = TRUE),
    haemaphysalis_longicornis = sum(haemaphysalis_longicornis_adult + haemaphysalis_longicornis_nymph + haemaphysalis_longicornis_larva, na.rm = TRUE),
    dermacentor_variabilis = sum(dermacentor_variabilis_adult + dermacentor_variabilis_nymph + dermacentor_variabilis_larva, na.rm = TRUE),
    amblyomma_americanum = sum(amblyomma_americanum_adult + amblyomma_americanum_nymph + amblyomma_americanum_larva, na.rm = TRUE),
    unknown = sum(unknown_adult + unknown_nymph + unknown_larva + unknown_unknown, na.rm = TRUE)
  ) %>%
  pivot_longer(-c(county, transect_type), names_to = "species", values_to = "count") %>%
  pivot_wider(names_from = c(county, transect_type), values_from = count, values_fill = 0)
```

    ## `summarise()` has grouped output by 'county'. You can override using the
    ## `.groups` argument.

``` r
print(species_county_transect)
```

    ## # A tibble: 5 × 20
    ##   species Brooklyn_Edge Brooklyn_Interior Brooklyn_Trail Brooklyn_NA Nassau_Edge
    ##   <chr>           <dbl>             <dbl>          <dbl>       <dbl>       <dbl>
    ## 1 ixodes…             0                 0             10           0         239
    ## 2 haemap…             0                 1              0           0          12
    ## 3 dermac…             2                 0             19           3          27
    ## 4 amblyo…             0                 1              4           0           1
    ## 5 unknown             0                 0              0           0           1
    ## # ℹ 14 more variables: Nassau_Interior <dbl>, Nassau_Trail <dbl>,
    ## #   Nassau_NA <dbl>, Queens_Edge <dbl>, Queens_Interior <dbl>,
    ## #   Queens_Trail <dbl>, Queens_NA <dbl>, `Staten Island_Edge` <dbl>,
    ## #   `Staten Island_Interior` <dbl>, `Staten Island_Trail` <dbl>,
    ## #   `Staten Island_NA` <dbl>, Suffolk_Edge <dbl>, Suffolk_Interior <dbl>,
    ## #   Suffolk_Trail <dbl>
