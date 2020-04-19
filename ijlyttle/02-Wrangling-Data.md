Wrangling Data
================
Ian Lyttle
2020-04-19

The purpose of this document is to wrangle the data into useful forms.
We will write out two data frames: `iowa_counties` and
`iowa_county_population`.

``` r
library("fs")
library("tidyverse")
```

    ## ── Attaching packages ────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.0          ✓ purrr   0.3.3     
    ## ✓ tibble  2.1.3          ✓ dplyr   0.8.4     
    ## ✓ tidyr   1.0.0          ✓ stringr 1.4.0     
    ## ✓ readr   1.3.1.9000     ✓ forcats 0.4.0

    ## ── Conflicts ───────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library("lubridate")
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library("readxl")
library("rvest")
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
dir_source <- path("data", "download")
dir_target <- path("data", "wrangle")

dir_create(dir_target)
```

## Population data

``` r
iowa_county_population <-
  read_xls(
    path = path(dir_source, "iowa_counties_population.xls"), 
    sheet = "Counties", 
    range = "A7:M107"
  ) %>%
  transmute(
    fips = Fips,
    county = str_replace(Area, " County, Iowa", ""),
    population = `2019`
  ) %>%
  filter(fips > 19) %>%
  arrange(population) %>%
  mutate(
    cumulative_poulation = cumsum(population),
    quartile_population = cumulative_poulation/max(cumulative_poulation),
    population_group = cut(
      quartile_population, 
      breaks = c(0, 0.25, 0.51, 0.78, 1),
      labels = c("small", "mid-small", "mid-large", "large")
      # labels = c("FT-AA", "PW-HN", "ST-DA", "PK-LN")
    ),
    population_group = fct_rev(population_group)
  ) %>%
  arrange(desc(cumulative_poulation)) %>%
  print()
```

    ## # A tibble: 99 x 6
    ##     fips county   population cumulative_poula… quartile_popula… population_group
    ##    <dbl> <chr>         <dbl>             <dbl>            <dbl> <fct>           
    ##  1 19153 Polk         490161           3155070            1     large           
    ##  2 19113 Linn         226706           2664909            0.845 large           
    ##  3 19163 Scott        172943           2438203            0.773 mid-large       
    ##  4 19103 Johnson      151140           2265260            0.718 mid-large       
    ##  5 19013 Black H…     131228           2114120            0.670 mid-large       
    ##  6 19193 Woodbury     103107           1982892            0.628 mid-large       
    ##  7 19061 Dubuque       97311           1879785            0.596 mid-large       
    ##  8 19169 Story         97117           1782474            0.565 mid-large       
    ##  9 19049 Dallas        93453           1685357            0.534 mid-large       
    ## 10 19155 Pottawa…      93206           1591904            0.505 mid-small       
    ## # … with 89 more rows

## Cases data

Let’s read in the county data:

``` r
us_counties <- 
  read_csv(path(dir_source, "us-counties.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   date = col_date(format = ""),
    ##   county = col_character(),
    ##   state = col_character(),
    ##   fips = col_character(),
    ##   cases = col_double(),
    ##   deaths = col_double()
    ## )

For now, let’s get the Iowa data, only a few of the columns, and we will
create a column to note the type of aggregation (“none” for the
county-level data).

``` r
iowa_counties_nyt <-
  us_counties %>%
  filter(state == "Iowa") %>%
  select(date, county, cases, deaths) 
```

``` r
date_scrape <- 
  file_info(path(dir_source, "access.html")) %>%
  `[[`("change_time") %>% 
  max() %>%
  as.Date()

iowa_counties_scrape <- 
  read_html(path(dir_source, "access.html")) %>%
  html_node("table") %>%
  html_table(header = TRUE) %>%
  transmute(
    date = date_scrape,
    county = County,
    cases = Confirmed,
    deaths = Deaths
  ) %>%
  print()
```

    ##          date              county cases deaths
    ## 1  2020-04-19                Linn   395     25
    ## 2  2020-04-19                Polk   333     13
    ## 3  2020-04-19             Johnson   285      3
    ## 4  2020-04-19              Louisa   212      2
    ## 5  2020-04-19           Muscatine   207      4
    ## 6  2020-04-19          Black Hawk   192      1
    ## 7  2020-04-19                Tama   187      6
    ## 8  2020-04-19               Scott   159      3
    ## 9  2020-04-19            Marshall   155      0
    ## 10 2020-04-19          Washington   118      5
    ## 11 2020-04-19              Dallas    45      0
    ## 12 2020-04-19             Dubuque    44      1
    ## 13 2020-04-19             Clinton    41      0
    ## 14 2020-04-19            Woodbury    37      0
    ## 15 2020-04-19           Allamakee    36      3
    ## 16 2020-04-19              Jasper    36      0
    ## 17 2020-04-19               Henry    27      1
    ## 18 2020-04-19              Bremer    26      0
    ## 19 2020-04-19               Cedar    26      0
    ## 20 2020-04-19              Benton    23      1
    ## 21 2020-04-19               Story    18      0
    ## 22 2020-04-19       Pottawattamie    18      1
    ## 23 2020-04-19              Warren    18      0
    ## 24 2020-04-19         Cerro Gordo    14      0
    ## 25 2020-04-19               Jones    14      0
    ## 26 2020-04-19            Harrison    13      0
    ## 27 2020-04-19                Iowa    12      0
    ## 28 2020-04-19          Des Moines    11      0
    ## 29 2020-04-19           Poweshiek    10      1
    ## 30 2020-04-19              Shelby     9      0
    ## 31 2020-04-19            Plymouth     8      0
    ## 32 2020-04-19            Buchanan     8      0
    ## 33 2020-04-19             Mahaska     8      0
    ## 34 2020-04-19           Van Buren     8      0
    ## 35 2020-04-19               Sioux     7      0
    ## 36 2020-04-19             Clayton     7      1
    ## 37 2020-04-19 County Info Pending     7     NA
    ## 38 2020-04-19          Winneshiek     6      0
    ## 39 2020-04-19             Fayette     6      0
    ## 40 2020-04-19              Monona     6      0
    ## 41 2020-04-19            Crawford     6      1
    ## 42 2020-04-19               Boone     6      0
    ## 43 2020-04-19             Wapello     6      0
    ## 44 2020-04-19                Lyon     5      0
    ## 45 2020-04-19              Howard     4      0
    ## 46 2020-04-19              Grundy     4      0
    ## 47 2020-04-19             Jackson     4      0
    ## 48 2020-04-19             Guthrie     4      0
    ## 49 2020-04-19              Marion     4      0
    ## 50 2020-04-19           Jefferson     4      0
    ## 51 2020-04-19                Page     4      0
    ## 52 2020-04-19             Osceola     3      0
    ## 53 2020-04-19              Obrien     3      0
    ## 54 2020-04-19                Clay     3      0
    ## 55 2020-04-19             Hancock     3      0
    ## 56 2020-04-19           Chickasaw     3      0
    ## 57 2020-04-19             Webster     3      0
    ## 58 2020-04-19              Keokuk     3      0
    ## 59 2020-04-19             Madison     3      1
    ## 60 2020-04-19                 Lee     3      0
    ## 61 2020-04-19           Winnebago     2      0
    ## 62 2020-04-19            Mitchell     2      0
    ## 63 2020-04-19         Buena Vista     2      0
    ## 64 2020-04-19            Delaware     2      0
    ## 65 2020-04-19            Hamilton     2      0
    ## 66 2020-04-19              Hardin     2      0
    ## 67 2020-04-19               Mills     2      0
    ## 68 2020-04-19              Clarke     2      0
    ## 69 2020-04-19           Appanoose     2      2
    ## 70 2020-04-19           Dickinson     1      0
    ## 71 2020-04-19             Kossuth     1      0
    ## 72 2020-04-19               Worth     1      0
    ## 73 2020-04-19              Wright     1      0
    ## 74 2020-04-19            Franklin     1      0
    ## 75 2020-04-19              Butler     1      0
    ## 76 2020-04-19             Carroll     1      0
    ## 77 2020-04-19              Greene     1      0
    ## 78 2020-04-19             Audubon     1      0
    ## 79 2020-04-19                Cass     1      0
    ## 80 2020-04-19               Adair     1      0
    ## 81 2020-04-19          Montgomery     1      0
    ## 82 2020-04-19               Union     1      0
    ## 83 2020-04-19              Taylor     1      0

``` r
iowa_counties_combined <- iowa_counties_nyt

if (date_scrape > max(iowa_counties_nyt$date)) {
  iowa_counties_combined <- bind_rows(iowa_counties_nyt, iowa_counties_scrape)
}
```

``` r
iowa_counties <- 
  iowa_counties_combined %>%
  filter(county %in% iowa_county_population$county) %>%
  group_by(county) %>%
  arrange(date) %>%
  mutate(
    new_cases = cases - lag(cases, default = 0),
    new_deaths = deaths - lag(deaths, default = 0),
    new_cases_week_avg = (cases - lag(cases, n = 7, default = 0)) / 7,
    new_deaths_week_avg = (deaths - lag(deaths, n = 7, default = 0)) / 7,
    aggregation = "none"
  ) %>%
  ungroup() %>%
  arrange(desc(date), desc(cases)) %>%
  print()
```

    ## # A tibble: 1,956 x 9
    ##    date       county cases deaths new_cases new_deaths new_cases_week_…
    ##    <date>     <chr>  <dbl>  <dbl>     <dbl>      <dbl>            <dbl>
    ##  1 2020-04-19 Linn     395     25        63          0            21.7 
    ##  2 2020-04-19 Polk     333     13        40          0            22.3 
    ##  3 2020-04-19 Johns…   285      3        20          0            11.7 
    ##  4 2020-04-19 Louisa   212      2        35          0            19.4 
    ##  5 2020-04-19 Musca…   207      4        31          1            15.9 
    ##  6 2020-04-19 Black…   192      1        26          0            20.7 
    ##  7 2020-04-19 Tama     187      6        64          0            14.4 
    ##  8 2020-04-19 Scott    159      3         0          0             6.29
    ##  9 2020-04-19 Marsh…   155      0        72          0            17.7 
    ## 10 2020-04-19 Washi…   118      5         5          0             5.29
    ## # … with 1,946 more rows, and 2 more variables: new_deaths_week_avg <dbl>,
    ## #   aggregation <chr>

## Write

``` r
write_csv(iowa_counties, path(dir_target, "iowa-counties.csv"))
```

``` r
write_csv(iowa_county_population, path(dir_target, "iowa-county-population.csv"))
```
