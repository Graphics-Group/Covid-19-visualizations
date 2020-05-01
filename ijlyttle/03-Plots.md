COVID-19 Iowa
================
Ian Lyttle
2020-05-01

``` r
library("fs")
library("glue")
library("tidyverse")
```

    ## ── Attaching packages ────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.0          ✓ purrr   0.3.3     
    ## ✓ tibble  2.1.3          ✓ dplyr   0.8.4     
    ## ✓ tidyr   1.0.0          ✓ stringr 1.4.0     
    ## ✓ readr   1.3.1.9000     ✓ forcats 0.4.0

    ## ── Conflicts ───────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::collapse() masks glue::collapse()
    ## x dplyr::filter()   masks stats::filter()
    ## x dplyr::lag()      masks stats::lag()

``` r
library("USAboundaries") # also install_github("ropensci/USAboundariesData")
library("sf")
```

    ## Linking to GEOS 3.7.2, GDAL 2.4.2, PROJ 5.2.0

``` r
library("colorspace")
```

``` r
dir_source <- path("data", "wrangle")
dir_target <- path("data", "iowa")

dir_create(dir_target)
```

## Prepare data

Let’s read in the county data:

``` r
iowa_counties <- 
  read_csv(path(dir_source, "iowa-counties.csv")) %>%
  print()
```

    ## Parsed with column specification:
    ## cols(
    ##   date = col_date(format = ""),
    ##   county = col_character(),
    ##   cases = col_double(),
    ##   deaths = col_double(),
    ##   new_cases = col_double(),
    ##   new_deaths = col_double(),
    ##   new_cases_week_avg = col_double(),
    ##   new_deaths_week_avg = col_double(),
    ##   aggregation = col_character()
    ## )

    ## # A tibble: 2,984 x 9
    ##    date       county cases deaths new_cases new_deaths new_cases_week_…
    ##    <date>     <chr>  <dbl>  <dbl>     <dbl>      <dbl>            <dbl>
    ##  1 2020-05-01 Black…  1195     13        55          0            95   
    ##  2 2020-05-01 Polk    1176     37       191          2            85.9 
    ##  3 2020-05-01 Woodb…   922      1       180          0           105.  
    ##  4 2020-05-01 Linn     673     45         4          2            23.1 
    ##  5 2020-05-01 Marsh…   500      2        33          0            32.7 
    ##  6 2020-05-01 Johns…   475      6        14          0             8.14
    ##  7 2020-05-01 Dallas   408      0        90          0            44.1 
    ##  8 2020-05-01 Musca…   346     16        10          2            10.9 
    ##  9 2020-05-01 Louisa   274      2         5          0             2   
    ## 10 2020-05-01 Tama     270      7         4          0             4.71
    ## # … with 2,974 more rows, and 2 more variables: new_deaths_week_avg <dbl>,
    ## #   aggregation <chr>

Let’s have a quick look at all the counties.

``` r
ggplot(iowa_counties, aes(date, cases)) +
  geom_line(aes(color = county)) +
  scale_y_log10()
```

    ## Warning: Transformation introduced infinite values in continuous y-axis

![](03-Plots_files/figure-gfm/county-raw-1.png)<!-- -->

This plot is not all that informative, except to tell us that we have
the data we expect.

Let’s get just the data for the most-current data.

``` r
iowa_counties_current <- 
  iowa_counties %>%
  filter(date == max(date)) %>%
  arrange(desc(cases)) %>%
  print()
```

    ## # A tibble: 98 x 9
    ##    date       county cases deaths new_cases new_deaths new_cases_week_…
    ##    <date>     <chr>  <dbl>  <dbl>     <dbl>      <dbl>            <dbl>
    ##  1 2020-05-01 Black…  1195     13        55          0            95   
    ##  2 2020-05-01 Polk    1176     37       191          2            85.9 
    ##  3 2020-05-01 Woodb…   922      1       180          0           105.  
    ##  4 2020-05-01 Linn     673     45         4          2            23.1 
    ##  5 2020-05-01 Marsh…   500      2        33          0            32.7 
    ##  6 2020-05-01 Johns…   475      6        14          0             8.14
    ##  7 2020-05-01 Dallas   408      0        90          0            44.1 
    ##  8 2020-05-01 Musca…   346     16        10          2            10.9 
    ##  9 2020-05-01 Louisa   274      2         5          0             2   
    ## 10 2020-05-01 Tama     270      7         4          0             4.71
    ## # … with 88 more rows, and 2 more variables: new_deaths_week_avg <dbl>,
    ## #   aggregation <chr>

It could be useful to see the current distribution of number of cases
per county:

``` r
date_max <- unique(iowa_counties_current$date)

ggplot(iowa_counties_current) +
  geom_histogram(aes(x = cases), binwidth = 2) +
  theme_bw() + 
  labs(
    x = "number of reported cases in county",
    y = "number of counties",
    title = glue("Iowa: distribution of counties by number of COVID-19 cases - {date_max}"),
    caption = params$nyt_citation  
  ) 
```

![](03-Plots_files/figure-gfm/county-histogram-1.png)<!-- -->

On the right, we see Johnson, Polk, and Linn counties, with the greatest
number of reported cases. On the left, we see a lot of counties with
only a few cases.

Our eyes will let us pay attention to only a finite number of things at
a time, so I propose to set a threshold of 50 cases. We identify those
counties where the number of cases is greater than or equal to the
threshold.

``` r
counties_large <- 
  iowa_counties_current %>%
  filter(cases >= params$threshold_cases) %>%
  arrange(desc(cases)) %>%
  pull(county)

counties_large
```

    ##  [1] "Black Hawk" "Polk"       "Woodbury"   "Linn"       "Marshall"  
    ##  [6] "Johnson"    "Dallas"     "Muscatine"  "Louisa"     "Tama"      
    ## [11] "Scott"      "Jasper"     "Washington" "Dubuque"    "Allamakee" 
    ## [16] "Poweshiek"  "Bremer"

In addition to compiliing the data for the counties with large numbers
of cases, we also create aggregeted datasets that show:

  - the total for the entire state.
  - the total for all the counties that are not considered individually.

<!-- end list -->

``` r
iowa_total <- 
  iowa_counties %>%
  group_by(date) %>%
  summarize(
    county = factor(NA_character_, levels = counties_large),
    cases = sum(cases),
    deaths = sum(deaths),
    new_cases = sum(new_cases),
    new_deaths = sum(new_deaths),
    new_cases_week_avg = sum(new_cases_week_avg),
    new_deaths_week_avg = sum(new_deaths_week_avg),
    aggregation = "total"
  ) %>%
  arrange(desc(date)) %>%
  print()
```

    ## # A tibble: 55 x 9
    ##    date       county cases deaths new_cases new_deaths new_cases_week_…
    ##    <date>     <fct>  <dbl>  <dbl>     <dbl>      <dbl>            <dbl>
    ##  1 2020-05-01 <NA>    7812    170       698          8             480.
    ##  2 2020-04-30 <NA>    7121    162       298         14             455.
    ##  3 2020-04-29 <NA>    6823    148       460         12             437 
    ##  4 2020-04-28 <NA>    6363    136       502          9             389.
    ##  5 2020-04-27 <NA>    5861    127       371          9             384.
    ##  6 2020-04-26 <NA>    5491    118       382          6             368 
    ##  7 2020-04-25 <NA>    5109    112       650          6             371 
    ##  8 2020-04-24 <NA>    4459    106       520         10             304 
    ##  9 2020-04-23 <NA>    3939     96       175          8             257.
    ## 10 2020-04-22 <NA>    3764     88       126          7             253.
    ## # … with 45 more rows, and 2 more variables: new_deaths_week_avg <dbl>,
    ## #   aggregation <chr>

``` r
iowa_remainder <- 
  iowa_counties %>%
  filter(!(county %in% counties_large)) %>%
  group_by(date) %>%
  summarize(
    county = factor(NA_character_, levels = counties_large),
    cases = sum(cases),
    deaths = sum(deaths),
    new_cases = sum(new_cases),
    new_deaths = sum(new_deaths),
    new_cases_week_avg = sum(new_cases_week_avg),
    new_deaths_week_avg = sum(new_deaths_week_avg),
    aggregation = "remainder"
  ) %>%
  arrange(desc(date)) %>%
  print()
```

    ## # A tibble: 54 x 9
    ##    date       county cases deaths new_cases new_deaths new_cases_week_…
    ##    <date>     <fct>  <dbl>  <dbl>     <dbl>      <dbl>            <dbl>
    ##  1 2020-05-01 <NA>     737     13        71          0             33.1
    ##  2 2020-04-30 <NA>     673     13        30          0             28.3
    ##  3 2020-04-29 <NA>     643     13        29          1             25.3
    ##  4 2020-04-28 <NA>     614     12        19          1             21.9
    ##  5 2020-04-27 <NA>     595     11        21          0             21.9
    ##  6 2020-04-26 <NA>     575     11        39          2             21.9
    ##  7 2020-04-25 <NA>     536      9        26          0             19.3
    ##  8 2020-04-24 <NA>     510      9        35          0             18.6
    ##  9 2020-04-23 <NA>     475      9         9          0             15.9
    ## 10 2020-04-22 <NA>     466      9         5          0             16.9
    ## # … with 44 more rows, and 2 more variables: new_deaths_week_avg <dbl>,
    ## #   aggregation <chr>

``` r
iowa_counties_large <- 
  iowa_counties %>%
  filter(county %in% counties_large) %>%
  mutate(
    county = factor(county, levels = counties_large)
  )
```

Let’s combine our
datasets:

``` r
data_combined <- bind_rows(iowa_counties_large, iowa_total, iowa_remainder)
```

If we wanted do one of those [FT-style
charts](https://www.ft.com/coronavirus-latest), we need to choose an
index day for each county and aggregation. We choose an arbitrary
threshold of 20 reported cases.

``` r
data_combined_index <- 
  data_combined %>%
  group_by(aggregation, county) %>%
  arrange(date) %>%
  mutate(
    exceeds_threshold_cases = cases >= params$threshold_index_cases,
    index_cases = cumsum(exceeds_threshold_cases) - 1,
    exceeds_threshold_new_cases = new_cases >= params$threshold_new_cases,
    index_new_cases = cumsum(cumany(exceeds_threshold_new_cases)) - 1
  ) 
```

    ## Warning: Factor `county` contains implicit NA, consider using
    ## `forcats::fct_explicit_na`
    
    ## Warning: Factor `county` contains implicit NA, consider using
    ## `forcats::fct_explicit_na`
    
    ## Warning: Factor `county` contains implicit NA, consider using
    ## `forcats::fct_explicit_na`

We combine all these datasets into a plot:

``` r
plot_cases <- 
  ggplot(data_combined, aes(date, cases)) +
  geom_line(aes(color = county, linetype = aggregation)) +
  geom_point(
    data = . %>% filter(date == max(date)),
    aes(color = county),
    show.legend = FALSE
  ) +
  scale_y_log10(
    sec.axis = dup_axis(name = NULL)
  ) + 
  scale_linetype(
    name = "grouping", 
    breaks = c("total", "remainder", "none"),
    labels = c("state total", "all other counties", "county")
  ) +
  scale_color_discrete(breaks = counties_large) +
  coord_cartesian(
    ylim = c(10, NA)
  ) +
  theme_bw() + 
  labs(
    x = NULL,
    y = "number of reported cases",
    title = "Iowa: COVID-19 cases by reporting date",
    subtitle = "Includes counties with most reported cases, also aggregations",
    caption = params$nyt_citation  
  )
```

``` r
plot_new_cases_week_avg <- 
  ggplot(data_combined, aes(date, new_cases_week_avg)) +
  geom_line(aes(color = county, linetype = aggregation)) +
  geom_point(
    data = . %>% filter(date == max(date)),
    aes(color = county),
    show.legend = FALSE
  ) +
  scale_y_log10(sec.axis = dup_axis(name = NULL)) + 
  scale_linetype(
    name = "grouping", 
    breaks = c("total", "remainder", "none"),
    labels = c("state total", "all other counties", "county")
  ) +
  scale_color_discrete(breaks = counties_large) +
  theme_bw() + 
  labs(
    x = NULL,
    y = "number of new reported cases",
    title = "Iowa: COVID-19 daily new cases (7-day rolling average) by reporting date",
    subtitle = "Includes counties with most reported cases, also aggregations",
    caption = params$nyt_citation  
  )
```

``` r
# function that takes a set of limits, returns the breaks
# - we want breaks every 7 days.
f_break <- function(limits) {

  low <- floor(limits[1] / 7) * 7
  hi <- ceiling(limits[2] / 7) * 7
  
  seq(low, hi, by = 7)
}

# function that returns data for "doubling" lines
#' @param data          `data.frame`
#' @param var_index     `character`, name of variable in `data` containing day-index
#' @param var_number    `character`, name of variable in `data` contiaining number-data
#' @param days_doubling `numeric`, vector with number-of-days to include
#'
#' @return `data.frame` with variables `days_doubling`, `index`, `number`, `label`
#'
f_doubling <- function(data, var_index, var_number, threshold_number,
                       days_doubling = c(2, 4, 7, 14)) {

  index_range <- range(data[[var_index]])
  number_range <- range(data[[var_number]])
  
  max_number <- number_range[2]
  max_index <- index_range[2]

  # for each `days_doubling`, determine the index when days are maximum 
  by_number <- 
    crossing(days_doubling, number = max_number) %>%
    mutate(
      index = days_doubling * log(number/threshold_number, 2)
    )
    
  by_index <- 
    crossing(days_doubling, index = index_range) %>%
    mutate(
      number = threshold_number * 2^(index/days_doubling)
    )

  combined <- 
    dplyr::bind_rows(by_index, by_number) %>%
    dplyr::select(days_doubling, index, number) %>%
    dplyr::mutate(
      label = ifelse(index > 0, glue::glue("{days_doubling}-day doubling"), "")
    ) %>%
    dplyr::filter(index <= max_index, number <= max_number) %>%
    dplyr::arrange(days_doubling, index) 
  
  combined
}

plot_cases_ft <- 
  ggplot(
    data_combined_index %>% filter(index_cases >= 0), 
    aes(index_cases, cases)
  ) +
  geom_line(
    data = 
      . %>% 
      f_doubling("index_cases", "cases", params$threshold_index_cases), 
    aes(x = index, y = number, group = factor(days_doubling)),
    color = "grey80",
    linetype = "twodash"
  ) +
  geom_text(
    data = 
      . %>% 
      f_doubling("index_cases", "cases", params$threshold_index_cases) %>%
      filter(str_length(label) > 0), 
    aes(x = index, y = number, group = factor(days_doubling), label = label),
    hjust = "right", 
    nudge_y = 0.05,
    color = "grey80"
  ) +
  geom_line(
    aes(color = county, linetype = aggregation), 
    alpha = 0.25
  ) +  
  geom_line(
    data = . %>% filter(date >= max(date) - 14),
    aes(color = county, linetype = aggregation), 
    alpha = 0.5
  ) +  
  geom_line(
    data = . %>% filter(date >= max(date) - 7),
    aes(color = county, linetype = aggregation), 
    alpha = 1
  ) +
  geom_point(
    data = . %>% filter(date == max(date)),
    aes(color = county),
    show.legend = FALSE
  ) +
  geom_text(
    data = . %>% filter(date == max(date), aggregation == "none"),
    aes(x = index_cases, label = county, color = county),
    hjust = "left",
    nudge_x = .5,
    size = 3,
    show.legend = FALSE
  ) +
  geom_text(
    data = . %>% 
      ungroup() %>%
      filter(date == max(date), aggregation != "none") %>%
      mutate(
        label = fct_recode(
          aggregation, 
          `state total` = "total", 
          `all other counties` = "remainder"
        )
      ),
    aes(x = index_cases, label = label),
    color = "grey10",
    hjust = "left",
    nudge_x = 0.5,
    size = 3,
    show.legend = FALSE
  ) +
  scale_x_continuous(breaks = f_break, expand = expansion(add = c(0, 4))) +
  scale_y_log10(
    sec.axis = dup_axis(name = NULL)
  ) + 
  scale_linetype(
    name = "grouping", 
    breaks = c("total", "remainder", "none"),
    labels = c("state total", "all other counties", "county")
  ) +  
  scale_color_discrete(breaks = counties_large) +
  guides(color = FALSE, linetype = FALSE) +
  theme_bw() +
  labs(
    x = glue("days since {params$threshold_index_cases} cases"),
    y = "number of reported cases",
    title = glue("Iowa: COVID-19 cumulative-case trajectories as of {max(data_combined_index$date)}"),
    subtitle = "Includes counties with most reported cases, also aggregations",
    caption = params$nyt_citation
  )
```

``` r
plot_new_cases_week_avg_ft <- 
  ggplot(
    data_combined_index %>% filter(index_new_cases >= 0),
    aes(index_new_cases, new_cases_week_avg)
  ) +
  geom_line(
    aes(color = county, linetype = aggregation), 
    alpha = 0.25
  ) +  
  geom_line(
    data = . %>% filter(date >= max(date) - 14),
    aes(color = county, linetype = aggregation), 
    alpha = 0.5
  ) +  
  geom_line(
    data = . %>% filter(date >= max(date) - 7),
    aes(color = county, linetype = aggregation), 
    alpha = 1
  ) +
  geom_point(
    data = . %>% filter(date == max(date)),
    aes(color = county),
    show.legend = FALSE
  ) +
  geom_text(
    data = . %>% filter(date == max(date), aggregation == "none"),
    aes(x = index_new_cases, label = county, color = county),
    hjust = "left",
    nudge_x = .5,
    size = 3,
    show.legend = FALSE
  ) +
  geom_text(
    data = . %>% 
      ungroup() %>%
      filter(date == max(date), aggregation != "none") %>%
      mutate(
        label = fct_recode(
          aggregation, 
          NULL = "county",
          `state total` = "total", 
          `all other counties` = "remainder"
        )
      ),
    aes(x = index_new_cases, label = label),
    color = "grey10",
    hjust = "left",
    nudge_x = 0.5,
    size = 3,
    show.legend = FALSE
  ) +
  scale_x_continuous(breaks = f_break, expand = expansion(add = c(0, 4))) +
  scale_y_log10(
    sec.axis = dup_axis(name = NULL)
  ) + 
  scale_linetype(
    name = "grouping", 
    breaks = c("total", "remainder", "none"),
    labels = c("state total", "all other counties", "county")
  ) +
  scale_color_discrete(breaks = counties_large) +
  guides(color = FALSE, linetype = FALSE) +
  theme_bw() +
  labs(
    x = glue("days since {params$threshold_index_new_cases} new cases"),
    y = "number of new reported cases",
    title = glue("Iowa: COVID-19 new-case (7-day rolling average) trajectories as of {max(data_combined_index$date)}"),
    subtitle = "Includes counties with most reported cases, also aggregations",
    caption = params$nyt_citation
  )
```

### County groups

Let’s look at the poulation of counties. I’d like to sort Iowa’s
counties into four groups, ordered by county-population, such that each
*group* of counties contains approximately the same population.

``` r
iowa_county_population <- 
  read_csv(path(dir_source, "iowa-county-population.csv")) %>%
  print()
```

    ## Parsed with column specification:
    ## cols(
    ##   fips = col_double(),
    ##   county = col_character(),
    ##   population = col_double(),
    ##   cumulative_poulation = col_double(),
    ##   quartile_population = col_double(),
    ##   population_group = col_character()
    ## )

    ## # A tibble: 99 x 6
    ##     fips county   population cumulative_poula… quartile_popula… population_group
    ##    <dbl> <chr>         <dbl>             <dbl>            <dbl> <chr>           
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

You can see that the “first” group contains two counties: Polk and Linn;
the “second” group contains seven counties, from Scott to Dallas. This
second group of counties is home to the Regents’ Universities, Iowa’s
part of the Quad cities, Dubuque, Sioux City, and suburban Des Moines.

The “third” and “fourth” groups contain 24 and 66 counties,
respectively. Here’s a map of Iowa’s counties, showing these groups:

``` r
iowa_map <- us_counties(resolution = "low", state = "Iowa")
```

I am interested to compare how the trajectory of recorded cases behaves,
or might behave differently, among these groups of counties.

``` r
group_colors <- 
  colorspace::sequential_hcl(palette = "Blues 3", n = 6)[1:4] # drop lightest

iowa_map_with_group <- 
  iowa_map %>%
  left_join(
    iowa_county_population %>% select(county, population_group),
    by = c(name = "county")  
  )
```

``` r
plot_iowa_map_group <-
  ggplot() +
  geom_sf(
    data = iowa_map_with_group, 
    aes(fill = population_group),
    color = "white"
  ) + 
  scale_fill_manual(values = group_colors) + 
  labs(
    title = "Iowa counties by population-group",
    subtitle = "Each population-group has about 1/4 of state's population (2019 estimate)",
    caption = params$iowa_citation,
    fill = "group"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank()
  )
```

Let’s look at proportions of counties, population, and (most-recent)
reported cases and deaths.

``` r
iowa_counts <-
  iowa_county_population %>% select(county, population, population_group) %>%
  left_join(
    iowa_counties_current,
    by = "county"
  ) %>%
  mutate(
    cases = ifelse(is.na(cases), 0, cases),
    deaths = ifelse(is.na(deaths), 0, deaths)
  ) %>%
  group_by(population_group) %>%
  summarize(
    counties = n(),
    population = sum(population),
    cases = sum(cases),
    deaths = sum(deaths)
  ) %>%
  print()
```

    ## # A tibble: 4 x 5
    ##   population_group counties population cases deaths
    ##   <chr>               <int>      <dbl> <dbl>  <dbl>
    ## 1 large                   2     716867  1849     82
    ## 2 mid-large               7     846299  3370     32
    ## 3 mid-small              24     812518  1555     37
    ## 4 small                  66     779386  1038     19

``` r
vars_count <- c("counties", "population", "cases", "deaths")

iowa_counts_tall <-
  iowa_counts %>%
  pivot_longer(
    all_of(vars_count), 
    names_to = "category", 
    values_to = "count"
  ) %>%
  mutate(
    category = factor(category, rev(vars_count))
  ) %>%
  print()
```

    ## # A tibble: 16 x 3
    ##    population_group category    count
    ##    <chr>            <fct>       <dbl>
    ##  1 large            counties        2
    ##  2 large            population 716867
    ##  3 large            cases        1849
    ##  4 large            deaths         82
    ##  5 mid-large        counties        7
    ##  6 mid-large        population 846299
    ##  7 mid-large        cases        3370
    ##  8 mid-large        deaths         32
    ##  9 mid-small        counties       24
    ## 10 mid-small        population 812518
    ## 11 mid-small        cases        1555
    ## 12 mid-small        deaths         37
    ## 13 small            counties       66
    ## 14 small            population 779386
    ## 15 small            cases        1038
    ## 16 small            deaths         19

``` r
plot_iowa_proportions <-
  ggplot(
    iowa_counts_tall, 
    aes(x = category, y = count)
  ) + 
  geom_col(aes(fill = population_group), position = position_fill(reverse = TRUE)) + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = group_colors) + 
  coord_flip() +
  labs(
    x = "",
    y = "proportion",
    fill = "group",
    title = glue("Iowa: COVID-19 proportions - as of {date_max}"),
    caption = params$nyt_citation
  ) +
  theme_bw()
```

``` r
iowa_counties_group <-
  iowa_counties %>%
  left_join(
    iowa_county_population %>% select(county, population_group),
    by = "county"
  )

iowa_counties_group_agg <-
  iowa_counties_group %>%
  group_by(date, population_group) %>%
  summarize(
    cases = sum(cases),
    deaths = sum(deaths),
    new_cases = sum(new_cases),
    new_deaths = sum(new_deaths),
    new_cases_week_avg = sum(new_cases_week_avg),
    new_deaths_week_avg = sum(new_deaths_week_avg)
  ) %>%
  ungroup()
```

``` r
plot_cases_group <- 
  ggplot(iowa_counties_group_agg, aes(date, cases)) +
  geom_line(
    aes(color = population_group)
  ) +  
  geom_point(
    data = . %>% filter(date == max(date)),
    aes(color = population_group),
    show.legend = FALSE
  ) +
  scale_color_manual(values = group_colors) +
  scale_y_log10(sec.axis = dup_axis(name = NULL)) + 
  theme_bw() + 
  labs(
    x = NULL,
    y = "number of reported cases",
    color = "group",
    title = "Iowa: COVID-19 cases by reporting date",
    caption = params$nyt_citation  
  )
```

``` r
plot_new_cases_group <- 
  ggplot(iowa_counties_group_agg, aes(date, new_cases_week_avg)) +
  geom_line(
    aes(color = population_group)
  ) +  
  geom_point(
    data = . %>% filter(date == max(date)),
    aes(color = population_group),
    show.legend = FALSE
  ) +
  scale_color_manual(values = group_colors) +
  scale_y_log10(sec.axis = dup_axis(name = NULL)) + 
  theme_bw() + 
  labs(
    x = NULL,
    y = "number of new reported cases",
    color = "group",
    title = "Iowa: COVID-19 daily new cases (7-day rolling average) by reporting date",
    caption = params$nyt_citation  
  )
```

``` r
iowa_counties_group_agg_ft <- 
  iowa_counties_group_agg %>%
  group_by(population_group) %>%
  arrange(date) %>%
  mutate(
    exceeds_threshold_cases = cases > params$threshold_index_cases,
    index_cases = cumsum(exceeds_threshold_cases) - 1,
    exceeds_threshold_new_cases = new_cases >= params$threshold_index_new_cases,
    index_new_cases = cumsum(cumany(exceeds_threshold_new_cases)) - 1    
  ) 

plot_cases_group_ft <-
  ggplot(
    iowa_counties_group_agg_ft %>% filter(exceeds_threshold_cases), 
    aes(index_cases, cases)) +
  geom_line(
    data = 
      . %>% 
      f_doubling("index_cases", "cases", params$threshold_index_cases), 
    aes(x = index, y = number, group = factor(days_doubling)),
    color = "grey80",
    linetype = "twodash"
  ) +
  geom_text(
    data = 
      . %>% 
      f_doubling("index_cases", "cases", params$threshold_index_cases) %>%
      filter(str_length(label) > 0), 
    aes(x = index, y = number, label = label),
    hjust = "right", 
    nudge_y = 0.05,
    color = "grey80"
  ) +
  geom_line(aes(color = population_group)) +
  geom_point(
    data = . %>% filter(date == max(date)),
    aes(color = population_group),
    show.legend = FALSE
  ) +
  scale_x_continuous(breaks = f_break) +
  scale_y_log10(
    sec.axis = dup_axis(name = NULL)
  ) + 
  scale_color_manual(values = group_colors) +
  theme_bw() +
  labs(
    x = glue("days since {params$threshold_index_cases} cases"),
    y = "number of reported cases",
    color = "group",
    title = "Iowa: COVID-19 cumulative-case trajectories",
    caption = params$nyt_citation
  )
```

``` r
plot_new_cases_group_ft <-
  ggplot(
    iowa_counties_group_agg_ft %>% filter(index_new_cases >= 0), 
    aes(index_new_cases, new_cases_week_avg)) +
  geom_line(aes(color = population_group)) +
  geom_point(
    data = . %>% filter(date == max(date)),
    aes(color = population_group),
    show.legend = FALSE
  ) +
  scale_x_continuous(breaks = f_break) +
  scale_y_log10(
    sec.axis = dup_axis(name = NULL)
  ) + 
  scale_color_manual(values = group_colors) +
  theme_bw() +
  labs(
    x = glue("days since {params$threshold_index_new_cases} new cases"),
    y = "number of new reported cases",
    color = "group",
    title = "Iowa: COVID-19 new-case trajectories (7-day rolling average)",
    caption = params$nyt_citation
  )
```

``` r
plot_iowa_deaths_ft <-
  ggplot(
    data_combined_index %>% filter(aggregation == "total", deaths > 0), 
    aes(x = date, y = deaths)
  ) +
  geom_line() +
  geom_point(
    data = . %>% filter(date == max(date))
  ) +
  scale_y_log10(
    sec.axis = dup_axis(name = NULL)
  ) + 
  theme_bw() +
  labs(
    x = NULL,
    y = "total number of fatalities",
    title = "Iowa: COVID-19 fatality trajectory",
    caption = params$nyt_citation
  )  
```

``` r
counties_closed_may_01 <- 
  c(
    "Allamakee", 
    "Benton", 
    "Black Hawk", 
    "Bremer", 
    "Dallas", 
    "Des Moines", 
    "Dubuque", 
    "Fayette", 
    "Henry", 
    "Iowa", 
    "Jasper", 
    "Johnson", 
    "Linn", 
    "Louisa", 
    "Marshall", 
    "Muscatine", 
    "Polk", 
    "Poweshiek", 
    "Scott", 
    "Tama", 
    "Washington", 
    "Woodbury"
  )

iowa_repoen <- 
  iowa_counties %>%
  mutate(
    open_may_01 = !(county %in% counties_closed_may_01)
  ) %>%
  group_by(date, open_may_01) %>%
  summarize(
    cases = sum(cases),
    deaths = sum(deaths),
    new_cases = sum(new_cases),
    new_deaths = sum(new_deaths),
    new_cases_week_avg = sum(new_cases_week_avg),
    new_deaths_week_avg = sum(new_deaths_week_avg)    
  ) %>%
  ungroup()
```

``` r
plot_repoen_cases <- 
  ggplot(iowa_repoen, aes(date, cases)) +
  geom_line(aes(color = open_may_01)) +
  geom_point(
    data = . %>% filter(date == max(date)),
    aes(color = open_may_01)
  ) +
  geom_text(
    data = 
      . %>% 
      filter(date == max(date)) %>%
      mutate(label = ifelse(open_may_01, "partial reopening", "remain closed")),
    aes(label = label, color = open_may_01),
    hjust = "left",
    nudge_x = .5,
    size = 3,
    show.legend = FALSE
  ) +
  scale_x_date(expand = expansion(add = c(0, 7))) +
  scale_y_log10(
    sec.axis = dup_axis(name = NULL)
  ) + 
  guides(color = FALSE) +
  coord_cartesian(
    ylim = c(1, NA)
  ) +
  theme_bw() + 
  labs(
    x = NULL,
    y = "number of reported cases",
    color = 'county "re-opening" May 1',
    title = "Iowa: COVID-19 cumulative-case trajectories",
    caption = params$nyt_citation  
  )
```

``` r
plot_repoen_new_cases <- 
  ggplot(iowa_repoen, aes(date, new_cases_week_avg)) +
  geom_line(aes(color = open_may_01)) +
  geom_point(
    data = . %>% filter(date == max(date)),
    aes(color = open_may_01)
  ) +
  geom_text(
    data = 
      . %>% 
      filter(date == max(date)) %>%
      mutate(label = ifelse(open_may_01, "partial reopening", "remain closed")),
    aes(label = label, color = open_may_01),
    hjust = "left",
    nudge_x = .5,
    size = 3,
    show.legend = FALSE
  ) +
  scale_x_date(expand = expansion(add = c(0, 7))) +
  scale_y_log10(
    sec.axis = dup_axis(name = NULL)
  ) + 
  guides(color = FALSE) +
  coord_cartesian(
    ylim = c(1, NA)
  ) +
  theme_bw() + 
  labs(
    x = NULL,
    y = "number of new reported cases",
    color = 'county "re-opening" May 1',
    title = "Iowa: COVID-19 daily new cases (7-day rolling average) by reporting date",
    caption = params$nyt_citation  
  )
```

## Discussion

``` r
plot_cases
```

![](03-Plots_files/figure-gfm/cases-county-1.png)<!-- -->

For another view, here’s an [FT-style
chart](https://www.ft.com/coronavirus-latest):

``` r
plot_cases_ft
```

![](03-Plots_files/figure-gfm/cases-county-ft-1.png)<!-- -->

``` r
plot_new_cases_week_avg
```

    ## Warning: Transformation introduced infinite values in continuous y-axis

![](03-Plots_files/figure-gfm/new-cases-county-1.png)<!-- -->

``` r
plot_new_cases_week_avg_ft
```

![](03-Plots_files/figure-gfm/new-cases-county-ft-1.png)<!-- -->

``` r
plot_iowa_map_group
```

![](03-Plots_files/figure-gfm/iowa-map-1.png)<!-- -->

``` r
plot_iowa_proportions
```

![](03-Plots_files/figure-gfm/iowa-proportions-1.png)<!-- -->

``` r
plot_cases_group
```

![](03-Plots_files/figure-gfm/cases-group-1.png)<!-- -->

``` r
plot_new_cases_group
```

![](03-Plots_files/figure-gfm/new-cases-group-1.png)<!-- -->

``` r
plot_cases_group_ft
```

![](03-Plots_files/figure-gfm/cases-group-ft-1.png)<!-- -->

``` r
plot_new_cases_group_ft
```

![](03-Plots_files/figure-gfm/new-cases-group-ft-1.png)<!-- -->

``` r
plot_iowa_deaths_ft
```

![](03-Plots_files/figure-gfm/iowa-deaths-ft-1.png)<!-- -->

``` r
plot_repoen_cases
```

![](03-Plots_files/figure-gfm/iowa-reopen-cases-1.png)<!-- -->

``` r
plot_repoen_new_cases
```

![](03-Plots_files/figure-gfm/iowa-reopen-new-cases-1.png)<!-- -->
