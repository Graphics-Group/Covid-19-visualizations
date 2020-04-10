COVID-19 Iowa
================
Ian Lyttle
2020-04-10

``` r
library("fs")
library("glue")
library("tidyverse")
```

    ## ── Attaching packages ──────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.0          ✓ purrr   0.3.3     
    ## ✓ tibble  2.1.3          ✓ dplyr   0.8.4     
    ## ✓ tidyr   1.0.0          ✓ stringr 1.4.0     
    ## ✓ readr   1.3.1.9000     ✓ forcats 0.4.0

    ## ── Conflicts ─────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::collapse() masks glue::collapse()
    ## x dplyr::filter()   masks stats::filter()
    ## x dplyr::lag()      masks stats::lag()

``` r
library("readxl")
library("USAboundaries") # also install_github("ropensci/USAboundariesData")
library("sf")
```

    ## Linking to GEOS 3.7.2, GDAL 2.4.2, PROJ 5.2.0

``` r
library("colorspace")
```

``` r
dir_source <- path("data", "download")
dir_target <- path("data", "iowa")

dir_create(dir_target)
```

## Prepare data

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
iowa_counties <-
  us_counties %>%
  filter(state == "Iowa") %>%
  select(date, county, cases, deaths) %>%
  mutate(aggregation = "none") %>%
  arrange(desc(date), desc(cases)) %>%
  print()
```

    ## # A tibble: 1,140 x 5
    ##    date       county     cases deaths aggregation
    ##    <date>     <chr>      <dbl>  <dbl> <chr>      
    ##  1 2020-04-09 Linn         215      9 none       
    ##  2 2020-04-09 Johnson      171      1 none       
    ##  3 2020-04-09 Polk         140      5 none       
    ##  4 2020-04-09 Scott         88      1 none       
    ##  5 2020-04-09 Muscatine     70      1 none       
    ##  6 2020-04-09 Washington    65      2 none       
    ##  7 2020-04-09 Tama          63      2 none       
    ##  8 2020-04-09 Louisa        41      0 none       
    ##  9 2020-04-09 Dallas        36      0 none       
    ## 10 2020-04-09 Dubuque       31      1 none       
    ## # … with 1,130 more rows

Let’s have a quick look at all the counties.

``` r
ggplot(iowa_counties, aes(date, cases)) +
  geom_line(aes(color = county)) +
  scale_y_log10()
```

![](01-Iowa_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

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

    ## # A tibble: 79 x 5
    ##    date       county     cases deaths aggregation
    ##    <date>     <chr>      <dbl>  <dbl> <chr>      
    ##  1 2020-04-09 Linn         215      9 none       
    ##  2 2020-04-09 Johnson      171      1 none       
    ##  3 2020-04-09 Polk         140      5 none       
    ##  4 2020-04-09 Scott         88      1 none       
    ##  5 2020-04-09 Muscatine     70      1 none       
    ##  6 2020-04-09 Washington    65      2 none       
    ##  7 2020-04-09 Tama          63      2 none       
    ##  8 2020-04-09 Louisa        41      0 none       
    ##  9 2020-04-09 Dallas        36      0 none       
    ## 10 2020-04-09 Dubuque       31      1 none       
    ## # … with 69 more rows

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
    caption = "Data from NYT, based on reports from state and local health agencies"  
  ) 
```

![](01-Iowa_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

On the right, we see Johnson, Polk, and Linn counties, with the greatest
number of reported cases. On the left, we see a lot of counties with
only a few cases.

``` r
# arbitrary threshold; will have to adjust as things change
threshold <- 25
```

Our eyes will let us pay attention to only a finite number of things at
a time, so I propose to set a threshold of 25 cases. We identify those
counties where the number of cases is greater than or equal to the
threshold.

``` r
counties_large <- 
  iowa_counties_current %>%
  filter(cases >= 20) %>%
  arrange(desc(cases)) %>%
  pull(county)

counties_large
```

    ##  [1] "Linn"       "Johnson"    "Polk"       "Scott"      "Muscatine" 
    ##  [6] "Washington" "Tama"       "Louisa"     "Dallas"     "Dubuque"   
    ## [11] "Black Hawk" "Clinton"

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
    aggregation = "total"
  ) %>%
  arrange(desc(date)) %>%
  print()
```

    ## # A tibble: 33 x 5
    ##    date       county cases deaths aggregation
    ##    <date>     <fct>  <dbl>  <dbl> <chr>      
    ##  1 2020-04-09 <NA>    1270     27 total      
    ##  2 2020-04-08 <NA>    1145     27 total      
    ##  3 2020-04-07 <NA>    1048     26 total      
    ##  4 2020-04-06 <NA>     946     25 total      
    ##  5 2020-04-05 <NA>     868     22 total      
    ##  6 2020-04-04 <NA>     786     11 total      
    ##  7 2020-04-03 <NA>     699     11 total      
    ##  8 2020-04-02 <NA>     614     11 total      
    ##  9 2020-04-01 <NA>     548      9 total      
    ## 10 2020-03-31 <NA>     498      7 total      
    ## # … with 23 more rows

``` r
iowa_remainder <- 
  iowa_counties %>%
  filter(!(county %in% counties_large)) %>%
  group_by(date) %>%
  summarize(
    county = factor(NA_character_, levels = counties_large),
    cases = sum(cases),
    deaths = sum(deaths),
    aggregation = "remainder"
  ) %>%
  arrange(desc(date)) %>%
  print()
```

    ## # A tibble: 32 x 5
    ##    date       county cases deaths aggregation
    ##    <date>     <fct>  <dbl>  <dbl> <chr>      
    ##  1 2020-04-09 <NA>     308      5 remainder  
    ##  2 2020-04-08 <NA>     290      5 remainder  
    ##  3 2020-04-07 <NA>     270      5 remainder  
    ##  4 2020-04-06 <NA>     251      4 remainder  
    ##  5 2020-04-05 <NA>     233      4 remainder  
    ##  6 2020-04-04 <NA>     218      2 remainder  
    ##  7 2020-04-03 <NA>     200      2 remainder  
    ##  8 2020-04-02 <NA>     175      2 remainder  
    ##  9 2020-04-01 <NA>     148      2 remainder  
    ## 10 2020-03-31 <NA>     132      2 remainder  
    ## # … with 22 more rows

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

``` r
threshold_index_cases <- 10
```

If we wanted do one of those [FT-style
charts](https://www.ft.com/coronavirus-latest), we need to choose an
index day for each county and aggregation. We choose an arbitrary
threshold of 10 reported cases.

``` r
data_combined_index <- 
  data_combined %>%
  group_by(aggregation, county) %>%
  arrange(date) %>%
  mutate(
    exceeds_threshold_cases = cases > threshold_index_cases,
    index_cases = cumsum(exceeds_threshold_cases) - 1
  ) %>%
  filter(exceeds_threshold_cases)
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
    y = "number of reported cases",
    title = "Iowa: COVID-19 cases by reporting date",
    subtitle = "Includes counties with most reported cases, also aggregations",
    caption = "Data from NYT, based on reports from state and local health agencies"  
  )
```

``` r
f_break <- function(limits) {

  low <- floor(limits[1] / 7) * 7
  hi <- ceiling(limits[2] / 7) * 7
  
  seq(low, hi, by = 7)
}


f_doubling <- function(data) {

  days_doubling <- c(2, 4, 7, 14)
  index_cases <- c(0, max(data$index_cases))
}

index_max <- max(data_combined_index$index_cases)
days_doubling <- c(2, 4, 7, 14)
index_cases <- c(0, index_max)

doubling <- 
  crossing(days_doubling, index_cases) %>%
  mutate(
    cases = 10 * 2^(index_cases/days_doubling),
    label = glue("{days_doubling} days")
  )

plot_cases_ft <- 
  ggplot(data_combined_index, aes(index_cases, cases)) +
  geom_line(
    data = doubling, 
    aes(group = factor(days_doubling)),
    color = "grey80",
    linetype = "twodash"
  ) +
  geom_text(
    data = doubling %>% filter(index_cases > 0), 
    aes(group = factor(days_doubling), label = label),
    hjust = 0, 
    nudge_x = -1.5,
    color = "grey80"
  ) +
  geom_line(aes(color = county, linetype = aggregation)) +
  geom_point(
    data = . %>% filter(date == max(date)),
    aes(color = county),
    show.legend = FALSE
  ) +
  scale_x_continuous(breaks = f_break) +
  scale_y_log10(
    sec.axis = dup_axis(name = NULL)
  ) + 
  scale_linetype(
    name = "grouping", 
    breaks = c("total", "remainder", "none"),
    labels = c("state total", "all other counties", "county")
  ) +
  scale_color_discrete(breaks = counties_large) +
  coord_cartesian(ylim = c(NA, 1000)) +
  theme_bw() +
  labs(
    x = glue("days since {threshold_index_cases} cases"),
    y = "number of reported cases",
    title = "Iowa: COVID-19 cumulative-case trajectories",
    subtitle = "Includes counties with most reported cases, also aggregations",
    caption = "Data from NYT, based on reports from state and local health agencies"  
  )
```

I decided to plot this using “real” time, rather than by “days from
x-number of cases” time, to make it easier to see statewide trends and
line them up with each other.

## Discussion

![](01-Iowa_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

To be *very* clear, I am not an epidemiologist, so it is entirely
possible that I am missing some important nuance here.

Assuming that testing standards do not vary (too much) across the state,
and remain relatively constant over time, here’s what I’m seeing from
this plot:

  - Considering the state total, cases are doubling every seven-or-so
    days.

  - The rate-of-growth for “all other counties” is tracking the
    rate-of-growth for the state, as a whole. This appears to be the
    case for the last two weeks.

Opinion/conjecture:

  - What we are seeing reported is a sense of what was *actually*
    happening a week or so previous.

  - By saying that we, as a state, don’t need to be doing anything
    differently, we are also saying that we are prepared to accept the
    doubling of cases every week days for the forseeable future.

For another view, here’s an [FT-style
chart](https://www.ft.com/coronavirus-latest):

![](01-Iowa_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

Let’s look at the poulation of counties. I’d like to sort Iowa’s
counties into four groups, ordered by county-population, such that each
*group* of counties contains approximately the same population.

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

You can see that the “first” group contains two counties: Polk and Linn;
the “second” group contains seven counties, from Scott to Dallas. This
second group of counties is home to the Regents’ Universities, Iowa’s
part of the Quad cities, Dubuque, Sioux City, and suburban Des Moines.

The “thrid” and “fourth” groups contain 24 and 66 counties,
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

ggplot() +
  geom_sf(
    data = iowa_map_with_group, 
    aes(fill = population_group),
    color = "white"
  ) + 
  scale_fill_manual(values = group_colors) + 
  labs(
    title = "Iowa Counties by population-group",
    subtitle = "Each population-group has about 1/4 of state's population (2019 estimate)",
    caption = "Source: Iowa Community Indicators Program, from US Census Bureau",
    fill = "group"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank()
  )
```

![](01-Iowa_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

Let’s look at proportions of counties, population, and (most-recent)
reported cases and deaths.

``` r
iowa_proportions <-
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
    ##   <fct>               <int>      <dbl> <dbl>  <dbl>
    ## 1 large                   2     716867   355     14
    ## 2 mid-large               7     846299   366      3
    ## 3 mid-small              24     812518   301      5
    ## 4 small                  66     779386   248      5

``` r
  ggplot(
    iowa_proportions, 
    aes(x = "1", y = population)
  ) + 
  geom_col(aes(fill = population_group), position = position_fill(reverse = TRUE)) + 
  scale_fill_manual(values = group_colors) + 
  coord_flip() +
  labs(
    fill = "group"
  ) +
  theme_bw()
```

![](01-Iowa_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

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
    deaths = sum(deaths)
  ) %>%
  ungroup()
```

``` r
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
    caption = "Data from NYT, based on reports from state and local health agencies"  
  )
```

![](01-Iowa_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

``` r
iowa_counties_group_agg_ft <- 
  iowa_counties_group_agg %>%
  group_by(population_group) %>%
  arrange(date) %>%
  mutate(
    exceeds_threshold_cases = cases > threshold_index_cases,
    index_cases = cumsum(exceeds_threshold_cases) - 1
  ) %>%
  filter(exceeds_threshold_cases)

ggplot(iowa_counties_group_agg_ft, aes(index_cases, cases)) +
  geom_line(
    data = doubling, 
    aes(group = factor(days_doubling)),
    color = "grey80",
    linetype = "twodash"
  ) +
  geom_text(
    data = doubling %>% filter(index_cases > 0), 
    aes(group = factor(days_doubling), label = label),
    hjust = 0, 
    nudge_x = -1.5,
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
  coord_cartesian(ylim = c(NA, 1000)) +
  theme_bw() +
  labs(
    x = glue("days since {threshold_index_cases} cases"),
    y = "number of reported cases",
    color = "group",
    title = "Iowa: COVID-19 cumulative-case trajectories",
    caption = "Data from NYT, based on reports from state and local health agencies"  
  )
```

![](01-Iowa_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->
