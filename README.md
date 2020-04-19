Covid-19-visualizations
================

Visualizations of the Covid-19 situation as it unfolds.

Post your favorite visualizations, make sure to explain the important
aspects.

  - [Ian Lyttle’s work](ijlyttle/README.md)

Links to great visualizations are also welcome\!

Generally: add what you want, but if you break it you fix it :)

John Hopkins and the NY Times are posting up-to-date information about
the number of Covid-19 cases.

## License

This work is licensed under a [Creative Commons Attribution 4.0
International License](https://creativecommons.org/licenses/by/4.0/).

## Examples

This chart shows the number of cases in Iowa - fill color shows recent
trend (last week) of the number of days in which cases double.

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

Here are a few of the charts, focusing on Iowa, that Ian has made, using
the [NYT data](https://github.com/nytimes/covid-19-data), [current
data](https://coronavirus.iowa.gov/pages/access) from the state, and
[county-population
data](https://www.icip.iastate.edu/tables/population/counties-estimates).

First is shows counties with large numbers of cases, also showing
aggregations for the state as a whole, as well as for counties not
otherwise
shown:

<img src="ijlyttle/03-Plots_files/figure-gfm/cases-county-1.png" width="864" />

We can remake this plot in the style of [FT
charts](https://www.ft.com/coronavirus-latest):

<img src="ijlyttle/03-Plots_files/figure-gfm/cases-county-ft-1.png" width="864" />

What I’m seeing here is that counties race out of the gate, then settle
down to a doubling-rate between a week to two-weeks. This was the case
until April 19, where I am seeing an across-the-board increase. Almost
400 new cases reported on Apr. 19, doubling previous record - I will be
interested to learn if there has been a change in testing protocol.

Again, following the style of FT, we can look at the seven-day rolling
average of new
cases:

<img src="ijlyttle/03-Plots_files/figure-gfm/new-cases-county-ft-1.png" width="864" />

As of 2020-04-19, I am **not** seeing a peak in new-cases being
approached.

Focusing on Johnson county, I see three waves of increases:

  - days 0-7: the initial cases associated with the Egyptian cruise
  - days 7-23: the establishment of community-spread
  - days 23-38: another rise perhaps associated with spring-breakers
    returning
  - days 38- , another uptick.

Another question I want to ask deals with differences between large
counties and and small counties. My idea was to split the counties into
groups, according to population. Eacg group of counties has
approximately the same
population:

<img src="ijlyttle/03-Plots_files/figure-gfm/iowa-map-1.png" width="864" />

Essentially, a quarter of the state’s population lives in the two
most-populous counties. Also a quarter of the state’s population lives
in the least-populous counties.

This lets us compare counts among the different
county-groups:

<img src="ijlyttle/03-Plots_files/figure-gfm/iowa-proportions-1.png" width="864" />

The proportion of reported cases is not terribly different from the
proportion of the population itself. That said, the largest counties
account for a little more than half the deaths; I think this is due to
the long-term care outbreaks, particularly in Linn County.

Finally, I’d like to show the seven-day rolling average of new-cases,
according to
group:

<img src="ijlyttle/03-Plots_files/figure-gfm/new-cases-group-1.png" width="864" />

This suggests to me that, since the last week of March, roughly the same
things are happenning all over the state, regardless of the
county-group. It could be that larger counties have more long-term care
facilites and that smaller counties have more meat-packing plants.

This is my opinion, but rather than explaining using extenuating
circumstances, I think the call to action is that we need to do more to
protect our vulneraablities, including packing plants, long-term care,
hopsitals. We *all* have a role to play in reducing the spread.
