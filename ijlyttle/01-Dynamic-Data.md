Dynamic Data
================
Ian Lyttle
2020-06-29

The purpose of this document is to download the datasets used from
dynamic sources, in particular the lastest data from the Iowa
Coronavirus website.

``` r
library("fs")
library("crrri")
```

``` r
dir_create("data")

dir_target <- path("data", "download")
dir_create(dir_target)
```

The data from the NYT is delayed by a day. It may be useful to scrape
the Iowa Coronavirus site for the latest information.

``` r
url_access <- "https://coronavirus.iowa.gov/pages/access"
html_file <- path(dir_target, "access.html")
css_selector <- "td"
```

We are going to use [rvest](https://rvest.tidyverse.org/) to page, but
there’s a problem. The page is not a static HTML site; the data we want
is injected into the site using JavaScript. This means that **rvest**
would be looking for the css tags in a document that does not have the
data table.

We need another tool for the toolbox:
[crrri](https://rlesur.github.io/crrri) - a package written by Romain
Lesur and Christophe Dervieux - which will let us build the page
locally, using Chrome. Then we will scrape it, using our local copy of
the page.

The **crrri** package is not on CRAN, and it requires that you have
Chrome installed on your computer, so that it can call it in the
background. To run Chrome, the **crrri** package will need to know where
to *find* Chrome. You can use `pagedown::find_chrome()` to find the
location on your computer (on mine, it is `"/Applications/Google
Chrome.app/Contents/MacOS/Google Chrome"`). The **crrri** package uses
the environment variable `HEADLESS_CHROME` to look for a default
    value.

``` r
chrome <- Chrome$new()
```

    ## Running '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome' \
    ##   --no-first-run --headless \
    ##   '--user-data-dir=/Users/sesa19001/Library/Application Support/r-crrri/chrome-data-dir-kyqgsfgz' \
    ##   '--remote-debugging-port=9222' '--proxy-server=http://127.0.0.1:9000' \
    ##   '--proxy-bypass-list=localhost;127.0.0.1;github.schneider-electric.com;repo.continuum.io'

``` r
client <- chrome$connect()
```

``` r
dump_DOM <- function(client, url, html_file) {
  Network <- client$Network
  Page <- client$Page
  Runtime <- client$Runtime
  Network$enable() %...>%
  { Page$enable() } %...>%
  { Network$setCacheDisabled(cacheDisabled = TRUE) } %...>% 
  { Page$navigate(url = url) } %...>%
  { Page$loadEventFired() } %...>% {
    Sys.sleep(10) # hacky - wait for a certain event, instead
  } %...>% { 
    Runtime$evaluate(
      expression = 'document.documentElement.outerHTML'
    ) 
  } %...>% {
    writeLines(c(.$result$value, "\n"), con = html_file) 
  } %>%
  finally(
    ~ client$disconnect()
  ) %...!% {
    cat("Error:", .$message, "\n")
  }
}
```

``` r
hold(
  client %...>% dump_DOM(url_access, html_file)  
)
```

    ## NULL

``` r
chrome$close()
```
