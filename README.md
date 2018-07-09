# where

> A city database and a game !

[![Travis build status](https://travis-ci.org/dreamRs/where.svg?branch=master)](https://travis-ci.org/dreamRs/where)


## Installation

Install development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dreamRs/where")
```


## City database

Get all available cities (109 373) with:

``` r
library(where)
cities <- get_cities()
```

Or for a specific country:

```r
uk <- get_cities(country_name = "United Kingdom")
```


Get statistics by country:

```r
get_countries()
```


## Test your geography skills!

Choose a country or continent and guess where cities are :

``` r
where::where()
```

![](img/where.png)


You can launch application in your browser by setting:

```r
options("where.viewer" = "browser")
```

You can use a default area to play with:

```r
options("where.area" = "France")
```


