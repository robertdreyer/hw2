Homework 2 Stat 433
================
Robert Dreyer
2022-10-14

[robertdreyer github](https://github.com/robertdreyer/hw2)

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
#install.packages("nycflights13")
```

``` r
library(nycflights13)
library(ggplot2)
```

``` r
flights <- flights
airlines <- airlines
airports <-airports
planes <- planes
weather <- weather
```

I will be analyzing location, season, and weather to find the best time
to fly that will avoid higher delay times. Delay times should vary on
location depending on airport size and the total number of flights
departing/arriving. I assume the airport with the least amount of
flights will have the shortest delay time. Next I will analyze the
season. Depending on the season, there could be an increase in travel
depending on vacation plans or other trip plans. Closely linked with
season is the weather, weather can delay and cancel flights depending on
the precipitation or wind conditions.

\#Location

``` r
flights %>% 
  left_join(airports, by=c(origin='faa')) %>%
  group_by(origin) %>%
  summarise(mean(dep_delay, na.rm = TRUE), n())
```

    ## # A tibble: 3 x 3
    ##   origin `mean(dep_delay, na.rm = TRUE)`  `n()`
    ## * <chr>                            <dbl>  <int>
    ## 1 EWR                               15.1 120835
    ## 2 JFK                               12.1 111279
    ## 3 LGA                               10.3 104662

``` r
flights %>% 
  ggplot(mapping = aes(x = dep_time, y = dep_delay, color = origin)) +
  geom_point()+
  facet_wrap(~origin)
```

    ## Warning: Removed 8255 rows containing missing values (geom_point).

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

If we select a time strictly on location, Newark Liberty Intl (EWR) has
the highest average delay at 15.1, followed by John F Kennedy Intl (JFK)
at 12.1, followed by La Guardia (LGA) at 10.3. Corresponding with the
average delays, the total number of flights of each airpot is also in
the same order: EWR, JFK, LGA. This signifies that EWR’s higher number
of flights increases the chances of delay, as well as delay time.

\#Season

``` r
season_avg <- flights %>%
  mutate(season = case_when(month == 3 ~ 'Spring', month == 4 ~ 'Spring', month == 5 ~ 'Spring', month == 6 ~ 'Summer', month == 7 ~ 'Summer', month == 8 ~ 'Summer', month == 9 ~ 'Fall', month == 10 ~ 'Fall', month == 11 ~ 'Fall', month == 12 ~ 'Winter', month == 1 ~ 'Winter', month == 2 ~ 'Winter')) %>%
  group_by(season)%>%
  summarise(mean(dep_delay, na.rm = TRUE))

season_avg
```

    ## # A tibble: 4 x 2
    ##   season `mean(dep_delay, na.rm = TRUE)`
    ## * <chr>                            <dbl>
    ## 1 Fall                              6.14
    ## 2 Spring                           13.4 
    ## 3 Summer                           18.3 
    ## 4 Winter                           12.6

``` r
flights %>%
  mutate(season = case_when(month == 3 ~ 'Spring', month == 4 ~ 'Spring', month == 5 ~ 'Spring', month == 6 ~ 'Summer', month == 7 ~ 'Summer', month == 8 ~ 'Summer', month == 9 ~ 'Fall', month == 10 ~ 'Fall', month == 11 ~ 'Fall', month == 12 ~ 'Winter', month == 1 ~ 'Winter', month == 2 ~ 'Winter')) %>%
  group_by(season, month)%>%
  mutate(mean(dep_delay, na.rm = TRUE))%>%
  ggplot(mapping = aes(x = dep_time, y = dep_delay, color = season)) +
  geom_point() +
  facet_wrap(~season) +
  geom_abline() 
```

    ## Warning: Removed 8255 rows containing missing values (geom_point).

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

If we analyze departure times based on season, Summer has the highest
average delay, then Spring, Winter, Fall. The times with the shortest
delays seem to occur at 5:00 am, and then increase at a constant rate
until 3:00 pm, and then gradually plateau. The early morning flights
seem to have similar delay times to the times after 3:00 pm.

\#Weather

``` r
flights %>% 
  mutate(season = case_when(month == 3 ~ 'Spring', month == 4 ~ 'Spring', month == 5 ~ 'Spring', month == 6 ~ 'Summer', month == 7 ~ 'Summer', month == 8 ~ 'Summer', month == 9 ~ 'Fall', month == 10 ~ 'Fall', month == 11 ~ 'Fall', month == 12 ~ 'Winter', month == 1 ~ 'Winter', month == 2 ~ 'Winter')) %>%
  left_join(weather, by=c('year'='year', 'month'='month','day'='day','hour'='hour', 'origin'='origin'))%>%
  group_by(season)%>%
  summarise(mean(wind_gust, na.rm = TRUE), sum(precip, na.rm = TRUE), mean(dep_delay, na.rm = TRUE))
```

    ## # A tibble: 4 x 4
    ##   season `mean(wind_gust, na.rm … `sum(precip, na.rm = … `mean(dep_delay, na.rm…
    ## * <chr>                     <dbl>                  <dbl>                   <dbl>
    ## 1 Fall                       25.0                   149.                    6.14
    ## 2 Spring                     25.7                   394.                   13.4 
    ## 3 Summer                     22.3                   635.                   18.3 
    ## 4 Winter                     27.2                   352.                   12.6

``` r
flights %>% 
  mutate(season = case_when(month == 3 ~ 'Spring', month == 4 ~ 'Spring', month == 5 ~ 'Spring', month == 6 ~ 'Summer', month == 7 ~ 'Summer', month == 8 ~ 'Summer', month == 9 ~ 'Fall', month == 10 ~ 'Fall', month == 11 ~ 'Fall', month == 12 ~ 'Winter', month == 1 ~ 'Winter', month == 2 ~ 'Winter')) %>%
  left_join(weather, by=c('year'='year', 'month'='month','day'='day','hour'='hour', 'origin'='origin'))%>%
  group_by(month)%>%
  summarise(mean(wind_gust, na.rm = TRUE), sum(precip, na.rm = TRUE), mean(dep_delay, na.rm = TRUE))
```

    ## # A tibble: 12 x 4
    ##    month `mean(wind_gust, na.rm … `sum(precip, na.rm = … `mean(dep_delay, na.rm…
    ##  * <int>                    <dbl>                  <dbl>                   <dbl>
    ##  1     1                     27.9                   58.6                   10.0 
    ##  2     2                     28.4                  117.                    10.8 
    ##  3     3                     26.5                  115.                    13.2 
    ##  4     4                     25.5                   56.2                   13.9 
    ##  5     5                     24.4                  223.                    13.0 
    ##  6     6                     23.1                  324.                    20.8 
    ##  7     7                     21.4                  125.                    21.7 
    ##  8     8                     22.0                  186.                    12.6 
    ##  9     9                     22.3                   67.2                    6.72
    ## 10    10                     23.9                   22.7                    6.24
    ## 11    11                     27.1                   59.4                    5.44
    ## 12    12                     23.8                  176.                    16.6

``` r
flights %>% 
  mutate(season = case_when(month == 3 ~ 'Spring', month == 4 ~ 'Spring', month == 5 ~ 'Spring', month == 6 ~ 'Summer', month == 7 ~ 'Summer', month == 8 ~ 'Summer', month == 9 ~ 'Fall', month == 10 ~ 'Fall', month == 11 ~ 'Fall', month == 12 ~ 'Winter', month == 1 ~ 'Winter', month == 2 ~ 'Winter')) %>%
  left_join(weather, by=c('year'='year', 'month'='month','day'='day','hour'='hour', 'origin'='origin'))%>%
  ggplot(mapping = aes(x = wind_gust, y = dep_delay, color = season)) +
  geom_point() +
  facet_wrap(~season)
```

    ## Warning: Removed 259042 rows containing missing values (geom_point).

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
flights %>% 
  mutate(season = case_when(month == 3 ~ 'Spring', month == 4 ~ 'Spring', month == 5 ~ 'Spring', month == 6 ~ 'Summer', month == 7 ~ 'Summer', month == 8 ~ 'Summer', month == 9 ~ 'Fall', month == 10 ~ 'Fall', month == 11 ~ 'Fall', month == 12 ~ 'Winter', month == 1 ~ 'Winter', month == 2 ~ 'Winter')) %>%
  left_join(weather, by=c('year'='year', 'month'='month','day'='day','hour'='hour', 'origin'='origin')) %>%
  ggplot(mapping = aes(x = precip, y = dep_delay, color = season)) +
  geom_point() +
  facet_wrap(~season)
```

    ## Warning: Removed 9783 rows containing missing values (geom_point).

![](README_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

By grouping the weather conditions by season, we can see that Summer had
the lowest average wind gusts but the highest total precipitation,
showing that precipitation may have a bigger impact on delay time than
wind gusts.

If you want to avoid delays as much as possible, you should fly in
November out of La Guardia on a day that it is not raining. To have the
lowest chances of a flight delay, a flight closest to 5:00 am should
have the lowest delay time.
