# wunderscraper
A package for sampling weather stations via Wunderground

## overview
Wunderscraper helps tap and organize a wealth of real-time weather data from
Wunderground.  The real-time nature of Wunderground's vast network of weather
stations must be sampled; it is impossible to collect data from all the
stations all the time.  Wunderscraper provides flexible spatial and temporal
sampling to efficiently build a representation of weather at hyper local scales.

## installation
- CRAN release
```r
install.packages('wunderscraper')
```

- development version
```r
devtools::install_github('cmarmstrong/wunderscraper')
```

## sampling
Sampling is a method for constructing a representation of a population.  At the
heart of sampling theory is _independence_; sampling one unit shouldn't change
the probability of sampling another. Spatial sampling is especially challenging
because units are not independent.  Measurements at one weather station will be
correlated with those at nearby stations.  One way to preserve spatial
independence is to partition space into units that are independent, and draw a
representation from each partition.

Sampling methods offer a couple of basic tools for preserving independence and
focusing on a population of interest.  Multistage sampling is the primary tool
for partitioning a population into independent units.  The initial stages draw
samples from a large unit, like regions or states, and later stages draw samples
from smaller units nested within the larger ones, eg counties or zip codes.
Stratified sampling is a tool for ensuring sub-populations recieve adequate
coverage.  Stratified sampling repeats a sample stage for each sub-population.
Stratified sampling is useful for evenly covering sub-populations, or for
oversampling a particularly small sub-population.  See the examples in the next
section for more details.

## features
- Wunderscraper is integrated with the tigris package for state and county
  administrative boundaries
```r
library(wunderscraper)
schedulerMMDD <- scheduler()
setApiKey(f="apikey.txt")
## sample 1 county and collect all weather stations.  Will keep only stations
## within the county administrative boundary, as determined from tigris
scrape(schedulerMMDD, c("GEOID", "ZCTA5", "id"), size=1)
```

- Multistage sampling provides efficient coverage over an area of interest
```r
data(zctaRel)
## monitor a tri-state area
triState <- zctaRel[zctaRel $STATEFP %in% c("09", "34", "36"), ]
repeat scrape(schedulerMMDD, c("STATEFP", "GEOID", "ZCTA5"), size=c(1, 10, 1, 10),
              sampleFrame=triState)
```

- Stratified sampling ensures all sub-populations are adequately covered
```r
## monitor a tri-state, stratified by state to ensure complete coverage each sample
repeat scrape(schedulerMMDD, c("GEOID", "ZCTA5"), size=c(10, 1, 10), strata=rep("STATEFP", 3),
              sampleFrame=triState)
```

- Set a schedule to control period of repeat samples
```r
## monitor a tri-state area with two hour period
plan(schedulerMMDD, '2 hours')
repeat {
    scrape(schedulerMMDD, c("GEOID", "ZCTA5"), size=c(10, 1, 10), strata=rep("STATEFP", 3),
           sampleFrame=triState)
    sync(schedulerMMDD)
}
```


- Create spatial grids on the fly for stages or strata
```r
## sample stations at a resolution of 0.01 degrees, one station per grid of resolution
scrape(schedulerMMDD, c("GEOID", "ZCTA5"), size=c(10, 1, 1), strata=c(NA, NA, "GRID"),
       cellsize=c(NA, 0.01))
```

- More examples in scrape
```r
?scrape
```
