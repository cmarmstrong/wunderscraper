# wunderscraper
A package for sampling weather stations via Wunderground

## overview
Wunderground offers a wealth of real-time weather data from personal weather
stations across the US.  Wunderscraper helps people sample Wunderground data.

## installation
```r
devtools::install_github('cmarmstrong/wunderscraper')
```

## sampling
Sampling is a method for constructing a representation of a population.  At the
heart of sampling theory is _independence_; sampling one unit shouldn't change
the probability of sampling another. Spatial sampling is especially challenging
because units are not independent.  Measurements at one weather station will be
correlated with those at nearby stations, and correlated samples will make
effects and relationships appear larger or more certain than they should be.
One way to preserve spatial independence is to partition space into units that
are independent, and draw a representation from each partition-unit.

Sampling methods offer a couple of basic tools for preserving independence and
focusing on a population of interest.  Multistage sampling is the primary tool
for partitioning a population into independent units.  The initial stages draw
samples from a large unit, like regions or states, and later stages draw samples
from smaller units, like counties or zip codes.  Stratified sampling is a tool
for ensuring sub-populations recieve adequate coverage.  Stratified sampling
repeats a sample stage for each sub-population.  See the examples in the next
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
scrape(schedulerMMDD, c("GEOID", "ZCTA5"), size=1)
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
    sync(scheduler)
}
```


- Create spatial grids on the fly for stages or strata
```r
## sample 1 state, create grid cells of 1 degree and sample 1 cell.  Will keep
## only stations within the cell.
scrape(schedulerMMDD, c("STATEFP", "GRID", "ZCTA5"), size=c(1, 1), cellsize=1)
```

- More examples in scrape
```r
?scrape
```
