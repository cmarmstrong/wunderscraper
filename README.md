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
because sampling units are not independent.  Measurements of weather at a point
in space are related to nearby points, and randomly sampling one of these points
ought to alter the probability that nearby points are later sampled.  One way to
preserve spatial independence is to partition space into units that are
independent, and draw a representation from each unit.

Sampling methods offer a couple of basic tools for preserving independence and
focusing on a population of interest.  Multistage sampling is the primary tool
for partitioning a population into independent units.  The initial stages draw
samples from a large unit, like regions or states.  From within the units of the
initial stages, later stages draw samples from smaller units, like counties or
zip codes.  Stratafied sampling is a tool for ensuring subpopulations recieve
adequate coverage.  Stratafied sampling repeats a sample stage for each
subpopulation.  See the examples in the next section for more details.

## features
- Wunderscraper is integrated with \code{\link{tigris}} for state and county
  administrative boundaries
  #' scrape(schedulerMMDD, c("GEOID", "ZCTA5"), size=c(1, NA, 1),
#'        strata=c(NA, NA, "GRID"), weight="COPOP", cellsize=c(NA, 0.01))
