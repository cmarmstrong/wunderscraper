zctaRel <- read.csv('data-raw/zcta_county_rel_10.txt', colClasses=c(ZCTA5='character', STATE='character', COUNTY='character', GEOID='character'))
zctaRel <- zctaRel[, c('ZCTA5', 'GEOID', 'COPOP', 'COAREA', 'COAREALAND')]

save(zctaRel, file='R/sysdata.rda')
