zctaRel <- read.csv('data-raw/zcta_county_rel_10.txt', colClasses=c(ZCTA5='character', STATE='character', COUNTY='character', GEOID='character'))
zctaRel $STATE <- substr(zctaRel $GEOID, 1, 2)
zctaRel $COUNTY <- substr(zctaRel $GEOID, 3, 5)
zctaRel <- zctaRel[, c('ZCTA5', 'STATE', 'COUNTY', 'GEOID', 'COPOP')]

save(zctaRel, file='data/zctaRel.rda')
