zctaRel <- read.csv('data-raw/zcta_county_rel_10.txt', colClasses=c(ZCTA5='character', STATE='character', COUNTY='character', GEOID='character'))
zctaRel $STATEFP <- zctaRel $STATE
zctaRel $COUNTYFP <- zctaRel $COUNTY
STPOP <- aggregate(as.numeric(COPOP)~STATEFP, FUN=sum, data=zctaRel)
zctaRel <- zctaRel[, c('ZCTA5', 'STATEFP', 'COUNTYFP', 'GEOID', 'STPOP', 'COPOP', 'ZPOP', 'POPPT')]

save(zctaRel, file='data/zctaRel.rda')
