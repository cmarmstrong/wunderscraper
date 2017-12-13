zctaRel <- read.csv('data-raw/zcta_county_rel_10.txt', colClasses=c(ZCTA5='character', STATE='character', COUNTY='character', GEOID='character'))
zctaRel $STATEFP <- zctaRel $STATE
zctaRel $COUNTYFP <- zctaRel $COUNTY
STPOP <- aggregate(as.numeric(COPOP)~STATEFP, FUN=sum, data=zctaRel)
names(STPOP) <- c('STATEFP', 'STPOP')
zctaRel <- merge(zctaRel, STPOP, by='STATEFP')
zctaRel <- zctaRel[, c('ZCTA5', 'STATEFP', 'COUNTYFP', 'GEOID', 'STPOP', 'COPOP', 'ZPOP', 'POPPT')]

save(zctaRel, file='data/zctaRel.rda')
