# load libraries
library(tidyverse)


# component specifics
media.area <- 750 # 750 m2/m3
biomedia.volume.ratio <- 2
biomedia.aeration.ratio <- 3

# system rates
nitrification.rate <- 0.4 # 0.4g TAN/m2/day
skimmer.ratio <- 0.3


# system efficiency 
passive.nitrification <- 0.05 # 5% passive nitrification rate
passive.denitrification <- 0.05 # 5% passive denitrification rate


# uk biofilter
biofilter.max <- max.loading %>%
  mutate(tan.max = protein.max * 0.092,
         nitrate.exchange = tan.max * 1e6 * (1 - passive.denitrification) / desired.nitrate / 1000, # exchange volume m3 to maintain nitrate levels 
         available.tan = tan.max * (1 - passive.nitrification) - (desired.tan / 1e6) * nitrate.exchange * 1000, # available tan after passive denitrification and water exchange
         biofilter.exchange = abs(available.tan  * 1e6 / (tan.c2 - tan.c1) / metabolic.period / 1000),
         biomedia.volume = available.tan / (nitrification.rate / 1000) / metabolic.period * 24 / media.area, # biomedia volume required m3
         biofilter.volume = biomedia.volume * biomedia.volume.ratio,
         biofilter.aeration = biomedia.aeration.ratio * biomedia.volume)

degass.calculations <- max.loading %>%
  mutate(tank.turnover = c(tank.areas$tank.turnover[1] + tank.areas$tank.turnover[2], tank.areas$tank.turnover[3], tank.areas$tank.turnover[4]),
         skimmer.flow = tank.turnover * skimmer.ratio,
         alkalinity.required = feed.max * 0.15,
         naoh.required = alkalinity.required / 1.25,
         co2.remaining = (feed.max * 0.5 - (naoh.required / 1.82)) * 0.9 * 0.5,
         degass.flowrate = biofilter.max$nitrate.exchange * (co2.cbest - co2.c1) + abs(co2.remaining * 1e6 / (co2.c2 - co2.c1)) / 24 / 1000)
  
# build uk system
uk.biofilter.max <- uk.max.loading %>%
  mutate(tan.max = protein.max * 0.092,
         nitrate.exchange = tan.max * 1e6 * (1 - passive.denitrification) / desired.nitrate / 1000, # exchange volume m3 to maintain nitrate levels 
         available.tan = tan.max * (1 - passive.nitrification) - (desired.tan / 1e6) * nitrate.exchange * 1000, # available tan after passive denitrification and water exchange
         biofilter.exchange = abs(available.tan  * 1e6 / (tan.c2 - tan.c1) / metabolic.period / 1000),
         biomedia.volume = available.tan / (nitrification.rate / 1000) / metabolic.period * 24 / media.area, # biomedia volume required m3
         biofilter.volume = biomedia.volume * biomedia.volume.ratio,
         biofilter.aeration = biomedia.aeration.ratio * biomedia.volume)

uk.degass.calculations <- uk.max.loading %>%
  mutate(tank.turnover = c(uk.tank.areas$tank.turnover[1] + uk.tank.areas$tank.turnover[2], uk.tank.areas$tank.turnover[3], uk.tank.areas$tank.turnover[4]),
         skimmer.flow = tank.turnover * skimmer.ratio,
         alkalinity.required = feed.max * 0.15,
         naoh.required = alkalinity.required / 1.25,
         co2.remaining = (feed.max * 0.5 - (naoh.required / 1.82)) * 0.9 * 0.5,
         degass.flowrate = biofilter.max$nitrate.exchange * (co2.cbest - co2.c1) + abs(co2.remaining * 1e6 / (co2.c2 - co2.c1)) / 24 / 1000)
  
