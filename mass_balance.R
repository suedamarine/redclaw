# load libraries
library(tidyverse)

# define water quality parameters
desired.tss <- 20 # max 20 mg/l in tanks
desired.o2 <- 5.0 # min 5 mg/l in tanks
desired.tan <- 1 # max 1 mg/l TAN in tanks
desired.nitrate <- 50 # max 50 mg/l nitrate in tanks
desired.co2 <- 15 # max 15 mg/l in tanks
desired.t <- 28 # system operating temp in C
site.altitude <- 50 # site altitude in metres
cone.bar <- 0.7
feeding.period <- 12
metabolic.period <- feeding.period + 4

# constants
k <- 1.42903
beta.oxygen <- exp(-58.3877 + 85.8079 *  (100/(desired.t + 273.15)) + 23.8439 * log((desired.t+273.15) / 100))
barometric.pressure <- 10^(2.88014 - site.altitude/19748.2)
water.v.pressure <- 4.7603 * exp(0.0645 * desired.t)
oxygen.percent <- 20.95
oxygen.saturation <- 1000 * k * beta.oxygen * oxygen.percent / 100 * ((barometric.pressure - water.v.pressure) / 760) # if cone used at pressure swap out barometric.pressure for cone.pressure
cone.pressure <- cone.bar * 750.06 + barometric.pressure

# component specifics
# system efficiency 
tan.c1 <- desired.tan
tan.cbest <- 0
tan.te <- 1
tan.c2 <- tan.c1 + tan.te * (tan.cbest - tan.c1)

co2.c1 <- desired.co2
co2.cbest <- 0.5
co2.te <- 0.7
co2.c2 <- co2.c1 + co2.te * (co2.cbest - co2.c1)

tss.c1 <- desired.tss
tss.cbest <- 0
tss.te <- 0.70
tss.c2 <- tss.c1 + tss.te * (tss.cbest - tss.c1)

oxy.c1 <- desired.o2 # min 5 mg/l in tanks
oxy.cbest <- oxygen.saturation
oxy.te <- 0.99
oxy.c2 <- oxy.c1 + oxy.te * (oxy.cbest - oxy.c1)

# system rates
min.tank.exchange <- 1.5 # 1.5 tank turnovers per hour
flow.contingency <- 1.25 # extra flow capacity for system
new.water <- 0.05 # 5% contingency for splash/ evaporation

# build production tanks
breeding.density <- 7
hatching.density <- 20
crayling.density <- 250
juvenile.density <- 19
tank.areas <- tibble(system = c("breeding", "hatching", "crayling", "juvenile"),
                     ind.number = c(max.loading$max.mod.number[1], max.loading$max.mod.number[1] * 24 / 52, max.loading$max.mod.number[2], max.loading$max.mod.number[3]),
                     area.required = ind.number / c(7, 20, 250, 19),
                     water.depth = c(0.2, 0.2, 0.1, 0.1),
                     tank.volumes = area.required * water.depth,
                     tank.turnover = tank.volumes * flow.contingency)

# parameter production
system.parameters <- max.loading %>%
  mutate(p.tan = protein.max * 0.092,
         tss = feed.max * 0.25,
         total.od = feed.max * 0.5,
         fish.het.od = feed.max * 0.37,
         nitrif.od = feed.max * 0.13,
         feed.co2 = 0.37 * feed.max * 1.375)

system.flows <- system.parameters %>%
  mutate(tan.flow = abs(p.tan * 1e6 / (tan.c2 - tan.c1) / metabolic.period / 1000),
         co2.flow = abs(feed.co2 * 1e6 / (co2.c2 - co2.c1) / 24 / 1000),
         tss.flow = abs(tss * 1e6 / (tss.c2 - tss.c1) / 24 / 1000),
         oxy.flow = abs(fish.het.od * 1e6 / (oxy.c2 - oxy.c1) / 24 / 1000)) %>%
  select(c(system, tan.flow, co2.flow, tss.flow, oxy.flow))

# build UK tanks
uk.tank.areas <- tibble(system = c("breeding", "hatching", "crayling", "juvenile"),
                     ind.number = c(uk.max.loading$max.mod.number[1], uk.max.loading$max.mod.number[1] * 24 / 52, uk.max.loading$max.mod.number[2], uk.max.loading$max.mod.number[3]),
                     area.required = ind.number / c(breeding.density, hatching.density, crayling.density, juvenile.density),
                     water.depth = c(0.2, 0.2, 0.1, 0.1),
                     tank.volumes = area.required * water.depth,
                     tank.turnover = tank.volumes * flow.contingency)

# uk parameter production
uk.system.parameters <- uk.max.loading %>%
  mutate(p.tan = protein.max * 0.092,
         tss = feed.max * 0.25,
         total.od = feed.max * 0.5,
         fish.het.od = feed.max * 0.37,
         nitrif.od = feed.max * 0.13,
         feed.co2 = 0.37 * feed.max * 1.375)

uk.system.flows <- uk.system.parameters %>%
  mutate(tan.flow = abs(p.tan * 1e6 / (tan.c2 - tan.c1) / metabolic.period / 1000),
         co2.flow = abs(feed.co2 * 1e6 / (co2.c2 - co2.c1) / 24 / 1000),
         tss.flow = abs(tss * 1e6 / (tss.c2 - tss.c1) / 24 / 1000),
         oxy.flow = abs(fish.het.od * 1e6 / (oxy.c2 - oxy.c1) / 24 / 1000)) %>%
  select(c(system, tan.flow, co2.flow, tss.flow, oxy.flow))

single.batch <- tibble(system = c("breeding", "hatching", "crayling", "juvenile"),
                       ind.number = c(3, 1, fecundity * hatch.rate * (1 - cray.daily.mortality)^crayling.harvest.day, fecundity * hatch.rate * (1 - cray.daily.mortality)^crayling.harvest.day * 0.01),
                       area.required = ind.number / c(breeding.density, hatching.density, crayling.density, juvenile.density),
                       water.depth = c(0.15, 0.15, 0.15, 0.15),
                       tank.volumes = area.required * water.depth,
                       tank.turnover = tank.volumes * flow.contingency)

