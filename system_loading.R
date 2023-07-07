# load libraries
library(tidyverse)

# define parameters
crayling.monthly.target <- 227000 # monthly target of crayling stage 2 (min six weeks)
hatching.frequency <- 3 # number of spawning events per year
fecundity <- 400 # number of eggs produced
crayling.survival <- 0.7 # proportion survival from egg to stage 2
breeding.females.1 <- 12 * crayling.monthly.target / fecundity / hatching.frequency / crayling.survival
breeding.females <- breeding.population / 3 * 2
breeding.males <- breeding.females / 2
weekly.egg.prod <- breeding.females * 400 * 3 / 365 * 7
hatch.rate <- 0.9
weekly.egg.hatch <- weekly.egg.prod * hatch.rate
cray.daily.mortality <- 0.00625
breeding.population <- 5200
average.adult.mass <- 100
uk.breeding.population <- breeding.population / 200
uk.breeding.females <- uk.breeding.population * 2 / 3
uk.weekly.egg.prod <- uk.breeding.females * 400 * 3 / 365 * 7
uk.weekly.egg.hatch <- uk.weekly.egg.prod * hatch.rate
harvest.proportion <- 0.99

# build system loading onto production plan
# modify bw.g using case when batch_day > 210 ~ 100
# change cray number mod by system not batch day
system_loading <- prod.plan.1 %>%
  mutate(cray.number = weekly.egg.hatch * (1 - cray.daily.mortality)^batch_day,
         cray.number.mod = case_when(system == "juvenile" ~ weekly.egg.hatch * (1 - cray.daily.mortality)^crayling.harvest.day * (1 - harvest.proportion) * (1 - cray.daily.mortality)^(batch_day - crayling.harvest.day),
                                     system == "crayling" ~ weekly.egg.hatch * (1 - cray.daily.mortality)^batch_day,
                                     system == "breeding" ~ breeding.population),
         cray.number.mod.2 = case_when(system == "juvenile" ~ weekly.egg.hatch * (1 - cray.daily.mortality)^crayling.harvest.day * (1 - harvest.proportion) * (1 - cray.daily.mortality)^(batch_day - crayling.harvest.day),
                                       system == "crayling" ~ weekly.egg.hatch * (1 - cray.daily.mortality)^crayling.1.g,
                                       system == "breeding" ~ breeding.population),
         bw.g = case_when(system == "crayling" ~ (bw_0^c_1 + c_2 * batch_day)^c_4,
                          system == "juvenile" ~ (bw_0^c_1 + c_2 * batch_day)^c_4,
                          system == "breeding" ~ average.adult.mass),
         unit = breeding.population * 2 / 3 * 3 / 365, # expected number of hatches per day
         total.bw = bw.g * cray.number.mod,
         feed.rate = case_when(exp(redclaw.feed.twinlog$coef[1]) * bw.g^redclaw.feed.twinlog$coef[2] >= 0.01 ~ exp(redclaw.feed.twinlog$coef[1]) * bw.g^redclaw.feed.twinlog$coef[2], TRUE ~ 0.01),
         feed.g = bw.g * feed.rate,
         total.feed.kg = feed.g * cray.number.mod / 1000,
         protein.proportion = case_when(bw.g < 5 ~ 0.325, TRUE ~ 0.256),
         total.protein = protein.proportion * total.feed.kg)

# calculate max number of individuals in system at any one time
max.occupancy <- system_loading %>%
  group_by(system, facility_day) %>%
  summarise(t_number = sum(cray.number.mod)) %>%
  summarise(max.number = max(t_number))

# calculate max number of units in system at any one time
unit.number <- system_loading %>%
  group_by(system, batch) %>%
  summarise(unit.sum = sum(unit)) %>%
  summarise(max.unit = max(unit.sum))

# get max loading for each system
max.loading <- system_loading %>%
  group_by(system, facility_day) %>%
  summarise(feed.sum = sum(total.feed.kg),
            protein.sum = sum(total.protein),
            t_number = sum(cray.number.mod),
            mod.number = sum(cray.number.mod.2)) %>%
  summarise(feed.max = max(feed.sum),
            protein.max = max(protein.sum),
            max.number = max(t_number),
            max.mod.number = max(mod.number))

grouped_loading <- max.loading %>%
  add_row(system = "breeding.juvenile",
          feed.max = max.loading$feed.max[1] - max.loading$feed.max[1] * 24 / 52 * 2 / 3 + max.loading$feed.max[3],
          protein.max = max.loading$protein.max[1] - max.loading$protein.max[1] * 24 / 52 * 2 / 3 + max.loading$protein.max[3]) %>%
  add_row(system = "crayling.hatching",
          feed.max = max.loading$feed.max[1] * 24 / 52 * 2 / 3 + max.loading$feed.max[2],
          protein.max = max.loading$protein.max[1] * 24 / 52 * 2 / 3 + max.loading$protein.max[2]) %>%
  filter(system %in% c("breeding.juvenile", "crayling.hatching"))


# print daily loading rates
day.loading <- system_loading %>%
  group_by(system, facility_day) %>%
  summarise(feed.sum = sum(total.feed.kg),
            protein.sum = sum(total.protein))

# export as csv
write.csv(day.loading, "tabs/day_loading.csv")

# build uk system loading onto production plan
uk.system_loading <- prod.plan.1 %>%
  mutate(cray.number = uk.weekly.egg.hatch * (1 - cray.daily.mortality)^batch_day,
         cray.number.mod = case_when(system == "juvenile" ~ uk.weekly.egg.hatch * (1 - cray.daily.mortality)^crayling.harvest.day * 0.01 * (1 - cray.daily.mortality)^(batch_day - crayling.harvest.day),
                                     system == "crayling" ~ uk.weekly.egg.hatch * (1 - cray.daily.mortality)^batch_day,
                                     system == "breeding" ~ uk.breeding.population),
         cray.number.mod.2 = case_when(system == "juvenile" ~ uk.weekly.egg.hatch * (1 - cray.daily.mortality)^crayling.harvest.day * (1 - harvest.proportion) * (1 - cray.daily.mortality)^(batch_day - crayling.harvest.day),
                                       system == "crayling" ~ uk.weekly.egg.hatch * (1 - cray.daily.mortality)^crayling.1.g,
                                       system == "breeding" ~ uk.breeding.population),
         bw.g = case_when(system == "crayling" ~ (bw_0^c_1 + c_2 * batch_day)^c_4,
                          system == "juvenile" ~ (bw_0^c_1 + c_2 * batch_day)^c_4,
                          system == "breeding" ~ 100),
         unit = uk.breeding.population * 2 / 3 * 3 / 365, # expected number of hatches per day
         total.bw = bw.g * cray.number.mod,
         feed.rate = case_when(exp(redclaw.feed.twinlog$coef[1]) * bw.g^redclaw.feed.twinlog$coef[2] >= 0.01 ~ exp(redclaw.feed.twinlog$coef[1]) * bw.g^redclaw.feed.twinlog$coef[2], TRUE ~ 0.01),
         feed.g = bw.g * feed.rate,
         total.feed.kg = feed.g * cray.number.mod / 1000,
         protein.proportion = case_when(bw.g < 5 ~ 0.325, TRUE ~ 0.256),
         total.protein = protein.proportion * total.feed.kg)

# calculate max number of individuals in system at any one time
uk.max.occupancy <- uk.system_loading %>%
  group_by(system, facility_day) %>%
  summarise(t_number = sum(cray.number.mod)) %>%
  summarise(max.number = max(t_number))

# calculate max number of units in system at any one time
uk.unit.number <- uk.system_loading %>%
  group_by(system, batch) %>%
  summarise(unit.sum = sum(unit)) %>%
  summarise(max.unit = max(unit.sum))

# get max loading for each system
uk.max.loading <- uk.system_loading %>%
  group_by(system, facility_day) %>%
  summarise(feed.sum = sum(total.feed.kg),
            protein.sum = sum(total.protein),
            t_number = sum(cray.number.mod),
            mod.number = sum(cray.number.mod.2)) %>%
  summarise(feed.max = max(feed.sum),
            protein.max = max(protein.sum),
            max.number = max(t_number),
            max.mod.number = max(mod.number))

# print daily loading rates
uk.day.loading <- uk.system_loading %>%
  group_by(system, facility_day) %>%
  summarise(feed.sum = sum(total.feed.kg),
            protein.sum = sum(total.protein))

prod.plan.1 %>%
  mutate(cray.number = weekly.egg.hatch * (1 - cray.daily.mortality)^batch_day,
         cray.number.mod = case_when(system == "juvenile" ~ weekly.egg.hatch * (1 - cray.daily.mortality)^crayling.harvest.day * (1 - harvest.proportion) * (1 - cray.daily.mortality)^(batch_day - crayling.harvest.day),
                                     system == "crayling" ~ weekly.egg.hatch * (1 - cray.daily.mortality)^batch_day,
                                     system == "breeding" ~ breeding.population),
         cray.number.mod.2 = case_when(system == "juvenile" ~ weekly.egg.hatch * (1 - cray.daily.mortality)^crayling.harvest.day * (1 - harvest.proportion) * (1 - cray.daily.mortality)^(batch_day - crayling.harvest.day),
                                       system == "crayling" ~ weekly.egg.hatch * (1 - cray.daily.mortality)^crayling.1.g, # modified here to calculate area required based on density at final day in system
                                       system == "breeding" ~ breeding.population),
         bw.g = case_when(system == "crayling" ~ (bw_0^c_1 + c_2 * batch_day)^c_4,
                          system == "juvenile" ~ (bw_0^c_1 + c_2 * batch_day)^c_4,
                          system == "breeding" ~ average.adult.mass),
         unit = breeding.population * 2 / 3 * 3 / 365, # expected number of hatches per day
         total.bw = bw.g * cray.number.mod,
         feed.rate = case_when(exp(redclaw.feed.twinlog$coef[1]) * bw.g^redclaw.feed.twinlog$coef[2] >= 0.01 ~ exp(redclaw.feed.twinlog$coef[1]) * bw.g^redclaw.feed.twinlog$coef[2], TRUE ~ 0.01),
         feed.g = bw.g * feed.rate,
         total.feed.kg = feed.g * cray.number.mod / 1000,
         protein.proportion = case_when(bw.g < 5 ~ 0.325, TRUE ~ 0.256),
         total.protein = protein.proportion * total.feed.kg)



