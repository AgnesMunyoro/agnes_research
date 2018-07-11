# 17/1/2017 

# then some text
# some other text


# Script for Agnes

# Aims: 

# Link data on regional population structure with fertility schedules in 
# England & Wales 

# develop code examples which can be adapted for exploratory analysis


# This is a comment. Any letters to the right of the # symbol will be ignored.

# Blocks of many
# lines can be
# commented or
# uncommented by
# selecting them
# and then pressing
# CTRL + SHIFT + C


# Useful code to put at the start of a script
rm(list= ls()) # This performs a 'clean sweep' of the R environment, so that objects from 
#previous sessions are not loaded by mistake.
pacman::p_load(
  tidyverse
) # This loads a series of packages that make R easier to work and communicate with

# Note: the first time you run this on your machine you may have to run the following

# install.packages("pacman")
# This has been commented so you do not run it by mistake. 
# As mentioned before you can remove the comment by selecting the line and pressing CTRL + SHIFT + C


# Data Sources -------------------------------------------------------------


# I will take two prepared data sources and place them in the 
# data directory 


# Dataset 1: fertility schedules 

# The first dataset is from the Human Fertility Database, combined with the Human Fertility Collection

# The code which produced the file is available here:
# https://github.com/JonMinton/comparative_fertility/blob/master/scripts/hfc_hfd_data_combine.R

# I will copy the file from my local machine as follows:

#file.copy("E://repos/comparative_fertility/data/data_combined_and_standardised.csv", "data/fertility_schedules.csv")

dta_schedules <- read_csv("data/fertility_schedules.csv")

dta_schedules

glimpse(dta_schedules)

# object %>% first_action() %>% second_action() %>% third_action()
# third_action(second_action(first_action(object))))

dta_schedules %>% glimpse()
# This contains data for many countries, but we are only interested in England & Wales

dta_schedules_enw <- dta_schedules %>% 
  filter(code == "GBRTENW") 

# dta_schedules %>% 
#   filter(code == "GBRTENW") -> dta_schedules_enw
# ->
# <-
# The dataset also includes data for years we are not going to explore further. We will filter them out later


# Dataset 2: Regional population structure 

# The second data are from the ONS's Components of Change dataset
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/datasets/componentsofchangebirthsdeathsandmigrationforregionsandlocalauthoritiesinenglandtable5

# There may currently be a more recent release of the dataset than the one I worked with 

# I will copy the data as follows

# file.copy(
#   "E://repos/uk_migrant_demography/data/tidied/englandwales_population.csv",
#   "data/engwales_population.csv"
#   )

dta_pop_struct <- read_csv("data/engwales_population.csv")


# To discuss: how to do basic aggregation, restructing, summary statistics, regional comparisons and so on


# We are particularly interested in comparing the actual number of babies with those estimated from applying 
# England & Wales' overall fertility schedule for a given year 
# We should probably use the age-structure from year t, as applied to that year's fertility structure, to 
# produce a prediction of expected number of babies in year t + 1


# To start with, let's look at the number of babies in each year overall, and by region

dta_pop_struct %>%
  filter(age == 0) %>% 
  group_by(year) %>% 
  summarise(babies = sum(population)) %>% 
  ggplot(aes(x = year, y = babies)) +
  geom_line()

# dta_pop_struct %>%
#   filter(age == 0) %>% 
#   group_by(year) %>% 
#   summarise(babies = sum(population)) %>% 
#   ggplot(aes(x = year, y = babies)) +
#   geom_point()

# Now let's do the same for each region

#common scale
dta_pop_struct %>%
  filter(age == 0) %>% 
  group_by(ons_region_name, year) %>% 
  summarise(babies = sum(population)) %>% 
  ggplot(aes(x = year, y = babies)) +
  geom_line() +
  facet_wrap( ~ ons_region_name)

# separate scales
dta_pop_struct %>%
  filter(age == 0) %>% 
  group_by(ons_region_name, year) %>% 
  summarise(babies = sum(population)) %>% 
  ggplot(aes(x = year, y = babies)) +
  geom_line() +
  facet_wrap(~ ons_region_name, scales = "free_y")


#indexed to first year 

dta_pop_struct %>%
  filter(age == 0) %>% 
  group_by(ons_region_name, year) %>% 
  summarise(babies = sum(population)) %>% 
  ungroup() %>% group_by(ons_region_name) %>% 
  mutate(baby_index = babies / babies[year == min(year)]) %>% 
  ggplot(aes(x = year, y = baby_index, colour = ons_region_name)) +
  geom_line() 

# dta_pop_struct %>%
#   filter(age == 0) %>% 
#   group_by(ons_region_name, year) %>% 
#   summarise(babies = sum(population)) %>% 
#   mutate(baby_index = babies / babies[year == min(year)]) %>% 
#   ggplot(aes(x = year, y = baby_index, colour = ons_region_name)) +
#   geom_line() 


# This makes it clearer that London is somewhat different in its 'baby growth rate'
# than the other English/Welsh ('Wenglish'?!) regions

# Let's look at this as a table 
dta_pop_struct %>%
  filter(age == 0) %>% 
  group_by(ons_region_name, year) %>% 
  summarise(babies = sum(population)) %>% 
  ungroup() %>% group_by(ons_region_name) %>% 
  mutate(baby_index = babies / babies[year == min(year)]) %>% 
  ungroup() %>% 
  select(ons_region_name, year, baby_index) %>% 
  spread(ons_region_name, baby_index) -> baby_index_table

baby_index_table
# We can save tables like this to Excel as follows 

# More manual way:
write.csv(baby_index_table, "clipboard")

# More automated way: 

pacman::p_load(openxlsx)

# dir.create("excel_workbooks")

# wb <- createWorkbook()
# addWorksheet(wb, "regional baby growth index")
# writeData(wb = wb, sheet = "regional baby growth index", baby_index_table)
# saveWorkbook(wb, file = "excel_workbooks/summary_tables.xlsx", overwrite = T)
# rm(wb)


# Now now try to produce some estimates for numbers of babies

# We can see the fertility schedules for the whole of England & Wales by year and age as follows 

dta_schedules_enw %>% 
  filter(year >= 2002, year <= 2013) %>% 
  ggplot(aes(x = age, y = asfr, group = year, colour = year)) + 
  geom_line() + 
  scale_color_distiller(type = "qual")

# Overall it appears fertility levels have been increasing from 2002 to 2013
# The age of peak fertility has been moving to the right slightly (to slightly older ages)
# But there has also been an increase in age-specific fertility rates at younger ages 

# Let's start by applying the fertility schedules to the whole of Wengland

dta_pop_struct %>% 
  filter(sex=="female") %>% 
  select(age, year, population) %>% 
  left_join(
    dta_schedules_enw %>% select(year, age, asfr)
  ) %>% 
  mutate(asfr = ifelse(is.na(asfr), 0, asfr)) %T>% print(sample_n(10)) %>% 
  mutate(babies_by_age = asfr * population) %T>% print(sample_n(10)) %>% 
  group_by(year) %>% 
  summarise(total_babies_expected = sum(babies_by_age)) %>% 
  ungroup() %>% 
  mutate(year = year + 1) %>% 
  filter(total_babies_expected != 0) -> baby_predictions_overall

baby_predictions_overall

# Note the last year in the fertility data are for 2011, which means the last year 
# we can make a prediction about is 2012

dta_schedules_enw %>% 
  summarise(min_year = min(year, na.rm = T), max_year = max(year, na.rm =T))


# Now let's join the expected with the actual number of babies 

dta_pop_struct %>% 
  select(sex, age, year, population) %>% 
  filter(age == 0) %>% 
  group_by(year) %>% 
  summarise(total_babies_observed = sum(population)) %>% 
  ungroup() %>% 
  inner_join(baby_predictions_overall) %>% 
  mutate(
    percent_difference = 100 * (total_babies_observed - total_babies_expected) / total_babies_expected
  ) -> baby_gap_overall

baby_gap_overall

# Overall, the differences are typically below 2% 
# This is of course fairly reassuring. 
# These differences may well be because the gestational period is 9 months
# But in projecting forward to the next year, we are forced to assume a 
# gestational period of 12 months instead. 


# We now want to do the same, but for each region

dta_pop_struct %>% 
  filter(sex=="female") %>% 
  select(region = ons_region_name, age, year, population) %>% 
  left_join(
    dta_schedules_enw %>% select(year, age, asfr)
  ) %>% 
  mutate(asfr = ifelse(is.na(asfr), 0, asfr)) %T>% print(sample_n(10)) %>% 
  mutate(babies_by_age = asfr * population) %T>% print(sample_n(10)) %>% 
  group_by(region, year) %>% 
  summarise(total_babies_expected = sum(babies_by_age)) %>% 
  ungroup() %>% 
  mutate(year = year + 1) %>% 
  filter(total_babies_expected != 0) -> baby_predictions_byregion

baby_predictions_byregion

dta_pop_struct %>% 
  select(region = ons_region_name, sex, age, year, population) %>% 
  filter(age == 0) %>% 
  group_by(region, year) %>% 
  summarise(total_babies_observed = sum(population)) %>% 
  ungroup() %>% 
  inner_join(baby_predictions_byregion) %>% 
  mutate(
    percent_difference = 100 * (total_babies_observed - total_babies_expected) / total_babies_expected
  ) -> baby_gap_byregion

baby_gap_byregion


baby_gap_byregion %>% 
  ggplot(aes(x = year, y = percent_difference, group = region, colour = region)) + 
  geom_line() + 
  geom_hline(yintercept = 0)


baby_gap_byregion %>% 
  ggplot(aes(x = year, y = percent_difference)) + 
  geom_line() + 
  geom_hline(yintercept = 0) +
  facet_wrap(~region)

# Contrary to my initial hypothesis, London is characterised by 
# fewer babies than expected given its population demographics.
# This seems to be an important result, with important implications 
# for the relationship between migration and fertility


# Some of the factors which might help explain these differences are :

# male:female ratios (general/age-specific)
# proportion of population that moves from one year to the next 
# i.e. proportion of population who are in-migrants 
# variables from other datasets 
# net migration as % of population 

# You might also be interested in comparing numbers of deaths with numbers of births 

# Deciding on what to explore will be a key part of the dissertation 

dta_pop_struct %>% 
  select(region = ons_region_name, sex, year, international_net, internal_net, deaths, population) %>% 
  group_by(region, year) %>% 
  summarise(
    international_net = sum(international_net),
    internal_net = sum(internal_net),
    deaths = sum(deaths),
    population = sum(population)
  ) %>% 
  ungroup() %>% 
  mutate(
    pct_international = 100 * international_net / population,
    pct_internal = 100 * internal_net / population
  ) %>% 
  select(region, year, pct_internal, pct_international) -> pct_net_migrant_byregion
  

pct_net_migrant_byregion %>% 
  gather(key = migrant_type, value = percent, pct_internal:pct_international) %>% 
  ggplot(aes(x = year, y = percent, linetype = migrant_type)) + 
  facet_wrap(~region) + 
  geom_line() +
  geom_hline(yintercept = 0)


# We can start to build simple regressions or do correlations as follows:

baby_gap_byregion %>% 
  select(region, year, babygap_percent = percent_difference) %>% 
  inner_join(pct_net_migrant_byregion) -> percent_data

model_01 <- lm(babygap_percent  ~ pct_internal + pct_international, data = percent_data)

summary(model_01)



# To discuss: implications of this model 

# Is there a London exceptionalism? 

model_02 <- lm(
  babygap_percent ~ pct_internal + pct_international + region, 
  data = percent_data
  )

summary(model_02)
  
# This gives some very different answers 
# Understanding what the comparisons between these two models show
# could well be a substantial part of the dissertation. 

# The adjusted R-squared values for both models are very different 

# We can test whether the more complicated model has enough 'bang for its buck' 
# using an ANOVA test, or looking at AIC or BIC
anova(model_01, model_02)

AIC(model_01, model_02)
BIC(model_01, model_02)

# Lower scores in AIC and BIC are better. 

# Note: to work with and visualise model results more easily you may want to look at 
# the broom package 


# Something else to consider is whether we should try to predict something else 
# the absolute or root-mean squared (RMS) percentage error in prediction, 
# rather than the size (and sign) of the gap between expected and observed


### Joining house price data 


hp_1 <- read_csv("data/ukhpi-comparison-all-hpi-east-of-england-from-2003-01-01-to-2012-12-01.csv")
hp_2 <- read_csv("data/ukhpi-comparison-all-hpi-north-east-from-2003-01-01-to-2012-12-01.csv")

hp_joined <- bind_rows(hp_1, hp_2)

# hp_joined %>% 
#   select(region = Name, 
#          month = Period,
#          volume = `Sales volume`,
#          hpi = `House price index All property types`
#          ) %>% 
#   mutate(month = paste0(month, "-01")) %>% 
#   mutate(month_proper = lubridate::ymd(month)) %>% 
#   mutate(year = lubridate::year(month_proper)) %>% 
#   select(region, volume, hpi, year) %>% 
#   group_by(region, year) %>% 
#   summarise(avg_hpi = mean(hpi)) %>% 
#   ungroup()

hpi_data <-
  hp_joined %>% 
  select(region = Name, 
         month = Period,
         volume = `Sales volume`,
         hpi = `House price index All property types`
  ) %>% 
  mutate(region = ifelse(region == "West Midlands Region", "West Midlands", region)) %>% 
  mutate(month = paste0(month, "-01")) %>% 
  mutate(month_proper = lubridate::ymd(month)) %>% 
  mutate(year = lubridate::year(month_proper)) %>% 
  select(region, volume, hpi, year) %>%
  mutate(tmp = volume * hpi) %>% 
  group_by(region, year) %>% 
  summarise(
    weighted_avg_hpi = sum(tmp) / sum(volume),
    avg_hpi = mean(hpi)
    ) %>% 
  ungroup()
  


