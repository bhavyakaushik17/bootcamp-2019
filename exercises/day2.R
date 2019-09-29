generation <- read.csv(here::here("data/ca_energy_generation.csv"), 
                       stringsAsFactors = F)
imports <- read.csv(here::here("data/ca_energy_imports.csv"), 
                    stringsAsFactors = F)

str(generation)

class(generation$datetime)

class(imports$datetime)

library(lubridate)
generation$datetime <- as_datetime(generation$datetime)
class(generation$datetime)

head(generation$datetime)

imports$datetime <- as_datetime(imports$datetime)
head(imports$datetime)

head(generation)

library(reshape2)
long_gen <- melt(generation, id.vars = "datetime",
                 variable.name = "source",
                 value.name = "usage")
head(long_gen)

long_gen[order(long_gen$datetime)[1:20], ]

merged_energy <- merge(generation, imports, by = "datetime")
dim(merged_energy)

head(merged_energy)

long_merged_energy <- melt(merged_energy, id.vars = "datetime",
                           variable.name = "source",
                           value.name = "usage")
head(long_merged_energy)

dim(long_merged_energy)

dim(long_gen)

library(tidyverse)

tmp <- select(merged_energy, biogas, biomass, geothermal, solar)
names(tmp)

tmp <- select(merged_energy, -biogas, -biomass, -geothermal, -solar)
names(tmp)

tmp <- select(merged_energy, contains("hydro"), starts_with("bio"))
names(tmp)

tmp <- filter(merged_energy, imports > 7000)
nrow(tmp)
head(tmp)

tmp <- filter(merged_energy, imports > 7000, natural_gas < 7000)
nrow(tmp)
head(tmp)

tmp <- mutate(long_merged_energy, log_usage = log(usage))
head(tmp)

tmp <- mutate(long_merged_energy, log_usage = log(usage), usage2 = usage^2, usage3 = usage^3)
head(tmp)

# total energy consumption
summarize(long_merged_energy, total = sum(usage, na.rm = T))

# mean energy consumption
summarize(long_merged_energy, mean_cons = mean(usage, na.rm = T))

# take df then filter it then select these variables
# you do not need to repeat the name of the dataframe!
long_merged_energy %>% 
    filter(source == "geothermal") %>% 
    select(-datetime) %>% 
    mutate(log_usage = log(usage)) %>% 
    summarize(mean_log_usage = mean(log_usage, na.rm = T))

merged_energy %>% 
    select(-datetime) %>% 
    select(contains("hydro")) %>%
    mutate(total_hydro = rowSums(., na.rm = T)) %>% 
    summarize(mean_total_hydro = mean(total_hydro, na.rm = T))

long_merged_energy %>% 
    group_by(source) %>% 
    summarize(sum_usage = sum(usage, na.rm = T))

gapminder <- read.csv(here::here("data/gapminder5.csv"))

gapminder %>% 
    group_by(year) %>% 
    summarize(mean_le = mean(lifeExp, na.rm = T),
              sd_lf = sd(lifeExp, na.rm = T))

long_merged_energy %>% 
    select(-datetime) %>% 
    filter(source == 'small_hydro'| source == 'large_hydro' | source == 'biogas' | source == 'biomass') %>%
    group_by(source) %>% 
    summarize(mean_usage = mean(usage, na.rm = T))

merged_energy %>% 
    select(datetime, contains("hydro"), contains("bio")) %>% 
    melt(id.vars = "datetime",
         variable.name = "source",
         value.name = "usage") %>% 
    group_by(source) %>% 
    summarize(mean_usage = mean(usage, na.rm = T))

# import data.table library
library(data.table)

data_file <- here::here("data", "ca_energy_generation.csv")

# read in two versions of data, one as a data.frame and one as a data.table
generation_df <- read.csv(data_file, stringsAsFactors = F)

generation_dt <- fread(data_file)

generation_dt[wind > 4400]

generation_dt[wind > 4400 & mday(datetime) == 7]

generation_dt[natural_gas <= 5000 & large_hydro > 2000]

generation_dt[solar > 10 & solar > median(solar)]

generation_dt[,.(newcol = 3*wind + solar*biogas/2)]

generation_dt[,newcol := NULL]

generation_dt[,total_hydro := small_hydro + large_hydro]

generation_dt[,.(mean(nuclear), mean(biogas))]

## not adding a column in the table
generation_dt[solar == 0, .(datetime, total_thermal = natural_gas + coal)]

generation_dt[,mean(nuclear), by = mday(datetime)]

generation_dt[,.(mean_nuc = mean(nuclear), mean_wind = mean(wind)), 
              by = mday(datetime)]

generation_dt[,median(solar), by = hour(datetime)]

generation_dt[solar>0, .(max_solar_generation = max(solar)), by = mday(datetime)]

generation_dt[,total_hydro := small_hydro + large_hydro]
generation_dt[,.(mean_nuc = mean(nuclear), mean_wind = mean(wind)), 
              by = mday(datetime)]

generation_dt[,.N]