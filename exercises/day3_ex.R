library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(here)
library(readr)

nys_schools <- read_csv(here::here("data/nys_schools.csv"))
nys_acs <- read_csv(here::here("data/nys_acs.csv"))

summary(nys_acs)
summary(nys_schools)


sum(df$col==value,na.rm=FALSE)


nys_acs$v1[nys_schools$v1==99] <- NA


nys_schools[nys_schools == '-99']

library(naniar)
nys_schools %>% replace_with_na(replace = c(-99,"-99"))
#> # A tibble: 5 x 4
#>   name              x y                 z
#>   <chr>         <dbl> <chr>         <dbl>
#> 1 N/A               1 N/A            -100
#> 2 N A               3 NOt available   -99
#> 3 N / A            NA 29              -98
#> 4 Not Available    NA 25             -101
#> 5 John Smith      -98 28               -1