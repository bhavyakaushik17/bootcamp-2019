library(tidyverse)
library(ggplot2)
source(here::here("data/day3_objects.R"))

ggplot(data = gapminder07) + 
    geom_point(mapping = aes(x = gdpPercap, y = lifeExp)) +
    labs(title = "Relationship between life expectancy and GDP per capita in 2007", 
        x = "GDP per capita", y = "Life expectancy")

ggplot(data = gapminder07) + 
    geom_point(mapping = aes(x = log(pop), y = log(gdpPercap))) +
    labs(title = "Relationship between log of GDP per capita and log of life expectancy in 2007", 
         x = "log of GDP per capita", y = "log of life expectancy") + theme_bw()

long_gen %>% 
    group_by(datetime) %>% 
    summarise(output=sum(output)) %>% 
    ggplot(.) + 
    geom_col(aes(x=datetime, y=output)) + 
    labs(title="Total energy generated, by hour", x="Hour", y="Output (MW)")

long_gen %>%
    filter(source == 'large_hydro' | source == 'small_hydro') %>% 
    group_by(datetime) %>% 
    summarise(output=sum(output)) %>% 
    ggplot(.) + 
    geom_col(aes(x=datetime, y=output)) + 
    labs(title="Total hydroelectric power generated, by hour", x="Hour", y="Output (MW)")

# can also use this inside filter: source %in% C("large_hydro", "small_hydro") or filter(str_detect(source,"hydro"))

generation %>% 
    mutate(hydro=large_hydro+small_hydro) %>%
    ggplot() + 
    geom_col(aes(x=datetime, y=hydro)) + 
    labs(title="Total hydro power generated, by hour", x="Hour", y="Output (MW)")

imports %>%
    ggplot() + 
    geom_line(aes(x=datetime, y=imports)) + 
    labs(title="Energy imports over time", x="Hour", y="Amount imported (MW)")

imports %>%
    ggplot() + 
    geom_line(aes(x=datetime, y=imports), size=1.2, col="red") + 
    labs(title="Energy imports over time", x="Hour", y="Amount imported (MW)")

generation %>% 
    ggplot() + 
    geom_area(aes(x=datetime, y=wind), fill="darkblue") + 
    labs(title="Hourly wind power generation, Sept 3-9", x="Hour", y="Output (MW)")

long_gen %>% 
    ggplot() + 
    geom_boxplot(aes(x=source, y=output)) + 
    labs(title="Amount of energy generated by each source, Sept 3-9", x="Source type", y="Output (MW)")

generation %>% 
    ggplot() + 
    geom_line(aes(x=datetime, y=large_hydro), col="turquoise3") + 
    geom_smooth(aes(x=datetime, y=large_hydro)) + 
    labs(title="Hydroelectric (large) generation per hour, Sept 3-9", x="Hour", y="Output (MW)")

long_gen %>% 
    group_by(source) %>%
    summarise(output=sum(output)) %>% 
    ggplot() + 
    geom_col(aes(x=source, y=output),  fill="darkred") + 
    geom_hline(aes(yintercept = mean(output))) +
    geom_hline(aes(yintercept = mean(output), col = "darkblue"), show.legend = TRUE) +
    geom_hline(aes(yintercept = median(output), col = "darkgreen"), show.legend = TRUE) +
    labs(title="Total hydro power generated, by hour", x="Source", y="Total Output (MW)")
imports %>%
    ggplot() + 
    geom_line(aes(x=datetime, y=imports), col="red") + 
    labs(title="Energy imports over time in California", subtitle="Hourly data from September 3-9, 2018",
         caption="Source: California Energy Commission", x="Hour", y="Amount imported (MW)")

imports %>%
    ggplot() + 
    geom_line(aes(x=datetime, y=imports), size=1.2, col="red") + 
    labs(title="Energy imports over time", x="Hour", y="Amount imported (MW)") + 
    scale_x_datetime(date_labels = ,date_breaks = )

imports %>%
    ggplot() + 
    geom_line(aes(x=datetime, y=imports), col="red") + 
    scale_x_datetime(date_labels="%H:%M", date_breaks="12 hours") +
    labs(title="Energy imports over time in California", subtitle="Hourly data from September 3-9, 2018", x="Hour", y="Amount imported (MW)")

imports %>%
    ggplot() + geom_line(aes(x=datetime, y=imports), col="red") + 
    scale_x_datetime(date_labels="%H:%M", date_breaks="12 hours") +
    labs(title="Energy imports over time in California", subtitle="Hourly data from September 3-9, 2018", x="Hour", y="Amount imported (MW)") + 
    theme(axis.text.x=element_text(angle=45, hjust=1, size=12))

long_gen %>% 
    mutate(date=lubridate::date(datetime)) %>% 
    group_by(date) %>% summarise(output=sum(output)) %>% 
    ggplot() + geom_col(aes(x=date, y=output)) + 
    labs(title="Total energy generated, by day", x="Day", y="Output (MW)")

long_gen %>% 
    mutate(date=lubridate::date(datetime)) %>% 
    group_by(date) %>% summarise(output=sum(output)) %>% 
    ggplot() + geom_col(aes(x=date, y=output)) + 
    labs(title="Total energy generated, by day", x="Day", y="Output (MW)") + 
    coord_flip()

long_merged_energy %>% 
    ggplot() + 
    geom_line(aes(x=datetime, y=output, group = source, col = source)) + 
    labs(title = "energy output over time")

long_merged_energy %>% 
    ggplot() + 
    geom_line(aes(x=datetime, y=output, group = source, col = source)) + 
    labs(title = "energy output over time")

long_merged_energy %>% 
    filter(source == "wind" | source == "solar" | source == "geothermal") %>%
    ggplot() + 
    geom_line(aes(x=datetime, y=output, group = source, col = source), size=1.5) + 
    labs(title = "energy output over time")