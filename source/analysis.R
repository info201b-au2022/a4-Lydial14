library(tidyverse)
library(ggplot2)

# The functions might be useful for A4
source("../source/a4-helpers.R")
incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

## Section 2  ---- 
#----------------------------------------------------------------------------#
# prop_male_jail_2018
# prop_male_jail_1970
# prop_female_jail_2018
# prop_female_jail_1970
# avg_white_jail_2018
# prop_white_jail_2018
# avg_black_jail_2018
# prop_black_jail_2018
#----------------------------------------------------------------------------#
#The average proportion of males in jail 2018
prop_male_jail_2018 <- incarceration %>%
  filter(year == 2018) %>%
  summarize(avg_male_2018 = mean(male_jail_pop/total_jail_pop, na.rm = TRUE)) %>%
  pull(avg_male_2018)

#The average proportion of males in jail 1970
prop_male_jail_1970 <- incarceration %>%
  filter(year == 1970) %>%
  summarize(avg_male_1970 = mean(male_jail_pop/total_jail_pop, na.rm = TRUE)) %>%
  pull(avg_male_1970)

#The average proportion of females in jail 2018
prop_female_jail_2018 <- incarceration %>%
  filter(year == 2018) %>%
  summarize(avg_female_2018 = mean(female_jail_pop/total_jail_pop, na.rm = TRUE)) %>%
  pull(avg_female_2018)

#The average proportion of females in jail 1970
prop_female_jail_1970 <- incarceration %>%
  filter(year == 1970) %>%
  summarize(avg_female_1970 = mean(female_jail_pop/total_jail_pop, na.rm = TRUE)) %>%
  pull(avg_female_1970)

#The average amount of White incarceration in 2018
avg_white_jail_2018 <- incarceration %>%
  filter(year == 2018) %>%
  summarize(avg_white_2018 = round(mean(white_jail_pop, na.rm = TRUE))) %>%
  pull(avg_white_2018)

#The average proportion of White incarceration in 2018
prop_white_jail_2018 <- incarceration %>%
  filter(year == 2018) %>%
  summarize(prop_white_2018 = mean(white_jail_pop/total_jail_pop, na.rm = TRUE)) %>%
  pull(prop_white_2018)

#The average amount of Black incarceration in 2018
avg_black_jail_2018 <- incarceration %>%
  filter(year == 2018) %>%
  summarize(avg_black_2018 = round(mean(black_jail_pop, na.rm = TRUE))) %>%
  pull(avg_black_2018)

#The average proportion of Black incarceration in 2018
prop_black_jail_2018 <- incarceration %>%
  filter(year == 2018) %>%
  summarize(prop_black_2018 = mean(black_jail_pop/total_jail_pop, na.rm = TRUE)) %>%
  pull(prop_black_2018)


## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# get_year_jail_pop 
# plot_jail_pop_for_us 
#----------------------------------------------------------------------------#
# This function gets the total population of prisoners each year from 1970 to 2018
get_year_jail_pop <- function() {
  total_pop_yearly <- incarceration %>%
    group_by(year) %>%
    summarise(pop_yearly = sum(total_jail_pop, na.rm = TRUE))
return(total_pop_yearly)   
}

# This function plots the total population of prisoners from years 1970 to 2018 into a bar chart
plot_jail_pop_for_us <- function()  {
  plot_jail_pop <- ggplot(data = get_year_jail_pop()) +
    geom_col(mapping = aes(x = year, y = pop_yearly)) +
    labs(title = "Growth of USA Prison Population from 1970-2018",
         x = "Year", 
         y = "Number of Prisoners") +
    scale_y_continuous(labels = scales::comma) 
  return(plot_jail_pop)   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# get_jail_pop_by_states()
# plot_jail_pop_by_states
# See Canvas
#----------------------------------------------------------------------------#
# This function gets the total jail population from 1970-2018 for a specified state
get_jail_pop_by_states <- function(states) {
  state_pop <- incarceration %>%
    filter(state %in% states) %>%
    group_by(year, state) %>%
    summarize(state = sum(total_jail_pop, na.rm = TRUE))
  return(state_pop)
}

#This function plots the above function in a line graph that represents how the prison population changes in a particular state throughout the years.
plot_jail_pop_by_states <- function(states) {
  plot_state_pop <- ggplot(data = get_jail_pop_by_states(states)) +
    geom_line(mapping = aes(x = year, y = state, color = state)) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Growth of Prison Population by State from 1970-2018",
         x = "Year",
         y = "Number of Prisoners") 
  return(plot_state_pop)
}
  
         
## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# get_pattern_of_inequality
# inequality_chart
# See Canvas
#----------------------------------------------------------------------------#
# This function creates a dataframe of the proportion of incarcerated black people and the proportion of black population
get_patten_of_inequality <- function(input_year) {
  pattern_of_inequality <- incarceration %>%
    filter(year == input_year) %>%
    group_by(state) %>%
    mutate(prop_black_jail = (black_jail_pop / total_jail_pop) * 100) %>% 
    mutate(prop_black_15_64 = (black_pop_15to64 / total_pop_15to64) * 100) %>% 
    filter(prop_black_jail <= 100) %>% 
    select(year, state, county_name, total_jail_pop, total_pop_15to64, prop_black_jail, prop_black_15_64)
  return(pattern_of_inequality)
}

# This function plots the population proportion of black people from the ages of 15 to 64 as the indepent variable, and the jail proportion of black people on the y-axis in a specified year. 
inequality_chart <- function(input_year) {
  plot_inequality <- ggplot(get_patten_of_inequality(input_year)) +
    geom_point(mapping = aes(x = prop_black_15_64, y = prop_black_jail, color = state)) +
    labs(title = "Percentage of Black Population Compared to Black Prison Percentage",
         x = "Black Population Percentage",
         y = "Percent of Black Jail Residents")
  return(plot_inequality)
}
  
## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# get_map_of_inequality
# state_map
# See Canvas
#----------------------------------------------------------------------------#
library("mapproj")

#This function maps the proportion of black people compared to the population proportion of black people across the USA
get_map_of_inequality <- function() {
  inequality_map <- incarceration %>%
    filter(year == 2018) %>%
    mutate(prop_black_jail = (black_jail_pop / total_jail_pop) * 100) %>% 
    filter(prop_black_jail <= 100) %>% 
    select(state, prop_black_jail) %>%
    group_by(state) %>%
    mutate(state = state.name[match(state,state.abb)]) %>% 
    mutate(state = tolower(word(state))) %>%
    na.omit() %>%
    summarize(mean_black_prop = mean(prop_black_jail)) 
  return(inequality_map)
}

state_map <- map_data("state") %>%
  rename(state = region) %>%
  left_join(get_map_of_inequality(), by="state")

#This function creates a map of the USA that depicts the average percent of black incarceration across a state. 
plot_map_of_inequality <- function() {
  plot_map <- ggplot(state_map) + 
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = mean_black_prop),
      color = "white", size = .1) +
    scale_fill_continuous(low = "White", high = "Red") +
    labs(
      title = "Average Percent of State Jail Population That is Black",
      fill = "Average Percent of Black Incarceration") 
  return(plot_map)
}

plot_map_of_inequality()
## Load data frame ---- 


