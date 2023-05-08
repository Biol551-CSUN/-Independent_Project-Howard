
# Load Packages
library(tidyverse)
library(here)
library(ggplot2)
library(crosstable)
library(dplyr)
library(vtable)
library(gganimate)
library(hrbrthemes)
library(viridis)

#Load in Data
pitcher_plant_data <- read.csv(here("Data/pitcher_plant_data.csv"))
View(pitcher_plant_data)

# 80 leaves total, 10 leaves for
# There are A - H Plant Ids
list_of_pp_ID <- pitcher_plant_data %>%
  select(Plant.ID.Alpha) %>%
  unique()
view(list_of_pp_ID)

# It might be best to go by plant id numeric. Let's look into this

list_of_pp_numeric <- pitcher_plant_data %>%
  select(Plant.ID.Numeric) %>%
  unique() 
view(list_of_pp_numeric) # There are 54 pitcher plants to look at (seen when adding tally())

pitcher_plant_data %>% 
  mutate_if(is.character, as.numeric)



# I am interested in the abundances of rotifers, protozoa, and nematodes. I would
# like to create one output that shows this in pitcher plants to see the general diversity. Just in general,
# We have some categories that we need to be numeric. Like Rotifers, Bodo
sum <- pitcher_plant_data %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) # Let's get a graph showing the sum of organisms
view(sum)




# I would also like to show how the age of the leaf and see if the abundances of those organisms change
# due to the age of the leaf.



# We will also see if the age of the leaf plays a part in the number of dead ants seen. Which
# is the main insect that you see you.


#You can create a time series for one of the specific plants and the nu

Plant.ID.A <- pitcher_plant_data %>%
  select(Plant.ID.Alpha, leaf_age.days.,ants)

# Here overtime, you can see that the number of dead ants, decreases as the age of the leaf decreases
Plant.ID.A %>%
  ggplot(aes(x = leaf_age.days.,
             y = ants)) +
  geom_point(stat = "identity") + 
  labs(title = " Insert Title ",
       x = 'Leaf Age (Days)',
       y = 'Number of Dead Ants') +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) 


Plant.ID.A %>%
  ggplot(aes(x = leaf_age.days.,
             y = ants)) +
  geom_point(shape=21, color="black", fill='yellow', size=2) +
  labs(title = " Insert Title ",
       x = 'Leaf Age (Days)',
       y = 'Number of Dead Ants') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

pitcher_plant_data$Plant.ID.Alpha


pitcher_plant_data %>%
  ggplot( aes(x= leaf_age.days., y= ants, group= Plant.ID.Alpha, color=Plant.ID.Alpha)) +
  geom_line() +
  geom_point() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Insert Title") +
  theme_ipsum() +
  ylab("Number of babies born") +
  transition_reveal(leaf_age.days)

# create a time series graph of different plant ids and the numbers of protos. found inside in the
# pitcher plant over time. This graph is meant to show how the age of the leaf affects the number
# of protozoans found inside of the pitcher plant which might indicate that over time, the leaves 
# ability to consume resources decreases which leads to the demise of the plant. 
