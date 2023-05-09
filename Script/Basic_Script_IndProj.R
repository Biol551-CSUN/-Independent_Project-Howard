
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
library(ggridges)
library(rcartocolor)
#Load in Data
data <- read.csv(here("Data/pitcher_plant_data.csv"))
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
  select(Plant.ID.Alpha, leaf_age_,ants)

# Here overtime, you can see that the number of dead ants, decreases as the age of the leaf decreases
Plant.ID.A %>%
  ggplot(aes(x = leaf_age_,
             y = ants)) +
  geom_point(stat = "identity") + 
  labs(title = " Insert Title ",
       x = 'Leaf Age (Days)',
       y = 'Number of Dead Ants') +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) 


Plant.ID.A %>%
  ggplot(aes(x = leaf_age_,
             y = ants)) +
  geom_point(shape=21, color="black", fill='yellow', size=2) +
  labs(title = " Insert Title ",
       x = 'Leaf Age (Days)',
       y = 'Number of Dead Ants') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

pitcher_plant_data$Plant.ID.Alpha


pitcher_plant_data %>%
  ggplot( aes(x= leaf_age_, y= ants, group= Plant.ID.Alpha, color=Plant.ID.Alpha)) +
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


# Convert the Date column to Date format
pitcher_plant_data$Date <- as.Date(pitcher_plant_data$Date, format = "%m/%d/%Y")

#Convert columns to numeric
pitcher_plant_data$Mites <- as.numeric(pitcher_plant_data$Mites)

### rotifers and after are characters, change to numeric

# Sum the number of each organism per sample
pitcher_plant_data$Total_organisms <- rowSums(pitcher_plant_data[,c("ants","spiders","beetle","Mosquito","Midges","Snot","Mites")])

print(Total_organisms)
# Create a time series plot of total organisms over time
# ggplot(pitcher_plant_data, aes(x=leaf_age_, y=Total_organisms)) + 
#   geom_line() +
#   labs(title = "Total Organisms Over Time in Plant A", x = "Age of Leaf", y = "Total Organisms")

# 

# ggplot(pitcher_plant_data, aes(x = 
#                                y = )) +
#   geom_jitter(aes(color = species),
#             alpha = 0.4,
#             width = 0.1
#             height = 0
#             size = 3,
#             show.legend = FALSE) +
#   scale_color_brewer(palette = "Dark2") + 
#   theme_light()


  # Create a ridgeline of different plant ids and the densit over the time of the leaf

  # create a subset of the data with only the plant abundance columns
  
  abundance_data <- data[, c(2:7)]
view(abundance_data)
# convert the data to a long format
abundance_data_long <- tidyr::gather(abundance_data, "leaf_age_", "abundance", -Plant.ID.Numeric)
view(abundance_data_long)

# plot the ridgeline density plot
ggplot(abundance_data_long, aes(x = leaf_age_,
                                y = Plant.ID.Numeric, 
                                height = abundance, 
                                fill = Plant.ID.Numeric)) +
  geom_density_ridges_gradient(scale = 3, 
                               rel_min_height = 0.01) +
  scale_fill_viridis_d(option = "A", 
                       begin = 0.1, 
                       end = 0.9) +
  labs(x = "Year", y = "Plant Numeric ID", fill = "Plant Numeric ID") +
  theme_minimal()

my_pal <- rcartocolor::carto_pal(n = 8, name = "Bold")[c(1, 3, 7, 2,4,5,6,8)]

g_ridges <- 
  ggplot(data, aes(leaf_age_, fct_rev(Plant.ID.Alpha), color = Plant.ID.Alpha, fill = spiders)) + 
  coord_cartesian(clip = "off") +
  scale_y_discrete(expand = c(.07, .07)) +
  scale_color_manual(values = my_pal, guide = "none") +
  scale_fill_manual(values = my_pal, guide = "none") +
  labs(x = "Leaf Age (Days)",
       y = "Plant ID Alpha") + 
  ggtitle("Organism Distribution Over Time") +
  theme_minimal()

g_ridges +
  ggridges::geom_density_ridges(
    alpha = .7, size = 1.5
  )
