# Libraries ----

library(tidyverse)
library(anytime)
library(plotly)
library(gganimate)
library(transformr)
library(gifski)
library(png)
library(usa)
library(usmap)

# Get population dataset ----

# Read the population dataset
population_dataset <- readxl::read_xlsx("population_and_density_change_data_table_US_by_state_1910to2020.xlsx") %>% 
  select(Name, `Geography Type`, Year, `Resident Population`, `Resident Population Density`)

# Clean data ----

colnames(population_dataset) <- gsub(" ", "_",colnames(population_dataset))

# Get the list of states and state count for creating ranking
state_list <- population_dataset %>% distinct(Name) %>% pull()
state_count <- length(state_list)

population_dataset_clean <- population_dataset %>% 
  # create a clean date column
  mutate(Date = anydate(str_c("01/01/",Year),"%m/%d/%y")) %>%
  # filter for state list
  filter(Geography_Type == "State") %>%
  # Create a rank column for population and density by year
  # Higher Rank implies higher density relative to other states
  group_by(Year) %>% 
  arrange(Resident_Population_Density) %>% 
  mutate(Rank_Population_Density = row_number()) %>% 
  ungroup() %>% 
  # Create label columns to use in plots    
  mutate(label_state = str_c(Name,"\n",Year)) %>%
  mutate(label_plotly = str_c(Name, "\n", Year, "\nResident Population Density: ", Resident_Population_Density, "\n", "Rank: ", Rank_Population_Density)) 

# Density plots ----

## Plotting function ----
# Select states to plot

state_selected <- c('California')
data <- population_dataset_clean
state_input <- state_selected

# Create a function to plot density against density ranking
plot_density_vs_rank <- function(data, state_input) {
  # Filter data for the states input
  data_filtered <- data %>% 
    filter(Name %in% (state_input))
  
  plot <- data_filtered %>%
    ggplot(aes(x = Rank_Population_Density, y = Resident_Population_Density,  color = Name)) +
    geom_point(aes(text = label_plotly), alpha = 0.5,
               show.legend = FALSE, size = 4) +
    geom_line(aes(x = Rank_Population_Density, y = Resident_Population_Density, color = Name), size = 1, alpha = 0.5)+
    theme_classic() +
    theme(
      text = element_text(family = "mukta"),
      plot.title = element_text(face = "bold",
                                size = 14,
                                hjust = .5),
      plot.subtitle = element_text(size = 12,
                                   hjust = .5),
      plot.caption = element_text(size = 10),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15)
    ) +
    ggrepel::geom_text_repel(aes(x = Rank_Population_Density, y = Resident_Population_Density, label = label_state, color = Name), nudge_x = 0.5, size = 5)  +
    xlim(c(0, state_count)) +
    labs(title = '', 
         caption = "Data: US population by state",
         x = 'Population Density Rank', 
         y = 'Population Density') 
  
  return(plot)    
}

## Plot a single state ----
density_rank_plot <- plot_density_vs_rank(data = population_dataset_clean, state_input = c('California'))

ggplotly(density_rank_plot, tooltip = "text")

## Plot for multiple states ----
density_rank_plot_multistate <- plot_density_vs_rank(data = population_dataset_clean, state_input = c('California', 'Washington', 'Alabama', 'Pennsylvania'))

ggplotly(density_rank_plot_multistate, tooltip = "text")

## Animate the plot over multistate (gif plot output) ----

density_rank_plot_multistate_animated <- density_rank_plot_multistate +
  transition_reveal(Date) + 
  ease_aes('linear')

animate(density_rank_plot_multistate_animated, duration =10, fps = 10, width = 900, height = 600, renderer = gifski_renderer())

## Save the output gif ----
anim_save(file = "density_rank_plot_multistate_animated.gif")

# Map plots ----

## Clean data for map plotting ----

state_dim <- as.data.frame(states) # For getting latitude and longitude

population_dataset_lat_long <- population_dataset_clean %>% 
  # Get population data
  select(name = Name, population = Resident_Population, Year)  %>%
  # Create a label for plotting
  mutate(label_plotly = str_c(name, "\n", Year, "\nPopulation: ", scales::number(population, scale = 1e-3, accuracy = 1, suffix = "K", big.mark = ","))) %>% 
  # Join with dim table to get state longitude and latidude
  left_join(state_dim %>% select(name, state=abb, lat, long)) %>% 
  select(lon = long, lat, population, state, Year,label_plotly) 


## Map plotting function
plot_usa_population_map <- function(data) {
  
  # Transform data for plotting points on map, note that the dataset to be transformed needs to contain "lat" and "lon" columns
  data_transformed <- usmap_transform(data)
  
  # Create the map plot
  plot_map <- plot_usmap(regions = "state", data=(data), values = "population",labels=TRUE)+
    geom_point(data = data_transformed, aes(x = x, y = y, text=label_plotly),
               color = "white", alpha = 0.25, size = 0.001) +
    labs(title = "US population by state",
         # subtitle = 'Year: {closest_state}',
         size = 20) +
    scale_fill_continuous(low = "white", high ="darkblue", 
                          name = "Population",label = scales::comma
    ) + 
    theme(
      legend.position = "right",
      plot.title = element_text(size=20),
      plot.subtitle = element_text(size = 20)
    )
  
  return(plot_map)
}

## Plot map for single year ----
usa_population_map_single_year <- plot_usa_population_map(data = population_dataset_lat_long %>% filter(Year %in% (2020)))

ggplotly(usa_population_map_single_year, tooltip = "text")

## Animate map ----
usa_population_map_multiyear <- plot_usa_population_map(data = population_dataset_lat_long %>% filter(Year %in% c(1910,2020)))

usa_population_map_animated <- usa_population_map_multiyear +
  labs(subtitle = 'Year: {closest_state}') +
  transition_states(Year) + 
  ease_aes('linear')

animate(usa_population_map_animated, duration =2, fps = 10, width = 900, height = 600, renderer = gifski_renderer())


## Save the output gif ----
anim_save(file = "usa_population_map_animated.gif")

