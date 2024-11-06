library(tidyverse)
library(palmerpenguins)
library(janitor)
library(here)
library(ggplot2)
library(dplyr)
library(tidyr)

source(here("functions","cleaning.R"))

#load raw data
penguins_raw <-read.csv(here("data","penguins_raw.csv"))

cleaning_penguins_columns <- function(raw_data){
  print("Cleaned names, removed comments,remove empty rows and cols,removed delta")
  raw_data %>%
    clean_names()%>%
    shorten_species()%>%
    remove_empty(c("rows","cols"))%>%
    select(-comments)
  select(-starts_with("delta"))
}

colnames(penguins_raw)
penguins_clean <- cleaning_penguins_columns(penguins_raw)
colnames(penguins_clean)


shorten_species(penguins_clean)
head(penguins_clean)
view(penguins_clean$species)

#initialize renv
install.packages("renv")

renv::init()
renv::snapshot()

#uses the new folder to install all the right libraries
renv::restore

penguins_clean <- read.csv(here("data","penguins_clean.csv"))
flipper_boxplot <- ggplot(
  data=penguins_clean,
  aes(x=species,
      y=flipper_length_mm))+
  geom_boxplot()
flipper_boxplot
#NA removed (non-finite)
penguins_flippers <-select (penguins_clean,c("species","flipper_length_mm"))
colnames(penguins_flippers)

penguins_flippers <-drop_na(penguins_flippers)
penguins_flippers

#Bad code above (overwritten), good code below
penguins_flippers <- penguins_clean %>%
  select(species,flipper_length_mm) %>%
  drop_na()

penguins_flippers

library(ggbeeswarm)
#try boxplot again; try penguins_flippers not penguins_clean
species_colours <- c("Adelie Penguin (Pygoscelis adeliae)"="darkorange",
                     "Chinstrap penguin (Pygoscelis antarctica)"="purple",
                     "Gentoo penguin (Pygoscelis papua)"="cyan4")
flipper_boxplot <- ggplot(
  data = penguins_flippers,
  aes(x = species, y = flipper_length_mm)
) +
  geom_boxplot(
    aes(color = species),
    show.legend = FALSE
  ) +
  geom_jitter(
    aes(color = species),
    alpha = 0.3,
    show.legend = FALSE,
    position = position_jitter(
      width = 0.2,
      seed = 0
    )
  ) +
  scale_color_manual(values = species_colours)+
  labs(
    x = "Penguin species",
    y = "Flipper length (mm)"
  )+
  theme_bw()


flipper_boxplot

#seed -> deal with randomness
#alpha -> transparency


plot_boxplot <-function(data,
                        x_column,
                        y_column,
                        x_label,
                        y_label,
                        colour_mapping){
  data<-data%>%
    drop_na({{y_column}})
  
  #Now we make the plot
  flipper_boxplot <- ggplot(
    data = data,
    aes(x = {{x_column}}, y = {{y_column}})
  ) +
    geom_boxplot(
      aes(color = {{x_column}}),
      show.legend = FALSE
    ) +
    geom_jitter(size=1,
                alpha = 0.3,
                show.legend = FALSE,
                position = position_jitter(
                  width = 0.2,
                  seed = 0
                )
    ) +
    scale_color_manual(values = species_colours)+
    labs(
      x = x_label,
      y = y_label
    )+
    theme_bw()
}
species_colours <- c("Adelie Penguin (Pygoscelis adeliae)"="darkorange",
                     "Chinstrap penguin (Pygoscelis antarctica)"="purple",
                     "Gentoo penguin (Pygoscelis papua)"="cyan4")

plot_boxplot <- function(data,
                         x_column,
                         y_column,
                         x_label,
                         y_label,
                         colour_mapping) {
  # Ensure dplyr is loaded for drop_na and tidy evaluation
  library(dplyr)
  # Drop rows with NA values in the specified y_column
  data <- data %>%
    drop_na({{ y_column }})
  
  # Now we make the plot
  flipper_boxplot <- ggplot(
    data = data,
    aes(x = {{ x_column }}, y = {{ y_column }})
  ) +
    geom_boxplot(
      aes(color = {{ x_column }}),
      show.legend = FALSE
    ) +
    geom_jitter(
      size = 1,
      alpha = 0.3,
      show.legend = FALSE,
      position = position_jitter(
        width = 0.2,
        seed = 0
      )
    ) +
    scale_color_manual(values = colour_mapping) +  # Use the passed color mapping
    labs(
      x = x_label,
      y = y_label
    ) +
    theme_bw()
  
  # Return the plot object
  return(flipper_boxplot)
}

