#### Install Packages ----
pkgs <- c('censobr', 'geobr', 'arrow', 'dplyr', 'ggplot2', 'quantreg', 'sf')

# install.packages(pkgs)
renv::restore() # Must install renv

library(censobr)
library(geobr)
library(arrow)
library(dplyr)
library(ggplot2)
library(quantreg)
library(sf)

# Options
options(arrow.unsafe_metadata = TRUE)

#### Weighting Areas ----
municipio <- '3509502' # Campinas (use code_weighting = "all" for Brazil-wide data)
ap <- geobr::read_weighting_area(code_weighting = '3509502', year = 2010, simplified = F, showProgress = T)

# Map
ggplot(data = ap) +
  geom_sf(aes(geometry = geom), color = "black") +
  theme_minimal() +
  labs(
    title = "Map of Weighting Areas",
    caption = "Data Source: IBGE"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "bottom"
  )

#### Microdata ----
# Download household data
domicilios <- read_households(
  year = 2010,                                  
  columns = NULL,       
  add_labels = NULL,
  as_data_frame = T, # Use F if memory is less than 16GB to save and work only in cache
  showProgress = T,  
  cache = T)

# Number of households connected to the sewage network
esgoto <- domicilios |> 
  compute() |>
  group_by(name_region, code_weighting) |>
  summarize(
    rede = sum(V0010[which(V0207 == '1')]), # Sum V0010 where V0207 is '1'
    total = sum(V0010)) |>                  # Total sum of V0010
  mutate(
    cobertura = rede / total,               # Coverage proportion
    code_weighting = as.character(code_weighting)) |>
  collect()

# Average rent value in the weighting area
aluguel <- domicilios |>
  compute() |>
  group_by(code_weighting) |>
  summarize(avgrent = weighted.mean(x = V2011, w = V0010, na.rm = TRUE)) |>
  collect()

#### Joining Microdata and Geobr ----
esgoto_sf <- dplyr::left_join(ap, esgoto, by = "code_weighting")
aluguel_sf <- dplyr::left_join(ap, aluguel, by = "code_weighting")

# Sewage Map
ggplot() +
  geom_sf(data = esgoto_sf, aes(fill = cobertura), color = NA) +
  geom_sf(data = ap, color = 'gray20', fill = NA) +
  labs(title = "Proportion of Households Connected to Sewage Network") +
  scale_fill_distiller(palette = "Greens", direction = 1, 
                       name = 'Proportion of\nhouseholds', 
                       labels = scales::percent) +
  theme_void() +
  theme(legend.position = 'bottom')

# Rent Map
ggplot() +
  geom_sf(data = aluguel_sf, aes(fill = avgrent), color = NA) +
  geom_sf(data = ap, color = 'gray20', fill = NA) +
  labs(title = "Average Rent by Weighting Area",
       subtitle = "Campinas, 2010") +
  scale_fill_distiller(palette = "Purples", direction = 1, 
                       name = 'Values\nin R$',
                       labels = scales::number_format(big.mark = ".")) +
  theme_void()
