# install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
#                    "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
# install.packages('rgeos')
library(rgeos)
library(tidyverse)
theme_set(theme_bw())
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale='medium',returnclass = 'sf')
class(world)
region_levels <- c('Celtic English', 'European', 'East Asian', 'Hispanic', 'South Asian', 'Arabic', 'Israeli', 'African')

# our_country_map <- read_tsv('https://raw.githubusercontent.com/greenelab/wiki-nationality-estimate/a92a37284ef5a097c3f499fef782fd20f40e050c/data/country_to_region.tsv') %>% 
#   rename('region' = Region) %>% 
#   recode_region_letter()

our_country_map <- read_tsv('https://raw.githubusercontent.com/greenelab/wiki-nationality-estimate/master/data/country_to_region.tsv') %>% 
  rename('region' = Region) %>% 
  recode_region()

my_world <- world %>% 
  rename(Country = 'name') %>% 
  left_join(our_country_map, by = 'Country')

(gworld <- ggplot(data = my_world) +
    geom_sf(aes(fill = fct_relevel(region, region_levels))) +
    # scale_fill_viridis_d(option = "plasma") +
    scale_fill_manual(values = c('#ffffb3', '#fccde5', '#b3de69', '#fdb462', '#80b1d3', '#8dd3c7', '#bebada', '#fb8072')) +
    theme(panel.background = element_rect(fill = "azure"),
          legend.title = element_blank(),
          panel.border = element_rect(fill = NA)))

ggsave('figs/2020-01-31_groupings.png', gworld)

world %>% 
  select(iso_a2, iso_a3, name, name_long, region_wb) %>% 
  write_tsv('data/countries/world-map.tsv')

my_world %>% 
  as_tibble() %>% 
  select(Country, region) %>% 
  write_tsv('data/countries/2020-01-31_groupings.tsv')

(gworld <- ggplot(data = world) +
  geom_sf(aes(fill = region_wb)) +
  # geom_rect(xmin = -102.15, xmax = -74.12, ymin = 7.65, ymax = 33.97, 
  #           fill = NA, colour = "black", size = 1.5) +
  scale_fill_viridis_d(option = "plasma") +
  theme(panel.background = element_rect(fill = "azure"),
        panel.border = element_rect(fill = NA),
        legend.title = element_blank()))

