##########################
#################  library
##########################

## clear workspace
rm(list = ls())
gc()

## this function will check if a package is installed, and if not, install it
list.of.packages <- c('magrittr', 'tidyverse', 
                      'sf',
                      'ggplot2', 'ggpattern', 'RColorBrewer', 'showtext',
                      'rworldmap')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

##########################
###################  parts
##########################

## fonts
font_add_google("Quattrocento Sans", "sans-serif")
showtext_auto()

##########################
#################  process
##########################

## world map
world = 
  st_as_sf(cleangeo::clgeo_Clean(getMap())) %>% 
  dplyr::select(NAME, ISO3, continent) %>% 
  rename_all(tolower) %>% 
  st_transform(st_crs('+proj=longlat +datum=WGS84 +no_defs'))

## get grid from get_gridded_patterns_by_gcm.R
grid = 
  st_read('data/data_grid/data_grid.shp',
          crs = st_crs('+proj=longlat +datum=WGS84 +no_defs')) %>%
  rename(grid.id = grid_id)

## get data from get_gridded_patterns_by_gcm.R
data = 
  read_parquet('results/cmip6_gridded_lmsp.parquet') %>% 
  group_by(grid.id) %>% 
  summarize(pattern.mean    = mean(pattern, na.rm = T),
            pattern.sd      = sd(pattern, na.rm = T),
            lmsp            = mean(lmsp, na.rm = T),
            sd.to.mean      = abs(pattern.sd/pattern.mean),
            model.agreement = case_when(sd.to.mean >= 7 ~ 'Low Model Agreement',
                                        T ~ 'High Model Agreement'),
            value           = case_when(model.agreement == 'Low Model Agreement' ~ NA_real_,
                                        T ~ pattern.mean))

## add to grid
plot.data =  
  left_join(grid, data) 

## plot without model disagreement
ggplot() +
  geom_sf(data = plot.data,
          aes(fill = precip.mean),
          color = NA) + 
  geom_sf(data = world,
          fill = NA) +
  scale_color_distiller(palette  = 'BrBG',
                        direction = +1,
                        na.value = 'white',
                        breaks   = scales::pretty_breaks(n = 6),
                        guide    = guide_colorbar(title.position = 'bottom'),
                        limits   = c(-2, 2),
                        values   = scales::rescale(c(-2, -0.5, 0, 0.5, 2))) +
  scale_fill_distiller(palette  = 'BrBG',
                       direction = +1,
                       na.value = 'white',
                       breaks   = scales::pretty_breaks(n = 6),
                       guide    = guide_colorbar(title.position = 'bottom'),
                       limits   = c(-2, 2),
                       values   = scales::rescale(c(-2, -0.25, 0, 0.25, 2))) +
  labs(fill    = 'Change in Precipitation (mm/day)') +
  theme_void() +
  theme(legend.position    = 'bottom',
        legend.key.width   = unit(2.5, 'cm'),
        legend.title.align = 0.5,
        text               = element_text(family = 'sans-serif'))

## export
ggsave('tsd/code/precipitation/figures/precipitation_changes_2100_without_disagreement.svg', width = 9, height = 4)

## plot with model disagreement a different color
ggplot() +
  geom_sf(data = plot.data,
          aes(fill = value),
          color = NA) + 
  geom_sf(data = world,
          fill = NA) +
  scale_color_distiller(palette  = 'BrBG',
                        direction = +1,
                        na.value = 'plum3',
                        breaks   = scales::pretty_breaks(n = 6),
                        guide    = guide_colorbar(title.position = 'bottom'),
                        limits   = c(-2, 2),
                        values   = scales::rescale(c(-2, -0.5, 0, 0.5, 2))) +
  scale_fill_distiller(palette  = 'BrBG',
                       direction = +1,
                       na.value = 'plum3',
                       breaks   = scales::pretty_breaks(n = 6),
                       guide    = guide_colorbar(title.position = 'bottom'),
                       limits   = c(-2, 2),
                       values   = scales::rescale(c(-2, -0.25, 0, 0.25, 2))) +
  labs(fill    = 'Change in Precipitation (mm/day)') +
  theme_void() +
  theme(legend.position    = 'bottom',
        legend.key.width   = unit(2.5, 'cm'),
        legend.title.align = 0.5,
        text               = element_text(family = 'sans-serif'))

## export
ggsave('tsd/code/precipitation/figures/precipitation_changes_2100_with_disagreement.svg', width = 9, height = 4)

## plot with model disagreement in patterns. CAREFUL, THIS TAKES 8 HOURS TO RENDER! 
p = 
  ggplot() +
  geom_sf_pattern(data = plot.data,
                  aes(fill    = value,
                      pattern = model.agreement),
                  color = NA,
                  # pattern_density = 0.01,
                  pattern_spacing = 0.01,
                  pattern_fill    = 'lightcoral') +
  geom_sf(data = world,
          fill = NA) +
  scale_color_distiller(palette  = 'BrBG',
                        direction = +1,
                        na.value = 'white',
                        breaks   = scales::pretty_breaks(n = 6),
                        guide    = guide_colorbar(title.position = 'bottom'),
                        limits   = c(-2, 2),
                        values   = scales::rescale(c(-2, -0.5, 0, 0.5, 2))) +
  scale_fill_distiller(palette  = 'BrBG',
                       direction = +1,
                       na.value = 'white',
                       breaks   = scales::pretty_breaks(n = 6),
                       guide    = guide_colorbar(title.position = 'bottom'),
                       limits   = c(-2, 2),
                       values   = scales::rescale(c(-2, -0.25, 0, 0.25, 2))) +
  scale_pattern_manual(values = c('High Model Agreement' = 'none', 
                                  'Low Model Agreement' = 'stripe')) +
  labs(fill    = 'Change in Precipitation (mm/day)') +
  theme_void() +
  theme(legend.position    = 'bottom',
        legend.key.width   = unit(2.5, 'cm'),
        legend.title.align = 0.5,
        text               = element_text(family = 'sans-serif')) +
  guides(pattern = 'none')

## export
ggsave('tsd/code/precipitation/figures/precipitation_changes_2100_with_disagreement_hatched.svg', 
       plot   = p, 
       width  = 9, 
       height = 4)







############### standard deviations
## plot model disagreement metric
ggplot() +
  geom_sf(data = plot.data,
          aes(fill = sd.to.mean),
          color = NA,
          alpha = 0.3) + 
  geom_sf(data = world,
          fill = NA) +
  scale_color_distiller(palette  = 'PiYG',
                        direction = -1,
                        na.value = 'white',
                        breaks   = scales::pretty_breaks(n = 6),
                        guide    = guide_colorbar(title.position = 'bottom'),
                        limits   = c(0, 16),
                        values   = scales::rescale(c(0, 8, 16))) +
  scale_fill_distiller(palette  = 'PiYG',
                       direction = -1,
                       na.value = 'white',
                       breaks   = scales::pretty_breaks(n = 6),
                       guide    = guide_colorbar(title.position = 'bottom'),
                       limits   = c(0, 16),
                       values   = scales::rescale(c(0, 8, 16))) +
  labs(fill    = 'Ratio of Std. Dev. to Mean Change in Precipitation') +
  theme_void() +
  theme(legend.position    = 'bottom',
        legend.key.width   = unit(2.5, 'cm'),
        legend.title.align = 0.5,
        text               = element_text(family = 'sans-serif'))

## export
ggsave('tsd/code/precipitation/figures/precipitation_changes_2100_without_disagreement_sd.svg', width = 9, height = 4)

## end of script. have a great day!