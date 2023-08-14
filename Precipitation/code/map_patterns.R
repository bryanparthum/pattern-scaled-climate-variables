##########################
#################  library
##########################

## clear workspace
rm(list = ls())
gc()

## this function will check if a package is installed, and if not, install it
list.of.packages <- c('magrittr', 'tidyverse', 
                      'stringi',
                      'sf',
                      'arrow',
                      'ggplot2', 'ggpattern', 'showtext',
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

## function to read in patterns from generate_changes_in_precip_annual.R
read_patterns <- function(x) {
  
  ## get filename to use for export
  filename = str_remove(basename(x), '.parquet')
  
  ## read file and make simple feature
  read_parquet(x) %>%
    mutate(esm = stri_split(filename, fixed='_')[[1]][1]) 
}

##########################
#################  process
##########################

## path to files
path = 'tsd/code/precipitation/output/patterns/annual/'

## read and stack patterns from each ESM
data = 
  list.files(path, pattern = '.parquet', full.names = T) %>% 
  map_df(~read_patterns(.))

## world map
world = 
  st_as_sf(cleangeo::clgeo_Clean(getMap()[-which(getMap()$ADMIN=='Antarctica'),])) %>% 
  dplyr::select(NAME,ISO3,continent) %>% 
  rename_all(tolower) %>% 
  st_transform("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

## make grid and intersect with world polygons
grid =
  stars::st_as_stars(st_bbox(world), dx = 75*1.609344*1e3, dy = 75*1.609344*1e3) %>%   ## in meters, miles * 1.609344 * 1e3 to get miles to meters
  st_as_sf() %>%
  # st_transform("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>% 
  # st_intersection(world) %>% 
  mutate(grid.id = seq(n()))

## make data simple features
data %<>% 
  st_as_sf(coords = c('lon', 'lat'),
           crs    = 4326) %>% 
  # st_transform("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  st_transform(st_crs(grid))

# ## check plot, looks good
# data %>%
#   filter(esm == 'ACCESS-CM2',
#          year == 2100) %>%
#   ggplot +
#   geom_sf(aes(color = change_in_precip,
#               geometry = geometry)) +
#   geom_sf(data = world,
#           fill = NA,
#           alpha = 0.1) +
#   labs(title = 'ACCESS-CM2',
#        color = '  Slope \n(mm/day)')

## map grid.id's to data
data %<>% 
  st_intersection(grid)

## summarise grid values
data.summary =
  data %>% 
  st_drop_geometry() %>% 
  group_by(grid.id, year) %>% 
  summarise(precip.mean = mean(change_in_precip, na.rm = T),
            precip.sd   = sd(change_in_precip, na.rm = T))

## get model agreement
data.summary %<>% 
  mutate(sd.to.mean      = abs(precip.sd/precip.mean),
         model.agreement = case_when(sd.to.mean >= 8 ~ 'Low Model Agreement',
                                    T ~ 'High Model Agreement'),
         value           = case_when(model.agreement == 'Low Model Agreement' ~ NA_real_,
                                   T ~ precip.mean))

## map data summary back to spatial grid
plot.data = 
  left_join(grid,
            data.summary,
            by = 'grid.id') %>% 
  filter(year == 2100)

## reproject for pretty maps
plot.data %<>%
  st_transform("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

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