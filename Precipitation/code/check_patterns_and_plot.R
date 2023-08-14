##########################
#################  library
##########################

## clear workspace
rm(list = ls())
gc()

## This function will check if a package is installed, and if not, install it
list.of.packages <- c('tidyverse',
                      'sf','ncdf4',
                      'ggplot2', 'rcartocolor',
                      'rworldmap')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

##########################
########  read patterns from https://github.com/JGCRI/linear_pattern_scaling
##########################

## path to file
file = 'tsd/code/precipitation/data/outputs/annual/ACCESS-CM2_ssp245_pr_annual_pattern.nc'

## open nc
nc = nc_open(file)

## check nc
nc

## get variable of interest
var = 'slope'

## create data frame
data =
  bind_cols(
    expand.grid(
      lon = ncdf4::ncvar_get(nc,'lon') ,
      lat = ncdf4::ncvar_get(nc, 'lat')
    ) %>%
      mutate(lon = if_else(lon >= 0 & lon <= 180, lon, lon-360)),  ## shift (0,360) to (-180, 180)
    t(ncdf4::ncvar_get(nc, var)) %>%
      as_tibble() %>%
      rownames_to_column(var = 'row.id') %>%         ## create indicator for row of matrix
      pivot_longer(-row.id,
                   names_to = 'column.id') %>%       ## create indicator for column of matrix
      group_by(row.id) %>%
      mutate(column.id = as.character(seq(n())),     ## create indicator for column of matrix
             value = value * 86400)                  ## convert from kg/m2/s per degC to mm/day per deg C (slope gets multiplied by Tgav to give precip)
  ) %>%
  st_as_sf(coords = c('lon', 'lat'),                 ## convert to simple feature, assuming projection is 4326
           crs = 4326) %>% 
  st_transform("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

## close nc
nc_close(nc)

## get world polygons 
world = 
  st_as_sf(cleangeo::clgeo_Clean(getMap())) %>% 
  st_transform("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

## check plot
data %>% 
  ggplot +
  geom_sf(aes(color = value)) + 
  geom_sf(data = world,
          fill = NA,
          alpha = 0.1) + 
  scale_color_carto_c(palette = 'Tropic') + 
  labs(title = basename(file),
       color = '  Slope \n(mm/day)')




## repeat above with a second ESM pattern
file = 'tsd/code/precipitation/data/outputs/annual/ACCESS-ESM1-5_ssp245_pr_annual_pattern.nc'

## open nc
nc = nc_open(file)

## check nc
nc

## get variable of interest
var = 'slope'

## create data frame
data =
  bind_cols(
    expand.grid(
      list(lon = ncdf4::ncvar_get(nc,'lon') ,
           lat = ncdf4::ncvar_get(nc, 'lat'))
    ) %>%
      mutate(lon = if_else(lon >= 0 & lon <= 180, lon, lon-360)),  ## shift (0,360) to (-180, 180)
    t(ncdf4::ncvar_get(nc, var)) %>%
      as_tibble() %>%
      rownames_to_column(var = 'row.id') %>%         ## create indicator for row of matrix
      pivot_longer(-row.id,
                   names_to = 'column.id') %>%       ## create indicator for column of matrix
      group_by(row.id) %>%
      mutate(column.id = as.character(seq(n())),     ## create indicator for column of matrix
             value = value * 86400)                  ## convert from kg/m2/s per degC to mm/day per deg C (slope gets multiplied by Tgav to give precip)
  ) %>%
  st_as_sf(coords = c('lon', 'lat'),                 ## convert to simple feature, assuming projection is 4326
           crs = 4326) %>% 
  st_transform("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

## close nc
nc_close(nc)

## check plot
data %>% 
  ggplot +
  geom_sf(aes(color = value)) + 
  geom_sf(data = world,
          alpha = 0.2) + 
  scale_color_carto_c(palette = 'Tropic') + 
  labs(title = basename(file),
       color = '  Slope \n(mm/day)')
