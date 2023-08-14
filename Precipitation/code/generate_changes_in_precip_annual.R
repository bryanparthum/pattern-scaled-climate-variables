## precipitation patterns from https://github.com/JGCRI/linear_pattern_scaling
## Cite as: Kravitz, Ben, & Snyder, Abigail. (2022). Pangeo-Enabled ESM Pattern Scaling (PEEPS): A customizable dataset of emulated Earth System Model output (1.1) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.7557622

##########################
#################  library
##########################

## clear workspace
rm(list = ls())
gc()

## This function will check if a package is installed, and if not, install it
list.of.packages <- c('tidyverse',
                      'arrow',
                      'ncdf4')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

##########################
###################  parts
##########################

## path to files
path = 'tsd/code/precipitation/data/outputs/annual/'

## get variable of interest
var = 'slope'

## test 
x = list.files(path, pattern = 'ssp245_pr', full.names = T)[1]

## function to read in patters and scale by gmst
get_patterns <- function(x) {
  
  ## get filename to use for export
  filename = str_remove(basename(x), '.nc')
  
  ## open netcdf file
  nc = nc_open(x)
  
  ## read file, make transformation, export
  expand_grid(
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
               slope = value * 86400)                  ## convert from kg/m2/s per degC to mm/day per deg C (slope gets multiplied by Tgav to give precip)
    ), #%>%
      # st_as_sf(coords = c('lon', 'lat'),                 ## convert to simple feature, assuming projection is 4326
      #          crs = 4326) %>% 
      # st_transform("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"),
    gmst) %>% 
    mutate(change_in_precip = slope * gmst) %>% 
  write_parquet(paste0('tsd/code/precipitation/output/patterns/annual/', filename, '.parquet'))
  
  ## close netcdf file
  nc_close(nc)
}

##########################
#################  process
##########################

## get vector of global mean surface temperature from GIVE, baseline 10k trials using RFF-SPs
gmst = 
  read_csv('tsd/code/precipitation/data/TempNorm_1850to1900_global_temperature_norm.csv', show_col_types = F) %>% 
  rename(gmst = global_temperature_norm,
         year = time) %>% 
  filter(year %in% c(2050, 2100)) %>% 
  group_by(year) %>% 
  summarize(gmst = mean(gmst, na.rm = T))

# ## get two-period vector of global mean surface temperature from GIVE, baseline 10k trials using RFF-SPs
# gmst = 
#   read_csv('tsd/code/precipitation/data/TempNorm_1850to1900_global_temperature_norm.csv', show_col_types = F) %>% 
#   rename(gmst = global_temperature_norm,
#          year = time) %>% 
#   mutate(period = case_when(year %in% 1995:2014 ~ '1995 to 2014',
#                             year %in% 2081:2100 ~ '2081 to 2100')) %>% 
#   filter(!is.na(period)) %>% 
#   group_by(period) %>% 
#   summarize(gmst = mean(gmst, na.rm = T))
# gmst.pct.change = gmst$gmst[2]/gmst$gmst[1]

## recover patterns and save
list.files(path, pattern = 'ssp245_pr', full.names = T) %>%
  lapply(get_patterns)

## end of script, have a great day!