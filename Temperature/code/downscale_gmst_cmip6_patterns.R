## Written by: US EPA, National Center for Environmental Economics; October 2021
## extracts CMIP6 patterns from: USGCRP contract with OSTP, developed for use in USG updated SC-GHGs, 10/6/2021
## weights by area, pop, and gdp from: http://www.cger.nies.go.jp/gcp/population-and-gdp.html

##########################
#################  library
##########################

## Clear worksace
rm(list = ls())
gc()

## This function will check if a package is installed, and if not, install it
list.of.packages <- c('magrittr','tidyverse','raster','rasterVis','sf','exactextractr','fasterize','stars','ggplot2','wesanderson','egg','RColorBrewer','rworldmap','cleangeo')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

##########################
########  recover patterns 
##########################

## all files in the directory
files <- list.files('data/downscaling/cmip6', full.names=T) %>%
  stringr::str_subset(., "broken", negate =T)

## base file
patterns = tibble()

for (SSP in c('ssp1','ssp2','ssp3')) {

for (i in 1:length(files)) {
pattern = raster(files[i], varname="pattern") %>% rotate()

## country polygons
world <- st_as_sf(cleangeo::clgeo_Clean(getMap()[-which(getMap()$ADMIN=='Antarctica'),])) %>% st_transform(st_crs(pattern)) %>% dplyr::select(NAME,ISO3,continent) %>% rename_all(tolower)

if (SSP=='ssp1') {
  ## downscaled economic data: http://www.cger.nies.go.jp/gcp/population-and-gdp.html
  mur = st_read('data/downscaling/murakami/grid.shp') %>% st_transform(st_crs(pattern))
  gdp = read_csv(paste0('data/downscaling/murakami/gdp_',SSP,'.csv')) %>% dplyr::select(gID,g1_2000,g1_2100)
  pop = read_csv(paste0('data/downscaling/murakami/pop_',SSP,'.csv')) %>% dplyr::select(gID,p1_2000,p1_2100)
  mur = left_join(left_join(mur,gdp),pop) 
  rm(gdp,pop)
  ## country temp patterns
  gdp.weights.2000 = fasterize(mur, pattern, field="g1_2000", fun="sum", background=mean(mur$g1_2000,na.rm=T))
  pop.weights.2000 = fasterize(mur, pattern, field="p1_2000", fun="sum", background=mean(mur$p1_2000,na.rm=T))
  gdp.weights.2100 = fasterize(mur, pattern, field="g1_2100", fun="sum", background=mean(mur$g1_2100,na.rm=T))
  pop.weights.2100 = fasterize(mur, pattern, field="p1_2100", fun="sum", background=mean(mur$p1_2100,na.rm=T))
} else if (SSP=='ssp2') {
  ## downscaled economic data: http://www.cger.nies.go.jp/gcp/population-and-gdp.html
  mur = st_read('data/downscaling/murakami/grid.shp') %>% st_transform(st_crs(pattern))
  gdp = read_csv(paste0('data/downscaling/murakami/gdp_',SSP,'.csv')) %>% dplyr::select(gID,g2_2000,g2_2100)
  pop = read_csv(paste0('data/downscaling/murakami/pop_',SSP,'.csv')) %>% dplyr::select(gID,p2_2000,p2_2100)
  mur = left_join(left_join(mur,gdp),pop) 
  rm(gdp,pop)
  ## country temp patterns
  gdp.weights.2000 = fasterize(mur, pattern, field="g2_2000", fun="sum", background=mean(mur$g2_2000,na.rm=T))
  pop.weights.2000 = fasterize(mur, pattern, field="p2_2000", fun="sum", background=mean(mur$p2_2000,na.rm=T))
  gdp.weights.2100 = fasterize(mur, pattern, field="g2_2100", fun="sum", background=mean(mur$g2_2100,na.rm=T))
  pop.weights.2100 = fasterize(mur, pattern, field="p2_2100", fun="sum", background=mean(mur$p2_2100,na.rm=T))
} else if (SSP=='ssp3') {
  ## downscaled economic data: http://www.cger.nies.go.jp/gcp/population-and-gdp.html
  mur = st_read('data/downscaling/murakami/grid.shp') %>% st_transform(st_crs(pattern))
  gdp = read_csv(paste0('data/downscaling/murakami/gdp_',SSP,'.csv')) %>% dplyr::select(gID,g3_2000,g3_2100)
  pop = read_csv(paste0('data/downscaling/murakami/pop_',SSP,'.csv')) %>% dplyr::select(gID,p3_2000,p3_2100)
  mur = left_join(left_join(mur,gdp),pop) 
  rm(gdp,pop)
  ## country temp patterns
  gdp.weights.2000 = fasterize(mur, pattern, field="g3_2000", fun="sum", background=mean(mur$g3_2000,na.rm=T))
  pop.weights.2000 = fasterize(mur, pattern, field="p3_2000", fun="sum", background=mean(mur$p3_2000,na.rm=T))
  gdp.weights.2100 = fasterize(mur, pattern, field="g3_2100", fun="sum", background=mean(mur$g3_2100,na.rm=T))
  pop.weights.2100 = fasterize(mur, pattern, field="p3_2100", fun="sum", background=mean(mur$p3_2100,na.rm=T))
}

# world$patterns.gdp <- exact_extract(pattern, world,fun='mean')
world$patterns.area <- exact_extract(pattern, world, fun='mean')
world$patterns.gdp.2000 <- exact_extract(pattern, world, weights=gdp.weights.2000, fun='weighted_mean')
world$patterns.pop.2000 <- exact_extract(pattern, world, weights=pop.weights.2000, fun='weighted_mean')
world$patterns.gdp.2100 <- exact_extract(pattern, world, weights=gdp.weights.2100, fun='weighted_mean')
world$patterns.pop.2100 <- exact_extract(pattern, world, weights=pop.weights.2100, fun='weighted_mean')

# convert to a df for plotting in two steps,
pattern_pts <- rasterToPoints(pattern, spatial=T) # First, to a SpatialPointsDataFrame
pattern_df  <- data.frame(pattern_pts) # Then to a 'conventional' dataframe
rm(pattern_pts)

## plot
plot.min=min(world$patterns.area,world$patterns.gdp.2100,world$patterns.pop.2100)
plot.max=max(world$patterns.area,world$patterns.gdp.2100,world$patterns.pop.2100)

p <- ggarrange(top=paste0(substr(str_split(files[i],'/')[[1]][[4]],17,nchar(str_split(files[i],'/')[[1]][[4]])-10)),
               heights=c(.5,1,1,1),ncol=1,
               pattern_df %>% ggplot() +
                 geom_raster(aes(x=x,y=y,fill=pattern)) + 
                 scale_fill_gradientn(colors=wes_palette("Zissou1"),limits=c(plot.min,plot.max),na.value="white") +
                 labs(fill='CMIP6 Pattern',x="",y="") +
                 theme_minimal() + 
                 theme(axis.title=element_blank(),
                       axis.text=element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank()),
               world %>% ggplot() +
                 geom_sf(aes(fill=patterns.area)) + 
                 scale_fill_gradientn(colors=wes_palette("Zissou1"),limits=c(plot.min,plot.max),na.value="white") +
                 labs(fill='Area-weighted',x="",y="") +
                 theme_minimal() + 
                 theme(axis.title=element_blank(),
                       axis.text=element_blank(),
                       panel.grid.major.x = element_blank(),
                       panel.grid.minor.x = element_blank()), 
               world %>% ggplot() +
                 geom_sf(aes(fill=patterns.gdp.2100)) + 
                 scale_fill_gradientn(colors=wes_palette("Zissou1"),limits=c(plot.min,plot.max),na.value="white") +
                 labs(fill=paste0('GDP-weighted \nYear: 2100 \nSSP: ',SSP),x="",y="") +
                 theme_minimal() + 
                 theme(axis.title=element_blank(),
                       axis.text=element_blank(),
                       panel.grid.major.x = element_blank(),
                       panel.grid.minor.x = element_blank()), 
               world %>% ggplot() +
                 geom_sf(aes(fill=patterns.pop.2100)) + 
                 scale_fill_gradientn(colors=wes_palette("Zissou1"),limits=c(plot.min,plot.max),na.value="white") +
                 labs(fill=paste0('Pop-weighted \n(Year: 2100) \nSSP: ',SSP),x="",y="") +
                 theme_minimal() + 
                 theme(axis.text=element_text(size=12),
                       panel.grid.major.x = element_blank(),
                       panel.grid.minor.x = element_blank()))
ggsave(paste0('output/downscaling/figures/cmip6/',SSP,'_2100_',substr(str_split(files[i],'/')[[1]][[4]],17,nchar(str_split(files[i],'/')[[1]][[4]])-10),'.png'),p,width=8.5,height=11)

patterns = bind_rows(patterns,world %>% st_drop_geometry() %>% mutate(scenario=SSP,source=substr(str_split(files[i],'/')[[1]][[4]],17,nchar(str_split(files[i],'/')[[1]][[4]])-10),source.id=i))

}

}

## only contains ssp1-3. ssp1 is representative of ssp5, and ssp2 is representative of ssp4
patterns %<>% bind_rows(.,pat %>% filter(scenario=='ssp1' | scenario=='ssp2') %>% mutate(scenario=case_when(scenario=='ssp1'~'ssp5',scenario=='ssp2'~'ssp4'))) %>% arrange(source.id,scenario,iso3)

## save
write_csv(patterns,'output/downscaling/patterns/cmip6/cmip6_pattern_scaling_by_country.csv')

## collapse to an average pattern across cmip models by country
write_csv(patterns %>% select(-c(source,source.id)) %>% group_by(name,iso3,continent,scenario) %>% summarise_all(mean,na.rm=T),
          'output/downscaling/patterns/cmip6/cmip6_pattern_scaling_by_country_mean.csv')

## END OF SCRIPT. Have a great day!