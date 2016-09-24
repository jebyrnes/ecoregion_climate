###### 0) Libraries and the like
library(dplyr)
library(purrr)
library(tidyr)
library(raster)
library(lubridate)
library(meowR)



###### 1) Get the regions
data("regions")
data("provinces")
data("realms")


###### 3) Load and rasterize HADSST data set from 1950 - 2013
hadsst <- raster::brick("~/Dropbox/src/HADSST/HadISST_sst.nc")
raster::NAvalue(hadsst) <- -1000 #make sure we don't have any super small NAs
yearIDx <- which(chron::years(hadsst@z$Date) %in% 1950:2015)
hadsst_subset <- raster::subset(hadsst, names(hadsst)[yearIDx]) 

###### 4) For each unique DF, extract the mean temp data

#First, a generalizable function
getPolyTempTrajectory <- function(unique_poly_names, sub_geo_polys, grouping){
 unique_poly <- # unique_poly_names %>%
  #mutate(polyName = .[[grouping]]) %>%
   data.frame(p = unique_poly_names, polyName = unique_poly_names) %>%
  group_by(p) %>%
  nest(.key = nameData) %>%
  
  #get unique polys
  mutate(poly = purrr::map(nameData, 
                           ~ sub_geo_polys[sub_geo_polys@data[[grouping]] == .$polyName,])) %>%
  
  #get cells from the raster
 # mutate(raster_cells = purrr::map(nameData, ~ class(as(.$poly, "SpatialPolygonsDataFrame"))))
  mutate(raster_cells = purrr::map(poly, ~ cellFromPolygon(hadsst_subset, .))) %>%
  
  #get temp array from raster based on cells
  mutate(temp_mat = purrr::map(raster_cells, ~ raster::extract(hadsst_subset, .[[1]]))) %>%
  
  #get an average for the region
  mutate(mean_temp = purrr:::map(temp_mat, ~ as.data.frame(t(colMeans(., na.rm=T))))) %>%
  
  #now make it back into a single data frame
  dplyr::select(p, mean_temp) %>%
  unnest() %>%
  gather(DateName, tempC, -p) %>%
  
  #pretty up the dates
  mutate(DateName = gsub("X", "", as.character(DateName))) %>%
  mutate(Year = year(parse_date_time(DateName, orders="ymd"))) %>%
  mutate(tempC=ifelse(tempC == -1000, NA, tempC))
  
  #fix up names
  names(unique_poly) <- gsub("^p$", grouping, names(unique_poly))
  
  return(unique_poly)

}

#Do the work
unique_region_temp <- getPolyTempTrajectory(levels(regions$ECOREGION), regions, "ECOREGION")
unique_province_temp <- getPolyTempTrajectory(levels(provinces$PROVINCE), provinces, "PROVINCE")
unique_realm_temp <- getPolyTempTrajectory(levels(realms$REALM), realms, "REALM")

###### 5) Load the unique regions
write.csv(unique_region_temp, "../derived_data/hadsst_regions.csv", row.names=F)
write.csv(unique_province_temp, "../derived_data/hadsst_provinces.csv", row.names=F)
write.csv(unique_realm_temp, "../derived_data/hadsst_realms.csv", row.names=F)

