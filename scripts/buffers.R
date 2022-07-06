####Apply a local projection to every feature in a longlat data set
###Example here given by https://gist.github.com/mdsumner/5af323f455d839b80c41bb043f5b2068
##Creation of buffers at each distance

# load packages

library("sf")
library("rlist")
library("lwgeom")
library("units")
library("dplyr")
library("raster")

####Cleaning coral reef data

#loading coral reef distribution shapefiles, points and multipolygons
c_pt_raw <- st_read("./Data/01_Data/WCMC008_CoralReef2018_Pt_v4.shp")
c_py_raw <- st_read("./Data/01_Data/WCMC008_CoralReef2018_Py_v4.shp")

#Summarise by PARENT_ISO and ISO3
ctry_py <- c_py_raw %>%
  group_by(PARENT_ISO, ISO3) %>% 
  summarise()

ctry_pt <- c_pt_raw %>%
  group_by(PARENT_ISO, ISO3) %>% 
  summarise()

#saving the cleaned data
st_write(obj <- ctry_py, dsn ="./Output/Coral_reef_py_clean.gpkg", delete_dsn =TRUE)
st_write(obj <- ctry_pt, dsn ="./Output/Coral_reef_pt_clean.gpkg", delete_dsn =TRUE)

#loading clean data (if required)
c_pt <- st_read("./Output/Coral_reef_pt_clean.gpkg")
c_py <- st_read("./Output/Coral_reef_py_clean.gpkg")


####Buffer creation
#Set up for custom function to project each row (country)
#universal row-wise (by feature i.e. country) projection functions

#project to a "local" Lambert Azimuthal Equal-Area projection
local_proj <- function(lonlat) {
  sprintf("+proj=laea +lon_0=%f +lat_0=%f +units=m +no_defs", lonlat[1], lonlat[2])
}

#finding centroid to project from
local_reproj <- function(x) {
  cc <- sf::st_coordinates(st_centroid(x))
  sf::st_transform(x, local_proj(cc))
}

#project to CRS 4326
world_crs <- function(x) {st_transform(x, crs = 4326)
}

#Project coral reef data - only required for polygon data
c_py_row <- lapply(split(c_py, 1:nrow(c_py)), local_reproj)
list.save(c_py_row, file="./Output/c_py_row_projections.RData")

## Buffers creation on locally projected polygons 

#100km buffer function
buffer_100 <- function(x) {st_buffer(x, dist = 100000)
}

#buffers around polygons
buffer_100_py <- lapply(c_py_row, buffer_100)
#save raw buffer data
list.save(buffer_100_py, file="./Output/buffer_100_py_row.RData")
#project buffers to CRS 4326
buffer_100_py_2<-lapply(buffer_100_py, world_crs)
#create into one dataframe
buffer_100_py_3<-do.call(rbind, buffer_100_py_2)
#save polygon buffer data
st_write(obj <- buffer_100_py_3, dsn ="./Output/Coral_poly_100km.gpkg", delete_dsn =TRUE)

#buffers around points
buffer_100_pt <- lapply(c_pt_row, buffer_100)
#save raw buffer data
list.save(buffer_100_pt, file="./Output/buffer_100_pt_row.RData")
#project buffers to CRS 4326
buffer_100_pt_2<-lapply(buffer_100_pt, world_crs)
#create into one dataframe
buffer_100_pt_3<-do.call(rbind, buffer_100_pt_2)
#save point buffer data
st_write(obj <- buffer_100_pt_3, dsn ="./Output/Coral_point_100km.gpkg", delete_dsn =TRUE)

#Load in data (if required)
buffer_100_py<- st_read("./Output/Coral_poly_100km.gpkg")
buffer_100_pt<- st_read("./Output/Coral_point_100km.gpkg")


#50km buffer function
buffer_50 <- function(x) {st_buffer(x, dist = 50000)
}

buffer_50_py <- lapply(c_py_row, buffer_50)
list.save(buffer_50_py, file="./Output/buffer_50_py_row.RData")
buffer_50_py_2<-lapply(buffer_50_py, world_crs)
buffer_50_py_3<-do.call(rbind, buffer_50_py_2)
st_write(obj <- buffer_50_py_3, dsn ="./Output/Coral_poly_50km.gpkg", delete_dsn =TRUE)

buffer_50_pt <- lapply(c_pt_row, buffer_50)
list.save(buffer_50_pt, file="./Output/buffer_50_pt_row.RData")
buffer_50_pt_2<-lapply(buffer_50_pt, world_crs)
buffer_50_pt_3<-do.call(rbind, buffer_50_pt_2)
st_write(obj <- buffer_50_pt_3, dsn ="./Output/Coral_point_50km.gpkg", delete_dsn =TRUE)

buffer_50_py<- st_read("./Output/Coral_poly_50km.gpkg")
buffer_50_pt<- st_read("./Output/Coral_point_50km.gpkg")


#30km buffer function
buffer_30 <- function(x) {st_buffer(x, dist = 30000)
}

buffer_30_py <- lapply(c_py_row, buffer_30)
list.save(buffer_30_py, file="./Output/buffer_30_py_row.RData")
buffer_30_py_2<-lapply(buffer_30_py, world_crs)
buffer_30_py_3<-do.call(rbind, buffer_30_py_2)
st_write(obj <- buffer_30_py_3, dsn ="./Output/Coral_poly_30km.gpkg", delete_dsn =TRUE)

buffer_30_pt <- lapply(c_pt_row, buffer_30)
list.save(buffer_30_pt, file="./Output/buffer_30_pt_row.RData")
buffer_30_pt_2<-lapply(buffer_30_pt, world_crs)
buffer_30_pt_3<-do.call(rbind, buffer_30_pt_2)
st_write(obj <- buffer_30_pt_3, dsn ="./Output/Coral_point_30km.gpkg", delete_dsn =TRUE)

buffer_30_py<- st_read("./Output/Coral_poly_30km.gpkg")
buffer_30_pt<- st_read("./Output/Coral_point_30km.gpkg")


#10km buffer function
buffer_10 <- function(x) {st_buffer(x, dist = 10000)
}

buffer_10_py <- lapply(c_py_row, buffer_10)
list.save(buffer_10_py, file="./Output/buffer_10_py_row.RData")
buffer_10_py_2<-lapply(buffer_10_py, world_crs)
buffer_10_py_3<-do.call(rbind, buffer_10_py_2)
st_write(obj <- buffer_10_py_3, dsn ="./Output/Coral_poly_10km.gpkg", delete_dsn =TRUE)

buffer_10_pt <- lapply(c_pt_row, buffer_10)
list.save(buffer_10_pt, file="./Output/buffer_10_pt_row.RData")
buffer_10_pt_2<-lapply(buffer_10_pt, world_crs)
buffer_10_pt_3<-do.call(rbind, buffer_10_pt_2)
st_write(obj <- buffer_10_pt_3, dsn ="./Output/Coral_point_10km.gpkg", delete_dsn =TRUE)

buffer_10_py<- st_read("./Output/Coral_poly_10km.gpkg")
buffer_10_pt<- st_read("./Output/Coral_point_10km.gpkg")


#5km buffer function
buffer_5 <- function(x) {st_buffer(x, dist = 5000)
}

buffer_5_py <- lapply(c_py_row, buffer_5)
list.save(buffer_5_py, file="./Output/buffer_5_py_row.RData")
buffer_5_py_2<-lapply(buffer_5_py, world_crs)
buffer_5_py_3<-do.call(rbind, buffer_5_py_2)
st_write(obj <- buffer_5_py_3, dsn ="./Output/Coral_poly_5km.gpkg", delete_dsn =TRUE)

buffer_5_pt <- lapply(c_pt_row, buffer_5)
list.save(buffer_5_pt, file="./Output/buffer_5_pt_row.RData")
buffer_5_pt_2<-lapply(buffer_5_pt, world_crs)
buffer_5_pt_3<-do.call(rbind, buffer_5_pt_2)
st_write(obj <- buffer_5_pt_3, dsn ="./Output/Coral_point_5km.gpkg", delete_dsn =TRUE)

buffer_5_py<- st_read("./Output/Coral_poly_5km.gpkg")
buffer_5_pt<- st_read("./Output/Coral_point_5km.gpkg")

#### Cleaning buffers

#loading in world data from GADM
world <- st_read("./Data/gadm36_levels.gpkg", "level0") #redirect when required
names(world)[1] <- "ISO3"
#load coral country information to add regionl data attributes
coral_countries_info<-read.csv("./Data/Coral_Reef_Countries.csv")

#100km clean-up
#this is gives overlapping portion of polygons
overlap <- st_intersection(buffer_100_py_row_3, buffer_100_pt_row_3)
#this is to delete the overlapping polygons
diffPoly <- st_difference(buffer_100_pt_row_3, st_union(overlap))
#joining the cleaned POINT buffer data with the POLYGON data
buffer_100_all_row<-rbind(diffPoly, buffer_100_py_row_3)
#deal with buffers that cross the dateline
buffer_100_all_row_1 <- buffer_100_all_row %>% st_wrap_dateline(options = c("WRAPDATELINE=YES",  "DATELINEOFFSET=180"), quiet = FALSE)
#cast geometries to match "MULTIPOLYGON" as there is a mix of MULTIPOLYGON and POLYGONS
buffer_100_all_row_2<-st_cast(buffer_100_all_row_1, "MULTIPOLYGON")
#selecting world polygons that contain buffers created from 100km
coral_poly<-world[buffer_100_all_row_2, ]
#clipping buffer polygons the overlap with the land
buffer_100_all_row_3<-st_intersection(buffer_100_all_row_2, coral_poly)
#dissolve using the world country ISO code, this should be the country borders so hopefully what I am looking for ##YAYAYAYAY!!!
buffer_100_all_row_one<-buffer_100_all_row_3%>%group_by(ISO3.1, NAME_0)%>%summarize()
#work out area of each country polygon
buffer_100_all_row_one$buffer_area_km2<-set_units(st_area(buffer_100_all_row_one), "km2")
#matching ISO3 codes to obtain the regional information 
buffer_100_all_row_one$region<-coral_countries_info$Ocean_Region[match(buffer_100_all_row_one$ISO3.1, coral_countries_info$ISO3)]
#save cleaned up buffers
st_write(obj = buffer_100_all_row_one, dsn ="./Data/Coral_buffer_100km_clean.gpkg", delete_dsn =TRUE)
#if required for loading
buffer_100km <- st_read("./Data/Coral_buffer_100km_clean.gpkg")


####Coral polygon dataframe
#This contains countries that around found within 100km of coral reefs from GADM world data
#Will be used to extract country-wide population data from LandScan

st_write(obj = coral_poly, dsn ="./Data/coral_poly.gpkg", delete_dsn =TRUE)


#50km clean-up
buffer_50_py<- st_read("./Output/Coral_poly_50km.gpkg")
buffer_50_pt<- st_read("./Output/Coral_point_50km.gpkg")

overlap <- st_intersection(buffer_50_py, buffer_50_pt)
diffPoly <- st_difference(buffer_50_pt, st_union(overlap))
buffer_50_all<-rbind(diffPoly, buffer_50_py)
buffer_50_all_1 <- buffer_50_all %>% st_wrap_dateline(options = c("WRAPDATELINE=YES",  "DATELINEOFFSET=180"), quiet = FALSE)
buffer_50_all_2<-st_cast(buffer_50_all_1, "MULTIPOLYGON")
coral_poly_50<-world[buffer_50_all_2, ]
buffer_50_all_3<-st_intersection(buffer_50_all_2, coral_poly_50)
buffer_50_all_one<-buffer_50_all_3%>%group_by(ISO3.1, NAME_0)%>%summarize()
buffer_50_all_one$buffer_area_km2<-set_units(st_area(buffer_50_all_one), "km2")
buffer_50_all_one$region<-coral_countries_info$Ocean_Region[match(buffer_50_all_one$ISO3.1, coral_countries_info$ISO3)]
st_write(obj = buffer_50_all_one, dsn ="./Data/Coral_buffer_50km_clean.gpkg", delete_dsn =TRUE)
buffer_50km <- st_read("./Data/Coral_buffer_50km_clean.gpkg")



#30km clean-up

buffer_30_py<- st_read("./Output/Coral_poly_30km.gpkg")
buffer_30_pt<- st_read("./Output/Coral_point_30km.gpkg")

overlap <- st_intersection(buffer_30_py, buffer_30_pt)
diffPoly <- st_difference(buffer_30_pt, st_union(overlap))
buffer_30_all<-rbind(diffPoly, buffer_30_py)
buffer_30_all_1 <- buffer_30_all %>% st_wrap_dateline(options = c("WRAPDATELINE=YES",  "DATELINEOFFSET=180"), quiet = FALSE)
buffer_30_all_2<-st_cast(buffer_30_all_1, "MULTIPOLYGON")
coral_poly_30<-world[buffer_30_all_2, ]
buffer_30_all_3<-st_intersection(buffer_30_all_2, coral_poly_30)
buffer_30_all_one<-buffer_30_all_3%>%group_by(ISO3.1, NAME_0)%>%summarize()  #here added grouping value, see if it makes any difference
buffer_30_all_one$buffer_area_km2<-set_units(st_area(buffer_30_all_one), "km2")
buffer_30_all_one$region<-coral_countries_info$Ocean_Region[match(buffer_30_all_one$ISO3.1, coral_countries_info$ISO3)]
st_write(obj = buffer_30_all_one, dsn ="./Data/Coral_buffer_30km_clean.gpkg", delete_dsn =TRUE)
buffer_30km <- st_read("./Data/Coral_buffer_30km_clean.gpkg")


#10km clean-up
buffer_10_py<- st_read("./Output/Coral_poly_10km.gpkg")
buffer_10_pt<- st_read("./Output/Coral_point_10km.gpkg")

overlap <- st_intersection(buffer_10_py, buffer_10_pt)
diffPoly <- st_difference(buffer_10_pt, st_union(overlap))
buffer_10_all<-rbind(diffPoly, buffer_10_py)
buffer_10_all_1 <- buffer_10_all %>% st_wrap_dateline(options = c("WRAPDATELINE=YES",  "DATELINEOFFSET=180"), quiet = FALSE)
buffer_10_all_2<-st_cast(buffer_10_all_1, "MULTIPOLYGON")
coral_poly_10<-world[buffer_10_all_2, ]
buffer_10_all_3<-st_intersection(buffer_10_all_2, coral_poly_10)
buffer_10_all_one<-buffer_10_all_3%>%group_by(ISO3.1, NAME_0)%>%summarize()
buffer_10_all_one$buffer_area_km2<-set_units(st_area(buffer_10_all_one), "km2")
buffer_10_all_one$region<-coral_countries_info$Ocean_Region[match(buffer_10_all_one$ISO3.1, coral_countries_info$ISO3)]
st_write(obj = buffer_10_all_one, dsn ="./Data/Coral_buffer_10km_clean.gpkg", delete_dsn =TRUE)
buffer_10km <- st_read("./Data/Coral_buffer_10km_clean.gpkg")


#5km clean-up
buffer_5_py<- st_read("./Output/Coral_poly_5km.gpkg")
buffer_5_pt<- st_read("./Output/Coral_point_5km.gpkg")

overlap <- st_intersection(buffer_5_py, buffer_5_pt)
diffPoly <- st_difference(buffer_5_pt, st_union(overlap))
buffer_5_all<-rbind(diffPoly, buffer_5_py)
buffer_5_all_1 <- buffer_5_all %>% st_wrap_dateline(options = c("WRAPDATELINE=YES",  "DATELINEOFFSET=180"), quiet = FALSE)
buffer_5_all_2<-st_cast(buffer_5_all_1, "MULTIPOLYGON")
coral_poly_5<-world[buffer_5_all_2, ]
buffer_5_all_3<-st_intersection(buffer_5_all_2, coral_poly_5)
buffer_5_all_one<-buffer_5_all_3%>%group_by(ISO3.1, NAME_0)%>%summarize()
buffer_5_all_one$buffer_area_km2<-set_units(st_area(buffer_5_all_one), "km2")
buffer_5_all_one$region<-coral_countries_info$Ocean_Region[match(buffer_5_all_one$ISO3.1, coral_countries_info$ISO3)]
st_write(obj = buffer_5_all_one, dsn ="./Data/Coral_buffer_5km_clean.gpkg", delete_dsn =TRUE)
buffer_5km <- st_read("./Data/Coral_buffer_5km_clean.gpkg")


#### US buffers - 

#Get US states data
us <- getData("GADM",country="USA",level=1)
us.states <- us[us$NAME_1 %in% states,]
us.states<-st_as_sf(us.states)

#Create buffers of US states
us_buffers_100km<-st_intersection(us.states,buffer_100km) 
us_buffers_50km<-st_intersection(us.states, buffer_50km) 
us_buffers_30km<-st_intersection(us.states,buffer_30km) 
us_buffers_10km<-st_intersection(us.states, buffer_10km) 
us_buffers_5km<-st_intersection(us.states, buffer_5km) 
us_buffers_1km<-st_intersection(us.states, buffer_1km) 

#get areas for each buffer zone 
us.states$area_km2<-set_units(st_area(us.states), "km2")
us_buffers_100km$buffer_area_km2<-set_units(st_area(us_buffers_100km), "km2")
us_buffers_50km$buffer_area_km2<-set_units(st_area(us_buffers_50km), "km2")
us_buffers_30km$buffer_area_km2<-set_units(st_area(us_buffers_30km), "km2")
us_buffers_10km$buffer_area_km2<-set_units(st_area(us_buffers_10km), "km2")
us_buffers_5km$buffer_area_km2<-set_units(st_area(us_buffers_5km), "km2")
us_buffers_1km$buffer_area_km2<-set_units(st_area(us_buffers_1km), "km2")

#save US buffers
st_write(us.states, "./Data/us.states.gpkg")
st_write(us_buffers_100km, "./Data/US_buffer_100km.gpkg")
st_write(us_buffers_50km, "./Data/US_buffer_50km.gpkg")
st_write(us_buffers_30km, "./Data/US_buffer_30km.gpkg")
st_write(us_buffers_10km, "./Data/US_buffer_10km.gpkg")
st_write(us_buffers_5km, "./Data/US_buffer_5km.gpkg")
st_write(us_buffers_1km, "./Data/US_buffer_1km.gpkg")
