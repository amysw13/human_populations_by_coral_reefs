####Apply a local projection to every feature in a longlat data set
###Example here given by https://gist.github.com/mdsumner/5af323f455d839b80c41bb043f5b2068
##Creation of buffers at each distance

#load packages

library("sf")
library("rlist")
library("lwgeom")


###universal rowwise (by feature i.e. country) projection functions

local_proj <- function(lonlat) {
  sprintf("+proj=laea +lon_0=%f +lat_0=%f +units=m +no_defs", lonlat[1], lonlat[2])
}


local_reproj <- function(x) {
  cc <- sf::st_coordinates(st_centroid(x))
  sf::st_transform(x, local_proj(cc))
}


world_crs <- function(x) {st_transform(x, crs = 4326)
}


###rowwise projections of polygons already created
c_py_row <- lapply(split(c_py, 1:nrow(c_py)), local_reproj)
list.save(c_py_row, file="./Output/c_py_row_projections.RData")


##Buffers creation on locally projected polygons 

#set buffers of 100km

buffer_100 <- function(x) {st_buffer(x, dist = 100000)
}

buffer_100_py <- lapply(c_py_row, buffer)
list.save(buffer_100_py, file="./Output/buffer_100_py_row.RData")
buffer_100_py_2<-lapply(buffer_100_py, world_crs)
buffer_100_py_3<-do.call(rbind, buffer_100_py_2)
st_write(obj <- buffer_100_py_3, dsn ="./Output/Coral_poly_100km.gpkg", delete_dsn =TRUE)

buffer_100_py_3<-st_read("./Output/Coral_poly_100km.gpkg")
buffer_100_py_3$buffer_area_km2<-set_units(st_area(buffer_100_py_3), "km2")


buffer_100_pt <- lapply(c_pt_row, buffer)
list.save(buffer_100_pt, file="./Output/buffer_100_pt_row.RData")
buffer_100_pt_2<-lapply(buffer_100_pt, world_crs)
buffer_100_pt_3<-do.call(rbind, buffer_100_pt_2)

st_write(obj <- buffer_100_pt_3, dsn ="./Output/Coral_point_100km.gpkg", delete_dsn =TRUE)

buffer_100_py<- st_read("./Output/Coral_poly_100km.gpkg")
buffer_100_pt<- st_read("./Output/Coral_point_100km.gpkg")


#set buffers of 50km

buffer <- function(x) {st_buffer(x, dist = 50000)
}

buffer_50_py <- lapply(c_py_row, buffer)
list.save(buffer_50_py, file="./Output/buffer_50_py_row.RData")
buffer_50_py_2<-lapply(buffer_50_py, world_crs)
buffer_50_py_3<-do.call(rbind, buffer_50_py_2)
st_write(obj <- buffer_50_py_3, dsn ="./Output/Coral_poly_50km.gpkg", delete_dsn =TRUE)


buffer_50_pt <- lapply(c_pt_row, buffer)
list.save(buffer_50_pt, file="./Output/buffer_50_pt_row.RData")
buffer_50_pt_2<-lapply(buffer_50_pt, world_crs)
buffer_50_pt_3<-do.call(rbind, buffer_50_pt_2)

st_write(obj <- buffer_50_pt_3, dsn ="./Output/Coral_point_50km.gpkg", delete_dsn =TRUE)

buffer_50_py<- st_read("./Output/Coral_poly_50km.gpkg")
buffer_50_pt<- st_read("./Output/Coral_point_50km.gpkg")




#set buffers of 30km

buffer <- function(x) {st_buffer(x, dist = 30000)
}

buffer_30_py <- lapply(c_py_row, buffer)
list.save(buffer_30_py, file="./Output/buffer_30_py_row.RData")
buffer_30_py_2<-lapply(buffer_30_py, world_crs)
buffer_30_py_3<-do.call(rbind, buffer_30_py_2)
st_write(obj <- buffer_30_py_3, dsn ="./Output/Coral_poly_30km.gpkg", delete_dsn =TRUE)


buffer_30_pt <- lapply(c_pt_row, buffer)
list.save(buffer_30_pt, file="./Output/buffer_30_pt_row.RData")
buffer_30_pt_2<-lapply(buffer_30_pt, world_crs)
buffer_30_pt_3<-do.call(rbind, buffer_30_pt_2)
st_write(obj <- buffer_30_pt_3, dsn ="./Output/Coral_point_30km.gpkg", delete_dsn =TRUE)

buffer_30_py<- st_read("./Output/Coral_poly_30km.gpkg")
buffer_30_pt<- st_read("./Output/Coral_point_30km.gpkg")


#set buffers of 10km
buffer <- function(x) {st_buffer(x, dist = 10000)
}

buffer_10_py <- lapply(c_py_row, buffer)
list.save(buffer_10_py, file="./Output/buffer_10_py_row.RData")
buffer_10_py_2<-lapply(buffer_10_py, world_crs)
buffer_10_py_3<-do.call(rbind, buffer_10_py_2)
st_write(obj <- buffer_10_py_3, dsn ="./Output/Coral_poly_10km.gpkg", delete_dsn =TRUE)


buffer_10_pt <- lapply(c_pt_row, buffer)
list.save(buffer_10_pt, file="./Output/buffer_10_pt_row.RData")
buffer_10_pt_2<-lapply(buffer_10_pt, world_crs)
buffer_10_pt_3<-do.call(rbind, buffer_10_pt_2)
st_write(obj <- buffer_10_pt_3, dsn ="./Output/Coral_point_10km.gpkg", delete_dsn =TRUE)

buffer_10_py<- st_read("./Output/Coral_poly_10km.gpkg")
buffer_10_pt<- st_read("./Output/Coral_point_10km.gpkg")


#set buffers of 5km
buffer <- function(x) {st_buffer(x, dist = 5000)
}

buffer_5_py <- lapply(c_py_row, buffer)
list.save(buffer_5_py, file="./Output/buffer_5_py_row.RData")
buffer_5_py_2<-lapply(buffer_5_py, world_crs)
buffer_5_py_3<-do.call(rbind, buffer_5_py_2)
st_write(obj <- buffer_5_py_3, dsn ="./Output/Coral_poly_5km.gpkg", delete_dsn =TRUE)


buffer_5_pt <- lapply(c_pt_row, buffer)
list.save(buffer_5_pt, file="./Output/buffer_5_pt_row.RData")
buffer_5_pt_2<-lapply(buffer_5_pt, world_crs)
buffer_5_pt_3<-do.call(rbind, buffer_5_pt_2)
st_write(obj <- buffer_5_pt_3, dsn ="./Output/Coral_point_5km.gpkg", delete_dsn =TRUE)

buffer_5_py<- st_read("./Output/Coral_poly_5km.gpkg")
buffer_5_pt<- st_read("./Output/Coral_point_5km.gpkg")
