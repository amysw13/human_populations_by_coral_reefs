####R workflow for human population extractions 

#setwd("/home/as17697")

#Load packages
if(!require(sf)){install.packages("sf"); library(sf)}
if(!require(velox)){install.packages("velox"); library(velox)}
if(!require(tibble)){install.packages("tibble"); library(tibble)}


#Load in polygons for extractions - coral reef countries and distance buffers
buffer_100km <- st_read("./data/Coral_buffer_100km_clean.gpkg")
buffer_50km <- st_read("./data/Coral_buffer_50km_clean.gpkg")
buffer_30km <- st_read("./data/Coral_buffer_30km_clean.gpkg")
buffer_10km <- st_read("./data/Coral_buffer_10km_clean.gpkg")
buffer_5km <- st_read("./data/Coral_buffer_5km_clean.gpkg")
buffer_1km <- st_read("./data/Coral_buffer_1km_clean.gpkg")

#Use coral_poly to only extract data for coral reef countries as this will speed up all analyses - 11.05.2022 - need to optimise the functions already written
coral_poly <- st_read("./data/coral_poly.gpkg") #All coral reef countries within 100km of coral reefs

#this is for the new format of Landscan data files - renamed 2018 to 3018 for cross checking data. 
filenames <- list.files(path = "./data/LandScan/",
                        pattern = ".tif",
                        full.names = TRUE,
                        recursive = TRUE)


####World & Coral Country (cc) extraction - Function 1####
extract_wld <-function(filenames)
{
  dat<-velox(filenames) #load file
  x_1 <- dat$extract(coral_poly, fun=function(t) sum(t,na.rm=TRUE)) #extraction using velox pkg
  #need to unlist the extracted values and sum the pop den
  x_2 <- unlist(lapply(x_1,FUN = function(x) if (!is.null(x)) sum(x, na.rm=TRUE) else NA))
  #join unlisted values to df
  x_3 <- cbind(coral_poly, x_2)
  #cleaning some of the data and adding year columns
  colnames(x_3)[colnames(x_3)=="x_2"] <- "Total_pop"
  #renaming column
  colnames(x_3)[colnames(x_3)=="ISO3.1"] <- "ISO3"
  #adding year column
  x_3<-tibble::add_column(x_3, Year = regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)), .after = "NAME_0") #add_column function from tibble package
  #drop geometry to allow for saving in .csv
  x_4 <- st_drop_geometry(x_3)
  ##saving data as .gpkg (retaining spatial data for mapping)
  st_write(x_3, dsn = paste0("./Output_pop/Coral_pop_ls",regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)),"_world_val.gpkg"), delete_dsn =TRUE)
  #saving population values into .csv for checking
  write.csv(x_4, file = paste0("./Output_pop/Pop_ls",regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)),"_world_val.csv"))
  
}


####Testing function on files for population extraction ####
for (f in filenames){
  print(f)
  extract_wld(f)
}

extract_100 <-function(filenames)
{
  dat<-velox(filenames, crs="+proj=longlat +datum=WGS84 +no_defs") #load file and project to 4362
  #clip raster prior to extraction
  x_1 <- dat$extract(buffer_100km, fun=function(t) sum(t,na.rm=TRUE)) #extraction from velox pkg
  #need to unlist the extracted values and sum the pop den
  x_2 <- unlist(lapply(x_1,function(x) if (!is.null(x)) sum(x, na.rm=TRUE) else NA))
  #join unlisted values to df
  x_3 <- cbind(buffer_100km, x_2)
  #renaming column
  colnames(x_3)[colnames(x_3)=="x_2"] <- "pop"
  #renaming column
  colnames(x_3)[colnames(x_3)=="ISO3.1"] <- "ISO3"
  #adding a Year column
  x_3<-add_column(x_3, Year = regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)), .after = "NAME_0") #add_column function from tibble package
  #adding a Distance column
  x_3<-add_column(x_3, Distance = "100km", .after = "Year")
  #drop geometry to allow for saving in .CSV
  x_4 <-st_drop_geometry(x_3)
  ##saving data as .gpkg
  st_write(x_3, dsn = paste0("./Output_pop/Coral_pop_ls",regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)),"_100_retest.gpkg"), delete_dsn =TRUE)
  #saving population values into .csv for checking
  write.csv(x_4, file = paste0("./Output_pop/Pop_ls",regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)),"_100_retest.csv"))
}

#Results 11.05.2022 - upon testing the results from majority of countries the same,
#with a few countries having differing values by a few %. Is this worth re-analysing the whole study

for (f in filenames){
  print(f)
  extract_100(f)
}

####50km population extraction - Function 3####
extract_50 <-function(filenames)
{
  dat<-velox(filenames, crs="+proj=longlat +datum=WGS84 +no_defs") #load file
  #clip raster prior to extraction
  x_1 <- dat$extract(buffer_50km, fun=function(t) sum(t,na.rm=TRUE)) #extraction from velox pkg
  #need to unlist the extracted values and sum the pop den
  x_2 <- unlist(lapply(x_1,function(x) if (!is.null(x)) sum(x, na.rm=TRUE) else NA))
  #join unlisted values to df
  x_3 <- cbind(buffer_50km, x_2)
  #renaming column
  colnames(x_3)[colnames(x_3)=="x_2"] <- "pop"
  #renaming column
  colnames(x_3)[colnames(x_3)=="ISO3.1"] <- "ISO3"
  #adding a Year column
  x_3<-add_column(x_3, Year = regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)), .after = "NAME_0") #add_column function from tibble package
  #adding a Distance column
  x_3<-add_column(x_3, Distance = "50km", .after = "Year")
  #drop geometry to allow for saving in .CSV
  x_4 <-st_drop_geometry(x_3)
  ##saving data as .gpkg
  st_write(x_3, dsn = paste0("./Output_pop/Coral_pop_ls",regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)),"_50.gpkg"), delete_dsn =TRUE)
  #saving population values into .csv for checking
  write.csv(x_4, file = paste0("./Output_pop/Pop_ls",regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)),"_50.csv"))
}


for (f in filenames){
  print(f)
  extract_50(f)
}

####30km population extraction - Function 4####
extract_30 <-function(filenames)
{
  dat<-velox(filenames, crs="+proj=longlat +datum=WGS84 +no_defs") #load file
  #clip raster prior to extraction
  x_1 <- dat$extract(buffer_30km, fun=function(t) sum(t,na.rm=TRUE)) #extraction from velox pkg
  #need to unlist the extracted values and sum the pop den
  x_2 <- unlist(lapply(x_1,function(x) if (!is.null(x)) sum(x, na.rm=TRUE) else NA))
  #join unlisted values to df
  x_3 <- cbind(buffer_30km, x_2)
  #renaming column
  colnames(x_3)[colnames(x_3)=="x_2"] <- "pop"
  #renaming column
  colnames(x_3)[colnames(x_3)=="ISO3.1"] <- "ISO3"
  #adding a Year column
  x_3<-add_column(x_3, Year = regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)), .after = "NAME_0") #add_column function from tibble package
  #adding a Distance column
  x_3<-add_column(x_3, Distance = "30km", .after = "Year")
  #drop geometry to allow for saving in .CSV
  x_4 <-st_drop_geometry(x_3)
  ##saving data as .gpkg
  st_write(x_3, dsn = paste0("./Output_pop/Coral_pop_ls",regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)),"_30.gpkg"), delete_dsn =TRUE)
  #saving population values into .csv for checking
  write.csv(x_4, file = paste0("./Output_pop/Pop_ls",regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)),"_30.csv"))
}


for (f in filenames){
  print(f)
  extract_30(f)
}

####10km population extraction - Function 5####
extract_10 <-function(filenames)
{
  dat<-velox(filenames, crs="+proj=longlat +datum=WGS84 +no_defs") #load file
  #clip raster prior to extraction
  x_1 <- dat$extract(buffer_10km, fun=function(t) sum(t,na.rm=TRUE)) #extraction from velox pkg
  #need to unlist the extracted values and sum the pop den
  x_2 <- unlist(lapply(x_1,function(x) if (!is.null(x)) sum(x, na.rm=TRUE) else NA))
  #join unlisted values to df
  x_3 <- cbind(buffer_10km, x_2)
  #renaming column
  colnames(x_3)[colnames(x_3)=="x_2"] <- "pop"
  #renaming column
  colnames(x_3)[colnames(x_3)=="ISO3.1"] <- "ISO3"
  #adding a Year column
  x_3<-add_column(x_3, Year = regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)), .after = "NAME_0") #add_column function from tibble package
  #adding a Distance column
  x_3<-add_column(x_3, Distance = "10km", .after = "Year")
  #drop geometry to allow for saving in .CSV
  x_4 <-st_drop_geometry(x_3)
  ##saving data as .gpkg
  st_write(x_3, dsn = paste0("./Output_pop/Coral_pop_ls",regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)),"_10.gpkg"), delete_dsn =TRUE)
  #saving population values into .csv for checking
  write.csv(x_4, file = paste0("./Output_pop/Pop_ls",regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)),"_10.csv"))
}


for (f in filenames){
  print(f)
  extract_10(f)
}


####5km population extraction - Function 6####
extract_5 <-function(filenames)
{
  dat<-velox(filenames, crs="+proj=longlat +datum=WGS84 +no_defs") #load file
  #clip raster prior to extraction
  x_1 <- dat$extract(buffer_5km, fun=function(t) sum(t,na.rm=TRUE)) #extraction from velox pkg
  #need to unlist the extracted values and sum the pop den
  x_2 <- unlist(lapply(x_1,function(x) if (!is.null(x)) sum(x, na.rm=TRUE) else NA))
  #join unlisted values to df
  x_3 <- cbind(buffer_5km, x_2)
  #renaming column
  colnames(x_3)[colnames(x_3)=="x_2"] <- "pop"
  #renaming column
  colnames(x_3)[colnames(x_3)=="ISO3.1"] <- "ISO3"
  #adding a Year column
  x_3<-add_column(x_3, Year = regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)), .after = "NAME_0") #add_column function from tibble package
  #adding a Distance column
  x_3<-add_column(x_3, Distance = "5km", .after = "Year")
  #drop geometry to allow for saving in .CSV
  x_4 <-st_drop_geometry(x_3)
  ##saving data as .gpkg
  st_write(x_3, dsn = paste0("./Output_pop/Coral_pop_ls",regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)),"_5.gpkg"), delete_dsn =TRUE)
  #saving population values into .csv for checking
  write.csv(x_4, file = paste0("./Output_pop/Pop_ls",regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)),"_5.csv"))
}


for (f in filenames){
  print(f)
  extract_5(f)
}