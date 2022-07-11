#population analysis 

#setwd("/home/as17697")

library("raster")
library("sf")
library("units")
library("velox")
library("tibble")

##reading in us_buffers
us.states<-st_read("./Data/us.states.gpkg")
us_buffer_100km<-st_read( "./data/US_buffer_100km.gpkg")
us_buffer_50km <-st_read("./data/US_buffer_50km.gpkg")
us_buffer_30km<-st_read( "./data/US_buffer_30km.gpkg")
us_buffer_10km<-st_read( "./data/US_buffer_10km.gpkg")
us_buffer_5km<-st_read( "./data/US_buffer_5km.gpkg")

filenames <- list.files(path = "./Data/LandScan/",
                        pattern = ".tif",
                        full.names = TRUE,
                        recursive = TRUE)


####World & Coral Country (cc) extraction - Function 1####
extract_wld_us <-function(filenames) 
{
  dat<-velox(filenames, crs="+proj=longlat +datum=WGS84 +no_defs") #load file
  x_1 <- dat$extract(us.states, fun=function(t) sum(t,na.rm=TRUE)) #extraction from velox pkg
  #need to unlist the extracted values and sum the pop den
  x_2 <- unlist(lapply(x_1,FUN = function(x) if (!is.null(x)) sum(x, na.rm=TRUE) else NA))
  #join unlisted values to df
  x_3 <- cbind(us.states, x_2)
  #cleaning some of the data and adding year columns
  colnames(x_3)[colnames(x_3)=="x_2"] <- "Total_pop"
  #renaming column
  colnames(x_3)[colnames(x_3)=="ISO3.1"] <- "ISO3"
  #adding year column
  x_3<-tibble::add_column(x_3, Year = regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)), .after = "NAME_0") #add_column function from tibble package
  #drop geometry to allow for saving in .csv
  x_4 <-sf::st_drop_geometry(x_3)
  ##saving data as .gpkg (retaining spatial data for mapping)
  st_write(x_3, dsn = paste0("./Output_pop/Coral_pop_ls",regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)),"_us_val.gpkg"), delete_dsn =TRUE)
  #saving population values into .csv for checking 
  write.csv(x_4, file = paste0("./Output_pop/Pop_ls",regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)),"_us_val.csv"))
}


####Testing function on files for population extraction ####
for (f in filenames){
  print(f)
  tic("Population Extraction - US_states") #start timing of extraction function
  extract_wld_us(f)
  toc(log = TRUE, quiet = TRUE)#closing timing and logging times
}


####100km population extraction - Function 2####
extract_100_us <-function(filenames) 
{
  dat<-velox(filenames, crs="+proj=longlat +datum=WGS84 +no_defs") #load file
  #clip raster prior to extraction
  x_1 <- dat$extract(us_buffer_100km, fun=function(t) sum(t,na.rm=TRUE)) #extraction from velox pkg
  #need to unlist the extracted values and sum the pop den
  x_2 <- unlist(lapply(x_1,function(x) if (!is.null(x)) sum(x, na.rm=TRUE) else NA))
  #join unlisted values to df
  x_3 <- cbind(us_buffer_100km, x_2)
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
  st_write(x_3, dsn = paste0("./Output_pop/Coral_pop_ls",regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)),"_us_100.gpkg"), delete_dsn =TRUE)
  #saving population values into .csv for checking 
  write.csv(x_4, file = paste0("./Output_pop/Pop_ls",regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)),"_us_100.csv"))
}



for (f in filenames){
  print(f)
  tic("Population Extraction - 100km") #start timing of extraction function
  extract_100_us(f)
  toc(log = TRUE, quiet = TRUE)#closing timing and logging times
}


####50km population extraction - Function 3####
extract_50_us <-function(filenames) 
{
  dat<-velox(filenames, crs="+proj=longlat +datum=WGS84 +no_defs") #load file
  #clip raster prior to extraction
  x_1 <- dat$extract(us_buffer_50km, fun=function(t) sum(t,na.rm=TRUE)) #extraction from velox pkg
  #need to unlist the extracted values and sum the pop den
  x_2 <- unlist(lapply(x_1,function(x) if (!is.null(x)) sum(x, na.rm=TRUE) else NA))
  #join unlisted values to df
  x_3 <- cbind(us_buffer_50km, x_2)
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
  st_write(x_3, dsn = paste0("./Output_pop/Coral_pop_ls",regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)),"_us_50.gpkg"), delete_dsn =TRUE)
  #saving population values into .csv for checking 
  write.csv(x_4, file = paste0("./Output_pop/Pop_ls",regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)),"_us_50.csv"))
}


for (f in filenames){
  print(f)
  tic("Population Extraction - 50km") #start timing of extraction function
  extract_50_us(f)
  toc(log = TRUE, quiet = TRUE)#closing timing and logging times
}

####30km population extraction - Function 4####
extract_30_us <-function(filenames) 
{
  dat<-velox(filenames, crs="+proj=longlat +datum=WGS84 +no_defs") #load file
  #clip raster prior to extraction
  x_1 <- dat$extract(us_buffer_30km, fun=function(t) sum(t,na.rm=TRUE)) #extraction from velox pkg
  #need to unlist the extracted values and sum the pop den
  x_2 <- unlist(lapply(x_1,function(x) if (!is.null(x)) sum(x, na.rm=TRUE) else NA))
  #join unlisted values to df
  x_3 <- cbind(us_buffer_30km, x_2)
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
  st_write(x_3, dsn = paste0("./Output_pop/Coral_pop_ls",regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)),"_us_30.gpkg"), delete_dsn =TRUE)
  #saving population values into .csv for checking 
  write.csv(x_4, file = paste0("./Output_pop/Pop_ls",regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)),"_us_30.csv"))
}


for (f in filenames){
  print(f)
  tic("Population Extraction - 30km") #start timing of extraction function
  extract_30_us(f)
  toc(log = TRUE, quiet = TRUE)#closing timing and logging times
}

####10km population extraction - Function 5####
extract_10_us <-function(filenames) 
{
  dat<-velox(filenames, crs="+proj=longlat +datum=WGS84 +no_defs") #load file
  #clip raster prior to extraction
  x_1 <- dat$extract(us_buffer_10km, fun=function(t) sum(t,na.rm=TRUE)) #extraction from velox pkg
  #need to unlist the extracted values and sum the pop den
  x_2 <- unlist(lapply(x_1,function(x) if (!is.null(x)) sum(x, na.rm=TRUE) else NA))
  #join unlisted values to df
  x_3 <- cbind(us_buffer_10km, x_2)
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
  st_write(x_3, dsn = paste0("./Output_pop/Coral_pop_ls",regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)),"_us_10.gpkg"), delete_dsn =TRUE)
  #saving population values into .csv for checking 
  write.csv(x_4, file = paste0("./Output_pop/Pop_ls",regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)),"_us_10.csv"))
}


for (f in filenames){
  print(f)
  tic("Population Extraction - 10km") #start timing of extraction function
  extract_10_us(f)
  toc(log = TRUE, quiet = TRUE)#closing timing and logging times
}


####5km population extraction - Function 6####
extract_5_us <-function(filenames) 
{
  dat<-velox(filenames, crs="+proj=longlat +datum=WGS84 +no_defs") #load file
  #clip raster prior to extraction
  x_1 <- dat$extract(us_buffer_5km, fun=function(t) sum(t,na.rm=TRUE)) #extraction from velox pkg
  #need to unlist the extracted values and sum the pop den
  x_2 <- unlist(lapply(x_1,function(x) if (!is.null(x)) sum(x, na.rm=TRUE) else NA))
  #join unlisted values to df
  x_3 <- cbind(us_buffer_5km, x_2)
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
  st_write(x_3, dsn = paste0("./Output_pop/Coral_pop_ls",regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)),"_us_5.gpkg"), delete_dsn =TRUE)
  #saving population values into .csv for checking 
  write.csv(x_4, file = paste0("./Output_pop/Pop_ls",regmatches(filenames, regexpr("[0-9].*[0-9]",filenames)),"_us_5.csv"))
}


for (f in filenames){
  print(f)
  tic("Population Extraction - 5km") #start timing of extraction function
  extract_5_us(f)
  toc(log = TRUE, quiet = TRUE)#closing timing and logging times
}

