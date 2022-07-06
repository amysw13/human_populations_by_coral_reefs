#population merging and wrangling

#load packages
library(dplyr)

##Population data merging 

#file locations of all 2020 buffer population data
ls2020_files<- list.files(path = "./Output_pop/",  
                      pattern = ".*_ls2020_[0-9].*\\.csv", #this matches the us files for distance buffers
                      full.names = TRUE,
                      recursive = TRUE)

#load files
ls2020_files <- lapply(ls2020_files, read.csv)

#merge files to one dataframe
ls_2020_pop <-dplyr::bind_rows(ls2020_files)

#ensure distances are ordered correctly 
ls_2020_pop$Distance<-factor(ls_2020_pop$Distance, levels = c("5km", "10km", "30km", "50km", "100km"))

#rename col 
colnames(ls_2020_pop)[colnames(ls_2020_pop)=="buffer_area_km2"] <- "buffer_area"


#2019 data
ls2019_files<- list.files(path = "./Output_pop/",  
                          pattern = ".*_ls2019_[0-9].*\\.csv", #this matches the us files for distance buffers
                          full.names = TRUE,
                          recursive = TRUE)

ls2019_files <- lapply(ls2019_files, read.csv)

ls_2019_pop <-dplyr::bind_rows(ls2019_files)

ls_2019_pop$Distance<-factor(ls_2019_pop$Distance, levels = c("5km", "10km", "30km", "50km", "100km"))

colnames(ls_2019_pop)[colnames(ls_2019_pop)=="buffer_area_km2"] <- "buffer_area"


##Creating world summaries of population

#2020 population data
ls_2020_wld_pop<-ls_2020_pop%>%
  group_by(Distance, Year)%>%
  dplyr::summarise(pop = sum(pop, na.rm = TRUE), area = sum(buffer_area, na.rm = TRUE),
            ISO3 = "WLD", NAME_0 = "World")


#2019 data

ls_2019_wld_pop<-ls_2019_pop%>%
  group_by(Distance, Year)%>%
  dplyr::summarise(pop = sum(pop, na.rm = TRUE), area = sum(buffer_area, na.rm = TRUE),
                   ISO3 = "WLD", NAME_0 = "World")

