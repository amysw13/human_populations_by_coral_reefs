#population merging and wrangling
#Examples for 2020 ans 2019 years

#load packages
library(dplyr)
library(tibble)
library("wbstats") #to obtain World Bank data

##create function for percentage change 
pct <- function(x) {(x/lag(x) - 1) * 100}

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

#US population files 2020
ls2020_files_us<- list.files(path = "./Output_pop/",  
                          pattern = ".*_ls2020_us_[0-9].*\\.csv", #this matches the us files for distance buffers
                          full.names = TRUE,
                          recursive = TRUE)

#load files
ls2020_files_us <- lapply(ls2020_files_us, read.csv)
#merge files to one dataframe
ls_2020_pop_us <-dplyr::bind_rows(ls2020_files_us)
#Ensure that the states have the corresponding region set up
ls_2020_pop_us<-tibble::add_column(ls_2020_pop_us, region = NA)
ls_2020_pop_us$region<-as.character(ls_2020_pop_us$region)
ls_2020_pop_us$NAME_1<-as.character(ls_2020_pop_us$NAME_1)
#add the regions
ls_2020_pop_us<-ls_2020_pop_us%>%
    mutate(region = case_when(
    NAME_1 == "Florida" & is.na(region) ~ "Atlantic",
    NAME_1 == "Hawaii" & is.na(region) ~ "Pacific",
    NAME_1 == "California" & is.na(region) ~ "Pacific",
    NAME_1 == "Arizona" & is.na(region) ~ "Pacific",
    TRUE ~region
  ))

#ensure distances are ordered correctly 
ls_2020_pop_us$Distance<-factor(ls_2020_pop_us$Distance, levels = c("5km", "10km", "30km", "50km", "100km"))
#rename cols
colnames(ls_2020_pop_us)[colnames(ls_2020_pop_us)=="buffer_area_km2"] <- "buffer_area"
colnames(ls_2020_pop_us)[colnames(ls_2020_pop_us)=="GID_0"] <- "ISO3"

#Replace USA with state extracted population
ls_2020_pop<-ls_2020_pop%>%
  dplyr::filter(!ISO3 %in% "USA")%>%
  full_join(ls_2020_pop_us)




#Add attribute data
ls_2020_pop$region<-coral_countries_info$Region_coral[match(ls_2020_pop$ISO3, coral_countries_info$ISO3)]
ls_2020_pop$SID<-coral_countries_info$SID[match(ls_2020_pop$ISO3, coral_countries_info$ISO3)]
ls_2020_pop$Income_Group<-coral_countries_info$Income_Group[match(ls_2020_pop$ISO3, coral_countries_info$ISO3)]




#2019 data
ls2019_files<- list.files(path = "./Output_pop/",  
                          pattern = ".*_ls2019_[0-9].*\\.csv", #this matches the us files for distance buffers
                          full.names = TRUE,
                          recursive = TRUE)

ls2019_files <- lapply(ls2019_files, read.csv)
ls_2019_pop <-dplyr::bind_rows(ls2019_files)
ls_2019_pop$Distance<-factor(ls_2019_pop$Distance, levels = c("5km", "10km", "30km", "50km", "100km"))
colnames(ls_2019_pop)[colnames(ls_2019_pop)=="buffer_area_km2"] <- "buffer_area"

#US population files 2019
ls2019_files_us<- list.files(path = "./Output_pop/",  
                             pattern = ".*_ls2019_us_[0-9].*\\.csv", #this matches the us files for distance buffers
                             full.names = TRUE,
                             recursive = TRUE)

ls2019_files_us <- lapply(ls2019_files_us, read.csv)

ls_2019_pop_us <-dplyr::bind_rows(ls2019_files_us)

ls_2019_pop_us<-tibble::add_column(ls_2019_pop_us, region = NA)
ls_2019_pop_us$region<-as.character(ls_2019_pop_us$region)
ls_2019_pop_us$NAME_1<-as.character(ls_2019_pop_us$NAME_1)

ls_2019_pop_us<-ls_2019_pop_us%>%
  mutate(region = case_when(
    NAME_1 == "Florida" & is.na(region) ~ "Atlantic",
    NAME_1 == "Hawaii" & is.na(region) ~ "Pacific",
    NAME_1 == "California" & is.na(region) ~ "Pacific",
    NAME_1 == "Arizona" & is.na(region) ~ "Pacific",
    TRUE ~region
  ))

ls_2019_pop_us$Distance<-factor(ls_2019_pop_us$Distance, levels = c("5km", "10km", "30km", "50km", "100km"))
colnames(ls_2019_pop_us)[colnames(ls_2019_pop_us)=="buffer_area_km2"] <- "buffer_area"
colnames(ls_2019_pop_us)[colnames(ls_2019_pop_us)=="GID_0"] <- "ISO3"

ls_2019_pop<-ls_2019_pop%>%
  dplyr::filter(!ISO3 %in% "USA")%>%
  full_join(ls_2019_pop_us)

ls_2019_pop$region<-coral_countries_info$Region_coral[match(ls_2019_pop$ISO3, coral_countries_info$ISO3)]
ls_2019_pop$SID<-coral_countries_info$SID[match(ls_2019_pop$ISO3, coral_countries_info$ISO3)]
ls_2019_pop$Income_Group<-coral_countries_info$Income_Group[match(ls_2019_pop$ISO3, coral_countries_info$ISO3)]

#add coral reef country wide population extractions
ls_2019_pop_cc <-read.csv("./Output_pop/Pop_ls2019_world_val.csv")
colnames(ls_2019_pop_cc)[colnames(ls_2019_pop_cc)=="Total_pop"] <- "SP.POP.TOTL"
ls_2019_pop_cc$SP.POP.TOTL<-as.numeric(ls_2019_pop_cc$SP.POP.TOTL)
#select only cols needed
ls_2019_pop_cc <- ls_2019_pop_cc %>%
  dplyr::select("ISO3", "NAME_0", "SP.POP.TOTL")
#join to existing dataframe
ls_2019_pop <- left_join(ls_2019_pop, ls_2019_pop_cc, by = c("ISO3", "NAME_0"))
#amend USA total population with state totals for 100km
US_total<-ls_2019_pop %>%
  dplyr::filter(ISO3 %in% "USA", Distance %in% "100km")%>%
  mutate(SP.POP.TOTL = sum(pop))
#replace USA total values
ls_2019_pop<-ls_2019_pop%>%
  mutate(SP.POP.TOTL = ifelse(ISO3 == "USA", US_total$SP.POP.TOTL, SP.POP.TOTL))

#Calculate more population statistics - pop den looks incorrect
ls_2019_pop<-ls_2019_pop%>%
  group_by(ISO3, Distance, Year)%>%
  mutate(pop_prop = (sum(pop)/sum(SP.POP.TOTL, na.rm =TRUE))*100,
         pop_den = (pop/buffer_area))%>%
  ungroup()
  
#remember to do this when joined with the rest of data!! 
ls_2019_pop<-ls_2019_pop%>%
  group_by(ISO3, Distance)%>%
  arrange(Year)%>%
  mutate(pop_change = pct(pop))%>%
  ungroup()



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

