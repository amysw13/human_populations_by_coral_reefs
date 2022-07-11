#population merging and wrangling
#Examples for 2020 ans 2019 years

#Set up####
#load packages
library(dplyr)
library(tibble)
library("wbstats") #to obtain World Bank data
library(tidyverse)

##create function for percentage change 
pct <- function(x) {(x/lag(x) - 1) * 100}

##Population data merging####

#file locations of all 2020 buffer population data
ls2020_files<- list.files(path = "./Output_pop/",  
                      pattern = ".*_ls2020_[0-9].*\\.csv", #this matches the us files for distance buffers
                      full.names = TRUE,
                      recursive = TRUE)

#load files
ls2020_files <- lapply(ls2020_files, read_csv)
#merge files to one dataframe
ls_2020_pop <-dplyr::bind_rows(ls2020_files)
#To some tidying to the data
ls_2020_pop$Year<-as.integer(ls_2020_pop$Year)
ls_2020_pop$Distance<-factor(ls_2020_pop$Distance, levels = c("5km", "10km", "30km", "50km", "100km"))
#rename col 
colnames(ls_2020_pop)[colnames(ls_2020_pop)=="buffer_area_km2"] <- "buffer_area"

#Add attribute data
ls_2020_pop$region<-coral_countries_info$Region_coral[match(ls_2020_pop$ISO3, coral_countries_info$ISO3)]
ls_2020_pop$SID<-coral_countries_info$SID[match(ls_2020_pop$ISO3, coral_countries_info$ISO3)]
ls_2020_pop$Income_Group<-coral_countries_info$Income_Group[match(ls_2020_pop$ISO3, coral_countries_info$ISO3)]

#US population files 2020 - prepare for merging later
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

#Add US state - total population values
us_state_2020<- list.files(path = "./Output_pop/",  
                            pattern = ".*2020_us_val.*\\.csv", #this matches the us files for distance buffers
                            full.names = TRUE,
                            recursive = TRUE)

#load files
us_state_2020 <- lapply(us_state_2020, read.csv)
#merge files to one dataframe
ls_2020_pop_state <-dplyr::bind_rows(us_state_2020)

#Select only wanted cols
ls_2020_pop_state<-ls_2020_pop_state%>%
  rename(ISO3 = GID_0,
         buffer_area = area_km2,
         SP.POP.TOTL = Total_pop)%>%
  dplyr::select(ISO3, NAME_0, NAME_1, Year, buffer_area, SP.POP.TOTL)

#Ensure that the states have the corresponding region set up
ls_2020_pop_state<-tibble::add_column(ls_2020_pop_state, region = NA)
ls_2020_pop_state$region<-as.character(ls_2020_pop_state$region)
ls_2020_pop_state$NAME_1<-as.character(ls_2020_pop_state$NAME_1)
ls_2020_pop_state$SP.POP.TOTL<-as.numeric(ls_2020_pop_state$SP.POP.TOTL)

ls_2020_pop_state<-ls_2020_pop_state%>%
  mutate(region = case_when(
    NAME_1 == "Florida" & is.na(region) ~ "Atlantic",
    NAME_1 == "Hawaii" & is.na(region) ~ "Pacific",
    NAME_1 == "California" & is.na(region) ~ "Pacific",
    NAME_1 == "Arizona" & is.na(region) ~ "Pacific",
    TRUE ~region
  ))

#Add state total data 
ls_2020_pop_us<-ls_2020_pop_us%>%
    mutate(SP.POP.TOTL= ifelse(match(ls_2020_pop_us$NAME_1, ls_2020_pop_state$NAME_1), ls_2020_pop_state$SP.POP.TOTL, 0))%>%
  dplyr::select(-X,-NAME_1)%>%
group_by(ISO3, NAME_0, Distance, Year, region)%>%
  summarise(pop = sum(pop),
         buffer_area = sum(buffer_area),
        SP.POP.TOTL = sum(SP.POP.TOTL))%>%
  ungroup()

#add coral reef country wide population extractions
ls_2020_pop_cc <-read.csv("./Output_pop/Pop_ls2020_world_val.csv")
colnames(ls_2020_pop_cc)[colnames(ls_2020_pop_cc)=="Total_pop"] <- "SP.POP.TOTL"
ls_2020_pop_cc$SP.POP.TOTL<-as.numeric(ls_2020_pop_cc$SP.POP.TOTL)
ls_2020_pop_cc$Year<-as.integer(ls_2020_pop_cc$Year)
#select only cols needed
ls_2020_pop_cc <- ls_2020_pop_cc %>%
  dplyr::select("ISO3", "SP.POP.TOTL")%>%
  dplyr::filter(!ISO3 %in% "USA") #removing USA here, so doesn't clash with next steps
#add country totals to dataframe
ls_2020_pop$SP.POP.TOTL<-ls_2020_pop_cc$SP.POP.TOTL[match(ls_2020_pop$ISO3, ls_2020_pop_cc$ISO3)]

#due to USA split into states need to save a "raw" country file - add US data later
write_csv(ls_2020_pop, "./Output_pop/ls_2020_pop_us_raw")


#2019 data####
ls2019_files<- list.files(path = "./Output_pop/",  
                          pattern = ".*_ls2019_[0-9].*\\.csv", #this matches the us files for distance buffers
                          full.names = TRUE,
                          recursive = TRUE)

ls2019_files <- lapply(ls2019_files, read_csv)
ls_2019_pop <-dplyr::bind_rows(ls2019_files)
ls_2019_pop$Year<-as.integer(ls_2019_pop$Year)
ls_2019_pop$Distance<-factor(ls_2019_pop$Distance, levels = c("5km", "10km", "30km", "50km", "100km"))
colnames(ls_2019_pop)[colnames(ls_2019_pop)=="buffer_area_km2"] <- "buffer_area"

ls_2019_pop$region<-coral_countries_info$Region_coral[match(ls_2019_pop$ISO3, coral_countries_info$ISO3)]
ls_2019_pop$SID<-coral_countries_info$SID[match(ls_2019_pop$ISO3, coral_countries_info$ISO3)]
ls_2019_pop$Income_Group<-coral_countries_info$Income_Group[match(ls_2019_pop$ISO3, coral_countries_info$ISO3)]

ls2019_files_us<- list.files(path = "./Output_pop/",  
                             pattern = ".*_ls2019_us_[0-9].*\\.csv", #this matches the us files for distance buffers
                             full.names = TRUE,
                             recursive = TRUE)

ls2019_files_us <- lapply(ls2019_files_us, read_csv)

ls_2019_pop_us <-dplyr::bind_rows(ls2019_files_us)

ls_2019_pop_us<-tibble::add_column(ls_2019_pop_us, region = NA)
ls_2019_pop_us$region<-as.character(ls_2019_pop_us$region)
ls_2019_pop_us$NAME_1<-as.character(ls_2019_pop_us$NAME_1)
ls_2019_pop_us$Year<-as.integer(ls_2019_pop_us$Year)

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

us_state_2019<- list.files(path = "./Output_pop/",  
                           pattern = ".*2019_us_val.*\\.csv", #this matches the us files for distance buffers
                           full.names = TRUE,
                           recursive = TRUE)

us_state_2019 <- lapply(us_state_2019 , read.csv)
ls_2019_pop_state <-dplyr::bind_rows(us_state_2019)

ls_2019_pop_state<-ls_2019_pop_state%>%
  rename(ISO3 = GID_0,
         buffer_area = area_km2,
         SP.POP.TOTL = Total_pop)%>%
  dplyr::select(ISO3, NAME_0, NAME_1, Year, buffer_area, SP.POP.TOTL)

ls_2019_pop_state<-tibble::add_column(ls_2019_pop_state, region = NA)
ls_2019_pop_state$region<-as.character(ls_2019_pop_state$region)
ls_2019_pop_state$NAME_1<-as.character(ls_2019_pop_state$NAME_1)
ls_2019_pop_state$SP.POP.TOTL<-as.numeric(ls_2019_pop_state$SP.POP.TOTL)

ls_2019_pop_state<-ls_2019_pop_state%>%
  mutate(region = case_when(
    NAME_1 == "Florida" & is.na(region) ~ "Atlantic",
    NAME_1 == "Hawaii" & is.na(region) ~ "Pacific",
    NAME_1 == "California" & is.na(region) ~ "Pacific",
    NAME_1 == "Arizona" & is.na(region) ~ "Pacific",
    TRUE ~region
  ))

#ls_2019_pop_us<-ls_2019_pop_us%>%
  mutate(SP.POP.TOTL= ifelse(match(ls_2019_pop_us$NAME_1, ls_2019_pop_state$NAME_1), ls_2019_pop_state$SP.POP.TOTL, 0))%>%
  dplyr::select(-X1,-NAME_1)%>%
  group_by(ISO3, NAME_0, Distance, Year, region)%>%
  summarise(pop = sum(pop),
            buffer_area = sum(buffer_area),
            SP.POP.TOTL = sum(SP.POP.TOTL))%>%
  ungroup()

ls_2019_pop_cc <-read.csv("./Output_pop/Pop_ls2019_world_val.csv")
colnames(ls_2019_pop_cc)[colnames(ls_2019_pop_cc)=="Total_pop"] <- "SP.POP.TOTL"
ls_2019_pop_cc$SP.POP.TOTL<-as.numeric(ls_2019_pop_cc$SP.POP.TOTL)
ls_2019_pop_cc$Year<-as.integer(ls_2019_pop_cc$Year)

ls_2019_pop_cc <- ls_2019_pop_cc %>%
  dplyr::select("ISO3", "SP.POP.TOTL")%>%
  dplyr::filter(!ISO3 %in% "USA") #removing USA here, so doesn't clash with next steps

ls_2019_pop$SP.POP.TOTL<-ls_2019_pop_cc$SP.POP.TOTL[match(ls_2019_pop$ISO3, ls_2019_pop_cc$ISO3)]


write_csv(ls_2019_pop, "./Output_pop/ls_2019_pop_us_raw")


##Joining to 2000 to 2018 dataframes####

#load in files
coral_pop_all_data_pct<-read_csv("./Output_pop/coral_pop_all_data_pct.csv") #country data
coral_pop_all_world_data_pct<-read_csv("./Output_pop/coral_pop_all_world_data_pct.csv") #global data

#clean up dataframes a little to be more streamlined
coral_pop_all_data_pct<-coral_pop_all_data_pct%>%
  dplyr::select(-count,-ISO2, -Sovereignty, -Sovereignty_ISO3, -Sovereignty_ISO2, -Ocean_Region)%>%
  rename(buffer_area = area)

coral_pop_all_data_pct$SP.POP.TOTL<-as.numeric(coral_pop_all_data_pct$SP.POP.TOTL)

coral_pop_all_world_data_pct<-coral_pop_all_world_data_pct%>%
  dplyr::select(-count,-ISO2, -Sovereignty, -Sovereignty_ISO3, -Sovereignty_ISO2, -Ocean_Region)%>%
  rename(buffer_area = area)

#Get population growth values from The World Bank####
pop_grow_20<- wb(indicator = "SP.POP.GROW", startdate = 2020, enddate = 2020)

pop_grow_20<-pop_grow_20%>%
  dplyr::select("iso3c", "date", "value")%>%
  rename("ISO3" = "iso3c",
         "Year" = "date",
         "SP.POP.GROW" = "value")

pop_grow_20$Year<-as.integer(pop_grow_20$Year)


pop_grow_19<- wb(indicator = "SP.POP.GROW", startdate = 2019, enddate = 2019)

pop_grow_19<-pop_grow_19%>%
  dplyr::select("iso3c", "date", "value")%>%
  rename("ISO3" = "iso3c",
         "Year" = "date",
         "SP.POP.GROW" = "value")

pop_grow_19$Year<-as.integer(pop_grow_19$Year)

#Join country level dataframes####

#Amending data for US calculations
#crc_pop_all - contains US extracted data - need to replace with State extracted data

#Replace USA with state extracted populations and add pop growth
us_2020<-ls_2020_pop%>%
  dplyr::filter(!ISO3 %in% "USA")%>%
  left_join(ls_2020_pop_us)%>%
  left_join(pop_grow_20)

us_2020$SID<-coral_countries_info$SID[match(us_2020$ISO3, coral_countries_info$ISO3)]
us_2020$Income_Group<-coral_countries_info$Income_Group[match(us_2020$ISO3, coral_countries_info$ISO3)]

#Replace USA with state extracted populations and add pop growth
us_2019<-ls_2019_pop%>%
  dplyr::filter(!ISO3 %in% "USA")%>%
  left_join(ls_2019_pop_us)%>%
  left_join(pop_grow_19)

us_2019$SID<-coral_countries_info$SID[match(us_2019$ISO3, coral_countries_info$ISO3)]
us_2019$Income_Group<-coral_countries_info$Income_Group[match(us_2019$ISO3, coral_countries_info$ISO3)]


#Join to rest of data
coral_pop_all_data_pct$Year<-as.integer(coral_pop_all_data_pct$Year)
crc_pop_all<-full_join(coral_pop_all_data_pct, us_2020)
crc_pop_all<-full_join(crc_pop_all, us_2019)

#amend all old US values to ensure the correct values have been taken throughout####

us_buffer_pop<-read_csv( "./Output_pop/us_buffer_pop.csv")

us_buffer_pop<-us_buffer_pop%>%
  dplyr::filter(!Distance %in% "1km", !Year %in% 3018)%>%
  rename(ISO3 = GID_0,
         buffer_area = buffer_area_km2)%>%
  dplyr::select(ISO3, NAME_0, NAME_1, Distance, Year, buffer_area, pop)

us_buffer_pop<-tibble::add_column(us_buffer_pop, region = NA)
us_buffer_pop$region<-as.character(us_buffer_pop$region)
us_buffer_pop$NAME_1<-as.character(us_buffer_pop$NAME_1)
us_buffer_pop$pop<-as.numeric(us_buffer_pop$pop)

us_buffer_pop<-us_buffer_pop%>%
  mutate(region = case_when(
    NAME_1 == "Florida" & is.na(region) ~ "Atlantic",
    NAME_1 == "Hawaii" & is.na(region) ~ "Pacific",
    NAME_1 == "California" & is.na(region) ~ "Pacific",
    NAME_1 == "Arizona" & is.na(region) ~ "Pacific",
    TRUE ~region
  ))

us_state_pop<-read_csv( "./Output_pop/us_state_pop.csv")

us_state_pop<-us_state_pop%>%
  filter(!Year %in% 3018)%>%
  rename(ISO3 = GID_0,
         buffer_area = area_km2,
         SP.POP.TOTL = Total_pop)%>%
  dplyr::select(ISO3, NAME_0, NAME_1, Year, SP.POP.TOTL)

us_state_pop<-tibble::add_column(us_state_pop, region = NA)
us_state_pop$region<-as.character(us_state_pop$region)
us_state_pop$NAME_1<-as.character(us_state_pop$NAME_1)
us_state_pop$SP.POP.TOTL<-as.numeric(us_state_pop$SP.POP.TOTL)

us_state_pop<-us_state_pop%>%
  mutate(region = case_when(
    NAME_1 == "Florida" & is.na(region) ~ "Atlantic",
    NAME_1 == "Hawaii" & is.na(region) ~ "Pacific",
    NAME_1 == "California" & is.na(region) ~ "Pacific",
    NAME_1 == "Arizona" & is.na(region) ~ "Pacific",
    TRUE ~region
  ))

us_pop_data<-left_join(us_buffer_pop, us_state_pop)

#add cols to make joining easier
us_pop_data<-tibble::add_column(us_pop_data, pop_prop = NA)
us_pop_data<-tibble::add_column(us_pop_data, pop_den = NA)
us_pop_data<-tibble::add_column(us_pop_data, pop_change = NA)
us_pop_data<-tibble::add_column(us_pop_data, SP.POP.GROW = NA)

us_pop_data$SID<-coral_countries_info$SID[match(us_pop_data$ISO3, coral_countries_info$ISO3)]
us_pop_data$Income_Group<-coral_countries_info$Income_Group[match(us_pop_data$ISO3, coral_countries_info$ISO3)]


crc_pop_all<-crc_pop_all%>%
  #dplyr::select(-X1)%>%
  dplyr::filter(!ISO3 %in% "USA")%>%
  rbind(us_pop_data)


#remove 1km distances
crc_pop_all<-crc_pop_all%>%
  dplyr::filter(!Distance %in% "1km", !pop == 0)

#Ensure AUS has income group filled in.
crc_pop_all$Income_Group[crc_pop_all$ISO3 == "AUS"]<-"High income"

#Save dataframe for creating final plotting dataframes
write_csv(crc_pop_all, "./data/crc_pop_all.csv")


##Creating world summaries of population####

#Downloading Global populations for calculating actual Global population proportion

pop_tot_20<- wb(indicator = "SP.POP.TOTL", startdate = 2020, enddate = 2020)

pop_tot_20<-pop_tot_20%>%
  dplyr::select("iso3c", "date", "value")%>%
  rename("ISO3" = "iso3c",
         "Year" = "date",
         "SP.POP.TOTL" = "value")

pop_tot_20<-pop_tot_20%>%
  dplyr::filter(ISO3 %in% "WLD")
  
pop_tot_20$Year<-as.integer(pop_tot_20$Year)


pop_tot_19<- wb(indicator = "SP.POP.TOTL", startdate = 2019, enddate = 2019)

pop_tot_19<-pop_tot_19%>%
  dplyr::select("iso3c", "date", "value")%>%
  rename("ISO3" = "iso3c",
         "Year" = "date",
         "SP.POP.TOTL" = "value")

pop_tot_19<-pop_tot_19%>%
  dplyr::filter(ISO3 %in% "WLD")

pop_tot_19$Year<-as.integer(pop_tot_19$Year)

#Insert correct pop grow

pop_grow_20_wld<-pop_grow_20%>%
  dplyr::filter(ISO3 %in% "WLD")

pop_grow_19_wld<-pop_grow_19%>%
  dplyr::filter(ISO3 %in% "WLD")

#2020 population data
ls_2020_wld_pop<-us_2020%>%
  group_by(Distance, Year)%>%
  dplyr::filter(!pop ==0 )%>%
  dplyr::summarise(n_countries = length(unique(ISO3)),
                   pop = sum(pop, na.rm = TRUE), 
                   buffer_area = sum(buffer_area, na.rm = TRUE),
                   ISO3 = "WLD", NAME_0 = "World")%>%
  dplyr::mutate(SP.POP.TOTL = ifelse(ISO3 %in% "WLD", pop_tot_20$SP.POP.TOTL, SP.POP.TOTL),
                SP.POP.GROW = ifelse(ISO3 %in% "WLD", pop_grow_20_wld$SP.POP.GROW, SP.POP.GROW),
                pop_prop = (sum(pop)/sum(SP.POP.TOTL, na.rm =TRUE))*100,
                pop_den = (pop/buffer_area))

#2019 data
ls_2019_wld_pop<-us_2019%>%
  group_by(Distance, Year)%>%
  dplyr::filter(!pop ==0 )%>%
  dplyr::summarise(n_countries = length(unique(ISO3)),
                   pop = sum(pop, na.rm = TRUE), 
                   buffer_area = sum(buffer_area, na.rm = TRUE),
                   ISO3 = "WLD", NAME_0 = "World")%>%
  dplyr::mutate(SP.POP.TOTL = ifelse(ISO3 %in% "WLD", pop_tot_19$SP.POP.TOTL, SP.POP.TOTL),
                SP.POP.GROW = ifelse(ISO3 %in% "WLD", pop_grow_19_wld$SP.POP.GROW, SP.POP.GROW),
                pop_prop = (sum(pop)/sum(SP.POP.TOTL, na.rm =TRUE))*100,
                pop_den = (pop/buffer_area))


#Join Global level dataframes
coral_pop_all_world_data_pct$Year<-as.integer(coral_pop_all_world_data_pct$Year)
wld_pop_all<-full_join(coral_pop_all_world_data_pct, ls_2019_wld_pop)
wld_pop_all<-full_join(wld_pop_all, ls_2020_wld_pop)

#calculate pop stats
wld_pop_all<-wld_pop_all%>%
  group_by(ISO3, Distance, Year)%>%
  mutate(country_pop = sum(pop),
         country_area_km2 = sum(buffer_area))%>%
  ungroup()

wld_pop_all<-wld_pop_all%>%
  group_by(ISO3, Distance)%>%
  arrange(Year)%>%
  mutate(pop_change = pct(pop),
         pop_prop = (pop/SP.POP.TOTL)*100,)%>%
  ungroup()


#remove 1km distances
wld_pop_all<-wld_pop_all%>%
  dplyr::filter(!Distance %in% "1km")


wld_pop_all<-wld_pop_all%>%
  group_by(Distance) %>%
  mutate(Average_growth = mean(pop_change, na.rm = TRUE))


#Save dataframe for plotting
write_csv(wld_pop_all, "./data/wld_pop_all.csv")

##Create dataframes for plotting, country, region, income_group and SID####
#crc_pop_all

#country stats

country_pop_all<-crc_pop_all%>%
    group_by(ISO3, Distance, Year)%>%
  summarise(pop = sum(pop, na.rm =TRUE), 
            SP.POP.TOTL = sum(SP.POP.TOTL, na.rm =TRUE),
            pop_prop = (sum(pop)/sum(SP.POP.TOTL, na.rm =TRUE))*100,
            tot_area = sum(buffer_area, na.rm = TRUE))%>%
  ungroup()


country_pop_all<-country_pop_all%>%
  group_by(ISO3, Distance)%>%
  arrange(Year)%>%
  mutate(pop_change = pct(pop),
         pop_den = (pop/tot_area))%>%
  ungroup()

country_pop_all$NAME_0<-coral_countries_info$Country_Name[match(country_pop_all$ISO3, coral_countries_info$ISO3)]
country_pop_all$region<-coral_countries_info$Region_coral[match(country_pop_all$ISO3, coral_countries_info$ISO3)]
country_pop_all$SID<-coral_countries_info$SID[match(country_pop_all$ISO3, coral_countries_info$ISO3)]
country_pop_all$Income_Group<-coral_countries_info$Income_Group[match(country_pop_all$ISO3, coral_countries_info$ISO3)]
country_pop_all$Populated<-coral_countries_info$Populated[match(country_pop_all$ISO3, coral_countries_info$ISO3)]

#save region data
write_csv(country_pop_all, "./data/country_pop_all.csv")

#region
region_pop_all<-crc_pop_all%>%
  group_by(Distance, Year, region)%>%
  summarise(n_countries = length(unique(ISO3)), #note may need to change this once added US values... 
            pop = sum(pop, na.rm =TRUE), 
            SP.POP.TOTL = sum(SP.POP.TOTL, na.rm =TRUE),
            pop_prop = (sum(pop)/sum(SP.POP.TOTL, na.rm =TRUE))*100,
            tot_area = sum(buffer_area, na.rm = TRUE))%>%
  ungroup()

region_pop_all<-region_pop_all%>%#
  group_by(Distance,  region)%>%
  arrange(Year)%>%
  mutate(pop_change = pct(pop),
         pop_den = pop/tot_area)%>%
  ungroup()

region_pop_all <-region_pop_all%>%
  group_by(region, Distance) %>%
  mutate(Average_growth = mean(pop_change, na.rm = TRUE))

#save region data
write_csv(region_pop_all, "./data/region_pop_all.csv")

#Income group
IG_pop_all<-crc_pop_all%>%
  dplyr::filter(!Income_Group %in% NA)%>%
  group_by(Distance, Year, Income_Group)%>%
  summarise(n_countries = length(unique(ISO3)), #note may need to change this once added US values... 
            pop = sum(pop, na.rm =TRUE), 
            SP.POP.TOT = sum(SP.POP.TOTL, na.rm =TRUE),
            pop_prop = (sum(pop)/sum(SP.POP.TOTL, na.rm =TRUE))*100,
            tot_area = sum(buffer_area, na.rm = TRUE))%>%
  ungroup()

IG_pop_all<-IG_pop_all%>%#
  group_by(Distance, Income_Group)%>%
  arrange(Year)%>%
  mutate(pop_change = pct(pop),
         pop_den = pop/tot_area)%>%
  ungroup()

IG_pop_all<- IG_pop_all%>%
  group_by(Income_Group, Distance)%>%
  arrange(Year)%>%
  mutate( Average_growth = mean(pop_change, na.rm = TRUE))%>%
  ungroup()


#save income group data
write_csv(IG_pop_all, "./data/IG_pop_all.csv")

#SIDS
SIDS_pop_all<-crc_pop_all%>%
  dplyr::filter(SID %in% "Yes")%>%
  group_by(Distance, Year)%>%
  summarise(n_countries = length(unique(ISO3)), #note may need to change this once added US values... 
            pop = sum(pop, na.rm =TRUE), 
            SP.POP.TOT = sum(SP.POP.TOTL, na.rm =TRUE),
            pop_prop = (sum(pop)/sum(SP.POP.TOTL, na.rm =TRUE))*100,
            tot_area = sum(buffer_area, na.rm = TRUE))%>%
  ungroup()

SIDS_pop_all<-SIDS_pop_all%>%#
  group_by(Distance)%>%
  arrange(Year)%>%
  mutate(pop_change = pct(pop),
         pop_den = pop/tot_area)%>%
  ungroup()


SIDS_pop_all <-SIDS_pop_all%>%
  group_by(Distance) %>%
  mutate(Average_growth = mean(pop_change, na.rm = TRUE))

#save income group data
write_csv(SIDS_pop_all, "./data/SIDS_pop_all.csv")
