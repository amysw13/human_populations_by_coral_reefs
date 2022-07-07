#population merging and wrangling
#Examples for 2020 ans 2019 years

#load packages
library(dplyr)
library(tibble)
library("wbstats") #to obtain World Bank data
library(tidyverse)

##create function for percentage change 
pct <- function(x) {(x/lag(x) - 1) * 100}

##Population data merging 

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

#add coral reef country wide population extractions
ls_2020_pop_cc <-read.csv("./Output_pop/Pop_ls2020_world_val.csv")
colnames(ls_2020_pop_cc)[colnames(ls_2020_pop_cc)=="Total_pop"] <- "SP.POP.TOTL"
ls_2020_pop_cc$SP.POP.TOTL<-as.numeric(ls_2020_pop_cc$SP.POP.TOTL)
ls_2020_pop_cc$Year<-as.integer(ls_2020_pop_cc$Year)
#select only cols needed
ls_2020_pop_cc <- ls_2020_pop_cc %>%
  dplyr::select("ISO3", "SP.POP.TOTL")
#join to existing dataframe
ls_2020_pop <- left_join(ls_2020_pop, ls_2020_pop_cc, by = c("ISO3"))
#amend USA total population with state totals for 100km
US_total<-ls_2020_pop %>%
  dplyr::filter(ISO3 %in% "USA", Distance %in% "100km")%>%
  mutate(SP.POP.TOTL = sum(pop))
#replace USA total values
ls_2020_pop<-ls_2020_pop%>%
  mutate(SP.POP.TOTL = ifelse(ISO3 == "USA", US_total$SP.POP.TOTL, SP.POP.TOTL))

#Calculate more population statistics - pop den looks incorrect
ls_2020_pop<-ls_2020_pop%>%
  group_by(ISO3, Distance, Year)%>%
  mutate(pop_prop = (sum(pop)/sum(SP.POP.TOTL, na.rm =TRUE))*100,
         pop_den = (pop/buffer_area))%>%
  ungroup()

#2019 data
ls2019_files<- list.files(path = "./Output_pop/",  
                          pattern = ".*_ls2019_[0-9].*\\.csv", #this matches the us files for distance buffers
                          full.names = TRUE,
                          recursive = TRUE)

ls2019_files <- lapply(ls2019_files, read_csv)
ls_2019_pop <-dplyr::bind_rows(ls2019_files)
ls_2019_pop$Year<-as.integer(ls_2019_pop$Year)
ls_2019_pop$Distance<-factor(ls_2019_pop$Distance, levels = c("5km", "10km", "30km", "50km", "100km"))
colnames(ls_2019_pop)[colnames(ls_2019_pop)=="buffer_area_km2"] <- "buffer_area"

#US population files 2019
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

ls_2019_pop<-ls_2019_pop%>%
  dplyr::filter(!ISO3 %in% "USA")%>%
  full_join(ls_2019_pop_us)

ls_2019_pop$region<-coral_countries_info$Region_coral[match(ls_2019_pop$ISO3, coral_countries_info$ISO3)]
ls_2019_pop$SID<-coral_countries_info$SID[match(ls_2019_pop$ISO3, coral_countries_info$ISO3)]
ls_2019_pop$Income_Group<-coral_countries_info$Income_Group[match(ls_2019_pop$ISO3, coral_countries_info$ISO3)]

ls_2019_pop_cc <-read.csv("./Output_pop/Pop_ls2019_world_val.csv")
colnames(ls_2019_pop_cc)[colnames(ls_2019_pop_cc)=="Total_pop"] <- "SP.POP.TOTL"
ls_2019_pop_cc$SP.POP.TOTL<-as.numeric(ls_2019_pop_cc$SP.POP.TOTL)
ls_2019_pop_cc$Year<-as.integer(ls_2019_pop_cc$Year)

ls_2019_pop_cc <- ls_2019_pop_cc %>%
  dplyr::select("ISO3", "SP.POP.TOTL")

ls_2019_pop <- left_join(ls_2019_pop, ls_2019_pop_cc, by = c("ISO3"))

US_total<-ls_2019_pop %>%
  dplyr::filter(ISO3 %in% "USA", Distance %in% "100km")%>%
  mutate(SP.POP.TOTL = sum(pop))

ls_2019_pop<-ls_2019_pop%>%
  mutate(SP.POP.TOTL = ifelse(ISO3 == "USA", US_total$SP.POP.TOTL, SP.POP.TOTL))

ls_2019_pop<-ls_2019_pop%>%
  group_by(ISO3, Distance, Year)%>%
  mutate(pop_prop = (sum(pop)/sum(SP.POP.TOTL, na.rm =TRUE))*100,
         pop_den = (pop/buffer_area))%>%
  ungroup()

##Creating world summaries of population
#2020 population data
ls_2020_wld_pop<-ls_2020_pop%>%
  group_by(Distance, Year)%>%
  dplyr::filter(!pop ==0 )%>%
  dplyr::summarise(n_countries = length(unique(ISO3)),
                  pop = sum(pop, na.rm = TRUE), 
                   buffer_area = sum(buffer_area, na.rm = TRUE),
            ISO3 = "WLD", NAME_0 = "World")%>%
  dplyr::mutate(SP.POP.TOTL = sum(ls_2020_pop_cc$SP.POP.TOTL),
                pop_prop = (sum(pop)/sum(SP.POP.TOTL, na.rm =TRUE))*100,
                pop_den = (pop/buffer_area))

#2019 data
ls_2019_wld_pop<-ls_2019_pop%>%
  group_by(Distance, Year)%>%
  dplyr::filter(!pop ==0 )%>%
  dplyr::summarise(n_countries = length(unique(ISO3)),
                    pop = sum(pop, na.rm = TRUE), 
                   buffer_area = sum(buffer_area, na.rm = TRUE),
                   ISO3 = "WLD", NAME_0 = "World")%>%
  dplyr::mutate(SP.POP.TOTL = sum(ls_2019_pop_cc$SP.POP.TOTL),
                pop_prop = (sum(pop)/sum(SP.POP.TOTL, na.rm =TRUE))*100,
                pop_den = (pop/buffer_area))

##Joining to 2000 to 2018 dataframes

#load in files
coral_pop_all_data_pct<-read_csv("./Output_pop/coral_pop_all_data_pct.csv") #country data
coral_pop_all_world_data_pct<-read_csv("./Output_pop/coral_pop_all_world_data_pct.csv") #global data

#clean up dataframes a little to be more streamlined
coral_pop_all_data_pct<-coral_pop_all_data_pct%>%
  dplyr::select(-count,-ISO2, -Sovereignty, -Sovereignty_ISO3, -Sovereignty_ISO2, -Ocean_Region)%>%
  rename(buffer_area = area)

coral_pop_all_world_data_pct<-coral_pop_all_world_data_pct%>%
  dplyr::select(-count,-ISO2, -Sovereignty, -Sovereignty_ISO3, -Sovereignty_ISO2, -Ocean_Region)%>%
  rename(buffer_area = area)

#Get population growth values from The World Bank
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

#Join country level dataframes
#ls_2019_pop
#ls_2020_pop
coral_pop_all_data_pct$Year<-as.integer(coral_pop_all_data_pct$Year)
crc_pop_all<-full_join(coral_pop_all_data_pct, ls_2019_pop)
crc_pop_all<-full_join(crc_pop_all, ls_2020_pop)

#calculate pop_change
crc_pop_all<-crc_pop_all%>%
  group_by(ISO3, Distance)%>%
  arrange(Year)%>%
  mutate(pop_change = pct(pop))%>%
  ungroup()

#Add Population growth data
crc_pop_all<-crc_pop_all%>%
  mutate(SP.POP.GROW = ifelse(Year == "2019", pop_grow_19$SP.POP.GROW, SP.POP.GROW),
         SP.POP.GROW = ifelse(Year == "2020", pop_grow_20$SP.POP.GROW, SP.POP.GROW))


#remove 1km distances
crc_pop_all<-crc_pop_all%>%
  dplyr::filter(!Distance %in% "1km", !pop == 0)

#Ensure AUS has income group filled in.
crc_pop_all$Income_Group[crc_pop_all$ISO3 == "AUS"]<-"High income"

#Save dataframe for plotting
write_csv(crc_pop_all, "./data/crc_pop_all.csv")

#Join Global level dataframes
coral_pop_all_world_data_pct$Year<-as.integer(coral_pop_all_world_data_pct$Year)
wld_pop_all<-full_join(coral_pop_all_world_data_pct, ls_2019_wld_pop)
wld_pop_all<-full_join(wld_pop_all, ls_2020_wld_pop)

#calculate pop_change
wld_pop_all<-wld_pop_all%>%
  group_by(ISO3, Distance)%>%
  arrange(Year)%>%
  mutate(pop_change = pct(pop))%>%
  ungroup()

#Add Population growth data
wld_pop_all<-wld_pop_all%>%
  mutate(SP.POP.GROW = ifelse(ISO3 == "WLD" & Year == "2019", pop_grow_19$SP.POP.GROW, SP.POP.GROW),
         SP.POP.GROW = ifelse(ISO3 == "WLD" & Year == "2020", pop_grow_20$SP.POP.GROW, SP.POP.GROW))

#remove 1km distances
wld_pop_all<-wld_pop_all%>%
  dplyr::filter(!Distance %in% "1km")

#Save dataframe for plotting
write_csv(wld_pop_all, "./data/wld.pop.all.csv")

##Create dataframes for plotting region, income_group and SIDS
#crc_pop_all

#region
region_pop_all<-crc_pop_all%>%
  group_by(Distance, Year, region)%>%
  summarise(n_countries = length(unique(ISO3)), #note may need to change this once added US values... 
            pop = sum(pop, na.rm =TRUE), 
            SP.POP.TOT = sum(SP.POP.TOTL, na.rm =TRUE),
            pop_prop = (sum(pop)/sum(SP.POP.TOTL, na.rm =TRUE))*100,
            tot_area = sum(buffer_area, na.rm = TRUE))%>%
  ungroup()

region_pop_all<-region_pop_all%>%#
  group_by(Distance,  region)%>%
  arrange(Year)%>%
  mutate(pop_change = pct(pop),
         pop_den = pop/tot_area)%>%
  ungroup()

#save region data
write_csv(region_pop_all, "./data/region.pop.all.csv")

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

#save income group data
write_csv(SIDS_pop_all, "./data/SIDS_pop_all.csv")
