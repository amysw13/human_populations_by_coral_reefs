####Plotting figures####
#Getting countries with 100% population proportion

#Set Up####
#loading packages
library(scales)
library(ggplot2)
library("rcartocolor")
#library(ggpmisc)
library(dplyr)
library(tibble)
#library(gridExtra)
library(ggpubr)
#library(ggforce)
library("patchwork")
library(tidyr)
library(cowplot)

#load in data for plotting
wld_pop_all<-read_csv( "./data/wld_pop_all.csv")
country_pop_all<-read_csv("./data/country_pop_all.csv")
region_pop_all<-read_csv( "./data/region_pop_all.csv")
IG_pop_all<-read_csv("./data/IG_pop_all.csv")
SIDS_pop_all<-read_csv( "./data/SIDS_pop_all.csv")

#order distances for plotting 
wld_pop_all$Distance<-factor(wld_pop_all$Distance, levels = c("5km", "10km", "30km", "50km", "100km"))
country_pop_all$Distance<-factor(country_pop_all$Distance, levels = c("5km", "10km", "30km", "50km", "100km"))
region_pop_all$Distance<-factor(region_pop_all$Distance, levels = c("5km", "10km", "30km", "50km", "100km"))
IG_pop_all$Distance<-factor(IG_pop_all$Distance, levels = c("5km", "10km", "30km", "50km", "100km"))
SIDS_pop_all$Distance<-factor(SIDS_pop_all$Distance, levels = c("5km", "10km", "30km", "50km", "100km"))

IG_pop_all<-IG_pop_all%>%
mutate(Income_Group = str_remove_all(Income_Group, " income"))%>%
dplyr:: filter(Income_Group !="Others")

IG_pop_all$Income_Group<-factor(IG_pop_all$Income_Group, levels = c("Low", "Lower middle", "Upper middle", "High"))

#100% population proportion####
#Finding the countries that are classed as populated 

prop_100_countries<-country_pop_all%>%
  group_by(ISO3)%>%
  arrange(Distance, Year)%>%
  mutate(pop_prop = round(pop_prop, 2))%>%
  dplyr::filter(Populated =="Yes",
                pop_prop == "100",
                Year == "2020",
                !ISO3 %in% c("XPI", "XSP"))%>%
  slice(1)


write.csv(prop_100_countries, file = "./data/100_prop_countries.csv")

##Set up for plotting####
#million for plotting high total population
div=1000000 
#regions colours
my_colors<-c("#66C5CC", "#F6CF71", "#F89C74" ,"#DCB0F2", "#87C55F", "#9EB9F3","#FE88B1")
#income groups colours
inc_colors<-c("#FE88B1", "#C9DB74", "#8BE0A4", "#B497E7", "#D3B484")

#ggplot theme
theme_main<-function () {
  theme_bw()+
    theme(strip.text.x = element_text(size = 8),
          axis.title.x = element_blank(),
          axis.text.x  = element_text(angle = 45, vjust = 0.5, size = 8, colour = "black"),
          axis.title.y = element_text(face = "bold", hjust = 0.5, size = 8),
          axis.text.y  = element_text(vjust = 0.5, size = 8, colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.title = element_text(face = "bold", size = 8),
          legend.text = element_text(size = 8),
          legend.title.align = 0.5,
          legend.position = "bottom",
          legend.key=element_blank(),
          #axis.line = element_line(colour = "black"), #ensuring axis is black
          #panel.border = element_blank(), #remove panel border
          panel.background = element_rect(fill = "transparent",colour = NA), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          legend.background = element_rect(fill = "transparent", color = NA), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent", color = NA))
}

theme_main_title<-function (){
  theme_bw()+
    theme(strip.text.x = element_text(size = 8),
          axis.title.x = element_text(face="bold", vjust=0.5, size=8),
          axis.text.x  = element_text(angle=45, vjust=0.5, size=8, colour = "black"),
          axis.title.y = element_text(face="bold", hjust=0.5, size=8),
          axis.text.y  = element_text(vjust=0.5, size=8, colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.title = element_text(face="bold", size = 8),
          legend.text = element_text(size = 8),
          legend.title.align = 0.5,
          legend.position = "bottom",
          legend.key=element_blank(),
          #axis.line = element_line(colour = "black"), #ensuring axis is black
          #panel.border = element_blank(), #remove panel border
          panel.background = element_rect(fill = "transparent",colour = NA), # bg of the panel
          plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          legend.background = element_rect(fill = "transparent", color = NA), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent", color = NA))
}



####Global####

#Total population#
Tot_plot<-ggplot(wld_pop_all)+
  geom_line(aes(x=Year, y= pop/div , colour = Distance), size=1)+
  geom_point( aes(x=Year, y= pop/div, colour = Distance), size=1)+
  scale_y_continuous(limits = c(20000/div, 1000000000/div), breaks = pretty_breaks(), labels = unit_format(unit = "M"))+
  scale_x_continuous(breaks=seq(2000,2020,4))+
  scale_color_carto_d(name = "Distance",
                      type = "qualitative", palette = "Bold", direction = -1)+
  labs(x = "Year", y = "Total population of coral reef countries (millions)")+
  theme_main_title()+
  guides(col=guide_legend(nrow = 1,byrow=TRUE, override.aes = list(size = 2)))

plot(Tot_plot)

#extract legend using cowplot - for figure panel
legend <- cowplot::get_legend(Tot_plot)
plot(legend)
Distance_legend <-as_ggplot(legend)

#Population proportion
glob_plot_prop<-ggplot(wld_pop_all, aes(x =Year, y = pop_prop, colour = Distance))+
  geom_line(size=1) +
  geom_point(size=1) +
  scale_color_carto_d(name = "Distance",
                      type = "qualitative", palette = "Bold", direction = -1)+
  labs(x = "Year", y = "Proportion of global population by coral reefs (%)")+
  scale_x_continuous(breaks=seq(2000,2020,4))+
  scale_y_continuous(breaks=seq(0,16,2))+
  theme_main()+
  guides(col=guide_legend(nrow = 1,byrow=TRUE, override.aes = list(size = 2)), position = "bottom")

plot(glob_plot_prop)

#Population density
Glob_pop_den<-ggplot(wld_pop_all, aes(x =Year, y =pop_den, colour = Distance))+
  geom_line(size=1) +
  geom_point(size=1) +
  scale_color_carto_d(name = "Distance",
                      type = "qualitative", palette = "Bold", direction = -1)+
  labs(x = "Year", y = expression(bold(atop("Population density of", paste("coral reef countries (people km"^-2,")")))))+
  scale_x_continuous(breaks=seq(2000,2020,4))+
  scale_y_continuous(breaks=seq(100,280,25))+
  expand_limits(y = 100)+
  theme_main()+
  guides(col=guide_legend(nrow = 1,byrow=TRUE, override.aes = list(size = 2)), position = "bottom")

plot(Glob_pop_den)

#Make figure panel
global_pop<-(glob_plot_prop|Tot_plot|Glob_pop_den)+ 
  plot_annotation(tag_levels = c('a'))+
  plot_layout(guides = 'collect')&
  theme(legend.position = "bottom",
        plot.tag = element_text(hjust = -4, vjust = -2, size = 12))

plot(global_pop)

ggsave("./Plot/Global_pop.png", global_pop, bg = "transparent", width = 19.5, height = 15, units =  "cm",
       dpi = 300)

##Global population change ####


glob_plot_change<-ggplot(wld_pop_all, aes(x =Year, y = pop_change, colour = Distance))+
  geom_line( aes(x =Year, y = SP.POP.GROW),colour = "black",
             linetype="solid", size = 1)+
  geom_line(size=1) +
  geom_point(size=1.5) +
  geom_line(aes(x =Year, y = Average_growth, colour = Distance),
            linetype="dashed", size = 1) +
  scale_color_carto_d(name = "Distance",
                      type = "qualitative", palette = "Bold", direction = -1)+
  labs(x = "Year", y = "Population Change (%)")+
  scale_x_continuous(breaks=seq(2000,2020,4))+
  facet_wrap(~Distance, nrow = 1)+
  theme_main()+
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.text.x  = element_text(angle = 45, vjust = 0.5, size = 10, colour = "black"),
        axis.title.y = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.text.y  = element_text(vjust = 0.5, size = 12, colour = "black"),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))+
  guides(col=guide_legend(nrow = 1,byrow=TRUE, override.aes = list(size = 2)))


plot(glob_plot_change)

ggsave("./Plot/SI/Pop_glob_change.png", glob_plot_change, bg = "transparent",  width = 21, height = 23, units =  "cm",
       dpi = 300)

##Regional####

region_plot<-ggplot(region_pop_all, aes(x=Year, y = pop/div, colour = region))+
  geom_line(size = 1) +
  geom_point(size = 1) +
  scale_color_manual(name = "Region", values= my_colors)+
  labs(x = "Year", y = expression(bold(atop("Total population", "by region (millions)"))))+
  facet_wrap(~Distance, nrow = 1)+
  scale_x_continuous(breaks=seq(2000,2020,4))+
  scale_y_continuous(limits = c(20000/div, 600000000/div), breaks = pretty_breaks(),
                     labels = unit_format(unit = "M"))+
  theme_main()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text.x = element_blank())+
  guides(col=guide_legend(nrow = 2,byrow=TRUE, override.aes = list(size = 2)), position = "bottom")


plot(region_plot)

region_plot_prop<-ggplot(region_pop_all, aes(x =Year, y = pop_prop, colour = region))+
  geom_line(size=1) +
  geom_point(size=1) +
  scale_x_continuous(breaks=seq(2000,2020,4))+
  scale_color_manual(name = "Region", values= my_colors)+
  labs(x = "Year", y = expression(bold(atop("Proportion of", "regional population(%)"))))+
  facet_wrap(~Distance, nrow = 1)+
  theme_main()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  guides(col=guide_legend(nrow = 2,byrow=TRUE, override.aes = list(size = 2)), position = "bottom")

plot(region_plot_prop)

region_plot_den<-ggplot(region_pop_all, aes(x =Year, y = pop_den, colour = region))+
  geom_line(size=1) +
  geom_point(size=1) +
  scale_x_continuous(breaks=seq(2000,2020,4))+
  scale_y_continuous(breaks=seq(0,1250,250))+
  expand_limits(y = c(0,1250))+
  scale_color_manual(name = "Region", values= my_colors)+
  labs(x = "Year", y =expression(bold(atop("Population density", paste("(people km"^-2,")")))))+
  facet_wrap(~Distance, nrow = 1)+
  theme_main_title()+
  theme(strip.text.x = element_blank())+
  guides(col=guide_legend(nrow = 2,byrow=TRUE, override.aes = list(size = 2)), position = "bottom")


plot(region_plot_den)

regional_plots<-(region_plot_prop/region_plot/region_plot_den)+
  plot_layout(guides = 'collect')+
  plot_annotation(tag_levels = 'a')&
  theme(legend.position = "bottom",
        plot.tag = element_text(hjust = -3.5, vjust = -3, size = 12))

plot(regional_plots)

ggsave("./Plot/region_mains.png", regional_plots, bg = "transparent", width = 19.5, height = 15, units =  "cm",
       dpi = 300) #for transparent

#regional population change####

region_plot_change<-ggplot(region_pop_all, aes(x =Year, y = pop_change, colour = Distance))+
  geom_line(size=1) +
  geom_point(size=1.5) +
  geom_line(aes(x =Year, y = Average_growth, colour = Distance),
            linetype="dashed", size = 1) +
  scale_x_continuous(breaks=seq(2000,2020,4))+
  scale_color_carto_d(name = "Distance",
                      type = "qualitative", palette = "Bold", direction = -1)+
  labs(x = "Year", y = "Population Change (%)")+
  facet_grid(region~Distance,labeller = label_wrap_gen(width=10), scales = "free")+
  theme_main()+
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.text.x  = element_text(angle = 45, vjust = 0.5, size = 10, colour = "black"),
        axis.title.y = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.text.y  = element_text(vjust = 0.5, size = 12, colour = "black"),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))+
  guides(col=guide_legend(nrow = 1,byrow=TRUE, override.aes = list(size = 2)))

plot(region_plot_change)

ggsave("./Plot/SI/Pop_region_change.png", region_plot_change, bg = "transparent",  width = 21, height = 29, units =  "cm",
       dpi = 300)


#Income Group####

Inc_grp_plot<-ggplot(IG_pop_all, aes(x=Year, y = pop/div, colour = Income_Group))+
  geom_line(size = 1) +
  geom_point(size = 1) +
  scale_x_continuous(breaks=seq(2000,2020,4))+
  scale_y_continuous(limits = c(20000/div, 600000000/div), breaks = pretty_breaks(),
                     labels = unit_format(unit = "M"))+
  scale_color_manual(name = "Income Group", values= inc_colors)+
  labs(x = "Year", y = expression(bold(atop("Total population by", "income group (millions)"))))+
  facet_wrap(~Distance, nrow =1)+
  theme_main()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text.x = element_blank())+
  guides(col=guide_legend(nrow = 1,byrow=TRUE, override.aes = list(size = 2)), position = "bottom")

plot(Inc_grp_plot)

Inc_grp_plot_prop<-ggplot(IG_pop_all, aes(x =Year, y = pop_prop, colour = Income_Group))+
  geom_line(size= 1) +
  geom_point(size= 1) +
  scale_x_continuous(breaks=seq(2000,2020,4))+
  scale_y_continuous(breaks=seq(0,30,5))+
  scale_color_manual(name = "Income Group", values= inc_colors)+
  labs(x = "Year", y = expression(bold(atop("Proportion of income", "group population (%)"))))+
  facet_wrap(~Distance, nrow = 1)+
  theme_main()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  guides(col=guide_legend(nrow = 1,byrow=TRUE, override.aes = list(size = 2)), position = "bottom")

plot(Inc_grp_plot_prop)

Inc_grp_plot_den<-ggplot(IG_pop_all, aes(x =Year, y = pop_den, colour = Income_Group))+
  geom_line(size= 1) +
  geom_point(size= 1) +
  scale_x_continuous(breaks=seq(2000,2020,4))+
  scale_y_continuous(breaks=seq(0,450,50))+
  expand_limits(y = c(0,450))+
  scale_color_manual(name = "Income Group", values= inc_colors)+
  labs(x = "Year", y = expression(bold(atop("Population density",paste("(people km"^-2,")")))))+
  facet_wrap(~Distance, nrow = 1)+
  theme_main_title()+
  theme(strip.text.x = element_blank())+
  guides(col=guide_legend(nrow = 1,byrow=TRUE, override.aes = list(size = 2)), position = "bottom")

plot(Inc_grp_plot_den)


Inc_grp_pre<-(Inc_grp_plot_prop/Inc_grp_plot/Inc_grp_plot_den)+
  plot_layout(guides = 'collect')+
  plot_annotation(tag_levels = 'a')&
  theme(legend.position = "bottom",
        plot.tag = element_text(hjust = -3.5, vjust = -3, size = 12))

plot(Inc_grp_pre)

ggsave("./Plot/income_mains.png", Inc_grp_pre, bg = "transparent", width = 19.5, height = 15, units =  "cm",
       dpi = 300) #for transparent

#Income group population change####

Inc_grp_plot_change<-ggplot(IG_pop_all, aes(x =Year, y = pop_change, colour = Distance))+
  geom_line(size=1) +
  geom_point(size=1.5) +
  geom_line( aes(x =Year, y = Average_growth, colour = Distance),
             linetype="dashed", size = 1) +
  scale_x_continuous(breaks=seq(2000,2020,4))+
  scale_color_carto_d(name = "Distance",
                      type = "qualitative", palette = "Bold", direction = -1)+
  labs(x = "Year", y = "Population Change (%)")+
  facet_grid(Income_Group~Distance, labeller = label_wrap_gen(width=10) )+
  #facet_grid_paginate(region~Distance, ncol = 6, nrow = 2, page = 4)+
  theme_main()+
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.text.x  = element_text(angle = 45, vjust = 0.5, size = 10, colour = "black"),
        axis.title.y = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.text.y  = element_text(vjust = 0.5, size = 12, colour = "black"),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))+
  guides(col=guide_legend(nrow = 1,byrow=TRUE, override.aes = list(size = 2)))

plot(Inc_grp_plot_change)


ggsave("./Plot/SI/Inc_grp_plot_change.png", Inc_grp_plot_change, bg = "transparent",  width = 21, height = 25, units =  "cm",
       dpi = 300)

#SIDS####

Tot_plot_SID<-ggplot(SIDS_pop_all, aes(x = Year, y = pop/div , colour = Distance))+
  geom_line(size = 1)+
  geom_point(size = 1)+
  scale_y_continuous(limits = c(20000/div, 60000000/div), breaks = pretty_breaks(), labels = unit_format(unit = "M"))+
  scale_x_continuous(breaks=seq(2000,2020,4))+
  scale_color_carto_d(name = "Distance",
                      type = "qualitative", palette = "Bold", direction = -1)+
  labs( x = "Year", y = expression(bold(atop("Total population of SIDS", "classed coral reef countries (millions)"))))+
  theme_main()+
  theme(axis.title.x = element_text(face="bold", size=8),
        axis.line = element_line(colour = "black"))+ #remove panel border
  guides(col=guide_legend(nrow = 1,byrow=TRUE, override.aes = list(size = 2)), position = "bottom")


plot(Tot_plot_SID)

SID_plot_prop<-ggplot(SIDS_pop_all, aes(x =Year, y = pop_prop, colour = Distance))+
  geom_line(size= 1) +
  geom_point(size= 1) +
  scale_color_carto_d(name = "Distance",
                      type = "qualitative", palette = "Bold", direction = -1)+
  labs(x = "Year", y = "Proportion of SIDS population by coral reefs (%)")+
  scale_x_continuous(breaks=seq(2000,2020,4))+
  scale_y_continuous(breaks=seq(0,100,5))+
  theme_main()+
  theme(axis.line = element_line(colour = "black"))+
  guides(col=guide_legend(nrow = 1,byrow=TRUE, override.aes = list(size = 2)), position = "bottom")

plot(SID_plot_prop)

SID_pop_den<-ggplot(SIDS_pop_all, aes(x =Year, y =pop_den, colour = Distance))+
  geom_line(size= 1) +
  geom_point(size= 1) +
  scale_color_carto_d(name = "Distance",
                      type = "qualitative", palette = "Bold", direction = -1)+
  labs(x = "Year", y =expression(bold(atop("Population density of SIDS classed",paste("coral reef countries (people km"^-2,")")))))+
  scale_x_continuous(breaks=seq(2000,2020,4))+
  scale_y_continuous(breaks=seq(0,200,10))+
  expand_limits(y = c(80,170))+
  theme_main()+
  theme(axis.line = element_line(colour = "black"), #ensuring axis is black
  )+
  guides(col=guide_legend(nrow = 1,byrow=TRUE, override.aes = list(size = 2)), position = "bottom")

plot(SID_pop_den)

SID_plots2<-(SID_plot_prop|Tot_plot_SID|SID_pop_den)+
  plot_layout(guides = 'collect')+
  plot_annotation(tag_levels = 'a')&
  theme(legend.position = "bottom",
        plot.tag = element_text(hjust = -4.5, vjust = -2, size = 12))

SID_plots2

ggsave("./Plot/SID_plots.png", SID_plots2, bg = "transparent", width = 19.5, height = 15, units =  "cm",
       dpi = 300) 


#SIDS population change####

SID_pop_change<-ggplot(SIDS_pop_all, aes(x =Year, y = pop_change, colour = Distance))+
  geom_line(size=1) +
  geom_point(size=1.5) +
  geom_line(aes(x =Year, y = Average_growth, colour = Distance),
            linetype="dashed", size = 1) +
  scale_color_carto_d(name = "Distance",
                      type = "qualitative", palette = "Bold", direction = -1)+
  labs(x = "Year", y = "Population Change (%)")+
  scale_x_continuous(breaks=seq(2000,2020,4))+
  scale_y_continuous(breaks=seq(-10,16,1))+
  facet_wrap(~Distance, nrow = 1)+
  theme_main()+
  theme(legend.position = "none",
        axis.title.x = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.text.x  = element_text(angle = 45, vjust = 0.5, size = 10, colour = "black"),
        axis.title.y = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.text.y  = element_text(vjust = 0.5, size = 12, colour = "black"),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))+
  guides(col=guide_legend(nrow = 1,byrow=TRUE, override.aes = list(size = 2)))

plot(SID_pop_change)

ggsave("./Plot/SI/SID_pop_change.png", SID_pop_change, bg = "transparent", width = 21, height = 22, units =  "cm",
       dpi = 300) #for transparent

#Bump graphs - country rankings####


