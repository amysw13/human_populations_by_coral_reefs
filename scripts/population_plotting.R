####Plotting figures####

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
wld_pop_all<-read_csv( "./data/wld.pop.all.csv")
region_pop_all<-read_csv( "./data/region.pop.all.csv")
IG_pop_all<-read_csv("./data/IG_pop_all.csv")
SIDS_pop_all<-read_csv( "./data/SIDS_pop_all.csv")

#order distances for plotting 
wld_pop_all$Distance<-factor(wld_pop_all$Distance, levels = c("5km", "10km", "30km", "50km", "100km"))
region_pop_all$Distance<-factor(region_pop_all$Distance, levels = c("5km", "10km", "30km", "50km", "100km"))
IG_pop_all$Distance<-factor(IG_pop_all$Distance, levels = c("5km", "10km", "30km", "50km", "100km"))
SIDS_pop_all$Distance<-factor(SIDS_pop_all$Distance, levels = c("5km", "10km", "30km", "50km", "100km"))

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
