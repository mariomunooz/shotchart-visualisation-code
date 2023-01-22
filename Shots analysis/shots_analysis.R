#If one of the following packages are not installed uncomment the corresponding line

#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("devtools")
#remotes::install_github("tylermorganwall/rayshader")


# Call libraries

library(ggplot2)
library(tidyverse)
library(rayshader)
source("create_court.R")
source("compute_stats.R")

#FUNCTIONS



## IMPORT DATA
shotsData <- read.csv("shots_data.csv")
shotsDataframe <- data.frame(shotsData)

#Change of coordinates: 
#         - In our plot the y axis is centered at the baseline not at the center of the rim)
#         - So to represent the shots we need to adjust the y axis adding 5.25 feets
#         - Because the center of the rim is 13.75 feets from the free throw line, 
#           that is 5.25 feets from the baseline

shotsDataframe['plot_y'] = shotsDataframe['y'] + 5.25
shotsDataframe$fgmadeLogic <- as.logical(shotsDataframe$fgmade)

#SHOTCHARTS

#teams_shotchart <- plot_court('Field goal attempts') + 
#  geom_point(data = shotsDataframe, aes(x = x, y = plot_y, color = fgmadeLogic, fill = fgmadeLogic), alpha = 0.8, )+
#  scale_color_manual(values = c("green4","red3"), aesthetics = "color", breaks=c("TRUE", "FALSE"), labels=c("Made", "Missed"), name= "Field goals")+
#  scale_fill_manual(values = c("green2","red3"), aesthetics = "fill", breaks=c("TRUE", "FALSE"), labels=c("Made", "Missed"), name= "Field goals")+
#  facet_wrap(~ team)
#
#teams_shotchart




# COMPUTE ZONE STATS

#Define shot zones
shotsDataframe <- divide_shots_by_zone(shotsDataframe)

#Compute shot stats
shooting_tables <- create_shot_stats(shotsDataframe)
shotsAttemted   <- shooting_tables$attempted
shotsMade       <- shooting_tables$made
stats           <- shooting_tables$stats

print(shotsAttemted)
print(shotsMade)
print(stats)

# VISUALIZE ZONE STATS

shots_and_stats <- shots_add_stats(shotsDataframe, stats)


teams_shotchart <- plot_court('HOW THE TEAMS SHOT (By Zone)\n') + 
  geom_point(data = shots_and_stats, aes(x = x, y = plot_y, size = usage, color = efg, fill = efg), alpha = 0.8)+

  
  scale_color_gradient(low = "#033270", high = "#65010C", name = 'eFG%')+
  scale_fill_gradient(low = "#033270", high = "#65010C", name = 'eFG%')+
  labs(fill = 'eFG%', size = 'shot%')+
  guides(size = "none")+
  facet_wrap(~ team)


  
plot_gg(teams_shotchart, multicore = TRUE, width = 6, height = 5.5, height_aes = "color", scale = 300, 
        background = "#afceff",shadowcolor = "#3a4f70")





















