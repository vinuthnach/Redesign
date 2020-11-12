library(dplyr)
library(tidyr)
library(tidyverse)
library(lattice)
library(ggplot2)

proj <- read.csv('statproj.csv')
names(proj)

#Preparing the data

plt <- statproj %>% filter(County != 'Illinois') %>% filter(County != 'United States') %>% filter(County != 'Cook County') %>% filter(County != 'Pulaski County')
plt$AverageAnnualCount

#Plotting the GGPlot

pointLabs <- plt %>% 
  filter(AverageAnnualCount == max(AverageAnnualCount) |
           AverageAnnualCount == min(AverageAnnualCount)) 
plt1 + geom_label(data = pointLabs, aes(label = County),nudge_x = -4)


plt2 <- plt %>% select(County,DeathRate,LowerCI,UpperCI,AverageAnnualCount)

#Ploting the row-Labeled point graph

ggplot(plt2,
       aes(x = DeathRate, y = reorder(County,DeathRate),
           descend = TRUE)) +
  geom_point(shape = 21, fill = rgb(1,.55,.55),
             col = "black",size = 3.5) +
  labs(x = "DeathRate",y = 'County',
       title = paste0("DeathRate\n",
                      "in illinois for all the counties")) + hw +
  theme(axis.text.y = element_text(size = rel(.95),
                                   face = 'bold'))
#Preparing data for Perceptual group row-labeled point plot

plt2sort <- plt2 %>%
  arrange(desc(DeathRate))
plt2sort

fact <- as.character(plt2sort$County)  
plt2sort$County = factor(fact,levels = rev(nam))

grpLevels <- paste0('G',1:10)
grpNams <- rep(grpLevels, each = 10)
grpFactor <- factor(grpNams,levels = grpLevels)
plt2sort$Grp = grpFactor

grpFactor

plt2Gath <- gather(plt2sort,
                     value = DeathRate, key = varNames,
                     DeathRate:AverageAnnualCount,
                     factor_key = TRUE)
head(plt2Gath)


stripLabs <- c( 
  County = "County\nName",
  DeathRate = "Death\nRate",
  LowerCI = "CI\nRate",
  UpperCI = "Upper\nRate",
  AverageAnnualCount = "Average\nValue")
  
#Plotting the Perceptual-group row-labeled point graph

ggplot(plt2Gath,
       aes(x = DeathRate , y = County)) +
  geom_point(shape = 21,fill = rgb(1,0,0),
             col = "black",size = 2.8) +
  facet_grid(Grp~varNames, scale = "free",
             labeller = labeller(varNames = stripLabs)) +
  labs(x = "Death Rate",y = "County", title =
         "Cancer statistics in Illinois") + hw

