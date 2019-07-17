# Wrasse precon B hides analysis
# Adam Brooker 22nd February 2019

library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plyr)
library(tidyr)
library(cowplot)


workingdir = "H:/Acoustic tag - Preconditioning B/Data processing/6a. Coded Day CSV/hides" # change to location of data
hidefile.loc = "run_1LLF16S100258_day_hides.csv" # change to file to be analysed
hidefile.classes = c('NULL', 'factor', 'NULL', 'factor', 'POSIXct', 'double', 'double', 'double')


setwd(workingdir)                                                                                                    
hidefile <- read.csv(hidefile.loc, header = TRUE, sep = ",", colClasses = hidefile.classes) 
load.all.hides()

P7NW <- c('11805', '10965')
P7SE <- c('11553', '11217')
P8SW <- c('10377', '9313')
P8NE <- c('10657', '9761')

#add tide period and phase from dayfile
hidefile <- left_join(hidefile, dayfile[,c('EchoTime', 'TID', 'HEIGHT')], by = 'EchoTime') %>%
  fill(c(TID, HEIGHT), .direction = 'down') %>%
  fill(c(TID, HEIGHT), .direction = 'up')

# strip time from EchoTime and create new time column
hidefile$time <- as.POSIXct(as.ITime(hidefile$EchoTime))



# Plot P7NW tags
filter(hidefile, Period %in% P7NW) %>%
  ggplot() + geom_point(aes(PosX, PosY, colour = Period))

# Plot P7SE tags
filter(hidefile, Period %in% P7SE) %>%
  ggplot() + geom_point(aes(PosX, PosY, colour = Period))

# Plot P8SW tags
filter(hidefile, Period %in% P8SW) %>%
  ggplot() + geom_point(aes(PosX, PosY, colour = Period))

# Plot P8NE tags
filter(hidefile, Period %in% P8NE) %>%
  ggplot() + geom_point(aes(PosX, PosY, colour = Period))

# x coord vs. time
tidplot <- filter(hidefile, Period == '10657') %>%
  #filter(TID != 'Z', TID != '', TID != '') %>%
  filter(HEIGHT != '', HEIGHT != '', HEIGHT != '') %>%
  ggplot() + geom_point(aes(time, PosX, colour = HEIGHT), alpha = 0.1, stroke = 0, shape = 16, size = 2) +
  scale_y_continuous(limits = c(40, 75))

tidplot + facet_wrap(vars(HEIGHT))

# density plot
tag <- '9761'

mu <- filter(hidefile, Period == tag) %>%
  ddply('TID', summarise, grp.mean = median(PosX))

filter(hidefile, Period == tag) %>%
  #filter(TID != 'Z', TID != '', TID != '') %>%
  #filter(PHASE != '', PHASE != '', PHASE != '') %>%
  ggplot() + geom_density(aes(PosX, fill = TID), alpha = 0.3) +
  geom_vline(data = mu, aes(xintercept = grp.mean, colour = TID), linetype = 'dashed') +
  scale_x_continuous(limits = c(mean(mu$grp.mean)-10, mean(mu$grp.mean)+10))


# Split hide data by hide number and align paired tags by EchoTime

NWt1 <- filter(hidefile, Period == '11805') %>% distinct()
NWt2 <- filter(hidefile, Period == '10965') %>% distinct()
SEt1 <- filter(hidefile, Period == '11553') %>% distinct()
SEt2 <- filter(hidefile, Period == '11217') %>% distinct()
SWt1 <- filter(hidefile, Period == '10377') %>% distinct()
SWt2 <- filter(hidefile, Period == '9313') %>% distinct()
NEt1 <- filter(hidefile, Period == '10657') %>% distinct()
NEt2 <- filter(hidefile, Period == '9761') %>% distinct()

NW <- full_join(NWt1, NWt2, by = 'EchoTime') %>% 
  arrange(EchoTime) %>%
  mutate(time.diff = c(NA, diff(EchoTime, lag = 1))) %>%
  filter(time.diff < 11) %>%
  fill(c(PosX.x, PosY.x, PosZ.x, PosX.y, PosY.y, PosZ.y), .direction = 'down') %>%
  fill(c(PosX.x, PosY.x, PosZ.x, PosX.y, PosY.y, PosZ.y), .direction = 'up') %>%
  select(-Period.x, -PEN.x, -TID.x, -HEIGHT.x, -time.x, -Period.y, -PEN.y, -TID.y, -HEIGHT.y, -time.y) %>%
  mutate(x.diff = abs(PosX.x-PosX.y), y.diff = abs(PosY.x-PosY.y), z.diff = abs(PosZ.x-PosZ.y)) %>%
  mutate(hide = 'NW') %>%
  mutate(dist = round(sqrt((PosX.x-PosX.y)^2+(PosY.x-PosY.y)^2+(PosZ.x-PosZ.y)^2), digits = 3))
  
SE <- full_join(SEt1, SEt2, by = 'EchoTime') %>% 
  arrange(EchoTime) %>%
  mutate(time.diff = c(NA, diff(EchoTime, lag = 1))) %>%
  filter(time.diff < 11) %>%
  fill(c(PosX.x, PosY.x, PosZ.x, PosX.y, PosY.y, PosZ.y), .direction = 'down') %>%
  fill(c(PosX.x, PosY.x, PosZ.x, PosX.y, PosY.y, PosZ.y), .direction = 'up') %>%
  select(-Period.x, -PEN.x, -TID.x, -HEIGHT.x, -time.x, -Period.y, -PEN.y, -TID.y, -HEIGHT.y, -time.y) %>%
  mutate(x.diff = abs(PosX.x-PosX.y), y.diff = abs(PosY.x-PosY.y), z.diff = abs(PosZ.x-PosZ.y)) %>%
  mutate(hide = 'SE') %>%
  mutate(dist = round(sqrt((PosX.x-PosX.y)^2+(PosY.x-PosY.y)^2+(PosZ.x-PosZ.y)^2), digits = 3))

SW <- full_join(SWt1, SWt2, by = 'EchoTime') %>% 
  arrange(EchoTime) %>%
  mutate(time.diff = c(NA, diff(EchoTime, lag = 1))) %>%
  filter(time.diff < 9) %>%
  fill(c(PosX.x, PosY.x, PosZ.x, PosX.y, PosY.y, PosZ.y), .direction = 'down') %>%
  fill(c(PosX.x, PosY.x, PosZ.x, PosX.y, PosY.y, PosZ.y), .direction = 'up') %>%
  select(-Period.x, -PEN.x, -TID.x, -HEIGHT.x, -time.x, -Period.y, -PEN.y, -TID.y, -HEIGHT.y, -time.y) %>%
  mutate(x.diff = abs(PosX.x-PosX.y), y.diff = abs(PosY.x-PosY.y), z.diff = abs(PosZ.x-PosZ.y)) %>%
  mutate(hide = 'SW') %>%
  mutate(dist = round(sqrt((PosX.x-PosX.y)^2+(PosY.x-PosY.y)^2+(PosZ.x-PosZ.y)^2), digits = 3))


NE <- full_join(NEt1, NEt2, by = 'EchoTime') %>% 
  arrange(EchoTime) %>%
  mutate(time.diff = c(NA, diff(EchoTime, lag = 1))) %>%
  filter(time.diff < 9) %>%
  fill(c(PosX.x, PosY.x, PosZ.x, PosX.y, PosY.y, PosZ.y), .direction = 'down') %>%
  fill(c(PosX.x, PosY.x, PosZ.x, PosX.y, PosY.y, PosZ.y), .direction = 'up') %>%
  select(-Period.x, -PEN.x, -TID.x, -HEIGHT.x, -time.x, -Period.y, -PEN.y, -TID.y, -HEIGHT.y, -time.y) %>%
  mutate(x.diff = abs(PosX.x-PosX.y), y.diff = abs(PosY.x-PosY.y), z.diff = abs(PosZ.x-PosZ.y)) %>%
  mutate(hide = 'NE') %>%
  mutate(dist = round(sqrt((PosX.x-PosX.y)^2+(PosY.x-PosY.y)^2+(PosZ.x-PosZ.y)^2), digits = 3))


hide.diff <- bind_rows(NW, SE, SW, NE)


# density plot of difference between two tags on each hide
mu <- ddply(hide.diff, 'hide', summarise, grp.mean = median(dist))
ddply(hide.diff, 'hide', summarise, grp.mean = sd(dist))


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(hide.diff$dist)# Calculate modal average
mod <- ddply(hide.diff, 'hide', summarise, grp.mean = getmode(round(dist, 2)))

labeldf <- data.frame(dist = rep(10, 4), density = rep(0.45, 4), label = c('(a)', '(b)', '(c)', '(d)'), hide = c('NE', 'NW', 'SE', 'SW'))

ggplot(hide.diff) + 
  #geom_histogram(aes(dist, y = ..density..), colour = 'black', fill = 'white', binwidth = 0.1) +
  geom_density(aes(dist, fill = hide), alpha = 1) +
  geom_vline(data = mod, aes(xintercept = grp.mean), linetype = 'dashed', size = 1) +
  geom_vline(aes(xintercept = 1), linetype = 'solid', size = 1) +
  scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, 1), name = 'Distance (m)', expand = c(0, 0)) +
  scale_y_continuous(name = 'Density', expand = c(0, 0)) +
  facet_wrap(vars(hide)) +
  theme_classic() + 
  theme(legend.position = 'none', strip.background = element_blank(), strip.text.x = element_blank(), text = element_text(size = 14), panel.spacing = unit(2, 'lines')) +
  scale_fill_manual(values = rep('grey80', 4)) +
  geom_text(data = labeldf, aes(x = dist, y = density, label = label, fontface = 'bold', size = 30))

# Calculate quantiles

quantile(hide.diff[hide.diff$hide == 'NE','dist'], c(0.25, 0.75))
quantile(hide.diff[hide.diff$hide == 'NW','dist'], c(0.25, 0.75))
quantile(hide.diff[hide.diff$hide == 'SE','dist'], c(0.25, 0.75))
quantile(hide.diff[hide.diff$hide == 'SW','dist'], c(0.25, 0.75))

quantile(hide.diff[hide.diff$hide == 'NE','dist'], mean(hide.diff[hide.diff$hide == 'NE','dist'] <= 2)) # calculate quantile for given distance
quantile(hide.diff[hide.diff$hide == 'NW','dist'], mean(hide.diff[hide.diff$hide == 'NW','dist'] <= 2)) # calculate quantile for given distance
quantile(hide.diff[hide.diff$hide == 'SE','dist'], mean(hide.diff[hide.diff$hide == 'SE','dist'] <= 2)) # calculate quantile for given distance
quantile(hide.diff[hide.diff$hide == 'SW','dist'], mean(hide.diff[hide.diff$hide == 'SW','dist'] <= 2)) # calculate quantile for given distance

# Daily positioning rate

hidefile$date <- as.Date(hidefile$EchoTime)
hidefile$Period <- as.numeric(as.character(hidefile$Period))
hidefile <- hidefile %>% mutate(hide = ifelse(Period %in% P7NW, 'NW', ifelse(Period %in% P7SE, 'SE', ifelse(Period %in% P8NE, 'NE', 'SW'))))

hag <- select(hidefile, Period, hide, date) %>% 
  dplyr::count(Period, hide, date, name = 'dpings') %>% 
  mutate(ppings = round((60/(Period/1000))*1440)) %>% # calculate No. of pings in one day based on PRI
  mutate(pdiff = ppings-dpings) %>%
  mutate(pcdet = (dpings/ppings)*100) %>%
  filter(date != '2016-09-13' & date < '2016-10-14') %>%
  group_by(hide, date) %>% 
  dplyr::summarise(mean = mean(pcdet)) %>%
  mutate(day = seq(1, 30, 1))

ggplot(hag) + 
  geom_line(aes(x = day, y = mean, group = hide)) +
  geom_point(aes(x = day, y = mean, group = hide)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(10, 100, 10), name = 'Detection rate (%)') +
  scale_x_continuous(limits = c(0, 31), breaks = seq(0, 60, 5), name = 'Experiment day')

# mean and sd daily positioning rate for each hide
hag %>% group_by(hide) %>%
  dplyr::summarise(av = mean(mean), stdev = sd(mean))




# 33b. Load all hide data

load.all.hides <- function(){
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  
  hidefile <- data.frame()
  
  for(i in 1:length(files)){
    
    hidetemp <- read.csv(files[[i]], header = TRUE, sep = ",", colClasses = hidefile.classes)
    
    hidefile <- rbind(hidefile, hidetemp)
    
    hidefile <<- hidefile
  }
  
}




