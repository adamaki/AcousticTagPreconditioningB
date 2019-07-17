#Delousing efficiency project data analysis
#Adam Brooker
#29th August 2016

# LIST OF FUNCTIONS ------------------------------------------------------------------------------------------------

# 1. locations() = returns a summary matrix of pen locations for all fish
# 2. batch.locations() = returns a summary matrix of locations for all dayfiles in working directory and saves to an Excel spreadsheet
# 3a. depact() = returns depth and activity summary for all fish with standard deviations
# 3b. depact.se() = returns depth and activity summary for all fish with standard errors
# 4. depth.sum() = returns depth summary for each fish
# 5. batch.depth() = creates spreadsheet of mean depths +/- st dev for individual fish over multiple days
# 6. batch.totdepth() = batch function to return matrix of mean and standard error depths for all fish combined over multiple days
# 7. batch.activity() = creates spreadsheet of mean activity +/- st dev for each dayfile in working dir
# 8. batch.totactivity() = batch function to return matrix of mean and standard error activity for all fish combined over multiple days
# 9a. prop.coverage() = calculates fish coverage of pens 7 and 8
# 9b. hmean.prop.coverage() = calculates hourly mean fish coverage of pens 7 and 8
# 10a. batch.coverage() = calculates fish coverage of pens 7 and 8 over multiple days
# 10b. hmean.batch.coverage() = caculates hourly mean fish coverage of pens 7 and 8 over multiple days
# 10c. hmean.perfish.coverage - daily hourly coverage per fish for all days loaded as one file using load.all()
# 11a. fish.depth(period) = draws a plot of fish depth for the fish id specified
# 11b. fish.act(period) = draws a plot of fish activity for the fish id specified
# 12. fish.3depth(period1, period2, period3) = draws a plot of depths of 3 fish
# 13. fish.plot(period) = draws a plot of fish location for the fish id specified
# 14. fish.3plot(period1, period2, period3) = draws a plot of locations of 3 fish
# 15. add.fish(period, fishcol) = add a fish to the current plot (period = fish id, fishcol = number from 1-20)
# 16a. fish.hexplot(period) = draws a plot of fish location density for the fish id specified 
# 16b. hexplot.all(pen) = draws a plot of fish location density for all fish in the specified pen (7 or 8)
# 16c. draws plots of fish location density for all fish in pens 7 and 8 and plots side by side
# 17. fish.3dplot(period) = draws a 3d plot of fish location and depth
# 18. fish.3dmove(period) = draws a 3d interactive plot of fish location and depth
# 19a. plot.bydepth(period) = draws a plot of fish locations coloured by depth (blue = >15m, red = <15m)
# 19b. plot.byactivity(period) = draws a plot of fish locations coloured by activity
# 19c. plot.bylight(period) = draws a plot of fish locations coloured by time of day (dawn, day, dusk, night)
# 20. add.depthfish(period) = add a fish to the current plot coloured by depth
# 21. fractal() = calculate fractal dimensions for pens 7 & 8 using the box counting method. Returns plot of box counts with fractal dimension and R2
# 22. batch.fractals() = calculate fractal dimensions for each fish over several day files in a folder. Returns an Excel spreadsheet of fractal dimension and R2 for all fish each day
# 23. id.fractals() = calculate fractal dimensions for each fish on one day file. Returns table of fractal dimesions and R2 values and saves to Excel spreadsheet
# 24. plot.bytime(period) = draws a plot of fish locations colour coded according number of time divisions (bins)
# 25. batch.remove(period, start.day, no.days) = Removes single fish id from specified day files
# 26. prop.coverage.3d() = proportion coverage 3D (not sure this is working properly!)
# 27. ma.filter(period, smooth, thresh) = moving average filter function. Period = fish id, smooth = size of smoothing filter, thresh = data removal threshold in metres
# 28. add(period)  = add a single fish to a dayfile after cleaning data using ma.filter function
# 29. recode() = function to recode fish speeds and save to dayfile after cleaning data
# 30. batch.subset(variable, factors) = batch function to subset and save data according to specified variable and factors, variable = column to subset by, factors = list of variables in column
# 31a. heatplot.anim(pen, frames) = Create series of plots for animation (pen = pen number 7 or 8, frames = No. of frames, set to No. of hours in dataset)
# 31b. fishplot.anim <- function(pen, frames, framedur, animdur) = Create series of individual fish plots for animation. pen = pen to plot, frames = No. of frames to create, framedur = portion of time to plot for each frame in secs, animdur = length of fish trails in No. of frames (0 = cumulative frames)
# 32. fish.hist(pt) = draw histogram of fish depth or activity from fish files (pt = 'activity' or 'depth')
# 33. load.all() = Load all data files (.csv) in folder into single data frame
# 34. crop(xmin, xmax, ymin, ymax) = Crop edges of dataset to remove multipath
# 35. save() = Save loaded dayfile to .csv file of original name
# 36. distance() = calculate distance travelled for all individual fish in day file
# 37. batch.dist() = calculate distance travelled for all fish files in a folder
# 38. Load.dayfile() = load specified dayfile
# 39. multiplot() = off-the-shelf function to draw multiple ggplots
# 40. headplot() = draws two polar plots of headings for pens 7 and 8
# 41. turnplot() = draws two polar plots of turn angles for pens 7 and 8
# 42. bplot(period, step) = draw turn and velocity plots for period and step specified for whole dayfile
# 43. bcalc() = Perform behaviour calculations for loaded dayfile and add to dayfile
# 44. batch.bscalc() = calculate behaviour states for all dayfiles in working directory and save results to dayfiles
# 45. batch.bsprop() = calculate proportions of behaviour states for each dayfile in working directory
# 50a. bsf(static, cruise, save) = calculate behaviour state frequencies (static, cruise, burst) for pens 7 and 8. static = upper limit of static state, cruise = upper limit of cruise state, save = save plot and data file(T/F)
# 50b. bsf2(save) = calculate behaviour state frequencies (Rr, Rf, Ra, Ep, Ef, Ea) for pens 7 and 8. save = save plot and data file(T/F)

# NOTES -------------------------------------------------------------------------------------------------------------

# coverage grid size:
# mean swimming speed = 0.03m/s, max ping rate = 10 sec. Mean distan ce covered between pings = 0.03*10 = 0.3m
# Therefore: grid size = 0.3m


# Need to run the code below to get some functions to work (maybe!)
#library(devtools)
#install_github("plotflow", "trinker")


# ------
library(hexbin)
library(scatterplot3d)
library(rgl)
library(rJava)
library(XLConnectJars)
library(XLConnect) 
library(RColorBrewer)
library(colorspace)
library(colorRamps)
library(stats)
library(ggplot2)
library(animation)
detach("package:dplyr")
library(openxlsx)
library(xlsx)
library(chron)
library(lubridate)
library(magick)
#library(plyr)
#library(plotflow)
library(gridExtra)
library(cowplot)
library(zoo)
library(adehabitat)
library(adehabitatHR)
library(maptools)
library(sp)
library(Rwave)
#library(sowas)
library(WaveletComp)
library(dplyr)
library(tidyr)
library(data.table)



#ENTER YOUR VARIABLES HERE
workingdir = "H:/Acoustic tag - Preconditioning B/Data processing/Filtered Data/Recoded Day CSV" # change to location of data
dayfile.loc = "run_1LLF16S100259_day_coded.csv" # change to file to be analysed
masterfileloc = "H:/Data processing/AcousticTagFile_2016.xlsx" # change to location of AcousticTagFile.xlsx

workingdir = "H:/Acoustic tag - Preconditioning B/Data processing/Filtered Data/Recoded Fish CSV/Unconditioned" # change to location of data
dayfile.loc = "run_1LLF16S1007045_fish_coded.csv" # change to file to be analysed
masterfileloc = "H:/Data processing/AcousticTagFile_2016.xlsx" # change to location of AcousticTagFile.xlsx


#new dayfile classes
dayfile.classes = c('NULL', 'numeric', 'factor', 'factor', 'POSIXct', 'double', 'double', 
                    'double', 'double', 'double', 'double', 'double', 'double', 'double', 'double', 'double', 'factor',
                    'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
                    'double', 'double', 'double', 'double', 'double', 'double', 'double',
                    'double', 'double', 'double', 'double', 'double', 'double', 'double',
                    'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                    'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
                    'factor', 'factor', 'double', 'double', 'double', 'double', 'double', 
                    'double', 'double', 'double', 'double', 'double', 'double', 'double'
)


#old dayfile classes
dayfile.classes = c('NULL', 'numeric', 'factor', 'factor', 'POSIXct', 'double', 'double', 
                    'double', 'double', 'double', 'double', 'double', 'double', 'factor',
                    'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
                    'double', 'double', 'double', 'double', 'double', 'double', 'double',
                    'double', 'double', 'double', 'double', 'double', 'double', 'double',
                    'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                    'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                    'factor', 'factor', 'double', 'double', 'double', 'double', 'double', 
                    'double', 'double', 'double', 'double', 'double', 'double', 'double'
)

workingdir = "H:/Data processing/2016 Conditioning Study B/6a. Coded Day CSV/hides" # change to location of data
hidefile.loc = "run_1LLF16S100258_day_hides.csv" # change to file to be analysed
hidefile.classes = c('NULL', 'numeric', 'factor', 'factor', 'POSIXct', 'double', 'double', 'double')

# LOAD FILES-------------------------------------------------------------------------------------------------------------------

#LOAD LOCATIONS CODING DATA
library(xlsx)
locations.lookup <- read.xlsx(masterfileloc, sheetIndex = 12, startRow = 1, endRow = 47, colIndex = seq(1, 7)) # read in codes from Locations Coding spreadsheet

#locations.lookup <- readWorksheetFromFile(masterfileloc, sheet = 12, startRow = 1, endCol = 7) # read in codes from Locations Coding spreadsheet
rownames(locations.lookup) <- locations.lookup$Code


# LOAD DAYFILE
setwd(workingdir)                                                                                                    
dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = dayfile.classes)
                                                                          
#)) #read data into table

#SORT BY TIME AND TAG
dayfile <- dayfile[order(dayfile$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
dayfile <- dayfile[order(dayfile$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag


#LOAD HIDEFILE
setwd(workingdir)                                                                                                    
hidefile <- read.csv(hidefile.loc, header = TRUE, sep = ",", colClasses = hidefile.classes) 



# SANDPIT-----------------------------------------------------------------------------------------------------------------

# animated 3d plot
plot3d(fish.id$PosX, fish.id$PosY, fish.id$PosZ, pch = 20, xlim =  c(0, 35), ylim = c(5, 40), zlim = c(0, 26), xlab = 'X', ylab = 'Y', zlab = 'Z', type = 'l')
dir.create("animation")
for (i in 1:1000){
  view3d(userMatrix=rotationMatrix(pi/2 * i/1000, 0, 1, -1))
  rgl.snapshot(filename=paste("animation/frame-", sprintf("%03d", i), ".png", sep=""))
}


# hexplot for all fish
bin <- hexbin(dayfile$PosX, dayfile$PosY, xbins = 50)
plot(hexbin(dayfile$PosX, dayfile$PosY, xbins = 50), xlab = 'X', ylab = 'Y')


# pen 7 x,y plots
par(mfrow=c(3,3))
fish.3plot('7829', '8081', '7213')
fish.3plot('7269', '7045', '9229')
fish.3plot('9873', '7381', '9901')
fish.3plot('9453', '7129', '9397')
fish.3plot('8025', '8417', '9649')
fish.plot(9425)
fish.plot(8053)


# pen 8 x,y plots
par(mfrow=c(3,3))
fish.3plot('7857', '7773', '7437')
fish.3plot('9145', '8165', '9173')
fish.3plot('9677', '7745', '8529')
fish.3plot('9033', '7101', '8277')
fish.3plot('8109', '9537', '7661')
fish.plot('7241')
fish.plot('7409')


# pen 7 x,y plot
par(mfrow=c(1,1))
fishpal <- rainbow_hcl(20, c=100, l=63, start=-360, end=-32, alpha = 0.2)
fish.plot('9425')
add.fish('7829', fishcol = fishpal[19])
add.fish('8081', fishcol = fishpal[18])
add.fish('7213', fishcol = fishpal[17])
add.fish('7269', fishcol = fishpal[16])
add.fish('7045', fishcol = fishpal[15])
add.fish('9229', fishcol = fishpal[14])
add.fish('9873', fishcol = fishpal[13])
add.fish('7381', fishcol = fishpal[12])
add.fish('9901', fishcol = fishpal[11])
add.fish('9453', fishcol = fishpal[10])
add.fish('7129', fishcol = fishpal[9])
add.fish('9397', fishcol = fishpal[8])
add.fish('8025', fishcol = fishpal[7])
add.fish('8417', fishcol = fishpal[6])
add.fish('9649', fishcol = fishpal[5])
add.fish('8053', fishcol = fishpal[4])



# pen 8 x,y plot
par(mfrow=c(1,1))
fishpal <- rainbow_hcl(20, c=100, l=63, start=-360, end=-32, alpha = 0.2)
fish.plot('7857')
add.fish('7773', fishcol = fishpal[19])
add.fish('7437', fishcol = fishpal[18])
add.fish('9145', fishcol = fishpal[17])
add.fish('8165', fishcol = fishpal[16])
add.fish('9173', fishcol = fishpal[15])
add.fish('9677', fishcol = fishpal[14])
add.fish('7745', fishcol = fishpal[13])
add.fish('8529', fishcol = fishpal[12])
add.fish('9033', fishcol = fishpal[11])
add.fish('7101', fishcol = fishpal[10])
add.fish('8277', fishcol = fishpal[9])
add.fish('8109', fishcol = fishpal[8])
add.fish('9537', fishcol = fishpal[7])
add.fish('7661', fishcol = fishpal[6])
add.fish('7241', fishcol = fishpal[5])
add.fish('7409', fishcol = fishpal[4])



# pen 7 depth plots
par(mfrow=c(3,3))
fish.3depth('7829', '8081', '7213')
fish.3depth('7269', '7045', '9229')
fish.3depth('9873', '7381', '9901')
fish.3depth('9453', '7129', '9397')
fish.3depth('8025', '8417', '9649')
fish.depth(9425)
fish.depth(8053)


# pen 8 depth plots
par(mfrow=c(3,3))
fish.3depth('7857', '7773', '7437')
fish.3depth('9145', '8165', '9173')
fish.3depth('9677', '7745', '8529')
fish.3depth('9033', '7101', '8277')
fish.3depth('8109', '9537', '7661')
fish.depth('7241')
fish.depth('7409')

# pen 7 x,y plot by depth
par(mfrow=c(1,1))
depthpal <- diverge_hcl(30, h = c(11,266), c = 100, l = c(21,85), power = 0.6, alpha = 0.2)
plot.bydepth('9425')
add.depthfish('7829')
add.depthfish('8081')
add.depthfish('7213')
add.depthfish('7269')
add.depthfish('7045')
add.depthfish('9229')
add.depthfish('9873')
add.depthfish('7381')
add.depthfish('9901')
add.depthfish('9453')
add.depthfish('7129')
add.depthfish('9397')
add.depthfish('8025')
add.depthfish('8417')
add.depthfish('9649')
add.depthfish('8053')
rect(locations.lookup['7EW', 'xmin'], locations.lookup['7EW', 'ymin'], locations.lookup['7EW', 'xmax'], locations.lookup['7EW', 'ymax'], lty = 2) # 7EW edge
rect(locations.lookup['7ES', 'xmin'], locations.lookup['7ES', 'ymin'], locations.lookup['7ES', 'xmax'], locations.lookup['7ES', 'ymax'], lty = 2) # 7ES edge
rect(locations.lookup['7EE', 'xmin'], locations.lookup['7EE', 'ymin'], locations.lookup['7EE', 'xmax'], locations.lookup['7EE', 'ymax'], lty = 2) # 7EE edge
rect(locations.lookup['7EN', 'xmin'], locations.lookup['7EN', 'ymin'], locations.lookup['7EN', 'xmax'], locations.lookup['7EN', 'ymax'], lty = 2) # 7EN edge
rect(locations.lookup['7WHSE', 'xmin'], locations.lookup['7WHSE', 'ymin'], locations.lookup['7WHSE', 'xmax'], locations.lookup['7WHSE', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHSE
rect(locations.lookup['7WHNW', 'xmin'], locations.lookup['7WHNW', 'ymin'], locations.lookup['7WHNW', 'xmax'], locations.lookup['7WHNW', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHNW
rect(locations.lookup['7EW', 'xmin'], locations.lookup['7ES', 'ymin'], locations.lookup['7EE', 'xmax'], locations.lookup['7EN', 'ymax'], lwd = 2) # cage limits

#legend(32, 42, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30), fill = depthpal, pch = 20, cex = 0.7)


# pen 8 x,y plot by depth
par(mfrow=c(1,1))
depthpal <- diverge_hcl(30, h = c(11,266), c = 100, l = c(21,85), power = 0.6, alpha = 0.2)
plot.bydepth('7857')
add.depthfish('7773')
add.depthfish('7437')
add.depthfish('9145')
add.depthfish('8165')
add.depthfish('9173')
add.depthfish('9677')
add.depthfish('7745')
add.depthfish('8529')
add.depthfish('9033')
add.depthfish('7101')
add.depthfish('8277')
add.depthfish('8109')
add.depthfish('9537')
add.depthfish('7661')
add.depthfish('7241')
add.depthfish('7409')
rect(locations.lookup['8EW', 'xmin'], locations.lookup['8EW', 'ymin'], locations.lookup['8EW', 'xmax'], locations.lookup['8EW', 'ymax'], lty = 2) # 7EW edge
rect(locations.lookup['8ES', 'xmin'], locations.lookup['8ES', 'ymin'], locations.lookup['8ES', 'xmax'], locations.lookup['8ES', 'ymax'], lty = 2) # 7ES edge
rect(locations.lookup['8EE', 'xmin'], locations.lookup['8EE', 'ymin'], locations.lookup['8EE', 'xmax'], locations.lookup['8EE', 'ymax'], lty = 2) # 7EE edge
rect(locations.lookup['8EN', 'xmin'], locations.lookup['8EN', 'ymin'], locations.lookup['8EN', 'xmax'], locations.lookup['8EN', 'ymax'], lty = 2) # 7EN edge
rect(locations.lookup['8WHSW', 'xmin'], locations.lookup['8WHSW', 'ymin'], locations.lookup['8WHSW', 'xmax'], locations.lookup['8WHSW', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHSE
rect(locations.lookup['8WHNE', 'xmin'], locations.lookup['8WHNE', 'ymin'], locations.lookup['8WHNE', 'xmax'], locations.lookup['8WHNE', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHNW
rect(locations.lookup['8EW', 'xmin'], locations.lookup['8ES', 'ymin'], locations.lookup['8EE', 'xmax'], locations.lookup['8EN', 'ymax'], lwd = 2) # cage limits


# plot hides
temp <- dayfile
dayfile <- subset(temp, temp$Period == '11805' | temp$Period == '11553' | temp$Period == '11217' | temp$Period == '10965' | temp$Period == '10657' | temp$Period == '10377' | temp$Period == '9761' | temp$Period == '9313')
fishpal <- rainbow_hcl(20, c=100, l=63, start=-360, end=-32, alpha = 0.2)
dayfile$PEN <- '7'
fish.plot(11805)
add.fish('11553', fishcol = fishpal[1])
add.fish('11217', fishcol = fishpal[15])
add.fish('10965', fishcol = fishpal[5])
dayfile$PEN <- '8'
fish.plot(10657)
add.fish('10377', fishcol = fishpal[1])
add.fish('9761', fishcol = fishpal[13])
add.fish('9313', fishcol = fishpal[4])


#1 plot
par(mfrow=c(1,1))

#subset all fish from 1 pen
fish.id <- subset(dayfile, PEN == '7')

#mean fish swim speed
mean(fish.id$MSEC)

#create list of all files in working directory
files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)

# code for manaully removing dead fish ------------------------------------------------------------------------------------

tot.days <- unique(format(as.Date(dayfile$EchoTime, format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d')) # returns list of days in file
tot.days

dayfile <- dayfile[!(dayfile$Period == 7017),] # remove dead fish

write.csv(dayfile, file = dayfile.loc) #write output to file


# probability density functions


df = ggplot(subset(dayfile, PEN == '7' | PEN == '8'), aes(x=BLSEC, colour = PEN)) + theme(panel.background = element_rect(fill = 'white', colour = 'black'))
df = df + scale_x_log10(breaks = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), labels = c(0.01, '', '', '', '', '', '', '', '', 0.1, '', '', '', '', '', '', '', '', 1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100))
df = df + scale_y_log10(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000)) 
df + geom_freqpoly(binwidth = 0.5, size = 1)



# calculate behaviour state frequencies

bsffile <- dayfile[,c('Period', 'PEN', 'SEC', 'BLSEC')]
bsffile$BSF <- ifelse(bsffile$BLSEC <= 0.1, 'static', ifelse(bsffile$BLSEC > 0.1 & bsffile$BLSEC <= 1, 'cruise', 'burst'))
#dayfile$BSF <- ifelse(dayfile$BLSEC <= 0.1, 1, ifelse(dayfile$BLSEC > 0.1 & dayfile$BLSEC <= 1, 2, 3))
bsffile$BSFcount <- sequence(rle(bsffile$BSF)$lengths)
bsffile$CountTF <- c(ifelse(diff(bsffile$BSFcount, 1, 1) < 1, T, F), F)

bsfsum <- 0

for (i in 1:nrow(bsffile)){
  
  bsfsum <- bsfsum + bsffile[i, 'SEC']
  
  if(bsffile[i, 'CountTF'] == T & is.na(bsffile[i, 'SEC']) == F){
    
  bsffile[i,'BSFdur'] <- bsfsum
  bsfsum <- 0
    
  } else {
  
  bsffile[i,'BSFdur'] <- NA    
    
  }
  
}


# bsffile$round <- ifelse(bsffile$BSFdur < 100, round(bsffile$BSFdur, -1), ifelse(bsffile$BSFdur > 99, bsffile$round <- round(bsffile$BSFdur, -2), bsffile$round <- round(bsffile$BSFdur, -3)))

bsffile$round <- as.numeric(as.character(cut(bsffile$BSFdur, breaks = c(0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000), labels = c('1', '2', '5', '10', '20', '50', '100', '200', '500', '1000'))))


# generates table of BSF frequencies and draws plot

dayfile$BSF <- as.factor(dayfile$BSF)

bsftab <- as.data.frame(table(bsffile$round, bsffile$BSF, bsffile$PEN)) # tabulate frequencies of each duration and BSF
names(bsftab) <- c('dur', 'BSF', 'pen', 'freq')
bsftab$dur <- as.numeric(as.character(bsftab$dur))
bsftab$freq <- as.numeric(bsftab$freq)

# bsftab$bin <- .bincode(bsftab$freq, seq(1, 10000, 10))


# df = ggplot(bsftab, aes(x=dur, y=freq, colour = BSF)) + theme(panel.background = element_rect(fill = 'white', colour = 'black'))

sp = ggplot(subset(bsftab, BSF == 'static'), aes(x=dur, y=freq, colour = pen)) + theme(panel.background = element_rect(fill = 'white', colour = 'black'))
sp = sp + scale_x_log10(limits = c(10, 1000), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000))
sp = sp + scale_y_log10(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000)) 
sp = sp + geom_path(size = 1) + labs(title = 'Static', x = 'duration', y = 'frequency')

cp = ggplot(subset(bsftab, BSF == 'cruise'), aes(x=dur, y=freq, colour = pen)) + theme(panel.background = element_rect(fill = 'white', colour = 'black'))
cp = cp + scale_x_log10(limits = c(10, 1000), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000))
cp = cp + scale_y_log10(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000)) 
cp = cp + geom_path(size = 1) + labs(title = 'Cruise', x = 'duration', y = 'frequency')

bp = ggplot(subset(bsftab, BSF == 'burst'), aes(x=dur, y=freq, colour = pen)) + theme(panel.background = element_rect(fill = 'white', colour = 'black'))
bp = bp + scale_x_log10(limits = c(10, 1000), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000))
bp = bp + scale_y_log10(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000)) 
bp = bp + geom_path(size = 1) + labs(title = 'Burst', x = 'duration', y = 'frequency')


plot_grid(sp, cp, bp, align = 'hv', nrow = 2, ncol = 2)

#-------------------------------------------------------------------------------------------------------------------------------


ani.options(interval = 0.01)

saveGIF({  
  
  
  for (i in 1:100){
    plot(fish.id[1:i,'PosX'], fish.id[1:i,'PosY'], xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 0.8, xlim = c(5, 36), ylim = c(8, 41), type = 'p', col = fishpal[20]) # tight plot
    
    
  }
  
})



fish.id <- subset(dayfile, dayfile$PosY > 10 & dayfile$Period == 7409)
dayfile <- subset(dayfile, !(dayfile$Period == 7409))
dayfile <- rbind(dayfile, fish.id)


fish.id <- subset(dayfile, dayfile$Period == 8949)
fish.id <- subset(fish.id, duplicated(fish.id$EchoTime) == FALSE)

# code to day average env probe data
probe <- probe.DOT2
probe$day <- as.Date(probe$DO.time.2m)
mean.temp2m <- tapply(probe$Temp.2m, probe$day, mean)


# code to create animated gif from sequence of plot images

system.time({
  setwd(paste0(workingdir, '/animate'))
  files <- list.files(path = paste0(workingdir, '/animate'), pattern = '*.png', all.files = FALSE, recursive = FALSE)
  
  anim.frames <- image_read(files)
  
  animation <- image_animate(anim.frames, fps = 2, loop = 0, dispose = 'previous')
  
  image_write(animation, 'anim.gif')
}
)



# log scale and labels for activity histograms

# conditioned wrasse
hdep + scale_x_log10(breaks = c(0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008, 0.009, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.8, 0.09, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels = c('0.001', '', '', '', '', '', '', '', '', '0.01', '', '', '', '', '', '', '', '', '0.1', '', '', '', '', '', '', '', '', '1', '', '', '', '', '', '', '', '', '10')) + scale_y_continuous(limits = c(0, 100000)) + ggtitle('Conditioned wrasse activity histogram')

# unconditioned wrasse
hdep + scale_x_log10(breaks = c(0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008, 0.009, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.8, 0.09, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels = c('0.001', '', '', '', '', '', '', '', '', '0.01', '', '', '', '', '', '', '', '', '0.1', '', '', '', '', '', '', '', '', '1', '', '', '', '', '', '', '', '', '10')) + scale_y_continuous(limits = c(0, 100000)) + ggtitle('Unconditioned wrasse activity histogram')



# Analysis of behaviour states

daytemp <- dayfile
dayfile <- subset(daytemp, BS == 'Rr')

bstab <- table(dayfile$BS)
round(bstab/sum(bstab)*100, 2)

# kernel utilisation distributions using adehabitat

kudcols <- c(brewer.pal(4, 'Accent')[[1]], brewer.pal(4, 'Accent')[[4]]) # create colour palette for KUDs

x <- seq(0, 60, by = 0.5)
y <- seq(0, 60, by = 0.5)
xy <- expand.grid(x=x, y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE
class(xy)

coords <- dayfile[,c(1, 5, 6)] # extract x,y coords and fish id from dayfile
coordinates(coords) <- c('PosX', 'PosY') # convert to spatial points data frame object
ud <- kernelUD(coords, h = 'href', grid = xy, kern = 'bivnorm') # KUD calculation for adehabitatHR package

mcp100 <- mcp(coords, percent = 100)
ver50 <- getverticeshr(ud, 50) # extract 50% vertex for plotting
ver95 <- getverticeshr(ud, 95) # extract 95% vertex for plotting
plot(mcp100, col = NULL, axes = T, xlim = c(10, 45), ylim = c(0, 45)) # plot MCP100
plot(ver95, col = kudcols[[1]], axes = F, xlim = c(10, 45), ylim = c(0, 45), add=T) # plot KUD95
plot(ver50, col = kudcols[[2]], axes = F, xlim = c(10, 45), ylim = c(0, 45), add=T) # plot KUD50

ka <- kernel.area(ud, percent = c(50, 95), unin = 'm', unout = 'm2') # calculates area of KUD50, KUD95 and MCP100

ud <- kernelUD(dayfile[,5:6], h = 'href', grid = 50, same4all = T, kern = 'bivnorm') # KUD calculation for adehabitat package

mcpest <- mcp(dayfile[,5:6], id = dayfile$Period, percent = 100)
getvolumeUD(ud)

image(ud, axes = T, addpoints = F, xlim = c(0, 45))
image.khr(ud, addpoints = F, axes = T, xlim = c(10, 45), ylim = c(10, 50))

plot(mcpest, xlim = c(10, 45), ylim = c(0, 45))
par(new=T)
plot(ver, xlim = c(10, 45), ylim = c(0, 45), axes = F, fg = 'blue')
par(new=F)


# Code to calculate KUD50 and KUD95 for all fish in loaded dayfile and save as csv-----------------------------------------------------------
# N.B. this code will only work properly if all fish are from the same pen, so loading all fish from one group works best

fish <- unique(dayfile$Period)

if(unique(dayfile$PEN == 7)){
  
  x <- seq(0, 50, by = 0.5)
  y <- seq(0, 50, by = 0.5)
  xy <- expand.grid(x=x, y=y)
  coordinates(xy) <- ~x+y
  gridded(xy) <- TRUE
  class(xy)
  
} else {
  
  x <- seq(25, 70, by = 0.5)
  y <- seq(0, 50, by = 0.5)
  xy <- expand.grid(x=x, y=y)
  coordinates(xy) <- ~x+y
  gridded(xy) <- TRUE
  class(xy)  

}  

kud50 <- numeric()
kud95 <- numeric()
kudtab <- data.frame()

for (i in 1:length(fish)){
  
  daytemp <- subset(dayfile, Period == fish[[i]])
  
  coords <- daytemp[,c(1, 5, 6)] # extract x,y coords and fish id from dayfile
  coordinates(coords) <- c('PosX', 'PosY') # convert to spatial points data frame object
  ud <- kernelUD(coords, h = 'href', grid = xy, kern = 'bivnorm') # KUD calculation for adehabitatHR package
  
  ka <- kernel.area(ud, percent = c(50, 95), unin = 'm', unout = 'm2') # calculates area of KUD50, KUD95
  
  kud50 <- c(kud50, ka[1,1])
  kud95 <- c(kud95, ka[2,1])
  
}

kudtab <- cbind(fish, kud50, kud95)


# Code to calculate 3D KUD50 and KUD95 for all fish in loaded dayfile and save as csv-----------------------------------------------------------

fish <- unique(dayfile$Period)

if(unique(dayfile$PEN == 7)){
  
  x <- seq(0, 50, by = 0.5)
  y <- seq(0, 50, by = 0.5)
  xy <- expand.grid(x=x, y=y)
  coordinates(xy) <- ~x+y
  gridded(xy) <- TRUE
  class(xy)
  
} else {
  
  x <- seq(25, 70, by = 0.5)
  y <- seq(0, 50, by = 0.5)
  xy <- expand.grid(x=x, y=y)
  coordinates(xy) <- ~x+y
  gridded(xy) <- TRUE
  class(xy)  
  
}  

kud50 <- numeric()
kud95 <- numeric()
kudtab <- data.frame()
max.depth <- ceiling(max(dayfile$PosZ))

for (i in 1:length(fish)){
  
  daytemp <- subset(dayfile, Period == fish[[i]])
  #depth <- 0
  k50 <- 0
  k95 <- 0
  
  for (j in 1:max.depth){
  
  depthtemp <- subset(daytemp, PosZ > j-1 & PosZ < j)
    
  if (nrow(depthtemp) >4){
    
  coords <- depthtemp[,c(1, 5, 6)] # extract x,y coords and fish id from dayfile
  coordinates(coords) <- c('PosX', 'PosY') # convert to spatial points data frame object
  ud <- kernelUD(coords, h = 'href', grid = xy, kern = 'bivnorm') # KUD calculation for adehabitatHR package
  
  ka <- kernel.area(ud, percent = c(50, 95), unin = 'm', unout = 'm2') # calculates area of KUD50, KUD95
  k50 <- k50 + ka[1,1]
  k95 <- k95 + ka[2,1]
  #depth <- depth+1
  } else {
    k50 <- k50 + 0
    k95 <- k95 + 0
    #depth <- depth+1
  }

  }
  
  kud50 <- c(kud50, k50)
  kud95 <- c(kud95, k95)
  
}

kudtab <- cbind(fish, kud50, kud95)




# code to add day number to dayfile (need to do this for KUD code to work)

exp.dates <- unique(as.Date(dayfile$EchoTime))
exp.start <- 1
exp.length <- 30
exp.days <- seq(exp.start, exp.start+exp.length-1, 1)
names(exp.days) <- exp.dates
dayfile$day <- as.numeric(exp.days[as.character(as.Date(dayfile$EchoTime))])

# code to calculate cumulative KUD50s and KUD95s for each fish in loaded dayfile and save as csv -------------------------------------------


fish <- unique(dayfile$Period)

if(unique(dayfile$PEN == 7)){
  
  x <- seq(0, 50, by = 0.5)
  y <- seq(0, 50, by = 0.5)
  xy <- expand.grid(x=x, y=y)
  coordinates(xy) <- ~x+y
  gridded(xy) <- TRUE
  class(xy)
  
} else {
  
  x <- seq(25, 70, by = 0.5)
  y <- seq(0, 50, by = 0.5)
  xy <- expand.grid(x=x, y=y)
  coordinates(xy) <- ~x+y
  gridded(xy) <- TRUE
  class(xy)  
  
}  

kud50.cum <- data.frame()
kud95.cum <- data.frame()

for (i in 1:length(fish))
  
  {
  
  kud50 <- numeric()
  kud95 <- numeric()
  fishsub <- subset(dayfile, Period == fish[[i]])
  days <- unique(fishsub$day)
  prevdays <- dayfile[1,]
  prevdays <- prevdays[-c(1),]

  for (j in 1:length(unique(fishsub$day)))
    
    {
  
    daysub <- rbind(prevdays, subset(fishsub, day == days[[j]]))
    
    coords <- daysub[,c(1, 5, 6)] # extract x,y coords and fish id from dayfile
    coordinates(coords) <- c('PosX', 'PosY') # convert to spatial points data frame object
    ud <- kernelUD(coords, h = 'href', grid = xy, kern = 'bivnorm') # KUD calculation for adehabitatHR package
  
    ka <- kernel.area(ud, percent = c(50, 95), unin = 'm', unout = 'm2') # calculates area of KUD50, KUD95
  
    kud50 <- c(kud50, ka[1,1])
    kud95 <- c(kud95, ka[2,1])
    
    prevdays <- daysub
  
    }
  
  kud50.cum <- rbind(kud50.cum, kud50)
  kud95.cum <- rbind(kud95.cum, kud95)
  
  }

kud50.cum <- t(kud50.cum)
rownames(kud50.cum) <- days
colnames(kud50.cum) <- fish

kud95.cum <- t(kud95.cum)
rownames(kud95.cum) <- days
colnames(kud95.cum) <- fish

#plot cumulative kuds for all fish

par(mfrow=c(1,2))
plot(kud50.cum[,as.character(fish[1])], type = 'o', ylim = c(0,signif(max(kud50.cum), 2)))
for (k in 2:length(fish)){lines(kud50.cum[,as.character(fish[k])], type = 'o')}

plot(kud95.cum[,as.character(fish[1])], type = 'o', ylim = c(0,signif(max(kud95.cum), 2)))
for (k in 2:length(fish)){lines(kud95.cum[,as.character(fish[k])], type = 'o')}
par(mfrow=c(1,1))

# calculate asymptotes

asym <- numeric()

for (m in 1:length(fish))
  {
  
  kuddiff <- round(c(NA, abs(diff(kud50.cum[,as.character(fish[m])], 1)))/kud50.cum[,as.character(fish[m])], 3)

  for (n in 2:(length(kuddiff)-1))
    {
    if (kuddiff[n] <0.05 & kuddiff[n+1] <0.05){
    day.asym <- n+1
    break
    } else {
    day.asym <- NA
  }
  }
  asym <- c(asym, day.asym)
}

asym <- as.double(rownames(kud50.cum)[1])+asym-1
kud50.cum <- rbind(kud50.cum, asym)

asym <- numeric()

for (m in 1:length(fish))
{
  
  kuddiff <- round(c(NA, abs(diff(kud95.cum[,as.character(fish[m])], 1)))/kud95.cum[,as.character(fish[m])], 3)
  
  for (n in 2:(length(kuddiff)-1))
  {
    if (kuddiff[n] <0.05 & kuddiff[n+1] <0.05){
      day.asym <- n+1
      break
    } else {
      day.asym <- NA
    }
  }
  asym <- c(asym, day.asym)
}

asym <- as.double(rownames(kud95.cum)[1])+asym-1
kud95.cum <- rbind(kud95.cum, asym)

write.csv(kud50.cum, 'cumulativeKUD50.csv')
write.csv(kud95.cum, 'cumulativeKUD95.csv')


# code to calculate index of reuse (IOR) for each fish in loaded dayfile and save as csv -------------------------------------------


fish <- unique(dayfile$Period)

if(unique(dayfile$PEN == 7)){
  
  x <- seq(0, 50, by = 0.5)
  y <- seq(0, 50, by = 0.5)
  xy <- expand.grid(x=x, y=y)
  coordinates(xy) <- ~x+y
  gridded(xy) <- TRUE
  class(xy)
  
} else {
  
  x <- seq(25, 70, by = 0.5)
  y <- seq(0, 50, by = 0.5)
  xy <- expand.grid(x=x, y=y)
  coordinates(xy) <- ~x+y
  gridded(xy) <- TRUE
  class(xy)  
  
}  

kud50.ior <- data.frame()
kud95.ior <- data.frame()

for (i in 1:length(fish))
  
{
  
  ior50 <- numeric()
  ior95 <- numeric()
  fishsub <- subset(dayfile, Period == fish[[i]])
  days <- unique(fishsub$day)
  #prevdays <- dayfile[1,]
  #prevdays <- prevdays[-c(1),]
  
  
  daysub <- subset(fishsub, day == days[[1]])
  
  # calculate kuds for day 1
  
  coords <- daysub[,c(1, 5, 6)] # extract x,y coords and fish id from dayfile
  coordinates(coords) <- c('PosX', 'PosY') # convert to spatial points data frame object
  ud <- kernelUD(coords, h = 'href', grid = xy, kern = 'bivnorm') # KUD calculation for adehabitatHR package
  ka1 <- kernel.area(ud, percent = c(50, 95), unin = 'm', unout = 'm2') # calculates area of KUD50, KUD95
  
  kud1 <- coords # send coords to day 1 kud matrix
  kud1$Period <- 1 # recode ID to 1
  
  # calculae kuds for subsequent days and calculate iors
  
  for (j in 2:length(unique(fishsub$day)))
    
  {
    
    daysub <- subset(fishsub, day == days[[j]])
    
    coords <- daysub[,c(1, 5, 6)] # extract x,y coords and fish id from dayfile
    coordinates(coords) <- c('PosX', 'PosY') # convert to spatial points data frame object
    ud <- kernelUD(coords, h = 'href', grid = xy, kern = 'bivnorm') # KUD calculation for adehabitatHR package
    
    ka2 <- kernel.area(ud, percent = c(50, 95), unin = 'm', unout = 'm2') # calculates area of KUD50, KUD95
    
    kud2 <- coords # send coords to day 2 kud matrix
    kud2$Period <- 2 # recode ID to 2
    
    kud <- rbind(kud2, kud1) # combine coords for 2 days
    ov95 <- kerneloverlap(kud, method = 'HR', percent = 95) # calculate proportion 95% overlap of 2 days
    ov50 <- kerneloverlap(kud, method = 'HR', percent = 50) # calculate proportion 50% overlap of 2 days
    
    ov95 <- ov95[1,2]*ka2[2,1] # calculate area of kud95 overlap from proportion
    ov50 <- ov50[1,2]*ka2[1,1] # calculate area of kud50 overlap from proportion
    
    ta95 <- ka1[2,1]+ka2[2,1]
    ta50 <- ka1[1,1]+ka2[1,1]
    
    ior95 <- c(ior95, ov95/ta95)
    ior50 <- c(ior50, ov50/ta50)
    
    
    #kud50 <- c(kud50, ka[1,1])
    #kud95 <- c(kud95, ka[2,1])
    
    #prevdays <- daysub
    
    kud1 <- kud2
    kud1$Period <- 1
    ka1 <- ka2
    
  }
  
  kud50.ior <- rbind(kud50.ior, ior50)
  kud95.ior <- rbind(kud95.ior, ior95)
  
}

kud50.ior <- t(kud50.ior)
rownames(kud50.ior) <- days[-1]
colnames(kud50.ior) <- fish
kud50.ior[is.nan(kud50.ior)] <- 0 # replace NaNs with 0

kud95.ior <- t(kud95.ior)
rownames(kud95.ior) <- days[-1]
colnames(kud95.ior) <- fish
kud95.ior[is.nan(kud95.ior)] <- 0 # replace NaNs with 0

#plot daily IORs for all fish

par(mfrow=c(1,2))
plot(kud50.ior[,as.character(fish[1])], type = 'o', ylim = c(0,signif(max(kud50.ior), 2)))
for (k in 2:length(fish)){lines(kud50.ior[,as.character(fish[k])], type = 'o')}

plot(kud95.ior[,as.character(fish[1])], type = 'o', ylim = c(0,signif(max(kud95.ior), 2)))
for (k in 2:length(fish)){lines(kud95.ior[,as.character(fish[k])], type = 'o')}
par(mfrow=c(1,1))


write.csv(kud50.ior, 'IOR50.csv')
write.csv(kud95.ior, 'IOR95.csv')


# Spectrum wavelet sampling ---------------------------------------------------------------------------------------

# load all data into dayfile then move to daytemp and subset single fish to dayfile, then subset by parameter to test for periodicity, e.g. below 15m

#daytemp <- dayfile

wavfunc <- function(fish.id, subtype, subcode){
  
  # fish/group subset
  #dayfile <- subset(daytemp, Period == fish.id) # fish subset
  dayfile <- subset(daytemp, PEN == 7) # pen subset
  
  #calculate standardised detection frequencies
  
  #dayfile <- dayfile[order(dayfile$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
  dayfile <- dplyr::arrange(dayfile, EchoTime)
  
  datacut <- data.frame(dayfile$EchoTime, cuts = cut.POSIXt(dayfile$EchoTime, breaks = 'hour', labels = F)) # code hour by factor
  datacut$floor <- floor_date(datacut$dayfile.EchoTime, unit = 'hour') # floor dates to nearest hour
  
  hourbins <- data.frame(unique(datacut$floor), rle(datacut$cuts)$lengths) # create new data frame of hours and sum of pings for each hour
  colnames(hourbins) <- c('Date', 'sum')
  
  binlist <- data.frame(seq(floor_date(min(daytemp$EchoTime), unit = 'hour'), floor_date(max(daytemp$EchoTime), unit = 'hour'), by = 'hour')) # create list of all hours in dataset
  colnames(binlist) <- 'Date'
  
  hourbins <- binlist %>%
    left_join(hourbins, by = c('Date'='Date')) %>% # join time list to hourly ping sum list
    replace_na(list(sum = 1)) # replace nas with 1
  
  rownames(hourbins) <- hourbins$Date
  
  #hourbins <- data.frame(hourbins, daycuts = cut.POSIXt(hourbins$Date, breaks = 'day', labels = F)) # code day by factor
  #daymeans <- data.frame(tapply(hourbins$sum, hourbins$daycuts, mean)) # calculate daily mean ping rate and create new data frame of results
  
  #hourbins$daymean <- as.numeric(daymeans[as.character(hourbins$daycuts),]) # add daily mean ping rate to hourbins dataset
  #hourbins$SDF <- hourbins$sum/hourbins$daymean

  hourmean <- mean(hourbins$sum)
  SF <- hourbins$sum/hourmean # calculate standardising factor from control data
  
  rm(binlist, datacut, hourbins)
  
  
  # location subset
  if (subtype == 'c'){
    dayfile <- subset(dayfile, BIGC == '8CNW' | BIGC == '8CSW' | BIGC == '8CNE' | BIGC == '8CSE' | BIGC == '7CNW' | BIGC == '7CSW' | BIGC == '7CNE' | BIGC == '7CSE') # corner subset
    title <- paste0(as.character(fish.id), ' corners')
  }
  if (subtype == 'h'){
    dayfile <- subset(dayfile, HID == '8WHSW' | HID == '8WHNE' | HID == '7WHSE' | HID == '7WHNW') # hide subset
    title <- paste0(as.character(fish.id), ' hides')
  }
  if (subtype == 'd'){
    dayfile <- subset(dayfile, PosZ > 15) # depth subset
    title <- paste0(as.character(fish.id), ' >15m')
  }
  if (subtype == 'fb'){
    dayfile <- subset(dayfile, FDB == '8FBNE' | FDB == '8FBSW' | FDB == '7FBNW' | FDB == '7FBSE') # at feedblock subset
    title <- paste0(as.character(fish.id), ' feed blocks')
  }
  if (subtype == 'bs'){
    dayfile <- subset(dayfile, BS == subcode) # behaviour state subset
    title <- paste0(as.character(fish.id), ' ', as.character(subcode))
  }
  
  # Bin observations into hourly bins
  
  dayfile <- dayfile[order(dayfile$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
  
  datacut <- data.frame(dayfile$EchoTime, cuts = cut.POSIXt(dayfile$EchoTime, breaks = 'hour', labels = F)) # code hour by factor
  datacut$floor <- floor_date(datacut$dayfile.EchoTime, unit = 'hour') # floor dates to nearest hour
  
  hourbins <- data.frame(unique(datacut$floor), rle(datacut$cuts)$lengths) # create new data frame of hours and sum of pings for each hour
  colnames(hourbins) <- c('Date', 'sum')
  
  binlist <- data.frame(seq(floor_date(min(daytemp$EchoTime), unit = 'hour'), floor_date(max(daytemp$EchoTime), unit = 'hour'), by = 'hour')) # create list of all hours in dataset
  colnames(binlist) <- 'Date'
  
  hourbins <- binlist %>%
    left_join(hourbins, by = c('Date'='Date')) %>% # join time list to hourly ping sum list
    replace_na(list(sum = 1)) # replace nas with 1 (0 gives errors!)
  
  rownames(hourbins) <- hourbins$Date
  
  hourbins$SDF <- hourbins$sum/SF # calculate standardised detection frequencies
  
  rm(binlist, datacut)
  
  # create wavelets using WaveletComp package
  
  fish.wav <- analyze.wavelet(hourbins, "SDF",
                              loess.span = 0,
                              dt = 1, dj = 1/50,
                              lowerPeriod = 8,
                              upperPeriod = 48,
                              make.pval = T, n.sim = 10)
  
  # normalise power levels to 1
  pm <- fish.wav$Power # extract power matrix
  pfac <- 1/max(pm) # calculate normalising factor
  fish.wav$Power <- pm*pfac # normalise power matrix so max = 1
  rm(pm, pfac)
  
  wt.image2(fish.wav, color.key = 'i', n.levels = 250, show.date = T, col.contour = 'black', plot.ridge = F, siglvl = 0.05, 
            timelab = 'Date', periodlab = 'scale (h)', main = title,
            legend.params = list(lab = "wavelet power levels", label.digits = 2))
  
}



reconstruct(fish.wav, lwd = c(1,2), legend.coords = "bottomleft", plot.waves = F)#, sel.period = 24)

wt.avg(fish.wav, 'sum')


# get number of observations per fish per day
tab1 <- table(as.Date(dayfile$EchoTime), dayfile$Period)
is.na(tab1) <- !tab1
range(tab1[1:nrow(tab1),], na.rm = T)


#draw plots of behaviour states from bsf2 function output----------------------

grouppal <- c(brewer.pal(11, 'Spectral')[[2]], brewer.pal(11, 'Spectral')[[3]], brewer.pal(11, 'Spectral')[[4]], brewer.pal(11, 'Spectral')[[8]], brewer.pal(11, 'Spectral')[[9]], brewer.pal(11, 'Spectral')[[10]])

bstab$pen <- revalue(bstab$pen, c('7' = 'Acclimated', '8' = 'Non-acclimated')) # change group names
bstab$BS <- revalue(bstab$BS, c('Rr' = 'static', 'Rf' = 'milling', 'Ra' = 'active resident', 'Ep' = 'patrolling', 'Ef' = 'foraging', 'Ea' = 'active transient')) # rename behaviour states

bstab$dur <- bstab$dur/60 # convert duration to minutes

sp <- ggplot(bstab, aes(x=dur, y=freq, colour = pen, group = pen)) + theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
#  scale_x_log10(limits = c(10, 10000), 
#                breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), 
#                labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000)) +
  scale_x_log10(limits = c(0.1, 1000), 
                breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000), 
                labels = c(0.1, '', '', '', '', '', '', '', '', 1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000)) +
  scale_y_log10(limits = c(0.001, 1), breaks = c(0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008, 0.009, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.8, 0.09, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), 
                labels = c(bquote(10^-3), '', '', '', '', '', '', '', '', bquote(10^-2), '', '', '', '', '', '', '', '', bquote(10^-1), '', '', '', '', '', '', '', '', bquote(10^0))) +
  geom_path(size = 1) + labs(title = 'Behaviour state frequencies', x = 'duration (mins)', y = 'frequency') + scale_colour_manual(values = c(grouppal[[1]], grouppal[[4]])) + 
  theme(legend.title = element_blank()) +
  #guides(colour = F)# + geom_smooth(linetype = 'dashed',  method = 'nls', formula = y~a*x^b, se = F) + geom_text(size = 4.5, hjust = 0, aes(x = 100, y = 1, colour = grouppal[[2]], label = power_eqn(subset(bstab, pen == '7' & BS == 'Ea'))), parse = TRUE) + geom_text(size = 4.5, hjust = 0, aes(x = 100, y = 0.6, colour = grouppal[[1]], label = power_eqn(subset(bstab, pen == '8' & BS == 'Ea'))), parse = TRUE)
  facet_wrap(vars(BS))

#--------------------------------------------------------------------------------------------------------------------------------------------

#STATS

#one-way anova
dayfile2 <- subset(dayfile, BLSEC <10)
dayfile2$log_BLSEC <- log(dayfile2$BLSEC) # log transformation for normality
dayfile2 <- dayfile2[!is.infinite(dayfile2$log_BLSEC),] # remove infinite observations and copy to new data frame
dayfile2$log_BLSEC_trans <- dayfile2$log_BLSEC - floor(min(dayfile2$log_BLSEC)) # transpose to make all values positive (needs testing)
dayfile2$day <- ceiling((as.integer(dayfile2$EchoTime)-(17058*86400))/86400) # add new column of day No. (17058 is days since 1st Jan 1970 for start of experiment)


anova1 <- aov(log_BLSEC_trans~PEN, data = dayfile2) # one-way anova
summary(anova1)
sumanova <- unlist(summary(anova1)) # turns anova summary into vector

hist(unlist(subset(dayfile, PEN == 8, select = PosZ))) # histogram of pen 8 depths


# One-way anova to compare activity at different times of day-------------------------------------------------------------
# all comparisons are highly significant due to the big dataset (big Df)
# use eta squared to measure effect size

# extract required data
actdf <- dayfile[c(1, 3, 12, 46)] # extract required variables
actdf <- subset(actdf, Period == 8081) # extract single fish
actdf <- subset(actdf, SUN == 'D' | SUN == 'W' | SUN == 'K' | SUN == 'N') # extract observations with time of day codes
#actdf <- subset(actdf, SUN == 'D' | SUN == 'N') # extract observations with time of day codes
actdf$log_BLSEC <- log(actdf$BLSEC) # log transform data
actdf <- actdf[!is.infinite(actdf$log_BLSEC),] # remove infinite observations
actdf <- na.omit(actdf) # remove NAs
actdf$log_BLSEC_trans <- actdf$log_BLSEC - floor(min(actdf$log_BLSEC)) # transpose so all observations are positive


aovact <- aov(log_BLSEC_trans~SUN, data = actdf)
summary(aovact)
TukeyHSD(aovact)

boxplot(log_BLSEC_trans~SUN, data = actdf)
hist(log(actdf[which(actdf$SUN == 'W'),'BLSEC']))

library(lsr)
etaSquared(aovact)
# significant effect sizes
# small >0.01, medium >0.06, large >0.14

library(effsize)
cohen.d(actdf$log_BLSEC_trans, actdf$SUN)
# significant effect sizes
# small >0.2, medium >0.5, large >0.8


# Power analysis using pwr package-----------------------------

library(pwr)

cohen.ES(test = 'anov', size = 'medium')
cohen.ES(test = 'anov', size = 'small')
cohen.ES(test = 'anov', size = 'large')

pwr.anova.test(k = 2, f = 0.4, sig.level = 0.05, n = 40, power = NULL)

pwr.t.test(n = 10, d = 0.4, sig.level = 0.05, type = 'two')

pwr.f2.test(u = 10, v = NULL, f2 = 0.4, sig.level = 0.05, power = 0.8)


# One-way anova to compare depth and activity at different times of day for Precon B study -------------------------------------------------------------


# extract required data
statdf <- dayfile[,c(1, 3, 4, 7, 12, 46)] # extract required variables
#statdf <- subset(statdf, SUN == 'D')
statdf$date <- as.Date(statdf$EchoTime + hours(1))

statdf$log_BLSEC <- log(statdf$BLSEC) # log transform data
statdf <- statdf[!is.infinite(statdf$log_BLSEC),] # remove infinite observations
statdf <- na.omit(statdf) # remove NAs
statdf$log_BLSEC_trans <- statdf$log_BLSEC - floor(min(statdf$log_BLSEC)) # transpose so all observations are positive

statsubdf <- subset(statdf, PEN == 8) # extract group
statsubdf <- subset(statsubdf, SUN == 'D' | SUN == 'N') # extract observations with day and night codes
statsubdf <- subset(statsubdf, date == '2016-10-13')
#actdf <- subset(actdf, SUN == 'D' | SUN == 'N') # extract observations with time of day codes



# Mann-Whitney U Test for non-parametric depth data (coded in R as wilcox.test)
wilcox.test(statsubdf$PosZ~statsubdf$SUN)

# Cohen's d test for t-tests (Mann-Whitney is non-parametric t-test)
cohen.d(statsubdf$PosZ, statsubdf$SUN)
# significant effect sizes
# small >0.2, medium >0.5, large >0.8


# One-way anova with eta squared effect size test for activity data
aovact <- aov(log_BLSEC_trans~SUN, data = statsubdf)
summary(aovact)
TukeyHSD(aovact)

library(lsr)
etaSquared(aovact)
# significant effect sizes
# small >0.01, medium >0.06, large >0.14

# 2-way ANOVAS based on aggregated datasets-------------------------------------------------------------------------------

# http://www.sthda.com/english/wiki/two-way-anova-test-in-r

statdf <- dayfile[,c(1, 3, 4, 7, 12, 46)] # extract required variables
statdf$date <- as.Date(statdf$EchoTime + hours(1))

statsubdf <- subset(statdf, PEN == 7) # extract group
statsubdf <- subset(statsubdf, SUN == 'D' | SUN == 'N')

#aggregate dataset by means
#statsubdf <- aggregate(statsubdf, by = list(statsubdf$Period, statsubdf$PEN, statsubdf$date, statsubdf$SUN), FUN = mean, na.rm = T)
#statsubdf <- statsubdf[,c(1, 2, 3, 4, 8, 9)]
#colnames(statsubdf) <- c('Period', 'PEN', 'date', 'SUN', 'depth', 'speed')

aovresults <- aov(statsubdf$PosZ~statsubdf$SUN*statsubdf$date) # 2-way anova

summary(aovresults)

library(car)
Anova(aovresults, type = 'III') # computes test statistic for unbalanced design (unequal size datasets)

# https://www.r-bloggers.com/anova-and-tukeys-test-on-r/
library(agricolae)
hsdresult <- HSD.test(aovresults, c('statsubdf$date', 'statsubdf$SUN')) # Tukey HSD pairwise comparisons with grouping letters

# rearrange groupings dataset
hsdresult <- hsdresult$groups
hsdresult$comparison <- rownames(hsdresult)
rownames(hsdresult) <- seq(1, nrow(hsdresult), 1)
hsdresult <- hsdresult[,c(3, 1, 2)]
hsdresult <- arrange(hsdresult, comparison)
hsdresult


# calculate least-squares regressions of daily means per pen and compare slopes
library(lsmeans)

aggtest <- dayfile[,c(3, 7, 69)]
aggtest <- aggtest %>% group_by(PEN, day) %>% summarize_all(funs(mean, sd))
aggtest <- as.data.frame(aggtest)
#aggtest$day <- seq(1, 37, 1)
m.interaction <- lm(mean~day*PEN, data = aggtest)
anova(m.interaction)
m.lst <- lstrends(m.interaction, 'PEN', var = 'day')
m.lst
pairs(m.lst)

lm(mean~day, data = aggtest[aggtest$PEN == 8,]) # return slope and intercept


# Headings stat analysis------------------------------------------------

library(rcompanion)

statdf <- dayfile[,c(1, 3, 4, 12, 13, 47, 48)]
threshold <- 1
statdf <- subset(statdf, BLSEC >= threshold)
statdf <- subset(statdf, HEIGHT == 'S' | HEIGHT == 'N')

model <- lm(HEAD~HEIGHT, data = statdf)

freq <- hist(statdf$HEAD[statdf$PEN == '7'], breaks  = seq(0, 360, 45))
hfdf <- data.frame('heading' = freq$breaks[1:8], 'count' = freq$counts)
freq <- hist(statdf$HEAD[statdf$PEN == '8'], breaks  = seq(0, 360, 45))
hfdf$P8 <- freq$counts
colnames(hfdf) <- c('heading', 'acclimated', 'non_acclimated')
rownames(hfdf) <- hfdf$heading
hfdf$heading <- NULL
hfdf <- as.data.frame(hfdf)

# goodness of fit tests
#hfdf$acclimated <- hfdf$acclimated/sum(hfdf$acclimated)
#hfdf$non_acclimated <- hfdf$non_acclimated/sum(hfdf$non_acclimated)
hfdf$theo <- 1/8 # create theoretical distribution (equal distribution of heading frequencies)

chisq.test(x = hfdf$acclimated, p = hfdf$theo)
chisq.test(x = hfdf$non_acclimated, p = hfdf$theo)

# Cramer's V effect size test for nominal variables (http://rcompanion.org/handbook/H_03.html)
cramerVFit(x = hfdf$acclimated, p = hfdf$theo)
cramerVFit(x = hfdf$non_acclimated, p = hfdf$theo)

# small = > 0.042, medium = > 0.127 and large = > 0.212



# stats to calculate mean depth difference per day between wild and farmed fish (for publication plot)

statdf <- select(dayfile, Period, PEN, PosZ, day) %>%
  group_by(Period, PEN, day) %>%
  summarise(mdep = mean(PosZ)) 

statdf$day <- as.factor(statdf$day)
statdf$PEN <- as.factor(statdf$PEN)


for(i in 1:30){
  
  model <- aov(formula = mdep~PEN, data = filter(statdf, day == i))
  print(i)
  print(summary(model))
  
}

# anova to calculate difference between day 1 depth

statdf <- filter(statdf, day == '1')
model <- aov(mdep~PEN, data = statdf)


# Calculation of detection rates for methods paper

setwd('H:/Acoustic tag - Preconditioning B/Data processing/6a. Coded Day CSV/Final run/Recoded') # load unfiltered data for detection rates
load.all()

preconb$date <- as.Date(preconb$EchoTime)
preconb <- preconb %>% filter(Period %in% c(9873, 7381, 7045, 7269, 9901, 9425, 9649, 9453, 8025, 8081, 8277, 7241, 7409, 8109, 7101, 9145, 8529, 9173, 9537, 7857, 7437, 7745, 8165, 7661))

pbag <- select(preconb, Period, PEN, date) %>% 
  count(Period, date, name = 'dpings') %>% 
  mutate(ppings = round((60/(Period/1000))*1440)) %>% # calculate No. of pings in one day based on PRI
  mutate(pdiff = ppings-dpings) %>%
  mutate(pcdet = (dpings/ppings)*100) %>%
  filter(date != '2016-09-14') %>%
  group_by(date) %>% 
  summarise(mean = mean(pcdet), sd = sd(pcdet)) %>%
  mutate(day = seq(1, 29, 1))

write.csv(pbag, 'H:/Acoustic tag - Preconditioning B/Data processing/Filtered Data/Recoded Day CSV/Outputs/DetectionRates.csv')

ggplot(pbag) + 
  geom_line(aes(x = day, y = mean)) +
  geom_point(aes(x = day, y = mean)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(10, 100, 10), name = 'Detection rate (%)') +
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, 5), name = 'Experiment day') +
  geom_errorbar(aes(x = day, ymin = mean-sd, ymax = mean+sd))

# plot of daily detection rates for each tag
ggplot(pbag) +
  geom_line(aes(x = date, y = pcdet, group = Period, colour = as.factor(Period))) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(10, 100, 10), name = 'Detection rate (%)') +
  facet_wrap(~Period)

# Plot sea lice levels----------------------------------------

liceP7 <- XLConnect::readWorksheetFromFile('H:/Acoustic tag - Preconditioning B/AcousticTagFile_2016.xlsx', sheet = 6, startRow = 4, endRow = 28, startCol = 56, endCol = 63)
liceP7 <- filter(liceP7, Date > '2016-09-13' & Date < '2016-10-29' & !is.na(Total.lice))
liceP7$Total.lice <- liceP7$Total.lice/20
liceP7$pen <- 7

liceP8 <- XLConnect::readWorksheetFromFile('H:/Acoustic tag - Preconditioning B/AcousticTagFile_2016.xlsx', sheet = 6, startRow = 4, endRow = 28, startCol = 65, endCol = 72) 
liceP8 <- filter(liceP8, Date > '2016-09-13' & Date < '2016-10-29')
liceP8$Total.lice <- liceP8$Total.lice/20
liceP8$pen <- 8

lice <- rbind(liceP7, liceP8)
lice$pen <- as.factor(lice$pen)

ggplot(lice) +
  geom_line(aes(x = Date, y = Total.lice, group = pen, colour = pen), size = 1) +
  scale_x_datetime(name = 'Date', limits = c(as.POSIXct('2016-09-14'), as.POSIXct('2016-10-26')), date_breaks = '7 days', date_labels = '%d-%b', expand = c(0, 0)) +
  scale_y_continuous(name = 'Mean total lice', expand = c(0,0), limits = c(0, 11), breaks = seq(0, 11, 1)) +
  scale_colour_manual(values = c('7' = 'red', '8' = 'darkblue'), labels = c('Acclimatised', 'Non-acclimatised'), name = NULL) +
  ggtitle('Mean Total Lice')
  
ggplot(lice) +
  geom_line(aes(x = Date, y = Gravid.female, group = pen, colour = pen), size = 1) +
  scale_x_datetime(name = 'Date', limits = c(as.POSIXct('2016-09-14'), as.POSIXct('2016-10-26')), date_breaks = '7 days', date_labels = '%d-%b', expand = c(0, 0)) +
  scale_y_continuous(name = 'Mean gravid females', expand = c(0,0), limits = c(0, 0.7), breaks = seq(0, 0.7, 0.1)) +
  scale_colour_manual(values = c('7' = 'red', '8' = 'darkblue'), labels = c('Acclimatised', 'Non-acclimatised'), name = NULL) +
  ggtitle('Mean Gravid Female Lice')


# FUNCTIONS----------------------------------------------------------------------------------------------------------------------------------


# 1. FUNCTION TO CALCULATE SUMMARY OF FISH LOCATIONS
locations <- function()
{
  # pen 7 location summary
  dayfile.bot <- subset(dayfile, BOT == 'B' & PEN == '7' & SEC >= 0)
  dayfile.top <- subset(dayfile, BOT == 'Z' & PEN == '7' & SEC >= 0)
  dayfile.out <- subset(dayfile, OUT == '7OE' & PEN == '7' & SEC >= 0 | OUT == '7OS' & PEN == '7' & SEC >= 0 | OUT == '7ON' & PEN == '7' & SEC >= 0 | OUT == '7OW' & PEN == '7' & SEC >= 0)
  dayfile.edg <- subset(dayfile, EDG == '7EN' & PEN == '7' & SEC >= 0 | EDG == '7EW' & PEN == '7' & SEC >= 0 | EDG == '7ES' & PEN == '7' & SEC >= 0 | EDG == '7EE' & PEN == '7' & SEC >= 0)
  dayfile.hidc <- subset(dayfile, BIGC == '7CNW' & PEN == '7' & SEC >= 0 | BIGC == '7CSE' & PEN == '7' & SEC >= 0)
  dayfile.mtc <- subset(dayfile, BIGC == '7CSW' & PEN == '7' & SEC >= 0 | BIGC == '7CNE' & PEN == '7' & SEC >= 0)
  dayfile.cen <- subset(dayfile, CEN == '7MH' & PEN == '7' & SEC >= 0 | CEN == '7MM' & PEN == '7' & SEC >= 0 | CEN == '7ML' & PEN == '7' & SEC >= 0)
  dayfile.hid <- subset(dayfile, HID == '7WHSE' & PEN == '7' & SEC >= 0 | HID == '7WHNW' & PEN == '7' & SEC >= 0)
  dayfile.fdb <- subset(dayfile, FDB == '7FBSE'  & PEN == '7' & SEC >= 0| FDB == '7FBNW' & PEN == '7' & SEC >= 0)
  #location.sum <- data.frame(c(nrow(dayfile.bot), nrow(dayfile.top), nrow(dayfile.out), nrow(dayfile.edg), nrow(dayfile.bigc), nrow(dayfile.cen), nrow(dayfile.hid)))
  location.sum <- data.frame(c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.hidc$SEC, na.rm = T)/3600, sum(dayfile.mtc$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fdb$SEC, na.rm = T)/3600))
  rownames(location.sum) <- c('<15m', '>15m', 'outer', 'edge', 'hide_corner', 'empty_corner', 'centre', 'hides', 'feed block')
  colnames(location.sum) <- 'ConP7'
  
  # pen 8 location summary
  dayfile.bot <- subset(dayfile, BOT == 'B' & SEC >= 0 & PEN == '8' & SEC >= 0)
  dayfile.top <- subset(dayfile, BOT == 'Z' & PEN == '8' & SEC >= 0)
  dayfile.out <- subset(dayfile, OUT == '8OE' & PEN == '8' & SEC >= 0| OUT == '8OS' & PEN == '8' & SEC >= 0 | OUT == '8ON' & PEN == '8' & SEC >= 0 | OUT == '8OW' & PEN == '8' & SEC >= 0)
  dayfile.edg <- subset(dayfile, EDG == '8EN' & PEN == '8' & SEC >= 0 | EDG == '8EW' & PEN == '8' & SEC >= 0 | EDG == '8ES' & PEN == '8' & SEC >= 0 | EDG == '8EE' & PEN == '8' & SEC >= 0)
  dayfile.hidc <- subset(dayfile, BIGC == '8CSW' & PEN == '8' & SEC >= 0 | BIGC == '8CNE' & PEN == '8' & SEC >= 0)
  dayfile.mtc <- subset(dayfile, BIGC == '8CNW' & PEN == '8' & SEC >= 0 | BIGC == '8CSE' & PEN == '8' & SEC >= 0)
  dayfile.cen <- subset(dayfile, CEN == '8MH' & PEN == '8' & SEC >= 0 | CEN == '8MM' & PEN == '8' & SEC >= 0 | CEN == '8ML' & PEN == '8' & SEC >= 0)
  dayfile.hid <- subset(dayfile, HID == '8WHSW' & PEN == '8' & SEC >= 0 | HID == '8WHNE' & PEN == '8' & SEC >= 0)
  dayfile.fdb <- subset(dayfile, FDB == '8FBSW' & PEN == '8' & SEC >= 0 | FDB == '8FBNE' & PEN == '8' & SEC >= 0)
  #location.sum$UnconP8 <- c(nrow(dayfile.bot), nrow(dayfile.top), nrow(dayfile.out), nrow(dayfile.edg), nrow(dayfile.bigc), nrow(dayfile.cen), nrow(dayfile.hid))
  location.sum$UnconP8 <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.hidc$SEC, na.rm = T)/3600, sum(dayfile.mtc$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fdb$SEC, na.rm = T)/3600)
  location.sum
}

# 2. location summary for multiple day files. type = 'batch', 'days', or 'fish'

batch.locations <- function(type)
{
  
  locations.P7 <- data.frame(c('0', '0', '0', '0', '0', '0', '0', '0', '0'))
  colnames(locations.P7) <- 'ID'
  rownames(locations.P7) <- c('P7_<15m', 'P7_>15m', 'P7_outer', 'P7_edge', 'P7_hidecorner', 'P7_emptycorner', 'P7_centre', 'P7_hides', 'P7_feedblock')
  locations.P8 <- data.frame(c('0', '0', '0', '0', '0', '0', '0', '0', '0'))
  colnames(locations.P8) <- 'ID'
  rownames(locations.P8) <- c('P8_<15m', 'P8_>15m', 'P8_outer', 'P8_edge', 'P8_hidecorner', 'P8_emptycorner', 'P8_centre', 'P8_hides', 'P8_feedblock')
  
  get.locations7 <- function(){
    # pen 7 location summary
    dayfile.bot <<- subset(cutfile, BOT == 'B' & PEN == '7' & SEC >= 0)
    dayfile.top <<- subset(cutfile, BOT == 'Z' & PEN == '7' & SEC >= 0)
    dayfile.out <<- subset(cutfile, OUT == '7OE'  & PEN == '7' & SEC >= 0 | OUT == '7OS' & PEN == '7' & SEC >= 0 | OUT == '7ON' & PEN == '7' & SEC >= 0 | OUT == '7OW' & PEN == '7' & SEC >= 0)
    dayfile.edg <<- subset(cutfile, EDG == '7EN' & PEN == '7' & SEC >= 0 | EDG == '7EW' & PEN == '7' & SEC >= 0 | EDG == '7ES' & PEN == '7' & SEC >= 0 | EDG == '7EE' & PEN == '7' & SEC >= 0)
    dayfile.hidc <<- subset(cutfile, BIGC == '7CSE' & PEN == '7' & SEC >= 0 | BIGC == '7CNW' & PEN == '7' & SEC >= 0)
    dayfile.mtc <<- subset(cutfile, BIGC == '7CSW' & PEN == '7' & SEC >= 0 | BIGC == '7CNE' & PEN == '7' & SEC >= 0)
    dayfile.cen <<- subset(cutfile, CEN == '7MH' & PEN == '7' & SEC >= 0 | CEN == '7MM' & PEN == '7' & SEC >= 0 | CEN == '7ML' & PEN == '7' & SEC >= 0)
    dayfile.hid <<- subset(cutfile, HID == '7WHSE' & PEN == '7' & SEC >= 0 | HID == '7WHNW' & PEN == '7' & SEC >= 0)
    dayfile.fdb <<- subset(cutfile, FDB == '7FBSE' & PEN == '7' & SEC >= 0 | FDB == '7FBNW' & PEN == '7')
    #locations.P7[,as.character(i)] <<- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.hidc$SEC, na.rm = T)/3600, sum(dayfile.mtc$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fdb$SEC, na.rm = T)/3600)
  }
  
  get.locations8 <- function(){ 
    # pen 8 location summary
    dayfile.bot <<- subset(cutfile, BOT == 'B' & PEN == '8' & SEC >= 0)
    dayfile.top <<- subset(cutfile, BOT == 'Z' & PEN == '8' & SEC >= 0)
    dayfile.out <<- subset(cutfile, OUT == '8OE' & PEN == '8' & SEC >= 0 | OUT == '8OS' & PEN == '8' & SEC >= 0 | OUT == '8ON' & PEN == '8' & SEC >= 0 | OUT == '8OW' & PEN == '8' & SEC >= 0)
    dayfile.edg <<- subset(cutfile, EDG == '8EN' & PEN == '8' & SEC >= 0 | EDG == '8EW' & PEN == '8' & SEC >= 0 | EDG == '8ES' & PEN == '8' & SEC >= 0 | EDG == '8EE' & PEN == '8' & SEC >= 0)
    dayfile.hidc <<- subset(cutfile, BIGC == '8CSW' & PEN == '8' & SEC >= 0 | BIGC == '8CNE' & PEN == '8' & SEC >= 0)
    dayfile.mtc <<- subset(cutfile, BIGC == '8CNW' & PEN == '8' & SEC >= 0 | BIGC == '8CSE' & PEN == '8' & SEC >= 0)
    dayfile.cen <<- subset(cutfile, CEN == '8MH' & PEN == '8' & SEC >= 0 | CEN == '8MM' & PEN == '8' & SEC >= 0 | CEN == '8ML' & PEN == '8' & SEC >= 0)
    dayfile.hid <<- subset(cutfile, HID == '8WHSW' & PEN == '8' & SEC >= 0 | HID == '8WHNE' & PEN == '8' & SEC >= 0)
    dayfile.fdb <<- subset(cutfile, FDB == '8FBSW' & PEN == '8' & SEC >= 0 | FDB == '8FBNE' & PEN == '8')
    #locations.P8[,as.character(i)] <<- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.hidc$SEC, na.rm = T)/3600, sum(dayfile.mtc$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fdb$SEC, na.rm = T)/3600)
    
  }
  
  if(type == 'batch'){ # dayfiles in seperate files code
    
    files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)  
    
    for (i in 1:length(files))
    {
      dayfile.loc <- files[[i]]
      cutfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = dayfile.classes)
      
      #SORT BY TIME AND TAG
      cutfile <- cutfile[order(cutfile$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
      cutfile <- cutfile[order(cutfile$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
      
      get.locations7()
      locations.P7[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.hidc$SEC, na.rm = T)/3600, sum(dayfile.mtc$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fdb$SEC, na.rm = T)/3600)
      
      get.locations8()
      locations.P8[,as.character(i)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.hidc$SEC, na.rm = T)/3600, sum(dayfile.mtc$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fdb$SEC, na.rm = T)/3600)
      
    }
    
    location.sum <- rbind(locations.P7, locations.P8)  
    location.sum$ID <- NULL
    location.sum  
    write.csv(location.sum, 'LocationsOutput.csv')
    
  } else { 
    
    if(type == 'days'){
      
      days <- c(paste0(sort(unique(as.Date(dayfile$EchoTime))), ' 00:00:00'), paste0(max(unique(as.Date(dayfile$EchoTime)))+days(1), ' 00:00:00'))
      
      for(d in 1:length(days)-1){
        
        cutfile <- dayfile[dayfile$EchoTime > days[d] & dayfile$EchoTime < days[d+1],] 
        
        get.locations7()
        locations.P7[,as.character(d)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.hidc$SEC, na.rm = T)/3600, sum(dayfile.mtc$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fdb$SEC, na.rm = T)/3600)
        
        get.locations8()
        locations.P8[,as.character(d)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.hidc$SEC, na.rm = T)/3600, sum(dayfile.mtc$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fdb$SEC, na.rm = T)/3600)
        
      }
      
      location.sum <- rbind(locations.P7, locations.P8)  
      location.sum$ID <- NULL
      location.sum  
      write.csv(location.sum, 'LocationsOutput-days.csv')
      
    } else { # type == 'fish'
      
      fish <- sort(unique(dayfile$Period))
      
      for(f in 1:length(fish)){
        
        cutfile <- dayfile[dayfile$Period == fish[f],]
        
        get.locations7()
        locations.P7[,as.character(f)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.hidc$SEC, na.rm = T)/3600, sum(dayfile.mtc$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fdb$SEC, na.rm = T)/3600)
        
        get.locations8()
        locations.P8[,as.character(f)] <- c(sum(dayfile.bot$SEC, na.rm = T)/3600, sum(dayfile.top$SEC, na.rm = T)/3600, sum(dayfile.out$SEC, na.rm = T)/3600, sum(dayfile.edg$SEC, na.rm = T)/3600, sum(dayfile.hidc$SEC, na.rm = T)/3600, sum(dayfile.mtc$SEC, na.rm = T)/3600, sum(dayfile.cen$SEC, na.rm = T)/3600, sum(dayfile.hid$SEC, na.rm = T)/3600, sum(dayfile.fdb$SEC, na.rm = T)/3600)
        
      }
      
      location.sum <- rbind(locations.P7, locations.P8)  
      location.sum$ID <- NULL
      location.sum  
      write.csv(location.sum, 'LocationsOutput-fish.csv')
      
    }
    
  }
  
  #location.sum <- rbind(locations.P7, locations.P8)  
  #location.sum$ID <- NULL
  #location.sum  
  
  #write.csv(location.sum, 'LocationsOutput.csv')
}



# 3a. depth and activity summary
depact <- function()
{
  day <- subset(dayfile, SUN == 'D' & PEN == '7')
  night <- subset(dayfile, SUN == 'N' & PEN == '7')
  depact.sum <- data.frame(c(format(mean(day$PosZ), digits = 4), format(mean(night$PosZ), digits = 4), format(mean(day$MSEC), digits = 4), format(mean(night$MSEC), digits = 4)))
  rownames(depact.sum) <- c('mean depth day (m)', 'mean depth night (m)', 'mean activity day (BL/sec)', 'mean activity night (BL/sec)')
  colnames(depact.sum) <- 'mean.ConP7'
  depact.sum$sd.conP7 <-c(format(sd(day$PosZ), digits = 4), format(sd(night$PosZ), digits = 4), format(sd(day$MSEC), digits = 4), format(sd(night$MSEC), digits = 4))
  
  
  day <- subset(dayfile, SUN == 'D' & PEN == '8')
  night <- subset(dayfile, SUN == 'N' & PEN == '8')
  depact.sum$mean.UnconP8 <-c(format(mean(day$PosZ), digits = 4), format(mean(night$PosZ), digits = 4), format(mean(day$MSEC), digits = 4), format(mean(night$MSEC), digits = 4))
  depact.sum$sd.conP8 <-c(format(sd(day$PosZ), digits = 4), format(sd(night$PosZ), digits = 4), format(sd(day$MSEC), digits = 4), format(sd(night$MSEC), digits = 4))
  depact.sum
}


# 3b. depth and activity summary
depact.se <- function()
{
  day <- subset(dayfile, SUN == 'D' & PEN == '7')
  night <- subset(dayfile, SUN == 'N' & PEN == '7')
  depact.sum <- data.frame(c(format(mean(day$PosZ), digits = 4), format(mean(night$PosZ), digits = 4), format(mean(day$MSEC), digits = 4), format(mean(night$MSEC), digits = 4)))
  rownames(depact.sum) <- c('mean depth day (m)', 'mean depth night (m)', 'mean activity day (BL/sec)', 'mean activity night (BL/sec)')
  colnames(depact.sum) <- 'mean.ConP7'
  depact.sum$sd.conP7 <-c(format(sd(day$PosZ)/sqrt(length(day$PosZ)), digits = 4), format(sd(night$PosZ)/sqrt(length(night$PosZ)), digits = 4), format(sd(day$MSEC)/sqrt(length(day$MSEC)), digits = 4), format(sd(night$MSEC)/sqrt(length(night$MSEC)), digits = 4))
  
  
  day <- subset(dayfile, SUN == 'D' & PEN == '8')
  night <- subset(dayfile, SUN == 'N' & PEN == '8')
  depact.sum$mean.UnconP8 <-c(format(mean(day$PosZ), digits = 4), format(mean(night$PosZ), digits = 4), format(mean(day$MSEC), digits = 4), format(mean(night$MSEC), digits = 4))
  depact.sum$sd.conP8 <-c(format(sd(day$PosZ)/sqrt(length(day$PosZ)), digits = 4), format(sd(night$PosZ)/sqrt(length(night$PosZ)), digits = 4), format(sd(day$MSEC)/sqrt(length(day$MSEC)), digits = 4), format(sd(night$MSEC)/sqrt(length(night$MSEC)), digits = 4))
  depact.sum
}

# 4. function to return depth summary for each fish

depth.sum <- function(){
  sumfunc <- function(x){ c(min = min(x), max = max(x), range = max(x)-min(x), mean = mean(x), median = median(x), std = sd(x)) }
  depth.sum.tab <- cbind(Period = unique(dayfile$Period), do.call(rbind, tapply(dayfile$PosZ, dayfile$Period, sumfunc)))
  print(depth.sum.tab)
}


# 5. batch function to return matrix of mean and standard deviation depths for individual fish over multiple days

batch.depth <- function(){
  
  sumfunc <- function(x){ c(min = min(x), max = max(x), range = max(x)-min(x), mean = mean(x), median = median(x), std = sd(x)) }
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  depths.P7 <- data.frame(c('P7_dawn_mean', 'P7_dawn_stdev', 'P7_day_mean', 'P7_day_stdev', 'P7_dusk_mean', 'P7_dusk_stdev', 'P7_night_mean', 'P7_night_stdev'))
  colnames(depths.P7) <- 'ID'
  rownames(depths.P7) <- c('P7_dawn_mean', 'P7_dawn_stdev', 'P7_day_mean', 'P7_day_stdev', 'P7_dusk_mean', 'P7_dusk_stdev', 'P7_night_mean', 'P7_night_stdev')
  depths.P8 <- data.frame(c('P8_dawn_mean', 'P8_dawn_stdev', 'P8_day_mean', 'P8_day_stdev', 'P8_dusk_mean', 'P8_dusk_stdev', 'P8_night_mean', 'P8_night_stdev'))
  colnames(depths.P8) <- 'ID'
  rownames(depths.P8) <- c('P8_dawn_mean', 'P8_dawn_stdev', 'P8_day_mean', 'P8_day_stdev', 'P8_dusk_mean', 'P8_dusk_stdev', 'P8_night_mean', 'P8_night_stdev')
  
  for (i in 1:length(files))
  {
    dayfile.loc <- files[[i]]
    dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = c
                        (
                        'NULL', 'factor', 'factor', 'factor', 'POSIXct', 'double', 'double', 
                        'double', 'double', 'double', 'double', 'double', 'double', 'factor',
                        'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
                        'double', 'double', 'double', 'double', 'double', 'double', 'double',
                        'double', 'double', 'double', 'double', 'double', 'double', 'double',
                        'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                        'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                        'factor', 'factor', 'double', 'double', 'double', 'double', 'double', 
                        'double', 'double', 'double', 'double', 'double', 'double', 'double' 
                        )) #read data into table
    
    
    depths.dawn <- subset(dayfile, SUN == 'W' & PEN == '7')
    depths.day <- subset(dayfile, SUN == 'D' & PEN == '7')
    depths.dusk <- subset(dayfile, SUN == 'K' & PEN == '7')
    depths.night <- subset(dayfile, SUN == 'N' & PEN == '7')
    dawn.sum <- cbind(Period = unique(depths.dawn$Period), do.call(rbind, tapply(depths.dawn$PosZ, depths.dawn$Period, sumfunc)))
    day.sum <- cbind(Period = unique(depths.day$Period), do.call(rbind, tapply(depths.day$PosZ, depths.day$Period, sumfunc)))
    dusk.sum <- cbind(Period = unique(depths.dusk$Period), do.call(rbind, tapply(depths.dusk$PosZ, depths.dusk$Period, sumfunc)))
    night.sum <- cbind(Period = unique(depths.night$Period), do.call(rbind, tapply(depths.night$PosZ, depths.night$Period, sumfunc)))
    dawn.sum[is.na(dawn.sum)] <- 0
    day.sum[is.na(day.sum)] <- 0
    dusk.sum[is.na(dusk.sum)] <- 0
    night.sum[is.na(night.sum)] <- 0
    depths.P7[,as.character(i)] <- c(mean(dawn.sum[,'mean']), mean(dawn.sum[,'std']), mean(day.sum[,'mean']), mean(day.sum[,'std']), mean(dusk.sum[,'mean']), mean(dusk.sum[,'std']), mean(night.sum[,'mean']), mean(night.sum[,'std']))
    
    depths.dawn <- subset(dayfile, SUN == 'W' & PEN == '8')
    depths.day <- subset(dayfile, SUN == 'D' & PEN == '8')
    depths.dusk <- subset(dayfile, SUN == 'K' & PEN == '8')
    depths.night <- subset(dayfile, SUN == 'N' & PEN == '8')
    dawn.sum <- cbind(Period = unique(depths.dawn$Period), do.call(rbind, tapply(depths.dawn$PosZ, depths.dawn$Period, sumfunc)))
    day.sum <- cbind(Period = unique(depths.day$Period), do.call(rbind, tapply(depths.day$PosZ, depths.day$Period, sumfunc)))
    dusk.sum <- cbind(Period = unique(depths.dusk$Period), do.call(rbind, tapply(depths.dusk$PosZ, depths.dusk$Period, sumfunc)))
    night.sum <- cbind(Period = unique(depths.night$Period), do.call(rbind, tapply(depths.night$PosZ, depths.night$Period, sumfunc)))
    dawn.sum[is.na(dawn.sum)] <- 0
    day.sum[is.na(day.sum)] <- 0
    dusk.sum[is.na(dusk.sum)] <- 0
    night.sum[is.na(night.sum)] <- 0
    depths.P8[,as.character(i)] <- c(mean(dawn.sum[,'mean']), mean(dawn.sum[,'std']), mean(day.sum[,'mean']), mean(day.sum[,'std']), mean(dusk.sum[,'mean']), mean(dusk.sum[,'std']), mean(night.sum[,'mean']), mean(night.sum[,'std']))
  }
  
  depths.sum <- rbind(depths.P7, depths.P8)  
  #depths.sum$ID <- NULL
  depths.sum    
  loadWorkbook('DepthsOutput.xlsx', create = TRUE)
  writeWorksheetToFile('DepthsOutput.xlsx', depths.sum, 'Sheet 1')
}



# 6. batch function to return matrix of mean and standard error depths for all fish combined over multiple days

batch.totdepth <- function(type){
  
  sumfunc <- function(x){ c(min = min(x), max = max(x), range = max(x)-min(x), mean = mean(x), median = median(x), std = sd(x)) }
  
  depth.P7 <- data.frame(c('P7_dawn_mean', 'P7_dawn_se', 'P7_day_mean', 'P7_day_se', 'P7_dusk_mean', 'P7_dusk_se', 'P7_night_mean', 'P7_night_se', 'aov'))
  colnames(depth.P7) <- 'ID'
  rownames(depth.P7) <- c('P7_dawn_mean', 'P7_dawn_se', 'P7_day_mean', 'P7_day_se', 'P7_dusk_mean', 'P7_dusk_se', 'P7_night_mean', 'P7_night_se', 'aov')
  depth.P8 <- data.frame(c('P8_dawn_mean', 'P8_dawn_se', 'P8_day_mean', 'P8_day_se', 'P8_dusk_mean', 'P8_dusk_se', 'P8_night_mean', 'P8_night_se', 'aov'))
  colnames(depth.P8) <- 'ID'
  rownames(depth.P8) <- c('P8_dawn_mean', 'P8_dawn_se', 'P8_day_mean', 'P8_day_se', 'P8_dusk_mean', 'P8_dusk_se', 'P8_night_mean', 'P8_night_se', 'aov')
  
  
  if(type == 'batch'){  
    
    files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)  
    
    for (i in 1:length(files))
    {
      dayfile.loc <- files[[i]]
      dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = dayfile.classes) 
      
      
      depth.dawn <- subset(dayfile, SUN == 'W' & PEN == '7')
      depth.day <- subset(dayfile, SUN == 'D' & PEN == '7')
      depth.dusk <- subset(dayfile, SUN == 'K' & PEN == '7')
      depth.night <- subset(dayfile, SUN == 'N' & PEN == '7')
      depth.P7[,as.character(i)] <- c(mean(depth.dawn$PosZ), sd(depth.dawn$PosZ)/sqrt(length(depth.dawn)), mean(depth.day$PosZ), sd(depth.day$PosZ)/sqrt(length(depth.day)), mean(depth.dusk$PosZ), sd(depth.dusk$PosZ)/sqrt(length(depth.dusk)), mean(depth.night$PosZ), sd(depth.night$PosZ)/sqrt(length(depth.night)))
      
      depth.dawn <- subset(dayfile, SUN == 'W' & PEN == '8')
      depth.day <- subset(dayfile, SUN == 'D' & PEN == '8')
      depth.dusk <- subset(dayfile, SUN == 'K' & PEN == '8')
      depth.night <- subset(dayfile, SUN == 'N' & PEN == '8')
      depth.P8[,as.character(i)] <- c(mean(depth.dawn$PosZ), sd(depth.dawn$PosZ)/sqrt(length(depth.dawn)), mean(depth.day$PosZ), sd(depth.day$PosZ)/sqrt(length(depth.day)), mean(depth.dusk$PosZ), sd(depth.dusk$PosZ)/sqrt(length(depth.dusk)), mean(depth.night$PosZ), sd(depth.night$PosZ)/sqrt(length(depth.night)))
    }
    
  } else {
    
    if(type == 'days'){
      
      days <- c(paste0(sort(unique(as.Date(dayfile$EchoTime))), ' 00:00:00'), paste0(max(unique(as.Date(dayfile$EchoTime)))+days(1), ' 00:00:00'))
      
      for(d in 1:length(days)-1){
        
        daycut <- dayfile[dayfile$EchoTime > days[d] & dayfile$EchoTime < days[d+1],]
        
        depth.dawn <- subset(daycut, SUN == 'W' & PEN == '7') %>% 
          select(Period, PosZ)
        depth.day <- subset(daycut, SUN == 'D' & PEN == '7') %>% 
          select(Period, PosZ) 
        depth.dusk <- subset(daycut, SUN == 'K' & PEN == '7') %>% 
          select(Period, PosZ)
        depth.night <- subset(daycut, SUN == 'N' & PEN == '7') %>% 
          select(Period, PosZ)
        #mdep <- c(mean(depth.dawn$PosZ), mean(depth.day$PosZ), mean(depth.dusk$PosZ), mean(depth.night$PosZ))
        
        depth.dawn <- group_by(depth.dawn, Period) %>% 
          summarise(PosZ = mean(PosZ))
        depth.day <- group_by(depth.day, Period) %>% 
          summarise(PosZ = mean(PosZ))
        depth.dusk <- group_by(depth.dusk, Period) %>% 
          summarise(PosZ = mean(PosZ))
        depth.night <- group_by(depth.night, Period) %>% 
          summarise(PosZ = mean(PosZ))
        mdep <- c(mean(depth.dawn$PosZ), mean(depth.day$PosZ), mean(depth.dusk$PosZ), mean(depth.night$PosZ))
        sedep <-  c(sd(depth.dawn$PosZ)/sqrt(length(depth.dawn$PosZ)), sd(depth.day$PosZ)/sqrt(length(depth.day$PosZ)), sd(depth.dusk$PosZ)/sqrt(length(depth.dusk$PosZ)), sd(depth.night$PosZ)/sqrt(length(depth.night$PosZ)))
        
        # calculate day vs night anova
        if(nrow(depth.day) > 2 & nrow(depth.night) >2){
          depth.day$Period <- 'day'
          depth.night$Period <- 'night'
          aovdf <- rbind(depth.day, depth.night)
          model <- aov(PosZ~Period, aovdf)
          model <- summary(model)[[1]][["Pr(>F)"]][[1]]
        } else {
          model <- NA
        } 
        
        #depth.P7[,as.character(d)] <- c(mean(depth.dawn$PosZ), sd(depth.dawn$PosZ)/sqrt(length(depth.dawn$PosZ)), mean(depth.day$PosZ), sd(depth.day$PosZ)/sqrt(length(depth.day$PosZ)), mean(depth.dusk$PosZ), sd(depth.dusk$PosZ)/sqrt(length(depth.dusk$PosZ)), mean(depth.night$PosZ), sd(depth.night$PosZ)/sqrt(length(depth.night$PosZ)))
        depth.P7[,as.character(d)] <- c(mdep[[1]], sedep[[1]], mdep[[2]], sedep[[2]], mdep[[3]], sedep[[3]], mdep[[4]], sedep[[4]], model)
        
        
        depth.dawn <- subset(daycut, SUN == 'W' & PEN == '8') %>% 
          select(Period, PosZ)
        depth.day <- subset(daycut, SUN == 'D' & PEN == '8') %>% 
          select(Period, PosZ) 
        depth.dusk <- subset(daycut, SUN == 'K' & PEN == '8') %>% 
          select(Period, PosZ)
        depth.night <- subset(daycut, SUN == 'N' & PEN == '8') %>% 
          select(Period, PosZ)
        #mdep <- c(mean(depth.dawn$PosZ), mean(depth.day$PosZ), mean(depth.dusk$PosZ), mean(depth.night$PosZ))
        
        depth.dawn <- group_by(depth.dawn, Period) %>% 
          summarise(PosZ = mean(PosZ))
        depth.day <- group_by(depth.day, Period) %>% 
          summarise(PosZ = mean(PosZ))
        depth.dusk <- group_by(depth.dusk, Period) %>% 
          summarise(PosZ = mean(PosZ))
        depth.night <- group_by(depth.night, Period) %>% 
          summarise(PosZ = mean(PosZ))
        mdep <- c(mean(depth.dawn$PosZ), mean(depth.day$PosZ), mean(depth.dusk$PosZ), mean(depth.night$PosZ))
        sedep <-  c(sd(depth.dawn$PosZ)/sqrt(length(depth.dawn$PosZ)), sd(depth.day$PosZ)/sqrt(length(depth.day$PosZ)), sd(depth.dusk$PosZ)/sqrt(length(depth.dusk$PosZ)), sd(depth.night$PosZ)/sqrt(length(depth.night$PosZ)))
        
        # calculate day vs night anova
        if(nrow(depth.day) > 2 & nrow(depth.night) >2){
          depth.day$Period <- 'day'
          depth.night$Period <- 'night'
          aovdf <- rbind(depth.day, depth.night)
          model <- aov(PosZ~Period, aovdf)
          model <- summary(model)[[1]][["Pr(>F)"]][[1]]
        } else {
          model <- NA
        } 
        
        #depth.P8[,as.character(d)] <- c(mean(depth.dawn$PosZ), sd(depth.dawn$PosZ)/sqrt(length(depth.dawn$PosZ)), mean(depth.day$PosZ), sd(depth.day$PosZ)/sqrt(length(depth.day$PosZ)), mean(depth.dusk$PosZ), sd(depth.dusk$PosZ)/sqrt(length(depth.dusk$PosZ)), mean(depth.night$PosZ), sd(depth.night$PosZ)/sqrt(length(depth.night$PosZ)))
        depth.P8[,as.character(d)] <- c(mdep[[1]], sedep[[1]], mdep[[2]], sedep[[2]], mdep[[3]], sedep[[3]], mdep[[4]], sedep[[4]], model)
        
      }
      
    } else { # else type == fish
      
      fish <- sort(unique(dayfile$Period))
      
      for(f in 1:length(fish)){
        
        fishcut <- dayfile[dayfile$Period == fish[f],]
        
        depth.dawn <- subset(fishcut, SUN == 'W' & PEN == '7') %>% 
          select(EchoTime, PosZ)
        depth.day <- subset(fishcut, SUN == 'D' & PEN == '7') %>% 
          select(EchoTime, PosZ) 
        depth.dusk <- subset(fishcut, SUN == 'K' & PEN == '7') %>% 
          select(EchoTime, PosZ)
        depth.night <- subset(fishcut, SUN == 'N' & PEN == '7') %>% 
          select(EchoTime, PosZ)
        #mdep <- c(mean(depth.dawn$PosZ), mean(depth.day$PosZ), mean(depth.dusk$PosZ), mean(depth.night$PosZ))
        
        depth.dawn <- group_by(depth.dawn, as.Date(EchoTime)) %>% 
          summarise(PosZ = mean(PosZ))
        depth.day <- group_by(depth.day, as.Date(EchoTime)) %>% 
          summarise(PosZ = mean(PosZ))
        depth.dusk <- group_by(depth.dusk, as.Date(EchoTime)) %>% 
          summarise(PosZ = mean(PosZ))
        depth.night <- group_by(depth.night, as.Date(EchoTime)) %>% 
          summarise(PosZ = mean(PosZ))
        mdep <- c(mean(depth.dawn$PosZ), mean(depth.day$PosZ), mean(depth.dusk$PosZ), mean(depth.night$PosZ))
        sedep <-  c(sd(depth.dawn$PosZ)/sqrt(length(depth.dawn$PosZ)), sd(depth.day$PosZ)/sqrt(length(depth.day$PosZ)), sd(depth.dusk$PosZ)/sqrt(length(depth.dusk$PosZ)), sd(depth.night$PosZ)/sqrt(length(depth.night$PosZ)))
        
        # calculate day vs night anova
        if(nrow(depth.day) > 2 & nrow(depth.night) >2){
          depth.day$Period <- 'day'
          depth.night$Period <- 'night'
          aovdf <- rbind(depth.day, depth.night)
          model <- aov(PosZ~Period, aovdf)
          model <- summary(model)[[1]][["Pr(>F)"]][[1]]
        } else {
          model <- NA
        } 
        
        #depth.P7[,as.character(f)] <- c(mean(depth.dawn$PosZ), sd(depth.dawn$PosZ)/sqrt(length(depth.dawn$PosZ)), mean(depth.day$PosZ), sd(depth.day$PosZ)/sqrt(length(depth.day$PosZ)), mean(depth.dusk$PosZ), sd(depth.dusk$PosZ)/sqrt(length(depth.dusk$PosZ)), mean(depth.night$PosZ), sd(depth.night$PosZ)/sqrt(length(depth.night$PosZ)))
        depth.P7[,as.character(f)] <- c(mdep[[1]], sedep[[1]], mdep[[2]], sedep[[2]], mdep[[3]], sedep[[3]], mdep[[4]], sedep[[4]], model)
        
        depth.dawn <- subset(fishcut, SUN == 'W' & PEN == '8') %>% 
          select(EchoTime, PosZ)
        depth.day <- subset(fishcut, SUN == 'D' & PEN == '8') %>% 
          select(EchoTime, PosZ) 
        depth.dusk <- subset(fishcut, SUN == 'K' & PEN == '8') %>% 
          select(EchoTime, PosZ)
        depth.night <- subset(fishcut, SUN == 'N' & PEN == '8') %>% 
          select(EchoTime, PosZ)
        #mdep <- c(mean(depth.dawn$PosZ), mean(depth.day$PosZ), mean(depth.dusk$PosZ), mean(depth.night$PosZ))
        
        depth.dawn <- group_by(depth.dawn, as.Date(EchoTime)) %>% 
          summarise(PosZ = mean(PosZ))
        depth.day <- group_by(depth.day, as.Date(EchoTime)) %>% 
          summarise(PosZ = mean(PosZ))
        depth.dusk <- group_by(depth.dusk, as.Date(EchoTime)) %>% 
          summarise(PosZ = mean(PosZ))
        depth.night <- group_by(depth.night, as.Date(EchoTime)) %>% 
          summarise(PosZ = mean(PosZ))
        mdep <- c(mean(depth.dawn$PosZ), mean(depth.day$PosZ), mean(depth.dusk$PosZ), mean(depth.night$PosZ))
        sedep <-  c(sd(depth.dawn$PosZ)/sqrt(length(depth.dawn$PosZ)), sd(depth.day$PosZ)/sqrt(length(depth.day$PosZ)), sd(depth.dusk$PosZ)/sqrt(length(depth.dusk$PosZ)), sd(depth.night$PosZ)/sqrt(length(depth.night$PosZ)))
        
        # calculate day vs night anova
        if(nrow(depth.day) > 2 & nrow(depth.night) >2){
          depth.day$Period <- 'day'
          depth.night$Period <- 'night'
          aovdf <- rbind(depth.day, depth.night)
          model <- aov(PosZ~Period, aovdf)
          model <- summary(model)[[1]][["Pr(>F)"]][[1]]
        } else {
          model <- NA
        } 
        
        #depth.P8[,as.character(f)] <- c(mean(depth.dawn$PosZ), sd(depth.dawn$PosZ)/sqrt(length(depth.dawn$PosZ)), mean(depth.day$PosZ), sd(depth.day$PosZ)/sqrt(length(depth.day$PosZ)), mean(depth.dusk$PosZ), sd(depth.dusk$PosZ)/sqrt(length(depth.dusk$PosZ)), mean(depth.night$PosZ), sd(depth.night$PosZ)/sqrt(length(depth.night$PosZ)))
        depth.P8[,as.character(f)] <- c(mdep[[1]], sedep[[1]], mdep[[2]], sedep[[2]], mdep[[3]], sedep[[3]], mdep[[4]], sedep[[4]], model)
        
      }
      
    }
    
  }
  
  depths.sum <- rbind(depth.P7, depth.P8)  
  #depths.sum$ID <- NULL
  depths.sum    
  #loadWorkbook('DepthTotOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('DepthTotOutput.xlsx', depths.sum, 'Sheet 1')
  
  write.csv(depths.sum, 'DepthTotOutput.csv')
}


# 7. batch function to return matrix of mean and standard deviation activity for individual fish over multiple days

batch.activity <- function(){
  
  sumfunc <- function(x){ c(min = min(x), max = max(x), range = max(x)-min(x), mean = mean(x), median = median(x), std = sd(x)) }
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  activity.P7 <- data.frame(c('P7_dawn_mean', 'P7_dawn_stdev', 'P7_day_mean', 'P7_day_stdev', 'P7_dusk_mean', 'P7_dusk_stdev', 'P7_night_mean', 'P7_night_stdev'))
  colnames(activity.P7) <- 'ID'
  rownames(activity.P7) <- c('P7_dawn_mean', 'P7_dawn_stdev', 'P7_day_mean', 'P7_day_stdev', 'P7_dusk_mean', 'P7_dusk_stdev', 'P7_night_mean', 'P7_night_stdev')
  activity.P8 <- data.frame(c('P8_dawn_mean', 'P8_dawn_stdev', 'P8_day_mean', 'P8_day_stdev', 'P8_dusk_mean', 'P8_dusk_stdev', 'P8_night_mean', 'P8_night_stdev'))
  colnames(activity.P8) <- 'ID'
  rownames(activity.P8) <- c('P8_dawn_mean', 'P8_dawn_stdev', 'P8_day_mean', 'P8_day_stdev', 'P8_dusk_mean', 'P8_dusk_stdev', 'P8_night_mean', 'P8_night_stdev')
  
  for (i in 1:length(files))
  {
    dayfile.loc <- files[[i]]
    dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = c
                        (
                        'NULL', 'factor', 'factor', 'factor', 'POSIXct', 'double', 'double', 
                        'double', 'double', 'double', 'double', 'double', 'double', 'factor',
                        'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
                        'double', 'double', 'double', 'double', 'double', 'double', 'double',
                        'double', 'double', 'double', 'double', 'double', 'double', 'double',
                        'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                        'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                        'factor', 'factor', 'double', 'double', 'double', 'double', 'double', 
                        'double', 'double', 'double', 'double', 'double', 'double', 'double' 
                        )) #read data into table
    
    
    activity.dawn <- subset(dayfile, SUN == 'W' & PEN == '7')
    activity.day <- subset(dayfile, SUN == 'D' & PEN == '7')
    activity.dusk <- subset(dayfile, SUN == 'K' & PEN == '7')
    activity.night <- subset(dayfile, SUN == 'N' & PEN == '7')
    dawn.sum <- cbind(Period = unique(activity.dawn$Period), do.call(rbind, tapply(activity.dawn$BLSEC, activity.dawn$Period, sumfunc)))
    day.sum <- cbind(Period = unique(activity.day$Period), do.call(rbind, tapply(activity.day$BLSEC, activity.day$Period, sumfunc)))
    dusk.sum <- cbind(Period = unique(activity.dusk$Period), do.call(rbind, tapply(activity.dusk$BLSEC, activity.dusk$Period, sumfunc)))
    night.sum <- cbind(Period = unique(activity.night$Period), do.call(rbind, tapply(activity.night$BLSEC, activity.night$Period, sumfunc)))
    dawn.sum[is.na(dawn.sum)] <- 0
    day.sum[is.na(day.sum)] <- 0
    dusk.sum[is.na(dusk.sum)] <- 0
    night.sum[is.na(night.sum)] <- 0
    activity.P7[,as.character(i)] <- c(mean(dawn.sum[,'mean']), mean(dawn.sum[,'std']), mean(day.sum[,'mean']), mean(day.sum[,'std']), mean(dusk.sum[,'mean']), mean(dusk.sum[,'std']), mean(night.sum[,'mean']), mean(night.sum[,'std']))
    
    activity.dawn <- subset(dayfile, SUN == 'W' & PEN == '8')
    activity.day <- subset(dayfile, SUN == 'D' & PEN == '8')
    activity.dusk <- subset(dayfile, SUN == 'K' & PEN == '8')
    activity.night <- subset(dayfile, SUN == 'N' & PEN == '8')
    dawn.sum <- cbind(Period = unique(activity.dawn$Period), do.call(rbind, tapply(activity.dawn$BLSEC, activity.dawn$Period, sumfunc)))
    day.sum <- cbind(Period = unique(activity.day$Period), do.call(rbind, tapply(activity.day$BLSEC, activity.day$Period, sumfunc)))
    dusk.sum <- cbind(Period = unique(activity.dusk$Period), do.call(rbind, tapply(activity.dusk$BLSEC, activity.dusk$Period, sumfunc)))
    night.sum <- cbind(Period = unique(activity.night$Period), do.call(rbind, tapply(activity.night$BLSEC, activity.night$Period, sumfunc)))
    dawn.sum[is.na(dawn.sum)] <- 0
    day.sum[is.na(day.sum)] <- 0
    dusk.sum[is.na(dusk.sum)] <- 0
    night.sum[is.na(night.sum)] <- 0
    activity.P8[,as.character(i)] <- c(mean(dawn.sum[,'mean']), mean(dawn.sum[,'std']), mean(day.sum[,'mean']), mean(day.sum[,'std']), mean(dusk.sum[,'mean']), mean(dusk.sum[,'std']), mean(night.sum[,'mean']), mean(night.sum[,'std']))
  }
  
  activity.sum <- rbind(activity.P7, activity.P8)  
  #depths.sum$ID <- NULL
  activity.sum    
  #loadWorkbook('ActivityOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('ActivityOutput.xlsx', activity.sum, 'Sheet 1')
  
  write.xlsx(activity.sum, 'ActivityOutput.xlsx')
}


# 8. batch function to return matrix of mean and standard error activity for all fish combined over multiple days

batch.totactivity <- function(type){
  
  sumfunc <- function(x){ c(min = min(x), max = max(x), range = max(x)-min(x), mean = mean(x), median = median(x), std = sd(x)) }
  
  activity.P7 <- data.frame(c('P7_dawn_mean', 'P7_dawn_se', 'P7_day_mean', 'P7_day_se', 'P7_dusk_mean', 'P7_dusk_se', 'P7_night_mean', 'P7_night_se', 'P7_aov'))
  colnames(activity.P7) <- 'ID'
  rownames(activity.P7) <- c('P7_dawn_mean', 'P7_dawn_se', 'P7_day_mean', 'P7_day_se', 'P7_dusk_mean', 'P7_dusk_se', 'P7_night_mean', 'P7_night_se', 'P7_aov')
  activity.P8 <- data.frame(c('P8_dawn_mean', 'P8_dawn_se', 'P8_day_mean', 'P8_day_se', 'P8_dusk_mean', 'P8_dusk_se', 'P8_night_mean', 'P8_night_se', 'P8_aov'))
  colnames(activity.P8) <- 'ID'
  rownames(activity.P8) <- c('P8_dawn_mean', 'P8_dawn_se', 'P8_day_mean', 'P8_day_se', 'P8_dusk_mean', 'P8_dusk_se', 'P8_night_mean', 'P8_night_se', 'P8_aov')
  
  if(type == 'batch'){
    
    files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
    
    for (i in 1:length(files))
    {
      dayfile.loc <- files[[i]]
      dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = dayfile.classes) 
      
      activity.dawn <- subset(dayfile, SUN == 'W' & PEN == '7')
      activity.day <- subset(dayfile, SUN == 'D' & PEN == '7')
      activity.dusk <- subset(dayfile, SUN == 'K' & PEN == '7')
      activity.night <- subset(dayfile, SUN == 'N' & PEN == '7')
      activity.P7[,as.character(i)] <- c(mean(activity.dawn$BLSEC, na.rm = T), sd(activity.dawn$BLSEC, na.rm = T)/sqrt(length(activity.dawn)), mean(activity.day$BLSEC, na.rm = T), sd(activity.day$BLSEC, na.rm = T)/sqrt(length(activity.day)), mean(activity.dusk$BLSEC, na.rm = T), sd(activity.dusk$BLSEC, na.rm = T)/sqrt(length(activity.dusk)), mean(activity.night$BLSEC, na.rm = T), sd(activity.night$BLSEC, na.rm = T)/sqrt(length(activity.night)))
      
      activity.dawn <- subset(dayfile, SUN == 'W' & PEN == '8')
      activity.day <- subset(dayfile, SUN == 'D' & PEN == '8')
      activity.dusk <- subset(dayfile, SUN == 'K' & PEN == '8')
      activity.night <- subset(dayfile, SUN == 'N' & PEN == '8')
      activity.P8[,as.character(i)] <- c(mean(activity.dawn$BLSEC, na.rm = T), sd(activity.dawn$BLSEC, na.rm = T)/sqrt(length(activity.dawn)), mean(activity.day$BLSEC, na.rm = T), sd(activity.day$BLSEC, na.rm = T)/sqrt(length(activity.day)), mean(activity.dusk$BLSEC, na.rm = T), sd(activity.dusk$BLSEC, na.rm = T)/sqrt(length(activity.dusk)), mean(activity.night$BLSEC, na.rm = T), sd(activity.night$BLSEC, na.rm = T)/sqrt(length(activity.night)))
    }
    
    activity.sum <- rbind(activity.P7, activity.P8)  
    activity.sum    
    write.csv(activity.sum, 'ActivityTotOutput.csv')
    
    
  } else {
    
    if(type == 'days'){
      
      days <- c(paste0(sort(unique(as.Date(dayfile$EchoTime))), ' 00:00:00'), paste0(max(unique(as.Date(dayfile$EchoTime)))+days(1), ' 00:00:00'))
      
      for(d in 1:length(days)-1){
        
        daycut <- dayfile[dayfile$EchoTime > days[d] & dayfile$EchoTime < days[d+1],] 
        
        activity.dawn <- subset(daycut, SUN == 'W' & PEN == '7') %>% 
          select(Period, BLSEC)
        activity.day <- subset(daycut, SUN == 'D' & PEN == '7') %>% 
          select(Period, BLSEC) 
        activity.dusk <- subset(daycut, SUN == 'K' & PEN == '7') %>% 
          select(Period, BLSEC)
        activity.night <- subset(daycut, SUN == 'N' & PEN == '7') %>% 
          select(Period, BLSEC)
        #mact <- c(mean(activity.dawn$BLSEC, na.rm = T), mean(activity.day$BLSEC, na.rm = T), mean(activity.dusk$BLSEC, na.rm = T), mean(activity.night$BLSEC, na.rm = T))
        
        activity.dawn <- group_by(activity.dawn, Period) %>% 
          summarise(BLSEC = mean(BLSEC, na.rm = T))
        activity.day <- group_by(activity.day, Period) %>% 
          summarise(BLSEC = mean(BLSEC, na.rm = T))
        activity.dusk <- group_by(activity.dusk, Period) %>% 
          summarise(BLSEC = mean(BLSEC, na.rm = T))
        activity.night <- group_by(activity.night, Period) %>% 
          summarise(BLSEC = mean(BLSEC, na.rm = T))
        mact <- c(mean(activity.dawn$BLSEC, na.rm = T), mean(activity.day$BLSEC, na.rm = T), mean(activity.dusk$BLSEC, na.rm = T), mean(activity.night$BLSEC, na.rm = T))
        seact <-  c(sd(activity.dawn$BLSEC, na.rm = T)/sqrt(length(activity.dawn$BLSEC)), sd(activity.day$BLSEC, na.rm = T)/sqrt(length(activity.day$BLSEC)), sd(activity.dusk$BLSEC, na.rm = T)/sqrt(length(activity.dusk$BLSEC)), sd(activity.night$BLSEC, na.rm = T)/sqrt(length(activity.night$BLSEC)))
        #seact <-  c(sd(activity.dawn$BLSEC, na.rm = T), sd(activity.day$BLSEC, na.rm = T), sd(activity.dusk$BLSEC, na.rm = T), sd(activity.night$BLSEC, na.rm = T)) # sd
        
        # calculate day vs night anova
        if(nrow(activity.day) > 2 & nrow(activity.night) >2){
          activity.day$Period <- 'day'
          activity.night$Period <- 'night'
          aovdf <- rbind(activity.day, activity.night)
          model <- aov(BLSEC~Period, aovdf)
          model <- summary(model)[[1]][["Pr(>F)"]][[1]]
        } else {
          model <- NA
        } 
        
        #activity.P7[,as.character(d)] <- c(mean(activity.dawn$BLSEC), sd(activity.dawn$BLSEC)/sqrt(length(activity.dawn$BLSEC)), mean(activity.day$BLSEC), sd(activity.day$BLSEC)/sqrt(length(activity.day$BLSEC)), mean(activity.dusk$BLSEC), sd(activity.dusk$BLSEC)/sqrt(length(activity.dusk$BLSEC)), mean(activity.night$BLSEC), sd(activity.night$BLSEC)/sqrt(length(activity.night$BLSEC)))
        activity.P7[,as.character(d)] <- c(mact[[1]], seact[[1]], mact[[2]], seact[[2]], mact[[3]], seact[[3]], mact[[4]], seact[[4]], model)
        
        activity.dawn <- subset(daycut, SUN == 'W' & PEN == '8') %>% 
          select(Period, BLSEC)
        activity.day <- subset(daycut, SUN == 'D' & PEN == '8') %>% 
          select(Period, BLSEC) 
        activity.dusk <- subset(daycut, SUN == 'K' & PEN == '8') %>% 
          select(Period, BLSEC)
        activity.night <- subset(daycut, SUN == 'N' & PEN == '8') %>% 
          select(Period, BLSEC)
        #mact <- c(mean(activity.dawn$BLSEC, na.rm = T), mean(activity.day$BLSEC, na.rm = T), mean(activity.dusk$BLSEC, na.rm = T), mean(activity.night$BLSEC, na.rm = T))
        
        activity.dawn <- group_by(activity.dawn, Period) %>% 
          summarise(BLSEC = mean(BLSEC, na.rm = T))
        activity.day <- group_by(activity.day, Period) %>% 
          summarise(BLSEC = mean(BLSEC, na.rm = T))
        activity.dusk <- group_by(activity.dusk, Period) %>% 
          summarise(BLSEC = mean(BLSEC, na.rm = T))
        activity.night <- group_by(activity.night, Period) %>% 
          summarise(BLSEC = mean(BLSEC, na.rm = T))
        mact <- c(mean(activity.dawn$BLSEC, na.rm = T), mean(activity.day$BLSEC, na.rm = T), mean(activity.dusk$BLSEC, na.rm = T), mean(activity.night$BLSEC, na.rm = T))
        seact <-  c(sd(activity.dawn$BLSEC, na.rm = T)/sqrt(length(activity.dawn$BLSEC)), sd(activity.day$BLSEC, na.rm = T)/sqrt(length(activity.day$BLSEC)), sd(activity.dusk$BLSEC, na.rm = T)/sqrt(length(activity.dusk$BLSEC)), sd(activity.night$BLSEC, na.rm = T)/sqrt(length(activity.night$BLSEC)))
        #seact <-  c(sd(activity.dawn$BLSEC, na.rm = T), sd(activity.day$BLSEC, na.rm = T), sd(activity.dusk$BLSEC, na.rm = T), sd(activity.night$BLSEC, na.rm = T)) # sd
        
        # calculate day vs night anova
        if(nrow(activity.day) > 2 & nrow(activity.night) >2){
          activity.day$Period <- 'day'
          activity.night$Period <- 'night'
          aovdf <- rbind(activity.day, activity.night)
          model <- aov(BLSEC~Period, aovdf)
          model <- summary(model)[[1]][["Pr(>F)"]][[1]]
        } else {
          model <- NA
        } 
        
        #activity.P8[,as.character(d)] <- c(mean(activity.dawn$BLSEC), sd(activity.dawn$BLSEC)/sqrt(length(activity.dawn$BLSEC)), mean(activity.day$BLSEC), sd(activity.day$BLSEC)/sqrt(length(activity.day$BLSEC)), mean(activity.dusk$BLSEC), sd(activity.dusk$BLSEC)/sqrt(length(activity.dusk$BLSEC)), mean(activity.night$BLSEC), sd(activity.night$BLSEC)/sqrt(length(activity.night$BLSEC)))
        activity.P8[,as.character(d)] <- c(mact[[1]], seact[[1]], mact[[2]], seact[[2]], mact[[3]], seact[[3]], mact[[4]], seact[[4]], model)
        
      }
      
      activity.sum <- rbind(activity.P7, activity.P8)  
      activity.sum    
      write.csv(activity.sum, 'ActivityTotOutput-days.csv')
      
      
    } else { # else type == 'fish'
      
      fish <- sort(unique(dayfile$Period))
      
      for(f in 1:length(fish)){
        
        fishcut <- dayfile[dayfile$Period == fish[f],]
        
        activity.dawn <- subset(fishcut, SUN == 'W' & PEN == '7') %>% 
          select(EchoTime, BLSEC)
        activity.day <- subset(fishcut, SUN == 'D' & PEN == '7') %>% 
          select(EchoTime, BLSEC) 
        activity.dusk <- subset(fishcut, SUN == 'K' & PEN == '7') %>% 
          select(EchoTime, BLSEC)
        activity.night <- subset(fishcut, SUN == 'N' & PEN == '7') %>% 
          select(EchoTime, BLSEC)
        #mact <- c(mean(activity.dawn$BLSEC, na.rm = T), mean(activity.day$BLSEC, na.rm = T), mean(activity.dusk$BLSEC, na.rm = T), mean(activity.night$BLSEC, na.rm = T))
        
        activity.dawn <- group_by(activity.dawn, as.Date(EchoTime)) %>% 
          summarise(BLSEC = mean(BLSEC, na.rm = T))
        activity.day <- group_by(activity.day, as.Date(EchoTime)) %>% 
          summarise(BLSEC = mean(BLSEC, na.rm = T))
        activity.dusk <- group_by(activity.dusk, as.Date(EchoTime)) %>% 
          summarise(BLSEC = mean(BLSEC, na.rm = T))
        activity.night <- group_by(activity.night, as.Date(EchoTime)) %>% 
          summarise(BLSEC = mean(BLSEC, na.rm = T))
        mact <- c(mean(activity.dawn$BLSEC, na.rm = T), mean(activity.day$BLSEC, na.rm = T), mean(activity.dusk$BLSEC, na.rm = T), mean(activity.night$BLSEC, na.rm = T))
        seact <-  c(sd(activity.dawn$BLSEC, na.rm = T)/sqrt(length(activity.dawn$BLSEC)), sd(activity.day$BLSEC, na.rm = T)/sqrt(length(activity.day$BLSEC)), sd(activity.dusk$BLSEC, na.rm = T)/sqrt(length(activity.dusk$BLSEC)), sd(activity.night$BLSEC, na.rm = T)/sqrt(length(activity.night$BLSEC)))
        #seact <-  c(sd(activity.dawn$BLSEC, na.rm = T), sd(activity.day$BLSEC, na.rm = T), sd(activity.dusk$BLSEC, na.rm = T), sd(activity.night$BLSEC, na.rm = T)) # sd
        
        # calculate day vs night anova
        if(nrow(activity.day) > 2 & nrow(activity.night) >2){
          activity.day$Period <- 'day'
          activity.night$Period <- 'night'
          aovdf <- rbind(activity.day, activity.night)
          model <- aov(BLSEC~Period, aovdf)
          model <- summary(model)[[1]][["Pr(>F)"]][[1]]
        } else {
          model <- NA
        } 
        
        #activity.P7[,as.character(f)] <- c(mean(activity.dawn$BLSEC), sd(activity.dawn$BLSEC)/sqrt(length(activity.dawn$BLSEC)), mean(activity.day$BLSEC), sd(activity.day$BLSEC)/sqrt(length(activity.day$BLSEC)), mean(activity.dusk$BLSEC), sd(activity.dusk$BLSEC)/sqrt(length(activity.dusk$BLSEC)), mean(activity.night$BLSEC), sd(activity.night$BLSEC)/sqrt(length(activity.night$BLSEC)))
        activity.P7[,as.character(f)] <- c(mact[[1]], seact[[1]], mact[[2]], seact[[2]], mact[[3]], seact[[3]], mact[[4]], seact[[4]], model)
        
        activity.dawn <- subset(fishcut, SUN == 'W' & PEN == '8') %>% 
          select(EchoTime, BLSEC)
        activity.day <- subset(fishcut, SUN == 'D' & PEN == '8') %>% 
          select(EchoTime, BLSEC) 
        activity.dusk <- subset(fishcut, SUN == 'K' & PEN == '8') %>% 
          select(EchoTime, BLSEC)
        activity.night <- subset(fishcut, SUN == 'N' & PEN == '8') %>% 
          select(EchoTime, BLSEC)
        #mact <- c(mean(activity.dawn$BLSEC, na.rm = T), mean(activity.day$BLSEC, na.rm = T), mean(activity.dusk$BLSEC, na.rm = T), mean(activity.night$BLSEC, na.rm = T))
        
        activity.dawn <- group_by(activity.dawn, as.Date(EchoTime)) %>% 
          summarise(BLSEC = mean(BLSEC, na.rm = T))
        activity.day <- group_by(activity.day, as.Date(EchoTime)) %>% 
          summarise(BLSEC = mean(BLSEC, na.rm = T))
        activity.dusk <- group_by(activity.dusk, as.Date(EchoTime)) %>% 
          summarise(BLSEC = mean(BLSEC, na.rm = T))
        activity.night <- group_by(activity.night, as.Date(EchoTime)) %>% 
          summarise(BLSEC = mean(BLSEC, na.rm = T))
        mact <- c(mean(activity.dawn$BLSEC, na.rm = T), mean(activity.day$BLSEC, na.rm = T), mean(activity.dusk$BLSEC, na.rm = T), mean(activity.night$BLSEC, na.rm = T))
        seact <-  c(sd(activity.dawn$BLSEC, na.rm = T)/sqrt(length(activity.dawn$BLSEC)), sd(activity.day$BLSEC, na.rm = T)/sqrt(length(activity.day$BLSEC)), sd(activity.dusk$BLSEC, na.rm = T)/sqrt(length(activity.dusk$BLSEC)), sd(activity.night$BLSEC, na.rm = T)/sqrt(length(activity.night$BLSEC)))
        #seact <-  c(sd(activity.dawn$BLSEC, na.rm = T), sd(activity.day$BLSEC, na.rm = T), sd(activity.dusk$BLSEC, na.rm = T), sd(activity.night$BLSEC, na.rm = T)) # sd
        
        # calculate day vs night anova
        if(nrow(activity.day) > 2 & nrow(activity.night) >2){
          activity.day$Period <- 'day'
          activity.night$Period <- 'night'
          aovdf <- rbind(activity.day, activity.night)
          model <- aov(BLSEC~Period, aovdf)
          model <- summary(model)[[1]][["Pr(>F)"]][[1]]
        } else {
          model <- NA
        } 
        
        #activity.P8[,as.character(f)] <- c(mean(activity.dawn$BLSEC), sd(activity.dawn$BLSEC)/sqrt(length(activity.dawn$BLSEC)), mean(activity.day$BLSEC), sd(activity.day$BLSEC)/sqrt(length(activity.day$BLSEC)), mean(activity.dusk$BLSEC), sd(activity.dusk$BLSEC)/sqrt(length(activity.dusk$BLSEC)), mean(activity.night$BLSEC), sd(activity.night$BLSEC)/sqrt(length(activity.night$BLSEC)))
        activity.P8[,as.character(f)] <- c(mact[[1]], seact[[1]], mact[[2]], seact[[2]], mact[[3]], seact[[3]], mact[[4]], seact[[4]], model)
        
      }
      
      activity.sum <- rbind(activity.P7, activity.P8)  
      activity.sum    
      write.csv(activity.sum, 'ActivityTotOutput-fish.csv')
      
      
    }
  }  
  
  #activity.sum <- rbind(activity.P7, activity.P8)  
  #activity.sum    
  #write.csv(activity.sum, 'ActivityTotOutput.csv')
}


# 9a. proportion coverage

prop.coverage <- function(xmin7 = 15, xmax7 = 40, ymin7 = 15, ymax7 = 40, xmin8 = 42, xmax8 = 67, ymin8 = 15, ymax8 = 40, boxsize = 0.3) {
  fish.id <- subset(dayfile, PEN == '7')
  x.grid <- floor((fish.id$PosX - xmin7) / boxsize) + 1
  y.grid <- floor((fish.id$PosY - ymin7) / boxsize) + 1
  x.grid.max <- floor((xmax7 - xmin7) / boxsize) + 1
  y.grid.max <- floor((ymax7 - ymin7) / boxsize) + 1
  t.x <- sort(unique(x.grid))
  t.y <- sort(unique(y.grid))
  tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
  ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
  t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
  grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
  t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
  t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
  eg <- expand.grid(t.y,t.x)
  grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
  coverage.P7 <- matrix(c(length(which(grid.cov > 0)), length(grid.cov), length(which(grid.cov > 0))/length(grid.cov)), ncol = 3)
  colnames(coverage.P7) <- c('occupied', 'total', 'proportion')
  
  fish.id <- subset(dayfile, PEN == '8')
  x.grid <- floor((fish.id$PosX - xmin8) / boxsize) + 1
  y.grid <- floor((fish.id$PosY - ymin8) / boxsize) + 1
  x.grid.max <- floor((xmax8 - xmin8) / boxsize) + 1
  y.grid.max <- floor((ymax8 - ymin8) / boxsize) + 1
  t.x <- sort(unique(x.grid))
  t.y <- sort(unique(y.grid))
  tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
  ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
  t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
  grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
  t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
  t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
  eg <- expand.grid(t.y,t.x)
  grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
  coverage.P8 <- matrix(c(length(which(grid.cov > 0)), length(grid.cov), length(which(grid.cov > 0))/length(grid.cov)), ncol = 3)
  colnames(coverage.P8) <- c('occupied', 'total', 'proportion')
  
  coverage <- rbind(coverage.P7, coverage.P8) 
  rownames(coverage) <- c('P7', 'P8')
  coverage
}


# 9b. mean proportion coverage per hour

hmean.prop.coverage <- function(xmin7 = 15, xmax7 = 39, ymin7 = 15, ymax7 = 39, xmin8 = 41, xmax8 = 65, ymin8 = 15, ymax8 = 39, boxsize = 0.3) {
  
  fish.id <- subset(dayfile, PEN == '7')
  
  fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
  starttime <- fish.id[1,'EchoTime']-seconds(1)
  nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
  fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
  
  occupied <- numeric()
  total <- numeric()
  proportion <- numeric()
  
  for (i in 1:nhours){
    
    hoursub <- fish.id[fish.id$EchoTime > starttime & fish.id$EchoTime < starttime+hours(1),]   
    
    x.grid <- floor((hoursub$PosX - xmin7) / boxsize) + 1
    y.grid <- floor((hoursub$PosY - ymin7) / boxsize) + 1
    x.grid.max <- floor((xmax7 - xmin7) / boxsize) + 1
    y.grid.max <- floor((ymax7 - ymin7) / boxsize) + 1
    t.x <- sort(unique(x.grid))
    t.y <- sort(unique(y.grid))
    tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
    ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
    t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
    grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
    t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
    t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
    eg <- expand.grid(t.y,t.x)
    grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
    occupied <- c(occupied, length(which(grid.cov > 0)))
    total <- c(total, length(grid.cov))
    proportion <- c(proportion, length(which(grid.cov > 0))/length(grid.cov))
    
    starttime <- starttime+hours(1)
    
  }
  
  coverage.P7 <- matrix(c(mean(occupied), mean(total), mean(proportion)), ncol = 3)
  colnames(coverage.P7) <- c('occupied', 'total', 'proportion')
  
  
  fish.id <- subset(dayfile, PEN == '8')
  
  fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
  starttime <- fish.id[1,'EchoTime']-seconds(1)
  nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
  fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
  
  occupied <- numeric()
  total <- numeric()
  proportion <- numeric()
  
  for (i in 1:nhours){
    
    hoursub <- fish.id[fish.id$EchoTime >starttime & fish.id$EchoTime <starttime+hours(1),]   
    
    x.grid <- floor((hoursub$PosX - xmin8) / boxsize) + 1
    y.grid <- floor((hoursub$PosY - ymin8) / boxsize) + 1
    x.grid.max <- floor((xmax8 - xmin8) / boxsize) + 1
    y.grid.max <- floor((ymax8 - ymin8) / boxsize) + 1
    t.x <- sort(unique(x.grid))
    t.y <- sort(unique(y.grid))
    tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
    ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
    t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
    grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
    t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
    t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
    eg <- expand.grid(t.y,t.x)
    grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
    
    occupied <- c(occupied, length(which(grid.cov > 0)))
    total <- c(total, length(grid.cov))
    proportion <- c(proportion, length(which(grid.cov > 0))/length(grid.cov))
    
    starttime <- starttime+hours(1)
    
  }
  
  coverage.P8 <- matrix(c(mean(occupied), mean(total), mean(proportion)), ncol = 3)
  colnames(coverage.P8) <- c('occupied', 'total', 'proportion')
  
  coverage <- rbind(coverage.P7, coverage.P8) 
  rownames(coverage) <- c('P7', 'P8')
  coverage
}



# 10a. batch proportion coverage

batch.coverage <- function(xmin7 = 15, xmax7 = 39, ymin7 = 15, ymax7 = 39, xmin8 = 41, xmax8 = 65, ymin8 = 15, ymax8 = 39, boxsize = 0.3) {
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  coverage.P7 <- data.frame(c('P7'))
  colnames(coverage.P7) <- 'ID'
  rownames(coverage.P7) <- c('P7')
  coverage.P8 <- data.frame(c('P8'))
  colnames(coverage.P8) <- 'ID'
  rownames(coverage.P8) <- c('P8')
  
  for (i in 1:length(files))
  {
    dayfile.loc <- files[[i]]
    dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = c
                        (
                        'NULL', 'factor', 'factor', 'factor', 'POSIXct', 'double', 'double', 
                        'double', 'double', 'double', 'double', 'double', 'double', 'factor',
                        'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
                        'double', 'double', 'double', 'double', 'double', 'double', 'double',
                        'double', 'double', 'double', 'double', 'double', 'double', 'double',
                        'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                        'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                        'factor', 'factor', 'double', 'double', 'double', 'double', 'double', 
                        'double', 'double', 'double', 'double', 'double', 'double', 'double' 
                        )) #read data into table
    
    if(length(unique(dayfile$Period)) == 1) {
      
      if(unique(dayfile$PEN) == '7'){
        
        fish.id <- subset(dayfile, PEN == '7')
        x.grid <- floor((fish.id$PosX - xmin7) / boxsize) + 1
        y.grid <- floor((fish.id$PosY - ymin7) / boxsize) + 1
        x.grid.max <- floor((xmax7 - xmin7) / boxsize) + 1
        y.grid.max <- floor((ymax7 - ymin7) / boxsize) + 1
        t.x <- sort(unique(x.grid))
        t.y <- sort(unique(y.grid))
        tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
        ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
        t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
        grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
        t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
        t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
        eg <- expand.grid(t.y,t.x)
        grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
        coverage.P7[,as.character(i)] <-  length(which(grid.cov > 0))/length(grid.cov)
        coverage.P8[,as.character(i)] <- 'NA'
        
      }else{
        
        fish.id <- subset(dayfile, PEN == '8')
        x.grid <- floor((fish.id$PosX - xmin8) / boxsize) + 1
        y.grid <- floor((fish.id$PosY - ymin8) / boxsize) + 1
        x.grid.max <- floor((xmax8 - xmin8) / boxsize) + 1
        y.grid.max <- floor((ymax8 - ymin8) / boxsize) + 1
        t.x <- sort(unique(x.grid))
        t.y <- sort(unique(y.grid))
        tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
        ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
        t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
        grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
        t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
        t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
        eg <- expand.grid(t.y,t.x)
        grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
        coverage.P8[,as.character(i)] <- length(which(grid.cov > 0))/length(grid.cov)
        coverage.P7[,as.character(i)] <- 'NA'
      }
    }
    
    else {
      
      fish.id <- subset(dayfile, PEN == '7')
      x.grid <- floor((fish.id$PosX - xmin7) / boxsize) + 1
      y.grid <- floor((fish.id$PosY - ymin7) / boxsize) + 1
      x.grid.max <- floor((xmax7 - xmin7) / boxsize) + 1
      y.grid.max <- floor((ymax7 - ymin7) / boxsize) + 1
      t.x <- sort(unique(x.grid))
      t.y <- sort(unique(y.grid))
      tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
      ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
      t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
      grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
      t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
      t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
      eg <- expand.grid(t.y,t.x)
      grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
      coverage.P7[,as.character(i)] <-  length(which(grid.cov > 0))/length(grid.cov)
      
      fish.id <- subset(dayfile, PEN == '8')
      x.grid <- floor((fish.id$PosX - xmin8) / boxsize) + 1
      y.grid <- floor((fish.id$PosY - ymin8) / boxsize) + 1
      x.grid.max <- floor((xmax8 - xmin8) / boxsize) + 1
      y.grid.max <- floor((ymax8 - ymin8) / boxsize) + 1
      t.x <- sort(unique(x.grid))
      t.y <- sort(unique(y.grid))
      tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
      ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
      t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
      grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
      t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
      t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
      eg <- expand.grid(t.y,t.x)
      grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
      coverage.P8[,as.character(i)] <- length(which(grid.cov > 0))/length(grid.cov)
      
    }  
    
  }  
  
  coverage <- rbind(coverage.P7, coverage.P8)
  print(coverage)
  #loadWorkbook('CoverageOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('CoverageOutput.xlsx', coverage, 'Sheet 1')
  
  write.xlsx(coverage, 'CoverageOutput.xlsx')
}



# 10b. batch mean proportion coverage per hour

hmean.batch.coverage <- function(xmin7 = 15, xmax7 = 39, ymin7 = 15, ymax7 = 39, xmin8 = 41, xmax8 = 65, ymin8 = 15, ymax8 = 39, boxsize = 0.3) {
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  coverage.P7 <- data.frame(c('P7_mean_coverage', 'P7_sd'))
  colnames(coverage.P7) <- 'ID'
  rownames(coverage.P7) <- c('P7_mean_coverage', 'P7_sd')
  coverage.P8 <- data.frame(c('P8_mean_coverage', 'P8_sd'))
  colnames(coverage.P8) <- 'ID'
  rownames(coverage.P8) <- c('P8_mean_coverage', 'P8_sd')
  
  anova.list <- data.frame('P value')
  colnames(anova.list) <- 'ID'
  rownames(anova.list) <- 'P value'
  
  for (i in 1:length(files))
  {
    dayfile.loc <- files[[i]]
    dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = c
                        (
                        'NULL', 'factor', 'factor', 'factor', 'POSIXct', 'double', 'double', 
                        'double', 'double', 'double', 'double', 'double', 'double', 'factor',
                        'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
                        'double', 'double', 'double', 'double', 'double', 'double', 'double',
                        'double', 'double', 'double', 'double', 'double', 'double', 'double',
                        'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                        'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                        'factor', 'factor', 'double', 'double', 'double', 'double', 'double', 
                        'double', 'double', 'double', 'double', 'double', 'double', 'double' 
                        )) #read data into table
    
    if(length(unique(dayfile$Period)) == 1) {
      
      if(unique(dayfile$PEN) == '7'){
        
        fish.id <- subset(dayfile, PEN == '7')
        
        
        fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
        starttime <- fish.id[1,'EchoTime']-seconds(1)
        nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
        fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
        
        proportion.P7 <- numeric()
        
        for (j in 1:nhours){
          
          hoursub <- fish.id[fish.id$EchoTime > starttime & fish.id$EchoTime < starttime+hours(1),]  
          
          if (nrow(hoursub) > 1){
          
          
          x.grid <- floor((hoursub$PosX - xmin7) / boxsize) + 1
          y.grid <- floor((hoursub$PosY - ymin7) / boxsize) + 1
          x.grid.max <- floor((xmax7 - xmin7) / boxsize) + 1
          y.grid.max <- floor((ymax7 - ymin7) / boxsize) + 1
          t.x <- sort(unique(x.grid))
          t.y <- sort(unique(y.grid))
          tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
          ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
          t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
          grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
          t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
          t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
          eg <- expand.grid(t.y,t.x)
          grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
          
          proportion.P7 <- c(proportion.P7, length(which(grid.cov > 0))/length(grid.cov))
          
          } else {
            
          proportion.P7 <- c(proportion.P7, 0)  
            
          }
          
          starttime <- starttime+hours(1)
          
        }
        
        proportion.P7[proportion.P7 == 0] <- NA
        #coverage.P7[,as.character(i)] <-  mean(proportion, na.rm = T)
        coverage.P7[,as.character(i)] <-  c(mean(proportion.P7, na.rm = T), sd(proportion.P7, na.rm = T))
        coverage.P8[,as.character(i)] <- c('NA', 'NA')
        
      }else{
        
        fish.id <- subset(dayfile, PEN == '8')
        
        fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
        starttime <- fish.id[1,'EchoTime']-seconds(1)
        nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
        fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
        
        proportion.P8 <- numeric()
        
        for (j in 1:nhours){
          
          hoursub <- fish.id[fish.id$EchoTime >starttime & fish.id$EchoTime <starttime+hours(1),]   
          
          if (nrow(hoursub) > 1){
          
          x.grid <- floor((hoursub$PosX - xmin8) / boxsize) + 1
          y.grid <- floor((hoursub$PosY - ymin8) / boxsize) + 1
          x.grid.max <- floor((xmax8 - xmin8) / boxsize) + 1
          y.grid.max <- floor((ymax8 - ymin8) / boxsize) + 1
          t.x <- sort(unique(x.grid))
          t.y <- sort(unique(y.grid))
          tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
          ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
          t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
          grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
          t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
          t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
          eg <- expand.grid(t.y,t.x)
          grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
          
          proportion.P8 <- c(proportion.P8, length(which(grid.cov > 0))/length(grid.cov))
          
          } else {
            
            proportion.P8 <- c(proportion.P8, 0)  
            
          }
          
          
          starttime <- starttime+hours(1)
          
        }
        
        proportion.P8[proportion.P8 == 0] <- NA
        #coverage.P8[,as.character(i)] <-  mean(proportion, na.rm = T)
        coverage.P8[,as.character(i)] <- c(mean(proportion.P8, na.rm = T), sd(proportion.P8, na.rm = T))
        coverage.P7[,as.character(i)] <- c('NA', 'NA')
        
      }
    }
    
    else {
      
      fish.id <- subset(dayfile, PEN == '7')

      
      fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
      starttime <- fish.id[1,'EchoTime']-seconds(1)
      nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
      fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
      
      proportion.P7 <- numeric()
      
      for (j in 1:nhours){
        
        hoursub <- fish.id[fish.id$EchoTime > starttime & fish.id$EchoTime < starttime+hours(1),]   
        
        if (nrow(hoursub) > 1){
        
        x.grid <- floor((hoursub$PosX - xmin7) / boxsize) + 1
        y.grid <- floor((hoursub$PosY - ymin7) / boxsize) + 1
        x.grid.max <- floor((xmax7 - xmin7) / boxsize) + 1
        y.grid.max <- floor((ymax7 - ymin7) / boxsize) + 1
        t.x <- sort(unique(x.grid))
        t.y <- sort(unique(y.grid))
        tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
        ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
        t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
        grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
        t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
        t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
        eg <- expand.grid(t.y,t.x)
        grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
        
        proportion.P7 <- c(proportion.P7, length(which(grid.cov > 0))/length(grid.cov))
        
        } else {
          
          proportion.P7 <- c(proportion.P7, 0)  
          
        }
        
        starttime <- starttime+hours(1)
        
      }
      
      proportion.P7[proportion.P7 == 0] <- NA
      #coverage.P7[,as.character(i)] <-  mean(proportion, na.rm = T)
      coverage.P7[,as.character(i)] <-  c(mean(proportion.P7, na.rm = T), sd(proportion.P7, na.rm = T))
      
      
      proportion.P7 <- as.data.frame(proportion.P7)
      proportion.P7$pen <- 7
      names(proportion.P7) <- c('proportion', 'pen')
      
      
      fish.id <- subset(dayfile, PEN == '8')
      
      fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
      starttime <- fish.id[1,'EchoTime']-seconds(1)
      nhours <- length(unique(hour(fish.id[,'EchoTime'])))-1
      fish.id <- fish.id[order(fish.id$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
      
      proportion.P8 <- numeric()

      
      for (j in 1:nhours){
        
        hoursub <- fish.id[fish.id$EchoTime >starttime & fish.id$EchoTime <starttime+hours(1),]   
        
        if (nrow(hoursub) > 1){
        
        x.grid <- floor((hoursub$PosX - xmin8) / boxsize) + 1
        y.grid <- floor((hoursub$PosY - ymin8) / boxsize) + 1
        x.grid.max <- floor((xmax8 - xmin8) / boxsize) + 1
        y.grid.max <- floor((ymax8 - ymin8) / boxsize) + 1
        t.x <- sort(unique(x.grid))
        t.y <- sort(unique(y.grid))
        tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
        ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
        t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
        grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
        t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
        t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
        eg <- expand.grid(t.y,t.x)
        grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
        
        proportion.P8 <- c(proportion.P8, length(which(grid.cov > 0))/length(grid.cov))
        
        } else {
          
          proportion.P8 <- c(proportion.P8, 0)  
          
        }
        
        starttime <- starttime+hours(1)
        
      }
      
      proportion.P8[proportion.P8 == 0] <- NA
      #coverage.P8[,as.character(i)] <- mean(proportion, na.rm = T)
      coverage.P8[,as.character(i)] <- c(mean(proportion.P8, na.rm = T), sd(proportion.P8, na.rm = T))
      
      proportion.P8 <- as.data.frame(proportion.P8)
      proportion.P8$pen <- 8
      names(proportion.P8) <- c('proportion', 'pen')
      
      prop.perhr <- rbind(proportion.P7, proportion.P8)
      cov.anova <- aov(proportion~pen, data = prop.perhr)
      anova.sum <- unlist(summary(cov.anova))
      anova.list[,as.character(i)] <- anova.sum[9]
      
      
    }  
    
  }  
  
  coverage <- rbind(coverage.P7, coverage.P8, anova.list)
  print(coverage)
  
  write.xlsx(coverage, 'CoverageOutput_hmean.xlsx')
}


# 10c. hmean.perfish.coverage - daily hourly coverage per fish for all days loaded as one file using load.all()

hmean.perfish.coverage <- function(xmin7 = 15, xmax7 = 39, ymin7 = 15, ymax7 = 39, xmin8 = 41, xmax8 = 65, ymin8 = 15, ymax8 = 39, boxsize = 0.3) {
  
  #dayfile <- read.csv(files[1], header = TRUE, sep = ",", colClasses = dayfile.classes)
  fish7 <- sort(unique(dayfile$Period[dayfile$PEN == '7']))
  fish8 <- sort(unique(dayfile$Period[dayfile$PEN == '8']))
  
  days <- c(paste0(sort(unique(as.Date(dayfile$EchoTime))), ' 00:00:00'), paste0(max(unique(as.Date(dayfile$EchoTime)))+days(1), ' 00:00:00'))
  
  coverage.P7 <- data.frame(fish = fish7, pen = rep(7, length(fish7)))
  coverage.P8 <- data.frame(fish = fish8, pen = rep(8, length(fish8)))
  
  pencut <- subset(dayfile, PEN == '7')
  
  for(d in 1:length(days)-1){
    
    daycut <- pencut[pencut$EchoTime > days[d] & pencut$EchoTime < days[d+1],]
    daymean <- numeric()
    daysd <- numeric()
    
    for (f in 1:length(fish7)){
      
      fishcut <- daycut[daycut$Period == fish7[f],]  
      
      fishcut <- fishcut[order(fishcut$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
      starttime <- fishcut[1,'EchoTime']-(hours(1) + seconds(1))
      nhours <- length(unique(hour(fishcut[,'EchoTime'])))-1
      #fishcut <- fishcut[order(fishcut$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
      
      occupied <- numeric()
      total <- numeric()
      proportion <- numeric()
      
      for (i in 1:nhours){
        
        hoursub <- fishcut[fishcut$EchoTime > starttime & fishcut$EchoTime < starttime+hours(1),]   
        
        if(nrow(hoursub) > 1 & mean(hoursub$PosX) > xmin7 & mean(hoursub$PosX) < xmax7 & mean(hoursub$PosY) > ymin7 & mean(hoursub$PosY) < ymax7){
          
          x.grid <- floor((hoursub$PosX - xmin7) / boxsize) + 1
          y.grid <- floor((hoursub$PosY - ymin7) / boxsize) + 1
          x.grid.max <- floor((xmax7 - xmin7) / boxsize) + 1
          y.grid.max <- floor((ymax7 - ymin7) / boxsize) + 1
          t.x <- sort(unique(x.grid))
          t.y <- sort(unique(y.grid))
          tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
          ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
          t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
          grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
          t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
          t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
          eg <- expand.grid(t.y,t.x)
          grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
          occupied <- c(occupied, length(which(grid.cov > 0)))
          total <- c(total, length(grid.cov))
          proportion <- c(proportion, length(which(grid.cov > 0))/length(grid.cov))
          
        } else {proportion <- c(proportion, 0) }
        
        starttime <- starttime+hours(1)
        
        
      } # end of hour cut loop
      
      daymean <- c(daymean, mean(proportion))
      daysd <- c(daysd, sd(proportion))
      
    } # end of fishcut loop
    
    coverage.P7[,paste0(as.character(d), '_mean')] <- daymean
    coverage.P7[,paste0(as.character(d), '_sd')] <- daysd
    
  } # end of daycut loop
  
  
  pencut <- subset(dayfile, PEN == '8')
  
  for(d in 1:length(days)-1){
    
    daycut <- pencut[pencut$EchoTime > days[d] & pencut$EchoTime < days[d+1],]
    daymean <- numeric()
    daysd <- numeric()
    
    for (f in 1:length(fish8)){
      
      fishcut <- daycut[daycut$Period == fish8[f],]  
      
      fishcut <- fishcut[order(fishcut$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
      starttime <- fishcut[1,'EchoTime']-(hours(1) + seconds(1))
      nhours <- length(unique(hour(fishcut[,'EchoTime'])))-1
      #fishcut <- fishcut[order(fishcut$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
      
      occupied <- numeric()
      total <- numeric()
      proportion <- numeric()
      
      for (i in 1:nhours){
        
        hoursub <- fishcut[fishcut$EchoTime > starttime & fishcut$EchoTime < starttime+hours(1),]   
        
        if(nrow(hoursub) > 1 & mean(hoursub$PosX) > xmin8 & mean(hoursub$PosX) < xmax8 & mean(hoursub$PosY) > ymin8 & mean(hoursub$PosY) < ymax8){
          
          x.grid <- floor((hoursub$PosX - xmin8) / boxsize) + 1
          y.grid <- floor((hoursub$PosY - ymin8) / boxsize) + 1
          x.grid.max <- floor((xmax8 - xmin8) / boxsize) + 1
          y.grid.max <- floor((ymax8 - ymin8) / boxsize) + 1
          t.x <- sort(unique(x.grid))
          t.y <- sort(unique(y.grid))
          tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
          ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
          t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
          grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
          t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
          t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
          eg <- expand.grid(t.y,t.x)
          grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
          occupied <- c(occupied, length(which(grid.cov > 0)))
          total <- c(total, length(grid.cov))
          proportion <- c(proportion, length(which(grid.cov > 0))/length(grid.cov))
          
        } else {proportion <- c(proportion, 0) }
        
        starttime <- starttime+hours(1)
        
        
      } # end of hour cut loop
      
      daymean <- c(daymean, mean(proportion))
      daysd <- c(daysd, sd(proportion))
      
    } # end of fishcut loop
    
    coverage.P8[,paste0(as.character(d), '_mean')] <- daymean
    coverage.P8[,paste0(as.character(d), '_sd')] <- daysd
    
  } # end of daycut loop
  
  coverage <- rbind(coverage.P7, coverage.P8) 
  coverage[,'0_mean'] <- NULL
  coverage[,'0_sd'] <- NULL
  write.csv(coverage, 'CoverageOutput_hmeanperfish.csv')
  coverage <<- coverage
}



# 11a. draws a plot of fish depth for the fish id specified

fish.depth <- function(period)
{
  fish.id <- subset(dayfile, Period == period)
  plot(fish.id$EchoTime, fish.id$PosZ, xlab = 'Time', ylab = 'Depth (m)', ylim = c(35, 0), type = 'l', col = '#26b426')
  segments(fish.id[1,4], 15, fish.id[nrow(fish.id), 4], 15, lty = 2)
  legend('bottomleft', as.character(period), col = '#26b426', pch = 20, bty = 'n', pt.cex = 1.5, horiz = TRUE, y.intersp = 0)
  
}



# 11b. draws a plot of fish activity for the fish id specified

fish.act <- function(period)
{
  fish.id <- subset(dayfile, Period == period)
  plot(fish.id$EchoTime, fish.id$BLSEC, xlab = 'Time', ylab = 'Activity (BL/SEC)', ylim = c(0, 5), type = 'l', col = '#26b426')
  legend('bottomleft', as.character(period), col = '#26b426', pch = 20, bty = 'n', pt.cex = 1.5, horiz = TRUE, y.intersp = 0)
  
}

# 12. draws a plot of depths for three fish

fish.3depth <- function(period1, period2, period3)
{
  fish.id <- subset(dayfile, Period == period1)
  plot(fish.id$EchoTime, fish.id$PosZ, xlab = 'Time', ylab = 'Depth (m)', ylim = c(35,0), type = 'l', col = '#26b426')
  
  fish.id <- subset(dayfile, Period == period2)
  lines(fish.id$EchoTime, fish.id$PosZ, col = '#d80000')
  
  fish.id <- subset(dayfile, Period == period3)
  lines(fish.id$EchoTime, fish.id$PosZ, col = '#038ef0')
  segments(fish.id[1,4], 15, fish.id[nrow(fish.id), 4], 15, lty = 2)
  legend('bottom', as.character(c(period1, period2, period3)), col = c('#26b426', '#d80000', '#038ef0'), pch = 20, bty = 'n', pt.cex = 1.5, horiz = TRUE, y.intersp = 0)
}

# 13. draws a plot of fish location

fish.plot <- function(period)
{
  fishpal <- rainbow_hcl(20, c=100, l=63, start=-360, end=-32, alpha = 0.2)
  fish.id <- subset(dayfile, Period == period)
  par(mfrow=c(1,1))
  
  if(fish.id[1,3] == '7')
  {
    
    # plot(fish.id$PosX, fish.id$PosY, xlab = 'X', ylab = 'Y', pch = 20, cex = 0.8, xlim = c(0, 40), ylim = c(0, 45), type = 'p', col = rgb(0, 0.6, 0, 0.2)) # wider plot
    plot(fish.id$PosX, fish.id$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(10, 45), ylim = c(10, 45), type = 'l', col = fishpal[20]) # tight plot
    rect(locations.lookup['7EW', 'xmin'], locations.lookup['7EW', 'ymin'], locations.lookup['7EW', 'xmax'], locations.lookup['7EW', 'ymax'], lty = 2) # 7EW edge
    rect(locations.lookup['7ES', 'xmin'], locations.lookup['7ES', 'ymin'], locations.lookup['7ES', 'xmax'], locations.lookup['7ES', 'ymax'], lty = 2) # 7ES edge
    rect(locations.lookup['7EE', 'xmin'], locations.lookup['7EE', 'ymin'], locations.lookup['7EE', 'xmax'], locations.lookup['7EE', 'ymax'], lty = 2) # 7EE edge
    rect(locations.lookup['7EN', 'xmin'], locations.lookup['7EN', 'ymin'], locations.lookup['7EN', 'xmax'], locations.lookup['7EN', 'ymax'], lty = 2) # 7EN edge
    rect(locations.lookup['7WHSE', 'xmin'], locations.lookup['7WHSE', 'ymin'], locations.lookup['7WHSE', 'xmax'], locations.lookup['7WHSE', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHSE
    rect(locations.lookup['7WHNW', 'xmin'], locations.lookup['7WHNW', 'ymin'], locations.lookup['7WHNW', 'xmax'], locations.lookup['7WHNW', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHNW
    rect(locations.lookup['7FBSE', 'xmin'], locations.lookup['7FBSE', 'ymin'], locations.lookup['7FBSE', 'xmax'], locations.lookup['7FBSE', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 7FBSE
    rect(locations.lookup['7FBNW', 'xmin'], locations.lookup['7FBNW', 'ymin'], locations.lookup['7FBNW', 'xmax'], locations.lookup['7FBNW', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 7FBNW
    rect(locations.lookup['7EW', 'xmin'], locations.lookup['7ES', 'ymin'], locations.lookup['7EE', 'xmax'], locations.lookup['7EN', 'ymax'], lwd = 2) # cage limits
    #legend(1, 10, as.character(period), col = '#26b426', pch = 20, bty = 'n', pt.cex = 1.5, horiz = TRUE)
    
  }else{
    
    #plot(fish.id$PosX, fish.id$PosY, xlab = 'X', ylab = 'Y', pch = 20, cex = 0.8, xlim = c(25, 70), ylim = c(0, 45), type = 'p', col = rgb(0, 0.6, 0, 0.2)) # wider plot
    plot(fish.id$PosX, fish.id$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(37, 72), ylim = c(10, 45), type = 'l', col = fishpal[20]) # tight plot
    rect(locations.lookup['8EW', 'xmin'], locations.lookup['8EW', 'ymin'], locations.lookup['8EW', 'xmax'], locations.lookup['8EW', 'ymax'], lty = 2) # 7EW edge
    rect(locations.lookup['8ES', 'xmin'], locations.lookup['8ES', 'ymin'], locations.lookup['8ES', 'xmax'], locations.lookup['8ES', 'ymax'], lty = 2) # 7ES edge
    rect(locations.lookup['8EE', 'xmin'], locations.lookup['8EE', 'ymin'], locations.lookup['8EE', 'xmax'], locations.lookup['8EE', 'ymax'], lty = 2) # 7EE edge
    rect(locations.lookup['8EN', 'xmin'], locations.lookup['8EN', 'ymin'], locations.lookup['8EN', 'xmax'], locations.lookup['8EN', 'ymax'], lty = 2) # 7EN edge
    rect(locations.lookup['8WHSW', 'xmin'], locations.lookup['8WHSW', 'ymin'], locations.lookup['8WHSW', 'xmax'], locations.lookup['8WHSW', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHSE
    rect(locations.lookup['8WHNE', 'xmin'], locations.lookup['8WHNE', 'ymin'], locations.lookup['8WHNE', 'xmax'], locations.lookup['8WHNE', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHNW
    rect(locations.lookup['8FBNE', 'xmin'], locations.lookup['8FBNE', 'ymin'], locations.lookup['8FBNE', 'xmax'], locations.lookup['8FBNE', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 7FBNE
    rect(locations.lookup['8FBSW', 'xmin'], locations.lookup['8FBSW', 'ymin'], locations.lookup['8FBSW', 'xmax'], locations.lookup['8FBSW', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 7FBSW
    rect(locations.lookup['8EW', 'xmin'], locations.lookup['8ES', 'ymin'], locations.lookup['8EE', 'xmax'], locations.lookup['8EN', 'ymax'], lwd = 2) # cage limits
    #legend(25, 10, as.character(period), col = '#26b426', pch = 20, bty = 'n', pt.cex = 1.5, horiz = TRUE)
    
  }
}

# 14. Draws a plot of fish locations for 3 fish

fish.3plot <- function(period1, period2, period3)
{
  fish.id <- subset(dayfile, Period == period1)
  if(fish.id[1,3] == '7')
  {
    
    plot(fish.id$PosX, fish.id$PosY, xlab = 'X', ylab = 'Y', pch = 20, xlim = c(10, 45), ylim = c(10, 45), type = 'l', col = '#26b426')
    fish.id <- subset(dayfile, Period == period2)
    lines(fish.id$PosX, fish.id$PosY, pch = 20, col = '#d80000')
    fish.id <- subset(dayfile, Period == period3)
    lines(fish.id$PosX, fish.id$PosY, pch = 20, col = '#038ef0')
    rect(locations.lookup['7EW', 'xmin'], locations.lookup['7EW', 'ymin'], locations.lookup['7EW', 'xmax'], locations.lookup['7EW', 'ymax'], lty = 2) # 7EW edge
    rect(locations.lookup['7ES', 'xmin'], locations.lookup['7ES', 'ymin'], locations.lookup['7ES', 'xmax'], locations.lookup['7ES', 'ymax'], lty = 2) # 7ES edge
    rect(locations.lookup['7EE', 'xmin'], locations.lookup['7EE', 'ymin'], locations.lookup['7EE', 'xmax'], locations.lookup['7EE', 'ymax'], lty = 2) # 7EE edge
    rect(locations.lookup['7EN', 'xmin'], locations.lookup['7EN', 'ymin'], locations.lookup['7EN', 'xmax'], locations.lookup['7EN', 'ymax'], lty = 2) # 7EN edge
    rect(locations.lookup['7WHSE', 'xmin'], locations.lookup['7WHSE', 'ymin'], locations.lookup['7WHSE', 'xmax'], locations.lookup['7WHSE', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHSE
    rect(locations.lookup['7WHNW', 'xmin'], locations.lookup['7WHNW', 'ymin'], locations.lookup['7WHNW', 'xmax'], locations.lookup['7WHNW', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHNW
    rect(locations.lookup['7EW', 'xmin'], locations.lookup['7ES', 'ymin'], locations.lookup['7EE', 'xmax'], locations.lookup['7EN', 'ymax'], lwd = 2) # cage limits
    legend(1, 10, as.character(c(period1, period2, period3)), col = c('#26b426', '#d80000', '#038ef0'), pch = 20, bty = 'n', pt.cex = 1.5, horiz = TRUE)
    
  }else{
    
    plot(fish.id$PosX, fish.id$PosY, xlab = 'X', ylab = 'Y', pch = 20, xlim = c(37, 72), ylim = c(10, 45), type = 'l', col = '#26b426')
    fish.id <- subset(dayfile, Period == period2)
    lines(fish.id$PosX, fish.id$PosY, pch = 20, col = '#d80000')
    fish.id <- subset(dayfile, Period == period3)
    lines(fish.id$PosX, fish.id$PosY, pch = 20, col = '#038ef0')
    rect(locations.lookup['8EW', 'xmin'], locations.lookup['8EW', 'ymin'], locations.lookup['8EW', 'xmax'], locations.lookup['8EW', 'ymax'], lty = 2) # 7EW edge
    rect(locations.lookup['8ES', 'xmin'], locations.lookup['8ES', 'ymin'], locations.lookup['8ES', 'xmax'], locations.lookup['8ES', 'ymax'], lty = 2) # 7ES edge
    rect(locations.lookup['8EE', 'xmin'], locations.lookup['8EE', 'ymin'], locations.lookup['8EE', 'xmax'], locations.lookup['8EE', 'ymax'], lty = 2) # 7EE edge
    rect(locations.lookup['8EN', 'xmin'], locations.lookup['8EN', 'ymin'], locations.lookup['8EN', 'xmax'], locations.lookup['8EN', 'ymax'], lty = 2) # 7EN edge
    rect(locations.lookup['8WHSW', 'xmin'], locations.lookup['8WHSW', 'ymin'], locations.lookup['8WHSW', 'xmax'], locations.lookup['8WHSW', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHSE
    rect(locations.lookup['8WHNE', 'xmin'], locations.lookup['8WHNE', 'ymin'], locations.lookup['8WHNE', 'xmax'], locations.lookup['8WHNE', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHNW
    rect(locations.lookup['8EW', 'xmin'], locations.lookup['8ES', 'ymin'], locations.lookup['8EE', 'xmax'], locations.lookup['8EN', 'ymax'], lwd = 2) # cage limits
    legend(25, 10, as.character(c(period1, period2, period3)), col = c('#26b426', '#d80000', '#038ef0'), pch = 20, bty = 'n', pt.cex = 1.5, horiz = TRUE)
    
  }
}


# 15. Add a fish to the current plot

add.fish <- function(period, fishcol)
{
  fish.id <- subset(dayfile, Period == period)
  points(fish.id$PosX, fish.id$PosY, pch = 20, cex = 1, col = fishcol)
}

#16a. draws a plot of fish location density for the fish id specified 

fish.hexplot <- function(period)
  
{
  
  pen.col <- 'black'
  pen.size <- 1.4
  #plot.col <- rev(heat.colors(2, alpha = 1))
  plot.col <- matlab.like(1000)  
  
  fish.id <- subset(dayfile, Period == period)
  
  #pingmax <- as.integer((as.double(max(dayfile$EchoTime))-as.double(min(dayfile$EchoTime)))/500)
  pingmax <- 100
  
  if(dayfile[1, 'PEN'] == 7){  
    
    ggplot(fish.id, aes(fish.id$PosX, fish.id$PosY)) +
      geom_hex(bins = 55, alpha = 0.6) + scale_fill_gradientn(colours=plot.col, space = 'Lab', limits = c(0, pingmax), na.value = plot.col[length(plot.col)], name = 'No. pings') +
      annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CNW', 'xmin'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CNW', 'ymax'], colour = pen.col, size = pen.size) +  # pen boundary
      annotate('segment', x = locations.lookup['7CNW', 'xmin'], xend = locations.lookup['7CNE', 'xmax'], y = locations.lookup['7CNW', 'ymax'], yend = locations.lookup['7CNE', 'ymax'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['7CNE', 'xmax'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CNE', 'ymax'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CSW', 'ymax'], yend = locations.lookup['7CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['7CSW', 'xmax'], xend = locations.lookup['7CNW', 'xmax'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['7CNW', 'xmin'], xend = locations.lookup['7CNE', 'xmax'], y = locations.lookup['7CNW', 'ymin'], yend = locations.lookup['7CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['7CNE', 'xmin'], xend = locations.lookup['7CSE', 'xmin'], y = locations.lookup['7CNE', 'ymax'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('curve', x = locations.lookup['7WHNW', 'xmin']+1, xend = locations.lookup['7WHNW', 'xmax']-1, y = locations.lookup['7WHNW', 'ymin']+1, yend = locations.lookup['7WHNW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
      annotate('curve', x = locations.lookup['7WHNW', 'xmin']+1, xend = locations.lookup['7WHNW', 'xmax']-1, y = locations.lookup['7WHNW', 'ymin']+1, yend = locations.lookup['7WHNW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
      annotate('curve', x = locations.lookup['7WHSE', 'xmin']+1, xend = locations.lookup['7WHSE', 'xmax']-1, y = locations.lookup['7WHSE', 'ymin']+1, yend = locations.lookup['7WHSE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
      annotate('curve', x = locations.lookup['7WHSE', 'xmin']+1, xend = locations.lookup['7WHSE', 'xmax']-1, y = locations.lookup['7WHSE', 'ymin']+1, yend = locations.lookup['7WHSE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
      theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
      scale_x_continuous('x (m)', limits = c(10, 45)) + scale_y_continuous('y (m)', limits = c(10,45))
    
  } else {
    
    ggplot(fish.id, aes(fish.id$PosX, fish.id$PosY)) +
      geom_hex(bins = 55, alpha = 0.6) + scale_fill_gradientn(colours=plot.col, space = 'Lab', limits = c(0, pingmax), na.value = plot.col[length(plot.col)], name = 'No. pings') +
      annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CNW', 'xmin'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CNW', 'ymax'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['8CNW', 'xmin'], xend = locations.lookup['8CNE', 'xmax'], y = locations.lookup['8CNW', 'ymax'], yend = locations.lookup['8CNE', 'ymax'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['8CNE', 'xmax'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CNE', 'ymax'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CSW', 'ymax'], yend = locations.lookup['8CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('segment', x = locations.lookup['8CSW', 'xmax'], xend = locations.lookup['8CNW', 'xmax'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('segment', x = locations.lookup['8CNW', 'xmin'], xend = locations.lookup['8CNE', 'xmax'], y = locations.lookup['8CNW', 'ymin'], yend = locations.lookup['8CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('segment', x = locations.lookup['8CNE', 'xmin'], xend = locations.lookup['8CSE', 'xmin'], y = locations.lookup['8CNE', 'ymax'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('curve', x = locations.lookup['8WHSW', 'xmin']+1, xend = locations.lookup['8WHSW', 'xmax']-1, y = locations.lookup['8WHSW', 'ymin']+1, yend = locations.lookup['8WHSW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
      annotate('curve', x = locations.lookup['8WHSW', 'xmin']+1, xend = locations.lookup['8WHSW', 'xmax']-1, y = locations.lookup['8WHSW', 'ymin']+1, yend = locations.lookup['8WHSW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
      annotate('curve', x = locations.lookup['8WHNE', 'xmin']+1, xend = locations.lookup['8WHNE', 'xmax']-1, y = locations.lookup['8WHNE', 'ymin']+1, yend = locations.lookup['8WHNE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
      annotate('curve', x = locations.lookup['8WHNE', 'xmin']+1, xend = locations.lookup['8WHNE', 'xmax']-1, y = locations.lookup['8WHNE', 'ymin']+1, yend = locations.lookup['8WHNE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
      theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
      scale_x_continuous('x (m)', limits = c(35,70)) + scale_y_continuous('y (m)', limits = c(10,45))  
    
  }
}




#16b. draws a plot of fish location density for all fish in the specified pen (7 or 8)


hexplot.all <- function(pen)
{
  
  pen.col <- 'black'
  pen.size <- 1.4
  #plot.col <- rev(heat.colors(2, alpha = 1))
  plot.col <- matlab.like(1000)  
  
  if(pen == 7){  
    
    fish.id <- subset(dayfile, PEN == 7)  
    
    hexplot <- ggplot(fish.id, aes(fish.id$PosX, fish.id$PosY))
    hexplot <- hexplot + geom_hex(bins = 55, alpha = 0.6) + scale_fill_gradientn(colours=plot.col, space = 'Lab', limits = c(0, 1000), na.value = plot.col[length(plot.col)], name = 'No. pings')
    hexplot + annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CNW', 'xmin'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CNW', 'ymax'], colour = pen.col, size = pen.size) +  # pen boundary
      annotate('segment', x = locations.lookup['7CNW', 'xmin'], xend = locations.lookup['7CNE', 'xmax'], y = locations.lookup['7CNW', 'ymax'], yend = locations.lookup['7CNE', 'ymax'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['7CNE', 'xmax'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CNE', 'ymax'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CSW', 'ymax'], yend = locations.lookup['7CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['7CSW', 'xmax'], xend = locations.lookup['7CNW', 'xmax'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['7CNW', 'xmin'], xend = locations.lookup['7CNE', 'xmax'], y = locations.lookup['7CNW', 'ymin'], yend = locations.lookup['7CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['7CNE', 'xmin'], xend = locations.lookup['7CSE', 'xmin'], y = locations.lookup['7CNE', 'ymax'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('curve', x = locations.lookup['7WHNW', 'xmin']+1, xend = locations.lookup['7WHNW', 'xmax']-1, y = locations.lookup['7WHNW', 'ymin']+1, yend = locations.lookup['7WHNW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
      annotate('curve', x = locations.lookup['7WHNW', 'xmin']+1, xend = locations.lookup['7WHNW', 'xmax']-1, y = locations.lookup['7WHNW', 'ymin']+1, yend = locations.lookup['7WHNW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
      annotate('curve', x = locations.lookup['7WHSE', 'xmin']+1, xend = locations.lookup['7WHSE', 'xmax']-1, y = locations.lookup['7WHSE', 'ymin']+1, yend = locations.lookup['7WHSE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
      annotate('curve', x = locations.lookup['7WHSE', 'xmin']+1, xend = locations.lookup['7WHSE', 'xmax']-1, y = locations.lookup['7WHSE', 'ymin']+1, yend = locations.lookup['7WHSE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
      theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
      scale_x_continuous('x (m)', limits = c(10, 45)) + scale_y_continuous('y (m)', limits = c(10,45))
    
  } else {
    
    
    fish.id <- subset(dayfile, PEN == 8)  
    
    hexplot <- ggplot(fish.id, aes(fish.id$PosX, fish.id$PosY))
    hexplot <- hexplot + geom_hex(bins = 55, alpha = 0.6) + scale_fill_gradientn(colours=plot.col, space = 'Lab', limits = c(0, 1000), na.value = plot.col[length(plot.col)], name = 'No. pings') 
    hexplot + annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CNW', 'xmin'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CNW', 'ymax'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['8CNW', 'xmin'], xend = locations.lookup['8CNE', 'xmax'], y = locations.lookup['8CNW', 'ymax'], yend = locations.lookup['8CNE', 'ymax'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['8CNE', 'xmax'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CNE', 'ymax'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CSW', 'ymax'], yend = locations.lookup['8CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('segment', x = locations.lookup['8CSW', 'xmax'], xend = locations.lookup['8CNW', 'xmax'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('segment', x = locations.lookup['8CNW', 'xmin'], xend = locations.lookup['8CNE', 'xmax'], y = locations.lookup['8CNW', 'ymin'], yend = locations.lookup['8CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('segment', x = locations.lookup['8CNE', 'xmin'], xend = locations.lookup['8CSE', 'xmin'], y = locations.lookup['8CNE', 'ymax'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('curve', x = locations.lookup['8WHSW', 'xmin']+1, xend = locations.lookup['8WHSW', 'xmax']-1, y = locations.lookup['8WHSW', 'ymin']+1, yend = locations.lookup['8WHSW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
      annotate('curve', x = locations.lookup['8WHSW', 'xmin']+1, xend = locations.lookup['8WHSW', 'xmax']-1, y = locations.lookup['8WHSW', 'ymin']+1, yend = locations.lookup['8WHSW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
      annotate('curve', x = locations.lookup['8WHNE', 'xmin']+1, xend = locations.lookup['8WHNE', 'xmax']-1, y = locations.lookup['8WHNE', 'ymin']+1, yend = locations.lookup['8WHNE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
      annotate('curve', x = locations.lookup['8WHNE', 'xmin']+1, xend = locations.lookup['8WHNE', 'xmax']-1, y = locations.lookup['8WHNE', 'ymin']+1, yend = locations.lookup['8WHNE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
      theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
      scale_x_continuous('x (m)', limits = c(35,70)) + scale_y_continuous('y (m)', limits = c(10,45))
  }  
  
}


#16c. draws plots of fish location density for all fish in pens 7 and 8 and plots side by side

hexplot.compare <- function(pen)
{
  
  pen.col <- 'black'
  pen.size <- 1.4
  #plot.col <- rev(heat.colors(2, alpha = 1))
  plot.col <- matlab.like(1000)  
  
  #if(pen == 7){  
  
  fish.id7 <- subset(dayfile, PEN == 7)  
  
  hexplot7 <- ggplot(fish.id7, aes(fish.id7$PosX, fish.id7$PosY)) +
    geom_hex(bins = 55, alpha = 0.6) + scale_fill_gradientn(colours=plot.col, space = 'Lab', limits = c(0, 1000), na.value = plot.col[length(plot.col)], name = 'No. pings') +
    annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
    annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CNW', 'xmin'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CNW', 'ymax'], colour = pen.col, size = pen.size) +  # pen boundary
    annotate('segment', x = locations.lookup['7CNW', 'xmin'], xend = locations.lookup['7CNE', 'xmax'], y = locations.lookup['7CNW', 'ymax'], yend = locations.lookup['7CNE', 'ymax'], colour = pen.col, size = pen.size) + # pen boundary
    annotate('segment', x = locations.lookup['7CNE', 'xmax'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CNE', 'ymax'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
    annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CSW', 'ymax'], yend = locations.lookup['7CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
    annotate('segment', x = locations.lookup['7CSW', 'xmax'], xend = locations.lookup['7CNW', 'xmax'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
    annotate('segment', x = locations.lookup['7CNW', 'xmin'], xend = locations.lookup['7CNE', 'xmax'], y = locations.lookup['7CNW', 'ymin'], yend = locations.lookup['7CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
    annotate('segment', x = locations.lookup['7CNE', 'xmin'], xend = locations.lookup['7CSE', 'xmin'], y = locations.lookup['7CNE', 'ymax'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
    annotate('curve', x = locations.lookup['7WHNW', 'xmin']+1, xend = locations.lookup['7WHNW', 'xmax']-1, y = locations.lookup['7WHNW', 'ymin']+1, yend = locations.lookup['7WHNW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
    annotate('curve', x = locations.lookup['7WHNW', 'xmin']+1, xend = locations.lookup['7WHNW', 'xmax']-1, y = locations.lookup['7WHNW', 'ymin']+1, yend = locations.lookup['7WHNW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
    annotate('curve', x = locations.lookup['7WHSE', 'xmin']+1, xend = locations.lookup['7WHSE', 'xmax']-1, y = locations.lookup['7WHSE', 'ymin']+1, yend = locations.lookup['7WHSE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
    annotate('curve', x = locations.lookup['7WHSE', 'xmin']+1, xend = locations.lookup['7WHSE', 'xmax']-1, y = locations.lookup['7WHSE', 'ymin']+1, yend = locations.lookup['7WHSE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
    theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
    scale_x_continuous('x (m)', limits = c(10, 45)) + scale_y_continuous('y (m)', limits = c(10,45)) +
    ggtitle(label = 'Acclimated wrasse')
  
  #} else {
  
  
  fish.id8 <- subset(dayfile, PEN == 8)  
  
  hexplot8 <- ggplot(fish.id8, aes(fish.id8$PosX, fish.id8$PosY)) +
    geom_hex(bins = 55, alpha = 0.6) + scale_fill_gradientn(colours=plot.col, space = 'Lab', limits = c(0, 1000), na.value = plot.col[length(plot.col)], name = 'No. pings') +
    annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, size = pen.size) +
    annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CNW', 'xmin'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CNW', 'ymax'], colour = pen.col, size = pen.size) +
    annotate('segment', x = locations.lookup['8CNW', 'xmin'], xend = locations.lookup['8CNE', 'xmax'], y = locations.lookup['8CNW', 'ymax'], yend = locations.lookup['8CNE', 'ymax'], colour = pen.col, size = pen.size) +
    annotate('segment', x = locations.lookup['8CNE', 'xmax'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CNE', 'ymax'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, size = pen.size) +
    annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CSW', 'ymax'], yend = locations.lookup['8CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) +
    annotate('segment', x = locations.lookup['8CSW', 'xmax'], xend = locations.lookup['8CNW', 'xmax'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) +
    annotate('segment', x = locations.lookup['8CNW', 'xmin'], xend = locations.lookup['8CNE', 'xmax'], y = locations.lookup['8CNW', 'ymin'], yend = locations.lookup['8CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) +
    annotate('segment', x = locations.lookup['8CNE', 'xmin'], xend = locations.lookup['8CSE', 'xmin'], y = locations.lookup['8CNE', 'ymax'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) +
    annotate('curve', x = locations.lookup['8WHSW', 'xmin']+1, xend = locations.lookup['8WHSW', 'xmax']-1, y = locations.lookup['8WHSW', 'ymin']+1, yend = locations.lookup['8WHSW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
    annotate('curve', x = locations.lookup['8WHSW', 'xmin']+1, xend = locations.lookup['8WHSW', 'xmax']-1, y = locations.lookup['8WHSW', 'ymin']+1, yend = locations.lookup['8WHSW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
    annotate('curve', x = locations.lookup['8WHNE', 'xmin']+1, xend = locations.lookup['8WHNE', 'xmax']-1, y = locations.lookup['8WHNE', 'ymin']+1, yend = locations.lookup['8WHNE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
    annotate('curve', x = locations.lookup['8WHNE', 'xmin']+1, xend = locations.lookup['8WHNE', 'xmax']-1, y = locations.lookup['8WHNE', 'ymin']+1, yend = locations.lookup['8WHNE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
    theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
    scale_x_continuous('x (m)', limits = c(35,70)) + scale_y_continuous('y (m)', limits = c(10,45)) +
    ggtitle(label = 'Non-acclimated wrasse')
  
  #}  
  
  hexleg <- get_legend(hexplot7)
  hexplot7 <- hexplot7 + theme(legend.position = 'none')
  hexplot8 <- hexplot8 + theme(legend.position = 'none')
  
  plot_grid(hexplot7, hexplot8, hexleg, ncol = 3, nrow = 1, rel_widths = c(1,1, 0.2))
  
}


# 17. draws a 3d plot of fish location and depth

fish.3dplot <- function(period)
{
  fish.id <- subset(dayfile, Period == period)
  scatterplot3d(fish.id$PosX, fish.id$PosY, fish.id$PosZ, pch = 20, xlim =  c(10, 45), ylim = c(10, 45), zlim = c(26, 0))
}


# 18. draws a 3d interactive plot of fish location and depth

fish.3dmove <- function(period)
{
  fish.id <- subset(dayfile, Period == period)
  plot3d(fish.id$PosX, fish.id$PosY, fish.id$PosZ, cex = 1, xlim =  c(10, 45), ylim = c(10, 45), zlim = c(0, 35), xlab = 'X', ylab = 'Y', zlab = 'Z', type = 'l', col = '#26b426', lwd = 2)
}



# 19a. draws a plot of fish location by depth

plot.bydepth <- function(period)
{
  depthpal <- diverge_hcl(30, h = c(11,266), c = 100, l = c(21,85), power = 0.6)
  fish.id <- subset(dayfile, Period == period)
  
  if(fish.id[1,3] == '7')
  {
    
    # plot(fish.id$PosX, fish.id$PosY, xlab = 'X', ylab = 'Y', pch = 20, cex = 0.8, xlim = c(0, 40), ylim = c(0, 45), type = 'p', col = rgb(0, 0.6, 0, 0.2)) # wider plot
    plot(fish.id$PosX, fish.id$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(10, 45), ylim = c(10, 45), type = 'p', col = depthpal[round(fish.id$PosZ)]) # tight plot
    rect(locations.lookup['7EW', 'xmin'], locations.lookup['7EW', 'ymin'], locations.lookup['7EW', 'xmax'], locations.lookup['7EW', 'ymax'], lty = 2) # 7EW edge
    rect(locations.lookup['7ES', 'xmin'], locations.lookup['7ES', 'ymin'], locations.lookup['7ES', 'xmax'], locations.lookup['7ES', 'ymax'], lty = 2) # 7ES edge
    rect(locations.lookup['7EE', 'xmin'], locations.lookup['7EE', 'ymin'], locations.lookup['7EE', 'xmax'], locations.lookup['7EE', 'ymax'], lty = 2) # 7EE edge
    rect(locations.lookup['7EN', 'xmin'], locations.lookup['7EN', 'ymin'], locations.lookup['7EN', 'xmax'], locations.lookup['7EN', 'ymax'], lty = 2) # 7EN edge
    rect(locations.lookup['7WHSE', 'xmin'], locations.lookup['7WHSE', 'ymin'], locations.lookup['7WHSE', 'xmax'], locations.lookup['7WHSE', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHSE
    rect(locations.lookup['7WHNW', 'xmin'], locations.lookup['7WHNW', 'ymin'], locations.lookup['7WHNW', 'xmax'], locations.lookup['7WHNW', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHNW
    rect(locations.lookup['7EW', 'xmin'], locations.lookup['7ES', 'ymin'], locations.lookup['7EE', 'xmax'], locations.lookup['7EN', 'ymax'], lwd = 2) # cage limits
    legend(41, 42, as.character(1:30), col = depthpal, pch = 15, bty = 'n', cex = 1, pt.cex = 2.6, horiz = FALSE, y.intersp = 0.5, title = 'depth (m)', text.width = 0.2, yjust = 1)
    
    
  }else{
    
    #plot(fish.id$PosX, fish.id$PosY, xlab = 'X', ylab = 'Y', pch = 20, cex = 0.8, xlim = c(25, 70), ylim = c(0, 45), type = 'p', col = rgb(0, 0.6, 0, 0.2)) # wider plot
    plot(fish.id$PosX, fish.id$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(37, 72), ylim = c(10, 45), type = 'p', col = depthpal[round(fish.id$PosZ)]) # tight plot
    rect(locations.lookup['8EW', 'xmin'], locations.lookup['8EW', 'ymin'], locations.lookup['8EW', 'xmax'], locations.lookup['8EW', 'ymax'], lty = 2) # 7EW edge
    rect(locations.lookup['8ES', 'xmin'], locations.lookup['8ES', 'ymin'], locations.lookup['8ES', 'xmax'], locations.lookup['8ES', 'ymax'], lty = 2) # 7ES edge
    rect(locations.lookup['8EE', 'xmin'], locations.lookup['8EE', 'ymin'], locations.lookup['8EE', 'xmax'], locations.lookup['8EE', 'ymax'], lty = 2) # 7EE edge
    rect(locations.lookup['8EN', 'xmin'], locations.lookup['8EN', 'ymin'], locations.lookup['8EN', 'xmax'], locations.lookup['8EN', 'ymax'], lty = 2) # 7EN edge
    rect(locations.lookup['8WHSW', 'xmin'], locations.lookup['8WHSW', 'ymin'], locations.lookup['8WHSW', 'xmax'], locations.lookup['8WHSW', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHSE
    rect(locations.lookup['8WHNE', 'xmin'], locations.lookup['8WHNE', 'ymin'], locations.lookup['8WHNE', 'xmax'], locations.lookup['8WHNE', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHNW
    rect(locations.lookup['8EW', 'xmin'], locations.lookup['8ES', 'ymin'], locations.lookup['8EE', 'xmax'], locations.lookup['8EN', 'ymax'], lwd = 2) # cage limits
    legend(67, 42, as.character(1:30), col = depthpal, pch = 15, bty = 'n', cex = 1, pt.cex = 2.6, horiz = FALSE, y.intersp = 0.5, title = 'depth (m)', text.width = 0.2)
    
  }
}



# 19b. draws a plot of fish location by activity behaviour state

plot.byactivity <- function(period, static = 0.1, burst = 1)
{
  #activitypal <- heat_hcl(3, h = c(0,-100), c = c(40, 80), l = c(75,40), power = 1)
  activitypal <- brewer.pal(3, 'Set1')
  pen.col <- 'black'
  pen.size <- 0.8
  
  fish.id <- subset(dayfile, Period == period)
  fish.id$BS <- as.factor(ifelse(fish.id$BLSEC < 0.1, 'static', ifelse(fish.id$BLSEC >=0.1 & fish.id$BLSEC <1, 'cruise', 'burst')))
  fish.id$BS <- factor(fish.id$BS, levels = c('cruise', 'static', 'burst'))
  fish.id <- fish.id[order(fish.id$BS, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by behaviour state
  
  
  if (unique(fish.id$PEN == '7')) {
    
    fish.plot <- ggplot(fish.id, aes(PosX, PosY)) +
      scale_x_continuous('x (m)', limits = c(10, 45)) + scale_y_continuous('y (m)', limits = c(10,45)) + # set scale limits      
      theme(panel.background = element_rect(fill = 'white', colour = 'black')) + # white background, black lines
      geom_point(aes(colour = cut(BLSEC, c(-Inf, static, burst, Inf))), size = 3)  + scale_color_manual(name = 'activity (BL/sec)', values = c("(-Inf,0.1]" = activitypal[[3]], "(0.1,1]" = activitypal[[2]], "(1, Inf]" = activitypal[[1]]), labels = c('static (< 0.1)', 'cruise (0.1 - 1)', 'burst (>1)')) +
      annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CNW', 'xmin'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CNW', 'ymax'], colour = pen.col, size = pen.size) +  # pen boundary
      annotate('segment', x = locations.lookup['7CNW', 'xmin'], xend = locations.lookup['7CNE', 'xmax'], y = locations.lookup['7CNW', 'ymax'], yend = locations.lookup['7CNE', 'ymax'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['7CNE', 'xmax'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CNE', 'ymax'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CSW', 'ymax'], yend = locations.lookup['7CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['7CSW', 'xmax'], xend = locations.lookup['7CNW', 'xmax'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['7CNW', 'xmin'], xend = locations.lookup['7CNE', 'xmax'], y = locations.lookup['7CNW', 'ymin'], yend = locations.lookup['7CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['7CNE', 'xmin'], xend = locations.lookup['7CSE', 'xmin'], y = locations.lookup['7CNE', 'ymax'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('curve', x = locations.lookup['7WHNW', 'xmin']+1, xend = locations.lookup['7WHNW', 'xmax']-1, y = locations.lookup['7WHNW', 'ymin']+1, yend = locations.lookup['7WHNW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
      annotate('curve', x = locations.lookup['7WHNW', 'xmin']+1, xend = locations.lookup['7WHNW', 'xmax']-1, y = locations.lookup['7WHNW', 'ymin']+1, yend = locations.lookup['7WHNW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
      annotate('curve', x = locations.lookup['7WHSE', 'xmin']+1, xend = locations.lookup['7WHSE', 'xmax']-1, y = locations.lookup['7WHSE', 'ymin']+1, yend = locations.lookup['7WHSE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
      annotate('curve', x = locations.lookup['7WHSE', 'xmin']+1, xend = locations.lookup['7WHSE', 'xmax']-1, y = locations.lookup['7WHSE', 'ymin']+1, yend = locations.lookup['7WHSE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) # hide boundary
    fish.plot
    
  } else {
    
    fish.plot <- ggplot(fish.id, aes(PosX, PosY)) +
      scale_x_continuous('x (m)', limits = c(30,65)) + scale_y_continuous('y (m)', limits = c(8,43)) + 
      theme(panel.background = element_rect(fill = 'white', colour = 'black')) + # white background, black lines
      geom_point(aes(colour = cut(BLSEC, c(-Inf, static, burst, Inf))), size = 3)  + scale_color_manual(name = 'activity (BL/sec)', values = c("(-Inf,0.1]" = activitypal[[3]], "(0.1,1]" = activitypal[[2]], "(1, Inf]" = activitypal[[1]]), labels = c('static (< 0.1)', 'cruise (0.1 - 1)', 'burst (>1)')) +
      annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CNW', 'xmin'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CNW', 'ymax'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['8CNW', 'xmin'], xend = locations.lookup['8CNE', 'xmax'], y = locations.lookup['8CNW', 'ymax'], yend = locations.lookup['8CNE', 'ymax'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['8CNE', 'xmax'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CNE', 'ymax'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CSW', 'ymax'], yend = locations.lookup['8CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('segment', x = locations.lookup['8CSW', 'xmax'], xend = locations.lookup['8CNW', 'xmax'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('segment', x = locations.lookup['8CNW', 'xmin'], xend = locations.lookup['8CNE', 'xmax'], y = locations.lookup['8CNW', 'ymin'], yend = locations.lookup['8CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('segment', x = locations.lookup['8CNE', 'xmin'], xend = locations.lookup['8CSE', 'xmin'], y = locations.lookup['8CNE', 'ymax'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('curve', x = locations.lookup['8WHSW', 'xmin']+1, xend = locations.lookup['8WHSW', 'xmax']-1, y = locations.lookup['8WHSW', 'ymin']+1, yend = locations.lookup['8WHSW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
      annotate('curve', x = locations.lookup['8WHSW', 'xmin']+1, xend = locations.lookup['8WHSW', 'xmax']-1, y = locations.lookup['8WHSW', 'ymin']+1, yend = locations.lookup['8WHSW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
      annotate('curve', x = locations.lookup['8WHNE', 'xmin']+1, xend = locations.lookup['8WHNE', 'xmax']-1, y = locations.lookup['8WHNE', 'ymin']+1, yend = locations.lookup['8WHNE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
      annotate('curve', x = locations.lookup['8WHNE', 'xmin']+1, xend = locations.lookup['8WHNE', 'xmax']-1, y = locations.lookup['8WHNE', 'ymin']+1, yend = locations.lookup['8WHNE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1)# + # hide boundary
    fish.plot
    
  }
  
}



# 19c. draws a plot of fish location by time of day

plot.bylight <- function(period)
{
  
  lightpal <- brewer.pal(11, 'Spectral')
  lightpal <- c(lightpal[[4]], lightpal[[5]], lightpal[[3]], lightpal[[11]])
  pen.col <- 'black'
  pen.size <- 0.8
  
  fish.id <- subset(dayfile, Period == period)
  fish.id <- subset(fish.id, SUN == 'N' | SUN == 'W' | SUN == 'D' | SUN == 'K')
  #fish.id$BS <- as.factor(ifelse(fish.id$BLSEC < 0.1, 'static', ifelse(fish.id$BLSEC >=0.1 & fish.id$BLSEC <1, 'cruise', 'burst')))
  fish.id$SUN <- factor(fish.id$SUN, levels = c('D', 'W', 'K', 'N'))
  fish.id <- fish.id[order(fish.id$SUN, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by behaviour state
  fish.id$SUN <- factor(fish.id$SUN, levels = c('W', 'D', 'K', 'N'))
  
  if (unique(fish.id$PEN == '7')) {
    
    fish.plot <- ggplot(fish.id, aes(PosX, PosY)) +
      scale_x_continuous('x (m)', limits = c(10, 45)) + scale_y_continuous('y (m)', limits = c(10,45)) + # set scale limits      
      theme(panel.background = element_rect(fill = 'white', colour = 'black')) + # white background, black lines
      geom_point(aes(colour = SUN), size = 3)  + scale_color_manual(name = 'Time of day', values = lightpal, labels = c('Dawn', 'Day', 'Dusk', 'Night')) +
      annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CNW', 'xmin'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CNW', 'ymax'], colour = pen.col, size = pen.size) +  # pen boundary
      annotate('segment', x = locations.lookup['7CNW', 'xmin'], xend = locations.lookup['7CNE', 'xmax'], y = locations.lookup['7CNW', 'ymax'], yend = locations.lookup['7CNE', 'ymax'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['7CNE', 'xmax'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CNE', 'ymax'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CSW', 'ymax'], yend = locations.lookup['7CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['7CSW', 'xmax'], xend = locations.lookup['7CNW', 'xmax'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['7CNW', 'xmin'], xend = locations.lookup['7CNE', 'xmax'], y = locations.lookup['7CNW', 'ymin'], yend = locations.lookup['7CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['7CNE', 'xmin'], xend = locations.lookup['7CSE', 'xmin'], y = locations.lookup['7CNE', 'ymax'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('curve', x = locations.lookup['7WHNW', 'xmin']+1, xend = locations.lookup['7WHNW', 'xmax']-1, y = locations.lookup['7WHNW', 'ymin']+1, yend = locations.lookup['7WHNW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
      annotate('curve', x = locations.lookup['7WHNW', 'xmin']+1, xend = locations.lookup['7WHNW', 'xmax']-1, y = locations.lookup['7WHNW', 'ymin']+1, yend = locations.lookup['7WHNW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
      annotate('curve', x = locations.lookup['7WHSE', 'xmin']+1, xend = locations.lookup['7WHSE', 'xmax']-1, y = locations.lookup['7WHSE', 'ymin']+1, yend = locations.lookup['7WHSE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
      annotate('curve', x = locations.lookup['7WHSE', 'xmin']+1, xend = locations.lookup['7WHSE', 'xmax']-1, y = locations.lookup['7WHSE', 'ymin']+1, yend = locations.lookup['7WHSE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) # hide boundary
    fish.plot
    
  } else {
    
    fish.plot <- ggplot(fish.id, aes(PosX, PosY)) +
      scale_x_continuous('x (m)', limits = c(30,65)) + scale_y_continuous('y (m)', limits = c(8,43)) + 
      theme(panel.background = element_rect(fill = 'white', colour = 'black')) + # white background, black lines
      geom_point(aes(colour = SUN), size = 3)  + scale_color_manual(name = 'Time of day', values = lightpal, labels = c('Dawn', 'Day', 'Dusk', 'Night')) +
      annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CNW', 'xmin'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CNW', 'ymax'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['8CNW', 'xmin'], xend = locations.lookup['8CNE', 'xmax'], y = locations.lookup['8CNW', 'ymax'], yend = locations.lookup['8CNE', 'ymax'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['8CNE', 'xmax'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CNE', 'ymax'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CSW', 'ymax'], yend = locations.lookup['8CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('segment', x = locations.lookup['8CSW', 'xmax'], xend = locations.lookup['8CNW', 'xmax'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('segment', x = locations.lookup['8CNW', 'xmin'], xend = locations.lookup['8CNE', 'xmax'], y = locations.lookup['8CNW', 'ymin'], yend = locations.lookup['8CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('segment', x = locations.lookup['8CNE', 'xmin'], xend = locations.lookup['8CSE', 'xmin'], y = locations.lookup['8CNE', 'ymax'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('curve', x = locations.lookup['8WHSW', 'xmin']+1, xend = locations.lookup['8WHSW', 'xmax']-1, y = locations.lookup['8WHSW', 'ymin']+1, yend = locations.lookup['8WHSW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
      annotate('curve', x = locations.lookup['8WHSW', 'xmin']+1, xend = locations.lookup['8WHSW', 'xmax']-1, y = locations.lookup['8WHSW', 'ymin']+1, yend = locations.lookup['8WHSW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
      annotate('curve', x = locations.lookup['8WHNE', 'xmin']+1, xend = locations.lookup['8WHNE', 'xmax']-1, y = locations.lookup['8WHNE', 'ymin']+1, yend = locations.lookup['8WHNE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
      annotate('curve', x = locations.lookup['8WHNE', 'xmin']+1, xend = locations.lookup['8WHNE', 'xmax']-1, y = locations.lookup['8WHNE', 'ymin']+1, yend = locations.lookup['8WHNE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1)# + # hide boundary
    fish.plot
    
  }
  
}

# 20. Add a fish to the current plot

add.depthfish <- function(period)
{
  depthpal <- diverge_hcl(30, h = c(11,266), c = 100, l = c(21,85), power = 0.6, alpha = 0.2)
  fish.id <- subset(dayfile, Period == period)
  points(fish.id$PosX, fish.id$PosY, pch = 20, cex = 1, col = depthpal[round(fish.id$PosZ)])
}




# 21. Fractal dimension

fractal <- function(xmin7 = 5, xmax7 = 45, ymin7 = 5, ymax7 = 45, xmin8 = 35, xmax8 = 75, ymin8 = 5, ymax8 = 45, boxsize = 0.1) {
  
  fd.P7 <- data.frame(x = numeric, y = integer)
  fd.P8 <- data.frame(x = numeric, y = integer)
  bs <- boxsize
  
  pen.id <- subset(dayfile, PEN == '7')
  
  repeat {
    
    
    x.grid <- floor((pen.id$PosX - xmin7) / bs) + 1
    y.grid <- floor((pen.id$PosY - ymin7) / bs) + 1
    x.grid.max <- floor((xmax7 - xmin7) / bs) + 1
    y.grid.max <- floor((ymax7 - ymin7) / bs) + 1
    t.x <- sort(unique(x.grid))
    t.y <- sort(unique(y.grid))
    tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
    ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
    t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
    grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
    t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
    t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
    eg <- expand.grid(t.y,t.x)
    grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
    fd.P7 <- rbind(fd.P7, c(bs, length(which(grid.cov > 0))))
    bs <- bs*2
    
    if (bs > xmax7-xmin7 | bs > ymax7-ymin7)
    {break}
  }
  colnames(fd.P7) <- c('P7.boxsize', 'P7.count')
  bs <- boxsize
  
  
  fl <- lm(log(P7.count) ~ log(P7.boxsize), data=fd.P7)
  scatterplot(fd.P7$P7.boxsize, fd.P7$P7.count, log = 'xy', boxplots = FALSE, smoother = FALSE, grid = FALSE)
  text(1, 100, paste0('fd = ', as.character(round(fl$coefficients[[2]], 3)), '\nR2 = ', round(summary(fl)$r.squared, 4)))
  
  #scatterplot(fd.P7$P7.boxsize, fd.P7$P7.count, log = 'xy', boxplots = FALSE, smoother = FALSE, grid = FALSE)
  
  cat('Press [enter] to continue')
  line <- readline()
  
  pen.id <- subset(dayfile, PEN == '8')
  
  repeat{
    
    x.grid <- floor((pen.id$PosX - xmin8) / bs) + 1
    y.grid <- floor((pen.id$PosY - ymin8) / bs) + 1
    x.grid.max <- floor((xmax8 - xmin8) / bs) + 1
    y.grid.max <- floor((ymax8 - ymin8) / bs) + 1
    t.x <- sort(unique(x.grid))
    t.y <- sort(unique(y.grid))
    tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
    ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
    t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
    grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
    t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
    t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
    eg <- expand.grid(t.y,t.x)
    grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
    fd.P8 <- rbind(fd.P8, c(bs, length(which(grid.cov > 0))))
    bs <- bs*2
    
    if (bs > xmax8-xmin8 | bs > ymax8-ymin8)
    {break}
  }
  colnames(fd.P8) <- c('P8.boxsize', 'P8.count')
  
  fl <- lm(log(P8.count) ~ log(P8.boxsize), data=fd.P8)
  scatterplot(fd.P8$P8.boxsize, fd.P8$P8.count, log = 'xy', boxplots = FALSE, smoother = FALSE, grid = FALSE)
  text(1, 100, paste0('fd = ', as.character(round(fl$coefficients[[2]], 3)), '\nR2 = ', round(summary(fl)$r.squared, 4)))
  
  fd <- cbind(fd.P7, fd.P8) 
  fd
  
  
}


# 22. batch Fractal dimension

batch.fractals <- function(xmin7 = 5, xmax7 = 45, ymin7 = 5, ymax7 = 45, xmin8 = 35, xmax8 = 75, ymin8 = 5, ymax8 = 45, boxsize = 0.1) {
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  #fcount.P7 <- data.frame(x = numeric, y = integer)
  #fcount.P8 <- data.frame(x = numeric, y = integer)
  bs <- boxsize
  
  dayfile.loc <- files[[1]]
  dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = 'character')
  # dayfile[,1] <- NULL
  
  pen.id <- subset(dayfile, dayfile$PEN == '7')
  fish.ids7 <- unique(pen.id$Period)
  fd.P7 <- data.frame(fish.ids7)
  rownames(fd.P7) <- fd.P7[,1]
  colnames(fd.P7) <- 'Period'
  pen.id <- subset(dayfile, dayfile$PEN == '8')
  fish.ids8 <- unique(pen.id$Period)
  fd.P8 <- data.frame(fish.ids8)
  rownames(fd.P8) <- fd.P8[,1]
  colnames(fd.P8) <- 'Period'
  
  for (n in 1:length(files))
  {
    dayfile.loc <- files[[n]]
    dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = c
                        (
                        'NULL', 'factor', 'factor', 'factor', 'POSIXct', 'double', 'double', 
                        'double', 'double', 'double', 'double', 'double', 'double', 'factor',
                        'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
                        'double', 'double', 'double', 'double', 'double', 'double', 'double',
                        'double', 'double', 'double', 'double', 'double', 'double', 'double',
                        'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                        'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                        'factor', 'factor', 'double', 'double', 'double', 'double', 'double', 
                        'double', 'double', 'double', 'double', 'double', 'double', 'double' 
                        )) #read data into table
    
    fcount.P7 <- data.frame(x = numeric, y = integer)
    fcount.P8 <- data.frame(x = numeric, y = integer)  
    
    pen.id <- subset(dayfile, PEN == '7')
    
    for (i in 1:length(fish.ids7)){
      
      fish.id <- subset(pen.id, Period == fish.ids7[[i]])  
      
      if(nrow(fish.id) == 0){
        fd.P7[i,paste0(n, '.fractal')] <- NA
        fd.P7[i,paste0(n, '.R2')] <- NA
      }
      else{
        
        repeat {
          
          x.grid <- floor((fish.id$PosX - xmin7) / bs) + 1
          y.grid <- floor((fish.id$PosY - ymin7) / bs) + 1
          x.grid.max <- floor((xmax7 - xmin7) / bs) + 1
          y.grid.max <- floor((ymax7 - ymin7) / bs) + 1
          t.x <- sort(unique(x.grid))
          t.y <- sort(unique(y.grid))
          tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
          ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
          t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
          grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
          t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
          t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
          eg <- expand.grid(t.y,t.x)
          grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
          fcount.P7 <- rbind(fcount.P7, c(bs, length(which(grid.cov > 0))))
          bs <- bs*2
          
          if (bs > xmax7-xmin7 | bs > ymax7-ymin7)
          {break}
        }
        colnames(fcount.P7) <- c('P7.boxsize', 'P7.count')
        bs <- boxsize
        
        
        fl <- lm(log(P7.count) ~ log(P7.boxsize), data=fcount.P7)
        fd.P7[i,paste0(n, '.fractal')] <- round(fl$coefficients[[2]], 3)
        fd.P7[i,paste0(n, '.R2')] <- round(summary(fl)$r.squared, 4)
        #print(fcount.P7)
        
      }
      
    }
    
    pen.id <- subset(dayfile, PEN == '8')
    
    
    for (i in 1:length(fish.ids8)){
      
      fish.id <- subset(pen.id, Period == fish.ids8[[i]])
      
      if(nrow(fish.id) == 0){
        fd.P8[i,paste0(n, '.fractal')] <- NA
        fd.P8[i,paste0(n, '.R2')] <- NA
      }
      else{
        
        repeat{
          
          x.grid <- floor((fish.id$PosX - xmin8) / bs) + 1
          y.grid <- floor((fish.id$PosY - ymin8) / bs) + 1
          x.grid.max <- floor((xmax8 - xmin8) / bs) + 1
          y.grid.max <- floor((ymax8 - ymin8) / bs) + 1
          t.x <- sort(unique(x.grid))
          t.y <- sort(unique(y.grid))
          tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
          ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
          t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
          grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
          t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
          t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
          eg <- expand.grid(t.y,t.x)
          grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
          fcount.P8 <- rbind(fcount.P8, c(bs, length(which(grid.cov > 0))))
          bs <- bs*2
          
          if (bs > xmax8-xmin8 | bs > ymax8-ymin8)
          {break}
        }
        colnames(fcount.P8) <- c('P8.boxsize', 'P8.count')
        bs <- boxsize
        
        
        fl <- lm(log(P8.count) ~ log(P8.boxsize), data=fcount.P8)
        fd.P8[i,paste0(n, '.fractal')] <- round(fl$coefficients[[2]], 3)
        fd.P8[i,paste0(n, '.R2')] <- round(summary(fl)$r.squared, 4)
        #print(fcount.P8)
        
      }
      
    }
    
    remove(fcount.P7)
    remove(fcount.P8)
    
  }
  
  #fd.P7$fish.ids7 <- NULL
  #fd.P8$fish.ids8 <- NULL
  fd <- rbind(fd.P7, fd.P8) 
  fd
  #loadWorkbook('FractalOutput.xlsx', create = TRUE)
  #writeWorksheetToFile('FractalOutput.xlsx', fd, 'Sheet 1')
  
  write.xlsx(fd, 'FractalOutput.xlsx')
}




# 23. Invidual fish Fractal dimension

id.fractals <- function(xmin7 = 5, xmax7 = 45, ymin7 = 5, ymax7 = 45, xmin8 = 35, xmax8 = 75, ymin8 = 5, ymax8 = 45, boxsize = 0.1) {
  
  #files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  #fcount.P7 <- data.frame(x = numeric, y = integer)
  #fcount.P8 <- data.frame(x = numeric, y = integer)
  bs <- boxsize
  
  #dayfile.loc <- files[[1]]
  dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = c
                      (
                      'NULL', 'factor', 'factor', 'factor', 'POSIXct', 'double', 'double', 
                      'double', 'double', 'double', 'double', 'double', 'double', 'factor',
                      'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
                      'double', 'double', 'double', 'double', 'double', 'double', 'double',
                      'double', 'double', 'double', 'double', 'double', 'double', 'double',
                      'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                      'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                      'factor', 'factor', 'double', 'double', 'double', 'double', 'double', 
                      'double', 'double', 'double', 'double', 'double', 'double', 'double' 
                      )) #read data into table
  # dayfile[,1] <- NULL
  
  pen.id <- subset(dayfile, dayfile$PEN == '7')
  fish.ids7 <- unique(pen.id$Period)
  fd.P7 <- data.frame(fish.ids7)
  rownames(fd.P7) <- fd.P7[,1]
  colnames(fd.P7) <- 'Period'
  pen.id <- subset(dayfile, dayfile$PEN == '8')
  fish.ids8 <- unique(pen.id$Period)
  fd.P8 <- data.frame(fish.ids8)
  rownames(fd.P8) <- fd.P8[,1]
  colnames(fd.P8) <- 'Period'
  
  
  fcount.P7 <- data.frame(x = numeric, y = integer)
  fcount.P8 <- data.frame(x = numeric, y = integer)  
  
  pen.id <- subset(dayfile, PEN == '7')
  
  for (i in 1:length(fish.ids7)){
    
    fish.id <- subset(pen.id, Period == fish.ids7[[i]])  
    
    if(nrow(fish.id) == 0){
      fd.P7[i,'fractal'] <- NA
      fd.P7[i,'R2'] <- NA
    }
    else{
      
      repeat {
        
        x.grid <- floor((fish.id$PosX - xmin7) / bs) + 1
        y.grid <- floor((fish.id$PosY - ymin7) / bs) + 1
        x.grid.max <- floor((xmax7 - xmin7) / bs) + 1
        y.grid.max <- floor((ymax7 - ymin7) / bs) + 1
        t.x <- sort(unique(x.grid))
        t.y <- sort(unique(y.grid))
        tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
        ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
        t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
        grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
        t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
        t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
        eg <- expand.grid(t.y,t.x)
        grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
        fcount.P7 <- rbind(fcount.P7, c(bs, length(which(grid.cov > 0))))
        bs <- bs*2
        
        if (bs > xmax7-xmin7 | bs > ymax7-ymin7)
        {break}
      }
      colnames(fcount.P7) <- c('P7.boxsize', 'P7.count')
      bs <- boxsize
      
      
      fl <- lm(log(P7.count) ~ log(P7.boxsize), data=fcount.P7)
      fd.P7[i,'fractal'] <- round(fl$coefficients[[2]], 3)
      fd.P7[i,'R2'] <- round(summary(fl)$r.squared, 4)
      #print(fcount.P7)
      
    }
    
  }
  
  pen.id <- subset(dayfile, PEN == '8')
  
  
  for (i in 1:length(fish.ids8)){
    
    fish.id <- subset(pen.id, Period == fish.ids8[[i]])
    
    if(nrow(fish.id) == 0){
      fd.P8[i,'fractal'] <- NA
      fd.P8[i,'R2'] <- NA
    }
    else{
      
      repeat{
        
        x.grid <- floor((fish.id$PosX - xmin8) / bs) + 1
        y.grid <- floor((fish.id$PosY - ymin8) / bs) + 1
        x.grid.max <- floor((xmax8 - xmin8) / bs) + 1
        y.grid.max <- floor((ymax8 - ymin8) / bs) + 1
        t.x <- sort(unique(x.grid))
        t.y <- sort(unique(y.grid))
        tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
        ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
        t <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
        grid.cov <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
        t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
        t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
        eg <- expand.grid(t.y,t.x)
        grid.cov[cbind(eg$Var1,eg$Var2)] <- as.vector(t)  
        fcount.P8 <- rbind(fcount.P8, c(bs, length(which(grid.cov > 0))))
        bs <- bs*2
        
        if (bs > xmax8-xmin8 | bs > ymax8-ymin8)
        {break}
      }
      colnames(fcount.P8) <- c('P8.boxsize', 'P8.count')
      bs <- boxsize
      
      
      fl <- lm(log(P8.count) ~ log(P8.boxsize), data=fcount.P8)
      fd.P8[i,'fractal'] <- round(fl$coefficients[[2]], 3)
      fd.P8[i,'R2'] <- round(summary(fl)$r.squared, 4)
      #print(fcount.P8)
      
    }
    
  }
  
  #print(fcount.P7)
  #print(fcount.P8)
  
  
  
  #fd.P7$fish.ids7 <- NULL
  #fd.P8$fish.ids8 <- NULL
  fd <- rbind(fd.P7, fd.P8) 
  print(fd)
  loadWorkbook('FractalOutput.xlsx', create = TRUE)
  writeWorksheetToFile('FractalOutput.xlsx', fd, 'Sheet 1')
}


# 24. draws a plot of fish location coloured by time

plot.bytime <- function(period, units = 'd')
{
  fish.id <- subset(dayfile, Period == period)
  ifelse(units == 'd', timepoints <- unique(format(as.Date(dayfile$EchoTime, format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d')), ifelse(units == 'h', timepoints <- unique(trunc(dayfile$EchoTime, "hour")), print('Error: specify days (d) or hours (h)'))) 
  bins <- length(timepoints)
  timepal <- rainbow(bins, alpha = 0.2)
  par(mfrow=c(1,1))
  
  if(fish.id[1,3] == '7')
  {
    if(units == 'd'){
      plot(fish.id[which(format(as.Date(dayfile$EchoTime, format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d') == timepoints[[1]]),'PosX'], fish.id[which(format(as.Date(dayfile$EchoTime, format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d') == timepoints[[1]]),'PosY'], xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(10, 45), ylim = c(10, 45), type = 'p', col = timepal[1])
    }else{
      plot(fish.id[which(trunc(dayfile$EchoTime, "hour") == timepoints[1]),'PosX'], fish.id[which(trunc(dayfile$EchoTime, "hour") == timepoints[1]),'PosY'], xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(10, 45), ylim = c(10, 45), type = 'p', col = timepal[1])
    }
    
    legend(10, 47, as.character(1:bins), col = rainbow(bins, alpha = 1) , pch = 15, bty = 'n', pt.cex = 1.5, horiz = FALSE, y.intersp = 1, cex = (100-bins)/100)
    
    
    if(units == 'd'){
      for (i in 2:bins){
        points(fish.id[which(format(as.Date(dayfile$EchoTime, format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d') == timepoints[[i]]),'PosX'], fish.id[which(format(as.Date(dayfile$EchoTime, format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d') == timepoints[[i]]),'PosY'], pch = 20, cex = 1, col = timepal[i])
      }
    }else{
      for (i in 2:bins){   
        points(fish.id[which(trunc(dayfile$EchoTime, "hour") == timepoints[i]),'PosX'], fish.id[which(trunc(dayfile$EchoTime, "hour") == timepoints[i]),'PosY'], pch = 20, cex = 1, col = timepal[i])
      }
    }
    
    rect(locations.lookup['7EW', 'xmin'], locations.lookup['7EW', 'ymin'], locations.lookup['7EW', 'xmax'], locations.lookup['7EW', 'ymax'], lty = 2) # 7EW edge
    rect(locations.lookup['7ES', 'xmin'], locations.lookup['7ES', 'ymin'], locations.lookup['7ES', 'xmax'], locations.lookup['7ES', 'ymax'], lty = 2) # 7ES edge
    rect(locations.lookup['7EE', 'xmin'], locations.lookup['7EE', 'ymin'], locations.lookup['7EE', 'xmax'], locations.lookup['7EE', 'ymax'], lty = 2) # 7EE edge
    rect(locations.lookup['7EN', 'xmin'], locations.lookup['7EN', 'ymin'], locations.lookup['7EN', 'xmax'], locations.lookup['7EN', 'ymax'], lty = 2) # 7EN edge
    rect(locations.lookup['7WHSE', 'xmin'], locations.lookup['7WHSE', 'ymin'], locations.lookup['7WHSE', 'xmax'], locations.lookup['7WHSE', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHSE
    rect(locations.lookup['7WHNW', 'xmin'], locations.lookup['7WHNW', 'ymin'], locations.lookup['7WHNW', 'xmax'], locations.lookup['7WHNW', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHNW
    rect(locations.lookup['7EW', 'xmin'], locations.lookup['7ES', 'ymin'], locations.lookup['7EE', 'xmax'], locations.lookup['7EN', 'ymax'], lwd = 2) # cage limits
    
  }else{
    
    
    if(units == 'd'){
      plot(fish.id[which(format(as.Date(dayfile$EchoTime, format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d') == timepoints[[1]]),'PosX'], fish.id[which(format(as.Date(dayfile$EchoTime, format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d') == timepoints[[1]]),'PosY'], xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(25, 70), ylim = c(0, 45), type = 'p', col = timepal[1])
    }else{
      plot(fish.id[which(trunc(dayfile$EchoTime, "hour") == timepoints[1]),'PosX'], fish.id[which(trunc(dayfile$EchoTime, "hour") == timepoints[1]),'PosY'], xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(37, 72), ylim = c(10, 45), type = 'p', col = timepal[1])
    }
    
    legend(32, 47, as.character(1:bins), col = rainbow(bins, alpha = 1) , pch = 15, bty = 'n', pt.cex = 1.5, horiz = FALSE, y.intersp = 1, cex = (100-bins)/100)
    
    if(units == 'd'){ 
      for (i in 2:bins){
        points(fish.id[which(format(as.Date(dayfile$EchoTime, format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d') == timepoints[[i]]),'PosX'], fish.id[which(format(as.Date(dayfile$EchoTime, format='%Y-%m-%d %H:%M:%S'), '%Y-%m-%d') == timepoints[[i]]),'PosY'], pch = 20, cex = 1, col = timepal[i])
      }
    }else{
      for (i in 2:bins){    
        points(fish.id[which(trunc(dayfile$EchoTime, "hour") == timepoints[i]),'PosX'], fish.id[which(trunc(dayfile$EchoTime, "hour") == timepoints[i]),'PosY'], pch = 20, cex = 1, col = timepal[i])
      }
    }
    
    rect(locations.lookup['8EW', 'xmin'], locations.lookup['8EW', 'ymin'], locations.lookup['8EW', 'xmax'], locations.lookup['8EW', 'ymax'], lty = 2) # 7EW edge
    rect(locations.lookup['8ES', 'xmin'], locations.lookup['8ES', 'ymin'], locations.lookup['8ES', 'xmax'], locations.lookup['8ES', 'ymax'], lty = 2) # 7ES edge
    rect(locations.lookup['8EE', 'xmin'], locations.lookup['8EE', 'ymin'], locations.lookup['8EE', 'xmax'], locations.lookup['8EE', 'ymax'], lty = 2) # 7EE edge
    rect(locations.lookup['8EN', 'xmin'], locations.lookup['8EN', 'ymin'], locations.lookup['8EN', 'xmax'], locations.lookup['8EN', 'ymax'], lty = 2) # 7EN edge
    rect(locations.lookup['8WHSW', 'xmin'], locations.lookup['8WHSW', 'ymin'], locations.lookup['8WHSW', 'xmax'], locations.lookup['8WHSW', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHSE
    rect(locations.lookup['8WHNE', 'xmin'], locations.lookup['8WHNE', 'ymin'], locations.lookup['8WHNE', 'xmax'], locations.lookup['8WHNE', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHNW
    rect(locations.lookup['8EW', 'xmin'], locations.lookup['8ES', 'ymin'], locations.lookup['8EE', 'xmax'], locations.lookup['8EN', 'ymax'], lwd = 2) # cage limits
    
  }
  remove(timepoints)
}

# 25. Removes single fish id from specified day files

batch.remove <- function(period, start.day, no.days){
  
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  day1 <- grep(paste0('^..............', start.day, '_day_coded.csv'), files)
  end.day <- day1+(no.days-1)
  # dayfile.loc <- files[[grep(paste0('^..............', start.day, '_day_coded.csv'), files)]]
  
  for (i in day1:end.day) {
    dayfile <- read.csv(files[[i]], header = TRUE, sep = ",", colClasses = c('NULL', 'numeric', 'factor', 'factor', 'POSIXct', 'double', 'double', 
                                                                             'double', 'double', 'double', 'double', 'double', 'double', 'factor',
                                                                             'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
                                                                             'double', 'double', 'double', 'double', 'double', 'double', 'double',
                                                                             'double', 'double', 'double', 'double', 'double', 'double', 'double',
                                                                             'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                                                                             'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                                                                             'factor', 'factor', 'double', 'double', 'double', 'double', 'double', 
                                                                             'double', 'double', 'double', 'double', 'double', 'double', 'double'
                                                                             
    )) #read data into table
    
    dayfile <- dayfile[!(dayfile$Period == period),] # remove dead fish
    write.csv(dayfile, file = files[[i]]) #write output to file
    
  } 
  
}



# 26. proportion coverage 3D (not sure this is working properly!)

prop.coverage.3d <- function(xmin7 = 15, xmax7 = 40, ymin7 = 15, ymax7 = 40, xmin8 = 42, xmax8 = 67, ymin8 = 15, ymax8 = 40, zmin7 = 0, zmax7 = 15, zmin8 = 0, zmax8 = 15, boxsize = 0.3) {
  fish.id <- subset(dayfile, PEN == '7')
  x.grid <- floor((fish.id$PosX - xmin7) / boxsize) + 1
  y.grid <- floor((fish.id$PosY - ymin7) / boxsize) + 1
  z.grid <- floor((fish.id$PosZ - zmin7) / boxsize) + 1
  x.grid.max <- floor((xmax7 - xmin7) / boxsize) + 1
  y.grid.max <- floor((ymax7 - ymin7) / boxsize) + 1
  z.grid.max <- floor((zmax7 - zmin7) / boxsize) + 1
  t.x <- sort(unique(x.grid))
  t.y <- sort(unique(y.grid))
  t.z <- sort(unique(z.grid))
  tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
  ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
  tz.range <- c(min(which(t.z > 0)), max(which(t.z <= z.grid.max)))
  t.xy <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
  t.yz <- table(y.grid, z.grid)[ty.range[1]:ty.range[2],tz.range[1]:tz.range[2]]
  grid.cov.xy <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
  grid.cov.yz <- matrix(0,nrow=y.grid.max,ncol=z.grid.max)
  t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
  t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
  t.z <- t.z[(t.z > 0) & (t.z <= z.grid.max)]
  eg.xy <- expand.grid(t.y,t.x)
  eg.yz <- expand.grid(t.y,t.z)
  grid.cov.xy[cbind(eg.xy$Var1,eg.xy$Var2)] <- as.vector(t.xy)  
  grid.cov.yz[cbind(eg.yz$Var1,eg.yz$Var2)] <- as.vector(t.yz) 
  coverage.P7 <- matrix(c(round(length(which(grid.cov.xy > 0))+length(which(grid.cov.yz > 0)), digits = 3), round(length(grid.cov.xy)*((zmax7-zmin7)/boxsize), digits = 3), signif((length(which(grid.cov.xy > 0))+length(which(grid.cov.yz > 0)))/(length(grid.cov.xy)*((zmax7-zmin7)/boxsize)), digits = 3)), ncol = 3)
  coverage.P7
  colnames(coverage.P7) <- c('occupied', 'total', 'proportion')
  
  
  #density.pal <- heat_hcl(length(as.vector(t)))
  #eg$col <- as.vector(t)
  #plot(eg$Var1, eg$Var2, col = density.pal[eg$col], pch = 15, cex = 2.5)
  
  
  fish.id <- subset(dayfile, PEN == '8')
  x.grid <- floor((fish.id$PosX - xmin8) / boxsize) + 1
  y.grid <- floor((fish.id$PosY - ymin8) / boxsize) + 1
  z.grid <- floor((fish.id$PosZ - zmin8) / boxsize) + 1
  x.grid.max <- floor((xmax8 - xmin8) / boxsize) + 1
  y.grid.max <- floor((ymax8 - ymin8) / boxsize) + 1
  z.grid.max <- floor((zmax8 - zmin8) / boxsize) + 1
  t.x <- sort(unique(x.grid))
  t.y <- sort(unique(y.grid))
  t.z <- sort(unique(z.grid))
  tx.range <- c(min(which(t.x > 0)), max(which(t.x <= x.grid.max)))
  ty.range <- c(min(which(t.y > 0)), max(which(t.y <= y.grid.max)))
  tz.range <- c(min(which(t.z > 0)), max(which(t.z <= z.grid.max)))
  t.xy <- table(y.grid, x.grid)[ty.range[1]:ty.range[2],tx.range[1]:tx.range[2]]
  t.yz <- table(y.grid, z.grid)[ty.range[1]:ty.range[2],tz.range[1]:tz.range[2]]
  grid.cov.xy <- matrix(0,nrow=y.grid.max,ncol=x.grid.max)
  grid.cov.yz <- matrix(0,nrow=y.grid.max,ncol=z.grid.max)
  t.x <- t.x[(t.x > 0) & (t.x <=x.grid.max)]
  t.y <- t.y[(t.y > 0) & (t.y <=y.grid.max)]
  t.z <- t.z[(t.z > 0) & (t.z <= z.grid.max)]
  eg.xy <- expand.grid(t.y,t.x)
  eg.yz <- expand.grid(t.y,t.z)
  grid.cov.xy[cbind(eg.xy$Var1,eg.xy$Var2)] <- as.vector(t.xy)  
  grid.cov.yz[cbind(eg.yz$Var1,eg.yz$Var2)] <- as.vector(t.yz) 
  coverage.P8 <- matrix(c(round(length(which(grid.cov.xy > 0))+length(which(grid.cov.yz > 0)), digits = 3), round(length(grid.cov.xy)*((zmax8-zmin8)/boxsize), digits = 3), signif((length(which(grid.cov.xy > 0))+length(which(grid.cov.yz > 0)))/(length(grid.cov.xy)*((zmax8-zmin8)/boxsize)), digits = 3)), ncol = 3)
  coverage.P8
  colnames(coverage.P8) <- c('occupied', 'total', 'proportion')
  
  coverage <- rbind(coverage.P7, coverage.P8) 
  rownames(coverage) <- c('P7', 'P8')
  coverage
}



# 27. moving average filter function


ma.filter <- function(period, smooth = 20, thresh = 5){
  
  fish.id <- subset(dayfile, dayfile$Period == period)
  par(mfrow=c(2,2))
  #fish.id <- subset(fish.id, fish.id$SEC >5 | is.na(fish.id$SEC) == TRUE) # remove entries where time delay too low or too high
  plot(fish.id$PosX, fish.id$PosY, xlab = 'Original', ylab = '')
  axes <- par('usr')
  filt <- rep(1/smooth, smooth)
  rem.tot <- data.frame(numeric(0))
  iteration <- 0
  
  repeat{
    
    fish.id$PosX.ma <- filter(fish.id$PosX, filt, sides = 1)
    fish.id$PosY.ma <- filter(fish.id$PosY, filt, sides = 1)
    fish.id$PosZ.ma <- filter(fish.id$PosZ, filt, sides = 1)
    fish.id$PosX.ma <- as.numeric(fish.id$PosX.ma)
    fish.id$PosY.ma <- as.numeric(fish.id$PosY.ma)
    fish.id$PosZ.ma <- as.numeric(fish.id$PosZ.ma)
    
    rem <- subset(fish.id, !(fish.id$PosX < (fish.id$PosX.ma+thresh) & fish.id$PosX > (fish.id$PosX.ma-thresh) & fish.id$PosY < (fish.id$PosY.ma+thresh) & fish.id$PosY > (fish.id$PosY.ma-thresh) & fish.id$PosZ < (fish.id$PosZ.ma+thresh) & fish.id$PosZ > (fish.id$PosZ.ma-thresh) | is.na(fish.id$PosX.ma) == TRUE))
    fish.id <- subset(fish.id, fish.id$PosX < (fish.id$PosX.ma+thresh) & fish.id$PosX > (fish.id$PosX.ma-thresh) & fish.id$PosY < (fish.id$PosY.ma+thresh) & fish.id$PosY > (fish.id$PosY.ma-thresh) & fish.id$PosZ < (fish.id$PosZ.ma+thresh) & fish.id$PosZ > (fish.id$PosZ.ma-thresh) | is.na(fish.id$PosX.ma) == TRUE)
    
    rem.tot <- rbind(rem.tot, rem)
    iteration <- iteration+1
    
    if (nrow(rem) == 0){break}
    rem <- data.frame(numeric(0))
  }
  
  cat(paste('Iterations =', iteration, '\n', sep = ' '))
  cat(paste('obervations removed =', nrow(rem.tot), '\n', sep = ' '))
  cat(paste('observations remaining =', nrow(fish.id), '\n', sep = ' '))
  plot(rem.tot$PosX, rem.tot$PosY, xlim = c(axes[[1]], axes[[2]]), ylim = c(axes[[3]], axes[[4]]), xlab = 'Observations removed', ylab = '')
  plot(fish.id$PosX, fish.id$PosY, xlim = c(axes[[1]], axes[[2]]), ylim = c(axes[[3]], axes[[4]]), xlab = 'Observations remaining', ylab = '')
  plot(fish.id$EchoTime, fish.id$PosZ, xlab = 'Time series', type = 'l')
  
  fish.id$PosX.ma <- NULL
  fish.id$PosY.ma <- NULL
  fish.id$PosZ.ma <- NULL
  
  fish.id <<- fish.id
  
}

# 28. add single fish to dayfile after cleaning data using ma.filter

add <- function(period){
  
  dayfile <- subset(dayfile, !(dayfile$Period == period))
  dayfile <- rbind(dayfile, fish.id)
  #dayfile <- dayfile[order(dayfile$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
  #dayfile <- dayfile[order(dayfile$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
  
  dayfile <<- dayfile
  
}

# 29. function to recode fish speeds and save to dayfile after cleaning data

recode <- function(masterfileloc = "H:/Data processing/AcousticTagFile_2016.xlsx"){
  
  fishid_tbl <- readWorksheetFromFile(masterfileloc, sheet = 5, startRow = 18, endCol = 16) # read in code from Fish ID lookup table
  
  periods <- unique(dayfile$Period)
  SEC <- numeric(0)
  for(i in 1:length(periods)){
    SEC <- c(SEC, as.integer(c(NA, diff(subset(dayfile$EchoTime, dayfile$Period == periods[i]), lag = 1, differences = 1)))) # calculate time delay between pings
  }
  dayfile$SEC <- SEC
  rm(SEC)  
  
  dayfile$M <- round(c(0, sqrt(diff(dayfile$PosX)^2+diff(dayfile$PosY)^2+diff(dayfile$PosZ)^2)), digits = 3) # calculate distance between pings
  dayfile$MSEC <- round(dayfile$M/dayfile$SEC, digits = 3) # calculate swimming speed in m/sec
  dayfile$MSEC <- as.numeric(sub("Inf", "0", dayfile$MSEC)) # replace "Inf" entries
  dayfile <- subset(dayfile, !dayfile$SEC <0 | is.na(dayfile$SEC) == T) # remove negative time differences
  
  fishid.bl.lookup <- fishid_tbl$L_m # create fish ID lookup table
  names(fishid.bl.lookup) <- fishid_tbl$Period
  dayfile$BL <- as.numeric(fishid.bl.lookup[as.character(dayfile$Period)]) # add fish lengths to day file
  dayfile$BLSEC <- round(dayfile$MSEC/dayfile$BL, 3) # calculate BL per sec
  
  write.csv(dayfile, file = sub("coded.csv", "recoded.csv", dayfile.loc, ignore.case = FALSE, fixed = T)) #write output to file
  
}


# 30. batch function to subset and save data according to specified variable and factors

batch.subset <- function(variable = 'SUN', factors = c('N', 'W', 'D', 'K')) {
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  
  for (i in 1:length(files))
  {
    dayfile.loc <- files[[i]]
    dayfile <- read.csv(dayfile.loc, header = TRUE, sep = ",", colClasses = dayfile.classes) #c('NULL', 'numeric', 'factor', 'factor', 'POSIXct', 'double', 'double', 
                                                                              #'double', 'double', 'double', 'double', 'double', 'double', 'factor',
                                                                              #'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
                                                                              #'double', 'double', 'double', 'double', 'double', 'double', 'double',
                                                                              #'double', 'double', 'double', 'double', 'double', 'double', 'double',
                                                                              #'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                                                                              #'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                                                                              #'factor', 'factor', 'double', 'double', 'double', 'double', 'double', 
                                                                              #'double', 'double', 'double', 'double', 'double', 'double', 'double'
    #)) #read data into table
    
    #SORT BY TIME AND TAG
    dayfile <- dayfile[order(dayfile$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
    dayfile <- dayfile[order(dayfile$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag
    
    for (j in 1:length(factors))
    {
      assign(factors[[j]], subset(dayfile, dayfile[,variable] == factors[[j]]))  
      write.csv(get(factors[[j]]), file = sub('.csv', paste0('_', factors[[j]], '.csv'), files[[i]]))
      remove(list = ls(pattern = factors[[j]])) 
    }
    
  }
  
}



# 31a. Create series of heatplots for animation

heatplot.anim <- function(pen, frames){
  
 system.time({ 
  dir.create(paste0(workingdir, '/animate'))
  setwd(paste0(workingdir, '/animate'))
  
  #frames = 24
  #pen = 7
  
  #dayfile <- dayfile[order(dayfile$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
  
  pen.col <- 'black'
  pen.size <- 0.8
  plot.col <- matlab.like(1000)  
  
  pingmax <- as.integer((as.double(max(dayfile$EchoTime))-as.double(min(dayfile$EchoTime)))/(500*5))
  
  if(pen == 7){
    pen.group <- subset(dayfile, PEN == 7)
  } else {
    pen.group <- subset(dayfile, PEN == 8)
  }
  
  minseg <- pen.group[1,'EchoTime']-seconds(1)
  
  for(i in 1:frames){
    
    # creating a name for each plot file with leading zeros
    if (i < 10) {name = paste('000',i,'plot.png',sep='')}
    
    if (i < 100 && i >= 10) {name = paste('00',i,'plot.png', sep='')}
    if (i >= 100) {name = paste('0', i,'plot.png', sep='')}
    
    # code to prepare dataset for each frame
    maxseg <- pen.group[1, 'EchoTime']+hours(i)
    
    fish.id <- subset(pen.group, EchoTime > minseg & EchoTime < maxseg)
    
    #saves the plot as a .png file in the working directory
    #png(name)
    sun <- ifelse(fish.id[1, 'SUN'] == 'N', 'Night', ifelse(fish.id[1, 'SUN'] == 'W', 'Dawn', ifelse(fish.id[1, 'SUN'] == 'K', 'Dusk', ifelse(fish.id[1,'SUN'] == 'D', 'Day', sun))))
    
    if(fish.id[1, 'PEN'] == 7){  
      
      ggplot(fish.id, aes(fish.id$PosX, fish.id$PosY)) +
        geom_hex(bins = 55, alpha = 0.6) + scale_fill_gradientn(colours=plot.col, space = 'Lab', limits = c(0, pingmax), na.value = plot.col[length(plot.col)], name = 'No. pings') +
        annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
        annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CNW', 'xmin'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CNW', 'ymax'], colour = pen.col, size = pen.size) +  # pen boundary
        annotate('segment', x = locations.lookup['7CNW', 'xmin'], xend = locations.lookup['7CNE', 'xmax'], y = locations.lookup['7CNW', 'ymax'], yend = locations.lookup['7CNE', 'ymax'], colour = pen.col, size = pen.size) + # pen boundary
        annotate('segment', x = locations.lookup['7CNE', 'xmax'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CNE', 'ymax'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
        #annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CSW', 'ymax'], yend = locations.lookup['7CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
        #annotate('segment', x = locations.lookup['7CSW', 'xmax'], xend = locations.lookup['7CNW', 'xmax'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
        #annotate('segment', x = locations.lookup['7CNW', 'xmin'], xend = locations.lookup['7CNE', 'xmax'], y = locations.lookup['7CNW', 'ymin'], yend = locations.lookup['7CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
        #annotate('segment', x = locations.lookup['7CNE', 'xmin'], xend = locations.lookup['7CSE', 'xmin'], y = locations.lookup['7CNE', 'ymax'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
        annotate('curve', x = locations.lookup['7WHNW', 'xmin']+1, xend = locations.lookup['7WHNW', 'xmax']-1, y = locations.lookup['7WHNW', 'ymin']+1, yend = locations.lookup['7WHNW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
        annotate('curve', x = locations.lookup['7WHNW', 'xmin']+1, xend = locations.lookup['7WHNW', 'xmax']-1, y = locations.lookup['7WHNW', 'ymin']+1, yend = locations.lookup['7WHNW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
        annotate('curve', x = locations.lookup['7WHSE', 'xmin']+1, xend = locations.lookup['7WHSE', 'xmax']-1, y = locations.lookup['7WHSE', 'ymin']+1, yend = locations.lookup['7WHSE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
        annotate('curve', x = locations.lookup['7WHSE', 'xmin']+1, xend = locations.lookup['7WHSE', 'xmax']-1, y = locations.lookup['7WHSE', 'ymin']+1, yend = locations.lookup['7WHSE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
        annotate('text', x = 42, y = 42, label = paste(as.character(i), 'h', sep = ' ')) + # hour count
        annotate('text', x = 42, y = 40, label = sun) + # Time of day
        theme(panel.background = element_rect(fill = 'white', colour = 'black')) + # white background, black lines
        scale_x_continuous('x (m)', limits = c(10, 45)) + scale_y_continuous('y (m)', limits = c(10,45)) # set scale limits
      
    } else {
      
      ggplot(fish.id, aes(fish.id$PosX, fish.id$PosY)) +
        geom_hex(bins = 55, alpha = 0.6) + scale_fill_gradientn(colours=plot.col, space = 'Lab', limits = c(0, pingmax), na.value = plot.col[length(plot.col)], name = 'No. pings') +
        annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, size = pen.size) +
        annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CNW', 'xmin'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CNW', 'ymax'], colour = pen.col, size = pen.size) +
        annotate('segment', x = locations.lookup['8CNW', 'xmin'], xend = locations.lookup['8CNE', 'xmax'], y = locations.lookup['8CNW', 'ymax'], yend = locations.lookup['8CNE', 'ymax'], colour = pen.col, size = pen.size) +
        annotate('segment', x = locations.lookup['8CNE', 'xmax'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CNE', 'ymax'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, size = pen.size) +
        annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CSW', 'ymax'], yend = locations.lookup['8CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) +
        annotate('segment', x = locations.lookup['8CSW', 'xmax'], xend = locations.lookup['8CNW', 'xmax'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) +
        annotate('segment', x = locations.lookup['8CNW', 'xmin'], xend = locations.lookup['8CNE', 'xmax'], y = locations.lookup['8CNW', 'ymin'], yend = locations.lookup['8CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) +
        annotate('segment', x = locations.lookup['8CNE', 'xmin'], xend = locations.lookup['8CSE', 'xmin'], y = locations.lookup['8CNE', 'ymax'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) +
        annotate('curve', x = locations.lookup['8WHSW', 'xmin']+1, xend = locations.lookup['8WHSW', 'xmax']-1, y = locations.lookup['8WHSW', 'ymin']+1, yend = locations.lookup['8WHSW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
        annotate('curve', x = locations.lookup['8WHSW', 'xmin']+1, xend = locations.lookup['8WHSW', 'xmax']-1, y = locations.lookup['8WHSW', 'ymin']+1, yend = locations.lookup['8WHSW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
        annotate('curve', x = locations.lookup['8WHNE', 'xmin']+1, xend = locations.lookup['8WHNE', 'xmax']-1, y = locations.lookup['8WHNE', 'ymin']+1, yend = locations.lookup['8WHNE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
        annotate('curve', x = locations.lookup['8WHNE', 'xmin']+1, xend = locations.lookup['8WHNE', 'xmax']-1, y = locations.lookup['8WHNE', 'ymin']+1, yend = locations.lookup['8WHNE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
        annotate('text', x = 69, y = 42, label = paste(as.character(i), 'h', sep = ' ')) + # hour count
        annotate('text', x = 69, y = 40, label = sun) + # Time of day
        theme(panel.background = element_rect(fill = 'white', colour = 'black')) +
        scale_x_continuous('x (m)', limits = c(35,70)) + scale_y_continuous('y (m)', limits = c(10,45))  
      
    }
    
    ggsave(name)
    #write.csv(fish.id, paste0(as.character(i), '.csv'))
    
    #dev.off()
    minseg <- maxseg
  }
  
  
  setwd(workingdir)
 })
}



# 31b. Create series of individual fish plots for animation

fishplot.anim <- function(pen, frames, framedur, animdur){
  
  system.time({ 
    dir.create(paste0(workingdir, '/animate'))
    setwd(paste0(workingdir, '/animate'))
    
    dayfile <- dayfile[order(dayfile$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
    
    pen.col <- 'black'
    pen.size <- 0.8
    #fish.cols <- brewer.pal(8, 'Dark2')  
    
    #pingmax <- as.integer((as.double(max(dayfile$EchoTime))-as.double(min(dayfile$EchoTime)))/(500*5))
    
    if(pen == 7){
      pen.group <- subset(dayfile, PEN == 7)
    } else {
      pen.group <- subset(dayfile, PEN == 8)
    }
    
    fish.codes <- unique(pen.group$Period)
    
    if(length(fish.codes) < 9){
      colours <- brewer.pal(length(fish.codes), 'Dark2')  
    } else {
      colours <- c(brewer.pal(8, 'Dark2'), brewer.pal(length(fish.codes)-8, 'Set1'))  
    }
    
    colours <- sort(colours)
    
    
    minseg <- pen.group[1,'EchoTime']#-seconds(1)
    
    fish.id <- data.frame(Period = double(), PEN = factor(), EchoTime = as.POSIXct(character()), PosX = double(), PosY = double(), PosZ = double(), BLSEC = double())
    
    if(pen.group[1, 'PEN'] == 7){
    
    fish.plot <- ggplot() + #fish.id, aes(fish.id$PosX, fish.id$PosY)) +
      scale_x_continuous('x (m)', limits = c(10, 45)) + scale_y_continuous('y (m)', limits = c(10,45)) + # set scale limits      
      theme(panel.background = element_rect(fill = 'white', colour = 'black')) + # white background, black lines
      #geom_point(fish.id, aes(fish.id$PosX, fish.id$PosY)) + #scale_fill_gradientn(colours=plot.col, space = 'Lab', limits = c(0, pingmax), na.value = plot.col[length(plot.col)], name = 'No. pings') +
      annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CNW', 'xmin'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CNW', 'ymax'], colour = pen.col, size = pen.size) +  # pen boundary
      annotate('segment', x = locations.lookup['7CNW', 'xmin'], xend = locations.lookup['7CNE', 'xmax'], y = locations.lookup['7CNW', 'ymax'], yend = locations.lookup['7CNE', 'ymax'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['7CNE', 'xmax'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CNE', 'ymax'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, size = pen.size) + # pen boundary
      annotate('segment', x = locations.lookup['7CSW', 'xmin'], xend = locations.lookup['7CSE', 'xmax'], y = locations.lookup['7CSW', 'ymax'], yend = locations.lookup['7CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['7CSW', 'xmax'], xend = locations.lookup['7CNW', 'xmax'], y = locations.lookup['7CSW', 'ymin'], yend = locations.lookup['7CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['7CNW', 'xmin'], xend = locations.lookup['7CNE', 'xmax'], y = locations.lookup['7CNW', 'ymin'], yend = locations.lookup['7CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('segment', x = locations.lookup['7CNE', 'xmin'], xend = locations.lookup['7CSE', 'xmin'], y = locations.lookup['7CNE', 'ymax'], yend = locations.lookup['7CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) + # pen location boundary
      annotate('curve', x = locations.lookup['7WHNW', 'xmin']+1, xend = locations.lookup['7WHNW', 'xmax']-1, y = locations.lookup['7WHNW', 'ymin']+1, yend = locations.lookup['7WHNW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
      annotate('curve', x = locations.lookup['7WHNW', 'xmin']+1, xend = locations.lookup['7WHNW', 'xmax']-1, y = locations.lookup['7WHNW', 'ymin']+1, yend = locations.lookup['7WHNW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
      annotate('curve', x = locations.lookup['7WHSE', 'xmin']+1, xend = locations.lookup['7WHSE', 'xmax']-1, y = locations.lookup['7WHSE', 'ymin']+1, yend = locations.lookup['7WHSE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
      annotate('curve', x = locations.lookup['7WHSE', 'xmin']+1, xend = locations.lookup['7WHSE', 'xmax']-1, y = locations.lookup['7WHSE', 'ymin']+1, yend = locations.lookup['7WHSE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
      theme(legend.position = 'none')
      #annotate('text', x = 42, y = 42, label = paste(as.character(i), 'h', sep = ' ')) + # hour count
      #annotate('text', x = 42, y = 40, label = sun) + # Time of day

    } else {
      
    fish.plot <- ggplot() + #fish.id, aes(fish.id$PosX, fish.id$PosY)) +
      scale_x_continuous('x (m)', limits = c(35,70)) + scale_y_continuous('y (m)', limits = c(10,45)) +      
      theme(panel.background = element_rect(fill = 'white', colour = 'black')) + # white background, black lines
      annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CNW', 'xmin'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CNW', 'ymax'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['8CNW', 'xmin'], xend = locations.lookup['8CNE', 'xmax'], y = locations.lookup['8CNW', 'ymax'], yend = locations.lookup['8CNE', 'ymax'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['8CNE', 'xmax'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CNE', 'ymax'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, size = pen.size) +
      annotate('segment', x = locations.lookup['8CSW', 'xmin'], xend = locations.lookup['8CSE', 'xmax'], y = locations.lookup['8CSW', 'ymax'], yend = locations.lookup['8CSE', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('segment', x = locations.lookup['8CSW', 'xmax'], xend = locations.lookup['8CNW', 'xmax'], y = locations.lookup['8CSW', 'ymin'], yend = locations.lookup['8CNW', 'ymax'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('segment', x = locations.lookup['8CNW', 'xmin'], xend = locations.lookup['8CNE', 'xmax'], y = locations.lookup['8CNW', 'ymin'], yend = locations.lookup['8CNE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('segment', x = locations.lookup['8CNE', 'xmin'], xend = locations.lookup['8CSE', 'xmin'], y = locations.lookup['8CNE', 'ymax'], yend = locations.lookup['8CSE', 'ymin'], colour = pen.col, linetype = 'dotted', size = pen.size) +
      annotate('curve', x = locations.lookup['8WHSW', 'xmin']+1, xend = locations.lookup['8WHSW', 'xmax']-1, y = locations.lookup['8WHSW', 'ymin']+1, yend = locations.lookup['8WHSW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
      annotate('curve', x = locations.lookup['8WHSW', 'xmin']+1, xend = locations.lookup['8WHSW', 'xmax']-1, y = locations.lookup['8WHSW', 'ymin']+1, yend = locations.lookup['8WHSW', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
      annotate('curve', x = locations.lookup['8WHNE', 'xmin']+1, xend = locations.lookup['8WHNE', 'xmax']-1, y = locations.lookup['8WHNE', 'ymin']+1, yend = locations.lookup['8WHNE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = 1) + # hide boundary
      annotate('curve', x = locations.lookup['8WHNE', 'xmin']+1, xend = locations.lookup['8WHNE', 'xmax']-1, y = locations.lookup['8WHNE', 'ymin']+1, yend = locations.lookup['8WHNE', 'ymax']-1, colour = pen.col, size = pen.size, curvature = -1) + # hide boundary
      theme(legend.position = 'none')
    #  annotate('text', x = 69, y = 42, label = paste(as.character(i), 'h', sep = ' ')) + # hour count
    #  annotate('text', x = 69, y = 40, label = sun) + # Time of day
    
    }
    
    
  #for(j in 1:length(fish.codes)){
  #  assign(as.character(paste0('fish_', fish.codes[[j]])), data.frame(Period = double(), PEN = factor(), EchoTime = as.POSIXct(character()), PosX = double(), PosY = double(), PosZ = double(), BLSEC = double())) 
  #}  
    
    
    for(i in 1:frames){
      
      # creating a name for each plot file with leading zeros
      if (i < 10) {name = paste('000',i,'plot.png',sep='')}
      
      if (i < 100 && i >= 10) {name = paste('00',i,'plot.png', sep='')}
      if (i >= 100) {name = paste('0', i,'plot.png', sep='')}
      
      # code to prepare dataset for each frame
      maxseg <- pen.group[1, 'EchoTime']+seconds(i*framedur)
      
      #for(k in 1:length(fish.codes)){
      
        #assign(as.character(paste0('fish_', fish.codes[[k]])), rbind(get(as.character(paste0('fish_', fish.codes[[k]]))), subset(pen.group, EchoTime >= minseg & EchoTime < maxseg & Period == as.character(fish.codes[[k]]), select=c(Period, PEN, EchoTime, PosX, PosY, PosZ, BLSEC))))
      
      
      if(animdur == 0){
        
        fish.id <- rbind(fish.id, subset(pen.group, EchoTime >= minseg & EchoTime < maxseg, select=c(Period, PEN, EchoTime, PosX, PosY, PosZ, BLSEC, SUN)))
        
        } else{
          
        fish.id <- rbind(fish.id, subset(pen.group, EchoTime >= minseg & EchoTime < maxseg, select=c(Period, PEN, EchoTime, PosX, PosY, PosZ, BLSEC, SUN)))  
        fish.id <- subset(fish.id, EchoTime >= minseg-seconds(framedur*animdur))
        
        }
      
      
        #saves the plot as a .png file in the working directory
        sun <- ifelse(fish.id[1, 'SUN'] == 'N', 'Night', ifelse(fish.id[1, 'SUN'] == 'W', 'Dawn', ifelse(fish.id[1, 'SUN'] == 'K', 'Dusk', ifelse(fish.id[1,'SUN'] == 'D', 'Day', sun))))
      
        #xinput <- paste0('fish_', as.character(fish.codes[[k]]), '$PosX')
        #yinput <- paste0('fish_', as.character(fish.codes[[k]]), '$PosY')
        #fish.id <- fish.id[order(fish.id$EchoTime, na.last = FALSE, decreasing = TRUE, method = c("shell")),] # reverse chronological order
        #chronord <- as.factor(fish.id$EchoTime)
      
      
        if(pen.group[1, 'PEN'] == 7){
        fish.plot + geom_point(data = fish.id, aes(x = PosX, y = PosY, colour = as.factor(Period), alpha = as.factor(EchoTime)), size = 2) + scale_fill_manual(values = fish.cols) + scale_alpha_manual(values = seq(0.1, 1, length.out = nrow(fish.id))) +
        annotate('text', x = 41, y = 45, label = max(fish.id$EchoTime)) + # time stamp
        # annotate('text', x = 41, y = 43, label = '100x')    
        annotate('text', x = 41, y = 43, label = sun) # day period
        }
        
        if(pen.group[1, 'PEN'] == 8){
        fish.plot + geom_point(data = fish.id, aes(x = PosX, y = PosY, colour = as.factor(Period), alpha = as.factor(EchoTime)), size = 2) + scale_fill_manual(values = fish.cols) + scale_alpha_manual(values = seq(0.1, 1, length.out = nrow(fish.id))) +
        annotate('text', x = 66, y = 45, label = max(fish.id$EchoTime)) + # time stamp
        annotate('text', x = 66, y = 43, label = sun) # day period  
          
        }
        
        
        
        #fish.plot + geom_point(aes(x = fish.id$PosX, y = fish.id$PosY, colour = factor(fish.id$Period)))  + scale_alpha_discrete(range = c(1, 0.2))
        #fish.plot <- fish.plot + geom_point(aes(x = eval(parse(text = xinput)), y = eval(parse(text = yinput)), colour = fish.cols[[1]]))
        
       
        
      #}
      
      #print(fish.plot)
      
      ggsave(name)

      minseg <- maxseg
    }
    
    
    setwd(workingdir)
  })
}



# 32. draw histogram of fish depth or activity from fish files

fish.hist <- function(pt){

if(pt == 'depth'){plot.type <- 'PosZ'}
if(pt == 'activity'){plot.type <- 'BLSEC'}  
  
    
files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE) 

if(length(files) < 13){
colours <- brewer.pal(length(files), 'Set3')  
} else {
colours <- c(brewer.pal(12, 'Set3'), brewer.pal(length(files)-12, 'Set1'))  
}

colours <- sort(colours)

fish.codes <- substr(files, 15, 18)

for(i in 1: length(files)) {
                                                                             
assign(paste0('dayfile', as.character(i)), read.csv(files[[i]], header = TRUE, sep = ",", colClasses = dayfile.classes)) #c('NULL', 'numeric', 'factor', 'factor', 'POSIXct', 'double', 'double', 
                                                                          #'double', 'double', 'double', 'double', 'double', 'double', 'factor',
                                                                          #'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
                                                                          #'double', 'double', 'double', 'double', 'double', 'double', 'double',
                                                                          #'double', 'double', 'double', 'double', 'double', 'double', 'double',
                                                                          #'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                                                                          #'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                                                                          #'factor', 'factor', 'double', 'double', 'double', 'double', 'double', 
                                                                          #'double', 'double', 'double', 'double', 'double', 'double', 'double'
                                                                          
#))) #read data into table
  
  if(pt == 'activity'){
assign(paste0('dayfile', as.character(i)), subset(get(paste0('dayfile', (i))), BLSEC < 5 & BLSEC >= 0 ))
    
  }
  
#assign('dayfile1', subset(dayfile1, BLSEC < 10))  
  
}



hdep <- ggplot()
ggprops <- data.frame(x = numeric(0), y = numeric(0), group = character(0))

for(j in 1: length(files)){

#hdep <- print(hdep + geom_freqpoly(data = get(paste0('dayfile', as.character(j))), binwidth = 0.3, aes(get(paste0('dayfile', as.character(j)))[,'PosZ'])))
loop_input = paste0('geom_freqpoly(data = dayfile', as.character(j), ', binwidth = 0.3, size = 1, aes(dayfile', as.character(j), '$', plot.type, ', color = colours[[', (j), ']]))')
hdep <- hdep + eval(parse(text = loop_input))
#ggdata <- ggplot_build(hdep + eval(parse(text = loop_input)))
#ggprops <- rbind(ggprops, cbind(ggdata$data[[1]]$x, ggdata$data[[1]]$y/sum(ggdata$data[[1]]$y), j)) # calculate proportions for counts
#ggprops <- rbind(ggprops, cbind(ggdata$data[[1]]$x, ggdata$data[[1]]$y, j))
}

# scale frequency outputs by factor
#colnames(ggprops) <- c('x', 'y', 'group')
#countsums <- aggregate(ggprops$y, by=list(Category=ggprops$group), FUN=sum)
#groupmax <- aggregate(ggprops$y, by = list(ggprops$group), max)
#groupmax$x <- max(ggprops$y)/groupmax$x
#colnames(groupmax) <- c('group', 'prop')
#ggprops <- left_join(ggprops, groupmax, by = 'group')
#ggprops$y <- round(ggprops$y*ggprops$prop, 0)
#ggprops$prop <- NULL


#ggprops$group <- as.factor(ggprops$group)
#hdep <- hdep + geom_line(aes(ggprops$x, ggprops$y, group = ggprops$group, colour = ggprops$group))

hdep <- hdep + theme(panel.background = element_rect(fill = 'white', colour = 'black'))
hdep <- hdep + scale_colour_manual('Fish ID', labels = fish.codes, values = colours)

if(pt == 'depth'){
  hdep <- hdep + labs(x = 'Depth (m)', colour = 'fish ID') + scale_y_continuous(limits = c(0, 40000))
  hdep <- hdep + coord_flip() + scale_x_reverse()
}
if(pt == 'activity'){
  hdep <- hdep + labs(x = 'Activity (BL/s)', colour = 'fish ID') + 
    #scale_y_log10(limits = c(0.001, 10))
    scale_x_log10(limits = c(0.001, 10), breaks = c(0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008, 0.009, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels = c(0.001, '', '', '', '', '', '', '', '', 0.01, '', '', '', '', '', '', '', '', 0.1, '', '', '', '', '', '', '', '', 1, '', '', '', '', '', '', '', '', 10))

    }

print(hdep)
hdep <<- hdep


}


# 33. Load all data into single data frame

load.all <- function(){

files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  
dayfile <- data.frame()

for(i in 1:length(files)){

  #daytemp <- read.csv(files[[i]], header = TRUE, sep = ",", colClasses = dayfile.classes) #read data into table
  daytemp <- fread(files[[i]])
  
  dayfile <- rbind(dayfile, daytemp)

}

dayfile$EchoTime <- as.POSIXct(dayfile$EchoTime)
dayfile$V1 <- NULL

#SORT BY TIME AND TAG
#dayfile <- dayfile[order(dayfile$EchoTime, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by time
#dayfile <- dayfile[order(dayfile$Period, na.last = FALSE, decreasing = FALSE, method = c("shell")),] # sort by tag

dayfile <<- dayfile

}


#34. Crop edges of dataset to remove multipath

crop <- function(xmin = 30, xmax = 64, ymin = 7, ymax = 42){
  
  dayfile <- subset(dayfile, dayfile$PosY > ymin & dayfile$PosY < ymax & dayfile$PosX > xmin & dayfile$PosX < xmax)
  
  dayfile <<- dayfile
  
}



#35. Save loaded dayfile to .csv file of original name

save <- function(){
  
  write.csv(dayfile, file = dayfile.loc) #write output to file
  
}


#36. calculate distance travelled for each fish in dayfile

distance <- function(){
  
  fish.codes <- unique(dayfile$Period) 
  
  total.dist <- as.data.frame(setNames(replicate(2, numeric(0), simplify = F), c('Fish_ID', 'distance_m')))
  
  for (i in 1:length(fish.codes)){
    
    total.dist[i,] <- c(fish.codes[i], round(sum(dayfile[dayfile$Period == fish.codes[[i]],]$M), 1))
    
  }
  total.dist$distance_m <- as.double(total.dist$distance_m)
  ggplot(total.dist, aes(ID, distance_m)) + geom_bar(stat = 'identity') + scale_x_discrete('fish ID') + scale_y_continuous('distance (m)')
  total.dist
  
}

#37. calculate distance travelled in multiple fish files

batch.dist <- function(){
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  total.dist <- as.data.frame(setNames(replicate(2, numeric(0), simplify = F), c('Fish_ID', 'distance_km')))
  
  for(i in 1:length(files)){
    
    dayfile <- read.csv(files[[i]], header = TRUE, sep = ",", colClasses = c('NULL', 'numeric', 'factor', 'factor', 'POSIXct', 'double', 'double', 
                                                                             'double', 'double', 'double', 'double', 'double', 'double', 'factor',
                                                                             'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
                                                                             'double', 'double', 'double', 'double', 'double', 'double', 'double',
                                                                             'double', 'double', 'double', 'double', 'double', 'double', 'double',
                                                                             'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                                                                             'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                                                                             'factor', 'factor', 'double', 'double', 'double', 'double', 'double', 
                                                                             'double', 'double', 'double', 'double', 'double', 'double', 'double'
                                                                             
    )) #read data into table
    
    fish.codes <- unique(dayfile$Period) 
    total.dist[i,] <- c(fish.codes[1], (round(sum(dayfile[dayfile$Period == fish.codes[[1]],]$M), 2)/1000))
    
    
  }
  
  total.dist$distance_km <- as.double(total.dist$distance_km)
  total.dist$Fish_ID <- as.character(total.dist$Fish_ID)
  distplot <- ggplot(total.dist, aes(Fish_ID, distance_km)) + geom_bar(stat = 'identity') + scale_x_discrete('fish ID') + scale_y_continuous('distance (km)', expand = c(0, 0))
  total.dist  <<- total.dist
  print(distplot)
  return(distplot)
  #distplot <<- distplot
  
  
}


# 38. Load dayfile

load.dayfile <- function(filename){
  
  setwd(workingdir)  
  dayfile <- read.csv(filename, header = TRUE, sep = ",", colClasses = dayfile.classes)  
  
  dayfile <<- dayfile
  
  
}  


# 39. Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# 40. Polar plots of headings

headplot <- function(threshold = 0.1){
  
  p7 <- subset(dayfile, PEN == 7 & MSEC >= threshold)
  p8 <- subset(dayfile, PEN == 8 & MSEC >= threshold)
  
  pplot7 <- ggplot(p7, aes(HEAD))
  pplot7 <- pplot7 + geom_histogram(breaks = seq(0, 360, 10), color = 'black', alpha = 0, size = 0.75, closed = 'left') + 
    theme_minimal() + theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
    scale_x_continuous('', limits = c(0, 360), expand = c(0, 0), breaks = c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330)) +
    #scale_y_continuous(limits = c(0, 1500)) +
    coord_polar(theta = 'x', start = 0) +
    ggtitle('Acclimated wrasse') + theme(plot.title = element_text(hjust = 0.5))
  
  pplot8 <- ggplot(p8, aes(HEAD))
  pplot8 <- pplot8 + geom_histogram(breaks = seq(0, 360, 10), color = 'black', alpha = 0, size = 0.75) + 
    theme_minimal() + theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
    scale_x_continuous('', limits = c(0, 360), breaks = c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330)) +
    # scale_y_continuous(limits = c(0, 1500)) +
    coord_polar(theta = 'x', start = 0) +
    ggtitle('Non-acclimated wrasse') + theme(plot.title = element_text(hjust = 0.5))
  
  multiplot(pplot7, pplot8, cols = 2)
  
}


# 41. Polar plots of turn rates

turnplot <- function(){
  
  p7 <- subset(dayfile, PEN == 7)
  p7$TURNRATE <- p7$TURN/p7$SEC
  p8 <- subset(dayfile, PEN == 8)
  p8$TURNRATE <- p8$TURN/p8$SEC
  
  pplot7 <- ggplot(p7, aes(TURNRATE))
  pplot7 <- pplot7 + geom_histogram(breaks = seq(0, 30, 1), color = 'black', alpha = 0, size = 0.75, closed = 'left') + 
    theme_minimal() + theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
    scale_x_continuous('', limits = c(0, 30), expand = c(0, 0), breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 330)) +
    #scale_y_continuous(limits = c(0, 1500)) +
    coord_polar(theta = 'x', start = 0) +
    ggtitle('Wild wrasse') + theme(plot.title = element_text(hjust = 0.5))
  
  pplot8 <- ggplot(p8, aes(TURNRATE))
  pplot8 <- pplot8 + geom_histogram(breaks = seq(0, 30, 1), color = 'black', alpha = 0, size = 0.75) + 
    theme_minimal() + theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
    scale_x_continuous('', limits = c(0, 30), expand = c(0, 0), breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 330)) +
    # scale_y_continuous(limits = c(0, 1500)) +
    coord_polar(theta = 'x', start = 0) +
    ggtitle('Farmed wrasse') + theme(plot.title = element_text(hjust = 0.5))
  
  multiplot(pplot7, pplot8, cols = 2)
  
}



# 42.  draw turn / velocity plots for every step specified

bplot <- function(period, step = 100){
  
  daytemp <- subset(dayfile, Period == period)
  start <- step-step
  end <- step
  
  f5 <- rep(1/5, 5) # 5 step moving average filter
  f10 <- rep(1/10, 10) # 5 step moving average filter
  
  for (i in 1:floor(nrow(daytemp)/step)){
    
    sect <- daytemp[start:end,] # subset dayfile
    
    #par(mfrow=c(2,2))
    layout(matrix(c(1,2,3,3), 2, 2,byrow = T))
    par(new=F)
    par(mar = c(4, 4, 4, 4))# + 0.1)
    fishpal <- rainbow_hcl(20, c=100, l=63, start=-360, end=-32, alpha = 0.2)
    fish.id <- subset(dayfile, Period == period)
    
    # position plot
    
    if(fish.id[1,3] == '7')
    {
      
      plot(sect$PosX, sect$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(10, 45), ylim = c(10, 45), type = 'l', col = '#26b426') # tight plot
      rect(locations.lookup['7EW', 'xmin'], locations.lookup['7EW', 'ymin'], locations.lookup['7EW', 'xmax'], locations.lookup['7EW', 'ymax'], lty = 2) # 7EW edge
      rect(locations.lookup['7ES', 'xmin'], locations.lookup['7ES', 'ymin'], locations.lookup['7ES', 'xmax'], locations.lookup['7ES', 'ymax'], lty = 2) # 7ES edge
      rect(locations.lookup['7EE', 'xmin'], locations.lookup['7EE', 'ymin'], locations.lookup['7EE', 'xmax'], locations.lookup['7EE', 'ymax'], lty = 2) # 7EE edge
      rect(locations.lookup['7EN', 'xmin'], locations.lookup['7EN', 'ymin'], locations.lookup['7EN', 'xmax'], locations.lookup['7EN', 'ymax'], lty = 2) # 7EN edge
      rect(locations.lookup['7WHSE', 'xmin'], locations.lookup['7WHSE', 'ymin'], locations.lookup['7WHSE', 'xmax'], locations.lookup['7WHSE', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHSE
      rect(locations.lookup['7WHNW', 'xmin'], locations.lookup['7WHNW', 'ymin'], locations.lookup['7WHNW', 'xmax'], locations.lookup['7WHNW', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHNW
      rect(locations.lookup['7FBSE', 'xmin'], locations.lookup['7FBSE', 'ymin'], locations.lookup['7FBSE', 'xmax'], locations.lookup['7FBSE', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 7FBSE
      rect(locations.lookup['7FBNW', 'xmin'], locations.lookup['7FBNW', 'ymin'], locations.lookup['7FBNW', 'xmax'], locations.lookup['7FBNW', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 7FBNW
      rect(locations.lookup['7EW', 'xmin'], locations.lookup['7ES', 'ymin'], locations.lookup['7EE', 'xmax'], locations.lookup['7EN', 'ymax'], lwd = 2) # cage limits
  
      text(10, 45, adj = c(0, 1), label = paste0('Salmon feeding: ', sect[1,'SMEAL8'], '\nBiofouling: ', sect[1, 'BIOF8'], '\nSun: ', sect[1,'SUN'], '\nTide: ', sect[1, 'TID']), cex = 1) 
      text(45, 45, adj = c(1, 1), label = paste0(sect[1, 'EchoTime'], ' to ', sect[nrow(sect), 'EchoTime'], '\n', start, ' - ', end))
      
    }else{
      
      plot(sect$PosX, sect$PosY, xlab = 'X (m)', ylab = 'Y (m)', pch = 20, cex = 1, xlim = c(37, 72), ylim = c(10, 45), type = 'l', col = '#26b426') # tight plot
      rect(locations.lookup['8EW', 'xmin'], locations.lookup['8EW', 'ymin'], locations.lookup['8EW', 'xmax'], locations.lookup['8EW', 'ymax'], lty = 2) # 7EW edge
      rect(locations.lookup['8ES', 'xmin'], locations.lookup['8ES', 'ymin'], locations.lookup['8ES', 'xmax'], locations.lookup['8ES', 'ymax'], lty = 2) # 7ES edge
      rect(locations.lookup['8EE', 'xmin'], locations.lookup['8EE', 'ymin'], locations.lookup['8EE', 'xmax'], locations.lookup['8EE', 'ymax'], lty = 2) # 7EE edge
      rect(locations.lookup['8EN', 'xmin'], locations.lookup['8EN', 'ymin'], locations.lookup['8EN', 'xmax'], locations.lookup['8EN', 'ymax'], lty = 2) # 7EN edge
      rect(locations.lookup['8WHSW', 'xmin'], locations.lookup['8WHSW', 'ymin'], locations.lookup['8WHSW', 'xmax'], locations.lookup['8WHSW', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHSE
      rect(locations.lookup['8WHNE', 'xmin'], locations.lookup['8WHNE', 'ymin'], locations.lookup['8WHNE', 'xmax'], locations.lookup['8WHNE', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHNW
      rect(locations.lookup['8EW', 'xmin'], locations.lookup['8ES', 'ymin'], locations.lookup['8EE', 'xmax'], locations.lookup['8EN', 'ymax'], lwd = 2) # cage limits
      
      text(37, 45, adj = c(0, 1), label = paste0('Salmon feeding: ', sect[1,'SMEAL8'], '\nBiofouling: ', sect[1, 'BIOF8'], '\nSun: ', sect[1,'SUN'], '\nTide: ', sect[1, 'TID']), cex = 1) 
      text(73, 45, adj = c(1, 1), label = paste0(sect[1, 'EchoTime'], ' to ', sect[nrow(sect), 'EchoTime'], '\n', start, ' - ', end))
      
    }
    
    #depth plot
    
    plot(sect$EchoTime, sect$PosZ, xlab = 'Time', ylab = 'Depth (m)', ylim = c(35, 0), type = 'l', col = '#26b426')
    segments(sect[1,4], 15, sect[nrow(fish.id), 4], 15, lty = 2)
    legend('bottomleft', as.character(period), col = '#26b426', pch = 20, bty = 'n', pt.cex = 1.5, horiz = TRUE, y.intersp = 0)
    
    #turn/velocity plots
    
    par(mar = c(4, 6, 2, 4))# + 0.1)
    plot(sect$EchoTime, sect$TURN, xlab = 'Time', type = 'l', lwd = 2, col = 'lightgreen', ylab = '', yaxt = 'n', ylim = c(0, 180)) # plot turn
    axis(2, ylim = c(0, 180), at = c(0, 30, 60, 90, 120, 150, 180), labels = c('0', '30', '60', '90', '120', '150', '180'))
    #turnlag <- filter(sect$TURN, f5, sides=1) # filter turn
    #lines(sect$EchoTime, turnlag, col = 'darkgreen') # add moving average to plot
    
    #par(new = T)
    #plot(sect$EchoTime, sect$rollturn, xlab = 'Time', type = 'l', lwd = 2, col = 'lightblue', ylab = '', yaxt = 'n', ylim = c(0, 1500)) # plot turn
    #axis(4, ylim = c(0, 1500), at = c(0, 500, 1000, 1500), labels = c('0', '500', '1000', '1500'))
    #mtext(text = 'x10 rolling average of turn', side = 4, line = 2.5)
    
    par(new = T)
    #plot(sect$EchoTime, sect$HEAD, xlab = 'Time', type = 'l', lwd = 2, col = 'lightblue', ylab = '', yaxt = 'n', ylim = c(0, 360)) # plot turn
    axis(2, ylim = c(0, 360), line = 2, at = c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330, 360), labels = c('0', '30', '60', '90', '120', '150', '180', '210', '240', '270', '300', '330', '360'))
    mtext(2, text = 'Turn/heading (degrees)', line = 4.5)
    
    par(new = T)
    plot(sect$EchoTime, sect$MSEC, col = 'green', axes = F, xlab = '', ylab = '', type = 'l', lwd = 2, ylim = c(0, 0.8))
    axis(4, ylim = c(0, 1), line = 2, at = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8), labels = c('0', '0.1', '0.2', '0.3', '0.4', '0.5', '0.6', '0.7', '0.8'))
    #mtext(text = 'velocity (m/sec)', side = 4, line = 3.5)
    legend('topleft', legend = c('Turn', 'Velocity', 'Heading'), lty = 1, lwd = 2, col = c('lightgreen', 'red', 'lightblue'), horiz = T)
    vellag <- filter(sect$MSEC, f5, sides=1) # filter turn
    lines(sect$EchoTime, vellag, col = 'darkgreen') # add moving average to plot
    
    par(new = T)
    plot(sect$EchoTime, sect$BLSEC, col = 'red', axes = F, xlab = '', ylab = '', type = 'l', lwd = 2, ylim = c(0, 6))
    axis(4, ylim = c(0, 6), at = c(0, 1, 2, 3, 4, 5, 6), labels = c('0', '1', '2', '3', '4', '5', '6'))
    #mtext(text = 'velocity (BL/sec)', side = 4, line = 2.5)
    vellag <- filter(sect$BLSEC, f5, sides=1) # filter turn
    lines(sect$EchoTime, vellag, col = 'pink') # add moving average to plot
    
    legend('topleft', legend = c('Turn', 'Velocity', 'Heading'), lty = 1, lwd = 2, col = c('lightgreen', 'red', 'lightblue'), horiz = T)

    par(new = F)
    
    start <- start+step
    end <- end+step
    
    readline(prompt = 'Press [enter] to continue')
    
  }
  par(mfrow=c(1,1))
}


# 43. Perform behaviour calculations for loaded dayfile and add to dayfile

bcalc <- function(){
  
  #calculate difference in turn and 10 width rolling sum of turn
  dayfile$turndiff <- c(NA, abs(diff(dayfile$TURN, lag = 1)))
  dayfile$rollturnsumpersec <- c(rep(NA,4), rollapply(dayfile$turndiff, width = 10, FUN = sum, na.rm = T, align = 'center')/rollapply(dayfile$SEC, width = 10, FUN = sum, na.rm = T, align = 'center'), rep(NA, 5))
  
  # Displacement code
  
  # calculate rolling mean of x,y,z coords over 20 points
  dayfile$rollx <- c(rep(NA,19), rollapply(dayfile$PosX, width = 20, FUN = mean, na.rm = T, align = 'right'))#, rep(NA, 10))
  dayfile$rolly <- c(rep(NA,19), rollapply(dayfile$PosY, width = 20, FUN = mean, na.rm = T, align = 'right'))#, rep(NA, 10))
  dayfile$rollz <- c(rep(NA,19), rollapply(dayfile$PosZ, width = 20, FUN = mean, na.rm = T, align = 'right'))#, rep(NA, 10))
  
  # calculate rolling sum of time between pings over 20 points
  dayfile$rollsec <- c(rep(NA,19), rollapply(dayfile$SEC, width = 20, FUN = sum, na.rm = T, align = 'right'))#, rep(NA, 10))
  
  #calculate displacement
  dayfile$displace <- round(sqrt(abs(dayfile$PosX-dayfile$rollx)^2+abs(dayfile$PosY-dayfile$rolly)^2+abs(dayfile$PosZ-dayfile$rollz)^2)/dayfile$rollsec, digits = 3)
  
  # calculate rolling mean of velocity/sec
  dayfile$rollvel <- c(rep(NA,9), rollapply(dayfile$M, width = 10, FUN = sum, na.rm = T, align = 'right')/rollapply(dayfile$SEC, width = 10, FUN = sum, na.rm = T, align = 'center'))
  
  # calculate instantanous acceleration
  dayfile$acc <- c(NA, abs(diff(dayfile$MSEC, lag = 1)))
  
  #calculate acceleration mean over 10 points
  dayfile$accmean <- c(rep(NA,4), rollapply(dayfile$acc, width = 10, FUN = mean, na.rm = T, align = 'center'), rep(NA, 5)) # acceleration mean over 10 points
  
  dayfile <<- dayfile
  
}


# 44. calculate behaviour state for all dayfiles in working directory and save to dayfiles


batch.bscalc <- function(){
  
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  
  for(i in 1:length(files)){
    
    #day <- substr(files[[i]], 15, 17)
    dayfile <- read.csv(files[[i]], header = TRUE, sep = ",", colClasses = dayfile.classes)  
    
    #calculate difference in turn and 10 width rolling sum of turn
    dayfile$turndiff <- c(NA, abs(diff(dayfile$TURN, lag = 1)))
    dayfile$rollturnsumpersec <- c(rep(NA,4), rollapply(dayfile$turndiff, width = 10, FUN = sum, na.rm = T, align = 'center')/rollapply(dayfile$SEC, width = 10, FUN = sum, na.rm = T, align = 'center'), rep(NA, 5))
    
    # calculate rolling mean of x,y,z coords over 20 points
    dayfile$rollx <- c(rep(NA,19), rollapply(dayfile$PosX, width = 20, FUN = mean, na.rm = T, align = 'right'))#, rep(NA, 10))
    dayfile$rolly <- c(rep(NA,19), rollapply(dayfile$PosY, width = 20, FUN = mean, na.rm = T, align = 'right'))#, rep(NA, 10))
    dayfile$rollz <- c(rep(NA,19), rollapply(dayfile$PosZ, width = 20, FUN = mean, na.rm = T, align = 'right'))#, rep(NA, 10))
    
    # calculate rolling sum of time between pings over 20 points
    dayfile$rollsec <- c(rep(NA,19), rollapply(dayfile$SEC, width = 20, FUN = sum, na.rm = T, align = 'right'))#, rep(NA, 10))
    
    #calculate displacement
    dayfile$displace <- round(sqrt(abs(dayfile$PosX-dayfile$rollx)^2+abs(dayfile$PosY-dayfile$rolly)^2+abs(dayfile$PosZ-dayfile$rollz)^2)/dayfile$rollsec, digits = 3)
    
    # calculate rolling mean of velocity/sec
    #dayfile$rollvel <- c(rep(NA,9), rollapply(dayfile$M, width = 10, FUN = mean, na.rm = T, align = 'right')/rollapply(dayfile$SEC, width = 10, FUN = mean, na.rm = T, align = 'right'))
    
    # calculate rolling mean of BL/sec
    dayfile$rollvel <- c(rep(NA,9), (rollapply(dayfile$M, width = 10, FUN = mean, na.rm = T, align = 'right')/rollapply(dayfile$SEC, width = 10, FUN = mean, na.rm = T, align = 'right'))/rollapply(dayfile$BL, width = 10, FUN = mean, na.rm = T, align = 'right'))
    
    
    # calculate instantanous acceleration
    #dayfile$acc <- c(NA, abs(diff(dayfile$MSEC, lag = 1)))
    
    #calculate acceleration mean over 10 points
    #dayfile$accmean <- c(rep(NA,4), rollapply(dayfile$acc, width = 10, FUN = mean, na.rm = T, align = 'center'), rep(NA, 5)) # acceleration mean over 10 points
    
    # code behaviour state for each position    
    #dayfile$BS <- ifelse(dayfile$displace <= 0.015, ifelse(dayfile$rollvel <= 0.02, 'Rr', 'Ra'), ifelse(dayfile$accmean <= 0.05, 'C', ifelse(dayfile$rollvel <= 0.1, 'F', 'A')))
    
    #alternative behaviour state coding
    #dayfile$BS <- ifelse(dayfile$displace <= 0.015, ifelse(dayfile$rollvel <= 0.02, 'Rr', 'Ra'), ifelse(dayfile$rollturnsumpersec <= 4, 'C', ifelse(dayfile$rollvel <= 0.1, 'F', 'A')))
    
    # another alternative behaviour state coding
    #dayfile$BS <- ifelse(dayfile$displace <= 0.015, ifelse(dayfile$rollvel <= 0.02, 'Rr', ifelse(dayfile$rollvel >0.02 & dayfile$rollvel <=0.1, 'Rf', 'Ra')), ifelse(dayfile$rollturnsumpersec <= 4, 'Ep', ifelse(dayfile$rollvel <= 0.1, 'Ef', 'Ea')))
    
    # alternative behaviour state coding based on BL/SEC
    dayfile$BS <- ifelse(dayfile$displace <= 0.015, ifelse(dayfile$rollvel <= 0.15, 'Rr', ifelse(dayfile$rollvel >0.15 & dayfile$rollvel <=0.8, 'Rf', 'Ra')), ifelse(dayfile$rollturnsumpersec <= 4, 'Ep', ifelse(dayfile$rollvel <= 0.8, 'Ef', 'Ea')))
    
    
    dayfile$turndiff <- NULL
    dayfile$rollturnsumpersec <- NULL
    dayfile$rollx <- NULL
    dayfile$rolly <- NULL
    dayfile$rollz <- NULL
    dayfile$rollsec <- NULL
    dayfile$displace <- NULL
    dayfile$rollvel <- NULL
    dayfile$acc <- NULL
    dayfile$accmean <- NULL
    
    write.csv(dayfile, file = files[[i]]) #write output to file
    
  }
  
}



# 45. batch.bsprop() = calculate proportions of behaviour states for each dayfile in working directory

batch.bsprop <- function(){
  
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  bsproptab <- data.frame(c('Ea', 'Ef', 'Ep', 'Ra', 'Rf', 'Rr'))
  colnames(bsproptab) <- 'ID'
  
  for(i in 1:length(files)){
    
    dayfile <- read.csv(files[[i]], header = TRUE, sep = ",", colClasses = dayfile.classes)    
    
    #bstab <- table(dayfile$BS)
    daysub <- subset(dayfile, SEC < 600) # keep only records where gap between signals is less than 10 mins (600s)
    bstab <- aggregate(x = daysub$SEC, by = list(daysub$BS), FUN = 'sum', na.rm = T)
    
    bsproptab[,as.character(i)] <- as.vector(bstab$x)
    
  } 
  
  write.xlsx(bsproptab, 'bsproportions.xlsx')
  
}


# 46. Calculate kernel distribution utilisation for single fish file

kudcalc <- function(){

#kudcols <- c(brewer.pal(4, 'Accent')[[1]], brewer.pal(4, 'Accent')[[2]]) # create colour palette for KUDs
kudcols <- terrain.colors(4, alpha = 0.6)

if(unique(dayfile$PEN == 7)){
  
  x <- seq(0, 50, by = 0.5)
  y <- seq(0, 50, by = 0.5)
  xy <- expand.grid(x=x, y=y)
  coordinates(xy) <- ~x+y
  gridded(xy) <- TRUE
  class(xy)

} else {
  
  x <- seq(25, 70, by = 0.5)
  y <- seq(0, 50, by = 0.5)
  xy <- expand.grid(x=x, y=y)
  coordinates(xy) <- ~x+y
  gridded(xy) <- TRUE
  class(xy)  
  
  
}

coords <- dayfile[,c(1, 5, 6)] # extract x,y coords and fish id from dayfile
coordinates(coords) <- c('PosX', 'PosY') # convert to spatial points data frame object
ud <- kernelUD(coords, h = 'href', grid = xy, kern = 'bivnorm') # KUD calculation for adehabitatHR package

#mcp100 <- mcp(coords, percent = 100)
ver50 <- getverticeshr(ud, 50) # extract 50% vertex for plotting
ver95 <- getverticeshr(ud, 95) # extract 95% vertex for plotting
#plot(mcp100, col = NULL, axes = T, xlim = c(10, 45), ylim = c(0, 45)) # plot MCP100
ka <- kernel.area(ud, percent = c(50, 95), unin = 'm', unout = 'm2') # calculates area of KUD50, KUD95 and MCP100

if (unique(dayfile$PEN == 7)){

  plot(ver95, col = kudcols[[1]], axes = T, xlim = c(10, 45), ylim = c(10, 45), xlab = 'x (m)', ylab = 'y (m)') # plot KUD95
  plot(ver50, col = kudcols[[4]], axes = F, xlim = c(10, 45), ylim = c(10, 45), add=T) # plot KUD50

  rect(locations.lookup['7EW', 'xmin'], locations.lookup['7EW', 'ymin'], locations.lookup['7EW', 'xmax'], locations.lookup['7EW', 'ymax'], lty = 2) # 7EW edge
  rect(locations.lookup['7ES', 'xmin'], locations.lookup['7ES', 'ymin'], locations.lookup['7ES', 'xmax'], locations.lookup['7ES', 'ymax'], lty = 2) # 7ES edge
  rect(locations.lookup['7EE', 'xmin'], locations.lookup['7EE', 'ymin'], locations.lookup['7EE', 'xmax'], locations.lookup['7EE', 'ymax'], lty = 2) # 7EE edge
  rect(locations.lookup['7EN', 'xmin'], locations.lookup['7EN', 'ymin'], locations.lookup['7EN', 'xmax'], locations.lookup['7EN', 'ymax'], lty = 2) # 7EN edge
  rect(locations.lookup['7WHSE', 'xmin'], locations.lookup['7WHSE', 'ymin'], locations.lookup['7WHSE', 'xmax'], locations.lookup['7WHSE', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHSE
  rect(locations.lookup['7WHNW', 'xmin'], locations.lookup['7WHNW', 'ymin'], locations.lookup['7WHNW', 'xmax'], locations.lookup['7WHNW', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHNW
  rect(locations.lookup['7FBSE', 'xmin'], locations.lookup['7FBSE', 'ymin'], locations.lookup['7FBSE', 'xmax'], locations.lookup['7FBSE', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 7FBSE
  rect(locations.lookup['7FBNW', 'xmin'], locations.lookup['7FBNW', 'ymin'], locations.lookup['7FBNW', 'xmax'], locations.lookup['7FBNW', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 7FBNW
  rect(locations.lookup['7EW', 'xmin'], locations.lookup['7ES', 'ymin'], locations.lookup['7EE', 'xmax'], locations.lookup['7EN', 'ymax'], lwd = 2) # cage limits
  text(6, 44, labels = bquote(paste(KUD[50], ' = ', .(ka[1,1]), m^2)), adj = c(0,0))
  text(6, 42.5, labels = bquote(paste(KUD[95], ' = ', .(ka[2,1]), m^2)), adj = c(0,0))
  

} else{
  
  plot(ver95, col = kudcols[[1]], axes = T, xlim = c(40, 65), ylim = c(10, 45), xlab = 'x (m)', ylab = 'y (m)') # plot KUD95
  plot(ver50, col = kudcols[[4]], axes = F, xlim = c(40, 65), ylim = c(10, 45), add=T) # plot KUD50
  
  rect(locations.lookup['8EW', 'xmin'], locations.lookup['8EW', 'ymin'], locations.lookup['8EW', 'xmax'], locations.lookup['8EW', 'ymax'], lty = 2) # 7EW edge
  rect(locations.lookup['8ES', 'xmin'], locations.lookup['8ES', 'ymin'], locations.lookup['8ES', 'xmax'], locations.lookup['8ES', 'ymax'], lty = 2) # 7ES edge
  rect(locations.lookup['8EE', 'xmin'], locations.lookup['8EE', 'ymin'], locations.lookup['8EE', 'xmax'], locations.lookup['8EE', 'ymax'], lty = 2) # 7EE edge
  rect(locations.lookup['8EN', 'xmin'], locations.lookup['8EN', 'ymin'], locations.lookup['8EN', 'xmax'], locations.lookup['8EN', 'ymax'], lty = 2) # 7EN edge
  rect(locations.lookup['8WHSW', 'xmin'], locations.lookup['8WHSW', 'ymin'], locations.lookup['8WHSW', 'xmax'], locations.lookup['8WHSW', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHSE
  rect(locations.lookup['8WHNE', 'xmin'], locations.lookup['8WHNE', 'ymin'], locations.lookup['8WHNE', 'xmax'], locations.lookup['8WHNE', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHNW
  rect(locations.lookup['8FBNE', 'xmin'], locations.lookup['8FBNE', 'ymin'], locations.lookup['8FBNE', 'xmax'], locations.lookup['8FBNE', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 7FBNE
  rect(locations.lookup['8FBSW', 'xmin'], locations.lookup['8FBSW', 'ymin'], locations.lookup['8FBSW', 'xmax'], locations.lookup['8FBSW', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 7FBSW
  rect(locations.lookup['8EW', 'xmin'], locations.lookup['8ES', 'ymin'], locations.lookup['8EE', 'xmax'], locations.lookup['8EN', 'ymax'], lwd = 2) # cage limits
  text(31, 44, labels = bquote(paste(KUD[50], ' = ', .(ka[1,1]), m^2)), adj = c(0,0))
  text(31, 42.5, labels = bquote(paste(KUD[95], ' = ', .(ka[2,1]), m^2)), adj = c(0,0))
  
}


}


# 47. Calculate 3d kernel distribution utilisation for single fish file as 1m z-stack and save plots to file

kudcalc3d <- function(save = T){
  
  kudcols <- terrain.colors(4, alpha = 0.6)
  
  if(unique(dayfile$PEN == 7)){
    
    x <- seq(0, 50, by = 0.5)
    y <- seq(0, 50, by = 0.5)
    xy <- expand.grid(x=x, y=y)
    coordinates(xy) <- ~x+y
    gridded(xy) <- TRUE
    class(xy)
    
  } else {
    
    x <- seq(20, 75, by = 0.5)
    y <- seq(0, 55, by = 0.5)
    xy <- expand.grid(x=x, y=y)
    coordinates(xy) <- ~x+y
    gridded(xy) <- TRUE
    class(xy)  
    
  }
 
  max.depth <- ceiling(max(dayfile$PosZ))
  #k50 <- 0
  #k95 <- 0
  
  for (j in 1:max.depth){
    
    depthtemp <- subset(dayfile, PosZ > j-1 & PosZ < j)
    
    if (nrow(depthtemp) >50){
      
      coords <- depthtemp[,c(1, 5, 6)] # extract x,y coords and fish id from dayfile
      coordinates(coords) <- c('PosX', 'PosY') # convert to spatial points data frame object
      ud <- kernelUD(coords, h = 'href', grid = xy, kern = 'bivnorm') # KUD calculation for adehabitatHR package
      
      #kcont50 <- ceiling(((nrow(depthtemp)/nrow(dayfile))*0.5)*100)
      #kcont95 <- ifelse(((nrow(depthtemp)/nrow(dayfile))*0.95)*100 >3, ceiling(((nrow(depthtemp)/nrow(dayfile))*0.95)*100), 3)
      
      #if (kcont50 > 3){
      
      ver50 <- getverticeshr(ud, 50) # extract 50% vertex for plotting
      ver95 <- getverticeshr(ud, 95) # extract 95% vertex for plotting
      
      ka <- kernel.area(ud, percent = c(50, 95), unin = 'm', unout = 'm2') # calculates area of KUD50, KUD95
      
      #} else {
       
      #ver95 <- getverticeshr(ud, kcont95) # extract 95% vertex for plotting   
      #ka <- kernel.area(ud, percent = 95, unin = 'm', unout = 'm2') # calculates area of KUD95
      
      #}
      
      if (unique(dayfile$PEN == 7)){
        
        jpeg(file = paste0('kudplot_', unique(dayfile$Period), '_', j, '.jpg'))
        plot(ver95, col = kudcols[[1]], axes = T, xlim = c(10, 45), ylim = c(10, 45), xlab = 'x (m)', ylab = 'y (m)') # plot KUD95
        if (kcont50 >3) { plot(ver50, col = kudcols[[4]], axes = F, xlim = c(10, 45), ylim = c(10, 45), add=T) } # plot KUD50
        plot.pen(7)
        text(7, 44, labels = bquote(paste(KUD[50], ' = ', .(ka[1,1]), m^2)), adj = c(0,0))
        text(7, 42.5, labels = bquote(paste(KUD[95], ' = ', .(ka[2,1]), m^2)), adj = c(0,0))
        text(7, 41, labels = paste0('Depth = ', j), adj = c(0,0))
        dev.off()
        
      } else {
        
        jpeg(file = paste0('kudplot_', unique(dayfile$Period), '_', j, '.jpg'))
        plot(ver95, col = kudcols[[1]], axes = T, xlim = c(40, 65), ylim = c(10, 45), xlab = 'x (m)', ylab = 'y (m)') # plot KUD95
        if (kcont50> 3){ plot(ver50, col = kudcols[[4]], axes = F, xlim = c(40, 65), ylim = c(10, 45), add=T)} # plot KUD50
        plot.pen(8)
        text(32, 44, labels = bquote(paste(KUD[50], ' = ', .(ka[1,1]), m^2)), adj = c(0,0))
        text(32, 42.5, labels = bquote(paste(KUD[95], ' = ', .(ka[2,1]), m^2)), adj = c(0,0))
        text(32, 41, labels = paste0('Depth = ', j), adj = c(0,0))
        dev.off()
        
      }
      
    } else {
      
      #k50 <- k50 + 0
      #k95 <- k95 + 0
      if (unique(dayfile$PEN == 7)){
        jpeg(file = paste0('kudplot_', unique(dayfile$Period), '_', j, '.jpg'))
        plot(ver95, col = 'white', border = 'white', axes = T, xlim = c(10, 45), ylim = c(10, 45), xlab = 'x (m)', ylab = 'y (m)') # plot KUD95
        #plot(x = 1, y = 1, col = 'white', axes = F, xlim = c(10, 45), ylim = c(10, 45), add=T) # plot KUD50
        plot.pen(7)
        text(7, 44, labels = bquote(paste(KUD[50], ' = 0', m^2)), adj = c(0,0))
        text(7, 42.5, labels = bquote(paste(KUD[95], ' = 0', m^2)), adj = c(0,0))
        text(7, 41, labels = paste0('Depth = ', j), adj = c(0,0))
        dev.off()
        
      } else{
        jpeg(file = paste0('kudplot_', unique(dayfile$Period), '_', j, '.jpg'))
        plot(ver95, col = 'white', border = 'white', axes = T, xlim = c(40, 65), ylim = c(10, 45), xlab = 'x (m)', ylab = 'y (m)') # plot KUD95
        #plot(x = 1, y = 1, col = 'white', axes = F, xlim = c(40, 65), ylim = c(10, 45), add=T) # plot KUD50
        plot.pen(8)
        text(32, 44, labels = bquote(paste(KUD[50], ' = 0', m^2)), adj = c(0,0))
        text(32, 42.5, labels = bquote(paste(KUD[95], ' = 0', m^2)), adj = c(0,0))
        text(32, 41, labels = paste0('Depth = ', j), adj = c(0,0))
        dev.off()
        
      }
      
    }
    
  }
  
}


# 48. Plot outline of pen 7 or 8

plot.pen <- function(pen){
  
  if (pen == 7){
    
    rect(locations.lookup['7EW', 'xmin'], locations.lookup['7EW', 'ymin'], locations.lookup['7EW', 'xmax'], locations.lookup['7EW', 'ymax'], lty = 2) # 7EW edge
    rect(locations.lookup['7ES', 'xmin'], locations.lookup['7ES', 'ymin'], locations.lookup['7ES', 'xmax'], locations.lookup['7ES', 'ymax'], lty = 2) # 7ES edge
    rect(locations.lookup['7EE', 'xmin'], locations.lookup['7EE', 'ymin'], locations.lookup['7EE', 'xmax'], locations.lookup['7EE', 'ymax'], lty = 2) # 7EE edge
    rect(locations.lookup['7EN', 'xmin'], locations.lookup['7EN', 'ymin'], locations.lookup['7EN', 'xmax'], locations.lookup['7EN', 'ymax'], lty = 2) # 7EN edge
    rect(locations.lookup['7WHSE', 'xmin'], locations.lookup['7WHSE', 'ymin'], locations.lookup['7WHSE', 'xmax'], locations.lookup['7WHSE', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHSE
    rect(locations.lookup['7WHNW', 'xmin'], locations.lookup['7WHNW', 'ymin'], locations.lookup['7WHNW', 'xmax'], locations.lookup['7WHNW', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHNW
    rect(locations.lookup['7FBSE', 'xmin'], locations.lookup['7FBSE', 'ymin'], locations.lookup['7FBSE', 'xmax'], locations.lookup['7FBSE', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 7FBSE
    rect(locations.lookup['7FBNW', 'xmin'], locations.lookup['7FBNW', 'ymin'], locations.lookup['7FBNW', 'xmax'], locations.lookup['7FBNW', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 7FBNW
    rect(locations.lookup['7EW', 'xmin'], locations.lookup['7ES', 'ymin'], locations.lookup['7EE', 'xmax'], locations.lookup['7EN', 'ymax'], lwd = 2) # cage limits
    
  } else {
    
    rect(locations.lookup['8EW', 'xmin'], locations.lookup['8EW', 'ymin'], locations.lookup['8EW', 'xmax'], locations.lookup['8EW', 'ymax'], lty = 2) # 7EW edge
    rect(locations.lookup['8ES', 'xmin'], locations.lookup['8ES', 'ymin'], locations.lookup['8ES', 'xmax'], locations.lookup['8ES', 'ymax'], lty = 2) # 7ES edge
    rect(locations.lookup['8EE', 'xmin'], locations.lookup['8EE', 'ymin'], locations.lookup['8EE', 'xmax'], locations.lookup['8EE', 'ymax'], lty = 2) # 7EE edge
    rect(locations.lookup['8EN', 'xmin'], locations.lookup['8EN', 'ymin'], locations.lookup['8EN', 'xmax'], locations.lookup['8EN', 'ymax'], lty = 2) # 7EN edge
    rect(locations.lookup['8WHSW', 'xmin'], locations.lookup['8WHSW', 'ymin'], locations.lookup['8WHSW', 'xmax'], locations.lookup['8WHSW', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHSE
    rect(locations.lookup['8WHNE', 'xmin'], locations.lookup['8WHNE', 'ymin'], locations.lookup['8WHNE', 'xmax'], locations.lookup['8WHNE', 'ymax'], lty = 3, col = rgb(1, 0.6, 0, 0.4)) # 7WHNW
    rect(locations.lookup['8FBNE', 'xmin'], locations.lookup['8FBNE', 'ymin'], locations.lookup['8FBNE', 'xmax'], locations.lookup['8FBNE', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 7FBNE
    rect(locations.lookup['8FBSW', 'xmin'], locations.lookup['8FBSW', 'ymin'], locations.lookup['8FBSW', 'xmax'], locations.lookup['8FBSW', 'ymax'], lty = 3, col = rgb(1, 1, 0.1, 0.4)) # 7FBSW
    rect(locations.lookup['8EW', 'xmin'], locations.lookup['8ES', 'ymin'], locations.lookup['8EE', 'xmax'], locations.lookup['8EN', 'ymax'], lwd = 2) # cage limits
    
  }  
  
  
}


# 50a. calculate behaviour state frequencies

bsf <- function(static = 0.15, cruise = 1.1, save = T){

bsffile <- dayfile[,c('Period', 'PEN', 'SEC', 'BLSEC')]
bsffile$BSF <- ifelse(bsffile$BLSEC <= static, 'static', ifelse(bsffile$BLSEC > static & bsffile$BLSEC <= cruise, 'cruise', 'burst'))
bsffile$BSFcount <- sequence(rle(bsffile$BSF)$lengths)
bsffile$CountTF <- c(ifelse(diff(bsffile$BSFcount, 1, 1) < 1, T, F), F)


library(data.table)

setDT(bsffile)
bsffile[,BSFdur:=ifelse(CountTF == T, sum(SEC),0), by =.(rleid(BSF))] # sums secs for each behaviour bout

detach("package:data.table")

bsffile <- subset(bsffile, BSFdur > 0)
#bsffile$round <- as.numeric(as.character(cut(bsffile$BSFdur, breaks = c(0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000), labels = c('1', '2', '5', '10', '20', '50', '100', '200', '500', '1000'))))
bsffile$round <- as.numeric(as.character(cut(bsffile$BSFdur, breaks = c(0, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024), labels = c('1', '2', '4', '8', '16', '32', '64', '128', '256', '512', '1024'))))

# generates table of BSF frequencies and draws plot

bsffile$BSF <- as.factor(bsffile$BSF)

bsftab <- as.data.frame(table(bsffile$round, bsffile$BSF, bsffile$PEN)) # tabulate frequencies of each duration and BSF
names(bsftab) <- c('dur', 'BSF', 'pen', 'count')
bsftab$dur <- as.numeric(as.character(bsftab$dur))
bsftab$count <- as.numeric(bsftab$count)

bsfsum <- tapply(bsftab$count, list(bsftab$BSF, bsftab$pen), sum)
bsftab$freq <- ifelse(bsftab$BSF == 'static' & bsftab$pen == '7', bsftab$count / bsfsum[3,1], ifelse(bsftab$BSF == 'cruise' & bsftab$pen == '7', bsftab$count / bsfsum[2,1], ifelse(bsftab$BSF == 'burst' & bsftab$pen == '7', bsftab$count / bsfsum[1,1], ifelse(bsftab$BSF == 'static' & bsftab$pen == '8', bsftab$count / bsfsum[3,2], ifelse(bsftab$BSF == 'cruise' & bsftab$pen == '8', bsftab$count / bsfsum[2,2], ifelse(bsftab$BSF == 'burst' & bsftab$pen == '8', bsftab$count / bsfsum[1,2], NA))))))

bsftab <- subset(bsftab, bsftab$freq > 0)

power_eqn = function(df, start = list(a = 50, b = 1)){
  m = nls(freq ~ a*dur^b, start = start, data = df);
  #eq <- substitute(italic(y) == a  ~italic(x)^b, list(a = format(coef(m)[1], digits = 2), b = format(coef(m)[2], digits = 2)))
  eq <- substitute(italic(y) == a  ~italic(x)^b, list(a = format(coef(m)[1], digits = 2), b = format(coef(m)[2], digits = 2)))
  as.character(as.expression(eq));                 
}

grouppal <- c(brewer.pal(3, 'Set1')[[1]], brewer.pal(3, 'Set1')[[2]], brewer.pal(3, 'Set1')[[1]], brewer.pal(3, 'Set1')[[2]])

sp = ggplot(subset(bsftab, BSF == 'static'), aes(x=dur, y=freq, colour = pen)) + theme(panel.background = element_rect(fill = 'white', colour = 'black'))
sp = sp + scale_x_log10(limits = c(10, 1000), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000))
#sp = sp + scale_y_log10(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000)) 
sp = sp + scale_y_log10(limits = c(0.001, 1), breaks = c(0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008, 0.009, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.8, 0.09, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), labels = c(bquote(10^-3), '', '', '', '', '', '', '', '', bquote(10^-2), '', '', '', '', '', '', '', '', bquote(10^-1), '', '', '', '', '', '', '', '', bquote(10^0))) 
sp = sp + geom_path(size = 1) + labs(title = 'Static', x = 'duration', y = 'frequency') + guides(colour = F) + geom_smooth(linetype = 'dashed',  method = 'nls', formula = y~a*x^b, se = F) + geom_text(size = 4.5, hjust = 0, aes(x = 100, y = 1, colour = grouppal[[2]], label = power_eqn(subset(bsftab, pen == '7' & BSF == 'static'))), parse = TRUE) + geom_text(size = 4.5, hjust = 0, aes(x = 100, y = 0.6, colour = grouppal[[1]], label = power_eqn(subset(bsftab, pen == '8' & BSF == 'static'))), parse = TRUE) + scale_colour_manual(values = grouppal)

#+ geom_text(aes(x = 100, y = 1, label = lm_eqn(lm(log(freq) ~ log(dur), subset(bsftab, pen == '7')))), parse = TRUE) + geom_text(aes(x = 100, y = 0.7, label = lm_eqn(lm(log(freq) ~ log(dur), subset(bsftab, pen == '8')))), parse = TRUE)

cp = ggplot(subset(bsftab, BSF == 'cruise'), aes(x=dur, y=freq, colour = pen)) + theme(panel.background = element_rect(fill = 'white', colour = 'black'))
cp = cp + scale_x_log10(limits = c(10, 1000), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000))
#cp = cp + scale_y_log10(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000)) 
cp = cp + scale_y_log10(limits = c(0.001, 1), breaks = c(0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008, 0.009, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.8, 0.09, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), labels = c(bquote(10^-3), '', '', '', '', '', '', '', '', bquote(10^-2), '', '', '', '', '', '', '', '', bquote(10^-1), '', '', '', '', '', '', '', '', bquote(10^0))) 
#cp = cp + geom_path(size = 1) + labs(title = 'Cruise', x = 'duration', y = 'frequency') + guides(colour = F)
cp = cp + geom_path(size = 1) + labs(title = 'Cruise', x = 'duration', y = 'frequency') + guides(colour = F) + geom_smooth(linetype = 'dashed',  method = 'nls', formula = y~a*x^b, se = F) + geom_text(size = 4.5, hjust = 0, aes(x = 100, y = 1, colour = grouppal[[2]], label = power_eqn(subset(bsftab, pen == '7' & BSF == 'cruise'))), parse = TRUE) + geom_text(size = 4.5, hjust = 0, aes(x = 100, y = 0.6, colour = grouppal[[1]], label = power_eqn(subset(bsftab, pen == '8' & BSF == 'cruise'))), parse = TRUE) + scale_colour_manual(values = grouppal)

bp = ggplot(subset(bsftab, BSF == 'burst'), aes(x=dur, y=freq, colour = factor(pen, labels = c('conditioned', 'unconditioned')))) + theme(panel.background = element_rect(fill = 'white', colour = 'black'), legend.title = element_text(size = 16, face = 'bold'), legend.title.align = 0.5, legend.background = element_rect(colour = 'black', size = 1, linetype = 'solid'), legend.key.size = unit(1, 'cm'))
bp = bp + scale_x_log10(limits = c(10, 1000), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000))
#bp = bp + scale_y_log10(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000)) 
bp = bp + scale_y_log10(limits = c(0.001, 1), breaks = c(0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008, 0.009, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.8, 0.09, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), labels = c(bquote(10^-3), '', '', '', '', '', '', '', '', bquote(10^-2), '', '', '', '', '', '', '', '', bquote(10^-1), '', '', '', '', '', '', '', '', bquote(10^0))) 
#bp = bp + geom_path(size = 1) + labs(title = 'Burst', x = 'duration', y = 'frequency', colour = 'Group')
bp = bp + geom_path(size = 1) + labs(title = 'Burst', x = 'duration', y = 'frequency', colour = 'Group') + geom_smooth(linetype = 'dashed',  method = 'nls', formula = y~a*x^b, se = F) + geom_text(size = 4.5, hjust = 0, show.legend = F, aes(x = 100, y = 1, colour = grouppal[[2]], label = power_eqn(subset(bsftab, pen == '7' & BSF == 'burst'))), parse = TRUE) + geom_text(size = 4.5, hjust = 0, show.legend = F, aes(x = 100, y = 0.6, colour = grouppal[[1]], label = power_eqn(subset(bsftab, pen == '8' & BSF == 'burst'))), parse = TRUE) + scale_colour_manual(breaks = c('conditioned', 'unconditioned'), values = grouppal)

legend <- get_legend(bp)
bp = bp  + guides(colour = F)

bsfplot <- plot_grid(sp, cp, bp, legend, nrow = 2, ncol = 2)
daytext = paste('Day', substr(dayfile.loc, 15, 17), sep = ' ')
bsfplot <- bsfplot + draw_text(daytext, size = 16, x = 0.71, y = 0.33, hjust = 0)
print(bsfplot) 

if(save == T){
#ggsave(filename = sub('day_coded.csv', '_bsfplot.png', dayfile.loc), plot = bsfplot) 
save_plot(sub('day_coded.csv', '_bsfplot.png', dayfile.loc), bsfplot, ncol = 2.5, nrow = 2.5, base_aspect_ratio = 1.1, base_height = 4)  
write.csv(bsftab, file = sub("day_coded.csv", "_bsftable.csv", dayfile.loc))  
}

}


# 50b. calculate behaviour state frequencies (Rr, Rf, Ra, Ep, Ef, Ea) for pens 7 and 8. save = save plot and data file(T/F)

bsf2 <- function(save = T){
  
  
  bsffile <- dayfile[,c('Period', 'PEN', 'SEC', 'BLSEC', 'BS')]
  #bsffile$BSF <- ifelse(bsffile$BLSEC <= static, 'static', ifelse(bsffile$BLSEC > static & bsffile$BLSEC <= cruise, 'cruise', 'burst'))
  bsffile$BS <- as.character(bsffile$BS)
  bsffile$BScount <- sequence(rle(bsffile$BS)$lengths)
  bsffile$CountTF <- c(ifelse(diff(bsffile$BScount, 1, 1) < 1, T, F), F)
  
  
  library(data.table)
  
  setDT(bsffile)
  bsffile[,BSdur:=ifelse(CountTF == T, sum(SEC),0), by =.(rleid(BS))] # sums secs for each behaviour bout
  
  detach("package:data.table")
  
  #bsffile$BSdur <- with(bsffile, ave(SEC, cumsum(c(TRUE, BS[-1]!= BS[-nrow(bsffile)])), FUN = sum)*CountTF)
  
  bsffile <- subset(bsffile, BSdur > 0)
  #bsffile$round <- as.numeric(as.character(cut(bsffile$BSFdur, breaks = c(0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000), labels = c('1', '2', '5', '10', '20', '50', '100', '200', '500', '1000'))))
  bsffile$round <- as.numeric(as.character(cut(bsffile$BSdur, breaks = c(0, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192), labels = c('1', '2', '4', '8', '16', '32', '64', '128', '256', '512', '1024', '2048', '4096', '8192'))))
  
  # generates table of BSF frequencies and draws plot
  
  bsffile$BS <- as.factor(bsffile$BS)
  
  bstab <- as.data.frame(table(bsffile$round, bsffile$BS, bsffile$PEN)) # tabulate frequencies of each duration and BSF
  names(bstab) <- c('dur', 'BS', 'pen', 'count')
  bstab$dur <- as.numeric(as.character(bstab$dur))
  bstab$count <- as.numeric(bstab$count)
  
  bssum <- tapply(bstab$count, list(bstab$BS, bstab$pen), sum)
  
  bstab$freq <- ifelse(bstab$BS == 'Ea' & bstab$pen == '7', bstab$count / bssum[1,1], ifelse(bstab$BS == 'Ef' & bstab$pen == '7', bstab$count / bssum[2,1], ifelse(bstab$BS == 'Ep' & bstab$pen == '7', bstab$count / bssum[3,1], ifelse(bstab$BS == 'Ra' & bstab$pen == '7', bstab$count / bssum[4,1], ifelse(bstab$BS == 'Rf' & bstab$pen == '7', bstab$count / bssum[5,1], ifelse(bstab$BS == 'Rr' & bstab$pen == '7', bstab$count / bssum[6,1], 
                    ifelse(bstab$BS == 'Ea' & bstab$pen == '8', bstab$count / bssum[1,2], ifelse(bstab$BS == 'Ef' & bstab$pen == '8', bstab$count / bssum[2,2], ifelse(bstab$BS == 'Ep' & bstab$pen == '8', bstab$count / bssum[3,2], ifelse(bstab$BS == 'Ra' & bstab$pen == '8', bstab$count / bssum[4,2], ifelse(bstab$BS == 'Rf' & bstab$pen == '8', bstab$count / bssum[5,2], ifelse(bstab$BS == 'Rr' & bstab$pen == '8', bstab$count / bssum[6,2], NA))))))))))))
  
  bstab <- subset(bstab, bstab$freq > 0)
  
  power_eqn = function(df, start = list(a = 50, b = 1)){
    m = nls(freq ~ a*dur^b, start = start, data = df);
    #eq <- substitute(italic(y) == a  ~italic(x)^b, list(a = format(coef(m)[1], digits = 2), b = format(coef(m)[2], digits = 2)))
    eq <- substitute(italic(y) == a  ~italic(x)^b, list(a = format(coef(m)[1], digits = 2), b = format(coef(m)[2], digits = 2)))
    as.character(as.expression(eq));                 
  }
  
  #grouppal <- c(brewer.pal(3, 'Set1')[[1]], brewer.pal(3, 'Set1')[[2]], brewer.pal(3, 'Set1')[[1]], brewer.pal(3, 'Set1')[[2]])
  grouppal <- c(brewer.pal(11, 'Spectral')[[2]], brewer.pal(11, 'Spectral')[[3]], brewer.pal(11, 'Spectral')[[4]], brewer.pal(11, 'Spectral')[[8]], brewer.pal(11, 'Spectral')[[9]], brewer.pal(11, 'Spectral')[[10]])
  
  sp = ggplot(bstab, aes(x=dur, y=freq, colour = BS, group = BS)) + theme(panel.background = element_rect(fill = 'white', colour = 'black'))
  sp = sp + scale_x_log10(limits = c(10, 1000), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000))
  #sp = sp + scale_y_log10(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000)) 
  sp = sp + scale_y_log10(limits = c(0.001, 1), breaks = c(0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008, 0.009, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.8, 0.09, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), labels = c(bquote(10^-3), '', '', '', '', '', '', '', '', bquote(10^-2), '', '', '', '', '', '', '', '', bquote(10^-1), '', '', '', '', '', '', '', '', bquote(10^0))) 
  sp = sp + geom_path(size = 1) + labs(title = unique(bsffile$Period), x = 'duration', y = 'frequency') + scale_colour_manual(values = grouppal)# + guides(colour = F)# + geom_smooth(linetype = 'dashed',  method = 'nls', formula = y~a*x^b, se = F) + geom_text(size = 4.5, hjust = 0, aes(x = 100, y = 1, colour = grouppal[[2]], label = power_eqn(subset(bstab, pen == '7' & BS == 'Ea'))), parse = TRUE) + geom_text(size = 4.5, hjust = 0, aes(x = 100, y = 0.6, colour = grouppal[[1]], label = power_eqn(subset(bstab, pen == '8' & BS == 'Ea'))), parse = TRUE)
  
  #+ geom_text(aes(x = 100, y = 1, label = lm_eqn(lm(log(freq) ~ log(dur), subset(bsftab, pen == '7')))), parse = TRUE) + geom_text(aes(x = 100, y = 0.7, label = lm_eqn(lm(log(freq) ~ log(dur), subset(bsftab, pen == '8')))), parse = TRUE)
  
  #cp = ggplot(subset(bstab, BS == 'cruise'), aes(x=dur, y=freq, colour = pen)) + theme(panel.background = element_rect(fill = 'white', colour = 'black'))
  #cp = cp + scale_x_log10(limits = c(10, 1000), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000))
  #cp = cp + scale_y_log10(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000)) 
  #cp = cp + scale_y_log10(limits = c(0.001, 1), breaks = c(0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008, 0.009, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.8, 0.09, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), labels = c(bquote(10^-3), '', '', '', '', '', '', '', '', bquote(10^-2), '', '', '', '', '', '', '', '', bquote(10^-1), '', '', '', '', '', '', '', '', bquote(10^0))) 
  #cp = cp + geom_path(size = 1) + labs(title = 'Cruise', x = 'duration', y = 'frequency') + guides(colour = F)
  #cp = cp + geom_path(size = 1) + labs(title = 'Cruise', x = 'duration', y = 'frequency') + guides(colour = F) + geom_smooth(linetype = 'dashed',  method = 'nls', formula = y~a*x^b, se = F) + geom_text(size = 4.5, hjust = 0, aes(x = 100, y = 1, colour = grouppal[[2]], label = power_eqn(subset(bstab, pen == '7' & BS == 'cruise'))), parse = TRUE) + geom_text(size = 4.5, hjust = 0, aes(x = 100, y = 0.6, colour = grouppal[[1]], label = power_eqn(subset(bstab, pen == '8' & BS == 'cruise'))), parse = TRUE) + scale_colour_manual(values = grouppal)
  
  #bp = ggplot(subset(bstab, BSF == 'burst'), aes(x=dur, y=freq, colour = factor(pen, labels = c('farmed wrasse', 'wild wrasse')))) + theme(panel.background = element_rect(fill = 'white', colour = 'black'), legend.title = element_text(size = 16, face = 'bold'), legend.title.align = 0.5, legend.background = element_rect(colour = 'black', size = 1, linetype = 'solid'), legend.key.size = unit(1, 'cm'))
  #bp = bp + scale_x_log10(limits = c(10, 1000), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000))
  #bp = bp + scale_y_log10(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000), labels = c(1, '', '', '', '', '', '', '', '', 10, '', '', '', '', '', '', '', '', 100, '', '', '', '', '', '', '', '', 1000, '', '', '', '', '', '', '', '', 10000)) 
  #bp = bp + scale_y_log10(limits = c(0.001, 1), breaks = c(0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008, 0.009, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.8, 0.09, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), labels = c(bquote(10^-3), '', '', '', '', '', '', '', '', bquote(10^-2), '', '', '', '', '', '', '', '', bquote(10^-1), '', '', '', '', '', '', '', '', bquote(10^0))) 
  #bp = bp + geom_path(size = 1) + labs(title = 'Burst', x = 'duration', y = 'frequency', colour = 'Group')
  #bp = bp + geom_path(size = 1) + labs(title = 'Burst', x = 'duration', y = 'frequency', colour = 'Group') + geom_smooth(linetype = 'dashed',  method = 'nls', formula = y~a*x^b, se = F) + geom_text(size = 4.5, hjust = 0, show.legend = F, aes(x = 100, y = 1, colour = grouppal[[2]], label = power_eqn(subset(bstab, pen == '7' & BS == 'burst'))), parse = TRUE) + geom_text(size = 4.5, hjust = 0, show.legend = F, aes(x = 100, y = 0.6, colour = grouppal[[1]], label = power_eqn(subset(bstab, pen == '8' & BS == 'burst'))), parse = TRUE) + scale_colour_manual(breaks = c('farmed wrasse', 'wild wrasse'), values = grouppal)
  
  #legend <- get_legend(bp)
  #bp = bp  + guides(colour = F)
  
  #bsfplot <- plot_grid(sp, cp, bp, legend, nrow = 2, ncol = 2)
  #daytext = paste('Day', substr(dayfile.loc, 15, 17), sep = ' ')
  #bsfplot <- bsfplot + draw_text(daytext, size = 16, x = 0.71, y = 0.33, hjust = 0)
  #print(bsfplot) 
  print(sp)
  bstab <<- bstab
  
  #if(save == T){
  #ggsave(filename = sub('day_coded.csv', '_bsfplot.png', dayfile.loc), plot = bsfplot) 
  #  save_plot(sub('day_coded.csv', '_bsfplot.png', dayfile.loc), bsfplot, ncol = 2.5, nrow = 2.5, base_aspect_ratio = 1.1, base_height = 4)  
  #  write.csv(bstab, file = sub("day_coded.csv", "_bsftable.csv", dayfile.loc))  
  #}
  
}




# 51. density map of depth over time--------------------------------


# draw heatmap of time vs. depth

dayfile <- preconb[preconb$PEN == '8',]
dayfile <- dayfile[sample(nrow(dayfile), 3000000),] # random sample as large plots fail

library(viridis)

dm <- ggplot(dayfile[dayfile$PEN == '8',], aes(x = EchoTime, y = PosZ)) +
  stat_density_2d(geom = 'raster', aes(fill = stat(density)), contour = F) + scale_fill_viridis() +
  #scale_fill_gradientn(colours=plot.col, space = 'Lab', limits = c(0, 100), na.value = plot.col[length(plot.col)], name = 'No. pings') +
  geom_density_2d(aes(colour = PosZ)) + #ylim(25, 0) +
  #scale_y_continuous(expand = c(0, 0), limits = c(0, 25)) + 
  scale_y_reverse(name = 'Depth (m)', expand = c(0, 0), limits = c(25, 0)) +
  scale_x_datetime(name = 'Date', expand = c(0, 0))
dm + theme(legend.position = 'none') + ggtitle('Non-acclimated wrasse')


fish.heatmap <- function(fish){

dm <- ggplot(dayfile[dayfile$Period == fish,], aes(x = EchoTime, y = PosZ)) +
  stat_density_2d(geom = 'raster', aes(fill = stat(density)), contour = F) + scale_fill_viridis() +
  #scale_fill_gradientn(colours=plot.col, space = 'Lab', limits = c(0, 100), na.value = plot.col[length(plot.col)], name = 'No. pings') +
  geom_density_2d(aes(colour = PosZ)) + #ylim(25, 0) +
  #scale_y_continuous(expand = c(0, 0), limits = c(0, 25)) + 
  scale_y_reverse(name = 'Depth (m)', expand = c(0, 0), limits = c(25, 0)) +
  scale_x_datetime(name = 'Date', expand = c(0, 0))
dm + theme(legend.position = 'none')# + ggtitle('Non-acclimated wrasse')


}




# modified function from WaveletComp package

wt.image2 <-
  function(WT, my.series = 1, 
           plot.coi = T, 
           plot.contour = T, siglvl = 0.1, col.contour = "white", 
           plot.ridge = T, lvl = 0, col.ridge = "black", 
           color.key = "quantile", 
           n.levels=100, color.palette = "rainbow(n.levels, start=0, end=.7)",
           useRaster = T, max.contour.segments = 250000,
           plot.legend = T,
           legend.params = list(width=1.2, shrink=0.9, mar=5.1, n.ticks=6, label.digits=1, label.format="f", lab=NULL, lab.line=2.5),
           label.time.axis = T, show.date = F, date.format = NULL, timelab = NULL, 
           label.period.axis = T, periodlab = NULL,
           main = NULL,
           lwd = 2,
           graphics.reset = T,
           verbose = F){
    
    ################################################    
    
    if(verbose == T){
      out <- function(...){ cat(...) }
    }
    else{
      out <- function(...) { }
    }  
    
    default.options = options() 
    
    options(max.contour.segments = as.integer(max.contour.segments))
    
    #################################################
    
    axis.1 <- WT$axis.1
    axis.2 <- WT$axis.2 
    
    lwd.axis = 0.25
    
    series.data = WT$series
    
    ####################################
    ## Identify the scenario
    ####################################
    
    if (class(WT) == 'analyze.wavelet') {
      
      out("Your input object class is 'analyze.wavelet'...\n") 
      
      my.series = ifelse(names(series.data)[1] == 'date', names(series.data)[2], names(series.data)[1]) 
      
      Power = WT$Power
      Power.pval = WT$Power.pval
      Ridge = WT$Ridge     
      
    } 
    if (class(WT) == 'analyze.coherency') {   
      
      out("Your input object class is 'analyze.coherency'...\n") 
      
      if (is.numeric(my.series)) { 
        if (!is.element(my.series,c(1,2))) { stop("Please choose either series number 1 or 2!") }
        my.series = ifelse(names(series.data)[1] == 'date', names(series.data)[my.series+1], names(series.data)[my.series])  
      }
      
      ind = which( names(series.data) == my.series ) 
      which.series.num = ifelse(names(series.data)[1] == 'date', ind-1, ind)
      if (!is.element(which.series.num, c(1,2))) { stop("Your series name is not available, please check!") }
      
      if (which.series.num == 1) {
        Power = WT$Power.x
        Power.pval = WT$Power.x.pval
        Ridge = WT$Ridge.x
      }
      if (which.series.num == 2) {
        Power = WT$Power.y
        Power.pval = WT$Power.y.pval
        Ridge = WT$Ridge.y
      }      
      
    }   
    
    out(paste("A wavelet power image of your time series '", my.series, "' will be plotted...", sep=''),'\n')   
    
    
    if (is.element(color.key,c('interval','i'))) {    
      wavelet.levels = seq(from=0, to=max(Power), length.out=n.levels+1)
    }  
    if (is.element(color.key,c('quantile','q'))) {  
      wavelet.levels = quantile(Power, probs=seq(from=0, to=1, length.out=n.levels+1)) 
    }   
    key.cols = rev(eval(parse(text=color.palette)))
    
    # legend parameters  
    
    if (is.null(legend.params$width))         legend.params$width = 1.2
    if (is.null(legend.params$shrink))        legend.params$shrink = 0.9
    if (is.null(legend.params$mar)) legend.params$mar = ifelse(is.null(legend.params$lab), 5.1, 6.1)   
    if (is.null(legend.params$n.ticks))       legend.params$n.ticks = 6
    if (is.null(legend.params$label.digits))  legend.params$label.digits = 1
    if (is.null(legend.params$label.format))  legend.params$label.format = "f"
    if (is.null(legend.params$lab.line))      legend.params$lab.line = 2.5
    
    #######################################################################################
    ## start plotting
    #######################################################################################
    
    op = par(no.readonly = TRUE)
    
    image.plt  = par()$plt
    legend.plt = NULL
    
    if (plot.legend == T) {
      
      # construct plot regions for image and legend
      
      legend.plt = par()$plt
      
      char.size = par()$cin[1]/par()$din[1]
      
      hoffset       = char.size * par()$mar[4]
      legend.width  = char.size * legend.params$width
      legend.mar    = char.size * legend.params$mar
      
      legend.plt[2] = 1 - legend.mar
      legend.plt[1] = legend.plt[2] - legend.width
      
      vmar = (legend.plt[4] - legend.plt[3]) * ((1 - legend.params$shrink)/2)
      
      legend.plt[4] = legend.plt[4] - vmar
      legend.plt[3] = legend.plt[3] + vmar
      
      image.plt[2] = min(image.plt[2], legend.plt[1] - hoffset)
      
      # plot legend first
      
      par(plt = legend.plt)
      
      key.marks  = round(seq(from = 0, to = 1, length.out=legend.params$n.ticks)*n.levels)
      key.labels = formatC(as.numeric(wavelet.levels), digits = legend.params$label.digits, format = legend.params$label.format)[key.marks+1]
      
      image(1, seq(from = 0, to = n.levels), matrix(wavelet.levels, nrow=1), col = key.cols, breaks = wavelet.levels, useRaster=T, xaxt='n', yaxt='n', xlab='', ylab='')
      axis(4, lwd=lwd.axis, at=key.marks, labels=NA, tck=0.02, tcl=(par()$usr[2]-par()$usr[1])*legend.params$width-0.04)
      mtext(key.labels, side = 4, at = key.marks, line = 0.5, las=2)
      text(x = par()$usr[2] + (1.5+legend.params$lab.line)*par()$cxy[1], y=n.levels/2, labels=legend.params$lab, xpd=NA, srt = 270)
      
      box(lwd = lwd.axis)    
      
      par(new=TRUE, plt = image.plt)  
      
    }    
    
    #######################################################################################
    ## plot power image
    #######################################################################################
    
    image(axis.1, axis.2, t(Power), col = key.cols, breaks = wavelet.levels, useRaster = useRaster,
          ylab = "", xlab = '', axes = FALSE, main = main)        
    
    # plot contour lines?     
    if  ((plot.contour == T) & (is.null(Power.pval) == F)) {      
      contour(axis.1, axis.2, t(Power.pval) < siglvl, levels = 1, lwd = lwd, 
              add = TRUE, col = col.contour, drawlabels = FALSE)
    }
    
    # plot ridge?
    if  (plot.ridge == T) {    
      Ridge = Ridge * (Power >= lvl)      
      contour(axis.1, axis.2, t(Ridge), levels = 1, lwd = lwd,
              add = TRUE, col = col.ridge, drawlabels = FALSE)         
    }     
    
    # plot cone of influence?
    if (plot.coi == T) {
      polygon(WT$coi.1, WT$coi.2, border = NA, col = rgb(1, 1, 1, 0.5))
    }  
    
    box(lwd = lwd.axis)
    
    
    # label period axis ?  
    if (label.period.axis == T) {
      
      if (is.null(periodlab)) {periodlab='period'}
      
      #period.tick = unique(trunc(axis.2))
      #period.tick[period.tick<log2(WT$Period[1])] = NA
      #period.tick = na.omit(period.tick)
      period.tick <- c(1, 2, 3, 3.584963, 4, 4.5849625, 5) # my code to add in 24h tick mark
      period.tick.label = round(2^(period.tick))   
      axis(2, lwd = lwd.axis, at = period.tick, labels = NA, tck=0.02, tcl=0.5)
      axis(4, lwd = lwd.axis, at = period.tick, labels = NA, tck=0.02, tcl=0.5)
      mtext(period.tick.label, side = 2, at = period.tick, las = 1, line = 0.5)
      
      mtext(periodlab, side = 2, line = 2.5)
      
    }
    
    # label time axis ?   
    if (label.time.axis == T) {
      
      if (is.null(timelab)) {timelab='time'}
      
      if (show.date == F) {
        A.1 = axis(1, lwd = lwd.axis, labels=NA, tck = 0.0)
        mtext(A.1, side = 1, at = A.1, line = 0.5)
        mtext(timelab, side = 1, line = 2)
      }
      if (show.date == T) {  
        
        if (is.element('date',names(series.data))) { my.date = series.data$date }  
        else { my.date = rownames(series.data) }  
        
        if (is.null(date.format)) { my.date = as.Date(my.date) }
        else { my.date = as.POSIXct(my.date, format=date.format) }
        par(new=TRUE)
        #empty plot, but calendar
        plot(my.date, seq(min(axis.2),max(axis.2), length.out=WT$nc), type="n", xaxs = "i", yaxs ='i', yaxt='n', xlab="", ylab="",
             lwd = lwd.axis, tck=0.02, tcl=0.5)
        mtext(timelab, side = 1, line = 2.5)      
      }
      
      # my code for adding line at 12h and 24h-------------------------------------------      
      abline(h = 4.5849625, col = 'black', lty = 2, lwd = 2)
      abline(h = 3.584963, col = 'black', lty = 2, lwd = 2)
      # -------------------------------------------------------------------------
    }  
    
    
    
    #######################################################################################
    ## apropos graphical parameters
    #######################################################################################
    
    # reset contour line options
    options(default.options)
    
    # reset graphical parameters?
    if (graphics.reset == T) {
      par(op)
    }     
    
    # output of graphical parameters
    
    output = list(op = op, image.plt = image.plt, legend.plt=legend.plt)
    class(output) = "graphical parameters"
    
    out("Class attributes are accessible through following names:\n")
    out(names(output), "\n")    
    
    return(invisible(output)) 
  }





