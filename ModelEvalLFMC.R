#install.packages("zoo") # -> run only if the package is not yet installed
install.packages("tidyverse") 
#install.packages("fs")
#install.packages("terra")
install.packages("hydroTSM")
install.packages("pacman")
pacman::p_load(
  lubridate,  # general package for handling and converting dates  
  parsedate,   # has function to "guess" messy dates
  aweek,      # another option for converting dates to weeks, and weeks to dates
  zoo,        # additional date/time functions
  tidyverse,  # data management and visualization  
  rio)        # data import/export
pacman::p_load(lubridate)
install.packages("gridExtra")


library(zoo)
library(terra)
library(tidyverse)
library(fs)
library("hydroTSM")
library(gridExtra)
library(ggplot2)
library(gridExtra)

?nchar

# 1 Read the LST data from a csv file
#------------------------------------
setwd("C:/Users/Manuela/Desktop/Carto/TUD/Remote sensing-based Environmental mapping/Final Project")

# read the Globe LFMC data from a csv file

# Vector with file names
names = list.files("E07_final-exercise/data/Globe-LFMC/", pattern="*.csv")

results.df <- data.frame(site=NA, lat=NA, lon=NA, RMSE=NA)
?data.frame

#read.csv("E07_final-exercise/data/Globe-LFMC/", sep=";", header = TRUE, na.strings = "F")

fileData <- list()

?list

for(fileNum in 1:length(names)){
  
  fileData[[fileNum]] <- read.csv(file = paste0("E07_final-exercise/data/Globe-LFMC/",names[fileNum])
                                  , sep = ";", header = TRUE, na.strings = "F");
  
    "results.df$site <- fileData[[fileNum]]$Sitename;
    results.df$lat <- fileData[[fileNum]]$Latitude;
    results.df$lon <- fileData[[fileNum]]$Longitude;"
}


for (i in 1:length(names)) {
   
   results.df[i,1] <- fileData[[i]]$Sitename[1]
   results.df[i,2] <- fileData[[i]]$Latitude[1]
   results.df[i,3] <- fileData[[i]]$Longitude[1]
}

# 3 Define Drought and Wet season by looking at the SPI data
#-------------------------------------

# Read SPI datafile

setwd("C:/Users/Manuela/Desktop/Carto/TUD/Remote sensing-based Environmental mapping/Final Project/E07_final-exercise/data")

file.SPI <- "us49_southwest_reg_spg12_m_wld_19990101_20211201_m.nc"
spi <- rast(file.SPI)

plot(spi)
summary(spi)

?cbind

#Define coordinates
par(mfrow=c(1,2))
plot(spi)


for (i in 1:length(names)) {
  coord <- cbind(results.df[i,3], results.df[i,2])
  points( coord, pch=16)
  #text(coord, c(results.df[i,1]), pos = 1)
}

#coor[,]<- cbind(results.df[1,3], results.df[1,2])
#coor[,]<- cbind(results.df[2,3], results.df[2,2])


#Extract time series from the pixels by making a loop and creating a database with 
#points and spi
# + plot all the SPI ts

global.spi <- data.frame(site=NA, lat=NA, lon=NA)

for (i in 1:length(names)) {
  
  global.spi[i,1] <- fileData[[i]]$Sitename[1]
  global.spi[i,2] <- fileData[[i]]$Latitude[1]
  global.spi[i,3] <- fileData[[i]]$Longitude[1]
  
  coord <- cbind(results.df[i,3], results.df[i,2])
  spi.px<- terra::extract(spi, coord)
  x <- unlist(spi.px[1,])
  spi.px.ts <- zoo (x, time(spi))
  
  
  plot(na.omit(spi.px.ts), ylab="SPI", main= global.spi[i,1])
  points( spi.px.ts [ which(spi.px.ts < -1)], col="red")
  points( spi.px.ts [which(spi.px.ts > 1)], col="blue")
  
  #TODO: make histogram with dates with maximum values

  for (j in 1:240) {
    global.spi[i,j+3] <- spi.px.ts[j]
  }
}

for (i in 4:243){
  names(global.spi)[i]<- format(as.Date(time(spi.px.ts[i-3]), format = "%Y-%m-%d"))
}

#Average value of SPI by months on the whole series to find most wet and most dry

global.spi[63,1] <- "Mean"

for (i in 4:243){
  global.spi[63,i] <- mean(global.spi[, i], na.rm = TRUE)
}


?mean


#extreme values

    most.wet <- max(global.spi[63,4:ncol(global.spi)], na.rm = TRUE)
    most.wet
    most.wet.month <- which.max(global.spi[63,4:ncol(global.spi)])
    most.wet.month
    
    most.dry <- min(global.spi[63,4:ncol(global.spi)], na.rm = TRUE)
    most.dry 
    most.dry.month <- which.min(global.spi[63,4:ncol(global.spi)])
    most.dry.month
    
    # most.wet.month 2005-03-01 SPI mean. 1.174823
    # most.dry.month 2002-08-01 SPI mean. -2.234484

#Min and max values of average SPI since 2007, (for SWI comparison) 
    
    #From most wet to most drought
    mean.spi <- global.spi[63,]
    
    # SPI sorted in descending order
    mean.spi.sorted <- sort(mean.spi, decreasing = TRUE)
    
    # Most wet month since 2007 is 2016-04, SPI mean. 0.3706129
    # Most dry month since 2007 is 2013-02, SPI mean. -1.890426
    
    
    plot(spi$SPI12_211) # 2005-03-01 most wet, SPI mean. 1.174823
    plot(spi$SPI12_78) # 2016-04  most wet since 2007, SPI mean. 0.3706129
    
    plot(spi$SPI12_46) # 2002-08-01 most dry, SPI mean. -2.234484
    plot(spi$SPI12_173) # 2013-02  most dry since 2007, SPI mean. -1.890426

mean(global.spi[,4])
colnames(global.spi)[4]
ncol(average.spi)
colnames(average.spi)[1]
colnames(average.spi)[1] <- colnames(global.spi)[4]

# # Extract for DROUGHT values under -1
# drought.cond <- spi.px.ts[ which(spi.px.ts[,2] < -1)]
# plot(drought.cond, main="Drought conditions", type="l")
# 
# 
# # Extract for WET values over 1
# wet.cond <- spi.px.ts[ which(spi.px.ts[,2] > 1)]
# plot(wet.cond, main="Wet conditions", type="l")
# 
# # Plot TS and wet and dry conditions
# 
# plot(na.omit(spi.px.ts), ylab="SPI")
# lines(wet.cond, col="blue" )
# lines(drought.cond, col="red")



#4 for drought and wet conditions, look for the LFMC model and in situ measurements and create linear regression
#-------------------------------------        
#Get LFMC model to monthly data
#read model file

setwd("C:/Users/Manuela/Desktop/Carto/TUD/Remote sensing-based Environmental mapping/Final Project/E07_final-exercise/data")

file.VOD2LFMC <- "us49_southwest_reg_VOD2LFMC-B_v01_2000-02.2017-07.nc"
VOD2LFMC <- rast(file.VOD2LFMC)

plot(VOD2LFMC,2)

#convert model to time series monthly and create table

VOD2LFMC.sites <- data.frame(site=NA, lat=NA, lon=NA)

for (i in 1:length(names)) {
  
  VOD2LFMC.sites[i,1] <- fileData[[i]]$Sitename[1]
  VOD2LFMC.sites[i,2] <- fileData[[i]]$Latitude[1]
  VOD2LFMC.sites[i,3] <- fileData[[i]]$Longitude[1]
  
  coord <- cbind(results.df[i,3], results.df[i,2])
  VOD2LFMC.px<- terra::extract(VOD2LFMC, coord)
  x <- unlist(VOD2LFMC.px[1,])
  VOD2LFMC.px.ts <- zoo(x, as.Date(time(VOD2LFMC)))
  VOD2LFMC.px.ts.month <- daily2monthly(VOD2LFMC.px.ts, FUN = mean, na.rm = T)
  
  for (j in 1:length(VOD2LFMC.px.ts.month)) {
    VOD2LFMC.sites[i,j+3] <- VOD2LFMC.px.ts.month[j]
  }
  
  for (i in 4:length(VOD2LFMC.px.ts.month)){
    names(VOD2LFMC.sites)[i]<- format(as.Date(time(VOD2LFMC.px.ts.month[i-3]), format = "%Y-%m-%d"))
  }
  
}

  #Testing locations that have NA to see if it is correct. Yes, they "outside" the map
  
  # coord <- cbind(results.df[51,3], results.df[51,2])
  # 
  # plot(VOD2LFMC,2)
  # points(coord, pch=3)

#Convert data to zoo time series with LFMC.values
#insituLFMC.ts <- zoo(fileData[[1]][["LFMC.value"]], ymd(fileData[[1]][["Sampling.date"]]))

library(purrr)
library(dplyr)

#TRYING THINGS NOT WORKING
'# Combine all data frames in the list into one data frame
merged_df <- reduce(fileData, full_join)
# Extract specific columns from the merged data frame
extracted_df <- merged_df %>% select(Sitename, Latitude, Longitude, LFMC.value, Sampling.date)
library(tidyr)
# Reshape the data frame
reshaped_df <- extracted_df %>% pivot_wider(names_from = Sampling.date, values_from = LFMC.value, values_fn = list)
# Create a time series based on the "date" column
insituLFMC.ts<- zoo(extracted_df[, "LFMC.value"], order.by = extracted_df[, "Sampling.date"],)
'

#dfInsitu <- merge(fileData, by = "Sampling.date", all=True)
?merge

#insituLFMC.ts <- zoo(fileData[[i]], ymd(fileData[[i]][["Sampling.date"]]))
#insituLFMC.ts <- merge.zoo(insituLFMC.ts, zoo(fileData[[2]], ymd(fileData[[2]][["Sampling.date"]])),fill = NA)

insituLFMC <- data.frame(site=NA, lat=NA, lon=NA)

for (i in 1:length(names)) {
  
  insituLFMC[i,1] <- fileData[[i]]$Sitename[1]
  insituLFMC[i,2] <- fileData[[i]]$Latitude[1]
  insituLFMC[i,3] <- fileData[[i]]$Longitude[1]
  
  insituLFMC.ts <- zoo(fileData[[i]][["LFMC.value"]], ymd(fileData[[i]][["Sampling.date"]]))
  month <- format(index(insituLFMC.ts), "%Y-%m-%01")
  insituLFMC.month.ts <- aggregate(insituLFMC.ts ~ month, FUN=mean, na.rm=TRUE)
  insituLFMC.month.ts <- zoo(insituLFMC.month.ts[,2], order.by=insituLFMC.month.ts[,1])
  
  dates <- c()

    for (j in 1:length(insituLFMC.month.ts)) {
      date <- format(as.Date(time(insituLFMC.month.ts[j]), format = "%Y-%m-%d"))
      if (date %in% names(insituLFMC)) {
        insituLFMC[i, which(names(insituLFMC) == date)] <- insituLFMC.month.ts[j]
      }else{
        insituLFMC[i, ncol(insituLFMC) + 1] <- insituLFMC.month.ts[j]
        names(insituLFMC)[ncol(insituLFMC)] <- date
        #for (i in 2:nrow(insituLFMC)) {
         # insituLFMC[i, ncol(insituLFMC)] <- NA
        }
      dates <- c(dates, as.Date(date, format = "%Y-%m-%d"))
    }
    }
  

insituLFMC <-  insituLFMC[, c(1:3, order(names(insituLFMC)))]

#make regressions for the whole dataset.

#change name of three df columns
colnames(VOD2LFMC.sites)[211:213] <- c("2017-05-01", "2017-06-01", "2017-07-01")

#remove df last three columns
insituLFMC <- select(insituLFMC, -lat.1, -lon.1, -site.1)


par(mfrow=c(4,4))

plots <- list()
plots.D <- list()
plots.W <- list()

#Sites with most Insitu data

  na_counts_insitu <- rowSums(is.na(insituLFMC))
  na_counts_insitu.sorted <- sort(na_counts_insitu, decreasing = FALSE)


  plot_conditions <- function(i){
  
  
  wetdates <- names(global.spi[,4:ncol(global.spi)])[apply(global.spi[i,4:ncol(global.spi)], 1, function(x) x > 1)]
  drydates <- names(global.spi[,4:ncol(global.spi)])[apply(global.spi[i,4:ncol(global.spi)], 1, function(x) x < -1)]
  
  
  combinedLFMC <- rbind(insituLFMC[i,], VOD2LFMC.sites[i,])
  
  # Delete columns with NA values
  combinedLFMC <- combinedLFMC[, colSums(is.na(combinedLFMC)) == 0]
  
  #combinedLFMC <- na.omit(combinedLFMC)
  
  combinedLFMC_wet <- combinedLFMC[, (names(combinedLFMC) %in% wetdates | names(combinedLFMC) == "site" | 
                                        names(combinedLFMC) == "lon" | names(combinedLFMC) == "lat" )]
  combinedLFMC_dry <- combinedLFMC[, names(combinedLFMC) %in% drydates | names(combinedLFMC) == "site" | 
                                     names(combinedLFMC) == "lon" | names(combinedLFMC) == "lat"]
  combinedLFMC_normal <- combinedLFMC[, !names(combinedLFMC) %in% drydates & !names(combinedLFMC) %in% wetdates]
  
 
  'combinedLFMC$color_var <- ifelse(!names(combinedLFMC) %in% drydates & !names(combinedLFMC) %in% drydates, "Not Included", 
                         ifelse(names(combinedLFMC) %in% drydates, "Included in Dry", 
                                ifelse(names(combinedLFMC) %in% wetdates, "Included in Wet", "Included in both")))'
  
  row1n <- as.numeric(combinedLFMC_normal[1,4:ncol(combinedLFMC_normal)])
  row2n <- as.numeric(combinedLFMC_normal[2,4:ncol(combinedLFMC_normal)])
  
  row1w <- as.numeric(combinedLFMC_wet[1,4:ncol(combinedLFMC_wet)])
  row2w <- as.numeric(combinedLFMC_wet[2,4:ncol(combinedLFMC_wet)])
  
  row1d <- as.numeric(combinedLFMC_dry[1,4:ncol(combinedLFMC_dry)])
  row2d <- as.numeric(combinedLFMC_dry[2,4:ncol(combinedLFMC_dry)])
  
  'print(row1n)
  print(row1d)
  print(row1w)'
  
  
 plotn <- ggplot(data.frame(row1n, row2n), aes(row1n, row2n)) +
    geom_point(color = "#99CC99") +
    #geom_smooth(method = "lm", formula = row2d ~ row1d, color = "red")+
    scale_x_continuous(name = "insitu LFMC", limits = c(40, 140))+
    scale_y_continuous(name = "insitu LFMC", limits = c(20, 200))+
    #geom_abline(intercept = coef(lm(row1n ~ row2n, na.action = na.exclude))[1], slope = coef(lm(row1n ~ row2n, na.action = na.exclude))[2], colour = "#99CC99")+
    geom_abline(intercept = 0, slope = 1, color = "black")+ # 1:1 line = theoretical optimal fit 
    ggtitle(paste("Linear Regression for", combinedLFMC$site[1], "Normal conditions"))
    
  plotd <- ggplot(data.frame(row1d, row2d), aes(row1d, row2d)) +
    geom_point(color = "#CC9933") +
    #geom_smooth(method = "lm", formula = row2d ~ row1d, color = "red")+
    scale_x_continuous(name = "insitu LFMC", limits = c(40, 140))+
    scale_y_continuous(name = "insitu LFMC", limits = c(20, 200))+
    #geom_abline(intercept = coef(lm(row1d ~ row2d, na.action = na.exclude))[1], slope = coef(lm(row1d ~ row2d, na.action = na.exclude))[2], colour = "#CC9933")+
    geom_abline(intercept = 0, slope = 1, color = "black")+ # 1:1 line = theoretical optimal fit
    ggtitle(paste("Linear Regression for", combinedLFMC$site[1] , "Dry conditions"))
  
  plotw <- ggplot(data.frame(row1w, row2w), aes(row1w, row2w)) +
    geom_point(color = "#006699") +
    #geom_smooth(method = "lm", formula = row2d ~ row1d, color = "red")+
    scale_x_continuous(name = "insitu LFMC", limits = c(40, 140))+
    scale_y_continuous(name = "insitu LFMC", limits = c(20, 200))+
    #geom_abline(intercept = coef(lm(row1w ~ row2w, na.action = na.exclude))[1], slope = coef(lm(row1w ~ row2w, na.action = na.exclude))[2], colour = "#006699")+
    geom_abline(intercept = 0, slope = 1, color = "black")+ # 1:1 line = theoretical optimal fit
    ggtitle(paste("Linear Regression for", combinedLFMC$site[1] , "Wet conditions"))
  
  plots[[i]] <- plotn
  plots.D[[i]] <- plotd
  plots.W[[i]] <- plotw
  
  grid.arrange(plots.D[[i]], plots[[i]], plots.W[[i]], ncol = 3)
  
}

  
  plot_conditions(45)



grid.arrange(plots.D[[2]], plots[[2]], plots.W[[2]], ncol = 3)

#grid.arrange(grobs = plots, ncol = 2)

?geom_abline
?grid.arrange
par(mfrow=c(2,2))

 

plot(plots)
# 3 Model evaluation: how good is the model?
#-------------------------------------------

# a basic plot in model evaluation is a scatter plot of simulated against observed values
plot(data$gpp.obs, gpp.sim, pch=16, 
     xlab=paste("Observed", label), 
     ylab=paste("Simulated", label))
abline(0,1, col="blue") # 1:1 line = theoretical optimal fit
# What are you learning from this figure?
# simulated values tend to be lower than observed values

# to quantify the model performance, we could compute the RMSE and correlation
RMSE <- function(sim, obs) {
  sqrt(sum((sim - obs)^2) / length(sim)) # root mean squared error
} 
RMSE(gpp.sim, data$gpp.obs) 
cor(gpp.sim, data$gpp.obs) # correlation (base R function)


?grid.arrange

#make regressions for wet season

wetdates <- spi.px.ts[ which(spi.px.ts[,2] > 1)]
combinedLFMC.wet <- rbind(insituLFMC[1,], VOD2LFMC.sites[1,], wetdates)



