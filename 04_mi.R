install.packages("ncdf4")
install.packages("spatstat")
library(terra)
library(zoo)
library(ncdf4)
library(raster)
library(sp)
library(spatstat)

setwd("/Users/camilanarbaitzsarsur/Desktop/07_facultad/msc_cartography/tud/remote-sensing/E07_final-exercise")

#VOD_______________________________________________________________________________________________________________________

# VOD2LFMC - estimated from daily Ku-VOD and monthly MODIS LAI based on model B as described in Forkel et al. (2023)
            
            setwd("./data")
            lfmc.r <- rast("us49_southwest_reg_VOD2LFMC-B_v01_2000-02.2017-07.nc", subds="LFMC_med")
            lfmc.r
            plot(lfmc.r)
            summary(lfmc.r)

# Prepare lfmc SpatRaster - monthly
            date.lfmc <- time(lfmc.r) # Get the date from lfmc
            start.lfmc <- grep(as.Date("2007-01-01"), date.lfmc) # get number of first layer from lfmc SpatRaster for our period
            end.lfmc <- grep(as.Date("2017-07-31"), date.lfmc) # get number of last layer from lfmc SpatRaster for our period
            
            
            lfmc.rsub <- subset(lfmc.r, start.lfmc:end.lfmc) # 2001.01.01 - 2002.12.31
            lfmc.rsub
            
            lfmc.rsub.mon <- tapp(lfmc.rsub, index="yearmonths", fun="mean", na.rm=TRUE) # Calculate the mean for the SpatRaster
            lfmc.rsub.mon
            plot(lfmc.rsub.mon,12)
            
            
# Prepare lfmc SpatRaster - mean of all years
            years <-format(lfmc.rsub, "%Y")# extract the years from the date
            lfmc.rsub.yr <- tapp(lfmc.rsub, index="years", fun="mean", name="year", na.rm=TRUE) # Be patient, this might take some time!
            lfmc.rsub.yr
            plot(lfmc.rsub.yr)
            
            # Calculate the mean of all the rasters in the stack
            lfmc.mean <- mean(lfmc.rsub.yr) 
            
            # Plot the mean raster
            plot(lfmc.mean)
            
# Prepare lfmc SpatRaster - for draught and wet month
            date.lfmc <- time(lfmc.r) # Get the date from lfmc
            start.lfmc <- grep(as.Date("2002-08-01"), date.lfmc) # get number of first layer from lfmc SpatRaster for our period
            end.lfmc <- grep(as.Date("2002-08-31"), date.lfmc) # get number of last layer from lfmc SpatRaster for our period
            
            lfmc.rsub.dry <- subset(lfmc.r, start.lfmc:end.lfmc) # 2001.01.01 - 2002.12.31
            lfmc.rsub.dry
            plot(lfmc.rsub.dry)
            lfmc.dry <- tapp(lfmc.rsub.dry, index="yearmonths", fun="mean", na.rm=TRUE) # Calculate the mean for the SpatRaster
            plot(lfmc.dry)
            
            start.lfmc <- grep(as.Date("2005-03-01"), date.lfmc) # get number of first layer from lfmc SpatRaster for our period
            end.lfmc <- grep(as.Date("2005-03-31"), date.lfmc) # get number of last layer from lfmc SpatRaster for our period
            lfmc.rsub.wet <- subset(lfmc.r, start.lfmc:end.lfmc) # 2001.01.01 - 2002.12.31
            lfmc.rsub.wet
            plot(lfmc.rsub.wet)
            lfmc.wet <- tapp(lfmc.rsub.wet, index="yearmonths", fun="mean", na.rm=TRUE) # Calculate the mean for the SpatRaster
            plot(lfmc.wet)
            
            #setwd("/Users/camilanarbaitzsarsur/Desktop/07_facultad/msc_cartography/tud/remote-sensing/E07_final-exercise/01_ts-global-lfmc/global-lfmc")
            
                  library(RColorBrewer) # for nice colour palettes
                  display.brewer.all() # These are the palettes from RColorBrewer
                  source("Breaks.R") # This is a function to compute class breaks for plotting
                  source("BreakColours.R") # This is a function to interpolate colour palettes for class breaks
                  
                  ?RColorBrewer
                  
                  # compute class breaks and create the colour palette
                  #brks.dry <- Breaks(lfmc.dry) # use the function Breaks to compute class breaks
                  #brks.wet <- Breaks(lfmc.wet) # use the function Breaks to compute class breaks
                  b.lfmc <- seq(0,450, by=50)
                  cols.lfmc <- BreakColours(b.lfmc, pal="YlGn") # use the PiYG palette
            
                  
            plot(lfmc.dry,col=cols.lfmc, main = "LFMC Map for Driest Month")
            plot(lfmc.wet, col=cols.lfmc, main = "LFMC Map for Wettest Month")



#SPI_______________________________________________________________________________________________________________________

# Standardised Precipitation Index (SPI-12)
setwd("./data")
prep.r <- rast("us49_southwest_reg_spg12_m_wld_19990101_20211201_m.nc")

#plot(prep.r)
prep.r.resamp <- resample(prep.r, lfmc.r, method="bilinear") # resample the precipitation index data to get same spatial resolution as lfmc.
plot(prep.r.resamp)


###### Prepare precipitation SpatRaster
date.prep <- time(prep.r.resamp) # Get the date from precipitation

start.prep <- grep(as.Date("2007-01-01"), date.prep) # get number of first layer from precipitation SpatRaster for our period
end.prep <- grep(as.Date("2007-12-31"), date.prep) # get number of last layer from precipitation SpatRaster for our period (temporal resolution only monthly, so 2001-12-31 does not exist)

prep.rsub <- subset(prep.r.resamp, start.prep:end.prep) 
prep.rsub

library(sp)
library(raster)

# Load the SpatRaster object
prep.rsub <- brick(prep.rsub)

# Calculate the mean for each layer
mean_values <- calc(prep.rsub, mean)

# Find the layer with the minimum mean
min_mean_layer <- which.min(mean_values)
min_mean_month <- names(prep.rsub)[min_mean_layer]

# Find the layer with the maximum mean
max_mean_layer <- which.max(mean_values)
max_mean_month <- names(prep.rsub)[max_mean_layer]

# Print the results
cat("The layer with the minimum mean is", min_mean_month, "with a mean of", mean_values[min_mean_layer], "\n")
cat("The layer with the maximum mean is", max_mean_month, "with a mean of", mean_values[max_mean_layer], "\n")


# Prepare lfmc SpatRaster - for draught and wet month
            date.spi <- time(prep.r) # Get the date from lfmc
            start.spi <- grep(as.Date("2002-08-01"), date.spi) # get number of first layer from lfmc SpatRaster for our period
            end.spi <- grep(as.Date("2002-08-01"), date.spi) # get number of last layer from lfmc SpatRaster for our period
            
            spi.rsub.dry <- subset(prep.r, start.spi:end.spi) # 2001.01.01 - 2002.12.31
            spi.rsub.dry
            plot(spi.rsub.dry)
            #spi.dry <- tapp(spi.rsub.dry, index="yearmonths", fun="mean", na.rm=TRUE) # Calculate the mean for the SpatRaster
            spi.dry <- spi.rsub.dry
            spi.dry
            #plot(spi.dry)
            
            start.spi <- grep(as.Date("2005-03-01"), date.spi) # get number of first layer from lfmc SpatRaster for our period
            end.spi <- grep(as.Date("2005-03-01"), date.spi) # get number of last layer from lfmc SpatRaster for our period
            spi.rsub.wet <- subset(prep.r, start.spi:end.spi) # 2001.01.01 - 2002.12.31
            spi.rsub.wet
            spi.wet<-spi.rsub.wet
            plot(spi.wet)
            #lfmc.wet <- tapp(lfmc.rsub.wet, index="yearmonths", fun="mean", na.rm=TRUE) # Calculate the mean for the SpatRaster
            
            #setwd("/Users/camilanarbaitzsarsur/Desktop/07_facultad/msc_cartography/tud/remote-sensing/E07_final-exercise/01_ts-global-lfmc/global-lfmc")
            
            
                    library(RColorBrewer) # for nice colour palettes
                    display.brewer.all() # These are the palettes from RColorBrewer
                    source("Breaks.R") # This is a function to compute class breaks for plotting
                    source("BreakColours.R") # This is a function to interpolate colour palettes for class breaks
                    # compute class breaks and create the colour palette
                    b <- seq(-4,4, by=0.5)
                    cols.spi <- BreakColours(b, pal="Blues") # use the PiYG palette
                    
            
            plot(spi.dry,col=cols.spi,breaks=b, main = "SPI Map for Driest Month")
            plot(spi.wet, col=cols.spi,breaks=b, main = "SPI Map for Wettest Month")



#SWI_______________________________________________________________________________________________________________________

# Soil Water Index
swi<- "us49_southwest_reg_c_gls_SWI12.5km_ASCAT.20070101.20211121.10.days.nc"
swi.r<- rast(swi,subds="VOBS_010")
swi.r
plot(swi.r)
summary(swi.r)


#swi.agg.r <- aggregate(swi.r, 2, fun=mean)
#swi.agg.r 
#swi.0d25.r <- resample(swi.agg.r, lfmc.r)

swi.r.resamp <- resample(swi.r, lfmc.r, method="bilinear") # resample the swi data to get same spatial resolution as lfmc.
plot(swi.r.resamp)

###### Prepare swi SpatRaster
date.prep <- time(swi.r.resamp) # Get the date from precipitation

start.prep <- grep(as.Date("2007-01-01"), date.prep) # get number of first layer from precipitation SpatRaster for our period
end.prep <- grep(as.Date("2017-07-01"), date.prep) # get number of last layer from precipitation SpatRaster for our period (temporal resolution only monthly, so 2001-12-31 does not exist)

swi.rsub <- subset(swi.r.resamp, start.prep:end.prep) # 2001.01.01 - 2002.12.31
swi.rsub

swi.rsub.mon <- tapp(swi.rsub, index="yearmonths", fun="mean", na.rm=TRUE) # Calculate the mean for the SpatRaster
swi.rsub.mon
plot(swi.rsub.mon)




#Correlation_____________________________________________________________________________________________________________

###### LFMC x SPI
lfmc.br <- raster::brick(lfmc.rsub.mon) # import only brick function from raster package
lfmc.br
prep.br <- raster::brick(prep.rsub) # import only brick function from raster package
prep.br

?corLocal
map.corr <- raster::corLocal(lfmc.br,prep.br) # calculate local correlation coefficient
map.corr
plot(map.corr) # plot





###### LFMC x SWI
lfmc.br <- raster::brick(lfmc.rsub.mon) # import only brick function from raster package
lfmc.br
swi.br <- raster::brick(swi.rsub.mon) # import only brick function from raster package
swi.br

#?corLocal
map.corr <- raster::corLocal(lfmc.br,swi.br) # calculate local correlation coefficient
map.corr
plot(map.corr) # plot





###### SPI x SWI
prep.br <- raster::brick(prep.rsub) # import only brick function from raster package
prep.br
swi.br <- raster::brick(swi.rsub.mon) # import only brick function from raster package
swi.br

#?corLocal
map.corr <- raster::corLocal(prep.br,swi.br) # calculate local correlation coefficient
map.corr
plot(map.corr) # plot


# SPI 

