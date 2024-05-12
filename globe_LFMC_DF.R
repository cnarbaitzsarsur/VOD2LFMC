#install.packages("zoo") # -> run only if the package is not yet installed
#install.packages("tidyverse") 
#install.packages("fs")
#install.packages("terra")
#install.packages("hydroTSM")
#install.packages("pacman")
#install.packages("gridExtra")
install.packages("svglite")
install.packages("shapefiles")
install.packages("maptools")


pacman::p_load(
  lubridate,  # general package for handling and converting dates  
  parsedate,   # has function to "guess" messy dates
  aweek,      # another option for converting dates to weeks, and weeks to dates
  zoo,        # additional date/time functions
  tidyverse,  # data management and visualization  
  rio)        # data import/export
pacman::p_load(lubridate)


library(zoo)
library(terra)
library(tidyverse)
library("dplyr")
library(fs)
library("hydroTSM")
library(gridExtra)
library(ggplot2)
library(gridExtra)
library("tmap")
library(tidyr)
library("svglite")
library(sf)
library(shapefiles)
library(maptools)


?nchar

# CREATE DATA FRAME WITH ALL THE CSV FILES --------------------------------------------------------------------
      
  setwd("/Users/camilanarbaitzsarsur/Desktop/07_facultad/msc_cartography/tud/remote-sensing/E07_final-exercise/")
      
  # 1. Read the Globe LFMC data from a csv file
      
    # Vector with file names
    names = list.files("data/Globe-LFMC/", pattern="*.csv")
      
    results.df <- data.frame(site=NA, lat=NA, lon=NA, RMSE=NA)
    
    #read.csv("E07_final-exercise/data/Globe-LFMC/", sep=";", header = TRUE, na.strings = "F")
      
    fileData <- list()
      
      for(fileNum in 1:length(names)){
        fileData[[fileNum]] <- read.csv(file = paste0("data/Globe-LFMC/",names[fileNum])
                                          , sep = ";", header = TRUE, na.strings = "F");
        }
      
      
      for (i in 1:length(names)) {
         results.df[i,1] <- fileData[[i]]$Sitename[1]
         results.df[i,2] <- fileData[[i]]$Latitude[1]
         results.df[i,3] <- fileData[[i]]$Longitude[1]
         }

# DEFINE DROUGHT AND WET SEASON BY LOOKING AT THE SPI DATA --------------------------------------------------------------------

  # 1. Read SPI datafile
  
    setwd("/Users/camilanarbaitzsarsur/Desktop/07_facultad/msc_cartography/tud/remote-sensing/E07_final-exercise/data/")
    
    file.SPI <- "us49_southwest_reg_spg12_m_wld_19990101_20211201_m.nc"
    spi <- rast(file.SPI)
    
    plot(spi,1)
    summary(spi)

  # 2. Define coordinates
  
    par(mfrow=c(1,2))
    
     for (i in 1:length(names)) {
        coord <- cbind(results.df[i,3], results.df[i,2])
        points( coord, pch=16)
        #text(coord, c(results.df[i,1]), pos = 1)
      }
  
  # 3. Create a data frame with the global SPI just with the chosen sites
    
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

  # 4. Add Average value of SPI by months on the whole series to find most wet and most dry
  
    for (i in 4:length(global.spi)){
      global.spi[63,i] <- mean(global.spi[, i], na.rm=TRUE)
    }

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


# CREATE LINEAR REGRESSION -----------------------------------------------------------------------------------------
# for drought and wet conditions, look for the LFMC model and in situ measurements 
       
  # 1. Get LFMC model to monthly data
  
          setwd("/Users/camilanarbaitzsarsur/Desktop/07_facultad/msc_cartography/tud/remote-sensing/E07_final-exercise/data/")
          
          file.VOD2LFMC <- "us49_southwest_reg_VOD2LFMC-B_v01_2000-02.2017-07.nc"
          VOD2LFMC <- rast(file.VOD2LFMC)
          
          plot(VOD2LFMC,2)

      # Convert model to time series monthly and create table
      
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


          #Testing locations that have NA to see if it is correct. Yes, they are "outside" the map
          
            # coord <- cbind(results.df[51,3], results.df[51,2])
            # 
            # plot(VOD2LFMC,2)
            # points(coord, pch=3)
    
          #Convert data to zoo time series with LFMC.values
          #insituLFMC.ts <- zoo(fileData[[1]][["LFMC.value"]], ymd(fileData[[1]][["Sampling.date"]]))

    
      
  # 2. Get INSITU LFMC to monthly data
      
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
          

  # 3. Make regressions for the whole dataset.

          #change name of three df columns
          colnames(VOD2LFMC.sites)[211:213] <- c("2017-05-01", "2017-06-01", "2017-07-01")
  
          #remove df last three columns
          insituLFMC <- subset(insituLFMC, select = -lat.1)
          insituLFMC <- subset(insituLFMC, select = -lon.1)
          insituLFMC <- subset(insituLFMC, select = -site.1)
       
          
          
          #function for ploting regressions on all conditions
          plot_conditions <- function(i){
            
          wetdates <- names(global.spi[,4:ncol(global.spi)])[apply(global.spi[i,4:ncol(global.spi)], 1, function(x) x > 1)]
          drydates <- names(global.spi[,4:ncol(global.spi)])[apply(global.spi[i,4:ncol(global.spi)], 1, function(x) x < -1)]
          
          
          combinedLFMC <- rbind(insituLFMC[i,], VOD2LFMC.sites[i,])
          combinedLFMC <- combinedLFMC[, colSums(is.na(combinedLFMC)) == 0]
          
          combinedLFMC_wet <- combinedLFMC[, (names(combinedLFMC) %in% wetdates | names(combinedLFMC) == "site" | 
                                                names(combinedLFMC) == "lon" | names(combinedLFMC) == "lat" )]
          combinedLFMC_dry <- combinedLFMC[, names(combinedLFMC) %in% drydates | names(combinedLFMC) == "site" | 
                                             names(combinedLFMC) == "lon" | names(combinedLFMC) == "lat"]
          combinedLFMC_normal <- combinedLFMC[, !names(combinedLFMC) %in% drydates & !names(combinedLFMC) %in% wetdates]
          
          row1n <- as.numeric(combinedLFMC_normal[1,4:ncol(combinedLFMC_normal)])
          row2n <- as.numeric(combinedLFMC_normal[2,4:ncol(combinedLFMC_normal)])
          
          row1w <- as.numeric(combinedLFMC_wet[1,4:ncol(combinedLFMC_wet)])
          row2w <- as.numeric(combinedLFMC_wet[2,4:ncol(combinedLFMC_wet)])
          
          row1d <- as.numeric(combinedLFMC_dry[1,4:ncol(combinedLFMC_dry)])
          row2d <- as.numeric(combinedLFMC_dry[2,4:ncol(combinedLFMC_dry)])

          modelLFMC <- lm(row1n ~ row2n)
          modelLFMC.d <- lm(row1d ~ row2d)
          modelLFMC.w <- lm(row1w ~ row2w)
          
          # calculate RMSE
          RMSE <- c(sqrt(mean(modelLFMC$residuals^2)), 
                    sqrt(mean(modelLFMC.w$residuals^2)), 
                    sqrt(mean(modelLFMC.d$residuals^2)))
          
         '#CORRELATION POSITIVE OR NEGATIVE
            #Extract the response and predictor variables
                response <- modelLFMC$model[, "row1n"]
                predictor <- modelLFMC$model[, "row2n"]
            # Compute the correlation between the response and predictor variables
                correlation <- cor(response, predictor)
                correlation'
          
         plot <- ggplot() +
           geom_point(data = data.frame(row1n, row2n), aes(row1n, row2n, color = "Label 1")) +
           geom_smooth(data = data.frame(row1n, row2n), aes(row1n, row2n, color = "Label 1", fill = "Label 1"), method = "lm", formula = y ~ x) +
           geom_point(data = data.frame(row1w, row2w), aes(row1w, row2w, color = "Label 2")) +
           geom_smooth(data = data.frame(row1w, row2w), aes(row1w, row2w,color = "Label 2", fill = "Label 2"), method = "lm", formula = y ~ x) +
           geom_point(data = data.frame(row1d, row2d), aes(row1d, row2d, color = "Label 3")) +
           geom_smooth(data = data.frame(row1d, row2d), aes(row1d, row2d, color = "Label 3", fill = "Label 3"), method = "lm", formula = y ~ x) +
           geom_abline(intercept = 0, slope = 1, color = "gray") + 
           xlab("insitu LFMC") +
           ylab("model LFMC") +
           theme(axis.title.x = element_text(colour = "#293352"),
                  axis.title.y = element_text(colour = "#293352"))+
           ggtitle(paste("Linear Regression for", combinedLFMC$site[1])) +
           theme(title = element_text(colour = "#293352")) +
           scale_color_manual(values = c("Label 1" = "#52854C", "Label 2" = "#4E84C4", "Label 3" = "#FFDB6D"),
                              labels = c("Label 1" = "Normal conditions", "Label 2" = "Wet conditions", "Label 3" = "Dry conditions"),
                              guide = guide_legend(title = "")) +
           scale_fill_manual(values = c("Label 1" = "#52854C", "Label 2" = "#4E84C4", "Label 3" = "#FFDB6D"),
                             labels = c("Label 1" = "Normal conditions", "Label 2" = "Wet conditions", "Label 3" = "Dry conditions"),
                             guide = guide_legend(title = ""))+
          annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1, 
                   label = paste0("RMSE Normal: ", round(RMSE[1], 2), "\n",
                                  "RMSE Wet: ", round(RMSE[2], 2), "\n",
                                  "RMSE Dry: ", round(RMSE[3], 2)))
         
         plot
        
          }
          
     
          #Plot of three places with relevant amount of insitu data
          #Sites with most Insitu data
          
          na_counts_insitu <- rowSums(is.na(insituLFMC))
          na_counts_insitu.sorted <- sort(na_counts_insitu, decreasing = FALSE)
          
          #The sites 45, 34 and 36 where chosen
          LR_ReaderRanch <- plot_conditions(45)
          LR_LaTunaCanyon <- plot_conditions(34)
          LR_LaurelCanyon <- plot_conditions(36)
          LR_TylerFoote <- plot_conditions(60)
          LR_USBOR <- plot_conditions(61)
          LR_d10FlagstaffMtn <- plot_conditions(16)
          LR_SaulsCreek <- plot_conditions(50)
          
          plot_conditions(16)
          
    
          ggsave(file = "LR_ReaderRanch.svg",LR_ReaderRanch , width = 7, height = 7, units = "in", dpi = 300)
          ggsave(file = "LR_LaTunaCanyon.svg",LR_LaTunaCanyon , width = 7, height = 7, units = "in", dpi = 300)
          ggsave(file = "LR_LaurelCanyon.svg",LR_LaurelCanyon , width = 7, height = 7, units = "in", dpi = 300)
          ggsave(file = "LR_TylerFoote.svg",LR_TylerFoote , width = 7, height = 7, units = "in", dpi = 300)
          ggsave(file = "LR_USBOR.svg",LR_USBOR , width = 7, height = 7, units = "in", dpi = 300)
          ggsave(file = "LR_d10FlagstaffMtn.svg",LR_d10FlagstaffMtn , width = 7, height = 7, units = "in", dpi = 300)
          ggsave(file = "LR_SaulsCreek.svg",LR_SaulsCreek , width = 7, height = 7, units = "in", dpi = 300)
          
        
          #
          # Fit a linear model
          fit <- lm(y ~ x)
          
          # Generate predictions
          predictions <- predict(modelLFMC, newdata = data.frame(row2n = row2n))
          
          # Calculate the RMSE
          rmse <- sqrt(mean((row1n - predictions)^2))
          
          # Print the RMSE
          rmse
          This will return the RMSE as a single numeric value. In this example, the RMSE is calculated as the square root of the mean of the squared differences between the actual y values and the predicted values obtained from the linear model.
          
          
          
          
          
          
# MAPS --------------------------------------------------------------------------------------------------------

          # Make a map showing the points with their wetest and droughtest dates to see if the changing in dates seeing
          # in the SPI time series has a relation with the location of the points
          
          # 1. Create a new column in global.spi with max and min of each place
          
          global.spi$SPI.min <- apply(global.spi[c(4:243)],1, min)
          global.spi$SPI.max <- apply(global.spi[c(4:243)], 1, max)
          
          global.spi$SPI.min.month <- apply(global.spi[c(4:243)], 1, function(x){
                                      min_index <- which.min(x)
                                      return(colnames(global.spi)[min_index])
                                      })
          
          global.spi$SPI.max.month <- apply(global.spi[c(4:243)], 1, function(x){
                                      max_index <- which.max(x)
                                      return(colnames(global.spi)[max_index])
                                      })
          
          # 3. Create a simple feature with the global.spi data frame
         
          global.spi.minmax <- global.spi[-63,-(4:243)] # delete row no 63 and leave just columns with max and min
          global.spi1 <- global.spi[-63,]
          selected_points <- subset(global.spi1, row.names(global.spi1) %in% c(45,34,36,60,61,16,48))
          

          global.spi.sf <- st_as_sf(global.spi1, coords = c("lon", "lat"))
          global.spi.minmax.sf <- st_as_sf(global.spi.minmax, coords = c("lon", "lat"))
          selected_points.sf <- st_as_sf(selected_points, coords = c("lon", "lat"))
          
          
          
          # Change format on dates to better readability
          global.spi.minmax.sf$SPI.min.month <- as.Date(global.spi.minmax.sf$SPI.min.month, format = "%Y-%m-%d")
          global.spi.minmax.sf$SPI.min.month <- format(global.spi.minmax.sf$SPI.min.month, format = "%Y %B") 
          
          global.spi.minmax.sf$SPI.max.month <- as.Date(global.spi.minmax.sf$SPI.max.month, format = "%Y-%m-%d")
          global.spi.minmax.sf$SPI.max.month <- format(global.spi.minmax.sf$SPI.max.month, format = "%Y %B")
          
        
          # 4. Make a map with all the points
          
            #Create a boundin box with the wanted geograhic area
          
            my_bbox <- c(xmin=min(global.spi.minmax$lon),
                         ymin=min(global.spi.minmax$lat),
                         xmax=max(global.spi.minmax$lon+6),
                         ymax=max(global.spi.minmax$lat))
            
            #extract data from OSM
          
            setwd("/Users/camilanarbaitzsarsur/Desktop/07_facultad/msc_cartography/tud/remote-sensing/E07_final-exercise/data")
            coast <- shapefile("/Users/camilanarbaitzsarsur/Desktop/07_facultad/msc_cartography/tud/remote-sensing/E07_final-exercise/data/ne_50m_coastline.shp") # File from exercise 3
            
            # 4a. Map with all the data data and selected points
            
            

            tmap_mode("view")
              tm_shape(coast, bbox = my_bbox)+ tm_lines(alpha = 0.7) +
              tm_shape(global.spi.sf, projection="longlat")+ tm_symbols(col = "#BFBFC1")+
              tm_shape(selected_points.sf, projection = "longlat")+ 
              tm_symbols(col = "#731055", alpha = 0.7)
              
              tmap_save(map_plot,"selectedpoints.svg", width = 10, height = 8)
            
            ?tmap_save
              
            
            
      
            # 4b. Map with just min and max data

            tmap_mode("view")

            tm_shape(coast, bbox = my_bbox)+ tm_lines(alpha = 0.7) +
            tm_shape(global.spi.minmax.sf, projection="longlat")+ tm_bubbles(col = c("SPI.min.month","SPI.max.month")
                                                                                     , palette = c("#d64e12", "#f9a52c", "#efdf48","#8bd346", "#60dbe8", "#16a4d8"))
            
            