
###############################################################
#################################################################
#  
#	Enquist Lab Tutorial for Mapping Species Distribution Data
#		by Noah Charney and Brian Enquist - University of Arizona
#						        December 2014
#
################################################################
#################################################################

#	To follow along, once you set up your working directory 
#	in step #1, all you need to do is run each line:
#	depending on your platform, with your cursor on a line, 
#	typically you just hit, "Ctrl-R", or "Ctrl-Enter"
#
#	Remember, if you see an unfamiliar function, like "stack(...)"
#	you can read about it by typing "?stack" into the console
#

# -0- load necessary libraries
#you only need to run this first line if you haven't already
#downloaded these packages to your computer:

install.packages(c('raster','maptools','dismo'), dependencies=TRUE)

library(raster)
library(maptools)
library(dismo)

# -1- Set up working directory
# setwd('C:/Tutorial_Macro/') 	#write the pathname of the directory with
#the tutorial files here

setwd ("/Users/benquist/Desktop/Tutorial_Macro_bje/")   # for my mac this is
# how I set it up


# -2- Either Download "current" and "future" climate data 
#	from worldclim or access it from the class Dropbox folder
#	To downlaod from a Web Browser, go to "http://www.worldclim.org/Download"
#	CURRENT: Click on "Current," 
#		then find "Bioclim" under "Generic Grids: 10 arc-minutes"
#		download this file and unzip it to "Tutorial/bioclim_current/"
#	FUTURE: Click on "Future," then click on "10 minutes"
#		pick your favorite scenario/GCM, and click on "bi"
#		download this file and unzip it to "Tutorial/bioclim_future/"
#		
#	Note, that the current files we have downloaded are in ".bil" format
#	named as 'bio1.tif, bio2.tif, ..., bio19.tif'
#	But the future files are ".tif" format, with a naming convention
#		model, scenario, variables, year, bioclim layer
#	For instance, the file 'ac45bi5012.tif' is the following raster:
#		MODEL: ACCESS1-0
#		SCENARIO: RCP 45
#		VARIABLES: bioclim
#		YEAR:	2050
#		LAYER: bio12


# -3- read climate layers into R
#generate the names of the current & future climate files:
current.names <- sort(paste('bioclim_current/bio',1:19,'.bil',sep=''))
future.names <- list.files('bioclim_future',full.names=TRUE)

#check to make sure the two sets of file names contain 19 layers 
#	in the same order:
cbind(current.names,future.names)

#create multi-layer raster "stacks" of each set of layers
current.stack <- stack(current.names)
future.stack  <- stack(future.names)

names(future.stack) <- names(current.stack)	#names must match for SDM


# -4- read in political boundaries
states.shp <- readShapePoly('background_layers/US_Canada')

# -5- Read in point data (choose your favorite species)
point.data <- read.csv('point_data/Abies_lasiocarpa.csv')	
#inspect the first few lines:
head(point.data)

# -6- Crop climate layers to the extent of our species' points

x.limits 		<- range(point.data[,'x'])
y.limits 		<- range(point.data[,'y'])
crop.extent 	<- extent(c(x.limits,y.limits))
current.stack 	<- crop(current.stack,crop.extent)
future.stack 	<- crop(future.stack,crop.extent)

# -7- Read in range map (choose matching species code)
range.map <- readShapePoly('range_maps/abielasi')
#note that this layer is missing its projection information:
projection(range.map)

#so we need to define the projection
projection(range.map) <- CRS("+proj=longlat +datum=NAD27 +ellps=clrk66 +a=6378206.4 +rf=294.98")

# -8- Choose which layers to include in the model fit
cbind(1:19,names(current.stack))			#note the alphabetized bioclim order 
f.layers <- c(1,5,7:14,18)				#the numbers I chose are totally arbitrary
current.stack <- current.stack[[f.layers]]	#replace stacks with stacks of only 
future.stack <- future.stack[[f.layers]]		#	the selected layers

# -9- Fit species distribution model

### note here we are using the bioclim algorithm. Not maxent but very similar. For maxent see below . . .
## to learn more type the following ?bioclim

sdm.fit<- bioclim(x=current.stack,			#fit model to current climate
                  p=point.data[,-1]			#the '-1' excludes the first column
)
cur.prediction <- predict(object=sdm.fit,		#use the fit model
                          x=current.stack)	#and project to the current

fut.prediction <- predict(object=sdm.fit,		#use the fit model
                          x=future.stack)	#and project to the future

# -10- PLOT raw data
plotting.layer <- current.stack[[1]]#the "1" selects the first layer in the stack, try another!

#just for fun, let's set new y limits for plotting,
extent(plotting.layer)					#examine full extent
y.limits <- c(35,49)					#units here are lat/lon
x.limits <- c(extent(plotting.layer)@xmin,	#but use the full extent for x limits
              extent(plotting.layer)@xmax)		#

#(actually we chose the y limits specifically to fit nicely for part of "abla"
#within the dimensions of the plotting frame. You'll note with different 
#dimensions, distracting spaces with no data appear in your plots below. 
#Other solutions are to resize the window, change par('mai'), plot the 
#uncropped rasters behind, or ignore the spaces.)

plot(plotting.layer, 
     main='Subalpine Fir',		#plot title
     xlim = x.limits,			#if you want the full extent, then 
     ylim = y.limits,			#	exclude these two arguments
     xlab='longitude',			#can also add marginal text later with mtext()
     ylab='latitude'
)
plot(states.shp,add=TRUE,		#add state boundaries
     border=gray(0.3)			#line color, adjust gray by changing number
)
points(	x=point.data[,'x'],	#add point data
        y=point.data[,'y'],
        col= 'cyan',		#point color
        cex=0.2			#point size
)
plot(range.map, add=TRUE,		#add range map shapefile
     lwd=1.5,				#line width
     col='transparent'			#fill color (this is default for this object)
)

# -11- WRITE this plot to an image file
output.filename <- 'abla_rangemap.tif'
window.width <- dev.size('in')[1]				#get the width of the plot window in inches
resolution <- dev.size('px')[1]/dev.size('in')[1]	#get the screen resolution

dev.print(tiff, file=output.filename, width=window.width, units='in',res=resolution)
#do you see your image in the folder?

# -12- PLOT model outputs for the current and future predicted distributions
windows(width=7,height=4)		#make a new, wide window frame
par(mfrow = c(1,2))			#split plot frame in two

plot(cur.prediction, 			#plot the current prediction
     main='Current SDM',		#plot title
     col=rev(heat.colors(1000)),	#lets try a different color scheme (see ?heat.colors)
     legend=FALSE
)
plot(states.shp,add=TRUE,		#add state boundaries
     border=gray(0.4)			#line color, adjust gray by changing number
)
plot(range.map, add=TRUE,		#add range map shapefile
     border='blue'			#line width
)

plot(fut.prediction, 			#plot the future prediction
     main='Future SDM',		#plot title
     col=rev(heat.colors(1000)),	
     legend=FALSE
)
plot(states.shp,add=TRUE,		#add state boundaries
     border=gray(0.4)			#line color, adjust gray by changing number
)
plot(range.map, add=TRUE,		#add range map shapefile
     border='blue'			#line width
)

output.filename <- 'abla_rangemap_Curent_Future.tif'   #output the figure
window.width <- dev.size('in')[1]				#get the width of the plot window in inches
resolution <- dev.size('px')[1]/dev.size('in')[1]	#get the screen resolution

dev.print(tiff, file=output.filename, width=window.width, units='in',res=resolution)

	output.filename <- 'abla_rangemap_Curent_Future.tif'   #output the figure
	window.width <- dev.size('in')[1]				#get the width of the plot window in inches
	resolution <- dev.size('px')[1]/dev.size('in')[1]	#get the screen resolution

	dev.print(tiff, file=output.filename, width=window.width, units='in',res=resolution)

# -13- Output the maps and SAVE the fitted model projections to raster files

writeRaster(cur.prediction,filename='abla_current_raster.tif',format='GTiff')
writeRaster(fut.prediction,filename='abla_future_raster.tif',format='GTiff')

#to be sure it saved currectly let's read one of those files back in and plot it
test.current <- raster('abla_current_raster.tif')
test.current	#do the projection and extent look right?
plot(test.current)

# -14- Now, to run a maxent model:
#make sure you have a working updated version of java
#download maxent from this site: http://www.cs.princeton.edu/~schapire/maxent/
#place the files into the folder displayed in the R console when you run:
system.file('java', package='dismo')

max.fit <- maxent(x=current.stack,
                  p=point.data[,-1])
cur.prediction <- predict(object=max.fit, x=current.stack)
fut.prediction <- predict(object=max.fit, x=future.stack)


# -15- Read More
#	http://cran.r-project.org/web/packages/dismo/vignettes/sdm.pdf