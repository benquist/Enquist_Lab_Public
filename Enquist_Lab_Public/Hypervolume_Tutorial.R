##### ####################################
##### Enquist Lab Hypervolume tutorial by B.Blonder et al. from 
##### Hypervolume Tutorial based on http://cran.r-project.org/web/packages/hypervolume/hypervolume.pdf
##### See Blonder et al. 2014 GEB for discussion
##########################################

library(Rcpp)
library(methods)
library(rgl)

install.packages("hypervolume")
library(hypervolume)

#lets use the classic Iris flower dataset with standarized morphological 
#measures for several flower parts within and across species
data(iris)
head(iris) # note there are four traits of several measured iris species

#select a species of interest and lets explore 
setosa =iris[iris$Species=="setosa",1:3] 
head(setosa)
           
#calculate the hypervolume by choosing a sample number
#and bandwidth for kernel density
hv1=hypervolume(setosa, reps=1000,bandwidth=0.25)  
imp = hypervolume_importance(hv1) #note use this to get a named vector 
#with importance scores for each axis. Note that these scores are not 
#dimensionless but rather have linear units.

#summarize the result, note look for volume
summary(hv1)
plot(hv1)  # for this species plot the sampled hypervolume
# plot(hv1, pairplot = FALSE) #3-D plot

#choose another iris species
virginica =iris[iris$Species=="virginica",1:3]
head(virginica)
hv2=hypervolume(virginica, reps=1000,bandwidth=0.25) 
summary(hv2)
plot(hv2) 
# plot(hv2,pairplot = FALSE) #3-D plot

#choose another iris species
versicolor =iris[iris$Species=="versicolor",1:3]
head(versicolor)
hv3=hypervolume(versicolor, reps=1000,bandwidth=0.25) 
summary(hv3)
plot(hv3) 
# plot(hv3, pairplot = FALSE) #3-D plot

## Which species has the largest and smallest hypervolume?

### Let's do all species of iris 
genus =iris[,1:3] 
head(genus)
hv4=hypervolume(genus, reps=1000,bandwidth=0.25) 
summary(hv4)
plot(hv4)
plot(hv4, pairplot = FALSE) #3-D plot


## Note, the hypervolume measurements are sensitive to the kernel density threshold value
## that is why recommend that for any study that you not chance the threshold
## value unless you have clear justification. See Blonder et al. 2014 for discussion on
## threshold values.

## Other options 
## Use hypervolume_inclusion_test to determines if a set of points are within a hypervolume.

## Use hypervolume_join to combines multiple hypervolumes or hypervolume lists into a single 
## hypervolumeList suitable for analysis or plotting. For example merge hypervolumes of species
#data(iris)
#hv1 = hypervolume(subset(iris, Species=="setosa")[,1:4],bandwidth=0.2,name=􏰀setosa􏰀)
#hv2 = hypervolume(subset(iris, Species=="versicolor")[,1:4],bandwidth=0.2,name=􏰀versicolor􏰀)
#hv_all = hypervolume_join(hv1, hv2)
#plot(hv_all)

# Use hypervolume_set to compute the intersection, union, and unique components of two hypervolumes.

hv_set = hypervolume_set(hv1, hv2, check_memory=FALSE)
get_volume(hv_set)

#  neat, the union of the two hypervolumes is larger in volume than the unique areas

hv_set2 = hypervolume_set(hv1, hv3, check_memory=FALSE)
get_volume(hv_set2)

hv_set3 = hypervolume_set(hv2, hv3, check_memory=FALSE)
get_volume(hv_set3)

####################
####################
## Now let's put all the species together '
## and plot hypervolumes together. 
##
########

# rescale data
iris_scaled <- scale(iris[,1:4])  #note caclulations here will use all four traits
                                  # rescaling the data, read online R tutorial

# compute hypervolume for each species
bw <- 0.25  #we can assign the bandwidth to start

hv_versicolor <- hypervolume(iris_scaled[iris$Species=="versicolor",],bandwidth=bw,name='versicolor')
hv_setosa <- hypervolume(iris_scaled[iris$Species=="setosa",],bandwidth=bw,name='setosa')
hv_virginica <- hypervolume(iris_scaled[iris$Species=="virginica",],bandwidth=bw,name='virginica')

# create a hypervolumelist object
hv_all <- hypervolume_join(hv_versicolor,hv_setosa, hv_virginica)

# make a 3D plot using the first three axes
plot(hv_all, pair=F)

# try set operations on a species-pair
setops <- hypervolume_set(hv_versicolor, hv_virginica, check_memory=FALSE)

# plot overlaps
plot(setops)  # 2-D pairs plots
plot(setops, pair=F)  #3-D version of plot. Note, 3-D plot only looks at first three traits not the four

# get volumes of overlaps
setops_volumes <- get_volume(setops)

# calculate fractional overlap
fractional_overlap <- as.numeric(setops_volumes["Intersection"] / setops_volumes["Union"])
print(fractional_overlap)


###########
###########
## Now let's put all the species together '
## and plot hypervolumes together. 
##
########

# rescale data
iris_scaled <- scale(iris[,1:4])  #note caclulations here will use all four traits
                                  # rescaling the data, read online R tutorial

# compute hypervolume for each species
bw <- 0.25  #we can assign the bandwidth to start

hv_versicolor <- hypervolume(iris_scaled[iris$Species=="versicolor",],bandwidth=bw,name='versicolor')
hv_setosa <- hypervolume(iris_scaled[iris$Species=="setosa",],bandwidth=bw,name='setosa')
hv_virginica <- hypervolume(iris_scaled[iris$Species=="virginica",],bandwidth=bw,name='virginica')

# create a hypervolumelist object
hv_all <- hypervolume_join(hv_versicolor,hv_setosa, hv_virginica)

# make a 3D plot using the first three axes
plot(hv_all, pair=F)

# try set operations on a species-pair
setops <- hypervolume_set(hv_versicolor, hv_virginica, check_memory=FALSE)

# plot overlaps
plot(setops)  # 2-D pairs plots
plot(setops, pair=F)  #3-D version of plot. Note, 3-D plot only looks at first three traits not the four

# get volumes of overlaps
setops_volumes <- get_volume(setops)

# calculate fractional overlap
fractional_overlap <- as.numeric(setops_volumes["Intersection"] / setops_volumes["Union"])
print(fractional_overlap)


#######################################
######  Climate hypervolumes
######  and geographic/climate niches
#######################################

if (exists('doHypervolumeQuercusDemo')==TRUE)
{
  require(raster)
  require(maps)
  
  # load in lat/lon data
  data('quercus') 
  data_alba = subset(quercus, Species=="Quercus alba")[,c("Longitude","Latitude")]
  data_rubra = subset(quercus, Species=="Quercus rubra")[,c("Longitude","Latitude")]
  
  # get worldclim data from internet
  climatelayers = getData('worldclim', var='bio', res=10, path=tempdir())
  
  # z-transform climate layers to make axes comparable
  climatelayers_ss = climatelayers[[c(1,4,12,15)]]
  for (i in 1:nlayers(climatelayers_ss))
  {
    climatelayers_ss[[i]] <- (climatelayers_ss[[i]] - cellStats(climatelayers_ss[[i]], 'mean')) / cellStats(climatelayers_ss[[i]], 'sd') 
  }
  
  # extract transformed climate values
  climate_alba = extract(climatelayers_ss, data_alba)
  climate_rubra = extract(climatelayers_ss, data_rubra)
  
  # compute hypervolumes with auto-bandwidth for both species
  hv_alba = hypervolume(climate_alba,quantile=0.0,reps=1000,bandwidth=estimate_bandwidth(climate_alba),name='alba')
  hv_rubra = hypervolume(climate_rubra,quantile=0.0,reps=1000,bandwidth=estimate_bandwidth(climate_rubra),name='rubra')
  
  # determine intersection and unique components of the overlap
  hv_set = hypervolume_set(hv_alba, hv_rubra, check_memory=FALSE)
  
  # put all the output volumes in one convenient place
  volumes <- c(Alba=get_volume(hv_alba), Rubra=get_volume(hv_rubra), get_volume(hv_set))
  
  # do species distribution modeling
  # get all the climate values
  climatevalues = data.frame(getValues(climatelayers_ss))
  
  rubra_inout = hypervolume_inclusion_test(hv_rubra, climatevalues)
  alba_inout = hypervolume_inclusion_test(hv_alba, climatevalues)
  
  # convert to rasters by setting values in rasters with same extent/resolution
  rubra_map = raster(climatelayers_ss[[1]]); values(rubra_map) <- rubra_inout
  alba_map = raster(climatelayers_ss[[1]]); values(alba_map) <- alba_inout
  
  # then barplot of hypervolumes of each component
  barplot(volumes,horiz=TRUE,las=2,main="Hypervolume")
  
  # then pairs plot of the set operations
  plot(hv_set)
  
  # plot the geographic projections of the ranges
  par(mfrow=c(1,2))
  plot(rubra_map,col=c(NA,rgb(1,0,0,0.5)),legend=FALSE,xlim=c(-100,-50),ylim=c(20,60),main='Quercus rubra')
  map('world',add=TRUE)
  points(Latitude~Longitude,data=data_rubra,pch=3,cex=0.25)
  
  plot(alba_map,col=c(NA,rgb(0,0,1,0.5)),legend=FALSE,xlim=c(-100,-50),ylim=c(20,60),main='Quercus alba')
  map('world',add=TRUE)
  points(Latitude~Longitude,data=data_alba,pch=3,cex=0.25)
  par(mfrow=c(1,1))
  
  rm(doHypervolumeQuercusDemo)
} else
{
  message('Demo does not run by default to meet CRAN runtime requirements.')
  message('This demo requires internet access to download 10MB of climate data and')
  message('will take approximately 3 minutes to run.')
  message('To run the demo, type')
  message('\tdoHypervolumeQuercusDemo=TRUE')
  message('\tdemo(quercus)')
  message('at the R command line prompt.')
}





