# download maxent.jar 3.3.3k, and place the file in the
# desired folder
utils::download.file(url = "https://raw.githubusercontent.com/mrmaxent/Maxent/master/ArchivedReleases/3.3.3k/maxent.jar", 
                     destfile = paste0(system.file("java", package = "dismo"), 
                                       "/maxent.jar"), mode = "wb")  ## wb for binary file, otherwise maxent.jar can not execute

# prepare folders for data input and output
if (!file.exists("../data")) dir.create("../data")
if (!file.exists("../data/bioclim")) dir.create("../data/bioclim")
if (!file.exists("../data/studyarea")) dir.create("../data/studyarea")
if (!file.exists("../output")) dir.create("../output")
require(utils)

# download climate data from worldclim.org
utils::download.file(url = "http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/bio_10m_bil.zip", 
                     destfile = paste0("../data/bioclim/bio_10m_bil.zip"))
utils::unzip("../data/bioclim/bio_10m_bil.zip", exdir = "../data/bioclim/")

# This searches for all files that are in the path
# 'data/bioclim/' and have a file extension of .bil. You can
# edit this code to reflect the path name and file extension
# for your environmental variables
clim_list <- list.files("../data/bioclim/", pattern = ".bil$", 
                        full.names = T)  # '..' leads to the path above the folder where the .rmd file is located

# stacking the bioclim variables to process them at one go
clim <- raster::stack(clim_list)

# make ebd_s spatial
coordinates(ebd_s) <- ~LONGITUDE + LATITUDE
## look for erroneous points
plot(clim[[1]])  # to the first layer of the bioclim layers as a reference
plot(ebd_s, add = TRUE)  # plot the oc_unique on the above raster layer

# create a 4-decimal-degree buffer around the
# occurrence data
ebd_s_buff <- buffer(ebd_s, 4)

# plot the first element ([[1]]) in the raster stack
plot(clim[[1]])

plot(ebd_s, add = T, col = "red")  # adds occurrence data to the plot
plot(ebd_s_buff, add = T, col = "blue")  # adds buffer polygon to the plot

# crop study area to a manageable extent (rectangle shaped)
studyArea <- crop(clim,extent(ebd_s_buff))  


# the 'study area' created by extracting the buffer area from the raster stack
studyArea <- mask(studyArea,ebd_s_buff)
# output will still be a raster stack, just of the study area

# save the new study area rasters as ascii
writeRaster(studyArea,
            # a series of names for output files
            filename=paste0("../data/studyarea/",names(studyArea),".asc"), 
            format="ascii", ## the output format
            bylayer=TRUE, ## this will save a series of layers
            overwrite=T)


# select background points from this buffered area; when the number provided 
# to set.seed() function, the same random sample will be selected in the next line			
# use this code before the sampleRandom function every time, if you want to get
# the same "random samples"
set.seed(1) 
bg <- sampleRandom(x=studyArea,
                   size=10000,
                   na.rm=T, #removes the 'Not Applicable' points 
                   sp=T) # return spatial points 

bg <- sampleRandom (x = studyArea,
                    size = 1000,
                    na.rm = TRUE,
                    sp = TRUE) # return spatial points 



plot(studyArea[[1]])
# add the background points to the plotted raster
plot(bg,add=T) 
# add the occurrence data to the plotted raster
plot(occ_final,add=T,col="red")