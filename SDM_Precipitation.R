#loading bioclim data
bioclim.data2 <- getData(name = "worldclim",
                        var = "prec",
                        res = 2.5,
                        path = "data/")
#read files
ebd_s<- read_csv("/Users/gamageperera/Desktop/YEBU/YEBU_S.csv")

# Determine geographic extent of our data
max.lat <- ceiling(max(ebd_s$LATITUDE))
min.lat <- floor(min(ebd_s$LATITUDE))
max.lon <- ceiling(max(ebd_s$LONGITUDE))
min.lon <- floor(min(ebd_s$LONGITUDE))
geographic.extent <- extent(x = c(min.lon, max.lon, min.lat, max.lat))

# Load the data to use for our base map
data(wrld_simpl)

# Plot the base map
plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey95")

# Add the points for individual observation
points(x = ebd_s$LONGITUDE, 
       y = ebd_s$LATITUDE, 
       col = "olivedrab", 
       pch = 20, 
       cex = 0.75)
# And draw a little box around the graph
box()

# Crop bioclim data to geographic extent of saguaro
bioclim.data2 <- crop(x = bioclim.data2, y = geographic.extent)

# Reverse order of columns
ebd_s <- ebd_s[, c("LONGITUDE", "LATITUDE")]

# Build species distribution model
bc.model <- bioclim(x = bioclim.data2, p = ebd_s)

# Predict presence from model
predict.presence <- dismo::predict(object = bc.model, 
                                   x = bioclim.data2, 
                                   ext = geographic.extent)
# Plot base map
plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey95",
     main = "Precipitation")

# Add model probabilities
plot(predict.presence, add = TRUE)

# Redraw those country borders
plot(wrld_simpl, add = TRUE, border = "grey5")

# Add original observations
points(ebd_s$LONGITUDE, ebd_s$LATITUDE, col = "olivedrab", pch = 20, cex = 0.75)
box()

#The pseudo-absence point
# Use the bioclim data files for sampling resolution
bil.files <- list.files(path = "data/wc2-5", 
                        pattern = "*.bil$", 
                        full.names = TRUE)

# We only need one file, so use the first one in the list of .bil files
mask <- raster(bil.files[1])

# Set the seed for the random-number generator to ensure results are similar
set.seed(20210707)

# Randomly sample points (same number as our observed points)
background <- randomPoints(mask = mask,     # Provides resolution of sampling points
                           n = nrow(ebd_s),      # Number of random points
                           ext = geographic.extent, # Spatially restricts sampling
                           extf = 1.25)             # Expands sampling a little bit
head(background)

# Plot the base map
plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey95",
     main = "Presence and pseudo-absence points")

# Add the background points
points(background, col = "grey30", pch = 1, cex = 0.75)

# Add the observations
points(x = ebd_s$LONGITUDE, 
       y = ebd_s$LATITUDE, 
       col = "olivedrab", 
       pch = 20, 
       cex = 0.75)

box()
# Arbitrarily assign group 1 as the testing data group
testing.group <- 1

# Create vector of group memberships
group.presence <- kfold(x = ebd_s, k = 5) # kfold is in dismo package
head(group.presence)
# Should see even representation in each group
table(group.presence)

# Separate observations into training and testing groups
presence.train <- ebd_s[group.presence != testing.group, ]
presence.test <- ebd_s[group.presence == testing.group, ]

# Repeat the process for pseudo-absence points
group.background <- kfold(x = background, k = 5)
background.train <- background[group.background != testing.group, ]
background.test <- background[group.background == testing.group, ]

#Training and testing the model
# Build a model using training data
bc.model <- bioclim(x = bioclim.data, p = presence.train)

# Predict presence from model (same as previously, but with the update model)
predict.presence <- dismo::predict(object = bc.model, 
                                   x = bioclim.data, 
                                   ext = geographic.extent)
# Use testing data for model evaluation
bc.eval <- evaluate(p = presence.test,   # The presence testing data
                    a = background.test, # The absence testing data
                    model = bc.model,    # The model we are evaluating
                    x = bioclim.data)    # Climatic variables for use by model

# Determine minimum threshold for "presence"
bc.threshold <- threshold(x = bc.eval, stat = "spec_sens")

# Plot base map
plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey95")

# Only plot areas where probability of occurrence is greater than the threshold
plot(predict.presence > bc.threshold, 
     add = TRUE, 
     legend = FALSE, 
     col = c(NA, "olivedrab"))

# And add those observations
points(x = ebd_s$LONGITUDE, 
       y = ebd_s$LATITUDE, 
       col = "black",
       pch = "+", 
       cex = 0.75)

# Redraw those country borders
plot(wrld_simpl, add = TRUE, border = "grey5")
box()

