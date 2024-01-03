# SETUP

# The following 3 steps are essential, because maxEnt takes lots of java

# clears the R workspace by removing all objects that are currently stored in memory. This ensures that there are no conflicts or naming issues when running subsequent code.
rm(list = ls())
# -XX:+UseConcMarkSweepGC is a garbage collection algorithm that is used to manage memory in the JVM. This algorithm is designed to minimize pauses in the application while it is running.
# -Xmx8192m is used to set the maximum amount of memory that can be used by the JVM to 8 GB. This is a large amount of memory, and is useful when working with large datasets or when running memory-intensive computations.
options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))
# frees up memory that is no longer needed by the R session. This can help to reduce memory usage and improve the performance of the R session.
gc()

library(readr)
library(dplyr)
library(raster)
library(sf)
library(ggplot2)
library(dismo)

# timestamp
start.time <- Sys.time()

# Read LANDSAT 8 data

raw.list <- list.files("LANDSAT8/", pattern = glob2rx("*B*.TIF$"), full.names = T)

allbands <- stack(raw.list) # stack all bands into a dataframe
crs <- crs(allbands) # obtain coordinate reference system from landsat metadata
zone <- extent(185000, 220000, 405000, 480000) # get extent representing most of east Turkana
landsat8 <- crop(allbands, extent(zone)) # crop satellite data based on east Turkana extent
names(landsat8) <- c('ultrablue', 'blue', 'green', 'red', # naming bands 1 to 7
                     'NIR', 'SWIR1', 'SWIR2') # useful for plotting

# Read DEM data

areaDEM <- raster('DEM/dem.tif')

# Reconstruct missing values if there are any, using neighboring cells
area_dem <- focal(areaDEM, w = matrix(1,15,15), fun = mean, NAonly = TRUE, na.rm = TRUE)

# create slope and aspect variables
slope <- terra::terrain(area_dem, 'slope', unit = 'degrees')  # calculate slope
aspect <- terra::terrain(area_dem, 'aspect', unit = 'radians') # calculate aspect

# Identify locations where slope is zero
zero_slope_mask <- slope < 0.000000000001

# Replace corresponding values in aspect with 0, because it calculates them as 90 per default
northness <- cos(aspect)
northness[zero_slope_mask] <- 0
eastness <- sin(aspect)
eastness[zero_slope_mask] <- 0

# join elevation, slope and aspect into a single object

dem <- stack(area_dem, slope, northness, eastness)
names(dem) <- c('elevation', 'slope', 'northness', 'eastness')
dem <- projectRaster(dem, crs = crs)
srtm <- resample(dem, landsat8) # all extra NAs are remove in this step,
# since it is also cropping it to the desired (much smaller area)


# create vegetation index variable NDVI 

NDVI <- function(img, i, j){
  nir <- img[[i]]
  red <- img[[j]]
  vi <- (nir - red)/(nir + red)
  return(vi)
}

vi <- NDVI(landsat8, 5, 4)
names(vi) <- "vegetation"
plot(vi)

# join landsat 8 + NDVI + DEM into a single dataset

satellite <- stack(landsat8, vi, srtm)

# read vectors of areas where data collection did occur

ETVECTORS <- st_read("VECTORS/AREAS/ET_Collecting_Areas_jcoelho.shp") # East Turkana
all_areas <- st_transform(ETVECTORS, projection(satellite)) # needs to be reprojected
study_areas <- subset(all_areas, Area %in% c("1", "1A", "2", "3", "4", "5", "6",
                                             "6A", "7", "7A", "8", "8A", "9", "10",
                                             "11", "12", "13", "17", "101", "102",
                                             "103", "104", "105", "106", "116"))

ILERET <- st_read("VECTORS/AREAS/ILERET_PROPER_COLLECTING_AREAS.shp")
ILERET_areas <- st_transform(ILERET, projection(satellite)) # needs to be reprojected

AREA105 <- st_read("VECTORS/AREAS/ET_Collecting_Areas_POLYGON.shp")[40,] # reading shape files 105 IS THE 40th object
KARARI <- st_transform(AREA105, projection(satellite)) # needs to be reprojected

AREA103 <- st_read("VECTORS/AREAS/ET_Collecting_Areas_POLYGON.shp")[24,] # 103 is the 24th object
KOOBIFORA <- st_transform(AREA103, projection(satellite)) # needs to be reprojected

###

geo_fm <- st_read("VECTORS/KoobiForaFormation.shp")
geo_Fm <- st_transform(geo_fm, projection(satellite)) # needs to be reprojected
geo_Fm$Member <- factor(geo_Fm$Member, levels = c("Chari", "Okote", "KBS", "Burgi"))

# add geological info

delta_swamp <- st_read("VECTORS/delta_Area105.shp")
delta_swamp <- st_transform(delta_swamp, projection(satellite))

# read bone walks data

ED_POI2019 <- read_csv("WAYPOINTS_BADELF/EDpoi2019.csv")

bonewalks <- ED_POI2019 %>%
  group_by(POI) %>%
  summarise(n = n(), Mammal = sum(Mammal), Fish = sum(Fish), Reptile = sum(Reptile),
            Invertebrate = sum(Invertebrate), Hippo = sum(Hippo),
            LAT = mean(LAT), LONG = mean(LONG), Region = first(Region)) %>%
  data.frame()

# target variable *Y* as percentages of aquatic faunal input

bonewalks$aquatic <- rowSums(bonewalks[,4:7])/rowSums(bonewalks[,3:7])

# separate data frames for our 3 regions of data collection

bonewalks.ileret <- bonewalks[bonewalks$Region == 'Ileret',]
bonewalks.karari <- bonewalks[bonewalks$Region == 'Karari',]
bonewalks.koobifora <- bonewalks[bonewalks$Region == 'Koobi Fora',]

# Calculate total square meter of the region being analysed
res <- 30 # each pixel has 30 x 30 meters
sqmeters <- (satellite@ncols * res) * (satellite@nrows * res)
sqKM <- sqmeters / (1000 ^ 2)
sqKM

# PROCESSING COORDINATES

## convert it into SpatialPoints class object
bw.points <- cbind(bonewalks$LONG, bonewalks$LAT) # notice the change in order

sp.points <- SpatialPoints(bw.points, CRS('+proj=longlat +datum=WGS84'))
coords <- spTransform(sp.points, crs)

## are the coords gridded?
coords.raster <- rasterize(coords, satellite)
coo <- reclassify(coords.raster, c(0, 9999, 1)) # check upper limit before...
raster.df <- stack(satellite, coo)

# CREATE DIFFERENT AREAS FOR MODEL PREDICTIONS

pred.zone1 <- extent(ILERET_areas)
pred.zone2 <- extent(KARARI)
pred.zone3 <- extent(KOOBIFORA)

### PRE-MODELLING

raster.brick <- brick(raster.df) # turn multilayer band into a multilayer raster object
raster.Ileret <- crop(raster.brick, pred.zone1)
raster.Karari <- crop(raster.brick, pred.zone2)
raster.KoobiFora <- crop(raster.brick, pred.zone3)

# matrices for classification

nr <- getValues(raster.brick)
nr.Ileret <- getValues(raster.Ileret)
nr.Karari <- getValues(raster.Karari)
nr.KoobiFora <- getValues(raster.KoobiFora)

set.seed(1)

nc <- nlayers(raster.brick)
x.Ileret <- nr.Ileret[ , 1:(nc - 1)]
x.Karari <- nr.Karari[ , 1:(nc - 1)]
x.KoobiFora <- nr.KoobiFora[ , 1:(nc - 1)]
x <- nr[ , 1:(nc - 1)]

clrplts <- c("#5034db","#3498db","#2ecc71","#e74c3c","#c0392b","#e73c64","#e73cb1",
                      "yellow", "black", "darkgrey", "lightgrey", "lightgrey")

# timestamp
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken # ok below 4 min in my thinkpad t490, not bad

gc()

