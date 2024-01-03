library(ggplot2)

# landsat 8 is 16-bit and products were scaled to 0-55000
ls8max <- 55000 # but ranges of east turkana values range between 5 and 25k
ls8_range <- c(5000, 25000)

# get all areas
library(sf)
all_areas <- st_read("VECTORS/AREAS/ET_Collecting_Areas_jcoelho.shp")
all_areas <- st_transform(all_areas, projection(satellite)) # needs to be reprojected
all_areas$Size <- as.numeric(st_area(all_areas))


class_spdf <- as(raster.df, 'SpatialPixelsDataFrame')
class_df <- as.data.frame(class_spdf)
colnames(class_df) <- c('ultrablue', 'blue', 'green', 'red', 'NIR',
                        'SWIR1', 'SWIR2', 'NDVI', 'height', 'slope',
                        'north', 'east', 'fossils', 'longitude', 'latitude')

bw.coo <- class_df[complete.cases(class_df[,13]), 13:15]
bw.coo$`Fieldwork` <- 'bonewalk'
# band colors
band.col <- c('#5034db','#3498db','#2ecc71','#e74c3c','#c0392b','#e73c64','#e73cb1')


# ULTRABLUE

gg1ub <- ggplot() + 
  geom_raster(data = class_df, aes(longitude, latitude, fill = ultrablue)) +
  scale_fill_gradientn(colors = c('black', band.col[1], 'white')) +
  ggtitle('A) Ultrablue') + coord_sf() + theme_minimal() +
  theme(plot.title = element_text(color = band.col[1], face = 'bold'))

# BLUE 

gg2blue <- ggplot() + 
  geom_raster(data = class_df, aes(longitude, latitude, fill = blue)) +
  scale_fill_gradientn(colors = c('black', band.col[2], 'white')) +
  ggtitle('B) Blue') + coord_sf() + theme_minimal() +
  theme(plot.title = element_text(color = band.col[2], face = 'bold'))

# GREEN

gg3green <- ggplot() + 
  geom_raster(data = class_df, aes(longitude, latitude, fill = green)) +
  scale_fill_gradientn(colors = c('black', band.col[3], 'white')) +
  ggtitle('C) Green') + coord_sf() + theme_minimal() +
  theme(plot.title = element_text(color = band.col[3], face = 'bold'))

# RED

gg4red <- ggplot() + 
  geom_raster(data = class_df, aes(longitude, latitude, fill = red)) +
  scale_fill_gradientn(colors = c('black', band.col[4], 'white')) +
  ggtitle('D) Red') + coord_sf() + theme_minimal() +
  theme(plot.title = element_text(color = band.col[4], face = 'bold'))

# NEARINFRARED

gg5nir <- ggplot() + 
  geom_raster(data = class_df, aes(longitude, latitude, fill = NIR)) +
  scale_fill_gradientn(colors = c('black', band.col[5], 'white')) +
  ggtitle('E) Near-infrared') + coord_sf() + theme_minimal() +
  theme(plot.title = element_text(color = band.col[5], face = 'bold'))


# SWIR 1

gg6swir1 <- ggplot() + 
  geom_raster(data = class_df, aes(longitude, latitude, fill = SWIR1)) +
  scale_fill_gradientn(colors = c('black', band.col[6], 'white')) +
  ggtitle('F) Shortwave infrared  1') + coord_sf() + theme_minimal() +
  theme(plot.title = element_text(color = band.col[6], face = 'bold'))

# SWIR 2

gg7swir2 <- ggplot() + 
  geom_raster(data = class_df, aes(longitude, latitude, fill = SWIR2)) +
  scale_fill_gradientn(colors = c('black', band.col[7], 'white')) +
  ggtitle('G) Shortwave infrared  2') + coord_sf() + theme_minimal() +
  theme(plot.title = element_text(color = band.col[7], face = 'bold'))

# VEGETATION

gg0ndvi <- ggplot() + 
  geom_raster(data = class_df, aes(longitude, latitude, fill = NDVI)) +
  scale_fill_gradientn(colors = c('white', '#D9D1BA', 'darkgreen')) +
  ggtitle('H) Vegetation (NDVI)') + coord_sf() + theme_minimal() +
  theme(plot.title = element_text(color = 'darkgreen', face = 'bold'))


# ELEVATION

gg8dem <- ggplot() + 
  geom_raster(data = class_df, aes(longitude, latitude, fill = height)) +
  scale_fill_viridis_c(option = "E") +
  ggtitle('I) Digital Elevation Model') + coord_sf() + theme_minimal() +
  theme(plot.title = element_text(face = 'bold'))

# SLOPE

gg9slope <- ggplot() + 
  geom_raster(data = class_df, aes(longitude, latitude, fill = slope)) +
  scale_fill_gradient(low = 'white', high = 'black') +
  ggtitle('J) Slope') + coord_sf() + theme_minimal() +
  theme(plot.title = element_text(face = 'bold'))

# NORTHNESS

gg10northness <- ggplot() + 
  geom_raster(data = class_df, aes(longitude, latitude, fill = north)) +
  scale_fill_gradient(low = 'black', high = 'white') +
  ggtitle('K) Northness') + coord_sf() + theme_minimal() +
  theme(plot.title = element_text(face = 'bold'))

# EASTNESS

gg11eastness <- ggplot() + 
  geom_raster(data = class_df, aes(longitude, latitude, fill = east)) +
  scale_fill_gradient(low = 'black', high = 'white') +
  ggtitle('L) Eastness') + coord_sf() + theme_minimal() +
  theme(plot.title = element_text(face = 'bold'))


### JOIN PLOTS TOGETHER

library(ggpubr)

ggexport(gg1ub, gg2blue, gg3green, gg4red,
         gg5nir, gg6swir1, gg7swir2, gg0ndvi, 
         gg8dem, gg9slope, gg10northness, gg11eastness,
         filename = "Figure_2_raw_data_neo.png",
         ncol = 4, nrow = 3, res = 360,
         width = 7500, height = 9000)


ggexport(gg1ub, gg2blue, gg3green, gg4red,
         gg5nir, gg6swir1, gg7swir2, gg0ndvi, 
         gg8dem, gg9slope, gg10northness, gg11eastness,
         filename = "Figure_2_raw_data_PPTXVERSION_neo.png",
         ncol = 6, nrow = 2, res = 360,
         width = 1600, height = 900)



# older version included:


# # ASPECT
# 
# gg10aspect <- ggplot() + 
#   geom_raster(data = class_df, aes(longitude, latitude, fill = aspect)) +
#   scale_fill_gradient(low = 'black', high = 'white') +
#   # scale_fill_gradientn(colours = c("white", "#440154FF", "black", "#FDE725FF", "white") ) +
#   ggtitle('K) Aspect') + coord_sf() + theme_minimal() +
#   theme(plot.title = element_text(face = 'bold'))
# 
# # FOSSILS
# 
# gg11bw <- ggplot() + 
#   geom_sf(all_areas, mapping = aes(geometry = geometry)) +
#   geom_sf_text(data = all_areas, aes(label = Area, size = Size)) +
#   geom_point(data = bw.coo, aes(longitude, latitude, fill = Fieldwork), color = '#ff4ce4') +
#   ggtitle('L) Paleontological Collection Areas') + theme_minimal() +
#   theme(plot.title = element_text(face = 'bold')) +
#   coord_sf(xlim = c(zone[1:2]), ylim = zone[3:4]) +
#   guides(size = "none")

library(spatstat)
library(ggspatial)
rectangle_owin <- as.owin(zone[1:4])
ileret_owin <- as.owin(extent(ILERET_areas)[1:4])
karari_owin <- as.owin(extent(KARARI)[1:4])
kf_owin <- as.owin(extent(KOOBIFORA)[1:4])


extendedFig0 <- ggplot() +
  geom_sf(all_areas, mapping = aes(geometry = geometry), fill = '#ecf0f1', color = "white") +
  geom_polygon(data = as.data.frame(rectangle_owin), aes(x = x, y = y), size = .5,
               fill = "transparent", color = '#ff4ce4', linetype = "longdash") +
  geom_polygon(data = as.data.frame(ileret_owin), aes(x = x, y = y), size = .75,
               fill = "transparent", color = viridis::viridis(3)[1]) +
  geom_polygon(data = as.data.frame(karari_owin), aes(x = x, y = y), size = .75,
               fill = "transparent", color = viridis::viridis(3)[2]) +
  geom_polygon(data = as.data.frame(kf_owin), aes(x = x, y = y), size = .75,
               fill = "transparent", color = viridis::viridis(3)[3]) +
  geom_point(data = bw.coo, aes(longitude, latitude, fill = Fieldwork),
             color = '#ff4ce4', size = 0.1) +
  geom_sf_text(data = all_areas, aes(label = Area, size = Size^2)) +
  theme_minimal() +
  theme(plot.title = element_text(face = 'bold'),
        legend.position = "bottom", legend.box = "horizontal") +
  guides(size = "none") +
  labs(x = "Longitude", y = "Latitude") +
  annotation_scale(location = "br", width_hint = 0.25, text_cex = 0.7, 
                   bar_cols = "black") +
  scale_x_continuous(breaks = seq(36.1, 36.6, 0.1))

ggsave(filename = "SF0_BWs.png", plot = extendedFig0, bg = "white",
       scale = 2, width = 1200, height = 1920, units = "px", dpi = 360)

library(patchwork)

inverted.gg1 <- ggplot(bonewalks) +
  geom_col(aes(x = POI, y = aquatic, fill = Region), width = 0.7) +
  xlab("Bonewalks by # POI") + scale_fill_viridis_d() +
  ylab("Percentage of aquatic fauna") +
  # ggtitle("Aquatic vs terrestrial faunal input across bonewalks") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() + scale_x_reverse() + coord_flip() +
  theme(legend.position = "bottom", legend.box = "horizontal")

combined.Fig3 <- extendedFig0 + inverted.gg1 & plot_annotation(tag_levels = 'A')

ggsave(filename = "Figure_3_Bonewalks.png", plot = combined.Fig3, bg = "white",
       scale = 2, width = 1300, height = 1200, units = "px", dpi = 360)
