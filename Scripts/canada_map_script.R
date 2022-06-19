#------------------------------------------------------------------------------#
# Name : canada_map_script.R                                                   #
# Description : A map of Canada with its Census-defined boundaries             #
# Author : Pietro Violo                                                        #
# Date : June 18 2022                                                          #
#------------------------------------------------------------------------------#

# Libraries
library(geojsonio)
library(rmapshaper)
library(rgdal)
library(tidyverse)
library(spdplyr)
library(sf)
library(socviz)
library(tools)

theme_map <- function(base_size=9, base_family="") {
  require(grid)
  theme_bw(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          panel.spacing=unit(0, "lines"),
          plot.background=element_blank(),
          legend.justification = c(0,0),
          legend.position = c(0,0)
    )
}


# Read ArcGIS shapefile downloaded from Statistic Canada website with function from rgdal package
canada_raw <- readOGR(dsn = "./Data", layer = "lcd_000b21a_e",
                      use_iconv=TRUE, encoding="CP1250")

# Convert object to GeoJson format and simplify the polygons
canada_raw_json <- geojson_json(canada_raw)
canada_raw_sim <- ms_simplify(canada_raw_json)

# Save the simplified version, we will then solely use this version
geojson_write(canada_raw_sim, file = "Data/canada_cd.geojson")



# Now that we saved the simplified version, only load the .geojson map file and do not run the previous code
canada_cd <- st_read("data/canada_cd.geojson", quiet = TRUE)
canada_cd


# Transform to the Lambert Conformal Conic projection

canada_cd <- st_transform(canada_cd, crs = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

canada_cd

map_colors <-  RColorBrewer::brewer.pal(8, "Pastel1")
map_colors <- rep(map_colors, 37)

p <- ggplot(data = canada_cd, 
            mapping = aes(fill = PRUID))
p_out <- p + geom_sf(color = "gray60", 
                     size = 0.1) + 
  scale_fill_manual(values = map_colors) + 
  guides(fill = FALSE) + 
  theme_map() + 
  theme(panel.grid.major = element_line(color = "white"),
        legend.key = element_rect(color = "gray40", size = 0.1))
ggsave("figures/canada.pdf", p_out, height = 12, width = 15)
p_out





