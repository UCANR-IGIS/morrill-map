## This script downloads GeoJson files from Native-Land.ca, and converts it to GeoJSON
## This script is designed to be run line-by-line.

## Load Packages
library(sf)

# Set conflict preferences
library(dplyr)
library(conflicted)
conflict_prefer("filter", "dplyr", quiet = TRUE)
conflict_prefer("count", "dplyr", quiet = TRUE)
conflict_prefer("select", "dplyr", quiet = TRUE)

# Define Directories
natland_dir <- "D:/GitHub/uc_landgrant/morrill-map/native-land"; file.exists(natland_dir)
territories_fn <- file.path(dir_natland, "territories.geojson")

file.exists(territories_fn)  ## if the file already exists, you can skip the next section

#################################################################
## Download Territories
  
territories_url <- "https://native-land.ca/coordinates/indigenousTerritories.json"

if (download.file(url = territories_url, destfile = territories_fn) != 0) stop("Error downloading")

## The indigenous territories geojson file has a problem - some of the polygons
## have a z-coordinate (0), while others don't. This causes an error when you
## try to import it. To correct this, we replace all occurrences of ',0]' with ']'.

gson_orig_chr <- readLines(territories_fn)
gson_fixed_chr <- gsub(",0]", "]", gson_orig_chr)
cat(gson_fixed_chr, file = territories_fn, sep = "\n")

## Proof of the pudding - can you import it?
territories_sf <- sf::st_read(territories_fn)

plot(territories_sf %>% st_geometry())

######################################################################
## Next, pull out just those polygon that intersect California

territories_ca_fn <- file.path(dir_natland, "territories_ca.geojson")
file.exists(territories_ca_fn)  ## if the file already exists, you can skip the next section

## Import CA boundary
cabnd_fn <- "D:/GitHub/uc_landgrant/morrill-map/docs/data/ca_bnd.geojson"; file.exists(cabnd_fn)

cabnd_sf <- st_read(cabnd_fn, quiet = TRUE) %>% st_transform(4326)
cabnd_sf

territories_ca_sf <- territories_sf[cabnd_sf, , ,] 

library(tmap)
tm_shape(territories_ca_sf) +
  tm_borders(col = 'gray') +
tm_shape(cabnd_sf) +
  tm_borders(col = "red")

st_write(territories_ca_sf, dsn = territories_ca_fn)


head(territories_ca_sf)
