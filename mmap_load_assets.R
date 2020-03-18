## Load Libraries
library(readr)
library(dplyr)
library(sf)
library(httr)
library(assertthat)
library(htmltools)
library(DT)
library(crayon)
library(stringr)
library(RSQLite)
library(lubridate)

# Set conflict preference for commonly used functions from dplyr
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("distinct", "dplyr")
conflict_prefer("left_join", "dplyr")

## Load utility functions
source("mmap_utils.R")
source('mmap_importcsvs.R')
source("mmap_getgeom.R")

## Get arcgisbinding ready if needed
library(arcgisbinding)
arc.check_product()

## Define directories
dir_glo <- "glo/data"  ## location of the csv files from the GLO
dir_rdata <- "rdata"   ## location where rdata files should go
dir_shp <- "out_shp"       ## location where shapefiles should be saved
dir_geojson <- "out_geojson"   ## location where geojson files should be saved
dir_gbd <- "out_gdb"           ## directory where file geodatabase should be saved

comb_geopackage_fn <- file.path("out_gpkg", "morrill-patents.gpkg")
comb_geojson_fn <- file.path("out_geojson", "morrill-patents.geojson")

## Default Processing Options
states_to_process <- "SD"
skip_nodata <- TRUE
skip_completed <- FALSE
load_rdata <- c("always", "when-state-not-in-memory", "never")[2]
import_csv_if_needed <- FALSE

save_stats <- FALSE
save_badld <- FALSE

## One-time processing tasks
add_sig_year <- FALSE
geom_convert_single2multi <- FALSE
compute_lld <- F   ## uses the same pat_idx_option as get-geom
add_patentees <- F
make_archive_copy <- F
save_archive_copy <- F 


## Options for getting the geoms
get_geoms <- FALSE
use_archived_objs <- FALSE
pat_idx_option <- c("NAs", "all errors", "Status not returned", "fail", "all")[1]
pat_idx <- 1
(pause_after_n = list("1" = 0, "600" = 5, "3900" = 60 * 3))
save_stats <- F
save_badld <- F
add_sig_year <- FALSE

save_ind_geopackage <- FALSE
save_ind_shp <- FALSE
save_ind_geojson <- FALSE

save_comb_rdata <- FALSE
save_comb_geojson <- FALSE  ## save combined polygon layer as geojson
save_comb_rdata <- FALSE    ## save combined sf data frame as rdata
save_comb_gdb <- FALSE

debugme <- FALSE

