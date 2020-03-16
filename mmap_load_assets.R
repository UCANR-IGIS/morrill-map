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
#source("mmap_recompute_ld.R") - merged into utils

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
compute_ld <- F   ## uses the same pat_idx_option as get-geom
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

save_rdata <- FALSE
save_geopackage <- FALSE
save_shp <- FALSE
save_geojson_ind <- FALSE
save_geojson_comb <- FALSE

debugme <- FALSE

