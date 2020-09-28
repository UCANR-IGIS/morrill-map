## This script loads parcels and their associated attributes from
## https://github.com/HCNData/landgrabu-data, keeping only those
## that fall within California.

## Load Packages
library(sf)
library(dplyr)
library(readr)
library(conflicted)

## Optional
library(DBI)
library(RSQLite)

# Set conflict preferences
conflict_prefer("filter", "dplyr", quiet = TRUE)
conflict_prefer("count", "dplyr", quiet = TRUE)
conflict_prefer("select", "dplyr", quiet = TRUE)

# Define Directories
dir_hcn_shp <- "D:/GitHub/uc_landgrant/landgrabu-data/Morrill_Act_of_1862_Indigenous_Land_Parcels_Database/Shapefiles"
dir_hcn_csv <- "D:/GitHub/uc_landgrant/landgrabu-data/Morrill_Act_of_1862_Indigenous_Land_Parcels_Database/CSVs"
dir_hcn_out <- "D:/GitHub/uc_landgrant/landgrabu-data-filt"
file.exists(dir_hcn_shp); file.exists(dir_hcn_csv); file.exists(dir_hcn_out)

# Read in the parcels Shapefile:
hcnparc_all_sf <- st_read(file.path(dir_hcn_shp, "Parcel_Polygons.shp"))
nrow(hcnparc_all_sf)

## See if the parcels.csv file has any embedded nuls
parcels_raw <- read_file_raw(file.path(dir_hcn_csv, "Parcels.csv"))
sum(parcels_raw == 0)

## Import the CSV (takes 3-4 minutes, most of which is the conversions)
hcnparc_all_tbl <- readr::read_csv(file.path(dir_hcn_csv, "Parcels.csv"),
                                col_names = TRUE,
                                na = c("", "na", "NA", "#N/A"),
                                col_types = cols(
                                  MTRSA_LG = col_character(),
                                  Loc_State = col_factor(),
                                  Loc_County = col_character(),
                                  Acres = col_double(),
                                  LG_State = col_factor(),
                                  LG_Reason = col_factor(),
                                  University = col_factor(),
                                  Uni_Ben_History = col_factor(),
                                  Royce_ID = col_character(),
                                  Tribal_Nation = col_factor(),
                                  US_Acquired_Mode = col_factor(),
                                  Cession_States = col_factor(),
                                  Royce_Link = col_factor(),
                                  Yr_US_Acquire = col_factor(),
                                  Date_US_Acquire = col_character(),
                                  US_Paid_for_Parcel = col_character(),
                                  Endow_Raised_Parcel	= col_character(),
                                  Uni_Raise_US_Pay_Multiple = col_character(),
                                  Yr_ST_Accept = col_factor(),
                                  Yr_Uni_Assign = col_factor(),
                                  Yr_Patent	= col_integer(),
                                  Date_Patent	= col_integer(),
                                  Patentees = col_factor(),
                                  Patent_Source_Reason = col_factor(),
                                  Source_ID	 = col_factor(),
                                  Source = col_factor(),
                                  Source_Loc = col_factor(),
                                  Source_Type = col_factor(),
                                  Source_Form = col_factor(),
                                  Source_Acqu = col_factor(),	
                                  Source_Acqu_Detail = col_factor(),
                                  Located_GIS = col_factor(),
                                  Parcel_Link = col_character(),
                                  MTRSA = col_character(),
                                  MTRS = col_character(),
                                  A_or_L = col_factor(),
                                  Aliquot = col_factor(),
                                  Types = col_factor(),
                                  GISAcres = col_character(),
                                  GIS_Acre_Div_List_Acre = col_double(),
                                  Polygon = col_factor(),
                                  Accuracy = col_factor(),
                                  LG_Royce = col_factor()),
                            locale = readr::locale(encoding = "latin1")
                          )
## View results
dim(hcnparc_all_tbl)
## 79461    43

## Save a copy to disk (in a native R format)
save(hcnparc_all_tbl, file = file.path(dir_hcn_out, "hcnparc_all_tbl.RData"))
# load(file.path(dir_hcn_out, "hcnparc_all_tbl.RData"))

## Save this table as a SQLite database (for future use)
parc_db <- DBI::dbConnect(RSQLite::SQLite(), file.path(dir_hcn_out, "parcels.sqlite"))
DBI::dbListTables(parc_db)

## Write the table
DBI::dbWriteTable(parc_db, "parcels", hcnparc_all_tbl)  ## works
DBI::dbListTables(parc_db)

## Add some indices
dplyr::db_create_index(parc_db, "parcels", "Loc_State", name = "loc_state_idx", unique = FALSE)
dplyr::db_create_index(parc_db, "parcels", "LG_State", name = "lg_state_idx", unique = FALSE)

## Close connection
dbDisconnect(parc_db)

##########################################
## CREATE A SUBSET OF ROWS AND COLUMNS

cols_keep <- c("MTRSA_LG", "Loc_State", "Loc_County", "Acres", 
               "LG_State", "LG_Reason", "University", "Tribal_Nation", 
               "US_Acquired_Mode", "Royce_Link", "Yr_US_Acquire", 
               "Date_Patent", "Patentees", "Source_ID", "Parcel_Link")

hcnparc_loc_ca_tbl <- hcnparc_all_tbl %>% 
  select(all_of( cols_keep)) %>% 
  filter(Loc_State == "CA")

nrow(hcnparc_loc_ca_tbl)
## 16607

## Inner-join to attribute fields
hcnparc_loc_ca_sf <- hcnparc_all_sf %>% 
  inner_join(hcnparc_loc_ca_tbl, by = "MTRSA_LG")

dim(hcnparc_loc_ca_sf)
## 16609  19

##################################################
## Export those parcels located in California to GeoJSON
## (my preferred format for uploading to AGOL, doesn't truncate field names)

## Write to GeoJSON
st_write(obj = hcnparc_loc_ca_sf, 
         dsn = file.path(dir_hcn_out, "hcnparc_loc_ca.geojson"),
         layer = "ca_loc_parcels")

##################################################
## Subset those parcels that were transferred to CA to support 
## the University of California

hcnparc_lg_ca_tbl <- hcnparc_all_tbl %>% 
  select(all_of( cols_keep)) %>% 
  filter(LG_State == "CA")

nrow(hcnparc_lg_ca_tbl)
# 2395

# Join the Shapefile to to the attribute table

hcnparc_lg_ca_sf <- hcnparc_all_sf %>% 
  inner_join(hcnparc_lg_ca_tbl, by = "MTRSA_LG")
hcnparc_lg_ca_sf

## Export to GeoJSON

st_write(hcnparc_lg_ca_sf, 
         dsn = file.path(dir_hcn_out, "hcnparc_lg_ca.geojson"),
         layer = "ca_lg_parcels")



