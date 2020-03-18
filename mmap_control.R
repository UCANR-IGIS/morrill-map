## Import Land Patent Records and Create a Polygon Layer


## TODO LIST

## Consider grabbing the section in cases where status == 'fail' (verify 
## this is caused by too little data as opposed to a garbled LD)
## Add a global counter
## Make the 'save results every 1000 recorsd' an option

## Work on state summaries
## AGOL map - show me all the land that went to NY
## Think about what data I want to share

## read about block_nr and survey_nr. Coerce these fields to be characters
## Are they important?
## table(probs_df$col)
## block_nr ld_remarks section_nr  survey_nr 
##    3468         20          2       2999 


## Load required libraries; set default directory locations
source("mmap_load_assets.R")

## Load summaries that have already been completed
load("states_completed.RData"); load("states_nodata.RData") 
load("pat_stats.RData"); load("ld_bad.RData")

#################################################################################
## RUN THRU STATES
## Set the following options
#################################################################################

(states_to_process <- state.abb)
# (states_to_process <- state.abb[41:50])
(states_to_process <- c("SD", "CA", "AZ", "NM", "MI", "ID", "NV")[2])
# (states_to_process <- c("WA", "MN", "OR")[2])

## Skip a state that has been found to have no Morrill Act Patents (or no Patents at all)
skip_nodata <- TRUE

## Skip states that have already been imported and geoms retrieved
skip_completed <- FALSE
  
## If a RData file exists, load it first (regardless if there's one in memory)
(load_rdata <- c("always", "when-state-not-in-memory", "never")[2])

## If a RData file is not found, and the object is not already in Memory,
## try to import from a csv file?
(import_csv_if_needed <- F)

## Which set land descriptions to grab a geom
(get_geoms <- F)
(use_archived_objs <- F)
(pat_idx_option <- c("NAs", "all errors", "Status not returned", "fail", "Num features <> 1", 
                     "all", "var_pat_idx", "Rings list should be length 1 (simple polygon, no holes)")[8])
pat_idx <- 13780

## Pausing options (for API calls)
(pause_after_n = list("1" = 0, "600" = 5, "3900" = 60 * 3))
#(pause_after_n = list("1" = 1, "3900" = 60 * 3))  ## add a 1-sec delay between calls

## Other Processing Steps
save_stats <- F
save_badld <- F

## One-time Processing Tasks - Comment out when completed and will never ever be needed again
# add_sig_year <- FALSE
# geom_convert_single2multi <- FALSE
# add_patentees <- F

compute_lld <- F   ## regenerate the lld value (use after upgrading the aliqot_parts parser)
                    ## uses the same pat_idx_option as get-geom
(make_archive_copy <- F)
(save_archive_copy <- F)

(debugme <- FALSE)

## What to save when each iteration is done
save_ind_rdata <- F
save_ind_geopackage <- FALSE
save_ind_shp <- FALSE
save_ind_geojson <- FALSE

save_comb_rdata <- TRUE   
save_comb_geojson <- FALSE
save_comb_gdb <- TRUE

############################################
## RUN THE PROCESSING LOOP
############################################

mmap_show_opts()

source("mmap_process_states.R")

# x <- states_completed
# states_completed <- states_completed[!states_completed %in% states_nodata]

## In case it didn't save
save(states_completed, file="states_completed.RData")
save(states_nodata, file="states_nodata.RData")

##########################################################
## OR LOAD SAVED RESULTS
##########################################################
(state_abbrev <- c("KS", "SD", "CA", "MI")[4])
load(file.path(dir_rdata, paste0(state_abbrev, "_patent_data.RData")))

##########################################################
## EXPLORE RESULTS
##########################################################
(state_abbrev <- c("KS", "SD", "CA", "NM")[2])

## Plot all
plot(get(state_abbrev)$patents_ld_sf %>% st_geometry(), col="grey", axes=TRUE)

## Plot just the selected rows
plot(get(state_abbrev)$patents_ld_sf %>% 
       slice(pat_idx) %>% 
       st_geometry(), col="grey", axes=TRUE)

## Select on of the selected rows
i <- sample(pat_idx, 1)
plot(get(state_abbrev)$patents_ld_sf %>% slice(i) %>% st_geometry(), col="grey", axes=TRUE)
get(state_abbrev)$patents_ld_sf %>% st_drop_geometry() %>% slice(i) %>%  select(aliquot_parts, api_landdescription)

## View the distribution of api_status values
as.data.frame(table(get(state_abbrev)$patents_ld_sf$api_status))

View(pat_stats)

## Open in a View tab
View(get(state_abbrev)$patents_ld_sf %>% slice(pat_idx))
View(get(state_abbrev)$patents_ld_sf %>% filter(accession_nr == "0363-420"))

## View the unique values of aliquot parts
#View(ld_tbl %>% slice(1:20000) %>% group_by(aliquot_parts) %>% count(aliquot_parts) %>% arrange(aliquot_parts))

View(get(state_abbrev)$patents_ld_sf %>%st_drop_geometry() %>% slice(1:20000) %>% group_by(aliquot_parts) %>% count(aliquot_parts) %>% arrange(aliquot_parts))


##########################################
## TROUBLE SHOOTING API ERRORS
##########################################

## Patterns to process / investigate
##  11SW
##  175
## 2E½NW
## 	2NENW

## (CA2130__.151) W½NE2 needs to become ALIQUOT W2NE LOT 2

(pat_idx_bad <- which(get(state_abbrev)$patents_ld_sf$accession_nr == "0362-310"))
pat_idx_bad <- 26

## Identify ALL unsuccessful calls
pat_idx_bad <- which(get(state_abbrev)$patents_ld_sf$api_status != "success")[1:100]
pat_idx_bad <- pat_idx_bad[!is.na(pat_idx_bad)]
str(pat_idx_bad)

## Identify those where Status was not returned (indicative of a garbled ld)
pat_idx_bad <- which(get(state_abbrev)$patents_ld_sf$api_status == "Status not returned")
str(pat_idx_bad)

## Identify those where Status == failed (indicative of no data)
pat_idx_bad <- which(get(state_abbrev)$patents_ld_sf$api_status == "fail")
str(pat_idx_bad)

## Identify those where Status == Num features <> 1)
pat_idx_bad <- which(get(state_abbrev)$patents_ld_sf$api_status == "Num features <> 1")
str(pat_idx_bad)

## Identify those where Status == maybe a lake)
pat_idx_bad <- which(get(state_abbrev)$patents_ld_sf$api_status == "Rings list should be length 1 (simple polygon, no holes)")
str(pat_idx_bad)

pat_idx_bad <- 436:437

## Print bad rec to console
get(state_abbrev)$patents_ld_sf %>% st_drop_geometry() %>% slice(pat_idx_bad) 


## View the difficult aliquot_parts
View(get(state_abbrev)$patents_ld_sf[pat_idx_bad, "aliquot_parts"])

View(get(state_abbrev)$patents_ld_sf %>% st_drop_geometry() %>% slice(pat_idx_bad) 
     %>% distinct(aliquot_parts) %>% arrange(aliquot_parts) %>% pull(aliquot_parts))

writeClipboard(get(state_abbrev)$patents_ld_sf %>% st_drop_geometry() %>% slice(pat_idx_bad) 
     %>% distinct(aliquot_parts) %>% arrange(aliquot_parts) %>% pull(aliquot_parts))

## Create an HTML page of selected records
mmap_view_recs(get(state_abbrev), sample(pat_idx_bad, 30))
mmap_view_recs(get(state_abbrev), 436:437)

########################################################################
## Repair broken land descriptions

# state_abbrev <- "CA"
# (pat_idx <- 1:nrow(get(state_abbrev)$patents_ld_sf))
# pat_idx <- 2144
# pat_idx <- pat_idx_bad

# CA$patents_ld_sf %>% st_drop_geometry() %>% slice(pat_idx) %>% 
#   select(lld, api_url_middle, api_status, api_landdescription)

assign(state_abbrev, mmap_recompute_ld(get(state_abbrev), pat_idx))

# CA2$patents_ld_sf %>% st_drop_geometry() %>% slice(pat_idx) %>% 
#   select(lld, api_url_middle, api_status, api_landdescription)

## OR load R objects from disk
#load("patents_ld_sf.RData")

##########################################################################
# pat_curnt_recs <- datatable(patents_ld_sf %>% 
#             st_drop_geometry() %>% 
#             slice(pat_idx) %>% 
#             mutate(pat_rec = paste0("<a href='", pat_url_base, accession_nr, 
#                                     pat_url_mid, doc_class_code, "'>", 
#                                     accession_nr, "</a>"),
#                    api_rec = paste0("<a href='", api_url_base, 
#                                     api_url_middle, 
#                                     api_url_end, "'>link</a>")) %>% 
#             select(pat_rec, aliquot_parts, fractional_section, lld, 
#                    api_status, api_rec),
#           escape = FALSE
#           )
# html_fn <- "patent_recs_active.html"; htmlwidgets::saveWidget(pat_curnt_recs, file=html_fn); browseURL(html_fn)

#https://glorecords.blm.gov/details/patent/default.aspx?accession=0351-145&docClass=AGS     #patents_tbl <- patents_tbl %>% 
#  mutate(doc_url = paste0(pat_url_base, accession_nr, pat_url_mid, doc_class_code))
#View(patents_ld_sf[pat_idx,])

## Save to disk
## deprecated: save(patents_ld_sf, file="patents_ld_sf.RData")

##  load("patents_ld_sf.RData")


# ld_sfc <- st_sfc(polys_running_lst, crs = 4326)
# if (FALSE %in% st_is_valid(ld_sfc)) warning("Invalid geometry found")
# if (FALSE %in% st_is_simple(ld_sfc)) warning("Non-simple geometry found")

#parcels_df <- do.call(rbind, attr_tbl_lst) NOT NEEDED

# patents_ld_row1 <- patents_ld_tbl %>% slice(1)

# x <- cbind(patents_ld_row1, attr_running_tbl)
# head(x)
# names(x)

patents_ld_sf <- st_sf(cbind(patents_ld_row1, attr_running_tbl), geom = ld_sfc)
plot(patents_ld_sf %>% st_geometry(), col="red")
View(patents_ld_sf)

#####################################################
## Explore and Analyze

# load("patents_ld_sf.RData")
# load("patents_tbl.RData")

as.data.frame(table(patents_ld_sf$api_status))
plot(patents_ld_sf %>% slice(pat_idx) %>% st_geometry(), col="gray90")



########################################################
## Import MI Land Description

state_abbrev <- "MI"
ld_csv <- file.path(dir_glo, paste0(state_abbrev, "_Land_Description.csv"))
file.exists(ld_csv)
if (!file.exists(ld_csv)) stop(paste0("Can't find ", ld_csv))

## Import land descriptions
ld_coltypes <- cols(
  accession_nr = col_character(),
  doc_class_code = col_character(),
  descrip_nr = col_integer(),
  aliquot_parts = col_character(),
  section_nr = col_integer(),
  township_nr = col_double(),
  township_dir = col_character(),
  range_nr = col_character(),
  range_dir = col_character(),
  block_nr = col_character(),
  fractional_section = col_character(),   ## YN field
  survey_nr = col_character(),
  meridian_code = col_integer(),
  ld_remarks = col_character(),
  state_code = col_character()
)

x <- read_csv(file = ld_csv, col_names = TRUE, col_types = ld_coltypes) 
problems(x)

y <- read.csv(ld_csv, skipNul = TRUE, stringsAsFactors = F)
str(y)

?read.csv

##############
library(readr)

(state_abbrev <- c("MI", "CA")[2])

ld_csv <- file.path(dir_glo, paste0(state_abbrev, "_Land_Description.csv"))
file.exists(ld_csv)

d <- read_file_raw(ld_csv)
# Convert nulls to spaces
sum(d == 0)

d[d == 0] <- as.raw(32)

x <- read_csv(d, col_names = TRUE, col_types = ld_coltypes) 
str(x)

##############################

data.frame(state = state_abbrev,
           num_pat = nrow(get(state_abbrev)$patents_tbl),
           num_ld = nrow(get(state_abbrev)$patents_ld_sf),
           geom_nostatus = sum(get(state_abbrev)$patents_ld_sf$api_status == "Status not returned", na.rm = TRUE),
           geom_fail = sum(get(state_abbrev)$patents_ld_sf$api_status == "fail"),
           geom_multfeats = sum(get(state_abbrev)$patents_ld_sf$api_status == "Num features <> 1"),
           geom_island = sum(get(state_abbrev)$patents_ld_sf$api_status == 
                               "Rings list should be length 1 (simple polygon, no holes)"),
           geom_success_num = sum(get(state_abbrev)$patents_ld_sf$api_status == "success"),
           
           geom_success_pct = round(sum(get(state_abbrev)$patents_ld_sf$api_status == "success") /
                                      nrow(get(state_abbrev)$patents_ld_sf), 2) )

y <- get(state_abbrev)$patents_ld_sf$api_status
str(y)
table(y == "fail")
sum(is.na(y))

