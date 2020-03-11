# load("patents_tbl.RData")
# load("patents_ld_sfl.RData")

state_abbrev <- "CA"
load(file.path(dir_rdata, paste0(state_abbrev, "_patent_data.RData")))

## Add year to the attribute table
# CA$patents_tbl <- CA$patents_tbl %>% 
#   mutate(sig_year = as.POSIXct(signature_date, tz="EST",
#                                format="%m/%d/%Y %I:%M:%S %p") %>% lubridate::year())
# names(CA$patents_tbl)
# 
# ## Export to Shp
# shp_fn <- file.path(dir_shp, paste0(state_abbrev, "_patents_ld.shp"))
# if (file.exists(shp_fn)) cat(shp_fn, "already exists! \n")
# st_write(get(state_abbrev)$patents_ld_sf, dsn = shp_fn, delete_dsn = TRUE)



## Import Patentee table
patentee_csv <- "data/CA_Patentee.csv"
file.exists(patentee_csv)
patentee_tbl <- read_csv(file = patentee_csv, col_names = TRUE) %>% 
  mutate(patentee_name_comb = paste(patentee_last_name,
                                    patentee_first_name, 
                                    patentee_middle_name,
                                    sep="-"))



## Join it to the patents table DEPRECATED B/C ONE-TO-MANY RELATIONSHIP
# patents_patentee_tbl <- patents_tbl %>% 
#   left_join(patentee_tbl, by=c("accession_nr"))
# 
# View(patents_patentee_tbl %>% slice(1:30))
# names(patents_patentee_tbl)

##################################################
## Basic Stats

## Num patents
nrow(get(state_abbrev)$patents_tbl)

## Number by document type
get(state_abbrev)$patents_tbl %>% count(doc_class_code) 

## Number by authority code
## (262201 is the Morril Act)
get(state_abbrev)$patents_tbl %>% count(authority_code) 

## Number of successful geometries
as.data.frame(table(get(state_abbrev)$patents_ld_sf$api_status))

######################################
## ACREAGE - Overall Totals

## Total acreage
get(state_abbrev)$patents_tbl %>% summarise(total_acreage = sum(total_acres))

## Number of acres per state
x <- get(state_abbrev)$patents_tbl %>% 
       filter(!is.na(state_in_favor_of)) %>% 
       group_by(state_in_favor_of) %>% 
  summarise(num_patents = n(),
            total_acres = sum(total_acres) %>% round() %>% format(big.mark = "," )) %>% 
  arrange(desc(total_acres))
write_csv(x, path = "state_in_favor.csv") 
View(x)
######################################
## ACREAGE - Distribution

state_abbrev <- "CA"

## Summary stats of acreage
summary(get(state_abbrev)$patents_tbl %>% pull(total_acres))

## Look at the largest acreage
View(get(state_abbrev)$patents_tbl %>% top_n(50, total_acres) %>% 
  arrange(desc(total_acres)) %>% select(total_acres))

## Number of land parcels per patent - frequency table
as.data.frame(table(get(state_abbrev)$patents_ld_sf$descrip_nr))
hist(get(state_abbrev)$patents_ld_sf$descrip_nr, breaks = 50)

## Histogram of acreage < 1000 
hist(get(state_abbrev)$patents_tbl %>% filter(total_acres < 1000) %>% pull(total_acres), breaks = 20) 

## Cummulative acreage curve
acres_ordered <- get(state_abbrev)$patents_tbl %>% pull(total_acres) %>% sort()
acres_cumsum <- cumsum(acres_ordered)
plot(x=1:length(acres_ordered), y=acres_cumsum, type="l", ylab="Cummulative Acres", xlab="Patents", 
     main = "CA Morrill Act Patents\nCummulative Acreage")

#################################################
## Patentees

## Number of patentees per patent
## needs work
## patents_patentee_tbl %>% count(accession_nr) 

## Number of unique patentees
## DEPRECATED
## get(state_abbrev)$patents_patentee_tbl %>% select(patentee_name_comb) %>% n_distinct() 

####################################################
## Geographic Spread

plot(get(state_abbrev)$patents_ld_sf %>% slice(geom_found_idx) %>% st_geometry(), col="gray90")


## Acreage per county


## Temporal distribution
## acreage over time
patents_year <- CA$patents_tbl %>% 
  mutate(sig_year = as.POSIXct(signature_date, tz="EST",
                               format="%m/%d/%Y %I:%M:%S %p") %>% lubridate::year())

names(patents_year)
table(patents_year$sig_year)

acres_per_year_actual <- patents_year %>% group_by(sig_year) %>% summarise(acres = sum(total_acres))
acres_per_year_all <- data.frame(sig_year = seq(from = min(patents_year$sig_year, na.rm = TRUE),
                                                to = max(patents_year$sig_year, na.rm = TRUE))) %>%
  left_join(acres_per_year_actual, by = "sig_year")

View(acres_per_year_all)

idx <- which(acres_per_year_all$sig_year <= 1900)

barplot(height=acres_per_year_all$acres[idx], names.arg=acres_per_year_all$sig_year[idx], cex.names=0.75, 
          main="Morrill Act Patents:\nAcres Per Year (CA)", xlab="", ylab="acres")


