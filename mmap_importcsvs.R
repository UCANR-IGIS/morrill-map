## OK to Source on Save

mmap_import_csv <- function(state_abbrev, dir_glo = "glo/data", download_missing = TRUE,
                            dir_rdata = "rdata") {
  
  ## IMPORT CSV DATA
  
  ## This script will import land patent csv files downloaded from the BLM, 
  ## and generate a "blank" sf data frame called patents_ld_sf
  
  ## You typically do this only once. 
  
  ## These commands are designed to be run at the R console, either one-by-one or 
  ## by sourcing this script (e.g., source(""))
  
  ##############################################################
  ## DOWNLOADING THE CSV FILES  from the BLM 
  ##############################################################
  
  ## Download the data for your state from https://glorecords.blm.gov/BulkData/default.aspx
  ## Unzip the collection of csv files put them into the 'data' directory in the active directory
  
  ## Citation. In using the provided images or data, you are agreeing to include a reference to 
  ## the Bureau of Land Management, Eastern States Office and include a link to 
  ## https://glorecords.blm.gov from any application or website utilizing them.
  
  if (missing(state_abbrev)) stop("variable state_abbrev not found")
  
  patents_csv <- file.path(dir_glo, paste0(state_abbrev, "_Patent.csv"))
  # patents_csv <- "data/CA_Patent.csv"
  if (!file.exists(patents_csv)) {
    if (download_missing) {
      cat("Going to try to download data file for", state_abbrev, "\n")
      glo_zip_fn <- file.path("glo", bulk_data_glo(state_abbrev, fnonly = TRUE))
      glo_zip_url <- bulk_data_glo(state_abbrev)
      
      ## Test to see if the URL is valid
      resp <- httr::GET(glo_zip_url)
      
      if (resp$status_code == 200) {
        download.file(url = glo_zip_url, destfile = glo_zip_fn)
        if (file.exists(glo_zip_fn)) {
          unzip(glo_zip_fn, exdir = "glo")
        } else {
          stop(paste0("Couldn't download ", glo_zip_fn))
        }
      } else {
        cat("No data file found for", state_abbrev, "on GLO Bulk Data\n")  
        return(NULL)
      }

    } else {
      stop(paste0("Can't find ", patents_csv))  
    }

  }  
    
  
  ## See if there are any embedded nuls
  patents_raw <- read_file_raw(patents_csv)
  if (sum(patents_raw == 0) > 0) {
    cat(crayon::yellow("   Found embedded nuls in ", patents_csv, ". Converting to spaces. \n", sep = "")) 
    patents_raw[patents_raw == 0] <- as.raw(32)
    patents_import <- patents_raw
  } else {
    patents_import <- patents_csv
  }
  
  ## Import patent records where authority_code == 262201 
  ##  Authority code 262201 = July 2, 1862	State Grant-Agri College)
  
  date_fmt <- "%m/%d/%Y %I:%M:%S %p"
  #parse_date(x, format = fmt_str)
  
  patents_coltypes <- cols(
    accession_nr = col_character(),
    doc_class_code = col_character(),
    state_code = col_character(),
    blm_serial_nr = col_character(),
    authority_code = col_integer(),
    document_nr = col_character(),
    misc_document_nr = col_character(),
    indian_allotment_nr = col_character(),
    tribe = col_character(),
    l_o_code = col_integer(),
    
    #signature_present = col_logical(),   
    signature_present = col_character(), ## YN text
    
    #signature_date = col_datetime(),
    signature_date = col_date(format = date_fmt),
    
    #subsurface_reserved = col_logical(),
    subsurface_reserved = col_character(), ## YN text
    
    #metes_bounds = col_logical(),
    metes_bounds = col_character(), ## YN text
    
    #survey_date = col_datetime(),
    survey_date = col_date(format = date_fmt),
    
    #us_reservations = col_logical(),
    us_reservations = col_character(), ## YN text
    
    #cancelled_doc = col_logical(),
    cancelled_doc = col_character(), ## YN text
    
    geographic_name = col_character(),
    total_acres = col_double(),
    remarks = col_character(),
    
    #verify_flag = col_logical(),
    verify_flag = col_character(), ## YN text
    
    image_page_nr = col_integer(),
    military_rank = col_character(),
    militia = col_character(),
    alt_accession_nr = col_integer(),
    state_in_favor_of = col_character(),
    supreme_court_script_nr = col_character(),
    certificate_of_location = col_character(),
    coal_entry_nr = col_character()
  )
  
  patents_tbl <- read_csv(patents_import, col_names = TRUE, col_types = patents_coltypes) %>% 
    filter(authority_code == "262201") %>% 
    select(accession_nr, doc_class_code, state_code, authority_code, 
           signature_present, signature_date, total_acres, geographic_name,
           verify_flag, state_in_favor_of) %>% 
    mutate(sig_year = as.POSIXct(signature_date, tz="EST", 
                                 format="%m/%d/%Y %I:%M:%S %p") %>% lubridate::year())
  
  if (nrow(patents_tbl) == 0) {
    cat("No Morril Act patents found for ", state_abbrev, ". Done.\n", sep="")  
    return(NULL)
  }
  
  # patents_tbl <- patents_tbl %>%  MOVED ABOVE
  #   mutate(sig_year = as.POSIXct(signature_date, tz="EST", 
  #                                format="%m/%d/%Y %I:%M:%S %p") %>% lubridate::year())
  
  ## For testing purposes, select 10
  ## %>% sample_n(10)
  
  ## View results
  # dim(patents_tbl) 
  # View(patents_tbl) 
  
  ###################################################################################
  ## Note, we could have also used doc_class == "AGS" (Agricultural Scrip Patent), 
  ## which for California results in about 500 fewer patents. Not sure why.
  ## TODO fine out why doc_class == "AGS" and authority_code == 262201 produce 
  ## slightly different numbers of records (10400 and 10920 resp)
  ###################################################################################
  
  
  ###################################################################################
  # DEPRECATED - not needed, can make this on the fly
  #
  # Add a column for the URL for the patent record (note this is not the API URL)
  # looks like: https://glorecords.blm.gov/details/patent/default.aspx?accession=0364-001&docClass=AGS
  # pat_url_base <- "https://glorecords.blm.gov/details/patent/default.aspx?accession="
  # pat_url_mid <- "&docClass="
  # patents_tbl <- patents_tbl %>% 
  # mutate(doc_url = paste0(pat_url_base, accession_nr, pat_url_mid, doc_class_code))
  # patents_tbl <- patents_tbl %>% mutate(doc_url_mid = pat_url_mid)
  ###################################################################################
  
  ###################################################################################
  ## Import Patentee table
  ###################################################################################
  
  patentee_csv <- file.path(dir_glo, paste0(state_abbrev, "_Patentee.csv"))  
  if (!file.exists(patentee_csv)) stop(paste(patentee_csv, "not found"))

  ## See if there are any embedded nuls
  patentee_raw <- read_file_raw(patentee_csv)
  if (sum(patentee_raw == 0) > 0) {
    cat(crayon::yellow("   Found embedded nuls in ", patentee_csv, ". Converting to spaces."), "\n", sep = "") 
    patentee_raw[patentee_raw == 0] <- as.raw(32)
    patentee_import <- patentee_raw
  } else {
    patentee_import <- patentee_csv
  }
    
  patentee_tbl <- read_csv(file = patentee_import, col_names = TRUE, 
                           col_types = cols(
                             accession_nr = col_character(),
                             doc_class_code = col_character(),
                             patentee_seq_nr = col_integer(),
                             patentee_last_name = col_character(),
                             patentee_first_name = col_character(),
                             patentee_middle_name = col_character() )) %>% 
                    mutate(patentee_name_comb = paste0(patentee_last_name,
                             if_else(is.na(patentee_first_name), "", paste0("-", patentee_first_name)),
                             if_else(is.na(patentee_middle_name), "", paste0("-", patentee_middle_name))
    )
    )
  
  accession_patentees_tbl <- patentee_tbl %>% 
    semi_join(patents_tbl, by = "accession_nr") %>% 
    group_by(accession_nr) %>% 
    summarise(patentees = paste(patentee_name_comb, collapse = "; "))
  
  patents_tbl <- patents_tbl %>% left_join(accession_patentees_tbl, by = "accession_nr") 

  ###################################################################################
  ## Import the land description csv file
  ###################################################################################

  ld_csv <- file.path(dir_glo, paste0(state_abbrev, "_Land_Description.csv"))
  if (!file.exists(ld_csv)) stop(paste0("Can't find ", ld_csv))
  
  ## See if there are any embedded nuls
  ld_raw <- read_file_raw(ld_csv)
  if (sum(ld_raw == 0) > 0) {
    cat(crayon::yellow("   Found embedded nuls in ", ld_csv, ". Converting to spaces."), "\n", sep = "") 
    ld_raw[ld_raw == 0] <- as.raw(32)
    ld_import <- ld_raw
  } else {
    ld_import <- ld_csv
  }

  ## Import land descriptions
  ld_coltypes <- cols(
    accession_nr = col_character(),
    doc_class_code = col_character(),
    descrip_nr = col_integer(),
    aliquot_parts = col_character(),
    section_nr = col_integer(),      ## in CO these look like 22.U (perhaps U is unknown?)
    #township_nr = col_character(),
    township_nr = col_double(),
    township_dir = col_character(),
    range_nr = col_character(),
    range_dir = col_character(),  ## need to import as char because a small number look like "53.A"
    block_nr = col_character(),
    fractional_section = col_character(),   ## YN field
    survey_nr = col_character(),
    meridian_code = col_integer(),
    ld_remarks = col_character(),
    state_code = col_character()
  )
  
  # ld_tbl <- read_csv(file = ld_csv, col_names = TRUE, col_types = ld_coltypes)
  ld_tbl <- read_csv(ld_import, col_names = TRUE, col_types = ld_coltypes)
  
  ######################################################################################################  
  ## Join the land patent table to the land description table on the accession_nr column 
  ## (accession number). We omit the 'total area' column because these records are now
  ## land descriptions (parcels), whereas the 'total area' column came from the 'patents' table
  ######################################################################################################
  
  patents_ld_tbl <- patents_tbl %>%
    select(accession_nr, doc_class_code, state_code, authority_code,
           signature_present, signature_date, sig_year, patentees, 
           verify_flag, state_in_favor_of) %>%
    left_join(ld_tbl %>%
                select(accession_nr, descrip_nr, meridian_code, township_nr, township_dir,
                       range_nr, range_dir,
                       section_nr, fractional_section,
                       aliquot_parts),
              by= "accession_nr")
  

  ## Do some checks to see if we got any 'bad' land descriptions
  ## NEEDS WORK
  # idx <- 1
  # if (ca_patents_morrill_landdesc_tbl %>%
  #     slice(idx) %>%
  #     select(fractional_section) != "N") {
  #   warning("Found a record where the fraciontal section is not 'N'")
  # }
  # if (!ca_patents_morrill_landdesc_tbl %>%
  #     slice(idx) %>%
  #     select(section_nr) %>%
  #     numeric_string()) {
  #   warning("Found a record where the section number is not a numeric string")
  # }
  
  

  ######################################################################################################  
  ## Construct the sub-section part of the land description.
  ## This requires parsing the aliquot_parts column
  ######################################################################################################  

  ld_subsect <- ld_subsect_parse(patents_ld_tbl$aliquot_parts)

  ######################################################################################################  
  ## Add the legal land description as a new field.
  ## While we're at it, we'll add columns to record the status of the API call,
  ## and the 'middle part' of the API URL (to trouble-shoot failed searches), 
  ######################################################################################################  
  
  patents_ld_tbl <- patents_ld_tbl %>%
    mutate(lld = paste0(state_code, " ", sprintf("%02d", meridian_code),
                        " T", township_nr, township_dir,
                        " R", gsub(".0$", "", range_nr)  , range_dir,
                        " SEC ", section_nr,
                        ld_subsect),
           api_url_middle = NA,
           api_status = NA,
           api_landdescription = NA)

  ## Next, we'll create a geometry column with empty polygons. This will get filled in as we call the API
  empty_polys_sfc <- st_sfc(lapply(1:nrow(patents_ld_tbl), function(x) st_multipolygon()), crs = 4326)
  
  ## Combine the joined tibble and the empty geometry column into a new sf data frame
  ## that we can then start to fill using the API
  patents_ld_sf <- st_sf(patents_ld_tbl, geometry = empty_polys_sfc)

  ## Return a list object
  list(patents_tbl = patents_tbl, patents_ld_sf = patents_ld_sf)
}


