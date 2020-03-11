## OK to Source on Save

mmap_import_csv <- function(state_abbrev, dir_glo = "glo/data", download_missing = TRUE,
                            rdata_save = TRUE, dir_rdata = "rdata") {
  
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
  
  ## Find the xx_Patent.csv file and verify it exists
  
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
    
  
  ## Import patent records where authority_code == 262201 
  ##  Authority code 262201 = July 2, 1862	State Grant-Agri College)
  
  patents_tbl <- read_csv(file = patents_csv, col_names = TRUE) %>% 
    filter(authority_code == "262201") %>% 
    select(accession_nr, doc_class_code, state_code, authority_code, 
           signature_present, signature_date, total_acres, 
           verify_flag, state_in_favor_of) 
  
  if (nrow(patents_tbl) == 0) {
    cat("No Morril Act patents found for ", state_abbrev, ". Done.\n", sep="")  
    return(NULL)
  }
  
  patents_tbl <- patents_tbl %>% 
    mutate(sig_year = as.POSIXct(signature_date, tz="EST", 
                                 format="%m/%d/%Y %I:%M:%S %p") %>% lubridate::year())
  
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
  
  
  # DEPRECATED - not needed, can make this on the fly
  # Add a column for the URL for the patent record (note this is not the API URL)
  # pat_url_base <- "https://glorecords.blm.gov/details/patent/default.aspx?accession="
  # pat_url_mid <- "&docClass="
  # patents_tbl <- patents_tbl %>% 
  # mutate(doc_url = paste0(pat_url_base, accession_nr, pat_url_mid, doc_class_code))
  # patents_tbl <- patents_tbl %>% mutate(doc_url_mid = pat_url_mid)
  
  #edit(patents_tbl2 %>% select(accession_nr, doc_url))
  #https://glorecords.blm.gov/details/patent/default.aspx?accession=0364-001&docClass=AGS
  
  #################################################################
  ## Next, import the land description csv file
  ld_csv <- file.path(dir_glo, paste0(state_abbrev, "_Land_Description.csv"))
  #ld_csv <- "data/CA_Land_Description.csv"
  if (!file.exists(ld_csv)) stop(paste0("Can't find ", ld_csv))
  
  ## Import land descriptions
  ld_tbl <- read_csv(file = ld_csv, col_names = TRUE) 
  
  #dim(ld_tbl)
  #head(ld_tbl)
  
  ## If you want to look more closely at the parsing problems, run
  ## probs_df <- problems(read_csv(file = ld_csv, col_names = TRUE) )
  ## View(probs_df)
  
  ## TODO 
  ## read about block_nr and survey_nr. Coerce these fields to be characters
  ## Are they important?
  ## table(probs_df$col)
  ## block_nr ld_remarks section_nr  survey_nr 
  ##    3468         20          2       2999 
  
  ## Join the land patent table to the land description table on the accession_nr column 
  ## (accession number). We omit the 'total area' column because these records are now
  ## land descriptions (parcels), whereas the 'total area' column came from the 'patents'
  ## table
  
  patents_ld_tbl <- patents_tbl %>%
    select(accession_nr, doc_class_code, state_code, authority_code,
           signature_present, signature_date, sig_year,
           verify_flag, state_in_favor_of) %>%
    left_join(ld_tbl %>%
                select(accession_nr, descrip_nr, meridian_code, township_nr, township_dir,
                       range_nr, range_dir,
                       section_nr, fractional_section,
                       aliquot_parts),
              by= "accession_nr")
  
  #nrow(patents_ld_tbl)
  #View(patents_ld_tbl)
  
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
  
  
  
  # if (TRUE) {
  #   ## The first character of aliquot_parts is numeric
  #   aliquot_parts_lbl <- " LOT "
  # } else {
  #   aliquot_parts_lbl <- " SEC "
  # }
  # 
  # x <- !grepl("\\D", substr(patents_ld_tbl$aliquot_parts, start = 1, stop = 1))
  # head(x)
  # head(patents_ld_tbl$aliquot_parts)
  # 
  # aliquot_parts_lbl <- if (!grepl("\\D", substr(patents_ld_tbl$aliquot_parts, start = 1, stop = 1))) " LOT " else " SEC "
  # 
  # aliquot_parts_lbl <- ifelse(!grepl("\\D", substr(patents_ld_tbl$aliquot_parts, start = 1, stop = 1))) " LOT " else " SEC "
  # 
  # aliquot_parts_lbl <- if_else(!grepl("\\D", substr(patents_ld_tbl$aliquot_parts, start = 1, stop = 1)), " LOT ", " ALIQ ")
  # head(aliquot_parts_lbl)
  
  ## Construct the sub-section part of the land description.
  ## This requires parsing the aliquot_parts column
  ld_subsect <- ld_subsect_parse(patents_ld_tbl$aliquot_parts)
  
  ## DEPRECATED
  ## Create an empty vector for the legal land description
  ## ld_subsect <- rep(NA, nrow(patents_ld_tbl))
  
  ## Case 1. ALL numeric characters - treat as a lot number
  ## idx <- !grepl("\\D", patents_ld_tbl$aliquot_parts)
  ## ld_subsect[idx] <- paste0(" LOT ", patents_ld_tbl[idx, "aliquot_parts", drop=TRUE])
  ## idx_dealtwith <- which(idx)
  
  ## Case 2. Last digits before you hit alphabetic character are numeric (W½10)
  ## E½25
  ## DONT KNOW HOW TO HANLDE THESE
  
  ## 1SW should become ALIQ SW LOT 1 
  ## 5NW should become ALIQ NW LOT 5
  
  ## Case 3. A numeric number in the middle (E½2NW)
  ## ??
  
  ## Case All Else. Treat as a series of quadrants
  ## idx_else <- (1:nrow(patents_ld_tbl))[-idx_dealtwith]
  ## ld_subsect[idx_else] <- paste0(" ALIQ ", patents_ld_tbl[idx_else, "aliquot_parts", drop=TRUE])
  ## cat("Done.\n")
  
  ## Add the legal land description as a new field.
  ## While we're at it, we'll add columns to record the status of the API call,
  ## and the 'middle part' of the API URL (to trouble-shoot failed searches), 
  patents_ld_tbl <- patents_ld_tbl %>%
    mutate(lld = paste0(state_code, " ", sprintf("%02d", meridian_code),
                        " T", township_nr, township_dir,
                        " R", range_nr, range_dir,
                        " SEC ", section_nr,
                        ld_subsect),
           api_url_middle = NA,
           api_status = NA,
           api_landdescription = NA)
  
  # assign(paste0(state_abbrev, "patents_ld_tbl"),  
  #        patents_ld_tbl %>%  mutate(lld = paste0(state_code, " ", meridian_code, 
  #                       " T", township_nr, township_dir,
  #                       " R", range_nr, range_dir,
  #                       " SEC ", section_nr,
  #                       ld_subsect),
  #          api_url_middle = NA,
  #          api_status = NA,
  #          api_landdescription = NA),
  #        envir =globalenv())
  
  ## Next, we'll create a geometry column with empty polygons. This will get filled in as we call the API
  empty_polys_sfc <- st_sfc(lapply(1:nrow(patents_ld_tbl), function(x) st_polygon()), crs = 4326)
  
  ## Combine the joined tibble and the empty geometry column into a new sf data frame
  ## that we can then start to fill using the API
  patents_ld_sf <- st_sf(patents_ld_tbl, geometry = empty_polys_sfc)
  
  ## Save copies in the global environment
  #assign(paste0(state_abbrev, "_patents_tbl"), patents_tbl, envir =globalenv())
  #assign(paste0(state_abbrev, "_ld_tbl"), ld_tbl, envir =globalenv())
  #assign(paste0(state_abbrev, "_patents_ld_tbl"), patents_ld_tbl, envir =globalenv())
  #assign(paste0(state_abbrev, "_patents_ld_sf"), patents_ld_sf, envir =globalenv())

  
  #objs_created <- c(paste0(state_abbrev, "_patents_tbl"),
    #paste0(state_abbrev, "_ld_tbl"),
  #  paste0(state_abbrev, "_patents_ld_tbl"),
  #  paste0(state_abbrev, "_patents_ld_sf"))
  
  assign(state_abbrev, list(patents_ld_sf = patents_ld_sf,
                            patents_tbl = patents_tbl))
  
  if (rdata_save) {
    cat("Saving tables as RData files...")
    save(list = state_abbrev,
         file = file.path(dir_rdata, paste0(state_abbrev, "_patent_data.RData")) ,
         compress = TRUE,
         compression_level = 6)
    cat("Done\n")
  }
  
  #cat("Objects created:\n")
  #cat(paste0("  ", objs_created, "\n"), sep="")
  
  return(get(state_abbrev))
  
}





