
## Initialize objects needed to save results
if (save_geojson_comb) comb_sf <- NULL
if (save_badld) ld_bad <- list()
if (save_stats) pat_stats <- NULL

for (state_abbrev in states_to_process) {

  cat(state_abbrev, "\n")  
  if (skip_completed && (state_abbrev %in% states_completed)) {
    cat(crayon::yellow("  ", state_abbrev, "already completed. Skipping."), "\n") 
    next
  }
  
  if (skip_nodata && (state_abbrev %in% states_nodata)) {
    cat(crayon::yellow("  ", state_abbrev, "has no Morrill Act patents. Skipping."), "\n") 
    next
  }

  ## Load a saved RData file if needed
  if ((load_rdata == "always") || (load_rdata == "when-state-not-in-memory" && !exists(state_abbrev))) {
    rdata_fn <- file.path(dir_rdata, paste0(state_abbrev, "_patent_data.RData"))
    if (file.exists(rdata_fn)) {
      load(rdata_fn)
      #rdata_found <- TRUE
      cat(crayon::green("   imported rdata file for ", state_abbrev, sep=""), "\n")
    }
  }

  ## Import a CSV file if needed
  if (!exists(state_abbrev) && import_csv_if_needed) {
    if (import_csv_if_needed) {
      cat("   going to try to import a csv for", state_abbrev, "\n")
      assign(state_abbrev, mmap_import_csv(state_abbrev, 
                                           dir_glo = dir_glo, 
                                           dir_rdata = dir_rdata))
      ## If NULL was returned, record this as a state that has no data
      if (is.null(get(state_abbrev))) {
        if (!state_abbrev %in% states_nodata) states_nodata <- sort(c(states_nodata, state_abbrev))  
      }
    }
  } 

  if (exists(state_abbrev)) {
    if (!rdata_found && !import_csv_if_needed) {
      cat(crayon::green("   Using", state_abbrev, "in memory."), "\n") 
    }
  } else {
    cat(crayon::yellow("   No data for", state_abbrev, ". Skipping."), "\n") 
    next
  }

    ## If some patent records / ld were found, keep going
  if (!is.null(get(state_abbrev))) {
    
    ## View the number of patents and number of land descriptions
    cat(crayon::green("   number of patent records for ", state_abbrev, ": ", 
        nrow(get(state_abbrev)$patents_tbl), sep=""), "\n", sep="")
    cat(crayon::green("   number of land descriptions for ", state_abbrev, ": ", 
        nrow(get(state_abbrev)$patents_ld_sf), sep=""), "\n", sep="")

    if (save_stats) {
      pat_stats <- rbind(pat_stats,
         data.frame(state = state_abbrev,
            num_pat = nrow(get(state_abbrev)$patents_tbl),
            num_ld = nrow(get(state_abbrev)$patents_ld_sf),
            geom_nostatus = sum(get(state_abbrev)$patents_ld_sf$api_status == "Status not returned"),
            geom_fail = sum(get(state_abbrev)$patents_ld_sf$api_status == "fail"),
            geom_multfeats = sum(get(state_abbrev)$patents_ld_sf$api_status == "Num features <> 1"),
            geom_island = sum(get(state_abbrev)$patents_ld_sf$api_status == 
                                "Rings list should be length 1 (simple polygon, no holes)"),
            geom_success_num = sum(get(state_abbrev)$patents_ld_sf$api_status == "success"),
            
            geom_success_pct = round(sum(get(state_abbrev)$patents_ld_sf$api_status == "success") /
              nrow(get(state_abbrev)$patents_ld_sf), 2) ) )
    }

    
    if (save_badld) {
      ld_bad[[state_abbrev]] <- 
        list(nostatus = which(get(state_abbrev)$patents_ld_sf$api_status == "Status not returned"),
             fail = which(get(state_abbrev)$patents_ld_sf$api_status == "fail"),
             multfeats = which(get(state_abbrev)$patents_ld_sf$api_status == "Num features <> 1"),
             island = which(get(state_abbrev)$patents_ld_sf$api_status == 
                              "Rings list should be length 1 (simple polygon, no holes)") )
    }
        
    ## Compute the year signed (one-time-only-task)
    if (add_sig_year) {
      assign(state_abbrev, list(patents_tbl = get(state_abbrev)$patents_tbl %>% 
                                  mutate(sig_year = as.POSIXct(signature_date, tz="EST", 
                                      format="%m/%d/%Y %I:%M:%S %p") %>% lubridate::year()),
                                patents_ld_sf = get(state_abbrev)$patents_ld_sf %>% 
                                        mutate(sig_year = as.POSIXct(signature_date, tz="EST",
                                      format="%m/%d/%Y %I:%M:%S %p") %>% lubridate::year())))
      
    }

    if (geom_convert_single2multi) {
      if (is(get(state_abbrev)$patents_ld_sf$geometry, "sfc_POLYGON")) {
        assign(state_abbrev, list(patents_tbl = get(state_abbrev)$patents_tbl,
                                  patents_ld_sf = st_cast(get(state_abbrev)$patents_ld_sf, "MULTIPOLYGON")) )
        cat(crayon::yellow("   converted ", state_abbrev, " from single to multipolygon"), "\n") 
      } else if (is(get(state_abbrev)$patents_ld_sf$geometry, "sfc_MULTIPOLYGON")) {
        cat(crayon::yellow("   ", state_abbrev, " is already multipolygon. Skipping.", sep=""), "\n") 
        
      }
    }
    
    if (get_geoms || compute_ld) {
      ## Select which rows to process (pat_idx) by running *one* of the following
      if (pat_idx_option == "NAs") {
        pat_idx <- which(is.na(get(state_abbrev)$patents_ld_sf$api_status))  
        
      } else if (pat_idx_option == "all errors") {
        pat_idx <- which(get(state_abbrev)$patents_ld_sf$api_status != "success") ## excludes NAs
        
      } else if (pat_idx_option == "Status not returned") {
        pat_idx <- which(get(state_abbrev)$patents_ld_sf$api_status == "Status not returned")
        
      } else if (pat_idx_option == "fail") {
        pat_idx <- which(get(state_abbrev)$patents_ld_sf$api_status == "fail")
        
      } else if (pat_idx_option == "Num features <> 1") {
        pat_idx <- which(get(state_abbrev)$patents_ld_sf$api_status == "Num features <> 1")
        
      } else if (pat_idx_option == "all") {
        pat_idx <- 1:nrow(get(state_abbrev)$patents_ld_sf)
        
      } else if (pat_idx_option == "var_pat_idx" ) {
        if (!exists("pat_idx")) stop("pat_idx needs to be defined")
        
      } else {
        stop("Unknown value for pat_idx_option")
      }
      
    }
    
    ## (Re)compute the land description field (from which the API URL is made)
    if (compute_ld) {
      if (length(pat_idx) == 0) {
        cat(crayon::yellow("   no land descriptions match the criteria. Recompute lld"), "\n")
      } else {
        assign(state_abbrev, recompute_ld(get(state_abbrev), pat_idx) )
      }
      
    }
    
    #################################################################################
    ## Loop through API and update geometries
    #################################################################################
    if (get_geoms) {
      
      ## Choice 2. Select a number of rows manually 
      ## This might be good for testing purposes
      # (pat_idx <- 1:nrow(get(state_abbrev)$patents_ld_sf))
      
      ## Chioce 3. Select a specific row
      ## This might be useful if you're trouble-shooting
      # pat_idx <- 1:20
      # pat_idx <- 37:39
      
      ## RESET THE COLUMNS THAT HAVE BEEN (OR WILL BE) UPDATED BY THE API for the selected rows
      ## You might do this if you want to to 'rerun' some land descriptions.
      # assign(state_abbrev, mmap_blank_api_cols(get(state_abbrev), pat_idx))
      
      ## RUN THE LOOP TO FILL THE GEOMETRY
      if (length(pat_idx) == 0) {
        cat(crayon::yellow("   no land descriptions match the criteria. Skipping API calls."), "\n")
      } else {
        assign(state_abbrev, mmap_fill_geom(get(state_abbrev), pat_idx, 
                                            pause_after_n = pause_after_n))
      }
      
    }

    ##########################################################
    ## (Re)save to disk
    ## CAUTION: This will overwrite existing files
    ##########################################################
    
    if (save_rdata) {
      cat(crayon::green("   saving ", state_abbrev, "_patent_data.RData", sep = ""), "\n")
      save(list = state_abbrev,
           file = file.path(dir_rdata, paste0(state_abbrev, "_patent_data.RData")) ,
           compress = TRUE,
           compression_level = 6)
    }
    
    
    ## Identify non-null features
    # geom_found_idx <- which(!is.na(st_dimension(get(state_abbrev)$patents_ld_sf)))
    # str(geom_found_idx)
    #patents_ld_good_sf <- patents_ld_sf[geom_found_idx,]

    ## Export to Shapefile
    if (save_shp) {
      shp_fn <- file.path(dir_shp, paste0(state_abbrev, "_patents_ld.shp"))
      if (file.exists(shp_fn)) {
        cat(shp_fn, "already exists. Will not overwrite \n")
      } else {
        st_write(get(state_abbrev)$patents_ld_sf, dsn = shp_fn)
      }
    }
    
    ## Export to GeoJSON - Individual States
    if (save_geojson_ind) {
     st_write(get(state_abbrev)$patents_ld_sf, 
              dsn=file.path(dir_geojson, paste0(state_abbrev, "_patents_ld.geojson")),
              delete_dsn = TRUE)
    }
    
    ## Generated a Combined Simple Feature
    if (save_geojson_comb) {
      if (is.null(comb_sf)) {
        comb_sf <- get(state_abbrev)$patents_ld_sf
      } else {
        comb_sf <- rbind(comb_sf, get(state_abbrev)$patents_ld_sf)
      }
    }
    
        ## Export to geopackage
    if (save_geopackage) {
      ## Add the land descriptions polygon layer (may have a number of missing polygons)
      st_write(get(state_abbrev)$patents_ld_sf,
               dsn = comb_geopackage_fn,
               layer = paste0(state_abbrev, "_patents_ld"),
               delete_layer = TRUE)
      
      ## Add the patents table (non-spatial)
      con <- dbConnect(SQLite(), dbname = comb_geopackage_fn)
      #dbListTables(con)
      dbWriteTable(con, paste0(state_abbrev, "_patents_tbl"), 
                   get(state_abbrev)$patents_tbl, overwrite = TRUE)
      dbDisconnect(con)
     }
    
  
  }   #if (!is.null(get(state_abbrev)))
  
  
  if (!state_abbrev %in% states_completed) states_completed <- c(states_completed, state_abbrev) %>% sort()
  
}

save(states_completed, file="states_completed.RData")

## Generated a Combined Simple Feature
if (save_geojson_comb) {
  st_write(comb_sf, dsn = comb_geojson_fn, delete_dsn = TRUE)
}

if (save_stats) {
  save(pat_stats, file="pat_stats.RData")
  cat("pat_stats (re)created and saved\n")
}

if (save_badld) {
  save(ld_bad, file="ld_bad.RData")
  cat("ld_bad (re)created and saved\n")
}


