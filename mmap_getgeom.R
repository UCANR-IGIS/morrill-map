## Gets geoms from the GLO API
## Contains a single function - ok to Source on Save

mmap_fill_geom <- function(dfs, pat_idx, state_abbrev, 
                          pause_after_n = list("1" = 0, "3900" = 60 * 5),
                          save_api_urls = c("on_fail", "on_success")[1],
                          use_archived_objs = use_archived_objs) {

  ## pause_after_n defines the number of seconds to pause after each n api calls
  ## This can help to reduce timeouts or other issues on the server
  ## pause_after_n should be a list where the element name is the nth API call,
  ## and the value is the number of seconds to wait.
  ## pause_after_n <- list("1" = 0, "3900" = 60 * 5)
  
  ## save_api_urls determines when to save the 'middle' of API URL in the sf data frame
  ## Saving the URL when the API fails can help trouble-shoot problems
  ## Saving the URL for successful calls can you verify the geom was retrieved 
  ## and parsed correctly.
  ## The only down side to saving URLs is the memory required
  # save_api_urls <- c("on_fail", "on_success")[1]
  
  if (use_archived_objs) {
    rdata_fn <- file.path(dir_rdata, paste0("zbak_", state_abbrev, "_patent_data.RData"))
    if (file.exists(rdata_fn)) {
      load(rdata_fn)
      cat(crayon::green("   Found RData file for archived ", state_abbrev, sep=""), "\n")
      geom_source <- "archived_obj"
    } else {
      cat(crayon::yellow("   archived source not found, using BLM API \n"))
      geom_source <- "blm"
    }
  } else {
    geom_source <- "blm"
  }
  
  num_ld <- length(pat_idx)
  num_geoms_retrieved <- 0
  
  if (geom_source == "blm") {
    cat(crayon::yellow("   Getting geoms through BLM FindLD service"), "\n")
    
    n_to_pause <- as.numeric(names(pause_after_n)[unlist(pause_after_n) != 0])

    for (i in 1:num_ld) {
      idx <- pat_idx[i]
      
      if (is.na(idx)) next

      ## Pause after n. Note: I put the pause at the top of the loop due to the 
      ## large number of 'next' commands within
      if (0 %in% ((i - 1) %% n_to_pause)) {
        ## Need to pause
        if (i > 1) {
          pause_num <- tail(n_to_pause[((i - 1) %% n_to_pause) == 0], n= 1)
          if (pause_after_n[[as.character(pause_num)]] > 4) {
            cat(crayon::yellow("  =================================="), "\n")
            cat(crayon::yellow("  = Pausing", pause_after_n[[as.character(pause_num)]], "seconds"), "\n")
            cat(crayon::yellow("  =================================="), "\n")
          }
          Sys.sleep(pause_after_n[[as.character(pause_num)]])
        }
      }
      
      ## Save to disk every 500
      if ((i %% 500) == 0) {
        cat(crayon::green("   Saving RData file to disk"), "\n")
        
        ## Reset the bounding box to reflect the new geometries
        dfs$patents_ld_sf <- dfs$patents_ld_sf %>% slice(1:nrow(dfs$patents_ld_sf))
        
        ## Save dfs to state_abbrev (in the local namespace)
        assign(state_abbrev, dfs)
        
        ## Save to disk
        save(list = state_abbrev,
           file = file.path(dir_rdata, paste0(state_abbrev, "_patent_data.RData")) ,
           compress = TRUE, compression_level = 6)
      
      }
      
      ## Get the land description for this row
      # lld_str <- "NV 21 T38N R56E SEC 10 ALIQ SESW"
      lld_str <- dfs$patents_ld_sf %>% 
        st_drop_geometry() %>% 
        slice(idx) %>% 
        pull(lld)
      
      ## Turn this into a URL piece, making two substitutions
      ## (combining these into a single piped statement didn't work - not sure why)
      api_url_middle <- gsub(" ", "+", lld_str) 
      api_url_middle <- gsub("½", "2", api_url_middle, useBytes = FALSE) 
      
      # print(api_url_middle)
      #browser()
      # 
      
      ## Construct the URL for the API call
      #api_url_comb <- paste0(api_url_base, api_url_middle, api_url_end)
      # api_url_comb <- api_url(api_url_middle)
      
      #writeClipboard(url_comb)
      
      ## Extract the accession_nr
      patent_nr <- dfs$patents_ld_sf %>% 
        st_drop_geometry() %>% 
        slice(idx) %>% select(accession_nr) %>% as.character()
      
      cat("   ", i, " of ", num_ld, ". (", patent_nr, ", row ", idx, "): ", lld_str, "\n", sep="")
      if (debugme) cat("   ", crayon::yellow(api_url_middle), "\n", sep="")
      
      #cat("  ", url_middle, "\n")
      #cat("  ", lld_str, "\n")
      
      #browser()
      
      ## Get resposne from the server
      findld_resp <- httr::GET(api_url(api_url_middle))
      
      ## Check for a server connection error & save the URL for trouble-shooting
      if (findld_resp$status_code != 200) {
        err_msg <- httr::http_status(findld_resp$status_code)$message
        cat("     ", crayon::red(err_msg), "\n")
        dfs$patents_ld_sf[idx, "api_status"] <- err_msg
        if ("on_fail" %in% save_api_urls) dfs$patents_ld_sf[idx, "api_url_middle"] <- api_url_middle
        next
        ## see http://en.wikipedia.org/wiki/Http_status_codes for more info
      } 
      
      ## Parse the response ojbect into a list
      findld_json <- httr::content(findld_resp, type = "application/json")
      
      ## Check for more errors
      if (is.null(findld_json$status)) {
        err_msg <- "Status not returned"
        cat("     ", crayon::red(err_msg), "\n")
        dfs$patents_ld_sf[idx, "api_status"] <- err_msg
        if ("on_fail" %in% save_api_urls) dfs$patents_ld_sf[idx, "api_url_middle"] <- api_url_middle
        next
      } 
      
      if (findld_json$status != "success") {
        err_msg <- findld_json$status
        cat("    ", crayon::red(err_msg), "\n")
        dfs$patents_ld_sf[idx, "api_status"] <- err_msg
        if ("on_fail" %in% save_api_urls) dfs$patents_ld_sf[idx, "api_url_middle"] <- api_url_middle
        next
      } 
      
      if (length(findld_json$features) != 1) {
        # NO LONGER AN ERROR
        # err_msg <- "Num features <> 1"
        # cat("  ", crayon::red(err_msg), "\n")
        cat(crayon::yellow("     num features in this ld: ", length(findld_json$features), sep = ""), "\n")
        
        # dfs$patents_ld_sf[idx, "api_status"] <- err_msg
        # if ("on_fail" %in% save_api_urls) dfs$patents_ld_sf[idx, "api_url_middle"] <- api_url_middle
        # next
      }
      
      if (length(findld_json$features[[1]]) != 2) {        ## never seen this
        err_msg <- "features[[1]] doesnt have two elements"
        cat("    ", crayon::red(err_msg), "\n")
        dfs$patents_ld_sf[idx, "api_status"] <- err_msg
        if ("on_fail" %in% save_api_urls) dfs$patents_ld_sf[idx, "api_url_middle"] <- api_url_middle
        next
      }
      
      if (findld_json$features[[1]]$geometry$spatialReference$wkid != 4326) {
        err_msg <- "EPSG <> 4326"
        cat("    ", crayon::red(err_msg), "\n")
        dfs$patents_ld_sf[idx, "api_status"] <- err_msg
        if ("on_fail" %in% save_api_urls) dfs$patents_ld_sf[idx, "api_url_middle"] <- api_url_middle
        next
      }
  
      ## Look for a multi-ring polygon (could be a lake?)
      num_rings_per_feat <- sapply(findld_json$features, function(x) length(x$geometry$rings))
      if (FALSE %in% (num_rings_per_feat == 1)) {
        ## No longer an error
        cat("    ", crayon::yellow("Looks like a polygon with more than one ring"), "\n")
        
        # err_msg <- "Rings list should be length 1 (simple polygon, no holes)"
        # cat("    ", crayon::red(err_msg), "\n")
        # dfs$patents_ld_sf[idx, "api_status"] <- err_msg
        # if ("on_fail" %in% save_api_urls) dfs$patents_ld_sf[idx, "api_url_middle"] <- api_url_middle
        # next
      }
  
      ## Extract coordinates from the JSON object
      ## Get the coordinates for the rings, and put them in a list of lists of matrices
      
      ## This works well when there are multiple features
      # ring_coords_mat_lst <- lapply(findld_json$features, 
      #                         function(x) list(matrix(data=unlist(x$geometry$rings), ncol=2, byrow = TRUE)))
      
      ## This (should) work when there are multiple features potentially with multiple rings
      ring_coords_mat_lst <- lapply(findld_json$features, function(x) lapply(x$geometry$rings, function(y) matrix(data=unlist(y), ncol=2, byrow = TRUE) ))
      
      #matrix(data= unlist(findld_json$features[[1]]$geometry$rings[[1]]), ncol=2, byrow = TRUE)
      
      ## Construct the MULTIPOLYGON object
      ld_multipoly <- st_multipolygon(ring_coords_mat_lst, dim = "XY")

      ## Update geometry
      st_geometry(dfs$patents_ld_sf)[[idx]] <- ld_multipoly
      
      ## Update computed land description
      dfs$patents_ld_sf[idx, "api_landdescription"] <- findld_json$features[[1]]$attributes$landdescription
      
      ## Update success
      dfs$patents_ld_sf[idx, "api_status"] <- "success"
      
      ## Update api_url_middle
      if ("on_success" %in% save_api_urls) {
        dfs$patents_ld_sf[idx, "api_url_middle"] <- api_url_middle
      } else {
        if (!is.na(dfs$patents_ld_sf[idx, "api_url_middle", drop=TRUE])) {
          dfs$patents_ld_sf[idx, "api_url_middle"] <- NA
        }
      }
  
      num_geoms_retrieved <- num_geoms_retrieved + 1
    }
  
  } else if (geom_source == "archived_obj") {
    
    cat(crayon::yellow("   getting geoms from archived object"), "\n")
    
    ## Doing this through a loop - may not be the most efficient way but it only needs to be done once
    
    # create progress bar
    pb <- txtProgressBar(min = 0, max = num_ld, style = 3)

    for (i in 1:num_ld) {
      setTxtProgressBar(pb, i)
      
      idx <- pat_idx[i]
      if (is.na(idx)) next
      
      # browser()
      
      ## Get the land description for this row
      lld_str <- dfs$patents_ld_sf %>% st_drop_geometry() %>% 
        slice(idx) %>% pull(lld)
      
      ## See if this land description exists in the archived object
      arch_idx <- which(get(paste0(state_abbrev, "_bak"))$patents_ld_sf$lld == lld_str)
      
      if (length(arch_idx) > 0) {
        
        ## Copy over the geometry
        dfs$patents_ld_sf$geometry[[idx]] <- 
          get(paste0(state_abbrev, "_bak"))$patents_ld_sf$geometry[[arch_idx[1]]]
        
        ## Copy over other columns
        dfs$patents_ld_sf[idx, "api_landdescription"] <- 
          get(paste0(state_abbrev, "_bak"))$patents_ld_sf[arch_idx[1], "api_landdescription", drop = TRUE]

        dfs$patents_ld_sf[idx, "api_status"] <- 
          get(paste0(state_abbrev, "_bak"))$patents_ld_sf[arch_idx[1], "api_status", drop = TRUE]
        
        dfs$patents_ld_sf[idx, "api_url_middle"] <- 
          get(paste0(state_abbrev, "_bak"))$patents_ld_sf[arch_idx[1], "api_url_middle", drop = TRUE]

        ## Increment counter
        num_geoms_retrieved <- num_geoms_retrieved + 1

      }
      
    }
    close(pb)
      
  }
  
  ## Reset the bounding box to reflect the new geometries
  dfs$patents_ld_sf <- dfs$patents_ld_sf %>% slice(1:nrow(dfs$patents_ld_sf))
  
  cat("   Num geoms retrieved:", num_geoms_retrieved, "\n")
  cat("   If new geoms were retrieved, make sure GIS files are updated. \n")
  cat("   DONE\n")
  
  dfs
}



