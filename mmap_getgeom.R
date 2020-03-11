## Gets geoms from the GLO API
## Contains a single function - ok to Source on Save

mmap_fill_geom <- function(dfs, pat_idx, 
                          pause_after_n = list("1" = 0, "3900" = 60 * 5),
                          save_api_urls = c("on_fail", "on_success")[1]) {

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
  
  cat(crayon::yellow("   Getting geoms through BLM FindLD service"), "\n")
  
  num_ld <- length(pat_idx)
  num_geoms_retrieved <- 0
  n_to_pause <- as.numeric(names(pause_after_n)[unlist(pause_after_n) != 0])
  
  # Set up progress bar
  # pb <- txtProgressBar(min = 1, max = num_ld, style = 3)
  
  for (i in 1:num_ld) {
    idx <- pat_idx[i]
    
    if (is.na(idx)) next

    ## Auto-save if needed
    ## if (i %% 1000 == 0) save(patents_ld_sf, file="patents_ld_sf.RData")
    
    ## Pause after n
    ## We put the pause at the top of the loop due to the 
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
    
    # Update progress bar
    #setTxtProgressBar(pb, pat_idx)
    
    ## Get the land description for this row
    # lld_str <- "NV 21 T38N R56E SEC 10 ALIQ SESW"
    lld_str <- dfs$patents_ld_sf %>% 
      st_drop_geometry() %>% 
      slice(idx) %>% 
      select(lld) %>% 
      as.character()
    
    ## Turn this into a URL piece, making two substitutions
    ## (combining these into a single piped statement didn't work - not sure why)
    api_url_middle <- gsub(" ", "+", lld_str) 
    api_url_middle <- gsub("½", "2", api_url_middle, useBytes = FALSE) 
    
    # print(api_url_middle)
    # browser()
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
    #cat("  ", url_middle, "\n")
    #cat("  ", lld_str, "\n")
    
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

    ## Look for holes in the polygon(s)
    num_rings_per_feat <- sapply(findld_json$features, function(x) length(x$geometry$rings))
    if (FALSE %in% (num_rings_per_feat == 1)) {
      cat("    ", crayon::red("Looks like a polygon with a lake"), "\n")
      #browser()
      
      err_msg <- "Rings list should be length 1 (simple polygon, no holes)"
      cat("    ", crayon::red(err_msg), "\n")
      dfs$patents_ld_sf[idx, "api_status"] <- err_msg
      if ("on_fail" %in% save_api_urls) dfs$patents_ld_sf[idx, "api_url_middle"] <- api_url_middle
      next
    }

    ## Extract coordinates from the JSON object
    ## Get the coordinates for the rings, and put them in a list of lists of matrices
    ring_coords_mat_lst <- lapply(findld_json$features, 
                                  function(x) list(matrix(data=unlist(x$geometry$rings), ncol=2, byrow = TRUE)))
    
    ## Construct the MULTIPOLYGON object
    ld_multipoly <- st_multipolygon(ring_coords_mat_lst, dim = "XY")
    
    ## Get the rings from the geometry 
    #rings_lst <- findld_json$features[[1]]$geometry$rings
    
    ## Get the rings from the geometry 
    ##rings_lst <- findld_json$features[[1]]$geometry$rings
    
    # if (is.null(rings_lst)) {  ## NEVER SEEN THIS - TAKE IT OUT FOR NOW 
    #   err_msg <- "Rings not found (not a polygon?)"
    #   cat("  ", crayon::red(err_msg), "\n")
    #   dfs$patents_ld_sf[idx, "api_status"] <- err_msg
    #   if ("on_fail" %in% save_api_urls) dfs$patents_ld_sf[idx, "api_url_middle"] <- api_url_middle
    #   next
    # }

    ## Get the coordinates for this single ring
    #ring_coords_mat <- matrix(data=unlist(rings_lst), ncol=2, byrow = TRUE)
    
    ## Generate a st_polygon object 
    #ld_poly <- st_polygon(list(ring_coords_mat), dim = "XY")
    
    
    # ## SIMPLE POLYGON
    # ## Get the rings from the geometry 
    # rings_lst <- findld_json$features[[1]]$geometry$rings
    # 
    # ## Get the coordinates for this single ring
    # ring_coords_mat <- matrix(data=unlist(rings_lst), ncol=2, byrow = TRUE)
    #
    # ## Generate a st_polygon object 
    # ld_poly <- st_polygon(list(ring_coords_mat), dim = "XY")
    # 
    # ld_multipoly <- st_multipolygon(list(list(ring_coords_mat)), dim = "XY")   ##works
    
    ## Update geometry
    ##st_geometry(dfs$patents_ld_sf)[[idx]] <- ld_poly
    
    ## May need an error check here
    
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
  # close(pb)
  
  ## Reset the bounding box to reflect the new geometries
  dfs$patents_ld_sf <- dfs$patents_ld_sf %>% slice(1:nrow(dfs$patents_ld_sf))
  
  cat("DONE\n")
  cat("Num geoms retrieved:", num_geoms_retrieved, "\n")
  cat("If new geoms were retrieved, make sure GIS files are updated. \n")
  
  dfs
}



