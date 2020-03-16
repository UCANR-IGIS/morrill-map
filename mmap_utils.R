## This contains functions used elsewhere
## OK to Source on Save


pat_url <- function(accession_nr, doc_class_code) {
  ## Generate a URL for patents
  paste0("https://glorecords.blm.gov/details/patent/default.aspx?accession=", 
         accession_nr,
         "&docClass=",
         doc_class_code)
}

bulk_data_glo <- function(state_abbrev, fnonly = FALSE) {
  if (fnonly) {
    url_base <- ""
  } else {
    url_base <- "https://glorecords.blm.gov/BulkData/Patent/"
  }
  paste0(url_base, state_abbrev, "_Patent.zip")
}

api_url <- function(api_url_middle) {
  ## Construct the URL for the API call
  api_url_base <- "https://gis.blm.gov/arcgis/rest/services/Cadastral/BLM_Natl_PLSS_CadNSDI/MapServer/exts/CadastralSpecialServices/FindLD?legaldescription="
  api_url_end <- "&returnalllevels=&f=json"
  paste0(api_url_base, api_url_middle, api_url_end)
}

mmap_blank_api_cols <- function(dfs, pat_idx) {
  dfs$patents_ld_sf[pat_idx, "api_status"] <- NA
  dfs$patents_ld_sf[pat_idx, "api_url_middle"] <- NA
  dfs$patents_ld_sf[pat_idx, "api_landdescription"] <- NA
  dfs
}

mmap_view_recs <- function(dfs, pat_idx) {
  ## Show an interactive table
  pat_curnt_recs <- datatable(dfs$patents_ld_sf %>% 
                                st_drop_geometry() %>% 
                                slice(pat_idx) %>% 
                                mutate(pat_rec = paste0("<a href='", 
                                                        pat_url(accession_nr, doc_class_code),
                                                        "' target='_blank'>", accession_nr, "</a>"),
                                       api_rec = paste0("<a href='", api_url(api_url_middle), 
                                                        "'>link</a>")) %>% 
                                select(pat_rec, descrip_nr, aliquot_parts, fractional_section, lld, 
                                       api_status, api_rec),
                              escape = FALSE
  )
  
  html_fn <- "patent_recs.html"
  htmlwidgets::saveWidget(pat_curnt_recs, file=html_fn)
  browseURL(html_fn)
}

recompute_ld <- function(dfs, pat_idx) {
  
  ## (Re)construct the sub-section part of the land description.
  ld_subsect <- ld_subsect_parse(dfs$patents_ld_sf$aliquot_parts)[pat_idx]
  
  if (is.character(dfs$patents_ld_sf$meridian_code)) {
    meridian_code_str <- dfs$patents_ld_sf[pat_idx, "meridian_code", drop = TRUE]
  } else {
    meridian_code_str <- sprintf("%02d", dfs$patents_ld_sf[pat_idx, "meridian_code", drop = TRUE])
  } 
    
  ## Add this to the table
  dfs$patents_ld_sf[pat_idx, "lld"] <- dfs$patents_ld_sf %>% 
    st_drop_geometry() %>% slice(pat_idx) %>% 
    mutate(lld = paste0(state_code, " ", meridian_code_str,
                        " T", township_nr, township_dir,
                        " R", range_nr, range_dir,
                        " SEC ", section_nr,
                        ld_subsect)) %>% 
    pull(lld)
  
  ## Reset the geometry to empty
  for (i in pat_idx) dfs$patents_ld_sf$geometry[[i]] <- st_multipolygon()
  
  ## Reset the bounding box to reflect the new geometries
  dfs$patents_ld_sf <- dfs$patents_ld_sf %>% slice(1:nrow(dfs$patents_ld_sf))
  
  ## Reset the API fields 
  dfs$patents_ld_sf[pat_idx, "api_status"] <- NA
  dfs$patents_ld_sf[pat_idx, "api_url_middle"] <- NA
  dfs$patents_ld_sf[pat_idx, "api_landdescription"] <- NA
  
  
  dfs
}


ld_subsect_parse <- function(aliquot_parts) {
  
  cat(crayon::green("   parsing the aliquot_parts column..."))
  
  ## Create an empty vector for the sub-section component of the legal land description
  #ld_subsect <- rep(NA, nrow(patents_ld_tbl))
  ld_subsect <- rep(NA, length(aliquot_parts))
  
  ## Case 0. No aliquot parts at all (the LD is for an entire section)
  idx_na <- is.na(aliquot_parts)
  ld_subsect[idx_na] <- ""
  idx_dealtwith <- which(idx_na)

    ## Case 1. ALL numeric characters - treat as a lot number
  # idx <- !grepl("\\D", patents_ld_tbl$aliquot_parts)
  # ld_subsect[idx] <- paste0(" LOT ", patents_ld_tbl[idx, "aliquot_parts", drop=TRUE])
  idx <- !grepl("\\D", aliquot_parts) & !idx_na
  ld_subsect[idx] <- paste0(" LOT ", aliquot_parts[idx])
  idx_dealtwith <- c(idx_dealtwith, which(idx))
  
  ## Case 2. Numeric digit(s) followed by NESW½
  ## 1SW should become ALIQ SW LOT 1 
  ## 5NW should become ALIQ NW LOT 5
  ## RegEx Search Expression:
  ##    ^[0-9]+     means starting with one or more numeric characters
  ##    [NESW½]+$   means ending with one or more of the letters NESW½
  idx <- grepl("^[0-9]+[NESW]+$", aliquot_parts)
  #(leading_nums <- str_extract(x[idx], "^[0-9]+"))
  #(trailing_chars <- str_extract(x[idx], "[NESW½]+$"))
  ld_subsect[idx] <- paste0(" ALIQ ", str_extract(aliquot_parts[idx], "[NESW½]+$"),
                            " LOT ", str_extract(aliquot_parts[idx], "^[0-9]+"))
  idx_dealtwith <- c(idx_dealtwith, which(idx))
  
  ## Case 3. Last digits before you hit alphabetic character are numeric (W½10)
  ## E½25
  ## DONT KNOW HOW TO HANLDE THESE

  ## Case 3. A numeric number in the middle (E½2NW)
  ## ??
  
  ## Case All Else. Treat as a series of quadrants
  # idx_else <- (1:nrow(patents_ld_tbl))[-idx_dealtwith]
  # ld_subsect[idx_else] <- paste0(" ALIQ ", patents_ld_tbl[idx_else, "aliquot_parts", drop=TRUE])
  idx_else <- (1:length(aliquot_parts))[-idx_dealtwith]
  ld_subsect[idx_else] <- paste0(" ALIQ ", aliquot_parts[idx_else])

  cat(crayon::green("   Done.\n"))
  ld_subsect
}

mmap_show_opts <- function() {
  ## Utility function to show the values for all the processing settings
  
  vars_to_show <- c("dir_glo", "dir_rdata", "dir_shp", "dir_geojson", "dir_gbd", "comb_geopackage_fn", "comb_geojson_fn", "states_to_process", "skip_nodata", "skip_completed", "load_rdata", "import_csv_if_needed", "save_stats", "save_badld", "add_sig_year", "geom_convert_single2multi", "compute_ld", "add_patentees", "make_archive_copy", "save_archive_copy", "get_geoms", "use_archived_objs", "pat_idx_option", "pat_idx", "save_stats", "save_badld", "add_sig_year", "save_rdata", "save_geopackage", "save_shp", "save_geojson_ind", "save_geojson_comb")
  
  for (var in vars_to_show) {
    cat(var, ": ", ifelse(is.logical(get(var)), ifelse(get(var), green(get(var)), red(get(var))) , paste(get(var), collapse = ",")), 
        "\n", sep = "")
  }
  
}

mmap_del_vars <- function(states = state.abb, baks = TRUE) {
  
  ans <- readline(prompt = "This will delete objects in memory? Continue? y/n ")
  if (ans != "y") return(invisible(NULL))

  if (!identical(states, FALSE)) {
    objs_del <- states[states %in% ls(envir = .GlobalEnv)]
    if (length(objs_del) > 0) {
      rm(list = objs_del, envir = .GlobalEnv)
      cat("States deleted: ", paste(objs_del, collapse = ", "), "\n")
    } else {
      cat("No States found in memory \n")
    }
    
  }
  
  ## This willl delete all objects in memory that end with '_bak'. Use with care!!
  if (baks) {
    objs_del <- grep("_bak$", ls(envir = .GlobalEnv), value = TRUE)
    if (length(objs_del) > 0) {
      rm(list = objs_del, envir = .GlobalEnv)
      cat("Objects deleted: ", paste(objs_del, collapse = ", "), "\n")
    } else {
      cat("No backup objects found in memory \n")
    }
  }
  
  
}



## DEPRECATED - SEE compute_ld above
# mmap_recompute_ld <- function(dfs, pat_idx) {
#   
#   ## Construct the sub-section part of the land description.
#   ld_subsect <- ld_subsect_parse(dfs$patents_ld_sf$aliquot_parts)[pat_idx]
#   
#   dfs$patents_ld_sf[pat_idx, "lld"] <- patents_ld_sf %>% 
#     st_drop_geometry() %>% 
#     slice(pat_idx) %>% 
#     mutate(lld = paste0(state_code, " ", sprintf("%02d", meridian_code),
#                         " T", township_nr, township_dir,
#                         " R", range_nr, range_dir,
#                         " SEC ", section_nr,
#                         ld_subsect)) %>% 
#     pull(lld)
#   
#   dfs$patents_ld_sf[pat_idx, "api_status"] <- NA
#   dfs$patents_ld_sf[pat_idx, "api_url_middle"] <- NA
#   dfs$patents_ld_sf[pat_idx, "api_landdescription"] <- NA
#   
#   dfs
# }

