# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. The data dictionary pairs Category 6 "YAT = National Youth Service" with Category 7 "NYS = Yatta Station" for trait "Countries".
#    it reads as swapped (YAT looks like the Yatta abbreviation while NYS like the National Youth Service one). 
#    Only "YAT" occurs in 4035_data.xlsx; "NYS" is never used. YAT is treated below as the NYS Yatta School of Agriculture, Kithimani
# 2. Geocodes for Site "SUJ" ("Sujan") could not be identified

carob_script <- function(path) {

"
Dataset for: At least 7 promising clones from LTVR x LBHT and SAP 2013 population selected and bulked up for 2017 multiplication trials in Kenya

Performances of selected CIP clones over two years  at several sites with different water management supply regimes
"
	uri <- "doi:10.21223/GJNXGM"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "irrigated",
		response_vars = "yield",
		notes = NA,
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2026-08-28",
		carob_completion = 100,	
		carob_effort = 3
	)
	

	f1 <- ff[basename(ff) == "4035_data.xlsx"]
	f2 <- ff[basename(ff) == "4035_material_list.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	
	d1 <- r1
	colnames(d1)[colnames(d1) == "Accession number"] <- "variety_code"
	
	id_vars <- c("Order", "variety_code", "Total_Yield", "yield_above_mean_of_Checks")
	yield_cols <- setdiff(colnames(d1), id_vars)
	season_lookup <- c(
	  "Short_rains_13/14" = "Short Rains 13/14",
	  "Long_Rains_14"     = "Long Rains 14",
	  "Short_rains_14/15" = "Short Rains 14/15",
	  "Long_rains_2015"   = "Long Rains 2015"
	)
	
	site_lookup <- c(
	  SUJ  = "Sujan",
	  MA   = "Machakos",
	  UON  = "University of Nairobi",
	  KU   = "Kutus",
	  Hbay = "Homa Bay",
	  YAT  = "National Youth Service",
	  NYS  = "Yatta Station",
	  TM   = "TM"
	)
	
	parse_yield_colname <- function(x) {
	  y <- sub("_Yield$", "", x, ignore.case = TRUE)
	  y <- gsub("[ _]+", "_", trimws(y))
	  
	  season <- NA_character_
	  for (pat in names(season_lookup)) {
	    pat_norm <- gsub("[ _]+", "_", pat)
	    if (startsWith(y, pat_norm)) {
	      season <- season_lookup[[pat]]
	      y <- sub(paste0("^", gsub("([/])", "\\\\\\1", pat_norm)), "", y)
	      break
	    }
	  }
	  y <- gsub("^_+|_+$", "", y)
	  y <- gsub("-", "_", y)
	  y <- gsub("_+", "_", y)
	  
	  irrigated <- NA
	  if (grepl("IRRIG|IRRG", y, ignore.case = TRUE)) {
	    irrigated <- TRUE
	  } else if (grepl("(^|_)RF(_|$)", y, ignore.case = TRUE)) {
	    irrigated <- FALSE
	  }
	  
	  site_code <- gsub("(?i)(_?IRRIG_?|_?IRRG_?|(^|_)RF(_|$))", "_", y, perl = TRUE)
	  site_code <- gsub("^_+|_+$", "", site_code)
	  
	  list(season = season, site_code = site_code, irrigated = irrigated)
	}
	
	parsed <- lapply(yield_cols, parse_yield_colname)
	
	long_list <- vector("list", length(yield_cols))
	for (i in seq_along(yield_cols)) {
	  cn <- yield_cols[i]
	  p <- parsed[[i]]
	  sub <- data.frame(
	    variety_code = d1$variety_code,
	    season = p$season,
	    site_code = p$site_code,
	    irrigated = p$irrigated,   # NA where the column name states neither RF nor IRRIG/IRRG
	    yield = d1[[cn]] * 1000,
	    stringsAsFactors = FALSE
	  )
	  long_list[[i]] <- sub
	 
	  sub$irrigated[is.na(sub$irrigated)] <- FALSE
	  long_list[[i]] <- sub
	}
	d1 <- do.call(rbind, long_list)
	d1 <- d1[!is.na(d1$yield), ]     # drop season*site*irrigated combos that were not tested for that clone
	rownames(d1) <- NULL
	
	d2 <- r2
	colnames(d2)[colnames(d2) == "Accession number"] <- "variety_code"
	colnames(d2)[colnames(d2) == "Accession name"]   <- "variety"
	
	pf <- d2[["Parent Female"]]
	pm <- d2[["Parent Male"]]
	d2$variety_pedigree <- ifelse(
	  is.na(pf) & is.na(pm),
	  NA_character_,
	  paste(ifelse(is.na(pf), "?", pf), ifelse(is.na(pm), "?", pm), sep = " * ")
	)
	
	d <- merge(d1, d2, by = "variety_code", all.x = TRUE, sort = FALSE)
	
	# geo_from_source = FALSE hence geocodes where accessed from the following sources:
	#  MA  (Machakos)                         - https://www.geodatos.net/en/coordinates/kenya/machakos
	#  UON (University of Nairobi, Kabete)    - https://yandex.com/maps/org/university_of_nairobi_kabete_campus/92933407828/
	#                                            elevation: https://whatismyelevation.com (DEM at that lat/lon)
	#  KU  (Kutus)                            - https://www.findlatitudeandlongitude.com/l/County,+C74,+Kutus,+Kirinyaga+County,+Central+Kenya,+Kenya/6781536/
	#                                            elevation: https://whatismyelevation.com (DEM at that lat/lon)
	#  Hbay (Homa Bay)                        - https://www.countrycoordinate.com/city-homa-bay-kenya/
	#  YAT (NYS Yatta School of Agriculture,
	#       Kithimani, Machakos County)       - coordinates for Kithimani: https://www.getamap.net/maps/kenya/eastern/_kithimani/
	#                                            elevation: https://whatismyelevation.com (DEM at that lat/lon)
	#  SUJ (Sujan) and TM                     - NOT resolved
	
	site_locations <- data.frame(
	  site_code = c("SUJ", "MA", "UON", "KU", "Hbay", "YAT", "TM"),
	  location  = c("Sujan", "Machakos", "University of Nairobi (Kabete Campus)",
	                "Kutus", "Homa Bay", "NYS Yatta School of Agriculture (Kithimani)", "TM"),
	  longitude = c(NA, 37.26521, 36.733401, 37.236539, 34.453097, 37.45000, NA),
	  latitude  = c(NA, -1.52233, -1.256836, -0.474087, -0.535043, -1.18333, NA),
	  elevation = c(NA, 1619, 1881, 1700, 1193, 1325, NA),
	  stringsAsFactors = FALSE
	)
	
	d <- merge(d, site_locations, by = "site_code", all.x = TRUE, sort = FALSE)
	
  d$trial_id <- as.character(as.integer(as.factor(1)))
  d$country <- "Kenya"
  d$on_farm <- FALSE
  d$is_survey <-FALSE
	d$geo_from_source <- FALSE
  d$planting_date <- as.character(as.Date(NA))
	d$harvest_date  <- as.character(as.Date(NA))
  d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- d$S_fertilizer <- d$fertilizer_type <- NA
  d$crop <- tolower(d[["Crop Name CIP"]]) 
  d$yield_part <- "tubers"
	d$yield_moisture <- NA
	d$yield_isfresh <- NA
	
	d[c("Parent Male", "Parent Female", "Genus", "In Storage", "Population Group",
	    "Collecting number/Breeder code/Biosafety code","Crop Name CIP","site_code")] <- NULL
	d$yield[d$yield < 0] <- 0
	
	carobiner::write_files(path, meta, d)
}



