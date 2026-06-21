# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. some missing S_fertilizer data from raw csv.



carob_script <- function(path) {

"
Dataset for meta-analysis on sulfur fertilization effect on wheat protein and yield

This dataset contains the spreadsheet, the study references, and an R code tutorial explaining how the study was analyzed. This dataset includes the compiled data from the 55 studies used in the meta-analysis, which were sourced from published literature. While the original data for each individual study falls under the purview of the respective authors, the curated dataset used for the meta-analysis in this study is made available to ensure transparency and reproducibility. The dataset contains information such as study location, soil characteristics, fertilization rates and source, wheat yield, and protein content responses, enabling other researchers to investigate further the effects of sulfur fertilization on wheat performance across diverse environments.
"
	uri <- "doi:10.7910/DVN/4RLPP1"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=0,
		data_organization = "KSU",
		publication = "doi_10.3390_nitrogen5030037",
		project = NA,
		carob_date = "2026-06-17",
		design = "Randomised Complete Block",
		data_type = "experiment",
		treatment_vars = "S_fertilizer",
		response_vars = "yield;grain_protein", 
		carob_contributor = "Blessing Dzuda",
		carob_completion = 100,	
		notes = NA
	)
	
	f <- ff[basename(ff) == "Sulfur_datasets_GRoa.xlsx"]
	r <- carobiner::read.excel(f)

	d <- data.frame(
		country = r$country,
		latitude = r$latitude,
		longitude = r$longitude,
		crop ="wheat",
		planting_date=r$site_year,
		previous_crop =tolower(r$previous_crop),
		S_fertilizer = r$sulfur_rate_kg_ha,
		N_fertilizer = r$nitrogen_rate_kg_ha,
		soil_texture = trimws(tolower(r$soil_texture)),
		soil_S = r$soil_sulfate_mg_kg,
		yield = r$treated_yield_kg_ha,
		treatment = r$trt_name,
		rep=as.integer(r$rep_or_block),
		#variety=r$varieties, no specific crop variety is mentioned
		fertilizer_type=paste(r$sulfur_fertilizer,r$nitrogen_fertilizer,sep = ";"),
		fertilization_method=paste(r$sulfur_aplication,r$nitrogen_aplication,sep = ";"),
		fertilizer_timing=paste(r$sulfur_timing,r$nitrogen_timing,sep=";"),
		land_prep_method=tolower(r$tillage),
		soil_pH=r$pH,
		soil_SOM=r$om_percent,
		grain_protein=r$treated_protein_percent)
	
	d$irrigated <- r$irrigation=="Yes"
	d$planting_date <- as.character(sub("-.*", "", d$planting_date))
	d$trial_id <- paste(d$planting,d$location,sep="_")
	
	d$fertilizer_type <- trimws(tolower(d$fertilizer_type))
	
	fert_lookup <- c(
	  # Sulfur fertilizers
	  "calcium sulfate" = "gypsum",
	  "gypsum" = "gypsum",
	  "phosphogypsum" = "gypsum",
	  "ammonium sulfate" = "AS",
	  "ammonium sulfate (amidas®)" = "AS",
	  "ammonium thiosulfate" = "ATS",
	  "elemental s" = "unknown",      # elemental sulfur not in CAROB
	  "elemental  s" = "unknown",
	  "magnesium sulfate" = "MgSO4",
	  "potassium sulfate" = "SOP",
	  "copper sulfate" = "CuSO4",
	  "water soluble sulfate" = "unknown",
	  "sodium sulfate" = "unknown",
	  # Nitrogen fertilizers
	  "urea" = "urea",
	  "ammonium nitrate" = "AN",
	  "ammonium  nitrate" = "AN",
	  "urea ammonium nitrate" = "unknown",      # UAN not in CAROB
	  "urea ammoniun nitrate" = "unknown",
	  "calcium ammoniumnitrate" = "CAN",
	  "dap" = "DAP",
	  # Missing values
	  "na" = "none",
	  "none" = "none",
	  # Commercial products requiring investigation
	  "cosavet" = "unknown",
	  "reap®90 wdg" = "unknown",
	  "sulfan" = "unknown",
	  "yv thiotrac" = "unknown",
	  "axam super" = "unknown",
	  "agrium plus" = "unknown",
	  "bio sulphur" = "unknown",
	  # Non-mineral amendments
	  "soil mix" = "unknown",
	  "blood meal" = "unknown",
	  # Mixed products
	  "ammonium nitrate & urea" = "unknown",
	  "urea & ammonium nitrate" = "unknown",
	  "sodium nitrate & supernitro" = "unknown",
	  "ammonium nitrate & urea" = "unknown",
	  "urea & dap" = "unknown",
	  "dap & urea" = "unknown",
	  "sulfan & yv thiotrac" = "unknown")
	
	d$fertilizer_type <- gsub(" +", " ", d$fertilizer_type)
	
	parts <- strsplit(d$fertilizer_type, ";")
	
	d$fertilizer_type <- sapply(parts, function(x) {
	  paste(
	    ifelse(x[1] %in% names(fert_lookup), fert_lookup[x[1]], "unknown"),
	    ifelse(x[2] %in% names(fert_lookup), fert_lookup[x[2]], "unknown"),
	    sep = ";")})
	
	d$fertilizer_timing <- trimws(tolower(d$fertilizer_timing))
	d$fertilization_method <- trimws(tolower(d$fertilization_method))
	d$land_prep_method <- ifelse(is.na(d$land_prep_method),NA,
	  ifelse(d$land_prep_method == "conventional", "conventional", "none"))
	d$on_farm <- FALSE #experiment done in greenhouse(from publication)
	d$is_survey <- FALSE 
	d$geo_from_source <- TRUE
	d$yield_isfresh <- TRUE
  d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$country <- gsub("United State","United States",d$country)
	d$country <- gsub("United Statess","United States",d$country)
	
	d$soil_texture <- gsub("sandy","sand", d$soil_texture)
	
  prev_crop <- c(
    "oats"="oat",
    "rapeseed"="oilseed rape",
    "flax"="linsseed",
    "rapeseed"="rape",
    "maize"="corn",
    "unknown"="break crops",
    "unknown"="multiple",
    "guar"="cluster bean",
    "berseem clover"="egyptian clover")
  
  d$previous_crop <- prev_crop[as.character(d$previous_crop)]
  
  texture <- c(
    "sandy loam"="sand  loam",
    "sandy clay loam"="sand clay loam",
    "sand loam"="sandy loam",
    "silty loam"="silt loam")
  
  d$soil_texture <- texture[as.character(d$soil_texture)]
  
  #cleaning fertilizer columns
  # extracting midpoint of fertilizers provided as ramge
  range_mid <- function(x) {
    if (is.na(x)) return(NA_real_)
    if (grepl("-", x)) {
      parts <- as.numeric(trimws(strsplit(x, "-")[[1]]))
      return(mean(parts))
    }
    return(NA_real_)
  }
  
  split_sum <- function(x) {
    if (is.na(x)) return(NA_real_)
    if (grepl(",", x)) return(sum(as.numeric(trimws(strsplit(x, ",")[[1]]))))
    return(NA_real_)
  }
  
  d$N_fertilizer[d$N_fertilizer %in% c("Multiple", "146158169", "170210")] <- NA # not sure of the values
  
  # midpoint
  range_vals <- sapply(d$N_fertilizer, range_mid)
  split_vals <- sapply(d$N_fertilizer, split_sum)
  
  d$N_fertilizer[grepl("-", d$N_fertilizer)] <- NA 
  d$N_fertilizer[grepl(",", d$N_fertilizer)] <- NA 
  
  d$N_fertilizer <- ifelse(
    grepl(",", d$N_fertilizer),
    sapply(d$N_fertilizer, split_sum),
    ifelse(grepl("-", d$N_fertilizer),
      sapply(d$N_fertilizer, range_mid),
      as.numeric(d$N_fertilizer)))
  
  d$N_fertilizer[d$N_fertilizer %in% c(146158169, 170210)] <- NA # values too large for fertilizer application  
  
  d$S_fertilizer[grepl("-", d$S_fertilizer)] <- NA
  d$S_fertilizer <- ifelse(grepl("-", d$S_fertilizer),
    sapply(d$S_fertilizer, range_mid),
    as.numeric(d$S_fertilizer)
  )
  
  d$S_fertilizer[d$S_fertilizer %in% c(476, 952, 1071)] <- NA#values too big for fertilizer application
  
	carobiner::write_files(path, meta, d)
}
