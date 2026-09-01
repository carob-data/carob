# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
ESWYT 1-41 curated  yield data with gap filled phenology

Forty-one years of data from the Elite Spring Wheat Yield Trials (ESWYT), including average yield and phenological information at the site-year-OCC (occurrence) level.
"

	uri <- "doi:10.71682/10549387"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		carob_date = "2026-05-15",
		design = NA,
		data_type = NA,
		treatment_vars = "location;country",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		carob_completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "ESWYT 1-41 Yield_Covariates_20251218_blurred.xlsx"]
  
	r1 <- carobiner::read.excel(f1, sheet="Data")
	#r2 <- carobiner::read.excel(f1, sheet="ReadMe")
	r3 <- carobiner::read.excel(f1, sheet="Variable names")



##### process
	
	d <- data.frame(
		country = carobiner::fix_name(r1$Country, "title"),
		latitude = r1$Lat,
		longitude = r1$Long,
		planting_date = as.character(r1$sowing_Date),
		maturity_date = as.character(r1$maturity_Date),
		location = carobiner::fix_name(r1$Loc_desc, "title"),
		trial_id = r1$`Trial name`,
		harvest_date = as.character(r1$HarvestYr),
		yield = r1$`BLUE_YLD_t/ha`*1000,
		heading_date = as.character(r1$heading_Date),
		tmax = r1$avg_TMax_season_ºC,
		tmin = r1$avg_TMin_season_ºC,
		vapr = r1$avg_VPD_season_kPa,
		crop = "wheat",
		#crop_occurrence = r1$Occ, ## not sure how to capture this (occasionally, the same nursery was planted more than once at a given location in a given year)
		is_survey = FALSE, 
		on_farm = FALSE, 
		yield_moisture = NA_real_, 
		yield_part = "none", 
		geo_from_source = TRUE, 
		irrigated = NA,
		yield_isfresh = NA
	)
	
	
	i <- grepl("Swaziland", d$country)
	d$country[i] <- "Eswatini"
	
	i <- grepl("Tanzania", d$country) & grepl("Lyamungo", d$location)
	d$longitude[i] <-  37.256
	d$latitude[i] <- -3.2346
	d$geo_from_source[i] <- FALSE
	
	i <- grepl("Zambia", d$country) & grepl("Zamseed", d$location)
	d$longitude[i] <-  28.2929
	d$latitude[i] <- -15.286
	d$geo_from_source[i] <- FALSE
	
	
	d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- as.numeric(NA)
	
	
	carobiner::write_files(path, meta, d)
}

