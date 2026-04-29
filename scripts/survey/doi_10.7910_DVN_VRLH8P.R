# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Gender-disaggregated constraints to the adoption and yield impact of RiceAdvice Lite in Nigeria

This dataset aims to assess farmers' access to digital tool, and adoption constraints/opportunities for RiceAdvice Lite — a digital advisory tool for rice production. The survey enables evaluation of farmer decision-making, gender dynamics, digital inclusion, and performance outcomes with and without the tool.
"
	uri <- "doi:10.7910/DVN/VRLH8P"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "AfricaRice",
		publication =NA,
		project = NA,
		carob_date = "2026-03-04",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "data.xls"]
	f2 <- ff[basename(ff) == "dictonary.xls"]

	r1 <- carobiner::read.excel(f1, sheet="Data")
	#r1b <- carobiner::read.excel(f1, sheet="Sheet1")
	r2 <- carobiner::read.excel(f2, sheet="Data description")
	#r2b <- carobiner::read.excel(f2, sheet="Sheet1")

	## process files 

	d <- data.frame(
		country = carobiner::fix_name(r1$country, "title"),
		latitude = r1$latitude,
		longitude = r1$longitude,
		elevation = r1$altitude,
		location = trimws(r1$village),
		adm1 = carobiner::fix_name(r1$state, "title"),
		season = ifelse(grepl("wet", r1$rice_season), "wet", 
		          ifelse(grepl("dry", r1$rice_season), "dry", NA)) ,
		hhid = r1$rice_info_sources_farmer_group,
		yield = r1$ral_yield_kgha,
		#yield_no = r1$No_ral_yield_kgha,
		trial_id = r1$barcode_household,
		farmer_age = as.numeric(r1$age_HH_head),
		farmer_gender = r1$gender_HH_head,
		farmland_owned = ifelse(grepl("owned", r1$land_tenure) ,r1$rice_land_size, NA),
		farmer_education = r1$HH_education,
		irrigated = grepl("irrigated", r1$production_env),
		planting_method = r1$planting_method,
		farmland = r1$rice_land_size,
		fertilizer_used = grepl("yes", r1$agronomic_practices_apply_fertilizer),
		fertilizer_type = gsub(";none|none;", "", paste(ifelse(grepl("yes", r1$fertilizer_type_urea), "urea", "none"), ifelse(grepl("yes", r1$fertilizer_type_NPK_15_15_15), "NPK", "none"), ifelse(grepl("yes", r1$fertilizer_type_NPK_20_20_20), "NPK", "none"), ifelse(grepl("yes", r1$fertilizer_type_DAP), "DAP", "none"), sep = ";")) ,
		N_splits = as.integer(r1$fert_splits),
		seed_rate = r1$seed_current_kgha,
		fertilizer_amount1 = r1$`current_fert_amount_npk_15-15-15`,
		fertilizer_amount2 = r1$current_fert_amount_urea,
		K_fertilizer = r1$current_K2O_applied_kgha/1.2051,
		N_fertilizer = r1$`current_N_applied kgha`,
		P_fertilizer = r1$current_P2O5_applied_kgha/2.29,
		mobile_data_access = grepl("yes", r1$digital_tools_access_mobile_data),
		digital_tools_used = grepl("yes", r1$digital_tools_used),
		crop = "rice",
		planting_date = as.character(NA), 
		on_farm = FALSE, 
		is_survey = TRUE, 
		yield_part= "none", 
		yield_moisture = as.numeric(NA), 
		geo_from_source = TRUE,
		yield_isfresh = TRUE
		
	)

	d$fertilizer_amount <- rowSums(d[, c("fertilizer_amount1", "fertilizer_amount2")], na.rm = TRUE)
	d$fertilizer_amount1  <-  d$fertilizer_amount2  <-  NULL
	
	
carobiner::write_files(path, meta, d)
}


