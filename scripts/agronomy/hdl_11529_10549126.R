# R script for "carob"
# license: GPL (>=3)

## ISSUES



carob_script <- function(path) {

"
Replication Data for: Ms44-SPT: Unique genetic technology simplifies and improves hybrid maize seed production in sub-Saharan Africa

Hybrid maize seed production in Africa can face reduced yields based on the damage that may be incurred when detasseling female plants. Moreover, incomplete detasseling may lead to impurities hybrid seed production. A unique nuclear genetic male sterility seed production technology, referred to as Ms44-SPT, was developed to reduce these hybrid seed production challenges. Data collected to assess the impact of the Ms44-SPT technology on leaf loss during detasseling and on seed production are reported in this dataset.
"


	uri <- "hdl:11529/10549126"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT; ARC; KALRO",
		publication = "doi:10.1038/s41598-024-83931-1",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "seed_treatment",
		response_vars = "yield", 
		notes = "The leaf count files have not been processed because they do not contain useful information for carob",
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-06-19",
		carob_completion = 80,	
		carob_effort = 2
	)
	

	f1 <- ff[basename(ff) == "Supplementary file_COGS.xlsx"]
	f2 <- ff[basename(ff) == "COGS_analysis_script.R"]

	r1 <- unique(carobiner::read.excel(f1, sheet="yield_data"))
	#r2 <- carobiner::read.excel(f1, sheet="seed_grade_data") 
	#r3 <- carobiner::read.excel(f1, sheet="Leaf_counts")
	
	d <- data.frame(
		planting_date = as.character(r1$year),
		location = trimws(r1$location),
		country = r1$country,
		variety_pedigree = r1$pedigree,
		rep = as.integer(r1$rep),
		trial_id = r1$s,
		seed_treatment = r1$sterility,
		yield = r1$yield*1000,
		on_farm = TRUE, 
		is_survey = FALSE, 
		crop = "maize", 
		yield_part = "grain", 
		yield_moisture = NA_real_, 
		irrigated = TRUE, # from publication
		yield_isfresh = NA,
		plant_spacing = 75,
		row_spacing = 25
	)


	d$seed_treatment <- ifelse(grepl("^PP$", d$seed_treatment), paste(d$seed_treatment,"(", "pollen producing", ")", sep = " "), paste(d$seed_treatment, "(", "non pollen producing", ")", sep = " "))
	i <- grepl("Kitalale", d$location)
	d$location[i] <- "Kitalale"
 ### Adding lon and lat coordinate
	
	geo <- data.frame(
	  location = c("Cedara","Cedara 2" ,"Makhathini", "Kiboko", "Embu", "Perkerra", "Kakamega", "Kitale", "Kitalale", "Thika 2", "Kiminini", "Thika", "Kibos", "Alupe", "Kameeldrift", "Vaalharts", "Makh", "Endebess 2", "Endebess", "Nai", "TB7", "TB5"),
	  longitude = c(30.273, 30.273, 32.0841, 37.722, 37.459, 35.762, 34.7503, 35.0027, 34.9187, 37.0788, 34.925, 37.0824, 34.8159, 34.1291, 28.0063, 24.805, 26.532, 34.856, 34.856, 39.2958, 28.201, 28.201),
	  latitude = c(-29.533, -29.533, -27.560, -2.210, -0.538, 0.0818, 0.283, 1.0198, 1.0031, -1.035, 0.893, -1.037, -0.069, 0.490, -25.710, -27.945, -33.309, 1.074, 1.074, -3.796, -26.134, -26.134),
	  geo_from_source = FALSE
	)
	
	d <- merge(d, geo, by= "location", all.x = TRUE)
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	carobiner::write_files(path, meta, d)
}



