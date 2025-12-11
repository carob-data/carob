# R script for "carob"
# license: GPL (>=3)

## ISSUES
## The data mention 4 site but just one is given 

carob_script <- function(path) {

"
Cassava best planting practices set 4

The African Cassava Agronomy Initiative (ACAI) aims at improving cassava root yield and quality, and cassava supply to the processing sector. The project has 6 use cases of which best planting practices (BPP) is one. BPP is focusing on guiding farmers in choosing best-suited planting practices for cassava, with a focus on tillage operations and in close relation with improved weed control recommendations.
"

	uri <- "doi:10.25502/vgf3-aj58/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "ACAI",
		carob_date = "2025-11-10",
		design = NA,
		data_type = "experiment",
		treatment_vars = "land_prep_method;weeding_method",
	   response_vars = "yield;yield_marketable", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	
	f <- ff[basename(ff) == "bbp4_harvestdata.csv"]
	#f2 <- ff[basename(ff) == "treatment.csv"]
	
	r <- read.csv(f)
	#r1 <- read.csv(f1)
	
### process	

	d <- data.frame(
	   record_id = r$ID,
		country = r$Country,
		location = r$Site,
		plot_id = as.character(r$plot_no),
		rep = r$Rep,
		treatment = r$plough,
		land_prep_method = paste(ifelse(grepl("HARROW", r$Harrow), "harrowing", "none"), ifelse(grepl("RIDGE", r$RIDGE), "ridge tillage", "none"), sep = ";"),
		weeding_method = ifelse(grepl("manual", r$Weed_control), "hoeing", "herbicide") ,
		plant_density = r$plants_m2*10000,
		fwy_stems = r$main_stem_kg_m2*10000,
		dmy_stems = r$DM_main_stem_Mg_ha*1000,
		fwy_leaves = r$leaves_branches_kg_m2*10000,
		dmy_leaves = r$DM_leaves_branches_Mg_ha*1000,
		fwy_roots = rowSums(r[, c("mass_ok_roots_kg_m2", "mass_bad_roots_kg_m2")])*10000,
		yield_marketable = r$DM_ok_roots_Mg_ha*1000,
		yield = rowSums(r[, c("DM_bad_roots_Mg_ha", "DM_ok_roots_Mg_ha")])*1000 ,
		dmy_total = r$DM_above*1000,
		harvest_index = r$HI_total,
		latitude = 7.378,
		longitude = 3.955,
		trial_id = paste(r$Site, r$ID, sep = "-"), 
		planting_date = "2016", 
		on_farm = TRUE, 
		is_survey = FALSE, 
		crop = "cassava", 
		yield_part = "roots", 
		yield_moisture = 0,
		irrigated = NA, 
		geo_from_source = FALSE
	)
	
	d$treatment <- ifelse(grepl("DP", d$treatment), "double plough", 
	                      ifelse(grepl("SP", d$treatment), "Single plough", "Zero plough"))
	d$land_prep_method <- gsub("none;none", "none", ifelse(grepl("Zero plough", d$treatment), d$land_prep_method, paste("ploughing", d$land_prep_method, sep = ";"))) 
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	
	## remove 4 rows with yield == 0
	d <- d[d$yield>0,]
	
	### process soil data from doi:10.25502/beqq-et80/d
	
	ff1  <- carobiner::get_data("doi:10.25502/beqq-et80/d", path, group)
	f <- ff1[basename(ff1) == "bpp4_soildata_forckan.csv"]
	
	r1 <- read.csv(f)
	
	### process
	d1 <- data.frame(
	   country = r1$Country,
	   location = r1$Site,
	   rep = r1$REP,
	   depth_top = as.numeric(gsub("-| ", "", substr(r1$DEPTH_CM, 1, 2))),
	   depth_bottom = as.numeric(gsub("-", "", substr(r1$DEPTH_CM, 2, 5))),
	   soil_pH = r1$pH_H20_1_2_5,
	   soil_SOC = r1$OC_perc,
	   soil_N = r1$N_perc*10000,
	   soil_P = r1$MehP_ppm,
	   soil_P_method = "Mehlich3",
	   soil_K_exch = r1$K_coml_kg,
	   soil_Ca_exch = r1$Ca_coml_kg,
	   soil_Mg_exch = r1$Mg_coml_kg,
	   soil_sand = r1$Sand_perc,
	   soil_silt = r1$Silt_perc,
	   soil_clay = r1$Caly_perc
	)
	
	agg <- aggregate(. ~ rep + location + country+ soil_P_method , d1, function(x) mean(x))
	
	### merge soil data and yield data 
  d <- merge(d, agg, by= c("rep", "location", "country"), all.x = TRUE)
	

carobiner::write_files(path, meta, d)

}


