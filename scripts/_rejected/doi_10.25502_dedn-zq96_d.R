# R script for "carob"
# license: GPL (>=3)

## ISSUES

### This data is included in doi:10.25502/zbqv-k659/d

carob_script <- function(path) {

"
Cassava best planting practices set 5 soil data

The African Cassava Agronomy Initiative (ACAI) aims at improving cassava root yield and quality, and cassava supply to the processing sector. The project has 6 use cases of which best planting practices (BPP) is one. BPP is focusing on guiding farmers in choosing best-suited planting practices for cassava, with a focus on tillage operations and in close relation with improved weed control recommendations.
"

	uri <- "doi:10.25502/dedn-zq96/d"
	group <- "soil_samples"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "ACAI",
		carob_date = "2025-12-03",
		design = NA,
		data_type = NA,
		treatment_vars = NA,
		response_vars = "none", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f <- ff[basename(ff) == "bpp5_soildata_forckan.csv"]
	#f1 <- ff[basename(ff) == "column_dictionary_acai_bpp5_soildata.csv"]

	r <- read.csv(f)
#	r1 <- read.csv(f1)

### process

	d <- data.frame(
		record_id = r$ID,
		country = r$Country,
		location = r$Site,
		#rep = r$REP,
		#crop = "cassava",
		#land_prep_method = ifelse(grepl("DP", r$TRT), "deep ploughing",
		#                    ifelse(grep("SP", r$TRT), "reduced tillage", "none")) ,
		depth_top = as.numeric(gsub("-| ", "", substr(r$DEPTH_CM, 1, 2))),
		depth_bottom = as.numeric(gsub("-", "", substr(r$DEPTH_CM, 2, 5))),
		soil_pH = r$pH_H20_1_2_5,
		soil_SOC = r$OC_perc,
		soil_N = r$N_perc*10000,
		soil_P = r$MehP_ppm,
		soil_P_method = "Mehlich3",
		soil_K_exch = r$K_coml_kg,
		soil_Ca_exch = r$Ca_coml_kg,
		soil_Mg_exch = r$Mg_coml_kg,
		soil_sand = r$Sand_perc,
		soil_silt = r$Silt_perc,
		soil_clay = r$Caly_perc
	)

## RH: I am guessing there are 21 sites. Soil data needs high precision crds.	
	##### fixing geo-coordinate 
#	geo <- data.frame(
#	   location = c("Abeokuta","Saki"),
#	   longitude = c(3.362, 3.392),
#	   latitude = c(7.147, 8.673),
#	   geo_from_source = FALSE
#	)
	
#	d <- merge(d, geo, by= "location", all.x = TRUE)
	
	
	carobiner::write_files(path, meta, d)

}
