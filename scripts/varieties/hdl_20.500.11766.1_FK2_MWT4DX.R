# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Barley CWR stage 2 yield trial - Marocco 2021-2022

Assessment of Barley Crop Wild Relatives (CWR) - Stage 2: yield trials conducted at Marchouch, Annoucer and Sidi El Aidi stations during 2021-2022 cropping season. The dataset includes quality data.
"

	uri <- "hdl:20.500.11766.1/FK2/MWT4DX"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=6, minor=0,
		data_organization = "UM5;ICARDA",# UM5 : Mohammed V University
		publication = NA,
		project = NA,
		carob_date = "2026-04-07",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety_code",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "Barley_CWR_S2_2023.csv"]
	f2 <- ff[basename(ff) == "DataDictionary_ElementDescription.csv"]
	f3 <- ff[basename(ff) == "DataDictionary_Introduction.csv"]
	f4 <- ff[basename(ff) == "DataDictionary_UniqueIdentifier.csv"]

	r1 <- read.csv(f1, sep = ";")
	r2 <- read.csv(f2, sep = ";")
	r3 <- read.csv(f3, sep=";")
	r4 <- read.csv(f4, sep=";")
	
	d <- data.frame(
	   location = r1$Station,
	   variety_code = as.character(r1$GID),
	   rep = r1$Rep,
	   heading_days = r1$DTH,
	   maturity_days = r1$DTM,
	   plant_height = r1$PLH,
	   dmy_total = r1$BiomassYield*1000,
	   yield = r1$GrainYield*1000,
	   harvest_index = r1$HI,
	   grain_protein = r1$Grain_Protein_Content,
	   yield_moisture = r1$Grain_Moisture,
	   seed_weight = r1$TKW,
	   grain_Zn = r1$Zn_XRF/1000, ## mg/g
	   grain_Fe = r1$Fe_XRF/1000,
	   crop = "barley",
	   yield_isfresh = FALSE,
	   country = "Morocco",
	   planting_date = "2021",
	   trial_id = "1", 
	   on_farm = TRUE, 
	   is_survey = FALSE, 
	   yield_part = "grain", 
	   irrigated = NA
	)
	
	#### Fixing Lon and Lat coordinate
	
	geo <- data.frame(
	   location = c("Annoceur", "Marchouch", "Sidi El Aidi"),
	   longitude = c(-4.855, -6.6915, -7.6262),
	   latitude = c(33.665, 33.5616, 33.1246),
	   geo_from_source = FALSE
	)
	
	d <- merge(d, geo, by="location", all = TRUE)
	   
	d$N_fertilizer <- d$P_fertilizer <-  d$K_fertilizer <- as.numeric(NA)

	carobiner::write_files(path, meta, d)
}
