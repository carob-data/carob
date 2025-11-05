# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Cassava best planting practices total biomass

The African Cassava Agronomy Initiative (ACAI) aims at improving cassava root yield and quality, and cassava supply to the processing sector. The project has 6 use cases of which best planting practices (BPP) is one. BPP is focusing on guiding farmers in choosing best-suited planting practices for cassava, with a focus on tillage operations and in close relation with improved weed control recommendations.
"

	uri <- "doi:10.25502/qm2j-4q10/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "ACAI",
		carob_date = "2025-11-05",
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "plantparts_totalbiomass.csv"]
	f2 <- ff[basename(ff) == "treatment.csv"]
	#f3 <- ff[basename(ff) == "column_dictionary_plantparts_totalbiomass.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	#r3 <- read.csv(f3)

### process 

	d1 <- data.frame(
	   record_id = r1$ID,
		planting_date = as.character(r1$Year),
		country = r1$Country,
		location = r1$Location,
		treatment = r1$Treatment,
		N_fertilizer = r1$N_application_kg_ha,
		P_fertilizer = r1$P_application_kg_ha,
		K_fertilizer = r1$K_application_kg_ha,
		dmy_leaves = r1$DMLeaves_kg_ha,
		dmy_stems = r1$Dmstems_g_m2*10,
		dmy_roots = r1$DMRoots_g_m2*10, ## kg/ha
		yield = r1$DMRoots_g_m2*10,
		dmy_total = r1$Biomass_g_m2*10,
		crop = "cassava",
		yield_part = "roots",
		is_survey = FALSE,
		on_farm = TRUE,
		trial_id = paste(r1$Location, r1$ID, sep = "-"),
		yield_moisture = 0,
		irrigated = NA
	)

	###  treatment 
	
	d2 <- data.frame(
		treatment = r2$treatment_code,
		N_fertilizer = r2$N_kg_ha,
		P_fertilizer = r2$P_kg_ha,
		K_fertilizer = r2$K_kg_ha,
		S_fertilizer = r2$S_kg_ha,
		Mg_fertilizer = r2$Mg_kg_ha,
		Zn_fertilizer = r2$Zn_kg_ha,
		B_fertilizer = r2$B_kg_ha,
		Ca_fertilizer = r2$Ca_kg_ha
      )
	
	### merge d1 and d2 
	d <- merge(d1, d2, by = intersect(names(d1), names(d2)), all.x = TRUE)
	
	#### adding long and lat coordinate 
	
	 geo <- data.frame(
	    location = c("Benue", "CRS", "Edo" ),
	    longitude = c(8.695, 9.129, 5.928),
	    latitude = c(7.348, 6.449, 6.661),
	    geo_from_source = FALSE
	 )
	
	d <- merge(d, geo, by= "location", all.x = TRUE) 
	 

carobiner::write_files(path, meta, d)

}

