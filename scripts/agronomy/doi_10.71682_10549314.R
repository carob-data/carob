# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {


"Replication Data for: Management of maize‑legume conservation agriculture systems rather than varietal choice fosters human nutrition in Malawi
  
This study evaluated the impact of cropping systems (conventional ploughing, no-tillage, and conservation agriculture) and maize varieties on agricultural productivity and nutritional outcomes across multiple districts in Malawi.
Results indicated that conservation agriculture significantly enhanced crop yields and nutritional outputs compared to conventional methods, suggesting its potential to improve food security and climate resilience for smallholder farmers."

	uri <- "doi:10.71682/10549314"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "SLU;CIMMYT",
		publication = "doi.org/10.1007/s12571-024-01479-4",
		project = NA,
		data_type = "on-farm experiment",
		treatment_vars = "land_prep_method;variety",
		response_vars = "yield", 
		completion = 100,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2025-10-30",
		notes = NA, 
		design = "Randomized Complete Block"
	)
	
	f <- ff[basename(ff) == "Data_Muoni et al. 20250410.xlsx"]
  r <- carobiner::read.excel(f)

	d <- data.frame(
	  country="Malawi",
	  harvest_date=as.character(r$`Harvest Year`),
		adm2=r$District,
		adm3=r$Village,
		location=r$Village,
		crop_rotation=r$`Growth strategy`,
		treatment=r$Treatment,
		variety=r$Variety,
		dmy_stems=r$`Stalk yield (kg/ha)`,
		yield=r$`Grain yield (kg/ha)`
		#CA_years=r$`Years of CA`, not sure how to capture this variable
		)
		#Fixing mispelt adm2 locations
	d$adm2 <- gsub("Nkhotkota|Nkotakhota","Nkhotakota",d$adm2)
	d$adm2 <- gsub("Mchinga","Machinga",d$adm2)
	
	loc <- data.frame(
	  location= c("Malula","Enyezini","Chisepo","Chipeni","Zidyana","Mwansambo","Lemu",
	              "Matandika","Linga","Herbert","Songani","Chinguluwe","Champhira","Kaluluma"),
	  latitude= c(-14.8333,-11.4645,-13.6255,-13.8139,-14.0082,-12.9317, -13.8833,-13.2462,-11.45,-13.5396,-15.3849,-15.91667,-12.3326,-12.5811),
	  longitude= c(35,33.8647,33.4669,33.3786,34.6025,34.2811,33.3167,34.2953,34.2,33.02705,35.3184,35.05,33.6092,33.5186)
	  )
	d <- merge(d,loc,by="location",all.x = TRUE)
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$crop_rotation <- ifelse(d$treatment %in% c("Check", "CA+Mz"), "maize", "maize;legume")
	d$crop <- ifelse(d$crop_rotation=="maize","maize","legume")
	d$geo_from_source <- FALSE
	d$planting_date <- r$`Harvest Year`-1
	d$harvest_date  <- as.character(d$harvest_date)
	d$planting_date <- as.character(d$planting_date)#planting_date based on logic, its not stated for individual crop, 
	d$P_fertilizer <-9.17
  d$N_fertilizer <-67
  d$S_fertilizer <-4
  d$lime <- d$K_fertilizer <- as.numeric(NA)
  d$inoculated <- FALSE
  d$yield_part <- "grain"
	d$yield_moisture <- ifelse(d$crop=="maize",12.5,9)
	d$trial_id <- paste(d$location, as.character(d$planting_date), sep = "_")
	
	carobiner::write_files(path, meta, d)
}
