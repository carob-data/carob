# R script for "carob"

carob_script <- function(path) {

"This data set is from a long-term (2010-2016) trial set in sandy soils. The study seeks to monitor and evaluate the effects over time of conservation agriculture (CA) practices on crop yield, soil quality, weeds, pests and diseases. The trial was set as a randomised complete block design with the following treatments:

T1: Check plot (CP); traditional farmers practice using the mouldboard plough, maize as a sole crop, no residue retention, stubbles incorporated 
T2: Direct seeding with animal drawn seeder (DSM), maize as a sole crop, residue retention (at a rate of 2.5-3 t ha-1 in the first year, thereafter all crop residues retained) 
T3: Basin (BAM), maize as a sole crop, residue retention 
T4: Jab planter (JPM), maize as a sole crop, residue retention 
T5: Direct seeding with animal drawn seeder (DSMB), biochar incorporated, maize as a sole crop, residue retention 
T6: Direct seeding with animal drawn seeder (DSMP), maize-pigeon pea (Cajanus cajan) intercropping, residue retention 
T7: Crop rotation A1 (A1M): direct seeding with animal drawn seeder, maize-groundnut rotation (Phase 1), residue retention; Maize-Groundnut 
T8: Crop rotation A2(A2G): direct seeding with animal drawn seeder, maize-groundnuts rotation (Phase 2), residue retention; Groundnuts-Maize 
T9: Crop rotation B1 (B1M): direct seeding with animal drawn seeder, maize-sunflower rotation (Phase 1), residue retention; Maize-Sunflower 
T10: Crop rotation B2 (B2S): direct seeding with animal drawn seeder, maize-sunflower rotation (Phase 2), residue retention; Sunflower-Maize. (2016)]
"

	uri <- "hdl:11529/10842"
	group <- "agronomy"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIMMYT",
		publication= NA,
		project=NA,
		data_type= "experiment",
		carob_contributor= "Blessing Dzuda",
		carob_date="2024-04-12",
		treatment_vars = "crop_rotation;planting_method;residue_prevcrop",
		response_vars = "yield"
	)

	f <- ff[basename(ff) == "Domboshawa 2010.2016.xlsx"]

#### Maize

	r1 <- carobiner::read.excel(f)
	d1 <- data.frame(
		planting_date=as.character(r1$year),
		country=r1$Country,
		location=r1$Location,
		site="Domboshawa Training Centre",
		treatment=r1$Tmnt.,
#		crop=tolower(r1$Crop),
		crop="maize",
		rep=as.integer(r1$Rep),
		yield_part="grain",
		yield=r1$`Grain/cotton yield (kg/ha)`,
		fwy_residue = r1$`Biomass yield (kg/ha)`,
		latitude= -17.60527,
		longitude= 31.13669,
		trial_id="1"
	)
#	d1$crop <- gsub("maize/ppea", "maize;pigeon pea", d1$crop)
#	d1$crop <- carobiner::replace_values(d1$crop, "maize+cowpea", "maize;cowpea")
	
	
#### Other crops
	
	r2 <- carobiner::read.excel(f, sheet ="All legumes DTC")
	d2 <- data.frame(
		planting_date=as.character(r2$Year),
		country=r2$Country,
		location=r2$Location,
		site="Domboshawa Training Centre",
		treatment=r2$Tmnt.,
		crop=tolower(r2$Crop),
		rep=as.integer(r2$Rep),
		yield_part="seed",
		yield=r2$`Grain yield (kg/ha)`,
		fwy_residue = r2$`Biomass yield (kg/ha)`,
		latitude= -17.60527,
		longitude= 31.13669,
		trial_id="2"
	)
	d2$crop <- gsub("groundnuts|grountnuts", "groundnut", d2$crop)
	
	d <- rbind(d1, d2)
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$soil_type <- "sandy"

	d$previous_crop_residue_perc <- 100

## this could be added by using the residue from the previous year for the same plot
##	d$residue_prevcrop <- 
	
	d$land_prep_implement <- ifelse(d$treatment == 1, "mouldboard plough", "none") 
	d$planting_method <- ifelse(d$treatment == 1, "unknown", "direct") 
	rotations <- c(rep("none", 5), "maize;pigeon pea", "maize;pigeon pea", "maize;sunflower", "maize;sunflower")
	d$crop_rotation <- rotations[d$treatment]
	d$OM_used <- d$treatment == 5
	d$OM_type <- ifelse(d$treatment == 5, "biochar", "none")

	d$treatment <- as.character(d$treatment)
	d$geo_from_source <- FALSE
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(path, meta,d)
}


