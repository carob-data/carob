# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Herbicide names should come from Pretreat and Posttreat, not from the Treatment abbreviations.
# Pretreat and Posttreat should be separate variables. PostApplWAT is not yet kept.
# herbicide_efficacy and the time series of weed density (Wdden_m2_04WAP through 12WAP,
# and the TWControl, Broadleaf and Grass columns) are not yet kept.
# Some herbicide product names are still unresolved. See the abbreviation list below.
# soil_Ca was previously multiplied by 200 to leave cmol/kg. That factor is not applied here.


carob_script <- function(path) {

"
The Sustainable Weed Management Technologies for Nigeria project tested cassava weed management,
including herbicides, with smallholder farmers in the humid rainforest, forest transition savanna
and southern Guinea savanna. The 2017 on-farm trials cover cassava and cassava-maize plots.
"

	uri <- "doi:10.25502/7k15-wy47/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "herbicide_used;weeding_done",
		response_vars = "yield",
		carob_completion = 30,
		carob_effort = 0,
		carob_contributor = "Your Name",
		carob_date = "2026-10-30",
		notes = "",
		
	)

	f1 <- ff[basename(ff) == "Onfarm2017_All_Locations_DataFile_Rft.csv"]
	f2 <- ff[basename(ff) == "Onfarm2017_All_Locations_WeedSpecies_Rft.csv"]
	r1 <- read.csv(f1)
	r2 <- read.csv(f2)

	# RootYldtha and MzGrainYld_tha are t/ha.
	d <- data.frame(
		location = r1$Location,
		site = r1$Site,
		intercrops = r1$Cropsystem,
		treatment = r1$Treatment,
		latitude = r1$Latitude,
		longitude = r1$Longitude,
		herbicide_product = r1$Treatment,
		plant_density = r1$Standha_Cas,
		fwy_stems = r1$WtStemtha,
		yield_marketable = r1$NoOKRootsha,
		fw_yield = r1$RootYldtha * 1000,
		planting_date = as.character(as.Date(r1$Date_Planted_Cas, "%m/%d/%Y")),
		harvest_date = as.character(as.Date(r1$Date_Harvested_Cas, "%m/%d/%Y")),
		mze_planting_date = as.character(as.Date(r1$Date_Planted_Mz, "%m/%d/%Y")),
		mze_harvest_date = as.character(as.Date(r1$Date_Harvested_Mz, "%m/%d/%Y")),
		mze_plant_density = r1$Standha_Mz,
		cob_density = r1$MzCobCount_ha,
		mze_yield = r1$MzGrainYld_tha * 1000,
		soil_pH_KCl = r1$pH_KCl,
		soil_SOC = r1$OC_pct,
		soil_N = r1$N_pct,
		soil_P_Mehlich = r1$Meh_P_ppm,
		soil_Ca = r1$CMOL_perKg,
		soil_sand = r1$SAND_pct,
		soil_silt = r1$SILT_pct,
		soil_clay = r1$CLAY_pct,
		country = "Nigeria",
		on_farm = TRUE,
		is_survey = FALSE,
		irrigated = FALSE,
		geo_from_source = TRUE
	)

	d$herbicide_used <- !(grepl("FarmerPractise|Untreated", r1$Pretreat) &
		grepl("Shorthandledhoe|FarmerPractise|Untreated", r1$Posttreat))
	d$weeding_done <- grepl("Fa_Prt|_SHh$", r1$Treatment)

	# Abbreviations used in the previous draft. Order matters because some codes are prefixes of others.
	d$herbicide_product <- gsub("_", ";", d$herbicide_product)
	d$herbicide_product <- gsub("La", "isoxaflutole;aclonifen", d$herbicide_product)
	d$herbicide_product <- gsub("Fi", "flumioxazin", d$herbicide_product)
	d$herbicide_product <- gsub("Pr|Prt", "atrazine;s-metolachlor", d$herbicide_product)
	d$herbicide_product <- gsub("Ga", "s-metolachlor;terbuthylazine", d$herbicide_product)
	d$herbicide_product <- gsub("Se", "metribuzin", d$herbicide_product)
	d$herbicide_product <- gsub("Me", "isoxaflutole;indaziflam", d$herbicide_product)
	d$herbicide_product <- gsub("Mo", "flufenacet;diflufenican;flurtamone", d$herbicide_product)
	d$herbicide_product <- gsub("MPw", "foramsulfuron;iodosulfuron;thiencarbazone-methyl;cyprosulfamide", d$herbicide_product)
	d$herbicide_product <- gsub("M61", "foramsulfuron;iodosulfuron-methyl-sodium", d$herbicide_product)
	d$herbicide_product <- gsub("FuC", "fluazifop-butyl;lactofen", d$herbicide_product)
	d$herbicide_product <- gsub("SmC", "clethodim;lactofen", d$herbicide_product)
	d$herbicide_product <- gsub("RUp", "glyphosate", d$herbicide_product)
	d$herbicide_product <- gsub(";SHh", "", d$herbicide_product)

	# Weed density in the file is per m2. * 10000 converts it to per ha.
	d1 <- data.frame(
		location = r2$Location,
		site = r2$Site,
		treatment = r2$Treatment,
		weed_species = r2$WeedSpecies,
		weed_density = r2$Wdden_m2_12WAP * 10000
	)
	d <- merge(d, d1, by = c("location", "site", "treatment"), all.x = TRUE)

	d$intercrops <- gsub("Intercrop", "cassava;maize", d$intercrops)
	d$intercrops <- gsub("Monocrop", "cassava", d$intercrops)
	d$trial_id <- paste(d$location, d$site, sep = "_")

	csva <- d[grepl("cassava", d$intercrops), ]
	csva$crop <- "cassava"
	csva$yield_part <- "roots"
	csva$yield <- csva$fw_yield
	csva$intercrops <- gsub("cassava;|cassava", "", csva$intercrops)

	mze <- d[grepl("maize", d$intercrops), ]
	mze$crop <- "maize"
	mze$yield_part <- "grain"
	mze$yield <- mze$mze_yield
	mze$planting_date <- mze$mze_planting_date
	mze$harvest_date <- mze$mze_harvest_date
	mze$plant_density <- mze$mze_plant_density
	mze$intercrops <- gsub(";maize", "", mze$intercrops)

	d <- rbind(csva, mze)
	d$mze_harvest_date <- d$mze_planting_date <- d$mze_plant_density <- d$mze_yield <- d$fw_yield <- NULL
	d$intercrops[d$intercrops == ""] <- "none"

	carobiner::write_files(path, meta, d)
}
