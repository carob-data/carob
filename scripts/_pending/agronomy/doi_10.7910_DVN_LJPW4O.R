# R script for "carob"


# Notes EGB (2024-08-28):
# Biomass information (2a Dry matter measurements.xlsx) contains in-season measurements of biomass 
## that need to be captured (timevars).

carob_script <- function(path) {

"Response of maize to N and P in two trials in Uganda"

	uri <- "doi:10.7910/DVN/LJPW4O"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=5) ,
		publication=NA,
		carob_contributor="Eduardo Garcia Bendito",
		carob_effort = NA,
		carob_date="2021-06-18",
		data_type = "experiment",
		data_organization="CIAT",
		project="AgMIP",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;Zn_fertilizer;Ca_fertilizer;Mg_fertilizer;OM_used",
		response_vars = "yield;fwy_residue;leaf_N;leaf_P;leaf_K;leaf_Ca;leaf_Mg;grain_N;grain_P;grain_K;grain_Ca;grain_Mg;residue_N;residue_P;residue_K;residue_Ca;residue_Mg",
		last_modified = "2024-08-28",
		notes = "Assuming that the biomass, stover and soil records correspond to season 1. Season 2 is nowhere indicated in the protocols"
	)

	f <- ff[basename(ff) == "9a Yield data.xlsx"]
	r <- carobiner::read.excel(f)
	
	height <- carobiner::read.excel(ff[basename(ff) == "5a Plant growth data.xlsx"])
	height <- aggregate(height$`Plant Height (CM)`, by = list(height$Site, height$Season, height$Block, height$Treatment), FUN = mean, na.action = na.omit)
	colnames(height) <- c("Site", "Season", "Block", "Treatment", "plant_height")
	
	r1 <- merge(r, height, c("Site", "Season", "Block", "Treatment"))

	###########

	d <- data.frame(
		country = "Uganda",
		adm1 = "Wakiso",
		adm2 = "Jinja",
		adm3 = ifelse(r$Site == "Kawanda", "Nabweru", "Busukuma"),
		season = r$Season,
		trial_id = r$Site,
		block = r$Block,
		treatment = r$Treatment,
		latitude = ifelse(r$Site == "Kawanda", 0.4172778, 0.5256090),
		longitude = ifelse(r$Site == "Kawanda", 32.5355326, 32.6136960),
		geo_from_source = FALSE,
		planting_date = ifelse(r$Season == 1, "2013-08-10", "2014-03"),
		harvest_date = "2014",
		on_farm = FALSE,
		is_survey = FALSE,	
		crop = "maize",
		variety = "Longe 10H",
		variety_code = "SC627",
		yield = r$`Grain yield (kg/plot -5.625m2)` * (10000/5.625),
		fwy_residue = r$`Stover yield (kg/plot - 5.625m2)` * (10000/5.625),
		yield_part = "grain",
		insecticide_used = TRUE,
		insecticide_product = "Dursban",
		insecticide_timing = 60,
		herbicide_used = TRUE,
		herbicide_product = "glyphosate",
		land_prep_method = "ploughing",
		plant_density = 53333
	)
	
	# Adding Treatment information
	# EGB:
	# # From Protocol:
	# # "Nitrogen will be applied (30%) in planting holes as CAN (27.5%N) in order to supply some calcium (CaO – 3.5%)
	# # and magnesium (MgO – 4%);  30 DAP (30%) and 60 DAP (40%) as Urea (46%) in bands and incorporated or covered with soil.
	# # Potassium will be applied as muriate of potash (52% K), and P as triple super phosphate (20% P) at planting.
	# # Zinc, Boron and Molybdenum - 5 Zn, 0.5 Mo, 1B (kg ha−1) will be applied at planting using Zinc sulphate,
	# # Sodium molybdate and Borax to eliminate deficiencies, except for treatment (8) which will instead receive manure (5 Mg ha−1) at planting."
	d$N_fertilizer <- ifelse(r$Treatment %in% c(8,7,6,5,4), 200,
	                         ifelse(r$Treatment == 3, 60, 0))
	d$N_splits <- NA
	d$N_splits[d$N_fertilizer > 0] <- 3L
	
	d$P_fertilizer <- ifelse(r$Treatment %in% c(8,7,3,2), 90,
	                         ifelse(r$Treatment %in% c(6), 50,
	                                ifelse(r$Treatment %in% c(5), 20, 0)))
	d$K_fertilizer <- ifelse(r$Treatment == 1, 0, 75)
	d$Zn_fertilizer <- ifelse(r$Treatment %in% c(1,8), 0, 5)
	d$B_fertilizer <- ifelse(r$Treatment %in% c(1,8), 0, 1)
	d$Ca_fertilizer <- ifelse(r$Treatment %in% c(1,2), 0,
	                          ifelse(r$Treatment %in% c(4:8), ((0.577*10000)/5.625)*0.035, ((0.173*10000)/5.625)*0.035))
	d$Mg_fertilizer <- ifelse(r$Treatment %in% c(1,2), 0,
	                          ifelse(r$Treatment %in% c(4:8), ((0.577*10000)/5.625)*0.04, ((0.173*10000)/5.625)*0.04))
	d$OM_used <- r$Treatment == 8
	d$OM_type <- ifelse(r$Treatment == 8, "farmyard manure", NA)
	d$OM_amount <- ifelse(r$Treatment == 8, 5000, 0) # From protocols shared by author.
	d$N_organic <- d$OM_amount*(0.1796/100)*(0.755/100) # OM$K (%)
	d$P_organic <- d$OM_amount*(0.1796/100)*(200.67532467532467/1e+06) # OM$P (ppm)
	d$K_organic <- d$OM_amount*(0.1796/100)*(66.9072/1e+06) # OM$K (ppm)

	# EGB:
	# # Combine grains, leaves, residues and soil
	grains <- carobiner::read.excel(ff[basename(ff) == "4a Plant_Grains_sample  data.xlsx"])
	colnames(grains) <- c("Site", "Grain", "Field label", "Block", "Treatment", "Plot", "grain_N", "grain_P", "grain_K", "grain_Ca", "grain_Mg")
	leaves <- carobiner::read.excel(ff[basename(ff) == "6a Plant_Leaves at flowering_sample  data.xlsx"])
	colnames(leaves) <- c("Site", "Leaves at flowering", "Field label", "Block", "Treatment", "Plot", "leaf_N", "leaf_P", "leaf_K", "leaf_Ca", "leaf_Mg")
	stover <- carobiner::read.excel(ff[basename(ff) == "7a Plant_Stovers_sample data.xlsx"])
	colnames(stover) <- c("Site", "Stovers", "Field label", "Block", "Treatment", "Plot", "residue_N", "residue_P", "residue_K", "residue_Ca", "residue_Mg")
	soil <- carobiner::read.excel(ff[basename(ff) == "8a Soil lab data.xlsx"], skip = 12)
	soil$Block <- sub("^\\D*(\\d+).*$", "\\1",  soil$`Client's ref`)
	soil$Plot <- sub('.*(?=.{2}$)', '', soil$`Client's ref`, perl=T)
	soil1 <- soil[,c(1,4:17)]
	# Consider only the first 30 cm
	# # EGB: Should we?
	# soil1 <- soil1[soil1$`Depth (CM)` == "0-15" | soil1$`Depth (CM)` == "15-30", ]
	soil1$`P (ppm)`[soil1$`P (ppm)` == "trace"] = 0.01
	soil1$`P (ppm)` <- as.numeric(soil1$`P (ppm)`)
	soil1$Block <- as.numeric(soil1$Block)
	soil1$Plot <- as.numeric(soil1$Plot)
	soil2 <- aggregate(soil1[, c(3,4,5,6,7,8,9,10,11,12)], list(Site = soil1$Site, Block = soil1$Block, Plot = soil1$Plot), mean, na.rm = TRUE)
	soil2 <- soil2[order(soil2[,"Site"], soil2[,"Block"], soil2[,"Plot"]), ]
	soil2 <- carobiner::change_names(soil2,
	                                 from = colnames(soil2)[4:length(soil2)],
	                                 to = c("soil_pH", "soil_SOM", "soil_N", "soil_P_available",
	                                        "soil_Ca", "soil_Mg", "soil_K",
	                                        "soil_sand", "soil_clay", "soil_silt"))
	d1 <- merge(merge(merge(leaves,
	                        grains, by =  c("Site","Block","Plot")),
	                  stover, by =  c("Site","Block","Plot")),
	            soil2, by = c("Site","Block","Plot"), all.x = TRUE)
	d1 <- d1[,c("Site","Block","Plot","Treatment",
	            "leaf_N","leaf_P","leaf_K","leaf_Ca","leaf_Mg",
	            "grain_N","grain_P","grain_K","grain_Ca","grain_Mg",
	            "residue_N","residue_P","residue_K","residue_Ca","residue_Mg",
	            "soil_pH","soil_SOM","soil_N","soil_P_available","soil_Ca","soil_Mg","soil_K","soil_sand","soil_clay","soil_silt")]
	# EGB:
	# # Assuming the protocol was applied only the first season of the experiment
	d1$season <- 1
	# Join all data
	d <- merge(d, d1, by.x = c("trial_id", "block", "treatment", "season"), by.y = c("Site","Block","Treatment", "season"), all.x = TRUE)
	
	d$fertilizer_type[d$treatment == 1] <- NA
	d$fertilizer_type[d$treatment == 2] <- "TSP; KCl; ZnSO4; borax"
	d$fertilizer_type[d$treatment == 4] <- "CAN; KCl; urea; ZnSO4; borax"
	d$fertilizer_type[d$treatment %in% c(3,5,6,7,8)] <- "CAN; TSP; KCl; urea; ZnSO4; borax"
	d$irrigated <- TRUE
	d$row_spacing <- 75
	d$plant_spacing <- 25
	
	# % to kg/ha
	d$residue_N <- d$residue_N * d$yield
	d$residue_P <- d$residue_P * d$yield
	d$residue_K <- d$residue_K * d$yield
	d$residue_Ca <- d$residue_Ca * d$yield
	d$residue_Mg <- d$residue_Mg * d$yield
	
	# % or(‰) to mg/kg
	d$soil_N <- d$soil_N * 10000
	d$soil_K <- d$soil_K * 10000
	d$grain_N <- d$grain_N * 10000
	d$grain_P <- d$grain_P * 10000
	d$grain_K <- d$grain_K * 10000
	d$grain_Ca[d$grain_Ca == "trace"] = 0.01
	d$grain_Ca <- as.numeric(d$grain_Ca) * 10000
	d$grain_Mg <- d$grain_Mg * 10000
	d$leaf_N <- d$leaf_N * 10000
	d$leaf_P <- d$leaf_P * 10000
	d$leaf_K <- d$leaf_K * 10000
	d$leaf_Ca <- d$leaf_Ca * 10000
	d$leaf_Mg <- d$leaf_Mg * 10000
	
	d <- d[,c("country", "adm1", "adm2", "adm3", "latitude", "longitude", "geo_from_source", "trial_id", "planting_date", "harvest_date", "on_farm", "is_survey", "irrigated",
	          "crop", "variety", "variety_code", "yield", "yield_part", "fwy_residue",
	          "fertilizer_type", "N_fertilizer", "N_splits", "P_fertilizer", "K_fertilizer", "Zn_fertilizer", "B_fertilizer", "Ca_fertilizer", "Mg_fertilizer",
	          "OM_used", "OM_type", "OM_amount", "N_organic", "P_organic", "K_organic",
	          "leaf_N", "leaf_P", "leaf_K", "leaf_Ca", "leaf_Mg",
	          "grain_N", "grain_P", "grain_K", "grain_Ca", "grain_Mg",
	          "residue_N", "residue_P", "residue_K", "residue_Ca", "residue_Mg",
	          "soil_pH", "soil_SOM", "soil_N", "soil_P_available", "soil_Ca", "soil_Mg", "soil_K", "soil_sand", "soil_clay", "soil_silt",
	          "insecticide_used", "insecticide_product", "insecticide_timing", "herbicide_used", "herbicide_product", "land_prep_method",
	          "plant_density", "row_spacing","plant_spacing")]
	
	carobiner::write_files(meta, d, path=path)

}
