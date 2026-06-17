# R script for "carob"
# license: GPL (>=3)

## ISSUES



carob_script <- function(path) {

"
Field-level crop management, weather, and yield data of wheat from eastern India

The dataset comprises information from 5,689 wheat fields, capturing crop management from sowing to harvesting, including detailed field-level inputs and production practices. Collected over six years (2016–17 to 2021–22) across Bihar and the eastern part of Uttar Pradesh, it includes yield measurements obtained through physical crop-cutting experiments. Complementing this, field-specific weather data such as temperature and solar radiation were accessed from secondary source (global gridded meteorological product AgERA5 v2.0) to align with the wheat-growing season, providing a comprehensive view of yield performance under varying management and weather conditions.
"

	uri <- "doi:10.71682/10549424"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		carob_effort = NA,
		carob_date = "2026-05-26",
		design = NA,
		data_type = "experiment",
		treatment_vars = "land_prep_method;N_fertilizer;P_fertilizer;K_fertilizer",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		carob_completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "Wheat_Yld_Eastern_IND_Data.xlsx"]
	f2 <- ff[basename(ff) == "Wheat_Yld_Eastern_IND_Variables.xlsx"]

	r1 <- carobiner::read.excel(f1, na= "NA")
	r2 <- carobiner::read.excel(f2)

	### process
	
	d <- data.frame(
	  field_id = as.character(r1$Field_ID),
	  date = substr(r1$Year,1, 4),
	  adm2 = carobiner::fix_name(r1$District, "title"),
	  adm1 = carobiner::fix_name(r1$State, "title"),
	  soil_texture = tolower(r1$SoilType),
	  previous_crop = gsub("fallow", "none", tolower(r1$PreviousCrop)),
	  previous_crop_residue_perc = r1$PrevCropResidue,
	  land_prep_method = ifelse(grepl("CT", r1$CropEstablishment), "conventional", "none") ,
	  variety = r1$Variety,
	  seed_rate = r1$SeedRate,
	  planting_date = as.character(r1$SowingDate),
	  fertilizer_amount = rowSums(r1[, c("BasalDAP", "BasalMOP", "BasalNPK", "Split1Urea", "Split2Urea", "Split3Urea")], na.rm = TRUE) ,
	  Urea = rowSums(r1[, c("Split1Urea", "Split2Urea", "Split3Urea")], na.rm = TRUE),
	  fertilizer_type = "urea;DAP;KCl;NPK",
	  DAP = r1$BasalDAP,
	  MOP = r1$BasalMOP,
	  NPKGrade = format(as.POSIXct(r1$GradeNPK, tz = "UTC"), "%H-%M-%S"),
	  NPK = r1$BasalNPK,
	  Zn_fertilizer = r1$BasalZn,
	  irrigation_number = as.integer(r1$IrrigationNumber),
	  herbicide_product = tolower(r1$HerbicideName),
	  herbicide_used = TRUE,
	  fertilizer_used = TRUE,
	  weeding_times = as.integer(r1$WeedingNumber),
	  harvest_date = as.character(r1$HarvestDate),
	  harvest_days = r1$HarvestDay,
	  yield = r1$GrainYield *1000,
	  latitude = r1$Latitude,
	  longitude = r1$Longitude,
	  crop = "wheat",
	  country = "India",
	  tmax = rowMeans(r1[, c("Apr_Tmax", "Dec_Tmax", "Mar_Tmax", "Nov_Tmax")], na.rm = TRUE),
	  srad = rowMeans(sapply(r1[, c("Feb_Srad", "Jan_Srad")], as.numeric), na.rm = TRUE),
	  trial_id = paste(r1$Field_ID, r1$LandType, sep = "-"), 
	  on_farm = TRUE, 
	  is_survey = FALSE, 
	  yield_part = "grain", 
	  yield_moisture = as.numeric(NA), 
	  yield_isfresh = NA,
	  irrigated = TRUE, 
	  geo_from_source = TRUE
	)
	
	d$N_fertilizer <- ifelse(!is.na(d$NPKGrade), d$DAP*0.18 +  d$NPK *as.numeric(substr(d$NPKGrade, 1, 2))/100 + d$Urea *0.46,  d$DAP*0.18  + d$Urea *0.46)
	d$P_fertilizer <- ifelse(!is.na(d$NPKGrade), d$DAP*0.201 + d$NPK *as.numeric(substr(d$NPKGrade, 4, 5))/100, d$DAP*0.201)
	d$K_fertilizer <- ifelse(!is.na(d$NPKGrade), d$MOP*0.498 + d$NPK *as.numeric(substr(d$NPKGrade, 7, 8))/100, d$MOP*0.498)

	d$NPKGrade <- d$NPK <- d$Urea <- d$DAP <- d$MOP <- NULL 
	
	### Fixing soil texture
	
	P <- carobiner::fix_name(d$soil_texture)
	P <- gsub("heavy", "clay", P)
	P <- gsub("light", "sand", P)
	d$soil_texture <- P
	
	### Fixing herbicide product
	
	P <- carobiner::fix_name(d$herbicide_product)
	P <- gsub("metsulfuron\\+sulfosulfuron \\(total\\)", "metsulfuron;sulfosulfuron", P)
	P <- gsub("total, 2,4-d", "2,4-D", P)
	P <- gsub("2,4-dichlorophenoxyacetic acid \\(2,4-d\\)", "2,4-D", P)
	P <- gsub("affinity\\+topic", "carfentrazone-ethyl", P)
	P <- gsub("sulphosulfuron 75% \\(leader\\)\\+carfentrazone ethyl 40df \\(affinity\\)", "sulfosulfuron;carfentrazone-ethyl", P)
	P <- gsub("sulfosulfuron 75% wg \\(leader\\)", "sulfosulfuron", P)
	P <- gsub("total\\+affinity", "carfentrazone-ethyl", P)
	P <- gsub("clodinafob, affinity", "clodinafop;carfentrazone-ethyl", P)
	P <- gsub("leader, affinity", "sulfosulfuron;carfentrazone-ethyl", P)
	P <- gsub("clodinafop 15 w.p. \\(topik\\)", "clodinafop", P)
	P <- gsub("carfentrazone ethyl 40df \\(affinity\\)", "carfentrazone-ethyl", P)
	P <- gsub("affinity,leader", "carfentrazone-ethyl;sulfosulfuron", P)
	P <- gsub("fit", "none", P)
	P <- gsub("clodinafop propargyl 15% \\+ metsulfuron methyl 1% wp \\(vesta\\)", "clodinafop;metsulfuron", P)
	P <- gsub("2,4-d, affinity", "2,4-D;carfentrazone-ethyl", P)
	P <- gsub("metsulfuron 20 w.p. \\(algrip\\)", "metsulfuron", P)
	P <- gsub("2,4-d\\+leader", "2,4-D;sulfosulfuron", P)
	P <- gsub("broadway", "sulphosulfuron", P)
	P <- gsub("topik\\+metsulfuron", "clodinafop;metsulfuron", P)
	P <- gsub("carfentrazone ethyl 40df \\(affinity\\)\\+clodinofop 15w.p. \\(topic\\)", "carfentrazone-ethyl;clodinafop", P)
	P <- gsub("metsulfuron\\+sulphosulfuron \\(total\\)\\+carfentrazone ethyl 40df \\(affinity\\)", "metsulfuron;sulphosulfuron;carfentrazone-ethyl", P)
	P <- gsub("carfentrazone\\+sulphosulfuron 45% \\(broadway\\)", "carfentrazone-ethyl;sulphosulfuron", P)
	P <- gsub("clodinafop 15 w.p. \\(topik\\)\\+metsulfuron 20 w.p. \\(algrip\\)", "clodinafop;metsulfuron", P)
	P <- gsub("sulfosulfuon, 2,4-d", "2,4-D;sulfosulfuron", P)
	P <- gsub("metsulfuron\\+sulfosulfuron, 2,4-d", "2,4-D;metsulfuron;sulfosulfuron", P)
	P <- gsub("caretrazone\\+sulfosulfuron", "carfentrazone-ethyl;sulfosulfuron", P)
	P <- gsub("carfentrazone-ethyl\\+clodinofop 15w.p. \\(topic\\)", "carfentrazone-ethyl;clodinafop", P)
	P <- gsub("carfentrazone\\+sulphosulfuron 45% \\(sulphosulfuron\\)", "carfentrazone-ethyl;sulphosulfuron", P)
	P <- gsub("clodinafob\\+2,4-d", "2,4-D;clodinafop", P)
	P <- gsub("clodinafop\\+metsulfuron", "metsulfuron;clodinafop", P)
	P <- gsub("metsulfuron\\+sulphosulfuron \\(total\\)\\+carfentrazone-ethyl", "metsulfuron;sulphosulfuron;carfentrazone-ethyl", P)
	d$herbicide_product <- P
	
	
	carobiner::write_files(path, meta, d)
}


