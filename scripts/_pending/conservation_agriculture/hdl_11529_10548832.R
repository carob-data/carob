# R script for "carob"

# todo  the treatments are missing (see bottom of script)

carob_script <- function(path) {

"Grain yield data collected from Conservation Agriculture (CA) systems across experiments of varying experimental duration, established in trial locations of Malawi, Mozambique, Zambia, and Zimbabwe under an increasingly variable climate. Data contains different agro-environmental yield response moderators such as type of crop diversifcation and amount of rainfall and aims to identify cropping systems that may provide both short-term gains and longer-term sustainability."

	uri <- "hdl:11529/10548832"
	group <- "agronomy"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication = "doi:10.1016/j.agee.2021.107812",
		project = NA,
		data_type = "experiment",
		response_vars = "yield",
		treatment_vars = NA, 
		carob_contributor = "Blessing Dzuda",
		carob_effort = NA,
		carob_date = "2024-05-31"
	)
	
	f <- ff[basename(ff) == "DATA SA 2005 to 2019.xls"]
	r <- carobiner::read.excel(f, sheet = "Raw Data")

	d <- data.frame(
		trial_id=r$Location,
		planting_date=as.character(r$Season),
		harvest_date=as.character(r$Season+1),
		treatment = r$System,
		rep=as.integer(r$Rep),
		rain=r$Rainfall,
		N_fertilizer=r$Nitrogen,
		P_fertilizer=r$Phosphorus,
		K_fertilizer=r$Potassium,
		soil_clay=r$Clay,
		soil_SOC=r$OrgC,
		fwy_total=r$Biomass,
		yield=r$Grain*1000,
		yield_part="grain",
		crop="maize"
	)
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	
	
	# from the paper, Table 1
	geo <- data.frame(
		trial_id = c("HRS", "MFTC", "CRS", "SRS", "DTC", "MRS"),
		location = c("Henderson Research Station (HRS)", "Monze Farmer Training Centre (MFTC)", "Chitedze Research Station (CRS)", "Sussundenga Research Station (SRS)", "Domboshawa Training Centre (DTC)", "Msekera Research Station (MRS)"),
		country = c("Zimbabwe", "Zambia", "Malawi", "Mozambique", "Zimbabwe", "Zambia"),
		#experimental_period  = c("2004–2019", "2005–2019", "2007–2019", "2006-2014", "2009–2019", "2011–201?"),
		#duration_of_experiment_years = c("15", "14", "12", "9", "10", "8"),
		latitude = c(-17.5727, -16.2402, -13.9732, -19.3169, -17.6091, -13.6450),
		longitude = c(30.9874, 27.4414, 33.6540, 33.2416, 31.1337, 32.5585),
		elevation  = c(1136, 1108, 1145, 608, 1543, 1018),
		soil_texture = c("loamy sand;sand", "sandy loam", "sandy loam", "sandy loam", "sandy loam", "sandy loam"),
		soil_type  = c("Arenosols and Luvisols", "Chromic Lixisols", "Ferruginous Latosols", "Haplic Lixisols", "Rhodic Lixisols", "Luvisols")
	)

	d <- merge(d, geo, by="trial_id", all.x=TRUE)	
	d$geo_from_source <- TRUE
	d$trial_id <- paste0(d$trial_id, "_", d$planting_date)
	d <- unique(d)

	g <- d$treatment
	g <- gsub("-","",g)
	g <- gsub("\\+", "2",g)# remove the plus 
	g <- gsub("BA" ,"basins",g)
	g <- gsub("CP" ,"conventional_ploughing",g)
	g <- gsub("DiS" ,"dibble_stick",g)
	g <- gsub("DiSMC" ,"dibble_stick_maize_cowpea_rotation",g) 
	g <- gsub("DiSM2C" , "dibble_stick_maize_cowpea_intercrop",g)
	g <- gsub("DiSM2Mp", "dibble_stick_maize_velve_bean_intercrop",g)
	g <- gsub("DiSM2Pp", "direct_seeding_maize_pigeonpea_intercrop",g)
	g <- gsub("DS", "direct_seeding_sole_maize",g)
	g <- gsub("DSMG", "direct_seeding_maize_groundnut_rotation",g)
	g <- gsub("DSMSf","direct_seeding_maize_sunflower_rotation",g)
	g <- gsub("DSM2C", "direct_seeding_maize_cowpea_intercrop",g)
	g <- gsub("DSMBio","direct_seeding_maize_biochar",g)
	g <- gsub("RI", "ripping", g)
	g <- gsub("RIM2C", "ripping_maize_cowpea_intercrop",g)
	g <- gsub("DSMCt", "direct_seeding_maize_cotton_rotation",g)
	g <- gsub("DSMCtS", "direct_seeding_maize_cotton_sunhemp_rotation",g)
	g <- gsub("CPMCt" , "conventional_ploughing_maize_cotton_rotation",g)
	g <- gsub("DSMC","direct_seeding_maize_cowpea_rotation",g)
	g <- gsub("DSMSy" ,"direct_seeding_maize_soyean_rotation",g)
	g <- gsub("DSMSfC", "direct_seeding_maize_sunflower_cotton_rotation",g)
	g <- gsub("DSM2Pp", "direct_seeding_maize_pigeonpea_intercrop",g)
	g <- gsub("JP", "jab_planter",g)
	# CP2 not defined
	d$treatment <- g
	
	### now these need to be split into variables on land prep, seeding method, and crop rotation


	
	carobiner::write_files(path, meta, d)
}



