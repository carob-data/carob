# R script for "carob"

carob_script <- function(path) {
  
"Response of Groundnut to Plant Density and Phosphorous application in the Sudan Savanna zone of Nigeria.  

Despite the recent release of several improved varieties of groundnut in Nigeria, the productivities have not increase significantly due to lack of commensurate recommendation in agronomic practices. Two groundnut varieties were evaluated for their response to different plant density and phosphorus application in two locations in the udan Savanna zone of Nigeria in 2012 and 2013. The groundnut were planted at density of 44444, 66667, and 133333 hills ha-1 with average of two plants per hill. Phosphorus was applied at rate of 0 or 20 kg P ha-1 . P fertilizer application increased pod and haulm yields by 26% and 16% respectively in Wudil. It increased pod and haulm yields by 62% and 27% respectively in Wudil. Pod and haulm yields, harvest index, revenue, profit and cost benefit ratio increased with increasing plant density. Samnut-24 produced pod yields that were significantly higher than Samnut-22 across treatments. Pod yields at density of 133,333 hills ha-1 was 31% higher than at 66667 and 40% than at 44,444 hills ha-1. Application of fertilizer increased profit by 22% and 49% in Wudil respectively. Planting at density of 133,333 hill ha-1 increased profit by 19% and 27% over 66,667 and 444444 hill ha-1 respectively in Wudil, while it increase profit by 9% in Wudil. cultivation of Samnut-24 at high density with phosphorus application will make groundnut production a more profitable venture in Sudan Savanna zone of Nigeria."
  
	uri <- "doi:10.21421/D2/STACVA"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)
  
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		publication=NA,
		carob_contributor="Siyabusa Mkuhlani",
		carob_date="2022-09-12",
		data_type="experiment",
		data_organization="ICRISAT",
		project=NA,
		response_vars = "yield",
		treatment_vars = "variety;plant_density;P_fertilizer"
	)
  
	f <- ff[basename(ff) == "Data file of Groundnut fertilizer plant density of combine Wudil..xlsx"]
	r <- carobiner::read.excel(f, fix_names=TRUE, sheet="Sheet1")
	
	d <- data.frame(
		planting_date=as.character(r$Year),
		location=r$Location,
		rep=as.integer(r$Replication.number),
		variety=r$Variety,
		treatment=paste(r$Variety, r$Fertilizer, r$Spacing, sep="_"),
		yield = r$Pod.weight,
		fwy_residue = r$fodder.weight,
		seed_weight = r$seed.weight,
		harvest_index = r$HI_Calc_ratio
	)

	d$trial_id <- d$planting_date

	d$P_fertilizer <- 0
	d$P_fertilizer[grep("F2", d$treatment)] <- 20

	d$N_fertilizer <- 0
	d$K_fertilizer <- 20
	d$fertilizer_type <- "unknown"

	d$plant_density <- 2 * c(44444, 66667, 133333)[r$Spacing/10]

	d$crop <- "groundnut"
	d$yield_part <- "pod"

	d$country <-  "Nigeria"
	d$adm1 <- "Kano"
	d$location <- "Wudil"
	d$longitude <- 8.8307
	d$latitude <- 11.8094
	d$geo_from_source <- FALSE
	 
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA

	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, d, path=path)
}


