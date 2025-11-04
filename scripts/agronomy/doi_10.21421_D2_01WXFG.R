# R script for "carob"

carob_script <- function(path) {
  
"Response of Groundnut to plant density and phosphorous application in the sudan savanna zone of Minjibir, Nigeria. 
Abstract: Despite the recent release of several improved varieties of groundnut in Nigeria the productivities have not increase significantly due to lack of commensurate recommendation in agronomic practices. Two groundnut varieties were evaluated for their response to different plant density and phosphorus application in two locations in the Sudan Savanna zone of Nigeria in 2012 and 2013. The groundnut were planted at density of 44444, 66667, and 133333 hills ha-1 with average of two plants per hill. Phosphorus was applied at rate of 0 or 20 kg P ha-1 . P fertilizer application increased pod and haulm yields by 26% and 16% respectively in Minjibir. It increased pod and haulm yields by 62% and 27% respectively in Wudil. Pod and haulm yields, harvest index, revenue, profit and cost benefit ratio increased with increasing plant density. Samnut-24 produced pod yields that were significantly higher than Samnut-22 across treatments. Pod yields at density of 133,333 hills ha-1 was 31% higher than at 66667 and 40% than at 44,444 hills ha-1. Application of fertilizer increased profit by 22% and 49% in Minjibir and Wudil respectively. Planting at density of 133,333 hill ha-1 increased profit by 19% and 27% over 66,667 and 444444 hill ha-1 respectively in Minjibir, while it increase profit by 9% in Wudil. Cultivation of Samnut-24 at high density with phosphorus application will make groundnut production a more profitable venture in Sudan Savanna zone of Minjibir, Nigeria."
  

	uri <- "doi:10.21421/D2/01WXFG"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)
	  
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		publication= "doi:10.12692/ijb/9.1.291-302",
		carob_contributor="Siyabusa Mkuhlani",
		carob_date="2022-09-12",
		data_type="experiment",
		data_organization="ICRISAT",
		project=NA,
		response_vars= "yield",
		treatment_vars="plant_density;P_fertilizer"		
	)
	  
	f <- ff[basename(ff) == "Data file of Groundnut to plant density and phosphorous application in Minjibir 2012-13.xlsx"]
	r <- carobiner::read.excel(f)
  
	d <- data.frame(
		country = "Nigeria",
		adm1 = "Kano",
		adm2 = r$Location,
		adm3 = "Wasai", #from the reference
		latitude = 8.67,
		longitude = 12.15,
		geo_from_source = FALSE,
		# sown during the growing seasons of 2012 and 2013 no actual dates mentioned
		planting_date = as.character(r$Year),
		harvest_date = as.character(r$Year),
		rep = as.integer(r$`Replication number`),
		variety = r$Variety,
		P_fertilizer = ifelse(r$Fertilizer == "F1", 0, 20),
		N_fertilizer = 0,
		K_fertilizer = 0,
		yield = r$PodKgHa,
		fwy_residue = r$FdWtKgHa,
		seed_weight = r$seedgm,
		crop = "groundnut",
		yield_part = "pod",
		trial_id = r$Location,
		on_farm = TRUE,
		is_survey = FALSE,	
		row_spacing = 75,

		flowering_days = r$Flw50,
		plant_spacing = r$Spacing,
		# as per the reference
		plant_density = ifelse(r$Spacing == 30, 44444,
					 ifelse(r$Spacing == 20, 66667, 133333)),
		fertilizer_type = "SSP" 
	)
	
	#soil properties from reference
	d$s1 <- paste(d$Year, d$adm2, sep = "_")
	ss <- data.frame(
		s1 = c("2012_Minjibir", "2013_Minjibir"),
		soil_SOC = c(0.221, 0.198 ),
		soil_sand = c(91.7, 89.8),
		soil_clay = c(4.2, 6.0),
		soil_silt = c(4.0, 4.2),
		soil_P = c(3.1, 3.6),
		soil_pH  = c(5.10, 5.0),
        rain     = c(994, 1054.3)
	)

	d <- merge(d, ss, by ="s1", all.x = TRUE)
	
	d$s1 <- NULL
	d$irrigated <- NA
	
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, d, path=path)
}


