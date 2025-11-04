# R script for "carob"

carob_script <- function(path) {

"Despite the recent release of several improved varieties of groundnut in Nigeria the productivities have not increase significantly due to lack of commensurate recommendation in agronomic practices. Two groundnut varieties were evaluated for their response to different plant density and phosphorus application in two locations in the Sudan Savanna zone of Nigeria in 2012 and 2013. The groundnut were planted at density of 44444, 66667, and 133333 hills ha-1 with average of two plants per hill.
Phosphorus was applied at rate of 0 or 20 kg P ha-1. P fertilizer application increased pod and haulm yields by 26% and 16% respectively in Minjibir. It increased pod and haulm yields by 62% and 27% respectively in Wudil. Pod and haulm yields, harvest index, revenue, profit and cost benefit ratio increased with increasing plant density. Samnut-24 produced pod yields that were significantly higher than Samnut-22 across treatments. Pod yields at density of 133,333 hills ha-1 was 31% higher than at 66667 and 40% than at 44,444 hills ha-1. Application of fertilizer increased profit by 22% and 49% in Minjibir and Wudil respectively. Planting at density of 133,333 hill ha-1 increased profit by 19% and 27% over 66,667 and 444444 hill ha-1 respectively in Minjibir, while it increase profit by 9% in Wudil. Cultivation of Samnut-24 at high density with phosphorus application will make groundnut production a more profitable venture in Sudan Savanna zone of Wudil, Nigeria."

	uri <- "doi:10.21421/D2/5O93XX"
	group <- "agronomy"


	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		publication = "doi:10.12692/ijb/9.1.291-302",
		carob_contributor = "Effie Ochieng'",
		carob_date = "2021-07-06",
		data_type = "experiment",
		data_organization = "ICRISAT",
		project = NA,
		response_vars= "yield",
		treatment_vars="plant_density;P_fertilizer"		
	)

	f <- ff[basename(ff) == "Data file of Groundnut to plant density and phosphorous application in Wudil 2012-13.xlsx"]

	r <- data.frame(readxl::read_excel(f))
	
	d <- data.frame(
		country = "Nigeria",
		adm1 = "Kano",
		adm2 = "Wudil",
		adm3 = "Wudil", #as per the reference
		trial_id = as.character(as.integer(as.factor(r$Location))),
		latitude = 11.793702,
		longitude = 8.838846,
		geo_from_source = TRUE, # Coordinates in metadata (JSON) as bounding box
		on_farm = FALSE,
		is_survey = FALSE,
		rep = as.integer(r$Replication.number),
		crop = "groundnut",
		yield_part = "pod",
		variety = r$Variety,
		yield = r$PodKgHa,
		fwy_residue = r$FdWtKgHa,
		seed_weight = r$Seed.weight,
		fertilizer_type = "SSP", # As indicated in the publication associated
		N_fertilizer = 0,
		P_fertilizer = ifelse(r$Fertilizer == "F1", 0, 20),
		K_fertilizer = 0,
		flowering_days = r$flw50,
		plant_spacing = r$Spacing,
		row_spacing = 75 # from the reference
	)	
		
	#sown during the growing seasons of 2012 and 2013 no actual dates mentioned
	d$harvest_date <- d$planting_date <- ifelse(r$Year == "2012", "2012", "2013") 

	d$plant_density <- ifelse(d$plant_spacing == 30, 44444,
	                   ifelse(d$plant_spacing == 20, 66667, 133333))# from the reference

	#soil properties from reference
	d$s1 <- paste(r$Year, d$adm2, sep = "_")

	ss <- data.frame(
			s1 = c( "2012_Wudil", "2013_Wudil"),
			soil_SOC = c(0.162 ,0.156 ),
			soil_sand = c(88.9 ,92.8),
			soil_clay = c(6.5 ,2.5),
			soil_silt = c(4.6 ,4.6),
	        soil_P = c(2.6 ,2.9),
	        soil_pH  = c(5.10, 5.0),
	        rain  = c(945.8, 907.4)
		)
	
	d <- merge(d, ss, by ="s1", all.x = TRUE)
	d$s1 <- NULL
	
	d$irrigated <- NA
	
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta,  d,  path=path)
}
