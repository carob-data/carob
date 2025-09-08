# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Replication Data for: Assessing rice production sustainability performance indicators and their gaps in twelve sub-Saharan African countries

In this paper, we quantified five sustainability performance indicators (grain yield, net profit, labor productivity, and nitrogen (N) and phosphorus (P) use efficiencies) to benchmark rice production systems in SSA.
"

	uri <- "doi:10.7910/DVN/6K4AB4"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "AfricaRice",
		publication = "doi:10.1016/j.fcr.2021.108263",
		project = "AfricaRice",
		carob_date = "2025-09-07",
		design = NA,
		data_type = "survey",
		treatment_vars = "N_fertilizer; P_fertilizer; K_fertilizer",
	   response_vars = "yield;crop_price", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
   	notes = NA
	)
	

	f <- ff[basename(ff) == "Assessing rice production sustainability performance indicators.xls"]
	
	r <- carobiner::read.excel(f)
	
	
	d <- data.frame(
		country = r$country,
		location = r$hub,
		N_fertilizer = r$elementalN,
		P_fertilizer = r$elementalP,
		K_fertilizer = r$elementalK,
		seed_rate = r$qseed,
		crop_price = r$sprice,
		herbicide_used = grepl("Yes", r$useherb),
		insecticide_used = grepl("Yes", r$useinect),
		yield = r$yield,
		irrigated= grepl("Irrigated", r$domain),
		currency= "USD",
		trial_id = paste(r$country, r$hub, r$laborinput, sep = "-" ), 
		planting_date = "2013", 
		on_farm = FALSE , 
		is_survey = TRUE, 
		crop = "rice", 
		yield_part = "grain", 
		yield_moisture = as.numeric(NA), 
		geo_from_source = FALSE,
		fertilizer_used= TRUE
	)

	
	geo <- data.frame(
	   location = c("Ndop", "Man", "Gagnoa", "Navrongo", "Kumasi", "Ambohibary", "Ankazomiriotra", "Sikasso", "Tillabéri", "Nassarawa", "Kano", "Kahama", "Région des Plateaux", "Région Maritime", "Glazoue", "Kouroumari", "Dagana", "Bo (South) and Kenema (East)"),
	   longitude = c(10.4447, -7.5517, -5.9862, -1.068, -1.6233, 46.768, 46.4908, -5.6778, 1.3432, 7.7055, 8.3791, 32.6012, 1.1682, 1.4135, 2.2439, -6.0384, -15.5040, -11.1960),
	   latitude = c(6.0009, 7.4547, 6.1015, 10.8586, 6.6986, -19.1513, -19.6918, 11.3166, 14.5198, 8.5338, 11.7554, -3.829, 7.5056, 6.2566, 7.9733, 14.9073, 16.5103, 7.863),
	   country= c("Cameroun", "Cote d'ivoire", "Cote d'ivoire", "Ghana", "Ghana", "Madagascar", "Madagascar", "Mali", "Niger", "Nigeria", "Nigeria", "Tanzania", "Togo", "Togo", "Benin", "Mali", "Senegal", "Sierra Leone")
	)
	
	d <- merge(d, geo, by= c("country", "location"), all.x = TRUE)
	
	P <- carobiner::fix_name(d$country)
	P <- gsub("Cameroun", "Cameroon", P)
	P <- gsub("Cote d'ivoire", "Côte d'Ivoire", P)
	d$country <- P
	
### remove duplicate rows
	d <- unique(d)
		
	carobiner::write_files(path, meta, d)

}



