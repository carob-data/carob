# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {

"
Replication Data for: Yield gap survey

To assess on-farm yield and yield gap for rice in sub-Saharan Africa
"

	uri <- "doi:10.7910/DVN/3LBXE8"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "AfricaRice",
		publication = "doi:10.1016/j.fcr.2017.02.014;doi:10.1016/j.eja.2016.12.010",
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "longitude;latitude;production_system",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-06-24",
		carob_completion = 100,	
		carob_effort = 2
	)
	

	f1 <- ff[basename(ff) == "YGS.xlsx"]
	f2 <- ff[basename(ff) == "Dictionnary.txt"]

	r1 <- carobiner::read.excel(f1)
	
## process	
	d <- data.frame(
		date = as.character(r1$Year),
		country = r1$Country,
		location = r1$Site,
		irrigated = grepl("IL", r1$PS),
		production_system = r1$PS,
		hh_size = r1$N,
		yield = r1$YIELD*1000,
		trial_id = paste(r1$Site, r1$Year, sep="-"), 
		on_farm = FALSE, 
		is_survey = TRUE, 
		crop = "rice", 
		yield_part = "grain", 
		yield_moisture = NA_real_, 
		yield_isfresh = TRUE,
		planting_date = as.character(NA)
	)
	
	
	Geo <- data.frame(
	  country = c("Benin", "Cameroun", "Côte d'Ivoire", "Ethiopia", "Ghana", "Ghana", "Ghana", "Madagascar", "Madagascar", "Mali", "Niger", "Nigeria", "Senegal", "Sierra Leone", "Gambia", "Gambia", "Benin", "Burkina Faso", "Burkina Faso", "Cameroun", "Côte d'Ivoire", "Ghana", "Guinea", "Niger", "Togo", "Togo", "Chad", "Congo, the Democratic Republic of the", "Mali", rep("Rwanda", 2), "Sierra Leone", rep("Tanzania, United Republic of", 2), "Uganda"),
	  location = c("Glazoue", "Lagdo", "Gagnoa", "Fogera", "Kumasi", "Navrongo", "Savelugu", "Ambohibary", "Ankazomiriotra", "Sikasso", "Tillabery", "Nasarawa", "Dagana", "Bo & Kenema", "Central River", "West Coast", "Malanville", "Cascades", "Hauts-bassins", "Ndop", "Man", "Afife", "Haute Guinée", "Gaya", "Région des Plateaux", "Région Maritime", "Tandjilé-Est", "Kinshasa, Bas-Congo, Bandundu", "Kouroumari", "Rugeramigozi (Gikonko II)", "Rwasave (Gikonko I)", "Tormabum", "Kahama", "Kilombero", "Eastern Uganda (Doho)"),
	  longitude = c(2.229, 13.665, -5.951, 37.586, -1.623, -1.088, -0.826, 47.816, 46.533, -6.866, 2.067, 8.238, -15.507, -11.347, -14.780, -16.500, 3.386, -4.435, -4.041, 10.436, -7.550, 0.909, -9.884, 3.460, 1.089, 1.205, 15.876, 15.394, -6.039, 29.8239, 29.8234, -12.006, 32.5929, 36.455, 34.0028),
	  latitude = c(8.168, 9.057, 6.133, 11.949, 6.699, 10.893, 9.626, -18.867, -19.660, 11.530, 13.549, 8.439, 16.517, 7.936, 13.575, 13.259, 11.862, 10.307, 11.389, 5.989, 7.410, 6.064, 10.674, 11.885, 7.451, 6.518, 9.589, -4.425, 14.9073, -2.585, -2.586, 7.419, -3.837, -8.142, 0.937),
	  geo_from_source = FALSE
	)

	d <- merge(d, Geo, by= c("country", "location"), all.x = TRUE)

		## fixing country 
	d$country <- gsub("Tanzania, United Republic of", "Tanzania", d$country)
	d$country <- gsub("Congo, the Democratic Republic of the", "Democratic Republic of the Congo", d$country)
	d$country <- gsub("Cameroun", "Cameroon", d$country)
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	carobiner::write_files(path, meta, d)
}



