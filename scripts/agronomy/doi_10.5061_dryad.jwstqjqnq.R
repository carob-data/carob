# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Optimum seeding rate for biomass sorghum in response to harvesting and planting dates in the Mid-Atlantic regions

This dataset includes measurements from a field study evaluating biomass sorghum growth and performance at two sites over three years. The dataset contains dry matter yield (Mg ha⁻¹), plant height (cm), stem diameter (mm), disease severity (%), and lodging severity (%). It also includes biomass yields harvested at different times within the growing season. These data were used in the manuscript to assess the impact of seeding rate on biomass sorghum performance in the Mid-Atlantic region in response to planting date and harvest time.
"

	uri <- "doi:10.5061/dryad.jwstqjqnq"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=6, minor=NA,
		data_organization = "SIU; VPI",#Virginia Polytechnic Institute
		publication = "doi:10.1038/s41598-026-41418-1",
		project = NA,
		carob_date = "2026-05-06",
		design = NA,
		data_type = "experiment",
		treatment_vars = "seed_density",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "Seeding_rate_Data.csv"]
	f2 <- ff[basename(ff) == "README.md"]

	r1 <- read.csv(f1, na= ".")
	#r2 <- read.???(f2)

	### process
	
	d <- data.frame(
	  harvest_date = paste(substr(r1$Environment..E., 1, 4), carobiner::eng_months_to_nr(r1$Harvest.time), sep = "-") ,
	  location = substr(r1$Environment..E., 5, 11),
	  seed_density = r1$Seeding..S..rate,
	  rep = r1$Block..B.,
	  dmy_total = r1$Dry.matter.yield..Mg.ha.*1000,
	  planting_date = ifelse(grepl("Late planting", r1$planting.date) & grepl("2017", r1$E), "2017-07-05",
	                  ifelse(grepl("Late planting", r1$planting.date) & grepl("2018", r1$E), "2018-07-02",
	                  ifelse(grepl("Late planting", r1$planting.date) & grepl("2019", r1$E), "2019-06-29",
	                  ifelse(grepl("Early planting", r1$planting.date) & grepl("2017Holland", r1$E), "2017-06-09",
	                  ifelse(grepl("Early planting", r1$planting.date) & grepl("2018Holland", r1$E), "2018-06-09",
	                  ifelse(grepl("Early planting", r1$planting.date) & grepl("2019Holland", r1$E), "2019-05-24",
	                  ifelse(grepl("Early planting", r1$planting.date) & grepl("2017Hare", r1$E), "2017-06-09",
	                  ifelse(grepl("Early planting", r1$planting.date) & grepl("2018Hare", r1$E), "2018-06-07", "2019-05-23")))))))), # from publication
	  yield = r1$Yield..Mg.ha.*1000,
	  plant_height = r1$Plant.height..cm.,
	  disease_severity = as.character(r1$Disease.severity....),
	  crop = "sorghum",
	  country ="United States",
	  soil_texture = ifelse(grepl("Holland", r1$E), "loam", "loamy sand"),
	  trial_id = paste(r1$Environment..E., r1$Block..B.), 
	  on_farm = TRUE, 
	  is_survey = FALSE, 
	  yield_part = "grain", 
	  yield_moisture = as.numeric(NA), 
	  geo_from_source = FALSE,
	  irrigated = NA,
	  yield_isfresh = TRUE
		
	)
	
	d$harvest_date <- gsub("-8", "-08", d$harvest_date)
	d$harvest_date <- gsub("-9", "-09", d$harvest_date)
	
	### Adding longitude and latitude coordinate
	
	geo <- data.frame(
	  location = c("Holland","Hare"),
	  longitude = c(-86.1050, -85.8210),
	  latitude = c(42.7898, 42.6978)
	)
	
	d <- merge(d, geo, by="location", all.x = TRUE)
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA) 

	carobiner::write_files(path, meta, d)
}

