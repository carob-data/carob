# R script for "carob"
# license: GPL (>=3)

## ISSUES
## The amounts of fertilizer used are not specified (NPK as a dry bulk blend broadcasted and lime) 

carob_script <- function(path) {

"
Optimum seeding rate for biomass sorghum in response to harvesting and planting dates in the Mid-Atlantic regions

This dataset includes measurements from a field study evaluating biomass sorghum growth and performance at two sites over three years. The dataset contains dry matter yield (Mg ha⁻¹), plant height (cm), stem diameter (mm), disease severity (%), and lodging severity (%). It also includes biomass yields harvested at different times within the growing season. These data were used in the manuscript to assess the impact of seeding rate on biomass sorghum performance in the Mid-Atlantic region in response to planting date and harvest time.
"

	uri <- "doi:10.5061/dryad.jwstqjqnq"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=6, minor=NA,
		data_organization = "SIU; VT",
		publication = "doi:10.1038/s41598-026-41418-1",
		project = NA,
		carob_effort = NA,
		carob_date = "2026-05-06",
		design = NA,
		data_type = "experiment",
		treatment_vars = "seed_density;planting_date;harvest_date",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		carob_completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "Seeding_rate_Data.csv"]
	r1 <- read.csv(f1, na= ".")

	### Seeding_rate_Data.csv stores two separate side-by-side tables:
	### cols 1-5: sequential harvest timing (Aug/Sep/Oct) in 2017 & 2019
	### cols 6-14: planting date (Early/Late) x seeding rate in 2017-2019 (final harvest)

	# Planting date x seeding rate table (cols 6-14)
	d1 <- data.frame(
	  trial_id = paste0(r1$E, "_planting"),
	  location = substr(r1$E, 5, 11),
	  seed_density = r1$S.rate,
	  rep = as.integer(r1$B),
	  planting_date = ifelse(grepl("Late planting", r1$planting.date) & grepl("2017", r1$E), "2017-07-05",
	                  ifelse(grepl("Late planting", r1$planting.date) & grepl("2018", r1$E), "2018-07-02",
	                  ifelse(grepl("Late planting", r1$planting.date) & grepl("2019", r1$E), "2019-06-29",
	                  ifelse(grepl("Early planting", r1$planting.date) & grepl("2017Holland", r1$E), "2017-06-09",
	                  ifelse(grepl("Early planting", r1$planting.date) & grepl("2018Holland", r1$E), "2018-06-09",
	                  ifelse(grepl("Early planting", r1$planting.date) & grepl("2019Holland", r1$E), "2019-05-24",
	                  ifelse(grepl("Early planting", r1$planting.date) & grepl("2017Hare", r1$E), "2017-06-09",
	                  ifelse(grepl("Early planting", r1$planting.date) & grepl("2018Hare", r1$E), "2018-06-07", "2019-05-23")))))))), # from publication
	  harvest_date = substr(r1$E, 1, 4),
	  yield = as.numeric(r1$Yield..Mg.ha.) * 1000,
	  dmy_total = as.numeric(r1$Yield..Mg.ha.) * 1000,
	  plant_height = r1$Plant.height..cm.,
	  stem_diameter = as.numeric(r1$Stem.diameter..mm.) / 10, # mm -> cm
	  disease_severity = as.character(r1$Disease.severity....),
	  soil_texture = ifelse(grepl("Holland", r1$E), "loam", "loamy sand")
	)

	# Sequential harvest timing table (cols 1-5)
	d2 <- data.frame(
	  trial_id = paste0(r1$Environment..E., "_harvest"),
	  location = substr(r1$Environment..E., 5, 11),
	  seed_density = r1$Seeding..S..rate,
	  rep = as.integer(r1$Block..B.),
	  planting_date = substr(r1$Environment..E., 1, 4),
	  harvest_date = sprintf("%s-%02d", substr(r1$Environment..E., 1, 4), as.integer(carobiner::eng_months_to_nr(r1$Harvest.time))),
	  yield = as.numeric(r1$Dry.matter.yield..Mg.ha.) * 1000,
	  dmy_total = as.numeric(r1$Dry.matter.yield..Mg.ha.) * 1000,
	  soil_texture = ifelse(grepl("Holland", r1$Environment..E.), "loam", "loamy sand")
	)

	d <- carobiner::bindr(d1, d2)

	d$crop <- "sorghum"
	d$country <- "United States"
	d$adm1 <- "Virginia"
	d$adm2 <- "Suffolk"
	# Virginia Tech Tidewater AREC (Holland and Hare research farms), Suffolk, VA
	d$longitude <- -76.7671
	d$latitude <- 36.6850
	d$geo_from_source <- FALSE
	d$fertilizer_type <- "lime;NPK;DAS" # (ammonium sulfate 24-0-0-2) from reference; amounts not specified
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$yield_part <- "aboveground biomass"
	d$yield_moisture <- 0
	d$yield_isfresh <- FALSE

	carobiner::write_files(path, meta, d)
}

