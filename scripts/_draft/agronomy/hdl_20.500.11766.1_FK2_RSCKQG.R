# R script for "carob"
# license: GPL (>=3)

## ISSUES
# yield is NA for the plots in Allal Tazi and Kfarchakhna and some of the plots in Guranghe Ambra and Debrezeit (they still contain other info, so they were left in)

# New variable names
# - row_type: # of rows of the barley spike (6-row)

carob_script <- function(path) {

"
Barley Landraces multi-location field trial - Morocco_Tunisia_Lebanon_Ethiopia 2023-2024

Assessment of Barley Landraces: multi-location field trial conducted in Morocco, Tunisia, Lebanon and Ethiopia during 2023-2024 cropping season.
"
	uri <- "hdl:20.500.11766.1/FK2/RSCKQG"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=5, minor=0,
		data_organization = "ICARDA",
		publication = "",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;dmy_total;heading_days;maturity_days;plant_height;spike_density", 
		notes = NA,
		carob_contributor = "Kudzaishe M. Muzata",
		carob_date = "2026-10-23",
		carob_completion = 80,	
		carob_effort = 6
	)
	

	f <- ff[basename(ff) == "Data.csv"]
	r <- read.csv(f, sep=";", na.strings="NA")

	d <- data.frame(
		location = carobiner::fix_name(r$Location, "title"),
		plot_id = as.character(r$Plot_Number),
		variety_code = trimws(r$Entry24),
		variety = trimws(r$ID),
		variety_traits = r$Trait,
		variety_origin = r$Origin,
		variety_type = trimws(r$Source23),
		row_type = r$Row_Type,
		plot_row = r$Row,
		plot_column = r$Column,
		heading_days = r$DTH,
		maturity_days = r$DTM,
		plant_height = r$PLH,
		spike_density = r$SN * 10000, # convert to spikes/ha from spikes/m2
		grain_per_spike_1 = r$Grain_per_Spike1,
		grain_per_spike_2 = r$Grain_per_Spike2,
		grain_per_spike_3 = r$Grain_per_Spike3,
		dmy_total = r$Biomass_Yield,
		yield = r$GY,
		seed_weight = r$TKW
	)

	d$variety <- sub("^IG[: ]*([0-9]+)", "IG:\\1", d$variety)
	d$crop <- "barley"
	d$yield_part <- "grain"
	d$trial_id <- d$location
	
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- FALSE

	d$location[d$location == "Sidi El Aydi"] <- "Sidi el Aïdi"
	geo <- data.frame(
		location = c("Mornag", "El Kef", "Kfarchakhna", "Tel Amara", "Melk Zhar", "Allal Tazi", "Marchouch", "Annoceur", "Sidi el Aïdi", "Guranghe Ambra", "Debrezeit"),
		country = c("Tunisia", "Tunisia", "Lebanon", "Lebanon", "Morocco", "Morocco", "Morocco", "Morocco", "Morocco", "Ethiopia", "Ethiopia"),
		longitude = c(10.2544, 8.6768, 35.7996, 35.9268, -9.5331, -6.3706, -6.2716, -6.2716, -7.4622, 37.2634, 39.0303),
		latitude = c(36.6024, 36.0466, 34.3230, 33.8129, 30.0247, 33.7042, 33.6632, 33.6632, 32.9583, 12.4029, 8.7486),
		geo_uncertainty = c(15242, 53869, 11084, 16828, 23066, 59439, 71809, 71809, 75765, 26136, 24860)
	)
    d <- merge(d, geo, by="location", all.x=TRUE)
	d$geo_from_source <- FALSE

	d$planting_date <- "2023"
	d$harvest_date  <- "2024"

	d$P_fertilizer <- NA
	d$K_fertilizer <- NA
	d$N_fertilizer <- NA

	d$yield_moisture <- NA
	d$yield_isfresh <- NA
	carobiner::write_files(path, meta, d)
}


