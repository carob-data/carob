# R script for "carob"
# license: GPL (>=3)

## NOTES
# Soil survey (Oct-Nov 2014) of 96 rice fields of farm households in the Central Luzon Loop Survey (CLLS). HHCODE and PARNO are the CLLS household and parcel codes, and FSIZE (ha) is copied from the CLLS. The soil measurements are new data, not a copy of the Loop surveys, but the fields (and FSIZE) come from the Loop survey: a human should decide how to link this to the CLLS scripts and whether FSIZE duplicates them.
# Methods from "METHODOLOGY DESCRIPTION.pdf" (Silva et al.) and Silva et al. 2017 (Eur. J. Agron. 82: 223-241, doi:10.1016/j.eja.2016.06.017, PDF included in the dataset).
# Composite auger samples (texture by hydrometer, Walkley-Black organic C, Olsen P, NH4OAc exchangeable K, pH 1:1 water) and core samples (bulk density) are taken at different depths, so each field has two records: composite (depth from "Auger Depth", 0-15 cm) and core (5-10 cm, "Core Depth").
# Exch. K meq/100g = cmol(+)/kg. "Region" column holds province names (adm1); "Municip" is the municipality (adm2).
# Field observations (land use, phenology, straw management, moisture, plough pan, bund height, soil colour) are kept on the composite record.

## ISSUES
# No field coordinates in the file, although the methodology note says latitude/longitude exist: ask IRRI. Municipality centroids are used. GADM 4.1 spells Pozzorubio with two z's, so that name was not changed. Cabanatuan, Gapan, San Jose, and Urdaneta were aligned to the GADM City names.
# The methodology note says composite samples were 0-20 cm; the data say "0 - 15". Data value used.
# Plough pan depth has a range value "15 - 20"; kept as text in plough_pan_depth_.
# 10 fields have no core (no bulk density); their core records have no soil data and are dropped (only the composite record remains).
# Paper's management/yield data for these fields are in the Loop survey, not here.
# Two composite samples have no pH, two no dry colour.
# write_files: unknown (new) variables plough_pan_depth_, bund_height_, soil_color_dry_, straw_management_, soil_moisture_status_, n_cores_, n_auger_samples_.
# data_organization set to "IRRI"; starter had "WUR" (first author at Wageningen) - consider "IRRI; WUR".

## NEW VARIABLES
# plough_pan_depth_: depth of the plough pan at the sampling location, cm (text, one range value)
# bund_height_: average height of the field bunds, cm
# soil_color_dry_: soil colour after drying in the laboratory (soil_color is the moist colour in the field)
# straw_management_: management of the rice straw at sampling (burnt, removed, incorporated, ...)
# soil_moisture_status_: field moisture at sampling (aerobic, anaerobic, both)
# n_cores_: number of core rings taken
# n_auger_samples_: number of auger samples composited


carob_script <- function(path) {

"
Explaining rice yields and yield gaps in Central Luzon, Philippines: An application of stochastic frontier analysis and crop modelling

The objective of the study was to decompose the rice yield gap into an efficiency, resource and technology yield gaps and to explain those using information related to crop management, farmers' objectives and constraints and production technology employed. Soil samples were collected to assess the influence of key soil properties on the efficiency yield gap.
"

	uri <- "doi:10.7910/DVN/HMRZID"
	group <- "soil_samples"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "IRRI",
		publication = "doi:10.1016/j.eja.2016.06.017",
		project = "Central Luzon Loop Survey",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 85,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "DATA SOIL SURVEY CLLS_FINAL.xlsx"]
	r <- carobiner::read.excel(f, sheet="Summary Data", na="NA")
	# first row holds units
	r <- r[-1, ]

	lu <- tolower(r[["Land Use"]])
	d1 <- data.frame(
		sample_id = paste0(r$ID, "_composite"),
		hhid = as.character(r$HHCODE),
		field_id = as.character(r$PARNO),
		field_size = as.numeric(r$FSIZE),
		adm1 = r$Region,
		adm2 = r$Municip,
		date = as.character(as.Date(r[["Sampling Date"]], format="%d.%m.%Y")),
		depth_top = as.numeric(sub(" - .*", "", r[["Auger Depth"]])),
		depth_bottom = as.numeric(sub(".* - ", "", r[["Auger Depth"]])),
		soil_clay = as.numeric(r$Clay),
		soil_silt = as.numeric(r$Silt),
		soil_sand = as.numeric(r$Sand),
		soil_SOC = as.numeric(r[["Org. C"]]),
		soil_P = as.numeric(r[["Olsen-P"]]),
		soil_P_method = "Olsen",
		soil_K_exch = as.numeric(r[["Exch. K"]]),
		soil_pH = as.numeric(r[["pH H2O"]]),
		soil_color = r$Color_BD,
		soil_color_dry_ = r$Color_AD,
		land_use = lu,
		crop = ifelse(lu %in% c("rice", "maize", "melon"), lu, NA),
		growth_stage = r$Phenology,
		straw_management_ = r[["Rice Straw"]],
		soil_moisture_status_ = r$Moisture,
		plough_pan_depth_ = r[["Plough Pan"]],
		bund_height_ = as.numeric(r[["Bund Height"]]),
		n_auger_samples_ = as.numeric(r$Auger_A)
	)

	core <- !is.na(r[["BD_50%"]])
	rc <- r[core, ]
	d2 <- data.frame(
		sample_id = paste0(rc$ID, "_core"),
		hhid = as.character(rc$HHCODE),
		field_id = as.character(rc$PARNO),
		field_size = as.numeric(rc$FSIZE),
		adm1 = rc$Region,
		adm2 = rc$Municip,
		date = as.character(as.Date(rc[["Sampling Date"]], format="%d.%m.%Y")),
		depth_top = as.numeric(sub(" - .*", "", rc[["Core Depth"]])),
		depth_bottom = as.numeric(sub(".* - ", "", rc[["Core Depth"]])),
		soil_bd = as.numeric(rc[["BD_50%"]]),
		n_cores_ = as.numeric(rc$Core)
	)

	d <- carobiner::bindr(d1, d2)
	d$country <- "Philippines"
	d$trial_id <- paste(d$hhid, d$field_id, sep="_")
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	# Cabanatuan is the GADM name Cabanatuan City
	d$adm2[which(d$adm1 == "Nueva Ecija" & d$adm2 == "Cabanatuan")] <- "Cabanatuan City"
	# Gapan is the GADM name Gapan City
	d$adm2[which(d$adm1 == "Nueva Ecija" & d$adm2 == "Gapan")] <- "Gapan City"
	# San Jose is the GADM name San Jose City
	d$adm2[which(d$adm1 == "Nueva Ecija" & d$adm2 == "San Jose")] <- "San Jose City"
	# Urdaneta is the GADM name Urdaneta City
	d$adm2[which(d$adm1 == "Pangasinan" & d$adm2 == "Urdaneta")] <- "Urdaneta City"
	# Coordinates are the GADM point-radius centroid of the municipality.
	geo <- data.frame(
		adm1 = c("Bulacan", "Bulacan", "La Union", "Nueva Ecija", "Nueva Ecija", "Nueva Ecija", "Nueva Ecija", "Nueva Ecija", "Nueva Ecija", "Nueva Ecija", "Pangasinan", "Pangasinan", "Pangasinan", "Pangasinan", "Pangasinan", "Pangasinan", "Pangasinan", "Pangasinan", "Tarlac", "Tarlac", "Tarlac"),
		adm2 = c("San Miguel", "San Rafael", "Rosario", "Bongabon", "Cabanatuan City", "Gapan City", "San Jose City", "San Leonardo", "Santo Domingo", "Talavera", "Pozzorubio", "Rosales", "Santa Maria", "Sison", "Tayug", "Umingan", "Urdaneta City", "Villasis", "Moncada", "San Manuel", "Tarlac City"),
		longitude = c(121.0401, 120.9915, 120.459, 121.2313, 120.9899, 121.0054, 120.9976, 120.9474, 120.8804, 120.9345, 120.5195, 120.6338, 120.694, 120.5495, 120.7435, 120.8199, 120.5586, 120.5717, 120.5534, 120.5974, 120.6018),
		latitude = c(15.1731, 14.9835, 16.2454, 15.6514, 15.4924, 15.2828, 15.7846, 15.3596, 15.62, 15.614, 16.1201, 15.8747, 15.9694, 16.177, 16.0188, 15.8987, 15.978, 15.9188, 15.7446, 15.8288, 15.4844),
		geo_uncertainty = c(12069, 9885, 6208, 12904, 12945, 14921, 9739, 6941, 7372, 11385, 6540, 6546, 7708, 11615, 5565, 14670, 9265, 7573, 10113, 6747, 12471),
		geo_source = "GADM 4.1, adm2"
	)
	d <- merge(d, geo, by=c("adm1", "adm2"), all.x=TRUE, sort=FALSE)
	d$geo_from_source <- FALSE

	carobiner::write_files(path, meta, d)
}
