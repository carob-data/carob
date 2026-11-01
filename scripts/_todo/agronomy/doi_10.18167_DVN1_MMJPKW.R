# R script for "carob"
# license: GPL (>=3)

## NOTES
# Measured grain and aboveground biomass from Crop_and_soil_measurements. mafruit and masec_n are t/ha. Wide yield is the maximum mafruit in the series (kg/ha), on the latest date when the maximum is tied. The series is in the long table.
# Saria and Sob rows are treatment means over three replicates; Bambey and Ntarla are single replicates (sheet comment). They are measured values. No simulated yield column is in this workbook.
# N_fertilizer is the sum of absolute_value (kg N/ha) on the Crop management sheet. P and K are not in that sheet. engrais 1 is AN and 3 is urea (sheet comment).
# Country is the site-name prefix (Burkina_Saria, Mali_Ntarla, Senegal_Bambey, Senegal_Sob), which matches geographicCoverage. Coordinates are the Site_location sheet.

## ISSUES
# P and K are not in the management sheet. Some experimental units have no grain yield. Early plant heights are below 10 cm.
# write_files: missing variables: project. missing variables: yield_moisture, K_fertilizer, P_fertilizer, harvest_date. NA detected: yield. unknown variable: grain_biomass_ (grain_B?). unknown variables: aboveground_biomass_. out of bounds: plant_height (4.794, 221.603).

## NOT PROCESSED
# Soil sheet (STICS-style parameters). Residue and tillage columns. Standard-deviation columns. Stem and leaf dry-matter components. Crop N concentrations.
# Global radiation, wind, and vapour pressure (units not confirmed).

## NEW VARIABLES
# grain_biomass_: mafruit time series, kg/ha.
# aboveground_biomass_: masec_n time series, kg/ha. Wide dmy_total is the value on the yield date.


carob_script <- function(path) {

"
Data for 'The STICS intercrop model accurately reproduces interspecific interactions and yield in  semi-arid West African cereal-legume intercropping systems'

The dataset contains experimental data on cereal-legume intercropping from four sites in West Africa (two sites in Senegal, Mali and Burkina Faso) for crop modelling calibration and use. The dataset contains observed data on sorghum, millet, and cowpea in sole crop and intercrops, soil properties and observed soil water content, management data and weather data.
"

	uri <- "doi:10.18167/DVN1/MMJPKW"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = "doi:10.1016/j.fcr.2026.110468",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety;intercrops",
		response_vars = "yield;dmy_total",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.9
	)

	f <- ff[basename(ff) == "Database_DeFreitas_et_al_2026.xlsx"]
	eu <- carobiner::read.excel(f, sheet = "Experimental_unit")
	loc <- carobiner::read.excel(f, sheet = "Site_location")
	cm <- carobiner::read.excel(f, sheet = "Crop management")
	meas <- carobiner::read.excel(f, sheet = "Crop_and_soil_measurements")
	w <- carobiner::read.excel(f, sheet = "Weather")

	eu$crop <- sub("_.*", "", eu$crop_cultivar)
	eu$variety <- sub("^[^_]+_", "", eu$crop_cultivar)
	eu$plant <- ifelse(eu$System == "Intercropping" & eu$crop == "cowpea", "plant_2", "plant_1")
	eu$year <- as.integer(sub(".*_", "", eu$weather))
	eu$trial_id <- paste(eu$site, eu$year, sep = "_")
	eu$plot_id <- paste(eu$Experimental_unit, eu$crop, sep = "_")

	partners <- split(eu$crop, eu$Experimental_unit)
	eu$intercrops <- mapply(function(id, crop) {
		x <- unique(partners[[as.character(id)]])
		x <- x[x != crop]
		if (length(x) == 0) "none" else paste(x, collapse = ";")
	}, eu$Experimental_unit, eu$crop, USE.NAMES = FALSE)
	eu$intercropped <- eu$System == "Intercropping"

	n_col <- grep("^absolute_value", names(cm), value = TRUE)[1]
	cm$Nrate <- as.numeric(cm[[n_col]])
	n_by <- aggregate(Nrate ~ crop_management, cm, function(x) {
		if (all(is.na(x))) NA else sum(x, na.rm = TRUE)
	})
	one_num <- function(x) {
		x <- as.numeric(x)
		x <- unique(x[!is.na(x)])
		if (length(x) == 1) x else NA
	}
	den <- aggregate(densitesem ~ crop_management, cm, one_num)
	names(den)[2] <- "plant_density"
	den$plant_density <- den$plant_density * 10000
	spc <- aggregate(interrang ~ crop_management, cm, one_num)
	names(spc)[2] <- "row_spacing"
	spc$row_spacing <- spc$row_spacing * 100
	plt <- aggregate(iplt0 ~ crop_management, cm, one_num)
	# Sheet comment: 1 AN, 2 UAN, 3 urea, 4 anhydrous ammonia, 5 ammonium sulfate, 6 ammonium phosphate, 7 calcium nitrate, 8 fixed efficiency.
	eng_map <- c("1" = "AN", "2" = "UAN", "3" = "urea", "4" = "anhydrous ammonia",
		"5" = "ammonium sulfate", "6" = "ammonium phosphate", "7" = "calcium nitrate", "8" = "fixed efficiency")
	cm$ftype <- unname(eng_map[as.character(cm$engrais)])
	ft <- aggregate(ftype ~ crop_management, cm, function(x) {
		x <- unique(x[!is.na(x) & x != ""])
		if (length(x) == 0) NA else paste(x, collapse = ";")
	})
	names(ft)[2] <- "fertilizer_type"
	irr <- aggregate(amount ~ crop_management, cm, function(x) any(!is.na(x) & as.numeric(x) > 0))
	names(irr)[2] <- "irrigated"

	eu <- merge(eu, n_by, by = "crop_management", all.x = TRUE)
	eu <- merge(eu, den, by = "crop_management", all.x = TRUE)
	eu <- merge(eu, spc, by = "crop_management", all.x = TRUE)
	eu <- merge(eu, plt, by = "crop_management", all.x = TRUE)
	eu <- merge(eu, ft, by = "crop_management", all.x = TRUE)
	eu <- merge(eu, irr, by = "crop_management", all.x = TRUE)
	eu <- merge(eu, loc, by = "site", all.x = TRUE)

	meas$mafruit <- as.numeric(meas$mafruit)
	meas$masec_n <- as.numeric(meas$masec_n)
	meas$Date <- as.Date(meas$Date)
	eu$yield <- NA
	eu$dmy_total <- NA
	eu$keep <- FALSE
	for (i in seq_len(nrow(eu))) {
		m <- meas[meas$Experimental_unit == eu$Experimental_unit[i] & meas$Plant == eu$plant[i], ]
		if (nrow(m) == 0) next
		eu$keep[i] <- TRUE
		ok <- which(!is.na(m$mafruit))
		if (length(ok) == 0) next
		best <- ok[m$mafruit[ok] == max(m$mafruit[ok])]
		best <- best[which.max(m$Date[best])]
		eu$yield[i] <- m$mafruit[best] * 1000
		eu$dmy_total[i] <- m$masec_n[best] * 1000
	}
	eu <- eu[eu$keep, ]

	cty <- ifelse(grepl("^Burkina", eu$site), "Burkina Faso",
		ifelse(grepl("^Mali", eu$site), "Mali",
		ifelse(grepl("^Senegal", eu$site), "Senegal", NA)))

	d <- data.frame(
		trial_id = eu$trial_id,
		plot_id = eu$plot_id,
		crop = eu$crop,
		variety = eu$variety,
		treatment = eu$variety,
		country = cty,
		location = sub("^[^_]+_", "", eu$site),
		latitude = eu$latitude,
		longitude = eu$longitude,
		geo_from_source = TRUE,
		station_id = eu$site,
		planting_date = as.character(as.Date(paste(eu$year, eu$iplt0), format = "%Y %j")),
		plant_density = eu$plant_density,
		row_spacing = eu$row_spacing,
		yield = eu$yield,
		yield_part = ifelse(eu$crop == "cowpea", "seed", "grain"),
		dmy_total = eu$dmy_total,
		N_fertilizer = eu$Nrate,
		fertilizer_type = eu$fertilizer_type,
		intercrops = eu$intercrops,
		intercropped = eu$intercropped,
		irrigated = eu$irrigated,
		on_farm = FALSE,
		is_survey = FALSE
	)

	key <- eu[, c("Experimental_unit", "plant", "plot_id")]
	meas <- merge(meas, key, by.x = c("Experimental_unit", "Plant"), by.y = c("Experimental_unit", "plant"))
	long <- data.frame(
		plot_id = meas$plot_id,
		date = as.character(meas$Date),
		plant_height = as.numeric(meas$hauteur) * 100,
		LAI = as.numeric(meas$lai_n),
		grain_biomass_ = meas$mafruit * 1000,
		aboveground_biomass_ = meas$masec_n * 1000
	)
	long <- long[long$plot_id %in% d$plot_id & !is.na(long$date), ]

	wth <- data.frame(
		station_id = w$site,
		date = as.character(as.Date(paste(w$year, w$month, w$day, sep = "-"))),
		tmin = as.numeric(w$minimum_temperature),
		tmax = as.numeric(w$maximum_temperature),
		prec = as.numeric(w$rainfall)
	)
	wth <- wth[wth$station_id %in% d$station_id & !is.na(wth$date), ]

	carobiner::write_files(path, meta, d, long = long, wth = wth)
}
