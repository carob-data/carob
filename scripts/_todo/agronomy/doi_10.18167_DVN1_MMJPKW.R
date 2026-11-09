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
# readme.txt: documentation or a codebook, not an observation table.

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
	r1 <- carobiner::read.excel(f, sheet = "Experimental_unit")
	r2 <- carobiner::read.excel(f, sheet = "Site_location")
	r3 <- carobiner::read.excel(f, sheet = "Crop management")
	r4 <- carobiner::read.excel(f, sheet = "Crop_and_soil_measurements")
	r5 <- carobiner::read.excel(f, sheet = "Weather")

	r1$crop <- sub("_.*", "", r1$crop_cultivar)
	r1$variety <- sub("^[^_]+_", "", r1$crop_cultivar)
	r1$plant <- ifelse(r1$System == "Intercropping" & r1$crop == "cowpea", "plant_2", "plant_1")
	r1$year <- as.integer(sub(".*_", "", r1$weather))
	r1$trial_id <- paste(r1$site, r1$year, sep = "_")
	r1$plot_id <- paste(r1$Experimental_unit, r1$crop, sep = "_")

	partners <- split(r1$crop, r1$Experimental_unit)
	r1$intercrops <- mapply(function(id, crop) {
		x <- unique(partners[[as.character(id)]])
		x <- x[x != crop]
		if (length(x) == 0) "none" else paste(x, collapse = ";")
	}, r1$Experimental_unit, r1$crop, USE.NAMES = FALSE)
	r1$intercropped <- r1$System == "Intercropping"

	n_col <- grep("^absolute_value", names(r3), value = TRUE)[1]
	r3$Nrate <- as.numeric(r3[[n_col]])
	n_by <- aggregate(Nrate ~ crop_management, r3, function(x) {
		if (all(is.na(x))) NA else sum(x, na.rm = TRUE)
	})
	one_num <- function(x) {
		x <- as.numeric(x)
		x <- unique(x[!is.na(x)])
		if (length(x) == 1) x else NA
	}
	den <- aggregate(densitesem ~ crop_management, r3, one_num)
	names(den)[2] <- "plant_density"
	den$plant_density <- den$plant_density * 10000
	spc <- aggregate(interrang ~ crop_management, r3, one_num)
	names(spc)[2] <- "row_spacing"
	spc$row_spacing <- spc$row_spacing * 100
	plt <- aggregate(iplt0 ~ crop_management, r3, one_num)
	# Sheet comment: 1 AN, 2 UAN, 3 urea, 4 anhydrous ammonia, 5 ammonium sulfate, 6 ammonium phosphate, 7 calcium nitrate, 8 fixed efficiency.
	eng_map <- c("1" = "AN", "2" = "UAN", "3" = "urea", "4" = "anhydrous ammonia",
		"5" = "ammonium sulfate", "6" = "ammonium phosphate", "7" = "calcium nitrate", "8" = "fixed efficiency")
	r3$ftype <- unname(eng_map[as.character(r3$engrais)])
	ft <- aggregate(ftype ~ crop_management, r3, function(x) {
		x <- unique(x[!is.na(x) & x != ""])
		if (length(x) == 0) NA else paste(x, collapse = ";")
	})
	names(ft)[2] <- "fertilizer_type"
	irr <- aggregate(amount ~ crop_management, r3, function(x) any(!is.na(x) & as.numeric(x) > 0))
	names(irr)[2] <- "irrigated"

	r1 <- merge(r1, n_by, by = "crop_management", all.x = TRUE)
	r1 <- merge(r1, den, by = "crop_management", all.x = TRUE)
	r1 <- merge(r1, spc, by = "crop_management", all.x = TRUE)
	r1 <- merge(r1, plt, by = "crop_management", all.x = TRUE)
	r1 <- merge(r1, ft, by = "crop_management", all.x = TRUE)
	r1 <- merge(r1, irr, by = "crop_management", all.x = TRUE)
	r1 <- merge(r1, r2, by = "site", all.x = TRUE)

	r4$mafruit <- as.numeric(r4$mafruit)
	r4$masec_n <- as.numeric(r4$masec_n)
	r4$Date <- as.Date(r4$Date)
	r1$yield <- NA
	r1$dmy_total <- NA
	r1$keep <- FALSE
	for (i in seq_len(nrow(r1))) {
		m <- r4[r4$Experimental_unit == r1$Experimental_unit[i] & r4$Plant == r1$plant[i], ]
		if (nrow(m) == 0) next
		r1$keep[i] <- TRUE
		ok <- which(!is.na(m$mafruit))
		if (length(ok) == 0) next
		best <- ok[m$mafruit[ok] == max(m$mafruit[ok])]
		best <- best[which.max(m$Date[best])]
		r1$yield[i] <- m$mafruit[best] * 1000
		r1$dmy_total[i] <- m$masec_n[best] * 1000
	}
	r1 <- r1[r1$keep, ]

	cty <- ifelse(grepl("^Burkina", r1$site), "Burkina Faso",
		ifelse(grepl("^Mali", r1$site), "Mali",
		ifelse(grepl("^Senegal", r1$site), "Senegal", NA)))

	d <- data.frame(
		trial_id = r1$trial_id,
		plot_id = r1$plot_id,
		crop = r1$crop,
		variety = r1$variety,
		treatment = r1$variety,
		country = cty,
		location = sub("^[^_]+_", "", r1$site),
		latitude = r1$latitude,
		longitude = r1$longitude,
		geo_from_source = TRUE,
		station_id = r1$site,
		planting_date = as.character(as.Date(paste(r1$year, r1$iplt0), format = "%Y %j")),
		plant_density = r1$plant_density,
		row_spacing = r1$row_spacing,
		yield = r1$yield,
		yield_part = ifelse(r1$crop == "cowpea", "seed", "grain"),
		dmy_total = r1$dmy_total,
		N_fertilizer = r1$Nrate,
		fertilizer_type = r1$fertilizer_type,
		intercrops = r1$intercrops,
		intercropped = r1$intercropped,
		irrigated = r1$irrigated,
		on_farm = FALSE,
		is_survey = FALSE
	)

	key <- r1[, c("Experimental_unit", "plant", "plot_id")]
	r4 <- merge(r4, key, by.x = c("Experimental_unit", "Plant"), by.y = c("Experimental_unit", "plant"))
	long <- data.frame(
		plot_id = r4$plot_id,
		date = as.character(r4$Date),
		plant_height = as.numeric(r4$hauteur) * 100,
		LAI = as.numeric(r4$lai_n),
		grain_biomass_ = r4$mafruit * 1000,
		aboveground_biomass_ = r4$masec_n * 1000
	)
	long <- long[long$plot_id %in% d$plot_id & !is.na(long$date), ]

	wth <- data.frame(
		station_id = r5$site,
		date = as.character(as.Date(paste(r5$year, r5$month, r5$day, sep = "-"))),
		tmin = as.numeric(r5$minimum_temperature),
		tmax = as.numeric(r5$maximum_temperature),
		prec = as.numeric(r5$rainfall)
	)
	wth <- wth[wth$station_id %in% d$station_id & !is.na(wth$date), ]

	carobiner::write_files(path, meta, d, long = long, wth = wth)
}
