# R script for "carob"
# license: GPL (>=3)

## NOTES
# Soil organic carbon under maca and pasture in the Peruvian high-Andean Puna (Lake Junín / Chinchaycocha basin, Junín department; Rolando et al., 2017, Geoderma 307: 65-72).
# X and Y are UTM Zone 18S (WGS84, EPSG:32718); converted to decimal longitude/latitude (verified: -76.21 to -75.87 E, -11.38 to -10.99 S, Lake Junín basin).
# Data_dictionary.xls prints g C/kg soil for TC, SOC, and SIC, but the actual values (TC 2.69-10.18, SOC 2.17-9.24) are in percent (%), verified both against SOC stock (0-30 cm x 0.96 g/cm3 x 4.36% = 126 Mg C/ha vs median 127.7 Mg C/ha) and against companion dataset doi:10.21223/P3/W2M5KJ where TOC is identical and labelled %.
# SOC stock is Mg C/ha (0-30 cm), converted to g/m2 (* 100) for soil_C_stock. Bulk density is g/cm3 (15-20 cm core).
# Trailing rows 92-93 have blank ID (separator row and mean Bulk density summary row) and are dropped.

## ISSUES
# soil_C_stock on organic-rich high-Andean Puna soils (3858-22833 g/m2) exceeds terminag's current valid_max (1000 g/m2); kept as reported.
# soil_C_inorg contains 0 where carbonates are absent (below terminag valid_min 0.01); kept as reported.
# Bulk density was sampled at 15-20 cm while SOC stock is reported for 0-30 cm; depth_top = 0, depth_bottom = 30.
# write_files: unknown variable case_study_; out of bounds soil_C_stock, soil_C_inorg.

## NOT PROCESSED
# Carbonates (presence dummy) and isotope values (d13SOC and d13C of size fractions).
# Aggregate and particulate fractions (WSA, POC, and C in the 2 mm, 250 um, and 53 um classes).
# Data_dictionary.xls: codebook, not an observation table.

## NEW VARIABLES
# case_study_: maca cropping system or cultivated pasture, as named in the dictionary.


carob_script <- function(path) {

"
Replication data for: Soil organic carbon stocks and fractionation under different land uses in the Peruvian high-Andean Puna
"

	uri <- "doi:10.21223/P3/JJTLCR"
	group <- "soil_samples"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 55,
		carob_effort = 0
	)

	# Inverse Transverse Mercator (WGS84 UTM Zone 18S -> decimal lon/lat)
	utm18s_to_ll <- function(x, y) {
		a <- 6378137.0
		f <- 1 / 298.257223563
		e2 <- 2 * f - f^2
		ep2 <- e2 / (1 - e2)
		k0 <- 0.9996
		lon0 <- -75 * pi / 180
		x <- x - 500000.0
		y <- y - 10000000.0
		M <- y / k0
		mu <- M / (a * (1 - e2 / 4 - 3 * e2^2 / 64 - 5 * e2^3 / 256))
		e1 <- (1 - sqrt(1 - e2)) / (1 + sqrt(1 - e2))
		phi1 <- mu + (3 * e1 / 2 - 27 * e1^3 / 32) * sin(2 * mu) +
			(21 * e1^2 / 16 - 55 * e1^4 / 32) * sin(4 * mu) +
			(151 * e1^3 / 96) * sin(6 * mu) + (1097 * e1^4 / 512) * sin(8 * mu)
		N1 <- a / sqrt(1 - e2 * sin(phi1)^2)
		T1 <- tan(phi1)^2
		C1 <- ep2 * cos(phi1)^2
		R1 <- a * (1 - e2) / (1 - e2 * sin(phi1)^2)^1.5
		D <- x / (N1 * k0)
		lat <- phi1 - (N1 * tan(phi1) / R1) * (D^2 / 2 -
			(5 + 3 * T1 + 10 * C1 - 4 * C1^2 - 9 * ep2) * D^4 / 24 +
			(61 + 90 * T1 + 298 * C1 + 45 * T1^2 - 252 * ep2 - 3 * C1^2) * D^6 / 720)
		lon <- lon0 + (D - (1 + 2 * T1 + C1) * D^3 / 6 +
			(5 - 2 * C1 + 28 * T1 - 3 * C1^2 + 8 * ep2 + 24 * T1^2) * D^5 / 120) / cos(phi1)
		data.frame(longitude = lon * 180 / pi, latitude = lat * 180 / pi)
	}

	f <- ff[basename(ff) == "Database_carbon.csv"]
	r <- read.csv(f, check.names=FALSE)
	# Drop trailing blank row and Excel summary mean row (blank ID)
	r <- r[!is.na(r$ID) & nzchar(trimws(as.character(r$ID))), ]

	ll <- utm18s_to_ll(as.numeric(r$X), as.numeric(r$Y))
	land <- trimws(as.character(r[["Land use"]]))
	land[!nzchar(land)] <- NA_character_
	case <- trimws(as.character(r[["Case Study"]]))
	case[!nzchar(case)] <- NA_character_

	d <- data.frame(
		trial_id = "Puna_SOC",
		sample_id = as.character(r$ID),
		country = "Peru",
		adm1 = "Junín",
		longitude = round(ll$longitude, 5),
		latitude = round(ll$latitude, 5),
		depth_top = 0,
		depth_bottom = 30,
		land_use = land,
		case_study_ = case,
		land_slope = as.numeric(r$Slope),
		ground_cover = as.numeric(r$Cover),
		soil_bd = as.numeric(r[["Bulk density"]]),
		# TC, SOC, and SIC are in % (verified against SOC stock and companion W2M5KJ)
		soil_C_total = as.numeric(r$TC),
		soil_SOC = as.numeric(r$SOC),
		soil_C_inorg = as.numeric(r$SIC),
		# Mg C/ha -> g/m2 (* 100)
		soil_C_stock = as.numeric(r[["SOC stock"]]) * 100,
		soil_MBC = as.numeric(r$MBC),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = TRUE
	)

	carobiner::write_files(path, meta, d)
}
