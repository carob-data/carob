# R script for "carob"
# license: GPL (>=3)

## NOTES
# Soil fertility under maca, fallow, native grassland, and cultivated pasture in the Junín department, Peru (Lake Junín / Chinchaycocha basin).
# X and Y are UTM Zone 18S (WGS84, EPSG:32718); converted to decimal longitude/latitude (verified: -76.21 to -75.87 E, -11.38 to -10.99 S, Junín).
# TOC is percent -> soil_SOC. TN is percent -> soil_N_total (mg/kg = % * 10000). Bray-1 P is mg/kg -> soil_P (soil_P_method = "Bray1").
# Exchangeable Ca, Mg, K, Na, Al+H, and CEC are cmol(+)/kg. Slope is in degrees -> land_slope; Cover is % -> ground_cover.
# Where Fertilization is "No", OM_used = FALSE, OM_amount = 0, OM_type = "none". "Did not answer" is left NA. Manure rate is Mg/ha/yr -> kg/ha (* 1000).
# Maca yield is the farmer's approximate yield from the previous season, Mg/ha/year, stored as kg/ha in maca_yield_ (maca is not yet in values_crop.csv).

## ISSUES
# Bray-1 P above 200 mg/kg is kept as reported.
# "Sheep manure" is mapped to "animal dung" in OM_type (raw label preserved in manure_type_), since fresh sheep manure is not yet in values_OM.csv.
# write_files: unknown variables manure_type_, maca_yield_; out of bounds soil_P (0.867, 517.219).

## NOT PROCESSED
# NH4-N and NO3-N: 14-day incubation mineralization rates in ug/cm2/day, not soil concentrations.
# Carbonates: presence dummy with no unit.
# D15N, TOC:N ratio, D13SOC, N15.R, WSA, pot-experiment guineagrass biomass (BG/AG/Total biomass, Ntotal).
# Land use years.
# Data_dictionary.xls: codebook, not an observation table.

## NEW VARIABLES
# manure_type_: raw fertilization/manure label (Sheep manure, Guano manure, Chicken manure)
# maca_yield_: approximate maca yield of the previous season, kg/ha (empty where land use is not maca)


carob_script <- function(path) {

"
Replication data for: Land Use Effects on Soil Fertility and Nutrient Cycling in the Peruvian High-Andean Puna Grasslands
"

	uri <- "doi:10.21223/P3/W2M5KJ"
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
		carob_completion = 65,
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

	f <- ff[basename(ff) == "Database_fertility.csv"]
	r <- read.csv(f, check.names=FALSE)

	ll <- utm18s_to_ll(as.numeric(r$X), as.numeric(r$Y))
	qty <- trimws(as.character(r[["Fertilization quantity"]]))
	qty[qty %in% c("Did not answer", "NA", "")] <- NA_character_
	fert <- trimws(as.character(r$Fertilization))
	rate <- as.numeric(qty)
	rate[which(fert == "No")] <- 0
	manure <- trimws(as.character(r[["Fertilization type"]]))
	manure[manure %in% c("No", "NA", "")] <- NA_character_
	om_map <- c("Guano manure" = "guano", "Chicken manure" = "poultry manure", "Sheep manure" = "animal dung")
	om_type <- unname(om_map[manure])
	om_type[which(fert == "No")] <- "none"

	maca <- as.numeric(r$Maca.yield) * 1000
	land <- trimws(as.character(r[["Land use"]]))
	maca[which(land != "Maca cropland")] <- NA_real_

	d <- data.frame(
		trial_id = "Junin_puna_fertility",
		sample_id = as.character(r$ID),
		country = "Peru",
		adm1 = "Junín",
		location = carobiner::fix_name(r$Location, "title"),
		longitude = round(ll$longitude, 5),
		latitude = round(ll$latitude, 5),
		land_use = land,
		land_slope = as.numeric(r$Slope),
		ground_cover = as.numeric(r$Cover),
		OM_used = ifelse(fert == "Yes", TRUE, ifelse(fert == "No", FALSE, NA)),
		OM_type = om_type,
		OM_amount = rate * 1000,
		manure_type_ = manure,
		maca_yield_ = maca,
		soil_pH = as.numeric(r$pH),
		soil_sand = as.numeric(r$Sand),
		soil_silt = as.numeric(r$Silt),
		soil_clay = as.numeric(r$Clay),
		soil_bd = as.numeric(r[["Bulk density"]]),
		soil_SOC = as.numeric(r$TOC),
		soil_N_total = as.numeric(r$TN) * 10000,
		soil_P = as.numeric(r[["Bray1-P"]]),
		soil_P_method = "Bray1",
		soil_CEC = as.numeric(r$CEC),
		soil_Ca_exch = as.numeric(r$Ca),
		soil_Mg_exch = as.numeric(r$Mg),
		soil_K_exch = as.numeric(r$K),
		soil_Na_exch = as.numeric(r$Na),
		soil_ex_acidity = as.numeric(r[["Al and H"]]),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = TRUE
	)

	carobiner::write_files(path, meta, d)
}
