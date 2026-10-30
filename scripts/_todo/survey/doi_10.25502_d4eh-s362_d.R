# R script for "carob"
# license: GPL (>=3)

## NOTES
# CSAT baseline, Mali (PAYS is code 1 on every plot row; homeGPS falls in Mali, about 13 N, 8 W). Plot table is plot.csv, one row per plot (Plot1_* columns).
# Plot1_size with plot1_szUnit Hectare is field_size. Ten rows in acres are multiplied by 0.404686.
# REGION, CERCLE, COMMUNE and VILLAGE are numeric codes. They are stored as codes. homeGPS is "latitude,longitude,elevation" (the first number is about 13 and the second about -7 or -8, which is Mali only if that order is lat, lon).
# Millet in this Sahel questionnaire is mapped to pearl millet. Sweet potato -> sweetpotato, Irish potato -> potato.
# plot1_freshHrvst and plot1_dryHrvst, and the NPK / urea / DAP quantity columns, have no unit in the column names. They are kept unconverted. The questionnaire PDF is in the deposit but was not used to assign kilograms.

## ISSUES
# Read csat_iita_baseline_questionnaire_mali_niger.pdf for the units of fresh/dry harvest and fertiliser quantities before computing yield or elemental N/P/K. A 1 ha cowpea plot with dry harvest 240 is plausible as kilograms but that is not stated in the csv.
# Admin codes are unlabeled (region 1-4, cercle such as 301, commune such as 30301). Georeference from the GPS and then attach GADM names.
# "Other, specify" was dropped. "Vegetables" was left as typed.
# field_m.csv has field area and an irrigation flag that was not merged (separate field id; confirm the join to PlotID).
# Niger is named in the abstract; this file's GPS and PAYS code are the Mali sample.
# No publication DOI.
# write_files: missing coordinates where homeGPS is blank, invalid crop "vegetables", missing carob_contributor. Elevation includes negative GPS altitudes.

## NEW VARIABLES
# region_code_, cercle_code_, commune_code_, village_code_: numeric admin codes
# harvest_fresh_: plot1_freshHrvst, unit not confirmed
# harvest_dry_: plot1_dryHrvst, unit not confirmed
# npk_amount_, urea_amount_, dap_amount_, manure_amount_: quantities as recorded, unit not confirmed

## NOT PROCESSED
# general_details_m.csv except homeGPS and the household id used to join it. Roster, income, crops listed at household level (hhCrops_*), training.
# field_m.csv: field size, soil, irrigation.
# adoption_of_improved_practices_m.csv, improved_crop_storage_technology_m.csv, other_improved_practices_m.csv.
# production_consumption_demand.csv, utilization_of_crop_revenue.csv, food_consumption and dietary_diversity files, food_insecurity, shocks, migration, income, assets, livestock, market_access, group_formation, climate and drought perception.
# plot.csv labour counts, wages, severity scores, seed and pesticide cost columns.
# csat_iita_baseline_questionnaire_mali_niger.pdf (see ISSUES).


carob_script <- function(path) {

"Baseline data for the CSAT project-Mali

The West African Sahel and dry savannahs cover a large northern sector of the sub-region from Nigeria through Benin, Niger, Burkina Faso, Mali, to Senegal where natural resource limitations, especially water scarcity and land degradation, are expected to be exacerbated by climate change. The main goal of the proposed project is to introduce climate smart agricultural technologies and innovations in the Sahel and dry savannah regions of Mali and Niger that will improve rural livelihoods and food security. In the first of the two components of the project, we will use innovation platforms (IPs) to scale out exisitng technologies along the value  chain of key commodities. In Niger the delivery mechanisms that the current Norwegian project (RED/SAACC-Niger) is applying will also be adopted. In the second project component  we  will conduct research  to develop and adapt additional technological options to expand the pool from which farmers and other value chain actors get to choose from.
"

	uri <- "doi:10.25502/d4eh-s362/d"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "CSAT",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.5
	)

	r <- read.csv(ff[basename(ff) == "plot.csv"], stringsAsFactors=FALSE)
	g <- read.csv(ff[basename(ff) == "general_details_m.csv"], stringsAsFactors=FALSE)

	crop <- tolower(trimws(r$plot1_cropCultvtd))
	crop[crop == "millet"] <- "pearl millet"
	crop[crop == "sweet potato"] <- "sweetpotato"
	crop[crop == "irish potato"] <- "potato"
	crop[crop %in% c("other, specify", "")] <- NA

	unit <- trimws(r$plot1_szUnit)
	size <- as.numeric(r$Plot1_size)
	size[unit == "Acre"] <- size[unit == "Acre"] * 0.404686
	size[!(unit %in% c("Hectare", "Acre"))] <- NA

	gps_txt <- as.character(g$homeGPS)
	bad_enc <- !is.na(gps_txt) & is.na(iconv(gps_txt, from="UTF-8", to="UTF-8"))
	if (any(bad_enc)) gps_txt[bad_enc] <- iconv(gps_txt[bad_enc], from="latin1", to="UTF-8", sub="")
	gps <- strsplit(gps_txt, ",", fixed=TRUE)
	lat <- lon <- elev <- rep(NA_real_, length(gps))
	isnum <- function(s) grepl("^-?[0-9]+(\\.[0-9]+)?$", s)
	for (i in seq_along(gps)) {
		z <- trimws(gps[[i]])
		if (length(z) < 2 || !isnum(z[1]) || !isnum(z[2])) next
		lat[i] <- as.numeric(z[1])
		lon[i] <- as.numeric(z[2])
		if (length(z) >= 3 && isnum(z[3])) elev[i] <- as.numeric(z[3])
	}
	# keep readings that fall in Mali; the file also has blank, shifted and non-numeric GPS
	bad_xy <- !is.na(lat) & (lat < 10 | lat > 25 | is.na(lon) | lon < -18 | lon > 5)
	lat[bad_xy] <- NA
	lon[bad_xy] <- NA
	loc <- data.frame(
		HOUSEHOLD = g$HOUSEHOLD,
		longitude = lon,
		latitude = lat,
		elevation = elev,
		stringsAsFactors = FALSE
	)
	loc <- loc[!duplicated(loc$HOUSEHOLD), ]

	d <- data.frame(
		hhid = as.character(r$HOUSEHOLD),
		plot_id = as.character(r$PlotID),
		country = "Mali",
		region_code_ = as.character(r$REGION),
		cercle_code_ = as.character(r$CERCLE),
		commune_code_ = as.character(r$COMMUNE),
		village_code_ = as.character(r$VILLAGE),
		crop = crop,
		field_size = size,
		harvest_fresh_ = r$plot1_freshHrvst,
		harvest_dry_ = r$plot1_dryHrvst,
		npk_amount_ = r$plot1_qtyNPK,
		urea_amount_ = r$plot1_qtyUrea,
		dap_amount_ = r$plot1_qtyDAP,
		manure_amount_ = r$plot1_qtyManure,
		stringsAsFactors = FALSE
	)
	d$HOUSEHOLD <- r$HOUSEHOLD
	d <- merge(d, loc, by="HOUSEHOLD", all.x=TRUE)
	d$HOUSEHOLD <- NULL
	d <- d[!is.na(d$crop) & !is.na(d$hhid), ]
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$geo_from_source <- !is.na(d$longitude) & !is.na(d$latitude)
	d$trial_id <- d$hhid

	carobiner::write_files(path, meta, d)
}
