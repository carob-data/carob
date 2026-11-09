# R script for "carob"
# license: GPL (>=3)

## NOTES
# SWEF / CSAW baseline, Sofala Province, Mozambique (Buzi, Caia, Machanga). responses.csv has Region, District and GPS as "latitude,longitude,elevation".
# Maize is section-c.-crop-budget.csv (one main plot per household in this file). Area is taken from section-a.-land.csv for the same Response and plot, in acres, times 0.404686. area_maize is not used as hectares because that column has no unit.
# Other crops are section-e.-other-crop.csv. Area_Unit Acres is converted the same way. Millet -> pearl millet, soybeans -> soybean.
# Harvest quantity is converted to kilograms only when the unit is 100Kg (quantity x 100). Maxi bag, mini bag and bowl are not converted. Yield (kg/ha) uses that kilogram figure and the acre-based field size.
# "Wang data" is the variety on the maize rows that were inspected; it is not kept as a variety name.
# District "Busi" is treated as Buzi (the district named in the abstract and in the other rows).
# NPK is recorded as fertilizer_type where applyfert is Yes. Qtyfert_1st has no unit, so elemental N/P/K are not calculated.

## ISSUES
# Maxi bag and mini bag weights are not in the files. Most harvests use Maxi bag, so yield is missing for those rows.
# Confirm area_maize is the same acre figure as section A. They were not cross-checked row by row.
# GPS altitudes include small negative values.
# No publication DOI.
# write_files: missing carob_contributor; unknown fertilizer_amount_, harvest_reported_, harvest_unit_; NA yield, yield_part, latitude, longitude. NCBA CLUSA is named in the abstract and is not a data_organization term, so the organization is IITA.

## NEW VARIABLES
# harvest_reported_: harvest quantity as recorded
# harvest_unit_: harvest unit as recorded
# fertilizer_amount_: Qtyfert_1st for maize (unit not stated)

## NOT PROCESSED
# section-b.-adoption.csv: SAS adoption history.
# section-c columns on seed source, sowing method, herbicide and insecticide cost.
# section-d quantities consumed, sold, stored and given (only the harvest quantity and unit are used).
# section-f.-expenditure.csv, section-g.-networks.csv.
# responses.csv household roster (age, sex, education, occupation), except district, region and GPS.
# section-e prices and costs (soldGHS, Pric_unit, cost_pltmat).


carob_script <- function(path) {

"Baseline Study for SWEF Project in Mozambique

The Sofala Province Water, Energy, and Food Security (SWEF) project and Climate Smart Agriculture and Water Project (CSAW) project are two 4-year – 1 August 2020 to 31 July 2024 – initiatives implemented by the International Institute of Tropical Agriculture (IITA) and the National Cooperative Business Association (NCBA), Cooperative League of the United States of America (CLUSA) in the Sofala province of Mozambique. Funded by the Austrian Development Agency (ADA), the initiatives aim to improve adaptive capacities to climate change, crop and livestock productivity and access to market, clean energy alternatives, safe water and sanitation facilities, and to reduce poverty among smallholder farmers in three districts of Sofala, namely, Búzi, Caia and Machanga.
"

	uri <- "doi:10.25502/fp09-8k31/d"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "SWEF",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = "yield",
		notes = "Implemented with NCBA CLUSA",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.5
	)

	f1 <- ff[basename(ff) == "responses.csv"]
	r1 <- read.csv(f1)
	f2 <- ff[basename(ff) == "section-a.-land.csv"]
	r2 <- read.csv(f2)
	f3 <- ff[basename(ff) == "section-c.-crop-budget.csv"]
	r3 <- read.csv(f3)
	f4 <- ff[basename(ff) == "section-d.-harvest-.csv"]
	r4 <- read.csv(f4)
	f5 <- ff[basename(ff) == "section-e.-other-crop.csv"]
	r5 <- read.csv(f5)

	gps <- strsplit(as.character(r1$GPS), ",", fixed=TRUE)
	lat <- lon <- elev <- rep(NA_real_, length(gps))
	for (i in seq_along(gps)) {
		z <- trimws(gps[[i]])
		if (length(z) < 2 || z[1] == "" || z[2] == "") next
		lat[i] <- as.numeric(z[1])
		lon[i] <- as.numeric(z[2])
		if (length(z) >= 3 && z[3] != "" && grepl("^-?[0-9]+(\\.[0-9]+)?$", z[3])) elev[i] <- as.numeric(z[3])
	}
	elev[!is.na(elev) & (elev < 0 | elev > 500)] <- NA
	dist <- trimws(r1$District)
	dist[tolower(dist) == "busi"] <- "Buzi"
	hh <- data.frame(
		Response = r1$Response,
		country = "Mozambique",
		adm1 = carobiner::fix_name(r1$Region, "title"),
		adm2 = carobiner::fix_name(dist, "title"),
		longitude = lon,
		latitude = lat,
		elevation = elev
	)
	hh <- hh[!is.na(hh$Response), ]
	hh <- hh[!duplicated(hh$Response), ]

	acres_to_ha <- function(value, unit) {
		u <- tolower(trimws(as.character(unit)))
		out <- rep(NA_real_, length(value))
		out[u %in% c("acre", "acres")] <- as.numeric(value[u %in% c("acre", "acres")]) * 0.404686
		out
	}

	r2$field_size <- acres_to_ha(r2$LndSiz_val, r2$LndSiz_unt)
	land_key <- data.frame(
		Response = r2$Response,
		plot = trimws(r2$Plt_farm),
		field_size = r2$field_size
	)
	land_key <- land_key[!duplicated(land_key[, c("Response", "plot")]), ]

	kg_from <- function(qty, unit) {
		u <- trimws(as.character(unit))
		qtxt <- trimws(as.character(qty))
		q <- rep(NA_real_, length(qtxt))
		num <- grepl("^[0-9]+(\\.[0-9]+)?$", qtxt)
		q[num] <- as.numeric(qtxt[num])
		ifelse(u == "100Kg", q * 100, NA_real_)
	}

	r4$plot <- trimws(r4$Plt_farmn)
	r4$harvest_kg <- kg_from(r4$Qty_Valu, r4$Qty_harUnt)
	harv_key <- r4[, c("Response", "plot", "Qty_Valu", "Qty_harUnt", "harvest_kg")]
	names(harv_key)[names(harv_key) == "Qty_Valu"] <- "harvest_reported_"
	names(harv_key)[names(harv_key) == "Qty_harUnt"] <- "harvest_unit_"

	r3$plot <- trimws(r3$mainplot)
	r3$Response <- r3$Response
	d1 <- data.frame(
		Response = r3$Response,
		plot_id = r3$plot,
		crop = "maize",
		yield_part = "grain",
		fertilizer_type = ifelse(tolower(trimws(r3$applyfert)) == "yes", "NPK", NA_character_),
		fertilizer_amount_ = r3$Qtyfert_1st
	)
	d1 <- merge(d1, land_key, by.x=c("Response", "plot_id"), by.y=c("Response", "plot"), all.x=TRUE)
	d1 <- merge(d1, harv_key, by.x=c("Response", "plot_id"), by.y=c("Response", "plot"), all.x=TRUE)

	ocrop <- tolower(trimws(r5$Crop_type))
	ocrop[ocrop == "millet"] <- "pearl millet"
	ocrop[ocrop == "soybeans"] <- "soybean"
	ocrop[ocrop %in% c("beans", "bean")] <- "common bean"
	ocrop[grepl("bambara", ocrop)] <- "bambara groundnut"
	ocrop[ocrop %in% c("", "na")] <- NA
	d2 <- data.frame(
		Response = r5$Response,
		plot_id = trimws(r5$Plt_farmn),
		crop = ocrop,
		field_size = acres_to_ha(r5$Area_Value, r5$Area_Unit),
		harvest_reported_ = r5$Value_Yield,
		harvest_unit_ = trimws(r5$Yield_Unit),
		fertilizer_type = NA_character_,
		fertilizer_amount_ = NA_real_
	)
	d2$harvest_kg <- kg_from(d2$harvest_reported_, d2$harvest_unit_)
	d2$yield_part <- ifelse(d2$crop == "pearl millet", "grain", ifelse(d2$crop %in% c("soybean", "common bean", "bambara groundnut"), "seed", NA_character_))

	d <- rbind(d1, d2)
	d$yield <- ifelse(!is.na(d$field_size) & d$field_size > 0, d$harvest_kg / d$field_size, NA_real_)
	d$harvest_kg <- NULL
	d$hhid <- as.character(d$Response)
	d <- merge(d, hh, by="Response", all.x=TRUE)
	d$Response <- NULL
	d <- d[!is.na(d$crop) & !is.na(d$hhid), ]
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$geo_from_source <- !is.na(d$longitude) & !is.na(d$latitude)
	d$trial_id <- d$hhid

	carobiner::write_files(path, meta, d)
}
