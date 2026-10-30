# R script for "carob"
# license: GPL (>=3)

## NOTES
# CIMMYT / IRRI processed household survey, northern Bangladesh (Dinajpur, Nilphamari, Rangpur), collected December 2022-June 2023. One row per household in bd-north-bangladesh-houshold-processed-survey-2023-cimmyt.csv (n=656).
# Reshaped to household x crop x season (aman, boro, aus) where area (ha) or production (kg) is above 0. field_size is the *_ta column (ha). yield is production_kg / area_ha.
# variety_type is the codebook index: 1 hybrid, 0.5 modern variety, 0.1 traditional variety. There is no variety name.
# OM_used is household-level sys_manure (0/1), repeated on every crop row. It is not a per-crop rate.
# adm1 is the division that contains the corrected district (distric_cor). The file's region column is the study region North, kept as study_region_.
# Metadata cites Jahanara et al. (2024), Characterizing farm households in Northern Bangladesh through a quantitative farming systems typology, CIMMYT-CGIAR. That is not a DOI, so publication is NA.

## ISSUES
# The file's own yield columns (*_yield, Mg/ha) do not match kg/ha (aman rice: correlation about 0.45, median absolute difference about 4.7 Mg/ha). yield is kg/area. The file's value is kept as yield_mg_ha_ where the column exists (it is missing for most vegetables).
# Some yields are far above a plausible kg/ha (aman rice up to about 396000; boro potato up to about 1.1e6). Left as recorded. Check area decimals and kg units before analysis.
# No fertilizer product or N/P/K rate. fertilizers_u is a count of fertilizer types, not a rate.
# Irrigation columns are season averages across crops, not this crop, so irrigated was not set.
# aus is not in the season vocabulary. gourd, and yield_part fiber (jute) and bulb (onion), are not in the vocabulary. Potato, garlic, and ginger use yield_part tubers. Nawabganj may need to be Chapai Nawabganj for GADM. Union was not assigned to adm4 because upazila is absent.
# OM_used is household-level, not crop-level. yield is NA where kilograms are missing but area is above 0.
# write_files: missing carob_contributor; invalid crop gourd; invalid season aus; invalid yield_part bulb and fiber; out of bounds yield (up to 1126400); high crop yield for groundnut, maize, potato, rice; NA yield.

## NOT PROCESSED
# Annual totals rice_prod, maize_prod, and the other *_prod / *_surf / *_yi columns (seasonal kg and ha are used instead).
# Fodder area and production, livestock (milk), labor, gender income-control, and food-security columns.
# Season irrigation shares (aman_maincrops_irri, boro_maincrops_irri, aus_maincrops_irri) and fertilizers_u.
# Homestead, orchard, and vegetable-diversity indices that are not a crop area or yield.

## NEW VARIABLES
# variety is not a name; variety_type holds the codebook class
# yield_mg_ha_: the file's yield column where present (Mg/ha); not used as yield
# crop_sold_kg_: kilograms sold, as recorded
# crop_income_thousand_taka_: crop sales income; dictionary unit is "Tk k" (thousands of taka)
# study_region_: study region in the file (North)
# union_: union name
# gps_from_: where the GPS point was taken (hh, field, other)


carob_script <- function(path) {

"
BD North-Bangladesh Household processed survey 2023 CIMMYT

Dataset processed of a household level survey to describe the main farm characteritics, production and resources allocation in North (Dinajpur, Nilphamari, Rangpur), collected on December 2022 -June 2023.
"

	uri <- "doi:10.25502/0apf-2230/d"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA; CIMMYT; IRRI",
		publication = NA,
		project = "Transforming Agrifood Systems in South Asia",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 1.2
	)

	f <- ff[basename(ff) == "bd-north-bangladesh-houshold-processed-survey-2023-cimmyt.csv"]
	r <- read.csv(f, check.names=FALSE, stringsAsFactors=FALSE)

	place <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA")] <- NA_character_
		carobiner::fix_name(x, "title")
	}

	vtype <- function(x) {
		x <- as.numeric(x)
		out <- rep(NA_character_, length(x))
		out[abs(x - 1) < 1e-6] <- "hybrid"
		out[abs(x - 0.5) < 1e-6] <- "modern variety"
		out[abs(x - 0.1) < 1e-6] <- "traditional variety"
		other <- !is.na(x) & is.na(out)
		out[other] <- as.character(x[other])
		out
	}

	spec <- data.frame(
		stem = c("rice", "maize", "jute", "wheat", "lentils", "mungbean", "tomato", "potato", "onion", "garlic", "tumeric", "ginger", "cabbage", "coriander", "chili", "mustard", "groundnut", "sweetpotato", "sunflower", "cumin", "gourds"),
		crop = c("rice", "maize", "jute", "wheat", "lentil", "mung bean", "tomato", "potato", "onion", "garlic", "turmeric", "ginger", "cabbage", "coriander", "chili pepper", "mustard", "groundnut", "sweetpotato", "sunflower", "cumin", "gourd"),
		yield_part = c("grain", "grain", "fiber", "grain", "seed", "seed", "fruit", "tubers", "bulb", "tubers", "rhizome", "tubers", "leaves", "leaves", "fruit", "seed", "seed", "roots", "seed", "seed", "fruit"),
		stringsAsFactors = FALSE
	)

	kg_stem <- function(stem) if (stem == "lentils") "lentil" else stem
	ta_col <- function(s, stem) if (s == "aman" && stem == "sunflower") "aman_sunfloer_ta" else paste0(s, "_", stem, "_ta")
	inc_col <- function(s, stem) if (stem == "tomato") paste0(s, "_tomaton_income") else paste0(s, "_", kg_stem(stem), "_income")
	vt_col <- function(s, stem) if (stem == "tomato") paste0(s, "_tomate_variety_type") else paste0(s, "_", stem, "_variety_type")
	y_col <- function(s, stem) paste0(s, "_", if (stem == "jute") "yute" else stem, "_yield")

	div <- c(Dinajpur="Rangpur", Nilphamari="Rangpur", Rangpur="Rangpur", Nawabganj="Rajshahi", Rajshahi="Rajshahi")
	adm2 <- place(r$distric_cor)
	adm1 <- unname(div[adm2])
	om <- rep(NA, nrow(r))
	manure <- as.numeric(r$sys_manure)
	om[!is.na(manure) & manure == 1] <- TRUE
	om[!is.na(manure) & manure == 0] <- FALSE

	loc <- place(r$village)
	union <- place(r$union)
	loc[is.na(loc)] <- union[is.na(loc)]
	gps <- trimws(tolower(as.character(r$geoplace)))
	gps[gps == ""] <- NA_character_

	parts <- list()
	for (s in c("aman", "boro", "aus")) {
		for (i in seq_len(nrow(spec))) {
			stem <- spec$stem[i]
			area <- as.numeric(r[[ta_col(s, stem)]])
			prod <- as.numeric(r[[paste0(s, "_", kg_stem(stem), "_kg")]])
			keep <- (!is.na(area) & area > 0) | (!is.na(prod) & prod > 0)
			if (!any(keep)) next
			yld <- rep(NA_real_, length(area))
			ok <- !is.na(area) & area > 0 & !is.na(prod)
			yld[ok] <- prod[ok] / area[ok]
			yc <- y_col(s, stem)
			ymg <- if (yc %in% names(r)) as.numeric(r[[yc]]) else rep(NA_real_, nrow(r))
			parts[[length(parts) + 1]] <- data.frame(
				hhid = as.character(r$ID)[keep],
				country = "Bangladesh",
				adm1 = adm1[keep],
				adm2 = adm2[keep],
				location = loc[keep],
				union_ = union[keep],
				study_region_ = place(r$region)[keep],
				latitude = as.numeric(r$lat)[keep],
				longitude = as.numeric(r$long)[keep],
				gps_from_ = gps[keep],
				crop = spec$crop[i],
				season = s,
				variety_type = vtype(r[[vt_col(s, stem)]])[keep],
				field_size = area[keep],
				yield = yld[keep],
				yield_part = spec$yield_part[i],
				yield_mg_ha_ = ymg[keep],
				crop_sold_kg_ = as.numeric(r[[paste0(s, "_", kg_stem(stem), "_sell")]])[keep],
				crop_income_thousand_taka_ = as.numeric(r[[inc_col(s, stem)]])[keep],
				OM_used = om[keep],
				stringsAsFactors = FALSE
			)
		}
	}

	d <- do.call(rbind, parts)
	rownames(d) <- NULL
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$geo_from_source <- TRUE
	d$trial_id <- as.character(seq_len(nrow(d)))

	carobiner::write_files(path, meta, d)
}
