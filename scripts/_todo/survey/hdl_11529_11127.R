# R script for "carob"
# license: GPL (>=3)

## NOTES
# Mozambique household survey, 2015/16, crop-production module. Geographic coverage is Mozambique. Plot location names in this module are empty, and the file has no coordinates.
# Dried harvest divided by subplot area in hectares is stored as yield. The label does not state the harvest unit. The per-hectare figures sit in a kilograms-per-hectare range, so the harvest was treated as kilograms. yield_isfresh is FALSE.
# Fresh harvest per hectare is not in that range and is not used.
# Maiz is maize. Bean is common bean. Peanut size classes are groundnut. Repolho is cabbage. Potato-reno is potato. Yam / Mandubim is yam. Sweet potato pulp not orange is sweetpotato.
# Yoke beans is kept as recorded.

## ISSUES
# No coordinates and no plot location names. The dried-harvest unit is not stated. Area of 0 leaves yield empty.
# Code -99 is treated as missing. Long and short seasons are stored as long rains and short rains. Couve and repolho are cabbage. Peanut classes are groundnut. Pumpkin leaf is pumpkin. Sugar cane is sugarcane. Paprika is chili pepper. Sweet orange flesh potatoes are sweetpotato. Yoke beans is not in the crop vocabulary.
# write_files: missing variables: latitude, longitude. out of bounds: yield (0, 385600). invalid terms: crop: yoke beans. low yield (tons not kg?): banana, cabbage, cassava, chili pepper, ginger, lettuce, pumpkin, sugarcane, yam. duplicates: duplicate records detected. NA detected: yield, yield_part.

## NOT PROCESSED
# Labour, costs, credit, herbicide and pesticide amounts, and urea and manure quantities. The amount columns do not state a unit.
# Stress scores, tenure, and soil fertility classes.
# The other questionnaire modules (income, food consumption, livestock, extension, and adoption of practices).


carob_script <- function(path) {

"
Pathways to sustainable intensification in Eastern and Southern Africa - Mozambique 2015

The Adoption Pathways project was part of a portfolio of projects that has contributed to the broader theme of sustainable intensification research led by the International Maize and Wheat Improvement Center (CIMMYT). The project was undertaken in Ethiopia, Kenya, Malawi, Mozambique and Tanzania. This file is the Mozambique 2015 crop-production module.
"

	uri <- "hdl:11529/11127"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=4,
		data_organization = "CIMMYT",
		publication = NA,
		project = "Adoption Pathways",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 35,
		carob_effort = 0.5
	)

	f <- ff[grepl("CROP PRODUCTION", basename(ff)) & grepl("\\.sav$", basename(ff))]
	r <- haven::read_sav(f)
	txt <- function(x) {
		z <- trimws(as.character(haven::as_factor(x)))
		z[z %in% c("", "NA", "-99")] <- NA_character_
		z
	}
	num <- function(x) {
		z <- haven::zap_labels(x)
		z <- trimws(as.character(z))
		z[z %in% c("", "NA", "-99", ".")] <- NA_character_
		as.numeric(z)
	}
	known <- c(
		"maiz"="maize", "maize"="maize", "bean"="common bean",
		"tobacco"="tobacco", "soybean"="soybean", "cowpea"="cowpea",
		"tomate"="tomato", "tomato"="tomato",
		"little peanut"="groundnut", "large peanut"="groundnut", "peanut"="groundnut",
		"other ground nuts"="groundnut",
		"cabbage"="cabbage", "repolho-cabbage"="cabbage", "couve-cabbage"="cabbage",
		"sorghum"="sorghum", "potato-reno"="potato", "onion"="onion",
		"sesame"="sesame", "garlic"="garlic", "yam /mandubim"="yam",
		"rice"="rice", "wheat"="wheat", "yoke beans"="yoke beans",
		"banana"="banana", "pea"="pea", "squash"="squash",
		"sweet potato pulp not orange"="sweetpotato",
		"sweet orange flesh potatoes"="sweetpotato",
		"cassava"="cassava", "cotton"="cotton", "millet"="millet",
		"ginger"="ginger", "lettuce"="lettuce", "pumpkin leaf"="pumpkin",
		"sugar cane"="sugarcane", "paprika"="chili pepper"
	)
	part_of <- c(
		maize="grain", "common bean"="seed", tobacco="leaves", soybean="seed",
		cowpea="seed", tomato="fruit", groundnut="seed", cabbage=NA_character_,
		sorghum="grain", potato="tubers", onion=NA_character_, sesame="seed",
		garlic="tubers", yam="tubers", rice="grain", wheat="seed",
		"yoke beans"=NA_character_, banana="fruit", pea="seed", squash=NA_character_,
		sweetpotato="roots", cassava="roots", cotton=NA_character_, millet="grain",
		ginger="tubers", lettuce=NA_character_, pumpkin="leaves",
		sugarcane="stems", "chili pepper"="fruit"
	)
	map_crop <- function(x) {
		key <- tolower(x)
		out <- unname(known[key])
		miss <- is.na(out) & !is.na(key)
		out[miss] <- key[miss]
		out
	}

	hh <- trimws(as.character(haven::zap_labels(r$HHID1)))
	area <- num(r$c105)
	season <- txt(r$c101)
	season[season == "long"] <- "long rains"
	season[season == "short"] <- "short rains"
	plot <- txt(r$c102)
	subp <- txt(r$c103)

	slot <- function(crop, variety, dry, slot_id) {
		cr <- map_crop(txt(crop))
		dy <- num(dry)
		ok <- !is.na(cr)
		y <- rep(NA_real_, length(cr))
		use <- ok & !is.na(area) & area > 0 & !is.na(dy)
		y[use] <- dy[use] / area[use]
		data.frame(
			country = "Mozambique",
			hhid = hh[ok],
			trial_id = paste(hh[ok], plot[ok], subp[ok], cr[ok], slot_id),
			plot_id = paste(plot[ok], subp[ok]),
			season = season[ok],
			field_size = area[ok],
			crop = cr[ok],
			variety = txt(variety)[ok],
			yield = y[ok],
			yield_part = unname(part_of[cr[ok]]),
			yield_isfresh = FALSE,
			is_survey = TRUE,
			on_farm = TRUE,
			geo_from_source = FALSE
		)
	}

	d <- rbind(
		slot(r$c111a, r$c112a, r$c159a, 1),
		slot(r$c111b, r$c112b, r$c159b, 2),
		slot(r$c111c, r$c112c, r$c159c, 3)
	)
	carobiner::write_files(path, meta, d)
}
