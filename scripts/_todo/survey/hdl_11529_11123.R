# R script for "carob"
# license: GPL (>=3)

## NOTES
# Ethiopia 2015 household survey, plot-level crop production module. Geographic coverage is Ethiopia. Plot location names are blank in this blurred file, and there are no coordinates.
# Dried and fresh harvest are recorded in a local unit. The unit is not named, so they are not converted to kilograms per hectare.
# Belg is stored as short rains and Meher as long rains.
# Bread wheat and durum wheat are wheat. Desi and kabuli chickpea are chickpea. The three teff colors are teff. Haricot bean is common bean. Irish potato is potato. Field pea is pea. Chat is khat. Gesho is gesho. Niger seed is kept as niger. The vocabulary entry has a trailing space, so the trimmed name is reported as invalid. Pepper is chili pepper. Gralic is garlic. Sweet potato is sweetpotato. Sugar cane is sugarcane. Cowpeas is cowpea.
# A subplot area share, when present, scales the subplot area. A share of 0 is treated as missing.

## ISSUES
# No coordinates. Plot location names in this blurred file are blank.
# Cabbage, carrot, and onion have no yield part in the vocabulary.
# Niger is reported as invalid because the vocabulary name has a trailing space.
# write_files: missing variables: latitude, longitude. unknown variables: dry_harvest_local_, fresh_harvest_local_. invalid terms: crop: niger. NA detected: yield_part. contributor:

## NEW VARIABLES
# dry_harvest_local_: dry harvest of that crop on the subplot, in the unnamed local unit.
# fresh_harvest_local_: fresh or green harvest of that crop on the subplot, in the unnamed local unit.

## NOT PROCESSED
# Fallow, grazing land, and eucalyptus rows.
# Fertilizer, manure, seed, herbicide, and pesticide quantities. Urea, DAP, and NPS amounts do not name a unit, and manure is not an elemental nutrient rate.
# Labour, costs, tenure, soil classes, and stress scores.
# The other questionnaire modules.


carob_script <- function(path) {

"
Pathways to sustainable intensification in Eastern and Southern Africa - Ethiopia 2015

The Adoption Pathways project was part of a portfolio of projects that has contributed to the broader theme of sustainable intensification research led by the International Maize and Wheat Improvement Center (CIMMYT) and made possible by the contribution of several teams from national and international research groups brought together by funding from the Australian Centre for International Agricultural Research (ACIAR). The project was undertaken in the five Eastern and Southern African countries of Ethiopia, Kenya, Malawi, Mozambique and Tanzania. This file is the Ethiopia 2015 plot-level crop production module.
"

	uri <- "hdl:11529/11123"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=4,
		data_organization = "CIMMYT",
		publication = NA,
		project = "Adoption Pathways",
		design = NA,
		data_type = "survey",
		treatment_vars = "crop",
		response_vars = "dry_harvest_local_",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.5
	)

	f <- ff[grepl("Plot level crop production", basename(ff)) & grepl("\\.sav$", basename(ff))]
	r <- haven::read_sav(f)

	txt <- function(x) {
		z <- trimws(as.character(haven::as_factor(x)))
		z[z %in% c("", "NA")] <- NA_character_
		z
	}
	num <- function(x) {
		z <- haven::zap_labels(x)
		z <- trimws(as.character(z))
		z[z %in% c("", "NA", "-99", "-88", ".")] <- NA_character_
		as.numeric(z)
	}
	known <- c(
		"banana"="banana", "barley"="barley", "bread wheat"="wheat",
		"cabbage"="cabbage", "carrot"="carrot", "cassava"="cassava",
		"chat (khat/miraa)"="khat", "coffee"="coffee", "cowpeas"="cowpea",
		"desi chickpea"="chickpea", "durum wheat"="wheat", "enset"="enset",
		"faba bean"="faba bean", "field pea(ater)"="pea",
		"finger millet"="finger millet", "ginger"="ginger", "gralic"="garlic",
		"groundnut"="groundnut", "haricot bean"="common bean",
		"hop (gesho)"="gesho", "irish potato"="potato",
		"kabuli chickpea"="chickpea", "lupin"="lupin", "maize"="maize",
		"mango"="mango", "mixed teff"="teff", "nigerseed (nug)"="niger",
		"onion"="onion", "pearl millet"="pearl millet", "pepper"="chili pepper",
		"red teff"="teff", "rice"="rice", "sesame"="sesame", "sorghum"="sorghum",
		"soybean"="soybean", "sugar cane"="sugarcane", "sweet potato"="sweetpotato",
		"tomato"="tomato", "white teff"="teff"
	)
	part_of <- c(
		banana="fruit", barley="grain", wheat="seed", cabbage=NA_character_,
		carrot=NA_character_, cassava="roots", khat="leaves", coffee="seed",
		cowpea="seed", chickpea="seed", enset="rhizome", "faba bean"="seed",
		pea="seed", "finger millet"="grain", ginger="tubers", garlic="tubers",
		groundnut="seed", "common bean"="seed", gesho="leaves", potato="tubers",
		lupin="seed", maize="grain", mango="fruit", teff="grain", niger="seed",
		onion=NA_character_, "pearl millet"="grain", "chili pepper"="fruit",
		rice="grain", sesame="seed", sorghum="grain", soybean="seed",
		sugarcane="stems", sweetpotato="roots", tomato="fruit"
	)
	map_crop <- function(x) {
		key <- tolower(x)
		out <- unname(known[key])
		out[is.na(key)] <- NA_character_
		out
	}

	hh <- trimws(as.character(haven::zap_labels(r$HHLDID)))
	area <- num(r$PLOTSIZEHA)
	season <- txt(r$Season)
	season[season == "Belg"] <- "short rains"
	season[season == "Meher"] <- "long rains"
	plot <- txt(r$M3A2)
	subp <- txt(r$M3A3)
	irr <- txt(r$IRRIGATION2)
	irrigated <- rep(NA, length(irr))
	irrigated[which(irr == "Yes")] <- TRUE
	irrigated[which(irr == "No")] <- FALSE

	slot <- function(crop, other, variety, othvar, share, dry, fresh, slot_id) {
		stated <- txt(crop)
		alt <- txt(other)
		use_alt <- !is.na(stated) & tolower(stated) == "other"
		stated[use_alt] <- alt[use_alt]
		cr <- map_crop(stated)
		ok <- !is.na(cr)
		sh <- num(share)[ok]
		sh[sh == 0] <- NA_real_
		fs <- area[ok]
		got <- !is.na(sh)
		fs[got] <- area[ok][got] * sh[got] / 100
		vy <- txt(variety)
		ov <- txt(othvar)
		vy_alt <- !is.na(vy) & tolower(vy) == "other"
		vy[vy_alt] <- ov[vy_alt]
		data.frame(
			country = "Ethiopia",
			hhid = hh[ok],
			trial_id = paste(hh[ok], plot[ok], subp[ok], slot_id),
			plot_id = paste(plot[ok], subp[ok]),
			season = season[ok],
			field_size = fs,
			crop = cr[ok],
			variety = vy[ok],
			treatment = cr[ok],
			dry_harvest_local_ = num(dry)[ok],
			fresh_harvest_local_ = num(fresh)[ok],
			irrigated = irrigated[ok],
			yield_part = unname(part_of[cr[ok]]),
			is_survey = TRUE,
			on_farm = TRUE,
			geo_from_source = FALSE
		)
	}

	d <- rbind(
		slot(r$CROPGRWN1, r$othCROPGRWN1, r$CROPVAR1, r$othCROPVAR1, r$AREASHARE1, r$AMTDRY1, r$AMTFRESH1, 1),
		slot(r$CROPGRWN2, r$othCROPGRWN2, r$CROPVAR2, r$othCROPVAR2, r$AREASHARE2, r$AMTDRY2, r$AMTFRESH2, 2),
		slot(r$CROPGRWN3, r$othCROPGRWN3, r$CROPVAR3, r$othCROPVAR3, r$AREASHARE3, r$AMTDRY3, r$AMTFRESH3, 3)
	)
	d <- d[!is.na(d$crop), ]
	carobiner::write_files(path, meta, d)
}
