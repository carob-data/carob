# R script for "carob"
# license: GPL (>=3)

## NOTES
# 544 plot surveys in the Middle West of Vakinankaratra (2015). One row per
# species with a rating > 0. Floristique and Enherbement are the DOC 1-9 notes.
# weed_species is the 5-letter code. pois de terre is bambara groundnut.
# Slash crops are intercrops. muc/mucuna is velvet bean; stylo is stylosanthes.

## ISSUES
# Latitude in the sheet is unsigned and is negated (the site is south of the
# equator). Longitude_habitation and Latitude_habitation are swapped and unused.
# write_files: missing variables: yield, yield_moisture, yield_part,
# K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.


## NOT PROCESSED
# Facteurs not mapped: farmer names and numbers, Commune, parcel codes and area,
# soil colour and texture, residues, densities, varieties, fertilizer and
# herbicide notes, and weeding dates and times. Enherbement_paysans repeats the
# survey in another layout. FLO, FAC, and PA repeat the workbook.


carob_script <- function(path) {

"
Weed survey in upland rice cropping system in Madagascar (2015)

Characterization of weed communities in rain-fed rice-based cropping systems in the Middle West area of ​​Vakinankaratra in Madagascar. Abundance scale notation 1-9. Intership within DP SPAD (FOFIFA - Cirad). This dataset covers 544 plant surveys for 79 weed species.
"

	uri <- "doi:10.18167/DVN1/NQYPEY"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=0,
		data_organization = "CIRAD",
		publication = NA,
		project = "DP SPAD projet STRADIV",
		design = "weed abundance survey",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "weed_species;ground_cover;weed_cover",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 65,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "MAD-RAK-2015-RIZ-AD.xlsx"]
	r1b <- carobiner::read.excel(f, sheet="Facteurs")
	r1c <- carobiner::read.excel(f, sheet="Floristique")

	get_row <- function(label) unname(unlist(r1b[r1b[[1]] == label, -1]))

	# MAD-RAK-2015-RIZ-AD-DOC.txt: 1-9 note to % cover, same table as Benin 2013.
	rating_to_cover <- c(1, 7, 15, 30, 50, 70, 85, 93, 100)

	crop_map <- c(
		arachide = "groundnut",
		"arachide/mais" = "groundnut_maize",
		"arachide/manioc" = "groundnut_cassava",
		mais = "maize",
		manioc = "cassava",
		"manioc/mais" = "cassava_maize",
		"niebe/mais" = "cowpea_maize",
		"pois de terre" = "bambara groundnut",
		"pois de terre/mais" = "bambara groundnut_maize",
		"pois de terre/manioc" = "bambara groundnut_cassava",
		riz = "rice",
		"riz/mais" = "rice_maize",
		soja = "soybean",
		"soja/mais" = "soybean_maize"
	)
	tok_map <- c(
		"arachide" = "groundnut", "mais" = "maize", "manioc" = "cassava",
		"niebe" = "cowpea", "riz" = "rice", "soja" = "soybean",
		"pois de terre" = "bambara groundnut", "jachere" = "none",
		"stylo" = "stylosanthes", "muc" = "velvet bean", "mucuna" = "velvet bean",
		"jachere stylo" = "stylosanthes", "stylo jachere" = "stylosanthes"
	)
	prev_src <- as.character(get_row("Precedent"))
	previous_crop <- rep(NA_character_, length(prev_src))
	for (i in seq_along(prev_src)) {
		one <- tolower(trimws(prev_src[i]))
		if (is.na(one) || !nzchar(one)) next
		one <- gsub("\\s*\\+\\s*", "+", one)
		one <- gsub("\\s+", " ", one)
		parts <- trimws(unlist(strsplit(one, "+", fixed=TRUE)))
		mapped <- unname(tok_map[parts])
		if (any(is.na(mapped))) {
			previous_crop[i] <- trimws(prev_src[i])
		} else {
			previous_crop[i] <- paste(unique(mapped), collapse=";")
		}
	}

	irr <- tolower(trimws(as.character(get_row("Irrigation"))))
	enh <- as.integer(as.numeric(get_row("Enherbement")))
	crop <- unname(crop_map[tolower(trimws(as.character(get_row("Culture_origine"))))])
	d1b <- data.frame(
		trial_id = names(r1b)[-1],
		crop = crop,
		previous_crop = previous_crop,
		date = as.character(as.integer(as.numeric(get_row("Annee")))),
		country = "Madagascar", # geographicCoverage
		location = trimws(as.character(get_row("Lieu"))),
		# Latitude is unsigned in the sheet. The site is south of the equator.
		longitude = as.numeric(get_row("Longitude")),
		latitude = -as.numeric(get_row("Latitude")),
		weed_cover = rating_to_cover[match(enh, 1:9)],
		irrigated = irr %in% c("inonde", "inondé"),
		stress = "weeds"
	)
	d1b$intercropped <- grepl("_", d1b$crop)
	d1b$on_farm <- TRUE
	d1b$is_survey <- TRUE
	d1b$geo_from_source <- TRUE

	present <- which(!is.na(r1c[, -1]) & r1c[, -1] > 0, arr.ind=TRUE)
	scores <- as.integer(as.numeric(r1c[, -1][present]))
	d <- data.frame(
		trial_id = names(r1c)[-1][present[, "col"]],
		weed_species = trimws(as.character(r1c[[1]][present[, "row"]])),
		ground_cover = rating_to_cover[match(scores, 1:9)]
	)
	d <- merge(d, d1b, by="trial_id", all.x=TRUE)

	carobiner::write_files(path, meta, d)
}
