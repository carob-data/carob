# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. One Floristique score (species HPYSU in survey CAM-VAL-0889) is 11, outside
#    the 1-9 abundance scale; weed_species_cover is set to NA for that record.
# 2. Floristique uses 5-letter Amatrop/Bayer codes; no species-name lookup sheet
#    is included in the workbook.
# 3. Facteurs Sol includes code "sc" (247 rows) and "sa" (180 rows) not listed in
#    the Meta donnees legend (which defines as = argilo-sableux, a = argileux,
#    s = sableux). "sa" is mapped to sandy clay loam; "sc" is left as NA in
#    soil_texture and kept as "sc" in soil_type.
# 4. No coordinates in the source; Mafa-Kilda village (Benoue, North Region,
#    Cameroon) was georeferenced from Mapcarta with 5 km uncertainty.

## NEW
# observation_stage_: timing of weed observation relative to tillage/weeding operations (from Epoque 2-7)
# quadrat_id_: quadrat number (1-5) within each plot x weeding modality (from Placette)

## MISSING
# yield, yield_moisture, yield_part, K_fertilizer, N_fertilizer, P_fertilizer,
# planting_date, harvest_date (not reported in this weed experiment dataset)

## NOTES
# On-farm cotton weeding experiment at Mafa-Kilda, Northern Cameroon (2001):
# 22 farmer fields x 2 weeding modalities (donkey-drawn mechanical vs hand
# weeding) x 5 quadrats x 5-7 observation stages (1217 surveys, 71 species).
# Plot-date records are in d (wide) and species % cover in long, linked by
# record_id.

## NOT PROCESSED
# Stagiaire (observer name) and Climat (constant "tropical").
# Meta donnees sheet and exported .txt files (FLO, FAC, PA-FLO) duplicate the
# Facteurs and Floristique sheets.


carob_script <- function(path) {

"
Effect of mechanical weeding compared to hand weeding on cotton weed flora in Northern Cameroon (2001)

Comparative experimental study of the effect of mechanical weeding compared to manual weeding on the weed flora of cotton in Northern Cameroon, within the framework of the project PRASAC. Abundance index rating 1-9. This dataset covers 1217 plant surveys for 71 weed species.
"

	uri <- "doi:10.18167/DVN1/9PDXYC"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
		data_organization = "CIRAD",
		publication = NA,
		project = "PRASAC",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "weeding_method;land_prep_method",
		response_vars = "weed_cover;weed_species;weed_species_cover",
		notes = NA,
		carob_contributor = NA,
		carob_LLM = "Gemini 3.1 Pro",
		carob_date = "2026-11-26",
		carob_completion = 90,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "CAM-VAL-2001-COT-AD.xlsx"]
	r1 <- carobiner::read.excel(f, sheet = "Facteurs")
	r2 <- carobiner::read.excel(f, sheet = "Floristique")

	get_row <- function(label) trimws(as.character(unname(unlist(r1[r1[[1]] == label, -1]))))

	# 1-9 abundance scale -> % cover (CAM-VAL-2001-COT-AD-DOC.txt)
	cover <- c(1, 7, 15, 30, 50, 70, 85, 93, 100)

	prev_map <- c(
		"arachide" = "groundnut",
		"jach\u00e8re" = "none",
		"ma\u00efs" = "maize",
		"ni\u00e9b\u00e9" = "cowpea",
		"sorgho" = "sorghum"
	)
	weed_map <- c(att = "mechanical weeding", man = "hand weeding")
	prep_map <- c(lab = "ploughing", sd = "none")
	epoque_map <- c(
		"1" = "before ploughing",
		"2" = "after ploughing",
		"3" = "before 1st weeding",
		"4" = "after 1st weeding",
		"5" = "before 2nd weeding",
		"6" = "after 2nd weeding",
		"7" = "before ridging",
		"8" = "after ridging"
	)
	sol_tex_map <- c(as = "sandy clay", s = "sand", sa = "sandy clay loam", sc = NA_character_)
	sol_type_map <- c(as = "argilo-sableux", s = "sableux", sa = "sablo-argileux", sc = "sc")

	survey_id <- names(r1)[-1]
	parc <- as.integer(as.numeric(get_row("Parcelle")))
	plac <- as.integer(as.numeric(get_row("Placette")))
	mod <- tolower(get_row("Modalit\u00e9"))
	itk <- tolower(get_row("ITK"))
	sol <- tolower(get_row("Sol"))
	epq <- as.character(as.integer(as.numeric(get_row("Epoque"))))
	enh <- as.integer(as.numeric(get_row("Enherbement")))
	irr <- tolower(get_row("Irrigation"))

	d <- data.frame(
		record_id = seq_along(survey_id),
		trial_id = paste0("Mafa-Kilda_", parc),
		plot_id = paste0("P", parc, "_", mod, "_", plac),
		quadrat_id_ = plac,
		date = as.character(as.Date(as.numeric(get_row("Date")), origin = "1899-12-30")),
		observation_stage_ = unname(epoque_map[epq]),
		country = "Cameroon",
		adm1 = "Nord",
		adm2 = "B\u00e9nou\u00e9",
		location = get_row("Lieu"),
		latitude = 9.1991,
		longitude = 13.4999,
		geo_uncertainty = 5000,
		geo_source = "Mapcarta (Mafa Kilda, B\u00e9nou\u00e9)",
		geo_from_source = FALSE,
		crop = "cotton",
		previous_crop = unname(prev_map[tolower(get_row("Precedent"))]),
		treatment = paste(unname(weed_map[mod]), unname(prep_map[itk]), sep = "; "),
		weeding_done = TRUE,
		weeding_method = unname(weed_map[mod]),
		land_prep_method = unname(prep_map[itk]),
		soil_texture = unname(sol_tex_map[sol]),
		soil_type = unname(sol_type_map[sol]),
		weed_cover = cover[match(enh, 1:9)],
		irrigated = irr %in% c("inonde", "inond\u00e9"),
		irrigation_method = ifelse(irr %in% c("inonde", "inond\u00e9"), "flood", "none"),
		stress = "weeds",
		intercropped = FALSE,
		on_farm = TRUE,
		is_survey = FALSE
	)

	score <- as.matrix(r2[, -1])
	j <- which(!is.na(score) & score > 0, arr.ind = TRUE)
	col_ids <- colnames(score)[j[, "col"]]
	sc_vals <- as.integer(score[j])
	long <- data.frame(
		record_id = match(col_ids, survey_id),
		trial_id = d$trial_id[match(col_ids, survey_id)],
		weed_species = trimws(as.character(r2[[1]][j[, "row"]])),
		weed_species_cover = cover[match(sc_vals, 1:9)]
	)

	carobiner::write_files(path, meta, d, long = long)
}
