# R script for "carob"
# license: GPL (>=3)

## NOTES
# 69 vegetable-plot surveys in Mauritius (2020). One row per species with a
# rating > 0. Floristique and Enherbement are the DOC 1-9 notes. weed_species
# is the 5-letter code. lalo is okra, giromon is pumpkin, songe is taro,
# roquette is rocket, and butturnut is winter squash. "serre" and "fin de culture"
# are not part of the crop name.

## ISSUES
# Hedges, ditches, bare or future plots, brède, fruitier, and Voeme are left
# as written. No coordinates. write_files: missing variables: yield,
# yield_moisture, yield_part, latitude, longitude, K_fertilizer, N_fertilizer,
# P_fertilizer, planting_date, harvest_date. invalid terms: crop: A venir,
# A venir cotonmili, A venir courgette, Bord de ravine, Brède, Champ non travaillé,
# Champ travaillé, Désherbant, Entre deux serres, Fossé, Fruitier,
# Future p arcelle SMARTinoccupée, Haie, Haie cultivée, Haie et fossé,
# Haie fruitière, Haie proche ravine, Le long du chemin, Non cultivé,
# Salade, brède, Serre vide butturnut à venir, Voeme.


## NOT PROCESSED
# Facteurs not mapped: n°, Exploitation, Type_culture (all maraichage),
# bordure/culture (b = border, c = crop), Altitude (m), Climat.
# Données origine, Méta Données, FLO, FAC, and PA repeat the workbook or the DOC.
# MAU-PAY-2020-MAR-AD-FLO.txt: not read. columns: "MAU-PAY-01" "MAU-PAY-02" "MAU-PAY-03" "MAU-PAY-04" "MAU-PAY-05" "MAU-PAY-06" "MAU-PAY-07" "MAU-PAY-08" "MAU-PAY-09" "MAU-PAY-10" "MAU-PAY-11" "MAU-PAY-12" "MAU-PAY-13" "MAU-PAY-14" "MAU-PAY-15" "MAU-PAY-16" "MAU-PAY-17" "MAU-PAY-18" "MAU-PAY-19" "MAU-PAY-20" "MAU-PAY-21" "MAU-PAY-22" "MAU-PAY-23" "MAU-PAY-24" "MAU-PAY-25" "MAU-PAY-26" "MAU-PAY-27" "MAU-PAY-28" "MAU-PAY-29" "MAU-PAY-30" "MAU-PAY-31" "MAU-PAY-32" "MAU-PAY-33" "MAU-PAY-34" "MAU-PAY-35" "MAU-PAY-36" "MAU-PAY-37" "MAU-PAY-38" "MAU-PAY-39" "MAU-PAY-40" "MAU-PAY-41" "MAU-PAY-42" "MAU-PAY-43" "MAU-PAY-44" "MAU-PAY-45" "MAU-PAY-46" "MAU-PAY-47" "MAU-PAY-48" "MAU-PAY-49" "MAU-PAY-50" "MAU-PAY-51" "MAU-PAY-52" "MAU-PAY-53" "MAU-PAY-54" "MAU-PAY-55" "MAU-PAY-56" "MAU-PAY-57" "MAU-PAY-58" "MAU-PAY-59" "MAU-PAY-60" "MAU-PAY-61" "MAU-PAY-62" "MAU-PAY-63" "MAU-PAY-64" "MAU-PAY-65" "MAU-PAY-66" "MAU-PAY-67" "MAU-PAY-68" "MAU-PAY-69". Observation records are missing from d until this file is standardized.
# MAU-PAY-2020-MAR-FAC.txt: not read. columns: "MAU-PAY-01" "MAU-PAY-02" "MAU-PAY-03" "MAU-PAY-04" "MAU-PAY-05" "MAU-PAY-06" "MAU-PAY-07" "MAU-PAY-08" "MAU-PAY-09" "MAU-PAY-10" "MAU-PAY-11" "MAU-PAY-12" "MAU-PAY-13" "MAU-PAY-14" "MAU-PAY-15" "MAU-PAY-16" "MAU-PAY-17" "MAU-PAY-18" "MAU-PAY-19" "MAU-PAY-20" "MAU-PAY-21" "MAU-PAY-22" "MAU-PAY-23" "MAU-PAY-24" "MAU-PAY-25" "MAU-PAY-26" "MAU-PAY-27" "MAU-PAY-28" "MAU-PAY-29" "MAU-PAY-30" "MAU-PAY-31" "MAU-PAY-32" "MAU-PAY-33" "MAU-PAY-34" "MAU-PAY-35" "MAU-PAY-36" "MAU-PAY-37" "MAU-PAY-38" "MAU-PAY-39" "MAU-PAY-40" "MAU-PAY-41" "MAU-PAY-42" "MAU-PAY-43" "MAU-PAY-44" "MAU-PAY-45" "MAU-PAY-46" "MAU-PAY-47" "MAU-PAY-48" "MAU-PAY-49" "MAU-PAY-50" "MAU-PAY-51" "MAU-PAY-52" "MAU-PAY-53" "MAU-PAY-54" "MAU-PAY-55" "MAU-PAY-56" "MAU-PAY-57" "MAU-PAY-58" "MAU-PAY-59" "MAU-PAY-60" "MAU-PAY-61" "MAU-PAY-62" "MAU-PAY-63" "MAU-PAY-64" "MAU-PAY-65" "MAU-PAY-66" "MAU-PAY-67" "MAU-PAY-68" "MAU-PAY-69". Observation records are missing from d until this file is standardized.
# MAU-PAY-2020-MAR-PA-FLO.txt: not read. columns: "MAU-PAY-01" "MAU-PAY-02" "MAU-PAY-03" "MAU-PAY-04" "MAU-PAY-05" "MAU-PAY-06" "MAU-PAY-07" "MAU-PAY-08" "MAU-PAY-09" "MAU-PAY-10" "MAU-PAY-11" "MAU-PAY-12" "MAU-PAY-13" "MAU-PAY-14" "MAU-PAY-15" "MAU-PAY-16" "MAU-PAY-17" "MAU-PAY-18" "MAU-PAY-19" "MAU-PAY-20" "MAU-PAY-21" "MAU-PAY-22" "MAU-PAY-23" "MAU-PAY-24" "MAU-PAY-25" "MAU-PAY-26" "MAU-PAY-27" "MAU-PAY-28" "MAU-PAY-29" "MAU-PAY-30" "MAU-PAY-31" "MAU-PAY-32" "MAU-PAY-33" "MAU-PAY-34" "MAU-PAY-35" "MAU-PAY-36" "MAU-PAY-37" "MAU-PAY-38" "MAU-PAY-39" "MAU-PAY-40" "MAU-PAY-41" "MAU-PAY-42" "MAU-PAY-43" "MAU-PAY-44" "MAU-PAY-45" "MAU-PAY-46" "MAU-PAY-47" "MAU-PAY-48" "MAU-PAY-49" "MAU-PAY-50" "MAU-PAY-51" "MAU-PAY-52" "MAU-PAY-53" "MAU-PAY-54" "MAU-PAY-55" "MAU-PAY-56" "MAU-PAY-57" "MAU-PAY-58" "MAU-PAY-59" "MAU-PAY-60" "MAU-PAY-61" "MAU-PAY-62" "MAU-PAY-63" "MAU-PAY-64" "MAU-PAY-65" "MAU-PAY-66" "MAU-PAY-67" "MAU-PAY-68" "MAU-PAY-69". Observation records are missing from d until this file is standardized.


carob_script <- function(path) {

"
Weed survey in vegetable growing in Mauritius (2020)

Characterization of weed communities of vegetable crops in Mauritius in 2019 within the framework of the Smart Agriculture Project. 
Abundance scale notation 1-9. This dataset covers 69 plant surveys for 131 weed species.
"

	uri <- "doi:10.18167/DVN1/MWLOQ0"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = NA,
		project = "Smart Agriculture Project",
		design = "weed abundance survey",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "weed_species;ground_cover;weed_cover",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "MAU-PAY-2020-MAR-AD.xlsx"]
	r1 <- carobiner::read.excel(f, sheet="Facteurs")
	r2 <- carobiner::read.excel(f, sheet="Floristique")

	get_row <- function(label) unname(unlist(r1[r1[[1]] == label, -1]))

	# MAU-PAY-2020-MAR-AD-DOC.txt: 1-9 note to % cover, same table as Benin 2013.
	rating_to_cover <- c(1, 7, 15, 30, 50, 70, 85, 93, 100)

	known <- c(
		"Calebasse" = "bottle gourd",
		"Carotte" = "carrot",
		"Carotte fin de culture" = "carrot",
		"Choux" = "cabbage",
		"Choux fin de culture" = "cabbage",
		"Giromon" = "pumpkin",
		"Haricot, carotte" = "common bean_carrot",
		"Aubergine lalo manioc" = "eggplant_okra_cassava",
		"Lalo manioc piment" = "okra_cassava_chili pepper",
		"Lalo, aubergine" = "okra_eggplant",
		"Maïs" = "maize",
		"Manioc cultivé" = "cassava",
		"Papaye" = "papaya",
		"Poireau" = "leek",
		"Roquette" = "rocket",
		"Serre aubergine" = "eggplant",
		"Serre butturnut" = "winter squash",
		"Serre cacahouette" = "groundnut",
		"Serre salade" = "lettuce",
		"Songe maurice" = "taro",
		"Tomate" = "tomato"
	)
	prev_map <- c(
		"calebasse" = "bottle gourd", "canne à sucre" = "sugarcane",
		"carotte" = "carrot", "laitue" = "lettuce", "oignon" = "onion",
		"salade" = "lettuce"
	)

	src <- trimws(as.character(get_row("Culture_origine")))
	crop <- unname(known[src])
	miss <- is.na(crop) & !is.na(src)
	crop[miss] <- src[miss]
	irr <- tolower(trimws(as.character(get_row("Irrigation"))))
	enh <- as.integer(as.numeric(get_row("Enherbement")))
	d1b <- data.frame(
		trial_id = names(r1)[-1],
		crop = crop,
		previous_crop = unname(prev_map[tolower(trimws(as.character(get_row("Précedent"))))]),
		date = as.character(get_row("Année")),
		country = "Mauritius", # geographicCoverage
		location = trimws(as.character(get_row("Lieu"))),
		weed_cover = rating_to_cover[match(enh, 1:9)],
		irrigated = irr %in% c("inonde", "inondé"),
		stress = "weeds"
	)
	d1b$intercropped <- grepl("_|,", d1b$crop)
	d1b$on_farm <- TRUE
	d1b$is_survey <- TRUE
	d1b$geo_from_source <- FALSE

	present <- which(!is.na(r2[, -1]) & r2[, -1] > 0, arr.ind=TRUE)
	scores <- as.integer(as.numeric(r2[, -1][present]))
	d <- data.frame(
		trial_id = names(r2)[-1][present[, "col"]],
		weed_species = trimws(as.character(r2[[1]][present[, "row"]])),
		ground_cover = rating_to_cover[match(scores, 1:9)]
	)
	d <- merge(d, d1b, by="trial_id", all.x=TRUE)

	carobiner::write_files(path, meta, d)
}
