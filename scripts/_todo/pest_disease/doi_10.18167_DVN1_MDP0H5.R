# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. The dataset combines on-farm phytoecological surveys ("suivi phytoeco", 200
#    records) and trial monitoring ("suivi essais", 163 records), recorded in
#    study_type_. Traitement mixes weeding, mulching, and fertilizer codes, and
#    is blank for 21 plots; kept in treatment with data_type = "survey" and
#    treatment_vars = "none".
# 2. Floristique uses 5-letter Amatrop/Bayer codes (no species-name lookup sheet
#    is provided), including one uncertain code "ZANSP?".
# 3. No coordinates in the source; Ban Cuon village (Ngoc Phai commune, Cho Don
#    district, Bac Kan province, Vietnam) was georeferenced with 3 km
#    uncertainty.

## NEW
# study_type_: survey vs experiment monitoring (from Type_suivi)
# observation_stage_: timing/stage of weed observation (from Observation)
# slope_class_: qualitative slope class (Pente1, Pente2, Pente3)
# field_age_years_: cultivation age class (from Age_parcelle)
# previous_land_use_: prior fallow/forest/pasture land cover description (from Precedent)
# mulch_used_: logical indicating whether mulch/paillage was explicitly applied

## MISSING
# yield, yield_moisture, yield_part, K_fertilizer, N_fertilizer, P_fertilizer,
# planting_date, harvest_date (not reported in this weed dataset)

## NOTES
# 363 phytoecological weed surveys in rainfed upland rice and maize on hillsides
# in Ban Cuon Valley, Bac Kan, Northern Vietnam (1999). Plot factors are in d
# (wide) and 1-9 species abundance scores converted to % cover are in long.

## NOT PROCESSED
# Type_culture (redundant with Culture_origine) and Climat (constant
# "subtropical"). Meta Donnees sheet and exported .txt files (FLO, FAC, PA-FLO)
# duplicate the Facteurs and Floristique sheets.


carob_script <- function(path) {

"
Weed survey in upland rice in Northern Viet Nam (1999)

Characterization of weed communities in rain-fed crops on slopes (rice, corn) in the context of the use of cover plants. Northern Viet Nam, Bac Kan Province, Ban Cuon Valley. Study by phytoecological surveys with abundance scale. This dataset covers 363 plant surveys for 113 weed species.
"

	uri <- "doi:10.18167/DVN1/MDP0H5"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=3,
		data_organization = "CIRAD",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "weed_species;weed_species_cover",
		notes = NA,
		carob_contributor = NA,
		carob_LLM = "Gemini 3.1 Pro",
		carob_date = "2026-11-26",
		carob_completion = 90,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "VIE-STE-1999-RIZ-AD.xlsx"]
	r1 <- carobiner::read.excel(f, sheet = "Facteurs")
	r2 <- carobiner::read.excel(f, sheet = "Floristique")

	get_row <- function(label) {
		x <- trimws(as.character(unname(unlist(r1[r1[[1]] == label, -1]))))
		x[x == ""] <- NA_character_
		x
	}

	crop_map <- c(mais = "maize", riz = "rice")
	topo_map <- c(haut = "upper slope", milieu = "middle slope", bas = "lower slope")
	soil_col_map <- c(brun = "brown", jaune = "yellow", rouge = "red")
	suivi_map <- c("suivi phyto\u00e9co" = "survey", "suivi essais" = "experiment")

	trt <- get_row("Traitement")
	trt_low <- tolower(trt)
	irr <- tolower(get_row("Irrigation"))

	d <- data.frame(
		trial_id = names(r1)[-1],
		plot_id = get_row("Parcelle"),
		date = as.character(as.integer(as.numeric(get_row("Annee")))),
		country = "Vietnam",
		adm1 = "B\u1eafc K\u1ea1n",
		adm2 = "Ch\u1ee3 \u0110\u1ed3n",
		adm3 = "Ng\u1ecdc Ph\u00e1i",
		location = get_row("Lieu"),
		latitude = 22.2195,
		longitude = 105.5764,
		geo_uncertainty = 3000,
		geo_source = "Wikipedia (B\u1ea3n Cu\u00f4n, Ng\u1ecdc Ph\u00e1i, Ch\u1ee3 \u0110\u1ed3n, B\u1eafc K\u1ea1n)",
		geo_from_source = FALSE,
		crop = unname(crop_map[tolower(get_row("Culture_origine"))]),
		treatment = ifelse(!is.na(trt_low) & trt_low == "aucun", "none", trt),
		study_type_ = unname(suivi_map[tolower(get_row("Type_suivi"))]),
		observation_stage_ = get_row("Observation"),
		landscape_position = unname(topo_map[tolower(get_row("Toposequence"))]),
		slope_class_ = get_row("Pente"),
		soil_color = unname(soil_col_map[tolower(get_row("Type_sol"))]),
		field_age_years_ = get_row("Age_parcelle"),
		previous_land_use_ = get_row("Precedent"),
		mulch_used_ = ifelse(trt_low %in% c("mulch", "paillage"), TRUE, ifelse(trt_low == "non paillage", FALSE, NA)),
		weeding_done = ifelse(trt_low == "desherb\u00e9", TRUE, ifelse(trt_low == "enherb\u00e9", FALSE, NA)),
		irrigated = irr %in% c("inonde", "inond\u00e9"),
		irrigation_method = ifelse(irr %in% c("inonde", "inond\u00e9"), "flood", "none"),
		stress = "weeds",
		intercropped = FALSE,
		on_farm = TRUE,
		is_survey = TRUE
	)

	# 1-9 abundance scale -> % cover (VIE-STE-1999-RIZ-AD-DOC.txt)
	cover <- c(1, 7, 15, 30, 50, 70, 85, 93, 100)
	score <- as.matrix(r2[, -1])
	j <- which(!is.na(score) & score > 0, arr.ind = TRUE)
	long <- data.frame(
		trial_id = colnames(score)[j[, "col"]],
		weed_species = trimws(as.character(r2[[1]][j[, "row"]])),
		weed_species_cover = cover[as.integer(score[j])]
	)

	carobiner::write_files(path, meta, d, long = long)
}
