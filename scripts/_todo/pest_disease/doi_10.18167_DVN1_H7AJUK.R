# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. The Annee row in Facteurs is an Excel auto-fill sequence (1999-2050) rather
#    than the survey year. Real observation dates come from the "date" row: 50
#    surveys in Aug-Sep 1999 and 2 surveys (CAM-MAR-37, CAM-MAR-38) in Oct-Nov
#    2000.
# 2. Lieu has "Sjapai" once alongside village code "Dj" (Djapai in Meta Donnees);
#    corrected to "Djapai".
# 3. Floristique uses code STRLU for 3 plots (CAM-MAR-17, -18, -36), while
#    "Donnee originales" labels the exact same 3 plots STRAS ("Striga asiatica
#    (L.) Kuntze", synonym Striga lutea); mapped accordingly.
# 4. No coordinates in the source; the 6 surveyed villages in the Maroua area
#    (Far North Cameroon) were georeferenced with 5 km uncertainty.

## NEW
# bunds_: field water-retention bunding (none, bunds, paddy bunds / casiers; from diguettes)
# field_age_years_: years of cultivation class (from an.culture)

## MISSING
# yield, yield_moisture, yield_part, K_fertilizer, N_fertilizer, P_fertilizer,
# planting_date, harvest_date (not collected in this weed survey)

## NOTES
# 52 phytoecological weed surveys in flood-recession sorghum (muskwari) on karal
# vertisols across 6 villages near Maroua, Far North Cameroon (1999-2000).
# Plot-level factors are in d (wide) and all 64 weed species mapped to full
# botanical names from "Donnee originales" are in long.

## NOT PROCESSED
# Annee (corrupted 1999-2050 sequence), Type_culture (constant "vivrier"),
# Climat (constant "tropical-sec"), village / code_karal / code_simple_karal /
# an.culture2 / code date (redundant short codes), and free-text inondation
# notes (summarized by Irrigation and inondation2).
# Meta Donnees sheet and exported .txt files (FLO, FAC, PA-FLO) duplicate the
# Facteurs and Floristique sheets.


carob_script <- function(path) {

"
Weed surveys in muskwari sorghum in Northern Cameroon (1999)

Study of the weed flora of flood sorghum (Muskwari) in the karal soil areas in Northern Cameroon, within the framework of the PRASAC project. Abundance index rating 1-9. This dataset covers 52 plant surveys for 64 weed species.
"

	uri <- "doi:10.18167/DVN1/H7AJUK"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "CIRAD",
		publication = NA,
		project = "PRASAC",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "weed_cover;weed_species;weed_species_cover",
		notes = NA,
		carob_contributor = NA,
		carob_LLM = "Gemini 3.1 Pro",
		carob_date = "2026-11-26",
		carob_completion = 90,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "CAM-MAR-1999-MSK-AD.xlsx"]
	r1 <- carobiner::read.excel(f, sheet = "Facteurs")
	r2 <- carobiner::read.excel(f, sheet = "Floristique")
	r3 <- carobiner::read.excel(f, sheet = "Donn\u00e9e originales", col_names = FALSE)

	get_row <- function(label) trimws(as.character(unname(unlist(r1[r1[[1]] == label, -1]))))

	# 1-9 abundance scale -> % cover (CAM-MAR-1999-MSK-AD-DOC.txt)
	cover <- c(1, 7, 15, 30, 50, 70, 85, 93, 100)

	loc <- get_row("Lieu")
	loc[loc == "Sjapai"] <- "Djapai"

	pos_map <- c(s = "upper slope", m = "middle slope", b = "lower slope")
	bund_map <- c(n = "none", o = "bunds", p = "paddy bunds")
	irr <- tolower(get_row("Irrigation"))
	inond2 <- tolower(get_row("inondation2"))
	wsol <- tolower(get_row("Wsol"))
	enh <- as.integer(as.numeric(get_row("Enherbement")))

	d <- data.frame(
		trial_id = names(r1)[-1],
		plot_id = as.character(as.integer(as.numeric(get_row("parcelle")))),
		date = as.character(as.Date(as.numeric(get_row("date")), origin = "1899-12-30")),
		country = "Cameroon",
		adm1 = "Extr\u00eame-Nord",
		location = loc,
		crop = "sorghum",
		soil_type = get_row("type_karal"),
		landscape_position = unname(pos_map[tolower(get_row("position"))]),
		land_prep_method = ifelse(wsol == "o", "tillage", ifelse(wsol == "n", "none", NA_character_)),
		bunds_ = unname(bund_map[tolower(get_row("diguettes"))]),
		field_age_years_ = get_row("an.culture"),
		weed_cover = cover[match(enh, 1:9)],
		irrigated = irr %in% c("inonde", "inond\u00e9"),
		irrigation_method = ifelse(irr %in% c("inonde", "inond\u00e9"), "flood", "none"),
		flooded = inond2 == "o",
		flood = inond2 %in% c("o", "no"),
		stress = "weeds",
		intercropped = FALSE,
		on_farm = TRUE,
		is_survey = TRUE
	)

	geo <- data.frame(
		location = c("Balaza", "Diguir", "Dargala", "Gawel", "Djapai", "Salak"),
		latitude = c(10.6923, 10.7183, 10.5273, 10.4101, 10.4333, 10.4558),
		longitude = c(14.4752, 14.5035, 14.6034, 14.0999, 14.3167, 14.2541),
		geo_uncertainty = 5000,
		geo_source = "Village center (Maroua area, Far North Cameroon)",
		geo_from_source = FALSE
	)
	d <- merge(d, geo, by = "location", all.x = TRUE, sort = FALSE)

	# Species lookup from Donnee originales (col 2 = code, col 1 = botanical name)
	i <- which(!is.na(r3[[2]]) & !is.na(r3[[1]]))
	spp <- data.frame(
		code = trimws(as.character(r3[[2]][i])),
		weed_species = trimws(as.character(r3[[1]][i]))
	)
	spp$code[spp$code == "STRAS"] <- "STRLU"

	score <- as.matrix(r2[, -1])
	j <- which(!is.na(score) & score > 0, arr.ind = TRUE)
	long <- data.frame(
		trial_id = colnames(score)[j[, "col"]],
		code = trimws(as.character(r2[[1]][j[, "row"]])),
		weed_species_cover = cover[as.integer(score[j])]
	)
	long <- merge(long, spp, by = "code", all.x = TRUE, sort = FALSE)
	long$code <- NULL

	carobiner::write_files(path, meta, d, long = long)
}
