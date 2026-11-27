# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Dataset title says 1996, but Date serials and Annee span both 1996
#    (1996-10-23, 1996-11-01) and 1997 (1997-08-01). The DOC.txt text line
#    mentioning "23 septembre - 5 octobre 2018" appears to be a typo and does
#    not match the data dates.
# 2. No coordinates in the source; the three survey regions (Koundara-Gaoual,
#    Kankan, Dabola) were georeferenced to town/midpoint centers with large
#    regional uncertainty (30-50 km).

## NEW
# weed_family_: botanical family of the weed species

## MISSING
# yield, yield_moisture, yield_part, K_fertilizer, N_fertilizer, P_fertilizer,
# planting_date, harvest_date (not collected in this weed survey)

## NOTES
# 110 cotton weed surveys across three cotton-growing regions of Guinea
# (Koundara-Gaoual, Kankan, Dabola) in 1996-1997. Plot-level records are in d
# (wide) and all 158 species mapped to full botanical names and families from
# the "Guinee 1996 1997 Origine" sheet are in long.

## NOT PROCESSED
# jour_de_mission, Observation_du_jour (mission survey sequence numbers),
# Climat (constant "tropical"), and Type_culture (identical to Culture_origine).
# Meta Donnees sheet and exported .txt files (FLO, FAC, PA-FLO) duplicate the
# Facteurs and Floristique sheets.


carob_script <- function(path) {

"
Weed survey in cotton crop in Guinea (1996)

Characterization of weed flora of cotton crop in Guinea in 1996, within the framework of the projects PCGK (Projet Coton Gaoual - Koundara) and PCK (Projet Coton Kankan). Abundance index rating 1-9. This dataset covers 110 plant surveys for 158 weed species.
"

	uri <- "doi:10.18167/DVN1/EVCQFR"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
		data_organization = "CIRAD",
		publication = NA,
		project = "PCGK; PCK",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "weed_cover;weed_species;weed_species_cover",
		notes = NA,
		carob_contributor = NA,
		carob_LLM = "Gemini 3.1 Pro",
		carob_date = "2026-11-26",
		carob_completion = 95,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "GUI-MAR-1996-COT-AD.xlsx"]
	r1 <- carobiner::read.excel(f, sheet = "Facteurs")
	r2 <- carobiner::read.excel(f, sheet = "Floristique")
	r3 <- carobiner::read.excel(f, sheet = " Guin\u00e9e 1996 1997 Origine", col_names = FALSE)

	get_row <- function(label) trimws(as.character(unname(unlist(r1[r1[[1]] == label, -1]))))

	# 1-9 abundance scale -> % cover (GUI-MAR-1996-COT-AD-DOC.txt)
	cover <- c(1, 7, 15, 30, 50, 70, 85, 93, 100)
	stage_map <- c("deb-cycle" = "early cycle", "fin-cycle" = "late cycle")
	irr <- tolower(get_row("Irrigation"))
	enh <- as.integer(as.numeric(get_row("Note_globale")))

	d <- data.frame(
		trial_id = names(r1)[-1],
		date = as.character(as.Date(as.numeric(get_row("Date")), origin = "1899-12-30")),
		country = "Guinea",
		location = get_row("Lieu"),
		crop = "cotton",
		growth_stage = unname(stage_map[tolower(get_row("Epoque/cycle"))]),
		weed_cover = cover[match(enh, 1:9)],
		irrigated = irr %in% c("inonde", "inond\u00e9"),
		irrigation_method = ifelse(irr %in% c("inonde", "inond\u00e9"), "flood", "none"),
		stress = "weeds",
		intercropped = FALSE,
		on_farm = TRUE,
		is_survey = TRUE
	)

	geo <- data.frame(
		location = c("Koundara-Gaoual", "Kankan", "Dabola"),
		adm1 = c("Bok\u00e9", "Kankan", "Faranah"),
		latitude = c(12.1167, 10.3854, 10.7433),
		longitude = c(-13.2500, -9.3057, -11.1078),
		geo_uncertainty = c(50000, 30000, 30000),
		geo_source = c(
			"Midpoint of Koundara and Gaoual (Bok\u00e9, Guinea)",
			"Kankan town center (Guinea)",
			"Dabola town center (Guinea)"
		),
		geo_from_source = FALSE
	)
	d <- merge(d, geo, by = "location", all.x = TRUE, sort = FALSE)

	# Full species names and families from the Origine sheet (col 4 = code, col 3 = species, col 2 = family)
	i <- which(!is.na(r3[[4]]))
	spp <- data.frame(
		code = trimws(as.character(r3[[4]][i])),
		weed_species = trimws(as.character(r3[[3]][i])),
		weed_family_ = trimws(as.character(r3[[2]][i]))
	)

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
