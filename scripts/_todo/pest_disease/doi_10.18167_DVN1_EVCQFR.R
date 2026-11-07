# R script for "carob"
# license: GPL (>=3)

## NOTES
# 110 cotton surveys in Guinea. One row per species with a rating > 0.
# Floristique is the DOC 1-9 scale. Note_globale is that same 1-9 note and is
# stored as weed_cover. weed_species is the 5-letter code. Location is Lieu
# (Dabola, Kankan, or Koundara-Gaoual).

## ISSUES
# Title says 1996; the file also has 1997-08-01. The DOC calendar line
# (23 September-5 October 2018) does not match the Date serials. No coordinates.
# write_files: missing variables: yield, yield_moisture, yield_part, latitude,
# longitude, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.


## NOT PROCESSED
# Facteurs not mapped: Type_culture, Climat, Epoque/cycle, jour_de_mission,
# Observation_du_jour. Méta Données and the origine sheet repeat the survey.
# FLO, FAC, and PA repeat the workbook or the DOC.
# GUI-MAR-1996-COT-AD-FLO.txt: not read. columns: "GUI-MAR-01" "GUI-MAR-02" "GUI-MAR-03" "GUI-MAR-04" "GUI-MAR-05" "GUI-MAR-06" "GUI-MAR-07" "GUI-MAR-08" "GUI-MAR-09" "GUI-MAR-10" "GUI-MAR-11" "GUI-MAR-12" "GUI-MAR-13" "GUI-MAR-14" "GUI-MAR-15" "GUI-MAR-16" "GUI-MAR-17" "GUI-MAR-18" "GUI-MAR-19" "GUI-MAR-20" "GUI-MAR-21" "GUI-MAR-22" "GUI-MAR-23" "GUI-MAR-24" "GUI-MAR-25" "GUI-MAR-26" "GUI-MAR-27" "GUI-MAR-28" "GUI-MAR-29" "GUI-MAR-30" "GUI-MAR-31" "GUI-MAR-32" "GUI-MAR-33" "GUI-MAR-34" "GUI-MAR-35" "GUI-MAR-36" "GUI-MAR-37" "GUI-MAR-38" "GUI-MAR-39" "GUI-MAR-40" "GUI-MAR-41" "GUI-MAR-42" "GUI-MAR-43" "GUI-MAR-44" "GUI-MAR-45" "GUI-MAR-46" "GUI-MAR-47" "GUI-MAR-48" "GUI-MAR-49" "GUI-MAR-50" "GUI-MAR-51" "GUI-MAR-52" "GUI-MAR-53" "GUI-MAR-54" "GUI-MAR-55" "GUI-MAR-56" "GUI-MAR-57" "GUI-MAR-58" "GUI-MAR-59" "GUI-MAR-60" "GUI-MAR-61" "GUI-MAR-62" "GUI-MAR-63" "GUI-MAR-64" "GUI-MAR-65" "GUI-MAR-66" "GUI-MAR-67" "GUI-MAR-68" "GUI-MAR-69" "GUI-MAR-70" "GUI-MAR-71" "GUI-MAR-72" "GUI-MAR-73" "GUI-MAR-74" "GUI-MAR-75" "GUI-MAR-76" "GUI-MAR-77" "GUI-MAR-78" "GUI-MAR-79" "GUI-MAR-80" "GUI-MAR-81" "GUI-MAR-82" "GUI-MAR-83" "GUI-MAR-84" "GUI-MAR-85" "GUI-MAR-86" "GUI-MAR-87" "GUI-MAR-88" "GUI-MAR-89" "GUI-MAR-90" "GUI-MAR-91" "GUI-MAR-92" "GUI-MAR-93" "GUI-MAR-94" "GUI-MAR-95" "GUI-MAR-96" "GUI-MAR-97" "GUI-MAR-98" "GUI-MAR-99" "GUI-MAR-100" "GUI-MAR-101" "GUI-MAR-102" "GUI-MAR-103" "GUI-MAR-104" "GUI-MAR-105" "GUI-MAR-106" "GUI-MAR-107" "GUI-MAR-108" "GUI-MAR-109" "GUI-MAR-110". Observation records are missing from d until this file is standardized.
# GUI-MAR-1996-COT-FAC.txt: not read. columns: "GUI-MAR-01" "GUI-MAR-02" "GUI-MAR-03" "GUI-MAR-04" "GUI-MAR-05" "GUI-MAR-06" "GUI-MAR-07" "GUI-MAR-08" "GUI-MAR-09" "GUI-MAR-10" "GUI-MAR-11" "GUI-MAR-12" "GUI-MAR-13" "GUI-MAR-14" "GUI-MAR-15" "GUI-MAR-16" "GUI-MAR-17" "GUI-MAR-18" "GUI-MAR-19" "GUI-MAR-20" "GUI-MAR-21" "GUI-MAR-22" "GUI-MAR-23" "GUI-MAR-24" "GUI-MAR-25" "GUI-MAR-26" "GUI-MAR-27" "GUI-MAR-28" "GUI-MAR-29" "GUI-MAR-30" "GUI-MAR-31" "GUI-MAR-32" "GUI-MAR-33" "GUI-MAR-34" "GUI-MAR-35" "GUI-MAR-36" "GUI-MAR-37" "GUI-MAR-38" "GUI-MAR-39" "GUI-MAR-40" "GUI-MAR-41" "GUI-MAR-42" "GUI-MAR-43" "GUI-MAR-44" "GUI-MAR-45" "GUI-MAR-46" "GUI-MAR-47" "GUI-MAR-48" "GUI-MAR-49" "GUI-MAR-50" "GUI-MAR-51" "GUI-MAR-52" "GUI-MAR-53" "GUI-MAR-54" "GUI-MAR-55" "GUI-MAR-56" "GUI-MAR-57" "GUI-MAR-58" "GUI-MAR-59" "GUI-MAR-60" "GUI-MAR-61" "GUI-MAR-62" "GUI-MAR-63" "GUI-MAR-64" "GUI-MAR-65" "GUI-MAR-66" "GUI-MAR-67" "GUI-MAR-68" "GUI-MAR-69" "GUI-MAR-70" "GUI-MAR-71" "GUI-MAR-72" "GUI-MAR-73" "GUI-MAR-74" "GUI-MAR-75" "GUI-MAR-76" "GUI-MAR-77" "GUI-MAR-78" "GUI-MAR-79" "GUI-MAR-80" "GUI-MAR-81" "GUI-MAR-82" "GUI-MAR-83" "GUI-MAR-84" "GUI-MAR-85" "GUI-MAR-86" "GUI-MAR-87" "GUI-MAR-88" "GUI-MAR-89" "GUI-MAR-90" "GUI-MAR-91" "GUI-MAR-92" "GUI-MAR-93" "GUI-MAR-94" "GUI-MAR-95" "GUI-MAR-96" "GUI-MAR-97" "GUI-MAR-98" "GUI-MAR-99" "GUI-MAR-100" "GUI-MAR-101" "GUI-MAR-102" "GUI-MAR-103" "GUI-MAR-104" "GUI-MAR-105" "GUI-MAR-106" "GUI-MAR-107" "GUI-MAR-108" "GUI-MAR-109" "GUI-MAR-110". Observation records are missing from d until this file is standardized.
# GUI-MAR-1996-COT-PA-FLO.txt: not read. columns: "GUI-MAR-01" "GUI-MAR-02" "GUI-MAR-03" "GUI-MAR-04" "GUI-MAR-05" "GUI-MAR-06" "GUI-MAR-07" "GUI-MAR-08" "GUI-MAR-09" "GUI-MAR-10" "GUI-MAR-11" "GUI-MAR-12" "GUI-MAR-13" "GUI-MAR-14" "GUI-MAR-15" "GUI-MAR-16" "GUI-MAR-17" "GUI-MAR-18" "GUI-MAR-19" "GUI-MAR-20" "GUI-MAR-21" "GUI-MAR-22" "GUI-MAR-23" "GUI-MAR-24" "GUI-MAR-25" "GUI-MAR-26" "GUI-MAR-27" "GUI-MAR-28" "GUI-MAR-29" "GUI-MAR-30" "GUI-MAR-31" "GUI-MAR-32" "GUI-MAR-33" "GUI-MAR-34" "GUI-MAR-35" "GUI-MAR-36" "GUI-MAR-37" "GUI-MAR-38" "GUI-MAR-39" "GUI-MAR-40" "GUI-MAR-41" "GUI-MAR-42" "GUI-MAR-43" "GUI-MAR-44" "GUI-MAR-45" "GUI-MAR-46" "GUI-MAR-47" "GUI-MAR-48" "GUI-MAR-49" "GUI-MAR-50" "GUI-MAR-51" "GUI-MAR-52" "GUI-MAR-53" "GUI-MAR-54" "GUI-MAR-55" "GUI-MAR-56" "GUI-MAR-57" "GUI-MAR-58" "GUI-MAR-59" "GUI-MAR-60" "GUI-MAR-61" "GUI-MAR-62" "GUI-MAR-63" "GUI-MAR-64" "GUI-MAR-65" "GUI-MAR-66" "GUI-MAR-67" "GUI-MAR-68" "GUI-MAR-69" "GUI-MAR-70" "GUI-MAR-71" "GUI-MAR-72" "GUI-MAR-73" "GUI-MAR-74" "GUI-MAR-75" "GUI-MAR-76" "GUI-MAR-77" "GUI-MAR-78" "GUI-MAR-79" "GUI-MAR-80" "GUI-MAR-81" "GUI-MAR-82" "GUI-MAR-83" "GUI-MAR-84" "GUI-MAR-85" "GUI-MAR-86" "GUI-MAR-87" "GUI-MAR-88" "GUI-MAR-89" "GUI-MAR-90" "GUI-MAR-91" "GUI-MAR-92" "GUI-MAR-93" "GUI-MAR-94" "GUI-MAR-95" "GUI-MAR-96" "GUI-MAR-97" "GUI-MAR-98" "GUI-MAR-99" "GUI-MAR-100" "GUI-MAR-101" "GUI-MAR-102" "GUI-MAR-103" "GUI-MAR-104" "GUI-MAR-105" "GUI-MAR-106" "GUI-MAR-107" "GUI-MAR-108" "GUI-MAR-109" "GUI-MAR-110". Observation records are missing from d until this file is standardized.


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
		design = "weed abundance survey",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "weed_species;ground_cover;weed_cover",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "GUI-MAR-1996-COT-AD.xlsx"]
	r1 <- carobiner::read.excel(f, sheet="Facteurs")
	r2 <- carobiner::read.excel(f, sheet="Floristique")

	get_row <- function(label) unname(unlist(r1[r1[[1]] == label, -1]))

	# GUI-MAR-1996-COT-AD-DOC.txt: 1-9 note to % cover, same table as Benin 2013.
	rating_to_cover <- c(1, 7, 15, 30, 50, 70, 85, 93, 100)

	crop_map <- c(cotonnier = "cotton")
	irr <- tolower(trimws(as.character(get_row("Irrigation"))))
	enh <- as.integer(as.numeric(get_row("Note_globale")))
	# Date is an Excel serial (1996-10-23, 1996-11-01, 1997-08-01).
	obs <- as.numeric(get_row("Date"))
	d1b <- data.frame(
		trial_id = names(r1)[-1],
		crop = unname(crop_map[tolower(trimws(as.character(get_row("Culture_origine"))))]),
		date = as.character(as.Date(obs, origin="1899-12-30")),
		country = "Guinea", # geographicCoverage
		location = trimws(as.character(get_row("Lieu"))),
		weed_cover = rating_to_cover[match(enh, 1:9)],
		irrigated = irr %in% c("inonde", "inondé"),
		stress = "weeds"
	)
	d1b$intercropped <- FALSE
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
