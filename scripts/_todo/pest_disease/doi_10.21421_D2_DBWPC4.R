# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Site name and coordinates are not stated explicitly in the file or Dataverse metadata; "SA" in AFVT-SA.xls was inferred as ICRISAT Sahelian Center (Sadoré, Niger) where Farid Waliyar conducted groundnut aflatoxin trials — needs human confirmation.
# Trial year is not stated in the metadata; companion file G2008.dta suggests 2008, so planting_date was left omitted pending verification.
# Moisture basis for pod yield and haulm yield (y_residue) is not stated.

## NEW
# aflatoxin_: aflatoxin concentration in groundnut pods/kernels, ppb

## MISSING
# planting_date, harvest_date, yield_moisture, irrigated, N_fertilizer, P_fertilizer, K_fertilizer

## NOTES
# Advanced groundnut breeding lines (15 ICGV lines plus resistant check 55-437 and susceptible check JL 24; 17 entries x 3 reps) evaluated in Niger for aflatoxin contamination.
# Sheet1 of AFVT-SA.xls contains the full 51-row table; Sheets 2-5 are empty, and G2008.dta is an identical copy of Sheet1.
# Pod yield and haulm yield (t/ha) were converted to kg/ha and mapped to yield (yield_part = "pod") and y_residue.

## NOT PROCESSED
# AFVT-SA.xls, Entrées!: entry number (1-17), redundant with genotype.
# AFVT-SA.xls, Sheets 2-5: empty.
# G2008.dta: exact duplicate of AFVT-SA.xls Sheet1.

carob_script <- function(path) {

"
Replicated Data for Advanced Breeding Lines Evaluated at Niger for Aflatoxin

Advanced Groundnut Breeding Lines Niger Aflatoxin
"

	uri <- "doi:10.21421/D2/DBWPC4"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 2,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;y_residue",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 85,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "AFVT-SA.xls"]
	r <- carobiner::read.excel(f, sheet = "Sheet1")
	nm <- names(r)

	variety <- trimws(r[[grep("notype", nm)]])

	d <- data.frame(
		country = "Niger",
		adm1 = "Tillabéri",
		location = "Sadoré",
		crop = "groundnut",
		variety = variety,
		treatment = variety,
		rep = as.integer(r[[grep("epetition", nm)]]),
		plot_id = as.character(r[[grep("^Plot", nm)]]),
		aflatoxin_ = as.numeric(r[[grep("ppb", nm)]]),
		yield = as.numeric(r[[grep("gousses", nm)]]) * 1000,
		y_residue = as.numeric(r[[grep("fanes", nm)]]) * 1000,
		yield_part = "pod",
		stress = "disease",
		trial_id = "1",
		on_farm = FALSE,
		is_survey = FALSE,
		longitude = 2.283,
		latitude = 13.250,
		geo_from_source = FALSE,
		geo_uncertainty = 2000,
		geo_source = "ICRISAT Sahelian Center, Sadoré (inferred from AFVT-SA)"
	)

	carobiner::write_files(path, meta, d)
}
