# R script for "carob"
# license: GPL (>=3)

## NOTES
# Advanced groundnut lines evaluated in Niger for aflatoxin. Sheet1 of AFVT-SA.xls is the only sheet with rows.
# Columns, in order: plot, entry, replication, genotype, aflatoxin (ppb), pod yield (t/ha), haulm yield (t/ha).
# Pod yield is t/ha x 1000, yield_part pod. Haulm yield is the same conversion; fresh versus dry is not stated.
# G2008.dta has the same 51 rows and the same columns with normalized names.

## ISSUES
# No site name, coordinates, year, plot size, fertilizer, or irrigation. The file name G2008.dta is not used as a planting year.
# Sheets 2–5 are empty.
# No publication DOI in the Dataverse record.
# write_files: missing carob_contributor, yield_moisture, latitude, longitude, irrigated, N_fertilizer, P_fertilizer, K_fertilizer, planting_date, harvest_date.
# unknown variables: aflatoxin_ppb_, haulm_yield_. The hint to rename haulm_yield_ to yield is the pod yield, which is already mapped.

## NOT PROCESSED
# AFVT-SA.xls, Entrées: entry number, one per genotype.
# AFVT-SA.xls sheets 2–5: empty.
# G2008.dta: same table as Sheet1.

## NEW VARIABLES
# aflatoxin_ppb_: aflatoxin in the pods, parts per billion
# haulm_yield_: haulm yield, kg/ha, moisture basis not stated

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
		response_vars = "yield;aflatoxin_ppb_",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "AFVT-SA.xls"]
	r <- carobiner::read.excel(f, sheet = "Sheet1")
	nm <- names(r)

	variety <- trimws(r[[grep("notype", nm)]])

	d <- data.frame(
		country = "Niger",
		crop = "groundnut",
		variety = variety,
		treatment = variety,
		rep = as.integer(r[[grep("epetition", nm)]]),
		plot_id = as.character(r[[grep("^Plot", nm)]]),
		aflatoxin_ppb_ = as.numeric(r[[grep("ppb", nm)]]),
		yield = as.numeric(r[[grep("gousses", nm)]]) * 1000,
		haulm_yield_ = as.numeric(r[[grep("fanes", nm)]]) * 1000,
		yield_part = "pod",
		stress = "disease",
		trial_id = "1",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
