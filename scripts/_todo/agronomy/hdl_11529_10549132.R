# R script for "carob"
# license: GPL (>=3)

## NOTES
# Station comparison of maize shellers. There is no field yield.
# The variety column has no header. Its values are Panoaya and Albatros.
# Hundred-kernel weight is converted to g per 1000 kernels.
# Grain moisture is the content just before shelling.
# The description places the work at the CIMMYT station in Texcoco.
# Geographic coverage is empty and the sheet has no country column, so country is not set.

## ISSUES
# write_files: missing variables: yield, country, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variable: grain_mechanical_damage (age?). unknown variables: shelling_efficiency.


## NOT PROCESSED
# Cob, total, and principal working capacity.
# Flotation index, tip cap, pericarp, germ, and endosperm fractions.
# Normal and abnormal seedlings.
# Operation type and working principle.


carob_script <- function(path) {

"
Data for: Evaluating Maize Shelling Technologies for Mexican Smallholders: Performance, Quality, and Decision Framework for Optimal Selection

CIMMYT implemented a study at its station in Texcoco, Mexico to compare the effectiveness of 7 different commercially available shelling options.
"

	uri <- "hdl:11529/10549132"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment;variety",
		response_vars = "germination_rate;seed_weight",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "CIMMYT-ShellingData-2024-09-24.xlsx"]
	r <- carobiner::read.excel(f, sheet="ShellingData")
	vcol <- names(r)[vapply(r, function(x) any(x == "Panoaya", na.rm=TRUE), logical(1))]
	variety <- as.character(r[[vcol[1]]])
	machine <- as.character(r$Machine)
	key <- paste(machine, variety)
	rep <- ave(seq_len(nrow(r)), key, FUN=seq_along)

	d <- data.frame(
		trial_id = "1",
		crop = "maize",
		yield_part = "grain",
		variety = variety,
		treatment = machine,
		rep = as.integer(rep),
		yield_moisture = as.numeric(r$Moisture_grain),
		seed_weight = as.numeric(r$HKW) * 10,
		germination_rate = as.numeric(r$Germination),
		shelling_efficiency = as.numeric(r$Shelling_efficiency),
		grain_mechanical_damage = as.numeric(r$Mechanical_damage),
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
