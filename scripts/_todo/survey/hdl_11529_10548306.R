# R script for "carob"
# license: GPL (>=3)

## NOTES
# TAMASA Nigeria agronomy panel survey, maize crop cuts, 2016. geographicCoverage is Kaduna, Kano, and Katsina.
# The state column is KD, KN, and KT. Those are stored as Kaduna, Kano, and Katsina.
# dry_yield(kg/ha) is grain dry yield. avgmc is percent moisture. The 12% moisture yield is not used.
# The column grainwt/25m2 names the cut area as 25 m2. That is plot_area.
# Latitude and longitude columns are in the file and are empty in every row. The file name says the coordinates were blurred.

## ISSUES
# No coordinates in the open columns.
# LGA, ward, and village are codes, not names.
# write_files: missing variables: latitude, longitude.

## NOT PROCESSED
# The household workbook (demographics and the full management module).
# Cob weights and stand counts used to compute the yield column.
# Quadrat-level intercrop weights. The crop of that intercrop is not named.


carob_script <- function(path) {

"
TAMASA Agronomy Panel Survey (APS) 2016: Household and maize crop-cut data from Nigeria

Household survey and maize crop-cut data collected in Kaduna, Kano, and Katsina States, Nigeria, during 2016.
"

	uri <- "hdl:11529/10548306"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "TAMASA",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.4
	)

	f <- ff[grepl("CC_Yield", basename(ff))][1]
	r <- as.data.frame(carobiner::read.excel(f, sheet="Data"))
	state <- as.character(r$state)
	state[state == "KD"] <- "Kaduna"
	state[state == "KN"] <- "Kano"
	state[state == "KT"] <- "Katsina"
	sp <- as.character(r[["rowdist(cm)"]])
	sp[sp %in% c("0")] <- NA_character_
	d <- data.frame(
		country = "Nigeria",
		adm1 = state,
		hhid = as.character(r$hhid),
		trial_id = as.character(r$hhid),
		plot_id = as.character(r$hhindex),
		crop = "maize",
		yield_part = "grain",
		yield = as.numeric(r[["dry_yield(kg/ha)"]]),
		yield_moisture = as.numeric(r$avgmc),
		yield_isfresh = FALSE,
		plot_area = 25,
		row_spacing = as.numeric(sp),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)
	d <- d[!is.na(d$hhid) & nzchar(d$hhid), ]

	carobiner::write_files(path, meta, d)
}
