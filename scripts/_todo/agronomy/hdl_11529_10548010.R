# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rabi 2014-15 validation trial. The yield sheet is named Opp-Maize. Cob length, kernel rows, height near 230 cm, and thousand-grain weight near 300 g are maize measurements. Entry codes are stored as recorded.
# Grain yield is Gy(t/ha), stored as kg/ha. Harvesting area is 2 on every row. Grain weight in grams divided by that area, as square meters, matches Gy(t/ha).
# CT is conventional tillage. ZT is zero tillage, stored as land_prep_method none.
# Harvest index in the file is a ratio. It is stored as percent.
# Stover and biomass are not labeled fresh or dry, so they are stored in y_residue and y_total.

## ISSUES
# No fertilizer, moisture, or dates in the yield sheet. Coordinates are approximate for Cooch Behar (geo_from_source = FALSE).

## NEW VARIABLES
# cob_length_cm_: length of cob.
# kernel_rows_: number of kernel rows on the cob.
# grains_per_cob_: number of grains on the cob.


## NOT PROCESSED
# Columns to the right of HI repeat means by CT and CA. They are not plot records.
# pv: averages of the plot sheet.
# code: a variety and node key. HD 2985 and PBW 343 are on that key and not in the yield sheet.
# Raw cob, grain, and straw weights in grams. Yield and the stover column already use them.


carob_script <- function(path) {

"
Farmer participatory on-farm trials with CA technologies comparing with farmers’ practices (CT), were conducted in several fields in each community. Likewise, farmer-participatory validation trials were conducted comparing to existing practices and to find out suitable and more profitable crop production practices, prioritized to increase visibility and to avoid implementation and management problems that emerge when utilizing small plots with significant edge effects. Most trials were replicated in several fields within each community and were farmer-managed with backstopping from project staff and NARES partners. Project partners and staff coordinated monitoring and data acquisition. Where possible, collaborating farmers were selected by the community, and the project worked with existing farmer groups, with groups of both men and women farmers.
"

	uri <- "hdl:11529/10548010"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
		data_organization = "CIMMYT",
		publication = NA,
		project = "SRFSI",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety;land_prep_method",
		response_vars = "yield;plant_height",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 60,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "Rabi 2014-15-validation trials-all nodes-Choochbehar.xlsx"]
	r <- carobiner::read.excel(f, sheet="Opp-Maize")
	r <- r[!is.na(r$VAR), ]

	till <- trimws(as.character(r$TILLAGE))
	prep <- ifelse(till == "CT", "conventional", ifelse(till == "ZT", "none", NA_character_))

	d <- data.frame(
		country = "India",
		adm1 = "West Bengal",
		adm2 = "Cooch Behar",
		location = carobiner::fix_name(trimws(as.character(r$NODE)), "title"),
		latitude = 26.324,
		longitude = 89.448,
		trial_id = paste(carobiner::fix_name(trimws(as.character(r$NODE)), "title"), "2014-15"),
		rep = as.integer(r$REP),
		crop = "maize",
		variety = trimws(as.character(r$VAR)),
		treatment = paste(trimws(as.character(r$VAR)), till),
		land_prep_method = prep,
		season = "rabi",
		yield_part = "grain",
		yield = as.numeric(r$`Gy(t/ha)`) * 1000,
		plant_height = as.numeric(r$HEIGHT...5),
		seed_weight = as.numeric(r$`1000 GR WEIGHT...14`),
		harvest_index = as.numeric(r$HI) * 100,
		y_residue = as.numeric(r$`Stover yield (t/ha)`) * 1000,
		y_total = as.numeric(r$`Biomass (t/ha)`) * 1000,
		cob_length_cm_ = as.numeric(r$`LENGTH OF COB...11`),
		kernel_rows_ = as.numeric(r$`NO. OF ROWS...10`),
		grains_per_cob_ = as.numeric(r$`NO, OF GRAINS...12`),
		plot_area = as.numeric(r$`Harvesting area`),
		on_farm = TRUE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	d$record_id <- as.integer(seq_len(nrow(d)))
	carobiner::write_files(path, meta, d)
}
