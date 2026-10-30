# R script for "carob"
# license: GPL (>=3)

## NOTES
# Kok Saroy, Tashkent. Coordinates, elevation, dates, plot size 5.25 m2, spacing, and planting density are on the Minimal and Installation sheets.
# Total tuber weight per plot (TTWP, kg) divided by 5.25 m2 and multiplied by 10 matches TTYNA (t/ha). TTYA and MTYA are the stand-adjusted yields in that same unit, multiplied by 1000 to kg/ha.
# The crop-management sheet records plant height, number of stems, and plant canopy on 2008-09-30. Those are the fieldbook columns PH, NS, and PC. Height is stored as cm because the values are 42-79. Canopy is stored as percent because the values are 60-100. Neither unit is printed in the dictionary.
# Design is RCBD on the Installation sheet and in the description. The fieldbook has 5 replicates (15 clones x 5 = 75 rows). The description says three repetitions.

## ISSUES
# No fertilizer rates or irrigation in the workbook.
# Average tuber weight (ATW) has no unit.
# The description's replicate count does not match the fieldbook.
# write_files: missing carob_contributor, yield_moisture, irrigated, K_fertilizer, N_fertilizer, P_fertilizer. unknown variables: yield_unadjusted_, yield_marketable_unadjusted_, stems_per_plant_.

## NEW VARIABLES
# yield_unadjusted_: total tuber yield not adjusted (TTYNA), kg/ha.
# yield_marketable_unadjusted_: marketable tuber yield not adjusted (MTYNA), kg/ha.
# stems_per_plant_: number of stems recorded on 2008-09-30.

## NOT PROCESSED
# Fieldbook tuber-category counts and weights (NMTCI, NMTCII, NNoMTP, MTWCI, MTWCII, NoMTWP, MTWP, NMTP). Components of the total and marketable yields.
# ATW, average tuber weight, unit not stated.
# Crop_management rows are the dates of the measurements already in the fieldbook. No product or dose is filled in.
# Charts and Ranks by Clone sheets, and Summary by clone. Summaries of the fieldbook.


carob_script <- function(path) {

"
Dataset for: Assessment of Heat-Tolerant Potato Varieties in Uzbekistan (2008)

Trials featuring heat-tolerant potato varieties were conducted in South West Central Asia (SWCA), specifically in Tashkent, Uzbekistan, during the year 2008. In this study, 15 potato clones were evaluated using a randomized complete block design (RCBD) with three repetitions
"

	uri <- "doi:10.21223/15FGBK"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;yield_marketable;plant_height;ground_cover",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 65,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "PTYL200807_TASHKENT_HEAT.xls"]
	r <- carobiner::read.excel(f, sheet="Fieldbook")
	m <- carobiner::read.excel(f, sheet="Material List")

	name <- trimws(as.character(m[["Clone or variety name"]]))
	code <- trimws(as.character(m[["Institutional number"]]))
	name[!nzchar(name)] <- NA_character_
	variety <- name
	variety[is.na(variety)] <- code[is.na(variety)]
	fem <- trimws(as.character(m[["Female code"]]))
	mal <- trimws(as.character(m[["Male code"]]))
	fem[!nzchar(fem)] <- NA_character_
	mal[!nzchar(mal)] <- NA_character_
	ped <- paste(fem, "x", mal)
	ped[is.na(fem) | is.na(mal)] <- NA_character_
	lookup <- data.frame(
		INSTN = code,
		variety = variety,
		variety_pedigree = ped,
		stringsAsFactors = FALSE
	)
	lookup <- lookup[!is.na(lookup$INSTN) & nzchar(lookup$INSTN), ]

	r$INSTN <- trimws(as.character(r$INSTN))
	if (any(!r$INSTN %in% lookup$INSTN)) stop("fieldbook clone missing from the material list")
	r <- merge(r, lookup, by="INSTN", all.x=TRUE)

	# 5.25 m2 plots. kg/plot / m2 * 10 = t/ha, checked against TTYNA.
	plot_m2 <- 5.25
	d <- data.frame(
		crop = "potato",
		variety = r$variety,
		variety_code = r$INSTN,
		variety_pedigree = r$variety_pedigree,
		treatment = r$variety,
		rep = as.integer(r$REP),
		plot_id = as.character(r$PLOT),
		country = "Uzbekistan",
		adm1 = "Tashkent",
		adm2 = "Tashkent",
		adm3 = "Tashkent",
		location = "Kok Saroy",
		latitude = 41.36399746,
		longitude = 69.14204836,
		elevation = 436,
		geo_from_source = TRUE,
		planting_date = "2008-07-12",
		harvest_date = "2008-11-04",
		plot_area = plot_m2,
		plant_spacing = 25,
		row_spacing = 70,
		# Installation density 57142.9 plants/ha = 30 tubers planted (NTP) on 5.25 m2.
		seed_density = 10000 * 30 / plot_m2,
		plant_density = as.numeric(r$NPE) / plot_m2 * 10000,
		tuber_density = as.numeric(r$TNTP) / plot_m2 * 10000,
		yield = as.numeric(r$TTYA) * 1000,
		yield_marketable = as.numeric(r$MTYA) * 1000,
		yield_unadjusted_ = as.numeric(r$TTYNA) * 1000,
		yield_marketable_unadjusted_ = as.numeric(r$MTYNA) * 1000,
		yield_part = "tubers",
		plant_height = as.numeric(r$PH),
		stems_per_plant_ = as.numeric(r$NS),
		ground_cover = as.numeric(r$PC),
		stress = "heat",
		trial_id = "Tashkent_2008",
		on_farm = NA,
		is_survey = FALSE,
		stringsAsFactors = FALSE
	)

	carobiner::write_files(path, meta, d)
}
