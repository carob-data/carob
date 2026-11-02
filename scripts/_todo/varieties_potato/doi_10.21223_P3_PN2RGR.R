# R script for "carob"
# license: GPL (>=3)

## NOTES
# Late blight on breeding clones, parents, and checks. LB1 to LB7 are the percentage of leaf area affected. Those scores are the long table, in column order. The dictionary does not date the scores.
# audpc on the sheet was computed from those percentages. The vocabulary AUDPC is the sum of the infected fraction, so the stored value is the sheet value divided by 100.
# Rows whose genotype is "empty" are unused field positions. They are left out. 218 of them.
# The description says the trial was planted on 2019-09-24 in Oxapampa, Pasco, Peru. The title and the table do not name that place, so country and coordinates are not stored. The Dataverse box is -75.4153, -10.6039.

## ISSUES
# No yield, fertilizer, irrigation, harvest date, or coordinates in the file.
# The description says 1810 m. A geospatial note says 1827 m. Neither is stored.
# 106 clone rows have no audpc, including 68 with no LB score. They are kept.
# write_files: missing yield, yield_moisture, yield_part, country, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date. unknown variables: genotype_role_, late_blight_pct_. The long table has no date or depth column.

## NOT PROCESSED
# 02_Material_list_LBHTC2.xlsx. The genotype and the type are already on the trial sheet. Capiro is listed twice.
# Row and column of the field plan. The plot number is the plot_id.
# 03_Data_dictionary_LBHTC2.xlsx. The definitions used above.


## NEW VARIABLES
# genotype_role_: test, parental, or check.
# late_blight_pct_: percent of leaf area affected at one score in the trial sequence.


carob_script <- function(path) {

"
Dataset for: Replication Data for: Late blight severity in breeding clones from CIP's LBHTC2 breeding population

This dataset contains field records of late blight progression from Trial A of the study \"Prediction of late blight severity in a large panel of potato genotypes using low-altitude aerial images and machine learning methods.\" The trial comprised 2,745 breeding clones from the LBHTC2 population of the CIP breeding program. It was planted on September 24, 2019, under field conditions with high endemic late blight pressure in Oxapampa, Pasco, Peru (1,810 m a.s.l.).
The dataset includes genotype and experimental-unit identifiers and successive visual assessments of disease progression at the plot level, expressed as the percentage of leaf area affected by late blight. The deposited records correspond specifically to the final Trial A dataset analyzed in the associated publication.
"

	uri <- "doi:10.21223/P3/PN2RGR"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "AUDPC;late_blight_pct_",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "01_Data_LBHTC2.xlsx"]
	r <- carobiner::read.excel(f, sheet="raw_trialA", na=c("", "NA"))
	r <- r[trimws(as.character(r$genotype)) != "empty", ]
	rownames(r) <- NULL
	variety <- trimws(as.character(r$genotype))

	d <- data.frame(
		trial_id = "LBHTC2",
		crop = "potato",
		variety = variety,
		treatment = variety,
		genotype_role_ = trimws(as.character(r$type)),
		rep = as.integer(r$rep),
		plot_id = as.character(r$plot),
		AUDPC = as.numeric(r$audpc) / 100,
		planting_date = "2019-09-24",
		stress = "disease",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	d$record_id <- seq_len(nrow(d))

	lbs <- paste0("LB", 1:7)
	long <- lapply(lbs, function(nm) {
		data.frame(
			record_id = d$record_id,
			order = as.integer(sub("^LB", "", nm)),
			late_blight_pct_ = as.numeric(r[[nm]])
		)
	})
	long <- do.call(rbind, long)
	rownames(d) <- NULL
	rownames(long) <- NULL

	carobiner::write_files(path, meta, d, long=long)
}
