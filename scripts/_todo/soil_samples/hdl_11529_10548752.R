# R script for "carob"
# license: GPL (>=3)

## NOTES
# Soil enzyme activities after seven years of the same management, cereal systems, north-west India. The title names India. The workbook does not name a site or give coordinates.
# Each enzyme sheet has a tidy block with columns rep, sc, stage, rhiz, and the activity. Those rows are kept. The absorbance calculations and the two-year means are not.
# Scenario, stage, and rhizosphere are the codes printed in that block (sc 1-4, stage a-d, rhiz r or nr). The workbook does not define them.
# The dataset description lists acid and alkaline phosphatase. One sheet is named Acid Phosphatase. The sheet named PHOSPHATASE is stored as phosphatase. It is not labeled alkaline in the file.
# Units are not stated in the workbook.

## ISSUES
# Scenario, stage, and rhizosphere codes are undefined in the file. No site and no coordinates.
# write_files: missing variables: latitude, longitude
# write_files: unknown variable: stage_ (age?)
# write_files: unknown variables: scenario_, rhizosphere_
# stage_ is the workbook stage code, not an age.

## NEW VARIABLES
# scenario_: management-scenario code from the workbook.
# stage_: stage code from the workbook.
# rhizosphere_: r or nr, as coded in the workbook.

## NOT PROCESSED
# Absorbance readings, standards, and the two-year mean blocks on each sheet.
# The PDF.


carob_script <- function(path) {

"
Climate Smart Agriculture Influences Soil Enzymes Activity under Cereal-based Systems of North-West India

Soil enzyme activities measured after seven years of the same management practices in cereal-based systems of north-west India.
"

	uri <- "hdl:11529/10548752"
	group <- "soil_samples"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "scenario_",
		response_vars = "soil_microbial_activity_value",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.5
	)

	label <- c(
		"Acid Phosphatase" = "acid phosphatase",
		"Arylsulfatase" = "arylsulphatase",
		"Beta glucosidase" = "beta-glucosidase",
		"Urease" = "urease",
		" PHOSPHATASE" = "phosphatase",
		"Flourescien Diacetate Hydrolase" = "fluorescein diacetate hydrolase",
		"DEHYDROGENASE " = "dehydrogenase",
		"cellulase" = "cellulase"
	)

	one <- function(sheet) {
		f <- ff[grepl("\\.xlsx$", basename(ff))][1]
		a <- carobiner::read.excel(f, sheet=sheet, col_names=FALSE)
		hdr <- NULL
		for (i in seq_len(min(8, nrow(a)))) {
			cells <- as.character(unlist(a[i, ]))
			if ("rep" %in% cells && "sc" %in% cells) {
				hdr <- i
				break
			}
		}
		cells <- as.character(unlist(a[hdr, ]))
		crep <- which(cells == "rep")[1]
		body <- a[(hdr + 1):nrow(a), ]
		rep <- as.character(body[[crep]])
		ok <- !is.na(rep) & nzchar(rep)
		body <- body[ok, ]
		data.frame(
			country = "India",
			location = "north-west India",
			trial_id = "enzyme",
			rep = as.integer(as.character(body[[crep]])),
			scenario_ = as.character(body[[crep + 1]]),
			stage_ = as.character(body[[crep + 2]]),
			rhizosphere_ = as.character(body[[crep + 3]]),
			soil_microbial_activity_category = unname(label[sheet]),
			soil_microbial_activity_value = as.numeric(as.character(body[[crep + 4]])),
			on_farm = FALSE,
			is_survey = FALSE,
			geo_from_source = FALSE
		)
	}

	d <- do.call(rbind, lapply(names(label), one))
	d$sample_id <- paste(d$soil_microbial_activity_category, d$rep, d$scenario_, d$stage_, d$rhizosphere_, sep="_")
	d <- d[!is.na(d$scenario_) & !is.na(d$soil_microbial_activity_value), ]
	carobiner::write_files(path, meta, d)
}
