# R script for "carob"
# license: GPL (>=3)

## NOTES
# The raw folder has the Ayacucho workbook only (Quishuarcancha). Processed with shared CIP HIDAP helper process_cip_lbvars(), then augmented with Crop_management harvest date, Material List species acronym (variety_type), and Fieldbook ASC_DW / ASC_FW / DM.
# ASC_DW and ASC_FW have no units printed in this workbook; kept as tuber_ascorbate_dw_ and tuber_ascorbate_fw_. DM (%) is stored as yield_moisture = 100 - DM.

## ISSUES
# No yield, fertilizer rates, or irrigation reported.
# Huancayo is named in the description and is not in the raw folder.
# Soil_analysis has no measurements. Weather_data is empty.

## NEW VARIABLES
# tuber_ascorbate_dw_, tuber_ascorbate_fw_: ASC_DW and ASC_FW (unit not printed in workbook).

## NOT PROCESSED
# Summary by clone, Original-VitaminC, Tabla Ayacucho, and Var List: summaries or source tables already represented by the fieldbook.
# Code Laboratory, empty Soil_analysis and Weather_data, Crop_management rows without products.


carob_script <- function(path) {

"
Dataset for: Field Evaluation and Nutritional Analysis of Stenotomum and Goniocalyx Potatoes in Ayacucho - 2007

340 accessions of Solanum stenotomum and Goniocalyx were harvested at two locations in Peru: Huancayo and Ayacucho. Sample preparation is currently underway for the determination of mineral content, as well as the quantification of total and individual carotenoid, phenolic, and vitamin C concentrations. Iron, zinc, phenolic, vitamin C, and carotenoid characterizations will be carried out for the Stenotomum and Goniocalyx potato germplasm.'
"

	uri <- "doi:10.21223/0NB09B"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "tuber_ascorbate_dw_;yield_moisture",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 70,
		carob_effort = 0.4
	)

	proc <- carobiner::get_function("process_cip_lbvars", path, group)
	f <- ff[basename(ff) == "TVIT200711_QUISCH_Ayacucho_Gon_Stn_Field.xls"]
	d <- proc(f)
	r3 <- carobiner::read.excel(f, sheet="Material List")
	r4 <- carobiner::read.excel(f, sheet="Fieldbook")
	r5 <- carobiner::read.excel(f, sheet="Crop_management")

	sp_map <- setNames(trimws(as.character(r3[["Species acronym"]])), trimws(as.character(r3[["Institutional number"]])))
	d$variety_type <- unname(sp_map[d$variety_code])
	d$variety <- trimws(gsub("\u00a0", " ", d$variety, fixed=TRUE))
	d$treatment <- d$variety
	d$tuber_ascorbate_dw_ <- as.numeric(r4$ASC_DW)
	d$tuber_ascorbate_fw_ <- as.numeric(r4$ASC_FW)
	d$yield_moisture <- 100 - as.numeric(r4$DM)

	harvest_row <- r5$Date[trimws(as.character(r5[["Intervention type"]])) == "Harvest"]
	if (length(harvest_row) > 0 && !is.na(harvest_row[1])) {
		d$harvest_date <- if (inherits(harvest_row[1], "POSIXt") || inherits(harvest_row[1], "Date")) {
			format(harvest_row[1], "%Y-%m-%d")
		} else {
			trimws(as.character(harvest_row[1]))
		}
	}
	d$trial_id <- "Quishuarcancha_2007"
	d$on_farm <- NA
	d$irrigated <- NA
	d <- d[!is.na(d$variety) & nzchar(d$variety), ]
	d$record_id <- seq_len(nrow(d))
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
