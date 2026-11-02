# R script for "carob"
# license: GPL (>=3)

## NOTES
# The raw folder has the Ayacucho workbook only (Quishuarcancha). The description also names Huancayo. That site is not in this folder. The material list has 192 clones, not the 340 accessions named in the description.
# Place, elevation, coordinates, and dates are the Minimal sheet. Design, plot size 1.8 m2, spacing 0.25 m by 0.90 m, and planting density are the Installation sheet. Begin date and the planting row are both 2007-11-21. The Minimal end date is 2008-05-26. The crop-management harvest row is 2008-06-15, and that date is used as harvest_date. Vine killing has no date and no product.
# ASC_DW and ASC_FW have no units in this workbook. They are stored under tuber chemistry names without a unit conversion. DM is not labeled either. In other CIP vitamin C fieldbooks the matching column is percent dry matter, so yield_moisture is 100 minus DM.

## ISSUES
# No yield.
# Huancayo is named in the description and is not in the raw folder.
# Soil_analysis has no measurements. Weather_data is empty.
# write_files: missing carob_contributor, yield, yield_part, irrigated, K_fertilizer, N_fertilizer, P_fertilizer. unknown variables: tuber_ascorbate_dw_, tuber_ascorbate_fw_.

## NEW VARIABLES
# tuber_ascorbate_dw_, tuber_ascorbate_fw_: ASC_DW and ASC_FW. Unit is not labeled in this file.

## NOT PROCESSED
# Summary by clone, Original-VitaminC, Tabla Ayacucho, and Var List: summaries or source tables already represented by the fieldbook.
# Code Laboratory.
# Soil_analysis and Weather_data: no measurements.
# Crop_management products: none are recorded.


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
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.35
	)

	f <- ff[grepl("QUISCH", basename(ff))]
	num <- function(x) as.numeric(as.character(x))
	one <- function(sheet, name) {
		hit <- sheet$Value[trimws(as.character(sheet$Factor)) == name]
		if (length(hit) == 0) return(NA_character_)
		x <- hit[1]
		if (inherits(x, "POSIXt") || inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
		trimws(as.character(x))
	}

	mini <- carobiner::read.excel(f, sheet="Minimal")
	inst <- carobiner::read.excel(f, sheet="Installation")
	mat <- carobiner::read.excel(f, sheet="Material List")
	fb <- carobiner::read.excel(f, sheet="Fieldbook")
	cm <- carobiner::read.excel(f, sheet="Crop_management")

	code <- trimws(as.character(mat[["Institutional number"]]))
	vname <- gsub("\u00a0", " ", as.character(mat[["Clone or variety name"]]), fixed=TRUE)
	vname <- trimws(vname)
	vname[!nzchar(vname)] <- NA_character_
	species <- trimws(as.character(mat[["Species acronym"]]))
	alias <- data.frame(variety_code=code, variety=vname, variety_type=species)
	alias <- alias[!duplicated(alias$variety_code), ]

	fb$variety_code <- trimws(as.character(fb$INSTN))
	fb <- merge(fb, alias, by="variety_code", all.x=TRUE)
	fb$variety[is.na(fb$variety) | !nzchar(fb$variety)] <- fb$variety_code[is.na(fb$variety) | !nzchar(fb$variety)]

	harvest_row <- cm$Date[trimws(as.character(cm[["Intervention type"]])) == "Harvest"]
	harvest_date <- harvest_row[1]
	if (inherits(harvest_date, "POSIXt") || inherits(harvest_date, "Date")) {
		harvest_date <- format(harvest_date, "%Y-%m-%d")
	} else {
		harvest_date <- trimws(as.character(harvest_date))
	}

	d <- data.frame(
		variety_code = fb$variety_code,
		variety = fb$variety,
		variety_type = fb$variety_type,
		treatment = fb$variety,
		rep = as.integer(fb$REP),
		plot_id = as.character(fb$PLOT),
		country = one(mini, "Country"),
		adm1 = one(mini, "Admin1"),
		adm2 = one(mini, "Admin2"),
		adm3 = one(mini, "Admin3"),
		location = one(mini, "Locality"),
		elevation = num(one(mini, "Elevation")),
		latitude = num(one(mini, "Latitude")),
		longitude = num(one(mini, "Longitude")),
		geo_from_source = TRUE,
		planting_date = one(mini, "Begin date"),
		harvest_date = harvest_date,
		trial_id = "Quishuarcancha_2007",
		plot_area = num(one(inst, "Plot size (m2)")),
		plant_spacing = num(one(inst, "Distance between plants (m)")) * 100,
		row_spacing = num(one(inst, "Distance between rows (m)")) * 100,
		seed_density = num(one(inst, "Planting density (plants/Ha)")),
		tuber_ascorbate_dw_ = num(fb$ASC_DW),
		tuber_ascorbate_fw_ = num(fb$ASC_FW),
		yield_moisture = 100 - num(fb$DM),
		crop = "potato",
		on_farm = NA,
		is_survey = FALSE
	)
	d <- d[!is.na(d$variety) & nzchar(d$variety), ]
	d$record_id <- seq_len(nrow(d))
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
