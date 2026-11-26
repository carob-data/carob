# R script for "carob"
# license: GPL (>=3)

## NOTES
# Two diploid-potato vitamin C trials (La Victoria, Huancayo, Junin and Aco, Paucartambo, Pasco, Peru), both field RCBDs with 10 plants on 2.7 m2.
# Processed using shared CIP HIDAP helper process_cip_lbvars(), then augmented with vitamin_C (ASC_FW, mg/100g FW), tuber_ascorbate_dw_ (ASC_DW, mg/100g DW), selection_cycle_, per-replicate soil samples for La Victoria, and Crop_management planting/harvest dates for Aco.
# AVDM (%) is stored as yield_moisture = 100 - AVDM (computed by process_cip_lbvars()).
# Aco soil_P is 299.38 mg/kg (above terminag max 200; kept as recorded).

## ISSUES
# No tuber yield, fertilizer rate, or irrigation statement.
# La Victoria Crop_management planting/harvest cells are blank; Minimal begin/end dates (2016-01-19 / 2016-06-09) are retained.
# Aco soil_P is 299.38 mg/kg (above terminag max 200); kept as recorded.

## NOT PROCESSED
# Weather_data (15-minute HOBO, rain has no unit), Crop_management sprays, soil total N %, other micronutrients, guano sample (Data5), Summary, Original_laboratory, Data_dictionary.xls.

## NEW VARIABLES
# selection_cycle_: recurrent-selection cycle (La Victoria from Material_List; Aco from Fieldbook OBS).
# tuber_ascorbate_dw_: ascorbic acid concentration (mg/100g dry weight). (Fresh weight ASC_FW is stored in standard vitamin_C in mg/100g FW.)


carob_script <- function(path) {

"
Dataset for: Genetic gains in the content of \"Vitamin C” in diploid potato for three cycles recurrent selection

The purpose of this study was to estimate the genetic gains of three cycles of recurrent selection for the Vitamin C content in diploid potatoes, and to determine the heritability and relationships with variables of agronomical importance. The field evaluation was conducted in La Victoria, Huancayo, Junin, Peru in a Randomized Complete Block Design, with four repetitions, evaluating 61 genotypes (16=cycle0, 2=cycle1, 21=cycle2, 22=cycle3) and in Acco, Paucartambo, Pasco, Peru in a Randomized Complete Block Design, with three repetitions, evaluating 55 genotypes (15=cycle0, 0=cycle1, 20=cycle2, 20=cycle3).
"

	uri <- "doi:10.21223/P3/G4MWZN"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP",
		publication = NA,
		project = "Advancing Achievements in Breeding for Early, Resilient and Nutritious Potato and Sweetpotato; Bio-fortified potato varieties to help overcome micronutrient malnutrition in East Africa",
		design = "Randomized Complete Block Design (RCBD)",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield_moisture;vitamin_C;tuber_ascorbate_dw_",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 75,
		carob_effort = 0.7
	)

	proc <- carobiner::get_function("process_cip_lbvars", path, group)

	one <- function(f) {
		d <- proc(f)
		r <- carobiner::read.excel(f, sheet="Fieldbook")
		r1 <- carobiner::read.excel(f, sheet="Material_List")
		r3 <- carobiner::read.excel(f, sheet="Soil_analysis")
		r4 <- carobiner::read.excel(f, sheet="Crop_management")

		blank <- function(x) {
			x <- trimws(as.character(x))
			x[!nzchar(x)] <- NA_character_
			x
		}
		as_date <- function(x) {
			if (length(x) == 0 || is.na(x[1])) return(NA_character_)
			x <- x[1]
			if (inherits(x, "POSIXt") || inherits(x, "Date")) return(as.character(as.Date(x)))
			x <- trimws(as.character(x))
			if (!nzchar(x)) return(NA_character_)
			if (grepl("^[0-9]+$", x)) return(as.character(as.Date(as.numeric(x), origin="1899-12-30")))
			as.character(as.Date(x))
		}
		clean_tex <- function(x) {
			x <- trimws(gsub("[[:space:]]+", " ", gsub("\u00a0", " ", as.character(x))))
			known <- c("CLAY SLIT" = "silty clay", "FRANK CLAY SLIT" = "silty clay loam", "Loamy sand soil" = "loamy sand")
			unname(known[x])
		}
		grab <- function(col, name) as.numeric(r3[[col]][r3$Abbreviature == name][1])
		grab_tex <- function(col) r3[[col]][r3$Abbreviature == "Soil texture"][1]

		cyc <- setNames(blank(r1$Cycle), trimws(as.character(r1$Accession_Number)))
		cycle <- unname(cyc[d$variety_code])
		obs <- blank(r$OBS)
		cycle[is.na(cycle)] <- obs[is.na(cycle)]

		d$treatment <- d$variety
		d$selection_cycle_ <- cycle
		d$tuber_ascorbate_dw_ <- as.numeric(r$ASC_DW)
		d$vitamin_C <- as.numeric(r$ASC_FW)
		d$adm1 <- carobiner::fix_name(d$adm1, "title")
		d$adm2 <- carobiner::fix_name(d$adm2, "title")
		d$adm3 <- carobiner::fix_name(d$adm3, "title")
		d$location <- carobiner::fix_name(d$location, "title")

		pdt <- as_date(r4$Date[r4$Intervention_type == "Planting"][1])
		hdt <- as_date(r4$Date[r4$Intervention_type == "Harvest"][1])
		if (!is.na(pdt)) d$planting_date <- pdt
		if (!is.na(hdt)) d$harvest_date <- hdt

		if (grepl("LAVICT", basename(f))) {
			cols <- paste0("Data", 1:4)
			j <- match(d$rep, 1:4)
			d$soil_pH <- vapply(cols, function(col) grab(col, "Soil PH"), numeric(1))[j]
			d$soil_SOM <- vapply(cols, function(col) grab(col, "Organic matter"), numeric(1))[j]
			d$soil_P <- vapply(cols, function(col) grab(col, "Phosphorus"), numeric(1))[j]
			d$soil_K <- vapply(cols, function(col) grab(col, "Potassium"), numeric(1))[j]
			d$soil_sand <- vapply(cols, function(col) grab(col, "Sand"), numeric(1))[j]
			d$soil_silt <- vapply(cols, function(col) grab(col, "Silt"), numeric(1))[j]
			d$soil_clay <- vapply(cols, function(col) grab(col, "Clay"), numeric(1))[j]
			d$soil_texture <- clean_tex(vapply(cols, grab_tex, character(1)))[j]
		} else {
			d$soil_texture <- clean_tex(grab_tex("Data1"))
		}
		d$on_farm <- FALSE
		d$irrigated <- NA
		d
	}

	f1 <- ff[basename(ff) == "PTVitaminC012016_LAVICT.xls"]
	f2 <- ff[basename(ff) == "PTVitaminC052015_ACO.xls"]
	d <- carobiner::bindr(one(f1), one(f2))
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
