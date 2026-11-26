# R script for "carob"
# license: GPL (>=3)

## NOTES
# Two diploid-potato yield trials (La Victoria, Huancayo, Junin and Aco, Paucartambo, Pasco, Peru), both RCBD with four replicates and 2.7 m2 plots.
# Processed using shared CIP HIDAP helper process_cip_lbvars(), which reads Minimal, Installation, Fieldbook (yield, plant_density, tuber_density), internal Material_List (variety, parents), and Soil_analysis (pH, SOM, P, K, sand, silt, clay); augmented here with Crop_management planting/harvest dates, selection_cycle_, and soil_texture.
# Aco soil_P is 299.38 mg/kg (above terminag max 200; kept as recorded).

## ISSUES
# No fertilizer rate, irrigation statement, or yield moisture reported.
# Some plots have NA yield.
# Soil_P in Aco is 299.38 mg/kg (above terminag max 200); kept as recorded.

## NOT PROCESSED
# Weather_data (15-minute HOBO, rain has no unit), Crop_management sprays, total N %, other soil nutrients, Summary sheet, Data_dictionary.xls.

## NEW VARIABLES
# selection_cycle_: recurrent-selection cycle on the material list (Cycle_0, Cycle_I, Cycle_2, Cycle_3).


carob_script <- function(path) {

"
Dataset for: Genetic gains in the content of \"Fe\" and \"Zn” in diploid potato for three cycles recurrent selection (Yield)

The purpose of this study was to estimate the genetic gains of three cycles of recurrent selection for the Zn and Fe content in potato diploids, and to determine the heritability and relationships with variables of agronomical importance. One trial was in La Victoria, Huancayo, Junin (61 genotypes: 16 cycle 0, 2 cycle 1, 21 cycle 2, 22 cycle 3). The other was in Aco, Paucartambo, Pasco (64 genotypes: 17 cycle 0, 4 cycle 1, 21 cycle 2, 22 cycle 3). Both were randomized complete block designs with four repetitions.
"

	uri <- "doi:10.21223/P3/L4PFIU"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP",
		publication = NA,
		project = "Advancing Achievements in Breeding for Early, Resilient and Nutritious Potato and Sweetpotato",
		design = "Randomized Complete Block Design (RCBD)",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield",
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
		r1 <- carobiner::read.excel(f, sheet="Material_List")
		r3 <- carobiner::read.excel(f, sheet="Soil_analysis")
		r4 <- carobiner::read.excel(f, sheet="Crop_management")

		cyc <- setNames(trimws(as.character(r1$Cycle)), trimws(as.character(r1$Accession_Number)))
		cyc[!nzchar(cyc)] <- NA_character_
		tex <- trimws(as.character(r3$Data1[r3$Abbreviature == "Soil texture"][1]))
		tex <- unname(c("CLAY-SILT" = "silty clay", "Loamy sand soil" = "loamy sand")[tex])

		d$treatment <- d$variety
		d$selection_cycle_ <- unname(cyc[d$variety_code])
		d$adm1 <- carobiner::fix_name(d$adm1, "title")
		d$adm2 <- carobiner::fix_name(d$adm2, "title")
		d$adm3 <- carobiner::fix_name(d$adm3, "title")
		d$location <- carobiner::fix_name(d$location, "title")
		d$planting_date <- as.character(as.Date(r4$Date[r4$Intervention_type == "Planting"][1]))
		d$harvest_date <- as.character(as.Date(r4$Date[r4$Intervention_type == "Harvest"][1]))
		d$soil_texture <- tex
		d$yield_moisture <- NULL
		d$on_farm <- FALSE
		d$irrigated <- NA
		d
	}

	f1 <- ff[basename(ff) == "PTYield012016_LAVICT.xls"]
	f2 <- ff[basename(ff) == "PTYield052015_ACO.xls"]
	d <- carobiner::bindr(one(f1), one(f2))
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
