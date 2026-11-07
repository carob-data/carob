# R script for "carob"
# license: GPL (>=3)

## NOTES
# Two diploid-potato yield trials, both RCBD with four replicates and 2.7 m2 plots (0.30 m between plants, 0.90 m between rows, 10 plants).
# La Victoria, Huancayo, Junin: planted 2015-12-22, harvested 2016-06-01. Aco, Paucartambo, Pasco: planted 2015-05-03, harvested 2015-10-12.
# Those dates are the Crop_management rows labeled Planting and Harvest. The La Victoria Minimal begin and end serials are 2016-01-19 and 2016-06-09.
# TTYNA is t/ha from the harvested plot. TTYA is the same yield adjusted up where fewer than the planted plants were harvested, so it is not used.
# Yield is TTYNA * 1000. Variety is the accession name, otherwise the accession code, otherwise the institutional number.
# Soil texture "CLAY-SILT" (11% sand, 45% silt, 44% clay) is stored as silty clay. "Loamy sand soil" (75% sand, 16% silt, 9% clay) is stored as loamy sand.
# Phosphorus and potassium are ppm on the soil sheet and are stored as mg/kg. Aco phosphorus is 299 mg/kg. The vocabulary maximum for soil_P is 200.
# The description spells the Pasco site Acco. The locality field says Aco.

## ISSUES
# No fertilizer rate and no irrigation statement.
# No yield moisture.
# Junin and San Agustin should be checked against GADM (Junín; the district spelling).
# write_files: missing yield_moisture, irrigated, K_fertilizer, N_fertilizer, P_fertilizer. unknown variable selection_cycle_. out of bounds soil_P (27.3, 299.38). NA in yield.

## NOT PROCESSED
# Weather_data: 15-minute HOBO records. The rain column has no unit.
# Crop_management sprays: product doses are ml or g per tank, with no area.
# Nitrogen total is a percentage. soil_N is extractable N in mg/kg, so the percentage is not stored.
# Soil nutrients other than pH, organic matter, texture fractions, and available P and K.
# ATW: average tuber weight, unit not labeled.
# TTYA, TTWP, TTWPL, TNTPL, PPH, and the Aco marketable-count columns. The yield uses TTYNA.
# Summary sheet.
# Data_dictionary.xls: documentation or a codebook, not an observation table.


## NEW VARIABLES
# selection_cycle_: recurrent-selection cycle on the material list (Cycle_0, Cycle_I, Cycle_2, Cycle_3). Cycle_I is the source spelling.


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
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.7
	)

	one <- function(f) {
		r <- carobiner::read.excel(f, sheet="Fieldbook")
		r1 <- carobiner::read.excel(f, sheet="Material_List")
		r2 <- carobiner::read.excel(f, sheet="Minimal")
		r3 <- carobiner::read.excel(f, sheet="Soil_analysis")
		r4 <- carobiner::read.excel(f, sheet="Crop_management")
		v <- setNames(trimws(as.character(r2$Value)), trimws(r2$Factor))
		soil <- function(name) as.numeric(r3$Data1[r3$Abbreviature == name][1])
		tex <- trimws(as.character(r3$Data1[r3$Abbreviature == "Soil texture"][1]))
		tex <- c("CLAY-SILT" = "silty clay", "Loamy sand soil" = "loamy sand")[tex]
		key <- trimws(as.character(r1$Accession_Number))
		nm <- trimws(as.character(r1$Accession_Name))
		nm[!nzchar(nm)] <- NA_character_
		code <- trimws(as.character(r1$Accession_Code))
		code[!nzchar(code)] <- NA_character_
		instn <- trimws(as.character(r$INSTN))
		i <- match(instn, key)
		variety <- nm[i]
		variety[is.na(variety)] <- code[i][is.na(variety)]
		variety[is.na(variety)] <- instn[is.na(variety)]
		blank <- function(x) {
			x <- trimws(as.character(x))
			x[!nzchar(x)] <- NA_character_
			x
		}
		plot_area <- 2.7
		data.frame(
			trial_id = v[["Trial_name"]],
			crop = "potato",
			variety = variety,
			variety_code = instn,
			treatment = variety,
			selection_cycle_ = blank(r1$Cycle[i]),
			female_parent = blank(r1$Female_AcceNumb[i]),
			male_parent = blank(r1$Male_AcceNumb[i]),
			rep = as.integer(r$REP),
			plot_id = as.character(r$PLOT),
			yield = as.numeric(r$TTYNA) * 1000,
			yield_part = "tubers",
			yield_isfresh = TRUE,
			plot_area = plot_area,
			plant_spacing = 30,
			row_spacing = 90,
			plant_density = as.numeric(r$NPH) / plot_area * 10000,
			country = "Peru",
			adm1 = carobiner::fix_name(v[["Admin1"]], "title"),
			adm2 = carobiner::fix_name(v[["Admin2"]], "title"),
			adm3 = carobiner::fix_name(v[["Admin3"]], "title"),
			location = carobiner::fix_name(v[["Locality"]], "title"),
			elevation = as.numeric(v[["Elevation"]]),
			latitude = as.numeric(v[["Latitude"]]),
			longitude = as.numeric(v[["Longitude"]]),
			planting_date = as.character(as.Date(r4$Date[r4$Intervention_type == "Planting"][1])),
			harvest_date = as.character(as.Date(r4$Date[r4$Intervention_type == "Harvest"][1])),
			soil_pH = soil("Soil PH"),
			soil_SOM = soil("Organic matter"),
			soil_P = soil("Phosphorus"),
			soil_K = soil("Potassium"),
			soil_sand = soil("Sand"),
			soil_silt = soil("Silt"),
			soil_clay = soil("Clay"),
			soil_texture = as.character(tex),
			on_farm = FALSE,
			is_survey = FALSE,
			geo_from_source = TRUE
		)
	}

	f1 <- ff[basename(ff) == "PTYield012016_LAVICT.xls"]
	f2 <- ff[basename(ff) == "PTYield052015_ACO.xls"]
	d <- rbind(one(f1), one(f2))
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
