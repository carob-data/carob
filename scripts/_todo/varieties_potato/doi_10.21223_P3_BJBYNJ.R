# R script for "carob"
# license: GPL (>=3)

## NOTES
# Two diploid-potato mineral trials (La Victoria, Huancayo, Junin and Aco, Paucartambo, Pasco, Peru), both RCBD with four replicates and 2.7 m2 plots.
# Processed using shared CIP HIDAP helper process_cip_lbvars(), then augmented with Crop_management planting/harvest dates, Material_List selection_cycle_, Soil_analysis texture, and tuber Fe/Zn measurements.
# FeDW and ZnDW (mg/kg dry weight) are converted to mg/g DW (/ 1000) for standard terminag tuber_Fe and tuber_Zn; fresh-weight FeFW/ZnFW (mg/100g FW) are kept in tuber_Fe_fw_ and tuber_Zn_fw_.
# AVDM (%) is stored as yield_moisture = 100 - AVDM (computed by process_cip_lbvars()).
# Soil texture "CLAY-SILT" is stored as silty clay; "Loamy sand soil" as loamy sand. Aco soil_P is 299.38 mg/kg (above terminag max 200; kept as recorded).

## ISSUES
# No tuber yield, fertilizer rate, or irrigation statement.
# Aco soil_P is 299.38 mg/kg (above terminag max 200); kept as recorded.

## NOT PROCESSED
# Weather_data: 15-minute HOBO records (rain column has no unit).
# Crop_management sprays and hilling (no nutrient rate per hectare).
# Total N %, other soil micronutrients, Fieldbook OBS/CODE_LAB, Summary, Original_Lab, Analysis, Histogramas, Data_dictionary.xls.

## NEW VARIABLES
# selection_cycle_: recurrent-selection cycle on the material list (Cycle_0, Cycle_I, Cycle_2, Cycle_3).
# tuber_Fe_fw_: iron concentration (mg/100g fresh weight).
# tuber_Zn_fw_: zinc concentration (mg/100g fresh weight).


carob_script <- function(path) {

"
Dataset for: Genetic gains in the content of \"Fe\" and \"Zn” in diploid potato for three cycles recurrent selection (Minerals)

The purpose of this study was to estimate the genetic gains of three cycles of recurrent selection for the Zn and Fe content in diploid potatoes, and to determine the heritability and relationships with variables of agronomical importance (minerals). The field evaluation was conducted in La Victoria, Huancayo, Junin, Peru in a Randomized Complete Block Design, with four repetitions, evaluating 61 genotypes (16=cycle0, 2=cycle1, 21=cycle2, 22=cycle3). And in Acco, Paucartambo, Pasco, Peru in a Randomized Complete Block Design, with four repetitions, evaluating 55 genotypes (15=cycle0, 0=cycle1, 20=cycle2, 20=cycle3).
"

	uri <- "doi:10.21223/P3/BJBYNJ"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP",
		publication = NA,
		project = "Advancing Achievements in Breeding for Early, Resilient and Nutritious Potato and Sweetpotato; Bio-fortified potato varieties to help overcome micronutrient malnutrition in East Africa",
		design = "Randomized Complete Block Design (RCBD)",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield_moisture;tuber_Fe;tuber_Zn",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 75,
		carob_effort = 0
	)

	proc <- carobiner::get_function("process_cip_lbvars", path, group)

	one <- function(f) {
		d <- proc(f)
		r <- carobiner::read.excel(f, sheet="Fieldbook")
		r1 <- carobiner::read.excel(f, sheet="Material_List")
		r3 <- carobiner::read.excel(f, sheet="Soil_analysis")
		r4 <- carobiner::read.excel(f, sheet="Crop_management")

		cyc <- setNames(trimws(as.character(r1$Cycle)), trimws(as.character(r1$Accession_Number)))
		cyc[!nzchar(cyc)] <- NA_character_
		tex <- trimws(gsub("[[:space:]]+", " ", gsub("\u00a0", " ", as.character(r3$Data1[r3$Abbreviature == "Soil texture"][1]))))
		tex <- unname(c("CLAY-SILT" = "silty clay", "Loamy sand soil" = "loamy sand")[tex])

		d$treatment <- d$variety
		d$selection_cycle_ <- unname(cyc[d$variety_code])
		# FeDW and ZnDW in mg/kg DW -> mg/g DW (terminag standard)
		d$tuber_Fe <- as.numeric(r$FeDW) / 1000
		d$tuber_Fe_fw_ <- as.numeric(r$FeFW)
		d$tuber_Zn <- as.numeric(r$ZnDW) / 1000
		d$tuber_Zn_fw_ <- as.numeric(r$ZnFW)
		d$adm1 <- carobiner::fix_name(d$adm1, "title")
		d$adm2 <- carobiner::fix_name(d$adm2, "title")
		d$adm3 <- carobiner::fix_name(d$adm3, "title")
		d$location <- carobiner::fix_name(d$location, "title")
		d$planting_date <- as.character(as.Date(r4$Date[r4$Intervention_type == "Planting"][1]))
		d$harvest_date <- as.character(as.Date(r4$Date[r4$Intervention_type == "Harvest"][1]))
		d$soil_texture <- tex
		d$on_farm <- FALSE
		d$irrigated <- NA
		d
	}

	f1 <- ff[basename(ff) == "PTMinerals012016_LAVICT.xls"]
	f2 <- ff[basename(ff) == "PTMinerals052015_ACO.xls"]
	d <- carobiner::bindr(one(f1), one(f2))
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
