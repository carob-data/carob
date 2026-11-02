# R script for "carob"
# license: GPL (>=3)

## NOTES
# Compilation of literature means and eRcane network means (Type_Source Review or eRcane). Each source row is split into sugarcane monoculture and sugarcane with the legume.
# Yield_SC_Mono and Yield_SC_IC are not labeled. Values 24-193 match tonnes of cane per hectare and are multiplied by 1000.
# Country names are the Country column, with South-Africa and Reunion aligned to terminag. Reunion in the file is stored as Réunion.
# Publication DOI is the Dataverse publicationIDNumber 10.1016/j.fcr.2023.108882.

## ISSUES
# No coordinates or harvest date. Many Year values are missing. Yields are study means, and some source rows repeat.
# Applied_N_R has no unit, so it is not N_fertilizer. P and K are absent. Cane yield is assumed t/ha and some values exceed 150 t/ha.
# write_files: missing yield_moisture, geo_from_source, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date; out of bounds yield (24300, 192854.938); duplicate records detected.

## NOT PROCESSED
# SD_Yield_SC_Mono, SD_Yield_SC_IC: standard deviations of the means
# Rec_Global, Rec_MH, Rec_PDS, I_PDS_IR_CYC, I_MH_IR_CYC, Ratio_PDS: derived indices
# Applied_N_R: unit not stated
# Initial_soil_C, Row_Distance, Row_Arr, climate normals: unit or code not defined in the file

carob_script <- function(path) {

"
Sugarcane yield response to legume intercropped: data from experimental network and review

Intercropping with legumes has attracted much attention worldwide, regarded as a sustainable alternative to chemical N-fertilizer. Many studies have investigated the effects of introducing legumes into sugarcane systems. However, how agro-environmental conditions influence the effect of legumes on sugarcane yield is still unclear. These data were compiled to performed a meta-analysis which i) quantify the effect of legume intercropping on sugarcane yield, ii) assess how agro-environmental and management conditions influence the response of sugarcane yield to intercropping, and iii) assess the influence of legumes in interaction with weeds on sugarcane yield. We combined data from a systematic literature review and experimental data from a network of trials conducted in the tropical island of La Reunion.
"

	uri <- "doi:10.18167/DVN1/RR8FQX"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = "doi:10.1016/j.fcr.2023.108882",
		project = NA,
		design = NA,
		data_type = "compilation",
		treatment_vars = "intercrops",
		response_vars = "yield",
		carob_LLM = "Cursor Grok 4.7",
		carob_date = "2026-10-31",
		carob_completion = 45,
		carob_effort = 0.7
	)

	f <- ff[basename(ff) == "DB_VF.csv"]
	r <- read.csv(f, check.names=FALSE, fileEncoding="latin1")

	leg <- trimws(r$Crop)
	leg[leg == "Mucuna pruriens"] <- "velvet bean"
	leg[leg == "Cajanus cajan"] <- "pigeon pea"
	leg[leg == "Canavalia ensiformis"] <- "jack bean"
	leg[leg == "Arachis hypogaea"] <- "groundnut"
	leg[leg == "Phaseolus lunatus"] <- "lima bean"
	leg[leg == "Phaseolus vulgaris"] <- "common bean"
	leg[leg == "Glycine max"] <- "soybean"
	leg[leg == "Sesbania rostrata"] <- "sesbania"
	leg[leg == "Vigna radiata"] <- "mung bean"
	leg[leg == "Cyamopsis tetragonoloba"] <- "guar"
	leg[leg == "Vigna mungo"] <- "black gram"
	leg[leg == "Cicer arietinum"] <- "chickpea"
	leg[leg == "Lens culinaris"] <- "lentil"
	leg[leg == "Pisum sativum"] <- "pea"
	leg[leg == "Desmodium intortum"] <- "desmodium"
	leg[leg == "Crotolaria juncea"] <- "sunn hemp"
	leg[substr(leg, 1, 11) == "Crotolaria " & leg != "sunn hemp"] <- "crotalaria"
	leg[substr(leg, 1, 17) == "Vigna unguiculata"] <- "cowpea"

	country <- trimws(r$Country)
	country[country == "South-Africa"] <- "South Africa"
	country[country == "Reunion"] <- "Réunion"
	# Country column. Reunion is the spelling in the file.

	base <- data.frame(
		country = country,
		location = trimws(gsub(".", ", ", r$Localisation, fixed=TRUE)),
		elevation = as.numeric(r$Altitude),
		crop = "sugarcane",
		variety = trimws(r$SC_Cultivar),
		planting_date = ifelse(is.na(r$Year), NA_character_, as.character(r$Year)),
		on_farm = r$Study_type == "on-farm",
		is_survey = FALSE,
		irrigated = r$Irrigation == "Y",
		yield_part = "stems",
		trial_id = paste(trimws(r$Data_source), r$Trial, r$Year),
		rain = as.numeric(r$Actual_rainfall)
	)
	# Actual_rainfall has no unit; values are in the range of mm.

	d1 <- base
	d1$yield <- as.numeric(r$Yield_SC_Mono) * 1000
	d1$intercrops <- "none"
	d1$treatment <- "monoculture"

	d2 <- base
	d2$yield <- as.numeric(r$Yield_SC_IC) * 1000
	d2$intercrops <- leg
	d2$treatment <- "intercrop"

	d <- rbind(d1, d2)
	d <- d[!is.na(d$yield), ]

	carobiner::write_files(path, meta, d)
}
