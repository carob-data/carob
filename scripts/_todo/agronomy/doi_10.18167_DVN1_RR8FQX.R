# R script for "carob"
# license: GPL (>=3)

## NOTES
# Compilation of 315 paired sugarcane monoculture vs sugarcane-legume intercropping study means from a global systematic literature review and the eRcane experimental network in Réunion (Viaud et al., 2023, Field Crops Research 295:108882, doi:10.1016/j.fcr.2023.108882).
# Each source row provides paired monoculture (Yield_SC_Mono) and legume-intercropped (Yield_SC_IC) fresh cane yields in t/ha (24.3-192.9 t/ha -> * 1000 for kg/ha, yield_part = "stems", yield_isfresh = TRUE). Monoculture controls repeated across multiple legume treatments within the same trial are deduplicated to avoid duplicate records.
# Applied_N_R (0-525, median 160 kg N/ha per Viaud et al., 2023) is mapped to N_fertilizer; Initial_soil_C (3.5-33 g/kg -> / 10 for %) is mapped to soil_SOC; Row_Distance (0.45-3.6 m -> * 100 for cm) is mapped to row_spacing; Actual_rainfall (mm) is mapped to rain.
# All 36 study locations across 10 countries are georeferenced to approximate site/city coordinates (geo_from_source = FALSE, geo_uncertainty = 15000 m); author should verify any exact station coordinates if needed.
# Country names align to terminag ("South-Africa" -> "South Africa", "Reunion" -> "Réunion").

## ISSUES
# Fresh sugarcane stem yields reach 192,855 kg/ha (192.9 t/ha), which exceeds terminag's yield upper bound (150,000 kg/ha) for high-yielding irrigated sugarcane; kept as reported.
# Wide-row dual-line sugarcane row spacings reach 360 cm (> terminag's row_spacing upper bound of 180 cm); kept as reported.
# Records are study/treatment means (with replicate counts rep_n_ and standard deviations yield_sd_), not individual plots; P_fertilizer, K_fertilizer, yield_moisture, and harvest_date are not in DB_VF.csv.
# Ten rows have Year = NA (so planting_date is NA).

## NEW VARIABLES
# study_source_: literature citation or eRcane trial code (Data_source)
# source_type_: "Review" (literature compilation) or "eRcane" (Réunion experimental network)
# crop_cycle_: sugarcane cycle ("P" = plant cane, "R1"-"R6" = ratoon 1-6)
# soil_texture_class_: broad soil texture class ("Coarse", "Medium", "Fine")
# legume_planting_dap_: days after sugarcane planting/ratooning when legume was sown (days)
# legume_destruction_dap_: days after sugarcane planting/ratooning when legume was terminated/harvested (days)
# cohabitation_days_: duration of sugarcane-legume cohabitation (days)
# yield_sd_: standard deviation of treatment-mean cane yield (kg/ha)
# rep_n_: number of replicates contributing to the reported mean
# rain_annual_mean_: long-term mean annual rainfall at the site (mm)
# temp_annual_mean_: long-term mean annual temperature at the site (C)
# temp_season_mean_: actual growing-season mean temperature (C)

## NOT PROCESSED
# DB_VF.csv: derived meta-analysis indices (I_PDS_IR_CYC, I_MH_IR_CYC, Ratio_PDS, Rec_Global, Rec_MH, Rec_PDS); redundant grouping columns (Ref, Subject, Id, Id1, Years_IC, Continent, Cycle2, Crop_Class_1-3, Climat_Koppen_1-3); Row_Arr (Excel time-corrupted ratio strings such as "01:02:00").


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
		carob_LLM = "Gemini 4 Neon",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_completion = 85,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "DB_VF.csv"]
	r <- read.csv(f, check.names=FALSE, fileEncoding="latin1")

	leg <- trimws(as.character(r$Crop))
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

	country <- trimws(as.character(r$Country))
	country[country == "South-Africa"] <- "South Africa"
	country[country == "Reunion"] <- "Réunion"

	# Hard-coded coordinates for the 36 unique study locations (estimated from city/region centers; geo_from_source = FALSE)
	geo <- data.frame(
		loc_raw = c(
			"Burdekin region. Australia", "Sunshine Coast. Australia",
			"Campos dos Goytacazes. Brazil", "Campos. Brazil", "Piracicaba. Brazil",
			"Santa Cruz. Brazil", "Seropédica. Brazil",
			"Dingdang town. China", "Guangzhou. China", "Wuming. China", "Xixiangtang. China",
			"Assayta. Ethiopia",
			"Ariyalur disctrict. India", "Coimbatore. India", "Dapoli. India", "Jalandhar. India",
			"Karnal. India", "Ludhiana. India", "Sameerwadi. India", "Vuyyuru. Krishna District. India",
			"Tabasco.México", "Badeggi. Nigeria",
			"Faisabaland. Pakistan", "Faisalabad. Pakistan", "Islamabad. Pakistan", "Sargodha. Pakistan",
			"Crescence. Sainte-Marie. Reunion", "Fontaine. Saint-Benoît",
			"P25 Montagne. Sainte-Marie. Reunion", "P25. Sainte-Marie. Reunion",
			"P25ITK. Sainte-Marie. Reunion", "P41. Sainte-Marie. Reunion",
			"Amatikulu. South-Africa", "Boschfontein. South-Africa",
			"Eston. South-Africa", "Sezela. South-Africa"
		),
		latitude = c(
			-19.58, -26.65,
			-21.75, -21.75, -22.73,
			-22.92, -22.74,
			23.17, 23.13, 23.16, 22.84,
			11.57,
			11.14, 11.02, 17.76, 31.33,
			29.69, 30.90, 16.33, 16.37,
			17.99, 9.06,
			31.45, 31.45, 33.68, 32.08,
			-20.90, -21.03,
			-20.90, -20.90,
			-20.90, -20.90,
			-29.05, -25.73,
			-29.88, -30.40
		),
		longitude = c(
			147.41, 153.07,
			-41.32, -41.32, -47.65,
			-43.69, -43.71,
			108.00, 113.26, 108.28, 108.31,
			41.44,
			79.08, 76.96, 73.19, 75.58,
			76.99, 75.86, 75.11, 80.85,
			-92.93, 6.14,
			73.14, 73.14, 73.05, 72.67,
			55.55, 55.71,
			55.55, 55.55,
			55.55, 55.55,
			31.53, 31.65,
			30.52, 30.68
		)
	)

	loc_raw <- trimws(as.character(r$Localisation))
	gidx <- match(loc_raw, geo$loc_raw)

	base <- data.frame(
		trial_id = paste(trimws(r$Data_source), r$Trial, ifelse(is.na(r$Year), "NA", r$Year), sep="_"),
		study_source_ = trimws(as.character(r$Data_source)),
		source_type_ = trimws(as.character(r$Type_Source)),
		country = country,
		location = trimws(gsub(".", ", ", loc_raw, fixed=TRUE)),
		latitude = geo$latitude[gidx],
		longitude = geo$longitude[gidx],
		geo_from_source = FALSE,
		geo_uncertainty = 15000,
		elevation = as.numeric(r$Altitude),
		crop = "sugarcane",
		variety = trimws(as.character(r$SC_Cultivar)),
		crop_cycle_ = trimws(as.character(r$Cycle)),
		planting_date = ifelse(is.na(r$Year), NA_character_, as.character(r$Year)),
		# Row_Distance in m -> cm
		row_spacing = as.numeric(r$Row_Distance) * 100,
		# Applied_N_R in kg N/ha (Viaud et al., 2023)
		N_fertilizer = as.numeric(r$Applied_N_R),
		# Initial_soil_C in g/kg -> %
		soil_SOC = as.numeric(r$Initial_soil_C) / 10,
		soil_texture_class_ = trimws(as.character(r$Texture)),
		rain = as.numeric(r$Actual_rainfall),
		rain_annual_mean_ = as.numeric(r$Average_rainfall),
		temp_annual_mean_ = as.numeric(r$Average_Temp),
		temp_season_mean_ = as.numeric(r$Actual_Temp),
		on_farm = r$Study_type == "on-farm",
		is_survey = FALSE,
		irrigated = r$Irrigation == "Y",
		yield_part = "stems",
		yield_isfresh = TRUE
	)

	# Monoculture controls (deduplicated within trial x management x yield to avoid repeating shared controls)
	d1 <- base
	d1$yield <- as.numeric(r$Yield_SC_Mono) * 1000
	d1$yield_sd_ <- as.numeric(r$SD_Yield_SC_Mono) * 1000
	d1$rep_n_ <- as.integer(r$n_Mono)
	d1$intercrops <- "none"
	d1$intercropped <- FALSE
	d1$treatment <- "monoculture"
	d1$legume_planting_dap_ <- NA_real_
	d1$legume_destruction_dap_ <- NA_real_
	d1$cohabitation_days_ <- NA_real_
	d1 <- d1[!duplicated(d1[, c("trial_id", "location", "variety", "crop_cycle_", "N_fertilizer", "yield")]), ]

	# Legume-intercropped treatments
	d2 <- base
	d2$yield <- as.numeric(r$Yield_SC_IC) * 1000
	d2$yield_sd_ <- as.numeric(r$SD_Yield_SC_IC) * 1000
	d2$rep_n_ <- as.integer(r$n_IC)
	d2$intercrops <- leg
	d2$intercropped <- TRUE
	d2$treatment <- paste0("intercrop_", trimws(as.character(r$Crop)))
	d2$legume_planting_dap_ <- as.numeric(r$Planting_DAP)
	d2$legume_destruction_dap_ <- as.numeric(r$Destruction_DAP)
	d2$cohabitation_days_ <- as.numeric(r$Cohabitation)

	d <- rbind(d1, d2)
	d <- d[!is.na(d$yield), ]
	d$plot_id <- paste(d$trial_id, seq_len(nrow(d)), sep="_")

	carobiner::write_files(path, meta, d)
}
