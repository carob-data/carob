# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Ludhiana BIOMASS (t/ha) does not state fresh vs dry basis; stored as y_total (kg/ha).
# Ludhiana topsoil (0-15 cm) NH4 and NO3 are reported in kg/ha without bulk density; converted to mg/kg by dividing by 2.24 (standard 0-15 cm furrow-slice factor, BD ~1.49 g/cm3, consistent with the 0.25088 = 2.24 * 0.112 titration increments in the raw values).

## MISSING
# P_fertilizer, K_fertilizer, yield_moisture, and harvest_date are not reported in the files.

## NOTES
# Three 2024/25 BNI (T3BL.3NsbS) spring-wheat yield trials in South Asia (CropSustaiN project):
# Bhairahawa (NWRP, Nepal), Islamabad (NARC, Pakistan), and Ludhiana (BISA, India), each 8 lines x 3 N rates (0/75/150 kg N/ha) x 3 reps = 72 plots (216 total).
# BNI_Nepal_2025.xlsx duplicates Bhairahawa yield components and is not used.
# For Pakistan, Yield Trial + Islamabad 2024_25, PAKISTAN.xlsx ("Agronomic data" & "Gen. info.") is combined with the matching 72-plot sheet "Agronomic Data" in BNI data_Final_ Pakistan 2024-25.xlsx (which adds grain N, protein, and 4 dated soil-N batches).
# Islamabad Yellow rust scores follow the Modified Cobb scale (e.g. 80M, 90S); stored as disease = "yellow rust" and disease_severity.
# Repeated canopy observations (ground_cover, NDVI, canopy_temperature, SPAD) and multi-date soil N analyses (NO3, NH4, total N, pH, SOM, mineral N) are stored in the long table.
# Site coordinates are estimated from the station names (not reported in the files).

## NOT PROCESSED
# BNI_Nepal_2025.xlsx: duplicates Bhairahawa yield components already in Yield Trial + Bhairahawa 2024_25 NEPAL.xlsx.
# Bhairahawa: Dats to Booting (days to booting).
# Islamabad: No. of Spikelets Average; N uptake (derived as yield * grain_N / 1000).
# Ludhiana: FLGLF (flag leaf length, cm), FLGLFW (flag leaf width, cm), LOI (lodging %, all 0).
# Field layout / map sheets in all three workbooks.


carob_script <- function(path) {

"
Yield trial data of CIMMYT Elite Spring Wheat lines with the BNI short arm translocation (T3BL.3NsbS) from Leymus racemosus in South Asia

Yield trial data of CIMMYT Elite Spring Wheat lines with the BNI short arm translocation in South Asia, 2024/25.
"

	uri <- "hdl:11529/10549335"
	group <- "varieties_wheat"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "CIMMYT; NARC",
		publication = NA,
		project = "CropSustaiN",
		design = "split-plot / RCBD; 8 wheat lines (4 BNI translocation + 4 recurrent parents) x 3 N rates (0/75/150 kg/ha) x 3 reps",
		data_type = "on-station experiment",
		treatment_vars = "variety;N_fertilizer",
		response_vars = "yield;y_total;heading_days;flowering_days;maturity_days;plant_height;panicle_length;spike_density;grains_per_spike;seed_weight;germination_rate;grain_N;grain_protein;ground_cover;NDVI;canopy_temperature;SPAD;disease_severity;soil_NO3;soil_NH4;soil_N_min_total",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_contributor = "Illiana Kwenda",
		carob_completion = 90,
		carob_effort = 1
	)

	# BNI lines carry CSMONO3B / Lr#n / BNI in the pedigree/name
	bni_type <- function(x) {
		ifelse(grepl("CSMONO3B|Lr#n|BNI", x, ignore.case=TRUE), "improved", "check")
	}
	num <- function(x) {
		s <- trimws(as.character(x))
		s[!nzchar(s) | s %in% c("NA", "n.a", "-")] <- NA_character_
		as.numeric(s)
	}
	xl_date <- function(x) {
		as.character(as.Date(as.numeric(x), origin="1899-12-30"))
	}

	## 1. Nepal, Bhairahawa (NWRP)
	# col_types="text" avoids readxl mixed date/numeric warnings from row 1 measurement dates
	f1 <- ff[basename(ff) == "Yield Trial + Bhairahawa 2024_25 NEPAL.xlsx"]
	r1_all <- carobiner::read.excel(f1, sheet="BNI trial CropSustaiN", col_types="text")
	hdr_dates <- unlist(r1_all[1, ])
	r1 <- r1_all[!is.na(r1_all$Genotypes), ]
	d1 <- data.frame(
		country = "Nepal",
		location = "Bhairahawa",
		plot_id = as.character(r1$PN),
		variety = trimws(r1$Genotypes),
		variety_code = as.character(r1[["ENTRY NO."]]),
		N_fertilizer = num(r1[["Nitrogen Level"]]),
		rep = as.integer(gsub("[^0-9]", "", r1$Rep)),
		planting_date = xl_date(r1[["Days of Sowing"]]),
		germination_rate = num(r1[["Emergence %"]]),
		heading_days = num(r1[["Days to Heading"]]),
		maturity_days = num(r1[["Dats to Maturity"]]),
		plant_height = num(r1[["Plant Height (cm)"]]),
		panicle_length = num(r1[["Spike Length (cm)"]]),
		spike_density = num(r1[["No. of spikes/m2"]]) * 10000,
		grains_per_spike = num(r1[["No. of grains/spike"]]),
		seed_weight = num(r1[["TGW (g)"]]),
		yield = num(r1[["GY(kg/ha)"]]),
		irrigated = NA,
		latitude = 27.518,
		longitude = 83.455,
		geo_uncertainty = 5000,
		geo_source = "estimated from Bhairahawa NWRP"
	)

	## 2. Pakistan, Islamabad (NARC)
	f2a <- ff[basename(ff) == "Yield Trial + Islamabad 2024_25, PAKISTAN.xlsx"]
	f2b <- ff[basename(ff) == "BNI data_Final_ Pakistan 2024-25.xlsx"]
	r2a <- carobiner::read.excel(f2a, sheet="Agronomic data", col_types="text")
	r2a <- r2a[!is.na(r2a[["Gtp name"]]), ]
	r2b <- carobiner::read.excel(f2b, sheet="Agronomic Data", skip=1, col_types="text")
	r2b <- r2b[!is.na(r2b[["Gtp name"]]), ]
	if (!identical(as.character(r2a$Plot), as.character(r2b$Plot))) stop("Islamabad plot mismatch")
	n2 <- num(gsub("[^0-9]", "", r2a[["N rate"]]))
	d2 <- data.frame(
		country = "Pakistan",
		location = "Islamabad",
		plot_id = as.character(r2a$Plot),
		variety = trimws(r2a[["Gtp name"]]),
		variety_code = as.character(r2a[["Gtp number"]]),
		N_fertilizer = n2,
		N_splits = ifelse(n2 > 0, 2L, 0L),
		# Gen. info.: First N 17.12.2024, 2nd irrigation + N 28.03.2025; 1st irrigation 24.01.2025
		fertilizer_date = ifelse(n2 > 0, "2024-12-17;2025-03-28", NA_character_),
		rep = as.integer(r2a$Rep),
		planting_date = "2024-12-17",
		heading_days = num(r2a[["Days to 50% heading"]]),
		plant_height = num(r2a[["PH (cm) Average"]]),
		panicle_length = num(r2a[["Spike length (cm) average"]]),
		yield = num(r2a[["Grain Yield (kg/ha)"]]),
		# Total N % in grain -> mg/g (* 10)
		grain_N = num(r2b[[32]]) * 10,
		grain_protein = num(r2b[[34]]),
		disease = "yellow rust",
		disease_severity = trimws(r2a[["Yellow rust"]]),
		soil_texture = tolower(trimws(r2b[[16]])),
		# Gen. info.: Plot area = 12 x 0.25m x 5.0m = 15.0m2; seed rate 12 g/m2 = 120 kg/ha
		plot_width = 3,
		plot_length = 5,
		plot_area = 15,
		row_spacing = 25,
		seed_rate = 120,
		irrigated = TRUE,
		irrigation_number = 2L,
		irrigation_dates = "2025-01-24;2025-03-28",
		latitude = 33.67,
		longitude = 73.075,
		geo_uncertainty = 8000,
		geo_source = "estimated from Islamabad NARC"
	)

	## 3. India, Ludhiana (BISA)
	# skip=10 so Plot/Entry is the header; row 5 of raw sheet gives DoS: 45606 = 2024-11-10
	f3 <- ff[basename(ff) == "Yield Trial + Ludhiana 2024_25 INDIA.xlsx"]
	r3 <- carobiner::read.excel(f3, sheet="Yield and Agronomic Data ", skip=10, col_types="text")
	r3 <- r3[!is.na(r3[[3]]), ]
	r3s <- carobiner::read.excel(f3, sheet="Soil N data Ludhiana 2024_25 ", skip=3, col_types="text")
	r3s <- r3s[!is.na(r3s[[8]]), ]
	if (!identical(as.character(r3[[1]]), as.character(r3s[[8]]))) stop("Ludhiana plot mismatch")
	d3 <- data.frame(
		country = "India",
		location = "Ludhiana",
		plot_id = as.character(r3[[1]]),
		variety = trimws(r3[[3]]),
		variety_code = as.character(r3[[2]]),
		N_fertilizer = num(r3[[5]]),
		rep = as.integer(r3[[4]]),
		planting_date = "2024-11-10",
		heading_days = num(r3[[6]]),
		flowering_days = num(r3[[7]]),
		maturity_days = num(r3[[8]]),
		plant_height = num(r3[[11]]),
		panicle_length = num(r3[[12]]),
		# BIOMASS is in t/ha (fresh vs dry not stated) -> kg/ha
		y_total = num(r3[[15]]) * 1000,
		yield = num(r3[[17]]) * 1000,
		seed_weight = num(r3[[18]]),
		# Soil N data sheet header: BISA Ludhiana, Sandy Loam, 83.12% sand, 7.99% silt, 8.88% clay
		soil_texture = "sandy loam",
		soil_sand = 83.12,
		soil_silt = 7.99,
		soil_clay = 8.88,
		plot_area = 7.2,
		irrigated = NA,
		latitude = 30.863,
		longitude = 75.807,
		geo_uncertainty = 5000,
		geo_source = "estimated from BISA Ludhiana"
	)

	d <- carobiner::bindr(d1, d2, d3)
	d$record_id <- seq_len(nrow(d))
	d$crop <- "wheat"
	d$yield_part <- "grain"
	d$treatment <- paste(d$variety, d$N_fertilizer, sep="_")
	d$variety_type <- bni_type(d$variety)
	d$trial_id <- paste(d$location, "2024", sep="_")
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$geo_from_source <- FALSE

	## Long table: repeated canopy traits and multi-date soil N analyses
	id1 <- d$record_id[d$location == "Bhairahawa"]
	id2 <- d$record_id[d$location == "Islamabad"]
	id3 <- d$record_id[d$location == "Ludhiana"]

	# Bhairahawa dated canopy measurements (dates from row 1 Excel serials)
	l1 <- carobiner::bindr(
		data.frame(record_id = id1, date = xl_date(hdr_dates[8]), ground_cover = num(r1[[8]])),
		data.frame(record_id = id1, date = xl_date(hdr_dates[18]), NDVI = num(r1[[18]])),
		data.frame(record_id = id1, date = xl_date(hdr_dates[19]), NDVI = num(r1[[19]])),
		data.frame(record_id = id1, date = xl_date(hdr_dates[20]), NDVI = num(r1[[20]])),
		data.frame(record_id = id1, date = xl_date(hdr_dates[21]), NDVI = num(r1[[21]])),
		data.frame(record_id = id1, date = xl_date(hdr_dates[22]), canopy_temperature = num(r1[[22]])),
		data.frame(record_id = id1, date = xl_date(hdr_dates[23]), canopy_temperature = num(r1[[23]])),
		data.frame(record_id = id1, date = xl_date(hdr_dates[24]), canopy_temperature = num(r1[[24]])),
		data.frame(record_id = id1, date = xl_date(hdr_dates[25]), SPAD = num(r1[[25]])),
		data.frame(record_id = id1, date = xl_date(hdr_dates[26]), SPAD = num(r1[[26]]))
	)

	# Islamabad 4 dated soil batches (Gen. info.: 2024-12-17, 2025-04-04, 2025-04-11, 2025-04-18)
	# Total N (%) * 10000 -> mg/kg; NO3 and NH4 are ppm = mg/kg
	l2 <- carobiner::bindr(
		data.frame(record_id = id2, date = "2024-12-17", growth_stage = "before sowing",
			soil_NO3 = num(r2b[[11]]), soil_NH4 = num(r2b[[12]]), soil_N_total = num(r2b[[13]]) * 10000,
			soil_pH = num(r2b[[14]]), soil_SOM = num(r2b[[15]])),
		data.frame(record_id = id2, date = "2025-04-04", growth_stage = "7 days after 2nd N",
			soil_NO3 = num(r2b[[17]]), soil_NH4 = num(r2b[[18]]), soil_N_total = num(r2b[[19]]) * 10000,
			soil_pH = num(r2b[[20]]), soil_SOM = num(r2b[[21]])),
		data.frame(record_id = id2, date = "2025-04-11", growth_stage = "14 days after 2nd N",
			soil_NO3 = num(r2b[[22]]), soil_NH4 = num(r2b[[23]]), soil_N_total = num(r2b[[24]]) * 10000,
			soil_pH = num(r2b[[25]]), soil_SOM = num(r2b[[26]])),
		data.frame(record_id = id2, date = "2025-04-18", growth_stage = "21 days after 2nd N",
			soil_NO3 = num(r2b[[27]]), soil_NH4 = num(r2b[[28]]), soil_N_total = num(r2b[[29]]) * 10000,
			soil_pH = num(r2b[[30]]), soil_SOM = num(r2b[[31]]))
	)

	# Ludhiana canopy traits by growth stage + 5 topsoil (0-15 cm) soil mineral N stages
	# NH4 and NO3 are reported in kg/ha; divide by 2.24 (0-15 cm furrow-slice mass, BD ~1.49 g/cm3) -> mg/kg
	l3 <- carobiner::bindr(
		data.frame(record_id = id3, growth_stage = "anthesis", NDVI = num(r3[[19]]), canopy_temperature = num(r3[[23]])),
		data.frame(record_id = id3, growth_stage = "flowering", SPAD = num(r3[[27]])),
		data.frame(record_id = id3, growth_stage = "grain filling", NDVI = num(r3[[20]]), canopy_temperature = num(r3[[24]]), SPAD = num(r3[[28]])),
		data.frame(record_id = id3, growth_stage = "milking", NDVI = num(r3[[21]]), canopy_temperature = num(r3[[25]])),
		data.frame(record_id = id3, growth_stage = "maturity", NDVI = num(r3[[22]]), canopy_temperature = num(r3[[26]])),
		data.frame(record_id = id3, growth_stage = "before sowing", depth_top = 0, depth_bottom = 15, soil_pH = 8.18,
			soil_NH4 = num(r3s[[13]]) / 2.24, soil_NO3 = num(r3s[[14]]) / 2.24, soil_N_min_total = num(r3s[[15]])),
		data.frame(record_id = id3, growth_stage = "7 days after 2nd N", depth_top = 0, depth_bottom = 15,
			soil_NH4 = num(r3s[[16]]) / 2.24, soil_NO3 = num(r3s[[17]]) / 2.24, soil_N_min_total = num(r3s[[18]])),
		data.frame(record_id = id3, growth_stage = "14 days after 2nd N", depth_top = 0, depth_bottom = 15,
			soil_NH4 = num(r3s[[19]]) / 2.24, soil_NO3 = num(r3s[[20]]) / 2.24, soil_N_min_total = num(r3s[[21]])),
		data.frame(record_id = id3, growth_stage = "21 days after 2nd N", depth_top = 0, depth_bottom = 15,
			soil_NH4 = num(r3s[[22]]) / 2.24, soil_NO3 = num(r3s[[23]]) / 2.24, soil_N_min_total = num(r3s[[24]])),
		data.frame(record_id = id3, growth_stage = "after harvest", depth_top = 0, depth_bottom = 15,
			soil_NH4 = num(r3s[[25]]) / 2.24, soil_NO3 = num(r3s[[26]]) / 2.24, soil_N_min_total = num(r3s[[27]]))
	)

	dlong <- carobiner::bindr(l1, l2, l3)
	rownames(dlong) <- NULL

	carobiner::write_files(path, meta, d, long=dlong)
}
