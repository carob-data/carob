# R script for "carob"
# license: GPL (>=3)

## NOTES
# Three 2024/25 BNI (T3BL.3NsbS) spring-wheat yield trials: Bhairahawa
# (Nepal), Islamabad (Pakistan), Ludhiana (India). N rates 0/75/150 kg/ha.
# BNI_Nepal_2025.xlsx duplicates Bhairahawa yield components and is not used.
# Pakistan BNI data_Final agronomic sheet is the same trial as the Islamabad
# file; Islamabad "Agronomic data" is used for that site.

## ISSUES
# Bhairahawa NDVI header dates cause readxl type warnings. Ludhiana sowing
# date not in the agronomic table. Soil N batch sheets and canopy NDVI/CT/SPAD
# not mapped. Site coordinates estimated (not in files). Yellow rust scores
# not mapped (scale not documented). Ludhiana BIOMASS t/ha omitted (fresh vs dry
# not stated). write_files: missing yield_moisture, P_fertilizer, K_fertilizer,
# harvest_date.


carob_script <- function(path) {

"
Yield trial data of CIMMYT Elite Spring Wheat lines with the BNI short arm translocation (T3BL.3NsbS) from Leymus racemosus in South Asia

Yield trial data of CIMMYT Elite Spring Wheat lines with the BNI short arm translocation in South Asia, 2024/25.
"

	uri <- "hdl:11529/10549335"
	group <- "varieties_wheat"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "CIMMYT; NARC",
		publication = NA,
		project = "CropSustaiN",
		design = "BNI translocation lines vs recurrent parents; N 0/75/150 kg/ha",
		data_type = "on-station experiment",
		treatment_vars = "variety;N_fertilizer",
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 70,
		carob_effort = 0.8
	)

	# BNI lines carry CSMONO3B / Lr#n in the pedigree/name
	bni_type <- function(x) {
		ifelse(grepl("CSMONO3B|Lr#n|BNI", x, ignore.case=TRUE), "improved", "check")
	}

	## Nepal, Bhairahawa (NWRP)
	f1 <- ff[basename(ff) == "Yield Trial + Bhairahawa 2024_25 NEPAL.xlsx"]
	r1 <- carobiner::read.excel(f1, sheet="BNI trial CropSustaiN")
	r1 <- r1[!is.na(r1$Genotypes), ]
	d1 <- data.frame(
		country = "Nepal",
		location = "Bhairahawa",
		plot_id = as.character(r1$PN),
		variety = r1$Genotypes,
		variety_code = as.character(r1[["ENTRY NO."]]),
		N_fertilizer = as.numeric(r1[["Nitrogen Level"]]),
		rep = as.integer(gsub("[^0-9]", "", r1$Rep)),
		planting_date = as.character(as.Date(r1[["Days of Sowing"]])),
		germination_rate = as.numeric(r1[["Emergence %"]]),
		ground_cover = as.numeric(r1[["GC%"]]),
		heading_days = as.numeric(r1[["Days to Heading"]]),
		maturity_days = as.numeric(r1[["Dats to Maturity"]]),
		plant_height = as.numeric(r1[["Plant Height (cm)"]]),
		spike_density = as.numeric(r1[["No. of spikes/m2"]]) * 10000,
		seed_weight = as.numeric(r1[["TGW (g)"]]),
		yield = as.numeric(r1[["GY(kg/ha)"]])
	)
	d1$latitude <- 27.518
	d1$longitude <- 83.455
	d1$geo_uncertainty <- 5000
	d1$geo_source <- "estimated from Bhairahawa NWRP"

	## Pakistan, Islamabad
	f2 <- ff[basename(ff) == "Yield Trial + Islamabad 2024_25, PAKISTAN.xlsx"]
	r2 <- carobiner::read.excel(f2, sheet="Agronomic data")
	r2 <- r2[!is.na(r2[["Gtp name"]]), ]
	d2 <- data.frame(
		country = "Pakistan",
		location = "Islamabad",
		plot_id = as.character(r2$Plot),
		variety = r2[["Gtp name"]],
		variety_code = as.character(r2[["Gtp number"]]),
		N_fertilizer = as.numeric(gsub("[^0-9]", "", r2[["N rate"]])),
		rep = as.integer(r2$Rep),
		planting_date = "2024-12-17",
		heading_days = as.numeric(r2[["Days to 50% heading"]]),
		plant_height = as.numeric(r2[["PH (cm) Average"]]),
		yield = as.numeric(r2[["Grain Yield (kg/ha)"]]),
		plot_area = 15
	)
	d2$latitude <- 33.67
	d2$longitude <- 73.075
	d2$geo_uncertainty <- 8000
	d2$geo_source <- "estimated from Islamabad"

	## India, Ludhiana (BISA). skip=10 so Plot/Entry is the header.
	f3 <- ff[basename(ff) == "Yield Trial + Ludhiana 2024_25 INDIA.xlsx"]
	r3 <- carobiner::read.excel(f3, sheet="Yield and Agronomic Data ", skip=10)
	r3 <- r3[!is.na(r3[[3]]), ]
	d3 <- data.frame(
		country = "India",
		location = "Ludhiana",
		plot_id = as.character(r3[[1]]),
		variety = as.character(r3[[3]]),
		variety_code = as.character(r3[[2]]),
		N_fertilizer = as.numeric(r3[[5]]),
		rep = as.integer(r3[[4]]),
		heading_days = as.numeric(r3[[6]]),
		flowering_days = as.numeric(r3[[7]]),
		maturity_days = as.numeric(r3[[8]]),
		plant_height = as.numeric(r3[[11]]),
		yield = as.numeric(r3[[17]]) * 1000,
		seed_weight = as.numeric(r3[[18]]),
		plot_area = 7.2
	)
	d3$latitude <- 30.863
	d3$longitude <- 75.807
	d3$geo_uncertainty <- 5000
	d3$geo_source <- "estimated from BISA Ludhiana"

	d <- carobiner::bindr(d1, d2, d3)
	d$crop <- "wheat"
	d$yield_part <- "grain"
	d$treatment <- paste(d$variety, d$N_fertilizer, sep="_")
	d$variety_type <- bni_type(d$variety)
	d$trial_id <- paste(d$location, "2024", sep="_")
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$geo_from_source <- FALSE

	carobiner::write_files(path, meta, d)
}
