# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. No plot area is reported, so sample cob weight (Cobwt) and grain weight (GWT) cannot be converted to kg/ha yield. Units are not stated in the dictionary, though values (~100 g cob and ~78 g grain per cob; EL ~13.7 cm) indicate grams and centimeters.
# 2. Fertilizer rates (N/P/K) and application rates for GREMEC (emamectin benzoate 50 g/kg), OMO detergent, Tithonia diversifolia leaves, Thevetia neriifolia leaves, wood ash, and soil treatment are not reported in the data.
# 3. Three rows (ID 158, 183, 217) have NA for Cobwt and GWT despite non-missing NumCobs and ear measurements.
# 4. Verify whether any separate publication reports this 12-treatment local-innovation trial; coordinates and elevations were taken from the companion paper at the same four experimental fields (doi:10.1093/jee/toae102).

## NEW
# agroecosystem_: agroecological zone as recorded ("forest" or "transition forest-savannah")
# pest_control_: FAW management innovation applied ("emamectin benzoate", "OMO detergent", "Tithonia diversifolia leaf", "Thevetia neriifolia leaf", "soil", "wood ash", or "none")
# cob_number_: number of cobs in the harvested sample (count)
# cob_weight_: total weight of sampled cobs (unit not stated; likely g)
# grain_weight_: total grain weight of sampled cobs (unit not stated; likely g)
# ear_length_: mean ear length (unit not stated; likely cm)
# kernel_rows_: mean number of kernel rows per ear (count)
# kernels_per_row_: mean number of kernels per row (count)

## MISSING
# yield, yield_moisture, irrigated, N_fertilizer, P_fertilizer, K_fertilizer, harvest_date

## NOTES
# Maize yield-component data (312 plots) from on-farm fall armyworm (Spodoptera frugiperda) management trials across 4 fields in Centre Region, Cameroon: Batchenga (Ndjie, Okongolo; transition forest-savannah) and Mbalmayo (Avebe, Memiam; forest) in 2020 (season 2) and 2021 (season 1). Companion pest/damage dataset is doi:10.25502/h02h-bq18/d.
# Field GPS coordinates and elevations are from Nanga et al. (2024, doi:10.1093/jee/toae102), which evaluated the same four fields and seasons; CKAN coverage coordinates (2.3232, 11.43653) do not match these districts.
# Treatment "ACRO6TZCOP4F2-fertilized" is normalized to variety "ACRO6TZCOMP4F2" (spelling typo missing "M"). Insecticide "GREMEC" (GREMEC 050 WG) is mapped to active ingredient "emamectin benzoate".

carob_script <- function(path) {

"
Impact of different technologies against Spodoptera frugiperda (Lepidoptera: Noctuidae) on the maize yield in the Tropical humid forest of Cameroon

Assess the link between damage, abundance and yield
"

	uri <- "doi:10.25502/zsnk-kk83/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = NA, minor = NA,
		data_organization = "IITA",
		publication = NA,
		project = "Coordinating elucidation of science and effectiveness of Local Innovations for Managing Fall armyworm in Africa",
		design = "Blocked on-farm comparison of 12 FAW-control, variety, and fertilizer treatments across two agroecosystems",
		data_type = "on-farm experiment",
		treatment_vars = "treatment;insecticide_used;insecticide_product;pest_control_",
		response_vars = "cob_number_;cob_weight_;grain_weight_;ear_length_;kernel_rows_;kernels_per_row_",
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 90,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "yield-data.csv"]
	r <- read.csv(f, check.names = FALSE)

	tr <- trimws(r$Treatment)

	variety <- rep(NA_character_, nrow(r))
	variety[grepl("ACRO6TZCO(M)?P4F2", tr)] <- "ACRO6TZCOMP4F2"
	variety[grepl("Local white", tr)] <- "Local white"

	fert <- rep(NA, nrow(r))
	fert[grepl("unfertilized|without fertilizer", tr, ignore.case = TRUE)] <- FALSE
	fert[is.na(fert) & grepl("fertilized|with fertilizer", tr, ignore.case = TRUE)] <- TRUE

	pest_ctrl <- rep("none", nrow(r))
	pest_ctrl[tr == "Insecticide-GREMEC"] <- "emamectin benzoate"
	pest_ctrl[tr == "Detergent-OMO"] <- "OMO detergent"
	pest_ctrl[tr == "Leaf of T. diversifolia"] <- "Tithonia diversifolia leaf"
	pest_ctrl[tr == "Leaf of T. neriifolia"] <- "Thevetia neriifolia leaf"
	pest_ctrl[tr == "Soil-treatment"] <- "soil"
	pest_ctrl[tr == "Wood ash-treatment"] <- "wood ash"

	loc <- carobiner::fix_name(trimws(r$District), "title")
	site <- carobiner::fix_name(trimws(r$Field), "title")
	year <- as.character(r[[7]])
	season <- ifelse(r[[3]] == 1, "first", ifelse(r[[3]] == 2, "second", NA_character_))

	# Field GPS coordinates and elevations from companion study at the same 4 sites (doi:10.1093/jee/toae102)
	geo <- data.frame(
		site = c("Avebe", "Memiam", "Okongolo", "Ndjie"),
		adm2 = c("Nyong-et-So'o", "Nyong-et-So'o", "Lekié", "Lekié"),
		latitude = c(3.36471, 3.47010, 4.25423, 4.34797),
		longitude = c(11.50853, 11.58514, 11.65100, 11.68201),
		elevation = c(673, 657, 510, 490)
	)
	gidx <- match(site, geo$site)

	d <- data.frame(
		country = "Cameroon",
		adm1 = "Centre",
		adm2 = geo$adm2[gidx],
		location = loc,
		site = site,
		latitude = geo$latitude[gidx],
		longitude = geo$longitude[gidx],
		elevation = geo$elevation[gidx],
		geo_from_source = FALSE,
		geo_uncertainty = 1000,
		geo_source = "doi:10.1093/jee/toae102",
		agroecosystem_ = trimws(r$Agroecosystem),
		planting_date = year,
		season = season,
		treatment = tr,
		variety = variety,
		fertilizer_used = as.logical(fert),
		insecticide_used = tr == "Insecticide-GREMEC",
		insecticide_product = ifelse(tr == "Insecticide-GREMEC", "emamectin benzoate", "none"),
		pest_control_ = pest_ctrl,
		rep = as.integer(r$Block),
		block_id = as.character(r$Block),
		crop = "maize",
		stress = "pests",
		pest_species = "Spodoptera frugiperda",
		cob_number_ = as.integer(r$NumCobs),
		cob_weight_ = as.numeric(r$Cobwt),
		grain_weight_ = as.numeric(r$GWT),
		ear_length_ = as.numeric(r$EL),
		kernel_rows_ = as.numeric(r[["Row Per ear"]]),
		kernels_per_row_ = as.numeric(r[["Kernnels Per row"]]),
		trial_id = paste(loc, site, year, season, sep = "_"),
		on_farm = TRUE,
		is_survey = FALSE,
		yield_part = "grain"
	)

	carobiner::write_files(path, meta, d)
}
