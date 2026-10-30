# R script for "carob"
# license: GPL (>=3)

## NOTES
# Grain composition (not yield) for maize, groundnut and soybean under
# Gliricidia intercrop vs sole-crop treatments in Eastern Province, Zambia.
# Treatment labels from maize/soil files: T1 gliricidia-maize 0% mineral,
# T2 sole maize 100% recommended (rate not given), T3 sole maize 0%, T5
# gliricidia-groundnut, T7 sole groundnut. Two lab rows per Lab_code kept.

## ISSUES
# Dataset is Gliricidia agroforestry in Zambia, not P sources / Minjingu.
# T4 and T6 are soybean treatments with no description in the files. T2
# recommended mineral rate not reported so N/P/K left unset. No grain yield.
# Mycotoxin (maize/groundnut) and Aspergillus soil files not mapped.
# District coordinates estimated. write_files: missing yield, yield_moisture,
# yield_part, planting_date, harvest_date.


carob_script <- function(path) {

"
Gliricidia impacts on soil fertility, crop yields and nutrition in Zambia

The main objective of the research is to advise Norad on the relevance of continued support to Gliricidia based agroforestry systems under CA – and to assess the potential return rate – in social, economic, environmental and agronomic terms of such systems, as well as potential risk of the farming system for smallholders.
1)To evaluate crop yields of maize-legume systems and study the impacts on soil nutrient balance (e.g. N, P, organic Carbon) under Gliricidia-based agroforestry systems at different levels of intensification and land degradation levels
2)To establish the impact of Gliricidia-based agroforestry systems (as a soil management practices) on the soil nutrients uptake and crop nutrient composition.
"

	uri <- "doi:10.25502/0fa9-dm27/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "Gliricidia Impacts on Soil fertility, Crop yields and Nutrition in Zambia",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "treatment",
		response_vars = "grain_protein",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 55,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "all-crops-and-treatment.csv"]
	r <- read.csv(f)
	r <- r[nzchar(as.character(r$Crop)) & !is.na(r$Crop), ]

	crop_map <- c(Maize="maize", Groundnut="groundnut", soybean="soybean",
		SOYBEAN="soybean", MAIZE="maize", GROUNDNUT="groundnut")
	trt <- trimws(as.character(r$Trt))
	# T1/T5 include Gliricidia; T2/T3 maize mineral contrast; T7 sole groundnut
	intercrops <- ifelse(trt %in% c("T1", "T5"), "gliricidia", NA_character_)
	om_used <- trt %in% c("T1", "T5")
	n0 <- trt %in% c("T1", "T3")

	d <- data.frame(
		country = "Zambia",
		adm1 = "Eastern",
		adm2 = carobiner::fix_name(r$District, "title"),
		location = carobiner::fix_name(trimws(r$Village), "title"),
		crop = unname(crop_map[r$Crop]),
		treatment = trt,
		intercrops = intercrops,
		intercropped = !is.na(intercrops),
		OM_used = om_used,
		OM_type = ifelse(om_used, "Gliricidia sepium leaves", NA_character_),
		N_fertilizer = ifelse(n0, 0, NA_real_),
		P_fertilizer = ifelse(n0, 0, NA_real_),
		K_fertilizer = ifelse(n0, 0, NA_real_),
		grain_protein = as.numeric(r$PROTEIN),
		sample_id = trimws(as.character(r$Lab_code))
	)
	vil <- trimws(as.character(r$Village))
	vil[vil %in% c("", " ")] <- NA
	d$location[d$location %in% c("", " ") | is.na(vil)] <- NA
	d$trial_id <- ifelse(is.na(vil), trimws(r$District), paste(trimws(r$District), vil, sep="_"))
	# two lab measurements per Lab_code
	d$rep <- as.integer(ave(seq_len(nrow(d)), d$sample_id, FUN=seq_along))
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- NA

	# district centroids, Eastern Province (not the swapped coverage_x/y in the JSON)
	geo <- data.frame(
		adm2 = c("Chasefu", "Chipangali", "Lumezi"),
		longitude = c(32.90, 32.75, 32.80),
		latitude = c(-11.95, -13.40, -12.50),
		geo_uncertainty = c(30000, 30000, 30000),
		geo_source = "estimated from district name"
	)
	d <- merge(d, geo, by="adm2", all.x=TRUE)
	d$geo_from_source <- FALSE

	carobiner::write_files(path, meta, d)
}
