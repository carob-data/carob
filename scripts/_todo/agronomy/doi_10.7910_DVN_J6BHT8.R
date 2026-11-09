# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm comparison of Zai, Zai with fertilizer microdosing, microdosing, and farmer practice. Crops are maize, millet, and sorghum. The codebook defines the codes in Data.csv.
# District 1 is Kassena Nankana and district 2 is Bongo. Community 1 is Nyangua, 2 is Gia, and 3 is Samboligo.
# Grain weight, plant height, days to maturity, plants harvested, and harvest index follow the codebook: kg/ha, cm, days, plants/ha, and percent.
# plant_density is the number of plants harvested per ha, which is what the codebook records.
# Country is Ghana from the description. on_farm is TRUE because the description says the work was on-farm. The codebook has no coordinates and no planting date. The description names other districts that are not in this file.
# Days to maturity in the file are 48 to 68, including for maize.

## ISSUES
# carob_contributor is unset until someone has read this script.
# write_files: missing yield_moisture, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.

## NOT PROCESSED
# FODDER and TBMSS. The codebook says kg/ha and does not say whether the weight is fresh or dry.
# PLNTHSD, NCOB, NPAN, and COBPAWT. Stand, cob, and panicle counts are per plot, and the plot area is not given. The panicle column has no label.
# Maize-Gia.csv and the other community files. They repeat the same trials without units, and one fodder value is the text 3.O4. Data.csv is the table with a codebook.
# Codebook.csv: documentation or a codebook, not an observation table.
# Maize-Nyangua.csv: not read. columns: PLOT-, TRT, Crop, Community, PLT STAND @ 14 DAYS, PLT HEIGHT @BOOTING, DAYS TO PHYSIOLOGICAL MATURITY, NO OF COBS/PANICCLE AT HARVEST, GRAIN WEIGHT, FODDER WEIGHT , FIELD WEIGHT, TOTAL YIELD, .... Observation records are missing from d until this file is standardized.
# Maize-Samboligo.csv: not read. columns: PLOT-, TRT, Crop, Community, PLT STAND @ 14 DAYS, PLT HEIGHT @BOOTING, DAYS TO PHYSIOLOGICAL MATURITY, NO OF COBS/PANICCLE AT HARVEST, GRAIN WEIGHT, FODDER WEIGHT , FIELD WEIGHT, TOTAL YIELD, .... Observation records are missing from d until this file is standardized.
# Millet-Gia.csv: not read. columns: PLOT-, TRT, Crop, Community, PLT STAND @ 14 DAYS, PLT HEIGHT @BOOTING, DAYS TO PHYSIOLOGICAL MATURITY, NO OF COBS/PANICCLE AT HARVEST, PANICCLE WEIGHT, FODDER WEIGHT , FIELD WEIGHT, TOTAL YIELD, .... Observation records are missing from d until this file is standardized.
# Millet-Samboligo.csv: not read. columns: PLOT-, TRT, Crop, Community, PLT STAND @ 14 DAYS, PLT HEIGHT @BOOTING, DAYS TO PHYSIOLOGICAL MATURITY, NO OF COBS/PANICCLE AT HARVEST, PANICLE WEIGHT, FODDER WEIGHT , FIELD WEIGHT, TOTAL YIELD, .... Observation records are missing from d until this file is standardized.
# Sorghum-Gia.csv: not read. columns: PLOT-, TRT, Crop, Community, PLT STAND @ 14 DAYS, PLT HEIGHT @BOOTING, DAYS TO PHYSIOLOGICAL MATURITY, NO OF COBS/PANICCLE AT HARVEST, PANICCLE WEIGHT, FODDER WEIGHT , FIELD WEIGHT, TOTAL YIELD, .... Observation records are missing from d until this file is standardized.
# Sorghum-Samboligo.csv: not read. columns: PLOT-, TRT, Crop, Community, PLT STAND @ 14 DAYS, PLT HEIGHT @BOOTING, DAYS TO PHYSIOLOGICAL MATURITY, NO OF COBS/PANICCLE AT HARVEST, PANICLE WEIGHT, FODDER WEIGHT , FIELD WEIGHT, TOTAL YIELD, .... Observation records are missing from d until this file is standardized.

carob_script <- function(path) {

"In Ghana, Zai has been successfully been introduced on a limited scale to farmers in the East Gonja, East Mamprusi districts in the Northern region and Guru and Builsa district in Upper East region Presbyterian Agricultural Services. However, Zai farming is an innovative technology which involves the burial of manure/organic matter in holes/ pits and planting the crop later on top of the pit. The objectives of this study are to evaluate and disseminate the Zai and fertilizer Micro-dosing techniques on-farm; to undertake cost benefit analysis of the Zai and fertilizer Micro-dosing; to explore the potential increase in grain yield of Zai and fertilizer Micro-dosing over farmers practice in the Africa RISING intervention communities; to undertake comparative analysis of the effect of Zai and fertilizer Micro-dosing on maize,sorghum, and millet."

	uri <- "doi:10.7910/DVN/J6BHT8"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)
	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "IITA",
		publication = NA,
		project = "Africa RISING",
		data_type = "on-farm experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		carob_effort = 0.4,
		carob_completion = 60,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		notes = "No coordinates or planting date. Fodder and total biomass were not used because fresh and dry weight are not distinguished. Maturity is 48 to 68 days."
	)

	f <- ff[basename(ff) == "Data.csv"]
	r <- read.csv(f)
	dist <- c("1" = "Kassena Nankana", "2" = "Bongo")
	comm <- c("1" = "Nyangua", "2" = "Gia", "3" = "Samboligo")
	crop <- c("1" = "maize", "2" = "millet", "3" = "sorghum")
	trt <- c("1" = "Zai practice", "2" = "Zai + microdosing", "3" = "microdosing", "4" = "farmer practice")

	d <- data.frame(
		country = "Ghana",
		adm2 = unname(dist[as.character(r$DIST)]),
		location = unname(comm[as.character(r$CMMTY)]),
		crop = unname(crop[as.character(r$CRP)]),
		treatment = unname(trt[as.character(r$TRT)]),
		yield = as.numeric(r$GYD),
		yield_part = "grain",
		plant_height = as.numeric(r$PLNTH),
		maturity_days = as.numeric(r$DPHMTY),
		plant_density = as.numeric(gsub(",", "", r$NPLTHV)),
		harvest_index = as.numeric(r$HINDX),
		on_farm = TRUE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	d$trial_id <- paste(d$crop, d$location, sep = "_")
	d$plot_id <- as.character(ave(seq_len(nrow(d)), d$trial_id, FUN = seq_along))
	d$record_id <- as.integer(seq_len(nrow(d)))

	carobiner::write_files(path, meta, d)
}
