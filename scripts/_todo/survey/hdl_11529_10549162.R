# R script for "carob"
# license: GPL (>=3)

## NOTES
# Household survey of agricultural-lime use on farm subplots in Rwanda, 2022 season A. The title and geographic coverage are Rwanda.
# Dry grain or tuber weight is kg per subplot. Yield is that weight divided by subplot area in ha. yield_isfresh is FALSE. The fresh column is a dry-equivalent and is not used.
# Harvest values -99 and 99999 are missing codes. -99 occurs on 12 major-crop rows and 99999 on 3.
# Lime quantity is kg applied over the last three cropping seasons, with 0 when none was applied. lime is that total divided by the current subplot area, so it is not a single-season rate.
# DAP is 18 percent N and 20.1 percent elemental P. Urea is 46 percent N. NPS, NPS with boron or zinc, and NPK blends have no stated grade and are not added to N, P, or K.
# Herbicide quantity is kg or litres, not kg of active ingredient, so only herbicide_used is stored.
# Plot location names are the text CONFIDENTIAL and are not stored.
# Subplot area is in hectares. It is not copied to plot_area, which is square metres for experimental plots.
# Fertilizer, lime, and herbicide were recorded once for the subplot. An intercrop row carries the same subplot totals. They were not split between the two crops.
# Fallow, planted trees, other fruits, other vegetables, and other fodders are not stored.

## ISSUES
# write_files: missing variables: latitude, longitude
# write_files: out of bounds: yield (0, 465000), lime (0, 87500), N_fertilizer (0, 4020), P_fertilizer (0, 4489)
# write_files: low yield (tons not kg?): cabbage
# write_files: NA detected: yield
# Lime is a three-season total divided by the current subplot area, so some rates exceed a single-season bound. Yield outliers are left as recorded. Harvest codes -99 and 99999 are missing, which leaves yield NA.

## NOT PROCESSED
# Costs, labour, seed source, stress scores, soil descriptions, and payment methods.
# QUANTITY_OF_FERTILIZER_NPK_USE_O and QUANTITY_OF_BLENDED_FERTILIZER_N, because the nutrient grades are not stated.
# The fresh-harvest columns.


carob_script <- function(path) {

"
Agricultural Lime Profitability in Rwanda (2022)

Household survey of lime use and crop harvest on farm subplots in Rwanda.
"

	uri <- "hdl:11529/10549162"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "crop",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "Module_12.csv"]
	x <- read.csv(f)

	crop_of <- c(
		"Irish potato"="potato", "Maize"="maize", "Common beans"="common bean",
		"Wheat"="wheat", "Faba/horse bean"="faba bean", "Sweet potato"="sweetpotato",
		"Field peas"="pea", "Sorghum"="sorghum", "Chick-peas"="chickpea",
		"Banana"="banana", "Cabbage"="cabbage", "Barley"="barley", "Cassava"="cassava"
	)
	part_of <- c(
		potato="tubers", maize="grain", "common bean"="seed", wheat="seed",
		"faba bean"="seed", sweetpotato="roots", pea="seed", sorghum="grain",
		chickpea="seed", banana="fruit", cabbage="leaves", barley="grain", cassava="roots"
	)

	kg <- function(v) {
		z <- as.numeric(v)
		z[z %in% c(-99, 99999)] <- NA_real_
		z
	}
	area <- as.numeric(x$SUB_PLOT_AREA_HA)
	area[!is.na(area) & area <= 0] <- NA_real_
	rate <- function(v) kg(v) / area

	dap <- rate(x$QUANDAP)
	urea <- rate(x$QUANTITY_OF_FERTILIZER_UREA_USED)
	hq <- kg(x$QUANTITY_OF_HERBICIDE_USED_ON_TH)
	crop <- unname(crop_of[as.character(x$MAIN_CROP_GROWN_ON_SUB_PLOT)])
	part <- unname(part_of[crop])

	d <- data.frame(
		country = "Rwanda",
		adm1 = carobiner::fix_name(x$PROVNC, "title"),
		adm2 = carobiner::fix_name(x$DISTRICT_WOREDA, "title"),
		trial_id = as.character(x$ACID_LIME_DIC_ID),
		hhid = as.character(x$ACID_LIME_DIC_ID),
		plot_id = paste(x$PLOT_ID_START_WITH_ONE_WITH_LIME, x$SUB_PLOT_ID, crop, sep="_"),
		crop = crop,
		variety = as.character(x$VARIETY_GROWN_ON_SUB_PLOT),
		yield_part = part,
		treatment = crop,
		yield = kg(x$TOTAL_HARVESTED_PER_SUB_PLOT_MA0) / area,
		yield_isfresh = FALSE,
		lime = rate(x$QUANTITY_APPLIED_DURING_THE_LAST),
		N_fertilizer = dap * 0.18 + urea * 0.46,
		P_fertilizer = dap * 0.201,
		herbicide_used = ifelse(is.na(hq), NA, hq > 0),
		intercrops = as.character(x$CROP_INTERCROPPED_ON_SUB_PLOT),
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)
	d$variety[d$variety %in% c("", "NA")] <- NA_character_
	d$intercrops[d$intercrops %in% c("", "NA", "None", "none")] <- NA_character_

	icrop <- unname(crop_of[d$intercrops])
	iok <- !is.na(icrop) & !is.na(kg(x$TOTAL_HARVESTED_PER_SUB_PLOT_IF0))
	extra <- d[iok, ]
	extra$plot_id <- paste(x$PLOT_ID_START_WITH_ONE_WITH_LIME[iok], x$SUB_PLOT_ID[iok], extra$crop, sep="_")
	extra$crop <- icrop[iok]
	extra$yield_part <- unname(part_of[extra$crop])
	extra$treatment <- extra$crop
	extra$variety <- NA_character_
	extra$yield <- (kg(x$TOTAL_HARVESTED_PER_SUB_PLOT_IF0) / area)[iok]
	extra$intercrops <- NA_character_
	d$intercrops <- icrop
	d <- rbind(d, extra)
	d <- d[!is.na(d$crop) & !is.na(d$treatment), ]
	d$copy <- ave(seq_len(nrow(d)), d$trial_id, d$plot_id, d$crop, FUN=seq_along)
	d$plot_id[d$copy > 1] <- paste(d$plot_id[d$copy > 1], d$copy[d$copy > 1], sep="_")
	d$copy <- NULL

	carobiner::write_files(path, meta, d)
}
