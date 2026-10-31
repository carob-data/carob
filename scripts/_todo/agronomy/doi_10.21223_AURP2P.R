# R script for "carob"
# license: GPL (>=3)

## NOTES
# Huancayo field experiment on nitrogen, fungicide, and late blight, 2000. The abstract states N rates of 0, 160, and 320 kg/ha. The file codes are N0, N160, and N320.
# Fungicide codes are F0, F4, and F10. The files do not give a product or a rate in kg/ha.
# Emergence is 2000-01-02 (Emergency_date). Late blight scores LB1-LB29 and canopy cover have their own dates. Canopy cover is a percentage (column name).
# TBDMC_kg_ha is total biomass dry matter in kg/ha at five dates. It is not fresh tuber yield.
# The file has three varieties: Amarilis, Yungay, and Tomasa. The abstract says two.

## ISSUES
# No coordinates for Huancayo in this dataset.
# No planting date, harvest date, P or K fertilizer, irrigation, or fresh tuber yield.
# Fungicide level is the workbook code, not a checked spray count or dose.
# Late blight score scale is not stated. Scores are stored as recorded.
# write_files: , yield, yield_moisture, yield_part, latitude, longitude, irrigated, K_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: fungicide_level_.

## NEW VARIABLES
# fungicide_level_: fungicide treatment code from Fungicide_rate (F0, F4, F10), with the letter removed. Unit not stated.

## NOT PROCESSED
# 07_SpraysOfFungicides_Huancayo.xls: application dates in a sheet with merged header rows. Not mapped to a product rate.
# DailyWeatherData_Huancayo.xls and HourlyWeatherData_Huancayo.xls: weather with header rows, not joined to plots.
# Ontology id columns in the date files.


carob_script <- function(path) {

"
Dataset for: Effect of nitrogen fertilization on potato late blight in the field in Huancayo, Junin Peru

The effect of nitrogen (N) fertilization on the development of potato late blight disease, caused by Phytophthora infestans (Mont.) de Bary, was studied through field experiments in two locations in Peru (Huancayo and Oxapampa). These datasets correspond to the field experiments in Huancayo. Three levels of N (0, 160, and 320 kg/ha) and three levels of fungicide were applied in all combinations on two potato (Solanum tuberosum L.) varieties with different levels of resistance (18 treatments), and in four repetitions. N had no effect on the disease when severity was very low or high. At intermediate levels of disease severity, increased N led to increased disease. In Huancayo, the effect of 0 vs. 160 kg·ha-1 of N on  the more susceptible variety was negligible because of increased late blight.  For the more resistant variety, late blight had little effect on yield when N was applied at 0 or 160 kg·ha-1. Increasing the N fertilization rate to 320 kg·ha-1 did not further increase yield in the absence of late blight, but decreased it sharply in the treatments where the disease was present because no fungicides were applied. These results indicate the importance of considering N fertilization levels in integrated management schemes for potato late blight, and in comparison of results across experiments. If farmers want to increase potato yields with N fertilizer, they may need to use more fungicides or varieties with higher resistance.
"

	uri <- "doi:10.21223/AURP2P"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;variety;fungicide_level_",
		response_vars = "dmy_total",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.5
	)

	f1 <- ff[basename(ff) == "01_data_LateBlightSeverity_Huancayo.xls"]
	f2 <- ff[basename(ff) == "02_LateBlight_Evaluation_date.xls"]
	f3 <- ff[basename(ff) == "03_Data_Canopy_cover_Huancayo.xls"]
	f4 <- ff[basename(ff) == "04_canopy_evaluation_days-1.xls"]
	f5 <- ff[basename(ff) == "05_TotalYield_Huancayo.xls"]
	f6 <- ff[basename(ff) == "06_Evaluation_date_Total biomass dry matter content.xls"]

	r <- carobiner::read.excel(f1, na="NA")
	rd <- carobiner::read.excel(f2, na="NA")
	rc <- carobiner::read.excel(f3, na="NA")
	rcd <- carobiner::read.excel(f4, na="NA")
	ry <- carobiner::read.excel(f5, na="NA")
	ryd <- carobiner::read.excel(f6, na="NA")

	nrate <- as.numeric(sub("^N", "", as.character(r$Nitrogen_rate)))
	flevel <- as.numeric(sub("^F", "", as.character(r$Fungicide_rate)))
	variety <- trimws(as.character(r$Variety))

	d <- data.frame(
		trial_id = "Huancayo_2000",
		country = "Peru",
		adm1 = "Junin",
		location = "Huancayo",
		crop = "potato",
		variety = variety,
		treatment = paste(variety, r$Nitrogen_rate, r$Fungicide_rate, sep="_"),
		rep = as.integer(r$Repetition),
		plot_id = as.character(r$id),
		N_fertilizer = nrate,
		fungicide_level_ = flevel,
		pathogen = "Phytophthora infestans",
		stress = "disease",
		emergence_date = "2000-01-02",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	iso <- function(x) as.character(as.Date(x))
	lb_date <- iso(rd$Evaluation_date[match(paste0("LB", seq_len(29)), rd$Abbreviation)])
	parts <- vector("list", 29)
	for (i in seq_len(29)) {
		parts[[i]] <- data.frame(
			plot_id = d$plot_id,
			date = lb_date[i],
			disease_severity = as.character(r[[paste0("LB", i)]]),
			ground_cover = NA_real_,
			dmy_total = NA_real_
		)
	}
	cc_names <- paste0("CC_G_Et_%_", seq_len(21))
	cc_date <- iso(rcd$Evaluation_date_CCG[match(cc_names, rcd$Abbreviation)])
	for (i in seq_along(cc_names)) {
		parts[[length(parts) + 1]] <- data.frame(
			plot_id = as.character(rc$id),
			date = cc_date[i],
			disease_severity = NA_character_,
			ground_cover = as.numeric(rc[[cc_names[i]]]),
			dmy_total = NA_real_
		)
	}
	bm_names <- paste0("TBDMC_kg_ha_Ev", seq_len(5))
	bm_date <- iso(ryd$Evaluation_date_TBDMC[match(bm_names, ryd$Abbreviation)])
	for (i in seq_along(bm_names)) {
		parts[[length(parts) + 1]] <- data.frame(
			plot_id = as.character(ry$id),
			date = bm_date[i],
			disease_severity = NA_character_,
			ground_cover = NA_real_,
			dmy_total = as.numeric(ry[[bm_names[i]]])
		)
	}
	dlong <- do.call(rbind, parts)
	dlong$trial_id <- "Huancayo_2000"
	keep <- !(is.na(dlong$disease_severity) & is.na(dlong$ground_cover) & is.na(dlong$dmy_total))
	dlong <- dlong[keep, ]
	dlong <- dlong[order(dlong$plot_id, dlong$date), ]
	rownames(dlong) <- NULL

	carobiner::write_files(path, meta, d, long=dlong)
}
