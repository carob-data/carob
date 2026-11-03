# R script for "carob"
# license: GPL (>=3)

## NOTES
# Huancayo field experiment (1999/2000 season) on nitrogen x fungicide x variety and potato late blight.
# Publication: Juarez HS, Amaro JR, Rivera MD, Parraga A, Hijmans RJ. The effect of nitrogen fertilization
#   on potato late blight in the field. CIP Program Report 1999-2000, pp. 69-72. (no DOI found; see ISSUES)
#   "the paper" below refers to this publication.
# Design (paper): 2 varieties x 3 N rates x 3 fungicide levels, 4 replicates; RCBD.
# Varieties: the paper reports Amarilis (moderately resistant) and Yungay (moderately susceptible), 36 plots each.
#   The data also have Tomasa (12 plots), only at N160 with all 3 fungicide levels. Tomasa is not in the paper.
#   The Tomasa rows are kept.
# Fungicide: the data file uses codes F0, F4 and F10. In the paper these are the Oxapampa codes; Huancayo had
#   F- (none), F7 (every 7 days) and F14 (every 14 days). Late blight severity in the data (LB26, all plots):
#   F0 >> F10 > F4. So F4 = sprayed every 7 days and F10 = every 14 days. Stored as fungicide_interval.
# Spray dates are from 07_SpraysOfFungicides_Huancayo.xls (columns "7" and "14"): the 7-day treatment had
#   17 sprays (2000-01-07 to 2000-04-28) and the 14-day treatment 5 sprays (2000-01-07 to 2000-03-03).
#   The 14-day sprays stopped early in March; reason not stated. F0 had 0 sprays.
#   fungicide_times (0, 5, 17) is used as the fungicide treatment variable.
# Late blight severity is in % of foliage infected (0-100; values 0-100 in steps of 5), not a fraction as in the paper.
# LB1-LB29 are 29 evaluations (2000-01-11 to 2000-05-30). LB27-LB29 were only scored on N320 plots
#   (all varieties); other plots have no score after LB26 (2000-05-15). Reason not stated; not filled in.
# Emergence is 2000-01-02 (Emergency_date in 02_LateBlight_Evaluation_date.xls).
# TBDMC_kg_ha is total biomass dry matter in kg/ha at five dates (dmy_total). It is not fresh tuber yield.
#   The paper (Table 1) gives tuber dry matter yield only as treatment means, so yield is not included.

## ISSUES
# The paper gives longitude as 75 14' "E"; Peru is west of Greenwich, so it is stored as negative (W).
# publication: CIP Program Report 1999-2000 has no DOI. Check CGSpace for a handle (hdl:10568/...).
# P and K: the paper says "phosphorus at 62 kg/ha" and "potassium at 100 kg/ha". Taken as elemental P and K.
#   They could be P2O5 and K2O; to check with the authors.
# P source was Ca(H2PO4)2 (monocalcium phosphate). Mapped to fertilizer_type "TSP" (assumption).
# Irrigation not stated in the paper (irrigated = NA).
# Whether infection was natural or inoculated is not stated (inoculated = NA).
# write_files: missing variables yield, yield_moisture, yield_part. The data have no plot-level tuber yield
#   (05_TotalYield_Huancayo.xls only has total biomass dry matter, TBDMC_kg_ha_Ev1-5). The paper gives tuber
#   dry matter yield only as treatment means (Table 1), so it is not used.

## NOT PROCESSED
# DailyWeatherData_Huancayo.xls and HourlyWeatherData_Huancayo.xls: weather data.
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
		publication = NA,  # CIP Program Report 1999-2000, pp. 69-72 (see NOTES); no DOI found
		project = NA,
		design = "RCBD",  # paper: factorial complete block design, 4 replicates
		data_type = "experiment",
		treatment_vars = "N_fertilizer;variety;fungicide_times",
		response_vars = "disease_severity;ground_cover;dmy_total",
		notes = "Fungicide codes F4 and F10 in the data correspond to spray intervals of 7 and 14 days (see script). Tomasa is not in the publication.",
		carob_contributor = "Bester Mudereri",
		carob_date = "2026-11-03",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 90,
		carob_effort = 3
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
	variety <- trimws(as.character(r$Variety))

	# fungicide: file codes -> spray interval in days (see NOTES)
	fcode <- trimws(as.character(r$Fungicide_rate))
	finterval <- c(F0 = NA, F4 = 7L, F10 = 14L)[fcode]
	fused <- fcode != "F0"

	# spray dates from 07_SpraysOfFungicides_Huancayo.xls
	# Excel rows 5-21 hold the dates; column B = 7-day treatment, column D = 14-day treatment
	f7 <- ff[basename(ff) == "07_SpraysOfFungicides_Huancayo.xls"]
	s <- carobiner::read.excel(f7, col_names=FALSE, range="A5:E21")
	sp7 <- as.character(as.Date(na.omit(s[[2]])))
	sp14 <- as.character(as.Date(na.omit(s[[4]])))
	# check that the expected block was read (17 and 5 sprays)
	stopifnot(length(sp7) == 17, length(sp14) == 5)
	fdates <- c(F0 = NA, F4 = paste(sp7, collapse=";"), F10 = paste(sp14, collapse=";"))
	ftimes <- c(F0 = 0L, F4 = length(sp7), F10 = length(sp14))

	d <- data.frame(
		trial_id = "Huancayo_2000",
		country = "Peru",
		adm1 = "Junin",
		location = "Huancayo",
		# coordinates and elevation from the paper (p. 70): 12 01'45" S, 75 14'0" (paper says E; corrected to W), 3300 m
		latitude = -12.0292,
		longitude = -75.2333,
		elevation = 3300,
		geo_from_source = TRUE,  # from the authors' publication
		crop = "potato",
		variety = variety,
		treatment = paste(variety, r$Nitrogen_rate, r$Fungicide_rate, sep="_"),
		rep = as.integer(r$Repetition),
		plot_id = as.character(r$id),
		# dates from the paper (p. 70); emergence from the dataset
		planting_date = "1999-12-17",
		emergence_date = "2000-01-02",
		harvest_date = "2000-06-02",  # last harvest
		# fertilizer from the paper (p. 70-71)
		N_fertilizer = nrate,  # as NH4NO3, banded
		N_splits = ifelse(nrate > 0, 2L, 0L),  # half at planting, half 32 days after emergence
		P_fertilizer = 62,  # as Ca(H2PO4)2, banded at planting (see ISSUES)
		K_fertilizer = 100,  # as KCl, banded at planting (see ISSUES)
		fertilizer_type = ifelse(nrate > 0, "AN;TSP;KCl", "TSP;KCl"),
		# fungicide from the paper (p. 70): chlorothalonil (Bravo 500), backpack sprayer, from emergence
		fungicide_used = fused,
		fungicide_product = ifelse(fused, "chlorothalonil", NA),
		fungicide_interval = unname(finterval),
		fungicide_times = unname(ftimes[fcode]),
		fungicide_dates = unname(fdates[fcode]),
		# plot layout from the paper (p. 70): 4 rows x 14 plants, rows 1 m apart, plants 0.3 m apart
		plot_area = 16.8,  # m2
		row_spacing = 100,  # cm
		plant_spacing = 30,  # cm
		plant_density = 10000 / (1 * 0.3),  # plants/ha
		irrigated = NA,
		inoculated = NA,
		pathogen = "Phytophthora infestans",
		disease = "potato late blight",
		stress = "disease",
		on_farm = FALSE,
		is_survey = FALSE
	)

	iso <- function(x) as.character(as.Date(x))
	lb_date <- iso(rd$Evaluation_date[match(paste0("LB", seq_len(29)), rd$Abbreviation)])
	parts <- vector("list", 29)
	for (i in seq_len(29)) {
		parts[[i]] <- data.frame(
			plot_id = d$plot_id,
			date = lb_date[i],
			disease_severity = as.character(r[[paste0("LB", i)]]),
			severity_scale = "0-100",  # % of foliage infected
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
			severity_scale = NA_character_,
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
			severity_scale = NA_character_,
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
