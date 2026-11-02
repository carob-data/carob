# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-station 2018 RCBD (4 reps) of pre- and post-emergence weed control in sole cassava, five Nigerian stations.
# Files used: onstation2018_lagon_all_locations_datafile_rft.csv. The dictionary was used for units only.
# RootYldtha is fresh root weight (t/ha -> kg/ha). Stem weight is mapped to fwy_stems; this dictionary does not say fresh or dry (the Bayer harvest dictionary in this series calls stem weight fresh).
# Visual scores are percent weed-control efficacy, not percent cover, so they are not weed_cover. Weed dry matter is g/m2 * 10 = kg/ha. Weed density is plants/m2 * 10000 = plants/ha.
# Repeated scores (2-15 weeks after planting) are in the long table. DAP = WAP * 7.
# Harvest stand is plant_density. Sprout counts over time are sprout_density_ (plants/ha) so the name is not repeated in the long table.
# Lagon is the vocabulary synonym for isoxaflutole and aclonifene. Monsoon Active is foramsulfuron + thiencarbazone-methyl + cyprosulfamide (abstract; salt name foramsulfuron-sodium). Lifeline is mapped to glufosinate-ammonium. Product rates are L/ha, not kg ai/ha.
# Hoe treatments are weeding, not herbicides. Untreated is herbicide_product "none" and weeding_times 0.
# Lat/Long in the file are used as recorded (geo_from_source TRUE).

## ISSUES
# No crop-injury column. The abstract says no injury was seen; that summary was not copied onto plots.
# No variety, fertilizer, irrigation, or plot area.
# Herbicide rates are litres of product per ha. Concentrations are not in the file, so herbicide_amount (kg ai/ha) is omitted. Paraquat is not in this file.
# IITA Ibadan planting date is stored as 05/06/2019 and harvest as 17/07/2019 (about six weeks) while Year is 2018 and root yields are 6-19 t/ha. Left as recorded; likely a year typo.
# Three plots have yield 0 and plant_density and stem_density 0 (vocabulary minimum is 500). Left as recorded.
# Hoe weeks (4, 8, 12 WAP) were not expanded to weeding_dates.
# 2018 post-emergence timing is 7 or 8 WAT where a chemical was applied (PostApplWAT 0 means not applied).
# Species-level densities are a separate dataset (doi:10.25502/we8c-t953).
# No publication DOI in the metadata.
# cyprosulfamide is rejected because the vocabulary entry has a leading space. The unspaced name is kept.
# write_files: missing carob_contributor, yield_moisture, irrigated, N/P/K fertilizer;
#   NA in treatment variable herbicide_timing (hoe and untreated);
#   unknown variables listed under NEW VARIABLES plus sprout_density_ (a sprout count, not shrub_density);
#   out of bounds plant_density and stem_density (zeros);
#   invalid terms cyprosulfamide;
#   datespan for the 24 IITA Ibadan rows.
# A fresh session may also warn that the terminag GitHub check returned HTTP 403. That is not from these data.

## NOT PROCESSED
# ID: spreadsheet row number.
# data_dictionary.csv: column definitions, already used for units.
# Two trailing rows with no plot id.
# Quadrat and per-row counts are not in this file (per-ha and per-m2 summaries are).

## NEW VARIABLES
# pre_herbicide_l_ha_: pre-emergence product rate (L/ha), NA when the pre-treatment is hoe or untreated.
# post_herbicide_l_ha_: post-emergence product rate (L/ha).
# post_adjuvant_: TRUE when the post treatment is recorded as Monsoon+Adjuvant (adjuvant product not named).
# marketable_root_density_: marketable roots per ha at harvest (NoOkRootsha).
# weed_control_: total visual weed-control efficacy (%).
# weed_control_broadleaf_: broadleaf control efficacy (%).
# weed_control_grass_: grass control efficacy (%).
# weed_control_sedge_: sedge control efficacy (%).
# weed_dmy_broadleaf_: broadleaf weed dry matter (kg/ha).
# weed_dmy_grass_: grass weed dry matter (kg/ha).
# weed_dmy_sedge_: sedge weed dry matter (kg/ha).
# weed_dmy_: total weed dry matter at 15 WAP (kg/ha); recorded for Ogun only.
# sprout_density_: cassava sprout count (plants/ha) at that DAP.


carob_script <- function(path) {

"
Evaluation of foramsulfuron-sodium + thiecarbazone-methyl + cyprosulfamide for post-emergence weed control in cassava (Onstation2018_Lagon_All_Locations)

Monsoon Active [foramsulfuron-sodium + thiecarbazone-methyl + cyprosulfamide ] was evaluated for post-emergence weed control cassava and cassava/maize intercrop for three years at six different sites across five States [Benue, Ogun, Osun, Oyo, and the Rivers States] in Nigeria. Monsoon was compared with Paraquat, a toxic herbicide already marked for de-registration in Nigeria. The evaluation included manual hoe weeding treatments. All post-emergence herbicides treatments in the assessment were applied under the canopy with a backpack spray fitted with a sprayhood. In 2020, a treatment where Monsoon was used under cassava canopy without a sprayhood was included in the assessment. All the trials were set up in a randomized complete block design and replicated four times. Cassava was monitored for injury. Treatment efficacy was assessed visually and from weed dry matter. Cassava and maize population at crop harvest and yield were assessed. The result showed that Monsoon was superior to Paraquat and manual hoe weeding in reducing weed pressure at most sites. Monsoon provided excellent control of major cassava weeds at all the sites. Monsoon did not affect crop stand negatively or depress cassava yield in both monocrop and intercropped plots. No crop injury was observed when Monsoon was applied under cassava canopy with or without a sprayhood at 7 to 8 weeks after planting cassava, indicating that Monsoon is suitable for post-emergence weed control in cassava and a potential replacement for Paraquat.
"

	uri <- "doi:10.25502/rnbm-3x44/d"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "PJ-NA2910",
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "herbicide_product;weeding_times;herbicide_timing",
		response_vars = "yield;fwy_stems;plant_density;stem_density",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 72,
		carob_effort = 1.5
	)

	f <- ff[basename(ff) == "onstation2018_lagon_all_locations_datafile_rft.csv"]
	r <- read.csv(f, na.strings = c("", " ", "NA", "na"), check.names = FALSE)
	r <- r[!is.na(r$UniqueID), ]

	num <- function(x) {
		if (is.null(x)) return(rep(NA_real_, nrow(r)))
		x <- trimws(as.character(x))
		x[x == ""] <- NA
		as.numeric(x)
	}
	ai <- function(x) {
		x <- trimws(as.character(x))
		ifelse(is.na(x), NA_character_,
			ifelse(grepl("^Lagon", x), "isoxaflutole;aclonifene",
			ifelse(grepl("^Lifeline", x), "glufosinate-ammonium",
			ifelse(grepl("^Monsoon", x), "foramsulfuron;thiencarbazone-methyl;cyprosulfamide",
			ifelse(grepl("^Paraquat", x), "paraquat dichloride", "none")))))
	}
	combine_ai <- function(a, b) {
		out <- rep("none", length(a))
		for (i in seq_along(a)) {
			parts <- unique(c(a[i], b[i]))
			parts <- parts[!is.na(parts) & parts != "none"]
			if (length(parts) > 0) out[i] <- paste(parts, collapse = ";")
		}
		out
	}
	l_ha <- function(x) {
		v <- rep(NA_real_, length(x))
		hit <- !is.na(x) & grepl("L/ha", x)
		if (!any(hit)) return(v)
		m <- regexpr("[0-9]+\\.?[0-9]*", x[hit])
		v[hit] <- as.numeric(regmatches(x[hit], m))
		v
	}
	hoe_n <- function(x) {
		x <- ifelse(is.na(x), "", x)
		ifelse(grepl("4_8_12", x), 3L, ifelse(grepl("4_8WAP", x), 2L, ifelse(grepl("HW", x), 1L, 0L)))
	}

	pre_ai <- ai(r$Pretreat)
	post_ai <- ai(r$Posttreat)
	wat <- num(r$PostApplWAT)
	timing <- ifelse(post_ai != "none" & !is.na(wat) & wat > 0, as.character(as.integer(wat * 7)), NA_character_)

	site <- r$Site
	site[site == "AgoOwu"] <- "Ago-Owu"
	site[site == "IITAIbadan"] <- "IITA Ibadan"

	d <- data.frame(
		trial_id = paste(site, r$Year, sep = "_"),
		plot_id = r$UniqueID,
		rep = as.integer(r$Rep),
		treatment = r$Treatment,
		country = "Nigeria",
		adm1 = r$Loc,
		location = site,
		latitude = as.numeric(r$Lat),
		longitude = as.numeric(r$Long),
		geo_from_source = TRUE,
		crop = "cassava",
		intercrops = "none",
		yield = num(r$RootYldtha) * 1000,
		yield_part = "roots",
		yield_isfresh = TRUE,
		fwy_stems = num(r$Wtstemtha) * 1000,
		plant_density = num(r$Standha_Cas),
		stem_density = num(r$Stemha_Cas),
		marketable_root_density_ = num(r$NoOkRootsha),
		planting_date = as.character(as.Date(r$Date_Planted_Cas, "%d/%m/%Y")),
		harvest_date = as.character(as.Date(r$Date_Harvested_Cas, "%d/%m/%Y")),
		herbicide_product = combine_ai(pre_ai, post_ai),
		herbicide_used = combine_ai(pre_ai, post_ai) != "none",
		herbicide_times = as.integer((pre_ai != "none") + (post_ai != "none")),
		herbicide_timing = timing,
		pre_herbicide_l_ha_ = l_ha(r$Pretreat),
		post_herbicide_l_ha_ = l_ha(r$Posttreat),
		post_adjuvant_ = grepl("Adjuvant", ifelse(is.na(r$Posttreat), "", r$Posttreat)),
		weeding_times = as.integer(pmax(hoe_n(r$Pretreat), hoe_n(r$Posttreat))),
		on_farm = FALSE,
		is_survey = FALSE
	)
	d$weeding_done <- d$weeding_times > 0
	d$weeding_method <- ifelse(d$weeding_done, "hoe", NA_character_)
	d$weeding_implement <- ifelse(d$weeding_done, "hoe", NA_character_)
	d$adm1 <- carobiner::fix_name(d$adm1, "title")
	d$location <- carobiner::fix_name(d$location, "title")
	d$trial_id <- paste(d$location, "2018", sep = "_")
	d$record_id <- as.integer(seq_len(nrow(d)))

	col <- function(name) {
		if (!name %in% names(r)) return(rep(NA_real_, nrow(r)))
		num(r[[name]])
	}
	one_wap <- function(w) {
		tag <- sprintf("%02d", w)
		data.frame(
			plot_id = r$UniqueID,
			DAP = as.integer(w * 7),
			weed_control_ = col(paste0("TWControl_", tag, "WAP")),
			weed_control_broadleaf_ = col(paste0("Broadleaf_", tag, "WAP")),
			weed_control_grass_ = col(paste0("Grass_", tag, "WAP")),
			weed_control_sedge_ = col(paste0("Sedge_", tag, "WAP")),
			weed_dmy_broadleaf_ = col(paste0("Broadleaf_gm2_", tag, "WAP")) * 10,
			weed_dmy_grass_ = col(paste0("Grass_gm2_", tag, "WAP")) * 10,
			weed_dmy_sedge_ = col(paste0("Sedge_gm2_", tag, "WAP")) * 10,
			weed_dmy_ = if (w == 15) col("WeedBMass_gm2_15WAP") * 10 else rep(NA_real_, nrow(r)),
			weed_density = col(paste0("Wddenm2_", tag, "WAP")) * 10000,
			sprout_density_ = col(paste0("CasSprt_ha_", tag, "WAP"))
		)
	}
	dlong <- do.call(rbind, lapply(c(2, 4, 6, 8, 10, 12, 15), one_wap))
	mcols <- c("weed_control_", "weed_control_broadleaf_", "weed_control_grass_", "weed_control_sedge_",
		"weed_dmy_broadleaf_", "weed_dmy_grass_", "weed_dmy_sedge_", "weed_dmy_", "weed_density", "sprout_density_")
	dlong <- dlong[rowSums(!is.na(dlong[, mcols])) > 0, ]
	dlong <- merge(dlong, d[, c("plot_id", "record_id")], by = "plot_id")
	dlong$plot_id <- NULL

	carobiner::write_files(path, meta, d, long = dlong)
}
