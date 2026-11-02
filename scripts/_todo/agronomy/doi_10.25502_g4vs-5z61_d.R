# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-station 2019 RCBD (4 reps), sole cassava and cassava/maize, five Nigerian stations.
# File used: onstation2019_lagon_all_locations_datafile_rft.csv. Dictionary used for units.
# RootYldtha is fresh roots (t/ha -> kg/ha). Maize grain is t/ha at 12% moisture (dictionary), so maize yield_moisture is 12.
# Intercrop plots are two rows (cassava and maize). Sole cassava has no maize row.
# Stem weight mapped to fwy_stems with the same caveat as the 2018 file: this dictionary does not say fresh or dry.
# Visual scores are percent control efficacy, not weed cover. Dry matter g/m2 * 10 = kg/ha. Density plants/m2 * 10000.
# Dates in this file are month/day/year (7/15/2020 and 10/18/2019 cannot be day/month).
# DAP = WAP * 7. Harvest plant_density stays on the wide table; sprout and maize germination counts are in the long table.
# Lagon = isoxaflutole;aclonifene. Monsoon Active = foramsulfuron;thiencarbazone-methyl;cyprosulfamide. Paraquat = paraquat dichloride (no rate in the file). Rates that are written as L/ha are product volume, not kg ai.
# GPS in the file is used as recorded, including the small monocrop vs intercrop differences at FUNAAB and Ucha.
# Long weed and stand rows are linked to the cassava row of the plot (same plot_id on the maize row).

## ISSUES
# No crop-injury column. The abstract's "no injury" result was not copied onto plots.
# No variety, fertilizer, irrigation, or plot area. No publication DOI in the metadata.
# herbicide_amount omitted: recorded rates are L product/ha, and Paraquat has no rate.
# PostApplWAT is blank on many plots and, where filled, is not one timing per treatment (Monsoon Active 1.5 L/ha occurs as 6, 7, 8 and 12 WAT). Timing is kept only where a number is stored.
# Hoe weeks in the treatment labels were not expanded to weeding_dates.
# cyprosulfamide is not accepted because that vocabulary entry has a leading space.
# Species densities are doi:10.25502/yk2e-ny35/d. That file's PlotNo/Replicate/Pretreat/Posttreat header is shifted; see that script.
# One plot has plant_density 0 and stem_density 0 (minimum is 500). Left as recorded. Maize stands up to about 52600 plants/ha are inside the maximum.
# write_files: missing carob_contributor, irrigated, N/P/K;
#   NA in herbicide_timing where the post treatment is hoe, untreated, or blank;
#   unknown variables in NEW VARIABLES (sprout_density_ is a sprout count, not shrub_density; maize_density_ is not vine_density);
#   out of bounds plant_density and stem_density because of zeros;
#   invalid term cyprosulfamide.
# Terminag GitHub 403 warnings, if printed, come from the vocabulary check rather than these data.

## NOT PROCESSED
# ID: row number. PlotNo is contained in UniqueID, which is plot_id.
# data_dictionary.csv: definitions only.
# Quadrat counts are not in this file.

## NEW VARIABLES
# pre_herbicide_l_ha_: pre-emergence product rate (L/ha).
# post_herbicide_l_ha_: post-emergence product rate (L/ha). NA for Paraquat, hoe, and untreated.
# marketable_root_density_: marketable cassava roots per ha.
# fwy_cobs_: fresh maize cob yield (kg/ha), from FrshCobYld_tha.
# weed_control_: total visual control efficacy (%).
# weed_control_broadleaf_: broadleaf control efficacy (%).
# weed_control_grass_: grass control efficacy (%).
# weed_control_sedge_: sedge control efficacy (%).
# weed_dmy_broadleaf_: broadleaf weed dry matter (kg/ha).
# weed_dmy_grass_: grass weed dry matter (kg/ha).
# weed_dmy_sedge_: sedge weed dry matter (kg/ha).
# sprout_density_: cassava sprouts (plants/ha) at that DAP.
# maize_density_: maize plants/ha at that DAP (germination counts).


carob_script <- function(path) {

"
Evaluation of foramsulfuron-sodium + thiecarbazone-methyl + cyprosulfamide for post-emergence weed control in cassava (Onstation2019_Lagon_All_Locations)

Monsoon Active [foramsulfuron-sodium + thiecarbazone-methyl + cyprosulfamide ] was evaluated for post-emergence weed control cassava and cassava/maize intercrop for three years at six different sites across five States [Benue, Ogun, Osun, Oyo, and the Rivers States] in Nigeria. Monsoon was compared with Paraquat, a toxic herbicide already marked for de-registration in Nigeria. The evaluation included manual hoe weeding treatments. All post-emergence herbicides treatments in the assessment were applied under the canopy with a backpack spray fitted with a sprayhood. In 2020, a treatment where Monsoon was used under cassava canopy without a sprayhood was included in the assessment. All the trials were set up in a randomized complete block design and replicated four times. Cassava was monitored for injury. Treatment efficacy was assessed visually and from weed dry matter. Cassava and maize population at crop harvest and yield were assessed. The result showed that Monsoon was superior to Paraquat and manual hoe weeding in reducing weed pressure at most sites. Monsoon provided excellent control of major cassava weeds at all the sites. Monsoon did not affect crop stand negatively or depress cassava yield in both monocrop and intercropped plots. No crop injury was observed when Monsoon was applied under cassava canopy with or without a sprayhood at 7 to 8 weeks after planting cassava, indicating that Monsoon is suitable for post-emergence weed control in cassava and a potential replacement for Paraquat.
"

	uri <- "doi:10.25502/g4vs-5z61/d"
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
		carob_completion = 74,
		carob_effort = 0.8
	)

	f <- ff[basename(ff) == "onstation2019_lagon_all_locations_datafile_rft.csv"]
	r <- read.csv(f, na.strings = c("", " ", "NA", "na"), check.names = FALSE)

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
	product <- combine_ai(pre_ai, post_ai)
	wat <- num(r$PostApplWAT)
	timing <- ifelse(post_ai != "none" & !is.na(wat) & wat > 0, as.character(as.integer(wat * 7)), NA_character_)
	site <- r$Site
	site[site == "AgoOwu"] <- "Ago-Owu"
	site[site == "Ucha_Makurdi"] <- "Ucha"
	weed_n <- as.integer(pmax(hoe_n(r$Pretreat), hoe_n(r$Posttreat)))
	inter <- r$CropSystem == "Intercrop"

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
		intercrops = ifelse(inter, "maize", "none"),
		yield = num(r$RootYldtha) * 1000,
		yield_part = "roots",
		yield_isfresh = TRUE,
		yield_moisture = as.numeric(NA),
		fwy_stems = num(r$Wtstemtha) * 1000,
		plant_density = num(r$Standha_Cas),
		stem_density = num(r$Stemha_Cas),
		marketable_root_density_ = num(r$NoOkRootsha),
		fwy_cobs_ = as.numeric(NA),
		planting_date = as.character(as.Date(r$Date_Planted_Cas, "%m/%d/%Y")),
		harvest_date = as.character(as.Date(r$Date_Harvested_Cas, "%m/%d/%Y")),
		herbicide_product = product,
		herbicide_used = product != "none",
		herbicide_times = as.integer((pre_ai != "none") + (post_ai != "none")),
		herbicide_timing = timing,
		pre_herbicide_l_ha_ = l_ha(r$Pretreat),
		post_herbicide_l_ha_ = l_ha(r$Posttreat),
		weeding_times = weed_n,
		on_farm = FALSE,
		is_survey = FALSE
	)

	mz <- d[inter, ]
	mz$crop <- "maize"
	mz$intercrops <- "cassava"
	mz$yield <- num(r$MzGrainYld_tha)[inter] * 1000
	mz$yield_part <- "grain"
	mz$yield_isfresh <- FALSE
	mz$yield_moisture <- 12
	mz$fwy_stems <- as.numeric(NA)
	mz$plant_density <- num(r$MzStandCount_ha)[inter]
	mz$stem_density <- as.numeric(NA)
	mz$marketable_root_density_ <- as.numeric(NA)
	mz$fwy_cobs_ <- num(r$FrshCobYld_tha)[inter] * 1000
	mz$planting_date <- as.character(as.Date(r$Date_Planted_Mz, "%m/%d/%Y"))[inter]
	mz$harvest_date <- as.character(as.Date(r$Date_Harvested_Mz, "%m/%d/%Y"))[inter]
	d <- rbind(d, mz)

	d$weeding_done <- d$weeding_times > 0
	d$weeding_method <- ifelse(d$weeding_done, "hoe", NA_character_)
	d$weeding_implement <- ifelse(d$weeding_done, "hoe", NA_character_)
	d$adm1 <- carobiner::fix_name(d$adm1, "title")
	d$location <- carobiner::fix_name(d$location, "title")
	d$trial_id <- paste(d$location, "2019", sep = "_")
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
			weed_density = col(paste0("Wddenm2_", tag, "WAP")) * 10000,
			sprout_density_ = col(paste0("CasSprt_ha_", tag, "WAP")),
			maize_density_ = col(paste0("MzGerm_ha_", tag, "WAP"))
		)
	}
	dlong <- do.call(rbind, lapply(c(2, 4, 6, 8, 10, 12, 15), one_wap))
	mcols <- c("weed_control_", "weed_control_broadleaf_", "weed_control_grass_", "weed_control_sedge_",
		"weed_dmy_broadleaf_", "weed_dmy_grass_", "weed_dmy_sedge_", "weed_density", "sprout_density_", "maize_density_")
	dlong <- dlong[rowSums(!is.na(dlong[, mcols])) > 0, ]
	cas <- d[d$crop == "cassava", c("plot_id", "record_id")]
	dlong <- merge(dlong, cas, by = "plot_id")
	dlong$plot_id <- NULL

	carobiner::write_files(path, meta, d, long = dlong)
}
