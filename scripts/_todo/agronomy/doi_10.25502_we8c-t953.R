# R script for "carob"
# license: GPL (>=3)

## NOTES
# Weed-species densities for the 2018 on-station cassava herbicide trials (same treatments and UniqueIDs as doi:10.25502/rnbm-3x44/d).
# File used: onstation2018_lagon_all_loc_weedspecies_datafile_rft.csv. Sole cassava only.
# One wide row per plot x species. Counts at 4, 6, 8, 10 and 12 WAP are in the long table (plants/m2 * 10000 = plants/ha). Blank cells stay NA, not zero. DAP = WAP * 7.
# Herbicide names follow the 2018 all-locations script: Lagon = isoxaflutole;aclonifene, Lifeline = glufosinate-ammonium, Monsoon+Adjuvant = foramsulfuron;thiencarbazone-methyl;cyprosulfamide. Rates in the treatment text are L product/ha.
# Species names are copied as recorded, including misspellings.

## ISSUES
# No yield, planting date, harvest date, crop injury, or coordinates in this file.
# GPS for these stations is in doi:10.25502/rnbm-3x44/d (UAM, FUNAAB, Ago-Owu, IITA Ibadan, Onne). Not copied here.
# States to georeference: Benue (UAM), Ogun (FUNAAB), Osun (Ago-Owu), Oyo (IITA Ibadan), Rivers (Onne).
# No fertilizer, variety, or irrigation. No publication DOI.
# cyprosulfamide is kept despite the vocabulary entry having a leading space.
# write_files: missing carob_contributor, yield, yield_moisture, yield_part, longitude, latitude, irrigated, N/P/K, planting_date, harvest_date;
#   NA herbicide_timing on hoe and untreated plots; unknown rate and adjuvant variables; invalid term cyprosulfamide.

## NOT PROCESSED
# ID: row number.
# data_dictionary.csv: definitions only.
# SpeciesCode: one-to-one with WeedSpecies in this file, so the name is kept and the code is not.

## NEW VARIABLES
# pre_herbicide_l_ha_: pre-emergence product rate (L/ha).
# post_herbicide_l_ha_: post-emergence product rate (L/ha).
# post_adjuvant_: TRUE when the post treatment is Monsoon+Adjuvant (product not named).


carob_script <- function(path) {

"
Evaluation of foramsulfuron-sodium + thiecarbazone-methyl + cyprosulfamide for post-emergence weed control in cassava (Onstation2018_Lagon_All_Loc_WeedSpecies)

Monsoon Active [foramsulfuron-sodium + thiecarbazone-methyl + cyprosulfamide ] was evaluated for post-emergence weed control cassava and cassava/maize intercrop for three years at six different sites across five States [Benue, Ogun, Osun, Oyo, and the Rivers States] in Nigeria. Monsoon was compared with Paraquat, a toxic herbicide already marked for de-registration in Nigeria. The evaluation included manual hoe weeding treatments. All post-emergence herbicides treatments in the assessment were applied under the canopy with a backpack spray fitted with a sprayhood. In 2020, a treatment where Monsoon was used under cassava canopy without a sprayhood was included in the assessment. All the trials were set up in a randomized complete block design and replicated four times. Cassava was monitored for injury. Treatment efficacy was assessed visually and from weed dry matter. Cassava and maize population at crop harvest and yield were assessed. The result showed that Monsoon was superior to Paraquat and manual hoe weeding in reducing weed pressure at most sites. Monsoon provided excellent control of major cassava weeds at all the sites. Monsoon did not affect crop stand negatively or depress cassava yield in both monocrop and intercropped plots. No crop injury was observed when Monsoon was applied under cassava canopy with or without a sprayhood at 7 to 8 weeks after planting cassava, indicating that Monsoon is suitable for post-emergence weed control in cassava and a potential replacement for Paraquat.
"

	uri <- "doi:10.25502/we8c-t953"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "PJ-NA2910",
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "herbicide_product;weeding_times;herbicide_timing",
		response_vars = "weed_density",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "onstation2018_lagon_all_loc_weedspecies_datafile_rft.csv"]
	r <- read.csv(f, na.strings = c("", " ", "NA", "na"), check.names = FALSE)

	num <- function(x) {
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
		crop = "cassava",
		intercrops = "none",
		weed_species = trimws(r$WeedSpecies),
		herbicide_product = product,
		herbicide_used = product != "none",
		herbicide_times = as.integer((pre_ai != "none") + (post_ai != "none")),
		herbicide_timing = ifelse(post_ai != "none" & !is.na(wat) & wat > 0, as.character(as.integer(wat * 7)), NA_character_),
		pre_herbicide_l_ha_ = l_ha(r$Pretreat),
		post_herbicide_l_ha_ = l_ha(r$Posttreat),
		post_adjuvant_ = grepl("Adjuvant", ifelse(is.na(r$Posttreat), "", r$Posttreat)),
		weeding_times = as.integer(pmax(hoe_n(r$Pretreat), hoe_n(r$Posttreat))),
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	d$weeding_done <- d$weeding_times > 0
	d$weeding_method <- ifelse(d$weeding_done, "hoe", NA_character_)
	d$weeding_implement <- ifelse(d$weeding_done, "hoe", NA_character_)
	d$adm1 <- carobiner::fix_name(d$adm1, "title")
	d$location <- carobiner::fix_name(d$location, "title")
	d$trial_id <- paste(d$location, "2018", sep = "_")
	d$record_id <- as.integer(seq_len(nrow(d)))

	pieces <- list()
	for (w in c(4, 6, 8, 10, 12)) {
		tag <- sprintf("Wddenm2_%02dWAP", w)
		dens <- num(r[[tag]]) * 10000
		keep <- !is.na(dens)
		if (!any(keep)) next
		pieces[[length(pieces) + 1]] <- data.frame(
			record_id = d$record_id[keep],
			DAP = as.integer(w * 7),
			weed_density = dens[keep]
		)
	}
	dlong <- do.call(rbind, pieces)

	carobiner::write_files(path, meta, d, long = dlong)
}
