# R script for "carob"
# license: GPL (>=3)

## NOTES
# Weed-species densities for the 2019 on-station cassava and cassava/maize herbicide trials (same UniqueIDs as doi:10.25502/g4vs-5z61/d).
# File used: onstation2019_lagon_all_locs_wdspecies_datafile_rft.csv.
# The header is shifted by two columns from PlotNo through Posttreat. Values were checked against the 2019 all-locations file:
#   header PlotNo is Pretreat, header Replicate is Posttreat, header Pretreat is PlotNo, header Posttreat is Replicate.
#   Treatment, SpeciesCode and WeedSpecies match their headers. PlotNo in the header is not used as the plot id; UniqueID is.
# One wide row per plot x species. Densities at 4-15 WAP are in the long table (plants/m2 * 10000). Blanks stay NA. DAP = WAP * 7.
# Intercrop rows stay one record (cassava with intercrops maize). There is no per-crop yield in this file.
# Lagon = isoxaflutole;aclonifene. Monsoon Active = foramsulfuron;thiencarbazone-methyl;cyprosulfamide. Paraquat = paraquat dichloride, with no rate in the file.
# Species names are copied as recorded.

## ISSUES
# No yield, dates, crop injury, or coordinates. GPS for these sites is in doi:10.25502/g4vs-5z61/d and was not copied.
# Georeference: Benue (Ucha), Ogun (FUNAAB), Osun (Ago-Owu), Oyo (IITA), Rivers (Onne).
# No PostApplWAT column, so herbicide_timing is omitted. No fertilizer, variety, or irrigation. No publication DOI.
# The header shift should be confirmed by the data owner; the mapping above is from matching UniqueIDs to the all-locations file.
# cyprosulfamide stays unspaced; the vocabulary entry has a leading space.
# write_files: missing carob_contributor, yield, yield_moisture, yield_part, longitude, latitude, irrigated, N/P/K, planting_date, harvest_date;
#   unknown rate variables; invalid term cyprosulfamide.

## NOT PROCESSED
# Id: row number.
# data_dictionary.csv: definitions. It describes Pretreat as numeric, which does not match the shifted values.
# SpeciesCode: one name per code here, so WeedSpecies is kept.
# The column labelled PlotNo is the pre-emergence treatment, which is processed under that meaning.

## NEW VARIABLES
# pre_herbicide_l_ha_: pre-emergence product rate (L/ha).
# post_herbicide_l_ha_: post-emergence product rate (L/ha). NA for Paraquat, hoe, and untreated.


carob_script <- function(path) {

"
Evaluation of foramsulfuron-sodium + thiecarbazone-methyl + cyprosulfamide for post-emergence weed control in cassava (Onstation2019_Lagon_All_Locs_WdSpecies)

Monsoon Active [foramsulfuron-sodium + thiecarbazone-methyl + cyprosulfamide ] was evaluated for post-emergence weed control cassava and cassava/maize intercrop for three years at six different sites across five States [Benue, Ogun, Osun, Oyo, and the Rivers States] in Nigeria. Monsoon was compared with Paraquat, a toxic herbicide already marked for de-registration in Nigeria. The evaluation included manual hoe weeding treatments. All post-emergence herbicides treatments in the assessment were applied under the canopy with a backpack spray fitted with a sprayhood. In 2020, a treatment where Monsoon was used under cassava canopy without a sprayhood was included in the assessment. All the trials were set up in a randomized complete block design and replicated four times. Cassava was monitored for injury. Treatment efficacy was assessed visually and from weed dry matter. Cassava and maize population at crop harvest and yield were assessed. The result showed that Monsoon was superior to Paraquat and manual hoe weeding in reducing weed pressure at most sites. Monsoon provided excellent control of major cassava weeds at all the sites. Monsoon did not affect crop stand negatively or depress cassava yield in both monocrop and intercropped plots. No crop injury was observed when Monsoon was applied under cassava canopy with or without a sprayhood at 7 to 8 weeks after planting cassava, indicating that Monsoon is suitable for post-emergence weed control in cassava and a potential replacement for Paraquat.
"

	uri <- "doi:10.25502/yk2e-ny35/d"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "PJ-NA2910",
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "herbicide_product;weeding_times",
		response_vars = "weed_density",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 52,
		carob_effort = 0.7
	)

	f <- ff[basename(ff) == "onstation2019_lagon_all_locs_wdspecies_datafile_rft.csv"]
	r <- read.csv(f, na.strings = c("", " ", "NA", "na"), stringsAsFactors = FALSE, check.names = FALSE)

	# Header is shifted; see NOTES. Do not read PlotNo/Replicate/Pretreat/Posttreat by those names.
	pretreat <- r$PlotNo
	posttreat <- r$Replicate
	rep <- as.integer(r$Posttreat)

	num <- function(x) {
		x <- trimws(as.character(x))
		x[x == ""] <- NA
		as.numeric(x)
	}
	ai <- function(x) {
		x <- trimws(as.character(x))
		ifelse(is.na(x), NA_character_,
			ifelse(grepl("^Lagon", x), "isoxaflutole;aclonifene",
			ifelse(grepl("^Monsoon", x), "foramsulfuron;thiencarbazone-methyl;cyprosulfamide",
			ifelse(grepl("^Paraquat", x), "paraquat dichloride", "none"))))
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

	pre_ai <- ai(pretreat)
	post_ai <- ai(posttreat)
	product <- combine_ai(pre_ai, post_ai)
	site <- r$Site
	site[site == "AgoOwu"] <- "Ago-Owu"
	site[site == "Ucha_Makurdi"] <- "Ucha"
	inter <- r$Cropsystem == "Intercrop"

	d <- data.frame(
		plot_id = r$UniqueID,
		rep = rep,
		treatment = r$Treatment,
		country = "Nigeria",
		adm1 = r$Loc,
		location = site,
		crop = "cassava",
		intercrops = ifelse(inter, "maize", "none"),
		weed_species = trimws(r$WeedSpecies),
		herbicide_product = product,
		herbicide_used = product != "none",
		herbicide_times = as.integer((pre_ai != "none") + (post_ai != "none")),
		pre_herbicide_l_ha_ = l_ha(pretreat),
		post_herbicide_l_ha_ = l_ha(posttreat),
		weeding_times = as.integer(pmax(hoe_n(pretreat), hoe_n(posttreat))),
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE,
		stringsAsFactors = FALSE
	)
	d$weeding_done <- d$weeding_times > 0
	d$weeding_method <- ifelse(d$weeding_done, "hoe", NA_character_)
	d$weeding_implement <- ifelse(d$weeding_done, "hoe", NA_character_)
	d$adm1 <- carobiner::fix_name(d$adm1, "title")
	d$location <- carobiner::fix_name(d$location, "title")
	d$trial_id <- paste(d$location, "2019", sep = "_")
	d$record_id <- as.integer(seq_len(nrow(d)))

	pieces <- list()
	for (w in c(4, 6, 8, 10, 12, 15)) {
		tag <- sprintf("Wddenm2_%02dWAP", w)
		dens <- num(r[[tag]]) * 10000
		keep <- !is.na(dens)
		if (!any(keep)) next
		pieces[[length(pieces) + 1]] <- data.frame(
			record_id = d$record_id[keep],
			DAP = as.integer(w * 7),
			weed_density = dens[keep],
			stringsAsFactors = FALSE
		)
	}
	dlong <- do.call(rbind, pieces)

	carobiner::write_files(path, meta, d, long = dlong)
}
