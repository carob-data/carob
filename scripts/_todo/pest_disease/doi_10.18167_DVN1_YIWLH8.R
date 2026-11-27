# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Exact coordinates of the six banana fields are not in the data files;
#    midpoint of the Dataverse geographicBoundingBox in northern Martinique is
#    used with 9 km uncertainty.
# 2. Sampling campaigns are coded 2Q/2V, 3Q/3V, 4Q/4V across the three
#    consecutive annual campaigns (2019-2021). Because soil properties and
#    canopy opening were measured only in the last campaign (4Q), campaigns 2,
#    3, and 4 are mapped to 2019, 2020, and 2021 respectively.
# 3. `diversity.csv` omits the within-plot sampling point (`Replicate` 1-8)
#    column present in the other files; all 3,004 rows are retained in `long`
#    indexed by the source row number (`order`) and linked to `d` by `trial_id`
#    (plot x year).
# 4. In `soil_properties.csv`, `Infiltrability` ranges 270-2521 mm/h (27-252
#    cm/h, exceeding terminag `soil_Ksat` valid_max = 40 cm/h) and is stored in
#    `soil_infiltrability_` (mm/h).

## NEW
# cropping_system_: management system ("agroforestry" or "conventional")
# pair_: paired farm/site block ("Bourguignon", "Petite_Barriere", "Canal_Fanny")
# campaign_: sampling campaign code in wide table (2Q, 3Q, 4Q)
# fauna_campaign_: sampling campaign code in long macrofauna table (2Q, 2V, 3Q, 3V, 4Q, 4V)
# litter_twigs_, litter_cover_crop_, litter_tree_leaves_, litter_banana_leaves_: litter mass by component (g/sample)
# canopy_openness_: mean hemispherical canopy openness (%)
# soil_void_ratio_: volume of voids / volume of solids (Iv)
# soil_microporosity_, soil_macroporosity_: soil microporosity and macroporosity (%)
# soil_CN_ratio_: soil C:N ratio
# soil_infiltrability_: soil infiltrability (mm/h, 0-10 cm)
# trap_method_: macrofauna sampling method ("Quadra litiere", "Quadra sol", "test beche")
# fauna_group_: broad taxon group ("Macro-arthropods" or "Earthworms")
# fauna_class_, fauna_order_, fauna_family_, fauna_species_: taxonomic identification
# fauna_abundance_: individuals per sample
# diversity_used_: status in diversity analysis ("yes", "no", "garder larve")

## MISSING
# yield, yield_moisture, yield_part, planting_date, harvest_date.

## NOTES
# Paired comparison of 3 agroforestry vs 3 conventional commercial banana
# fields in Martinique over 3 campaigns (2019-2021), 8 sampling points per
# plot. Wide table `d` combines litter composition (144 plot x rep x year
# samples), canopy openness (48 samples in 2021), and soil physico-chemical
# properties at 0-10 cm and 10-20 cm (96 samples in 2021). Long table `long`
# holds all 3,004 macroinvertebrate abundance records from `diversity.csv`.

## NOT PROCESSED
# Raw ImageJ particle-by-particle image measurements (`arthropods_imagej.csv`,
# `earthworms_imagej.csv`, `traits_litter.csv`) and their codebook spreadsheets
# (`*_labels.xlsx`, `diversity _label.csv`).


carob_script <- function(path) {

"
Abundance, Biomass, and Functional Traits of Macroinvertebrates and Environmental Parameters in Banana Agroecosystems: A Dataset Comparing Agroforestry and Conventional Systems

This dataset includes values for abundance, biomass, and functional traits of macroinvertebrates (arthropods and earthworms), as well as data on soil physicochemical properties, canopy cover and litter traits. A total of 144 samples were collected from the soil of 6 banana fields (3 conventional and 3 agroforestry) in Martinique. Three sampling campaigns were conducted over three consecutive years (2019, 2020, and 2021). Eight independent sampling points were defined per plot for each of which two distinct methods were used: (i) the litter quadrat followed by Tullgren extraction of macroarthropods and (ii) the soil quadrat followed by manual sorting of soil to collect earthworms until 30 cm depth. The individuals were identified to the most precise taxonomic level possible. Soil physico-chemical properties and vegetation cover were measured only during the last campaign.
"

	uri <- "doi:10.18167/DVN1/YIWLH8"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "cropping_system_",
		response_vars = "fauna_abundance_;canopy_openness_;soil_pH;soil_C_total;soil_bd",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 75,
		carob_effort = 0
	)

	read_fr <- function(fname) {
		f <- ff[basename(ff) == fname]
		r <- read.csv(f, sep=";", dec=",", na.strings=c("NA", ""), check.names=FALSE, fileEncoding="Latin1")
		names(r) <- trimws(names(r))
		for (nm in names(r)) {
			if (is.character(r[[nm]])) r[[nm]] <- trimws(enc2utf8(r[[nm]]))
		}
		r
	}

	norm_sys <- function(x) {
		ifelse(grepl("^agro", tolower(x)), "agroforestry", "conventional")
	}
	camp_year <- function(x) {
		c("2"="2019", "3"="2020", "4"="2021")[substr(trimws(x), 1, 1)]
	}

	## 1. Litter composition (144 samples: 6 plots x 8 reps x 3 campaigns 2Q/3Q/4Q)
	r_lit <- read_fr("composition_litter.csv")
	d_lit <- data.frame(
		sample_id = r_lit$Code,
		campaign_ = r_lit$Date,
		date = unname(camp_year(r_lit$Date)),
		plot_id = gsub("_", "-", r_lit$Plot),
		pair_ = r_lit$Pair,
		cropping_system_ = norm_sys(r_lit$System),
		rep = as.integer(r_lit$Replicate),
		litter_twigs_ = as.numeric(r_lit$Category_TW),
		litter_cover_crop_ = as.numeric(r_lit$Category_CC),
		litter_tree_leaves_ = as.numeric(r_lit$Category_TL),
		litter_banana_leaves_ = as.numeric(r_lit$Category_BL)
	)
	d_lit$trial_id <- paste(d_lit$plot_id, d_lit$date, sep="_")
	d_lit$treatment <- d_lit$cropping_system_

	## 2. Canopy openness (48 samples in 4Q / 2021, 3 hemispherical photos each)
	r_can <- read_fr("canopee_opening.csv")
	open1 <- 100 * as.numeric(r_can$Canopy_Opening_1) / as.numeric(r_can$Total_1)
	open2 <- 100 * as.numeric(r_can$Canopy_Opening_2) / as.numeric(r_can$Total_2)
	open3 <- 100 * as.numeric(r_can$Canopy_Opening_3) / as.numeric(r_can$Total_3)
	d_can <- data.frame(
		sample_id = r_can$Code,
		canopy_openness_ = rowMeans(cbind(open1, open2, open3), na.rm=TRUE)
	)
	d <- merge(d_lit, d_can, by="sample_id", all.x=TRUE)

	## 3. Soil properties (96 rows in 4Q / 2021: 48 samples x 2 depths 0-10 & 10-20 cm)
	r_soil <- read_fr("soil_properties.csv")
	d_soil <- data.frame(
		sample_id = r_soil$Code,
		soil_sample_date = "2021",
		depth_top = ifelse(r_soil$Depth == "0-10cm", 0, 10),
		depth_bottom = ifelse(r_soil$Depth == "0-10cm", 10, 20),
		soil_bd = as.numeric(r_soil$Dapp),
		soil_GWC = as.numeric(r_soil$Eaufield) / 100,
		soil_FC = as.numeric(r_soil$EauCC),
		soil_void_ratio_ = as.numeric(r_soil$Iv),
		soil_microporosity_ = as.numeric(r_soil$Microporosite),
		soil_macroporosity_ = as.numeric(r_soil$Macroporosite),
		soil_stones = as.numeric(r_soil$Pierrosite),
		soil_pH = as.numeric(r_soil$pH),
		soil_C_total = as.numeric(r_soil$C),
		soil_N_total = as.numeric(r_soil$N) * 10000,
		soil_CN_ratio_ = as.numeric(r_soil$CN),
		soil_infiltrability_ = as.numeric(r_soil$Infiltrability)
	)
	d <- merge(d, d_soil, by="sample_id", all.x=TRUE)

	d$country <- "Martinique"
	d$crop <- "banana"
	## Midpoint of Dataverse geographicBoundingBox (-61.1573..-61.0654 W, 14.7764..14.8983 N)
	d$latitude <- 14.8374
	d$longitude <- -61.1114
	d$geo_uncertainty <- 9000
	d$geo_source <- "Dataverse metadata geographicBoundingBox midpoint (northern Martinique)"
	d$geo_from_source <- FALSE
	d$on_farm <- TRUE
	d$is_survey <- TRUE

	## 4. Macroinvertebrate diversity & abundance (3,004 rows, linked via trial_id)
	r_div <- read_fr("diversity.csv")
	clean <- function(x) {
		x <- trimws(as.character(x))
		x[x == ""] <- NA_character_
		x
	}
	long <- data.frame(
		trial_id = paste(gsub("_", "-", r_div$Plot), unname(camp_year(r_div$Date)), sep="_"),
		order = as.integer(r_div[[1]]),
		fauna_campaign_ = clean(r_div$Date),
		trap_method_ = clean(r_div$trapping_method),
		fauna_group_ = clean(r_div$taxa),
		fauna_class_ = clean(r_div$nom_classe),
		fauna_order_ = clean(r_div$nom_ordre),
		fauna_family_ = clean(r_div$nom_famille),
		fauna_species_ = clean(r_div$nom_espece),
		fauna_abundance_ = as.numeric(r_div$abundance),
		diversity_used_ = clean(r_div$diversity)
	)

	carobiner::write_files(path, meta, d, long=long)
}
