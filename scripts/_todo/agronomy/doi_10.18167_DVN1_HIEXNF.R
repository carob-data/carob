# R script for "carob"
# license: GPL (>=3)

## NOTES
# Field plots at Neufchâteau, Capesterre-Belle-Eau, Guadeloupe, 2013. Not pots. The data paper describes one plant per 16 m2 plot.
# Species names and roles are Table 1 of doi:10.1016/j.dib.2020.105890. Code P is in the file and is not in that table.
# Aboveground dry mass is g per plant. kg/ha uses 10000/16. The paper reports 50 g urea (46% N) per plant and a rain-fed regime, and no P or K, so P and K are 0.
# Coordinates 16°05'N, 61°35'W are from that paper.
# Planting date is the collection date minus days after sowing.

## ISSUES
# No fruit yield. The harvest is vegetative biomass.
# Code P has no species name.
# Some cover crops are shorter than 10 cm, as measured.
# missing variables: yield, yield_moisture, yield_part, harvest_date
# unknown variable: rooting_depth_ (depth?)
# unknown variables: plant_role_
# out of bounds: plant_height (7, 460)
# invalid terms: crop: Arachis pintoi, Bidens pilosa, Centrosema pascuorum, Mucuna deeringiana, Neonotonia wightii, Pueraria phaseolides

## NOT PROCESSED
# Crown widths and projected area, leaf-blade and petiole fractions, SLA, LDMC, LNC, root diameter, specific root length, root length and weight density, and root-impact counts.
# Nodule score. The code is categorical and the scale is not defined in the variables file.
# data_CC_indiv_plant_variables.txt: not read. columns: Abbreviations, Variable names, Units, Organs, Calculation. Observation records are missing from d until this file is standardized.


## NEW VARIABLES
# plant_role_: cover crop, spontaneous, or banana, from Table 1. Missing for code P.
# rooting_depth_: maximum rooting depth, cm.


carob_script <- function(path) {

"
A dataset on above- and below-ground traits of 17 cover crops, two wild plant species and two banana cultivars, cultivated individually

This dataset describes 21 species of cover crops (17 sp.), wild plant species (2 sp.) and bananas (2 sp.), each repeated four times and cultivated individually under non-limiting conditions, on 16m² plots. Data were acquired at flowering (annuals with short cycle) or after six months of growth (other species/varieties). The traits and variables measured refer to whole plant (7 variables), leaves (21 variables), and roots (12 variables). This dataset is made available to enable comparisons between datasets, extended analysis and meta-analysis on cover crops
"

	uri <- "doi:10.18167/DVN1/HIEXNF"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = "doi:10.1016/j.dib.2020.105890",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "plant_height;dmy_total",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 40,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "data_CC_indiv_plant.txt"]
	r <- read.delim(f, na.strings=c("NA", ""), check.names=FALSE)
	names(r) <- make.unique(trimws(names(r)))
	r$esp <- trimws(r$esp)
	r <- r[!is.na(r$esp) & nzchar(r$esp), ]

	# Table 1 of the data paper. Pueraria is spelled phaseolides there. Mucana is the table spelling, stored as Mucuna deeringiana.
	sp <- data.frame(
		esp = c("AP", "B925", "Bcav", "BD", "BP", "BR", "CCG", "CP", "CS", "CZ", "DL", "GS", "MD", "N", "NW", "P", "PN", "PP", "RC", "SG", "TP"),
		crop = c("Arachis pintoi", "banana", "banana", "brachiaria", "Bidens pilosa", "brachiaria", "pigeon pea", "Centrosema pascuorum", "crotalaria", "crotalaria", "lablab", "gliricidia", "Mucuna deeringiana", "cowpea", "Neonotonia wightii", NA_character_, "bahia grass", "Pueraria phaseolides", "castor bean", "stylosanthes", "marigold"),
		variety = c("Arachis pintoi", "Cirad 925", "Cavendish", "Brachiaria decumbens", "Bidens pilosa", "Brachiaria ruziziensis", "Cajanus cajan Guadeloupe", "Centrosema pascuorum", "Crotalaria spectabilis", "Crotalaria zanzibarica", "Dolichos lablab", "Gliricidia sepium", "Mucuna deeringiana", "Vigna unguiculata David", "Neonotonia wightii", "P", "Paspalum notatum", "Pueraria phaseolides", "Ricinus communis", "Stylosanthes guianensis", "Tagetes patula"),
		plant_role_ = c("cover crop", "banana", "banana", "cover crop", "spontaneous", "cover crop", "cover crop", "cover crop", "cover crop", "cover crop", "cover crop", "cover crop", "cover crop", "cover crop", "cover crop", NA_character_, "cover crop", "cover crop", "cover crop", "cover crop", "cover crop")
	)
	r <- merge(r, sp, by="esp", all.x=TRUE)

	# g per plant on a 16 m2 plot, one plant per plot.
	to_ha <- 10000 / 16 / 1000
	collected <- as.Date(trimws(r$date), "%d/%m/%Y")
	block <- trimws(r$block)

	d <- data.frame(
		trial_id = "Neufchateau_2013",
		plot_id = paste(r$esp, block, sep="_"),
		rep = as.integer(sub("^B", "", block)),
		country = "Guadeloupe",
		location = "Neufchateau",
		# Data paper: 16°05'N, 61°35'W.
		latitude = 16 + 5/60,
		longitude = -(61 + 35/60),
		geo_from_source = TRUE,
		crop = r$crop,
		variety = r$variety,
		plant_role_ = r$plant_role_,
		treatment = r$variety,
		planting_date = as.character(collected - as.integer(r$DAS)),
		date = as.character(collected),
		plant_height = as.numeric(r$H),
		dmy_stems = as.numeric(r$DWs) * to_ha,
		dmy_leaves = as.numeric(r$DWleaf) * to_ha,
		dmy_total = as.numeric(r$BMa) * to_ha,
		rooting_depth_ = as.numeric(r$RDmax),
		# 50 g urea (46% N) per plant. No P or K is reported.
		N_fertilizer = 50 * 0.46 * (10000 / 16) / 1000,
		P_fertilizer = 0,
		K_fertilizer = 0,
		irrigated = FALSE,
		on_farm = FALSE,
		is_survey = FALSE
	)

	carobiner::write_files(path, meta, d)
}
