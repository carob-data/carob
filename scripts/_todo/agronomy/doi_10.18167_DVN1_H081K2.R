# R script for "carob"
# license: GPL (>=3)

## NOTES
# Akoupé-Zeudji, Côte d'Ivoire. Country and the city are geographicCoverage. Planting is 2023-05-16 or 2023-05-18.
# Irrigation is Ir or Ni. Fertiliser codes, from the meanings file, are g per tree per month: 0 is 0 g NPK and 100 g KCl; M is 25 g NPK and 85 g KCl; P is 75 g NPK and 50 g KCl. KCl starts at the fifth month. The NPK grade is not stated, so there is no kg/ha of N, P, or K.
# Bunch weight is kg per plant. Density is not stated.
# Cycle 1 and 2 heights are at flowering. Cycle 3 height is the first measurement on the follower (the column is spelled heigth). The long table keeps one row per plant and cycle.

## ISSUES
# missing variables: yield, yield_moisture, yield_part, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer
# unknown variables: fertiliser_regime_, npk_g_tree_month_, kcl_g_tree_month_
# unknown variables: cycle_, pseudostem_girth50_, pseudostem_girth100_, bunch_weight_, hands_, fingers_, living_leaves_, sigatoka_leaf4_, sigatoka_leaf5_, sigatoka_leaf6_, youngest_necrotic_leaf_
# out of bounds: flowering_days (172, 617), harvest_days (256, 617), plant_height (9, 469)

## NOT PROCESSED
# First-leaf length and width, observer, SPAD, leaf size at harvest, and finger grade, girth, and length.
# Ridge number.
# Villardebo count of new galleries only. The full index is pest_severity.
# INoVar_Dataverse_data meanings.csv: not read. columns: Unique ID per banana tree;Unique ID per banana tree. Observation records are missing from d until this file is standardized.


## NEW VARIABLES
# fertiliser_regime_: 0, M, or P, as coded in the file.
# npk_g_tree_month_: NPK product, g per tree per month. Code 0 is 0 g.
# kcl_g_tree_month_: KCl, g per tree per month from the fifth month.
# cycle_: crop cycle 1, 2, or 3.
# pseudostem_girth50_: girth 50 cm above the corm, cm.
# pseudostem_girth100_: girth 100 cm above the corm, cm.
# bunch_weight_: bunch weight, kg per plant.
# hands_: hands on the bunch.
# fingers_: fingers on the bunch.
# sigatoka_leaf4_: black sigatoka lesions on the fourth leaf, cycle 1.
# sigatoka_leaf5_: lesions on the fifth leaf.
# sigatoka_leaf6_: lesions on the sixth leaf.
# youngest_necrotic_leaf_: rank of the youngest leaf with necrotic tissue.
# living_leaves_: living leaves at harvest. The file spells the column "leaving".


carob_script <- function(path) {

"
Essai agronomique fertilisation et irrigation de 8 variétés de bananiers plantain en Côte d'Ivoire, 2023 - 2025

Essai agronomique visant à déterminer les variétés les plus adaptées aux conditions de culture du producteur. Données de croissance, de rendement et de dégâts de charançons (Villardebo) de 8 variétés de bananiers plantain sur deux cycles de culture. Cet essai inclus 3 modalités de fertilisation azoté et 2 modalités d'irrigation.

An agronomic trial designed to identify the varieties best suited to the grower's cultivation conditions. Data on growth, yield, and weevil damage (Villardebo) for 8 plantain varieties over two growing cycles. This trial included 3 nitrogen fertilization treatments and 2 irrigation treatments.
"

	uri <- "doi:10.18167/DVN1/H081K2"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	# English and French descriptions make yuri build two metadata rows and stop. Both texts stay in one description.
	jf <- list.files(file.path(path, "data", "raw", group, "doi_10.18167_DVN1_H081K2"), pattern="\\.json$", full.names=TRUE)
	js <- jsonlite::read_json(jf)
	fields <- js$data$latestVersion$metadataBlocks$citation$fields
	for (i in seq_along(fields)) {
		if (identical(fields[[i]]$typeName, "dsDescription") && length(fields[[i]]$value) > 1) {
			texts <- vapply(fields[[i]]$value, function(z) z$dsDescriptionValue$value, "")
			fields[[i]]$value[[1]]$dsDescriptionValue$value <- paste(texts, collapse="\n\n")
			fields[[i]]$value <- fields[[i]]$value[1]
			js$data$latestVersion$metadataBlocks$citation$fields <- fields
			jsonlite::write_json(js, jf, auto_unbox=TRUE)
		}
	}

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety;irrigated;fertiliser_regime_",
		response_vars = "plant_height;bunch_weight_;pest_severity",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 0.7
	)

	f <- ff[basename(ff) == "INoVar_Dataverse.csv"]
	r <- read.csv(f, sep=";", na.strings=c("NA", ""), check.names=FALSE)
	names(r) <- gsub("\\s+", " ", trimws(names(r)))

	vname <- c(BA="Batard", BE="Big Ebanga", BC="Blanche Créole", CE="Curare Enano", DH="Dominico Harton", MP="MP1", MB="Mbouroukou 3", OR="Orishele")
	# Meanings file, g per tree per month. Code 0 is no NPK.
	npk <- c("0"=0, "M"=25, "P"=75)
	kcl <- c("0"=100, "M"=85, "P"=50)
	fert <- as.character(r$fertilisation)

	d <- data.frame(
		trial_id = "Akoupe_2023",
		plot_id = as.character(r[["Unique ID per banana tree"]]),
		variety = unname(vname[r[["Plantain variety"]]]),
		treatment = paste(unname(vname[r[["Plantain variety"]]]), r$irrigation, fert),
		fertiliser_regime_ = fert,
		npk_g_tree_month_ = unname(npk[fert]),
		kcl_g_tree_month_ = unname(kcl[fert]),
		irrigated = r$irrigation == "Ir",
		planting_date = as.character(as.Date(r[["Plantation date"]])),
		country = "Côte d'Ivoire",
		location = "Akoupé-Zeudji",
		crop = "plantain",
		stress = "pests;disease",
		on_farm = TRUE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	one <- function(n, height_col, girth50, girth100, flower, harvest, bunch, hands, fingers, leaves, vill, s4, s5, s6, yls) {
		col_or <- function(nm, as_date=FALSE) {
			if (!nm %in% names(r)) {
				if (as_date) return(rep(NA_character_, nrow(r)))
				return(rep(NA_real_, nrow(r)))
			}
			if (as_date) return(as.character(as.Date(r[[nm]])))
			# Bunch weights use a comma as the decimal mark.
			x <- gsub(",", ".", trimws(as.character(r[[nm]])), fixed=TRUE)
			x[x %in% c("", "NA")] <- NA_character_
			as.numeric(x)
		}
		fl <- col_or(flower, TRUE)
		hv <- col_or(harvest, TRUE)
		when <- ifelse(!is.na(hv), hv, fl)
		data.frame(
			trial_id = d$trial_id,
			plot_id = d$plot_id,
			cycle_ = n,
			date = when,
			flowering_date = fl,
			harvest_date = hv,
			flowering_days = as.numeric(as.Date(fl) - as.Date(d$planting_date)),
			harvest_days = as.numeric(as.Date(hv) - as.Date(d$planting_date)),
			plant_height = col_or(height_col),
			pseudostem_girth50_ = col_or(girth50),
			pseudostem_girth100_ = col_or(girth100),
			bunch_weight_ = col_or(bunch),
			hands_ = col_or(hands),
			fingers_ = col_or(fingers),
			living_leaves_ = col_or(leaves),
			pest_severity = if (vill %in% names(r)) as.character(r[[vill]]) else rep(NA_character_, nrow(r)),
			sigatoka_leaf4_ = col_or(s4),
			sigatoka_leaf5_ = col_or(s5),
			sigatoka_leaf6_ = col_or(s6),
			youngest_necrotic_leaf_ = col_or(yls)
		)
	}

	d1 <- one(1,
		"Cycle 1 height of plantain tree at flowering",
		"Cycle 1 pseudostem girth 50cm above corm at flowering",
		"Cycle 1 pseudostem girth 100cm above corm at flowering",
		"Cycle 1 flowering date",
		"Cycle 1 date of harvest",
		"Cycle 1 weight of the bunch",
		"Cycle 1 number of hands of the bunch",
		"Cycle 1 number of fingers of the bunch",
		"Cycle 1 number of leaving leaves at harvest",
		"Cycle 1 Villardebo index at harvest",
		"Cycle 1 number of black sigatoka lesion of the fourth leaf",
		"Cycle 1 number of black sigatoka lesion of the fifth leaf",
		"Cycle 1 number of black sigatoka lesion of the sixth leaf",
		"Cycle 1 Youngest leave presenting necrotic tissues")
	d2 <- one(2,
		"Cycle 2 height of plantain tree at flowering",
		"Cycle 2 pseudostem girth 50cm above corm at flowering",
		"Cycle 2 pseudostem girth 100cm above corm at flowering",
		"Cycle 2 flowering date",
		"Cycle 2 date of harvest",
		"Cycle 2 weight of the bunch",
		"Cycle 2 number of hands of the bunch",
		"Cycle 2 number of fingers of the bunch",
		"Cycle 2 number of leaving leaves at harvest",
		"Cycle 2 Villardebo index at harvest",
		"", "", "", "")
	d3 <- one(3,
		"Cycle 3 heigth at measuring date",
		"Cycle 3 pseudostem girth 50cm above corm at measuring date",
		"Cycle 3 pseudostem girth 100cm above corm at measuring date",
		"Cycle 3 First date of measure for cycle 2",
		"",
		"", "", "", "", "", "", "", "", "")
	# That date is the first measurement on the follower, not flowering.
	d3$date <- d3$flowering_date
	d3$flowering_date <- NA_character_
	d3$flowering_days <- NA_real_
	dlong <- rbind(d1, d2, d3)
	measured <- !is.na(dlong$plant_height) | !is.na(dlong$bunch_weight_) | !is.na(dlong$date) | !is.na(dlong$pest_severity)
	dlong <- dlong[measured, ]
	dlong$date[is.na(dlong$date)] <- dlong$flowering_date[is.na(dlong$date)]

	carobiner::write_files(path, meta, d, long=dlong)
}
