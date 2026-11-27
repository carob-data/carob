# R script for "carob"
# license: GPL (>=3)

## ISSUES
# No site name is given in the raw files; CKAN coverage coordinates (3.87427, 11.462) fall near IITA-Cameroon (Nkolbisson, Yaoundé). Confirm the site name, maize variety, planting dates, and whether on_farm is FALSE from Abang et al. (2021, doi:10.1111/jen.12827).
# Dead_SB and Pupae are not assigned to a specific stemborer species in the raw file, so they remain as plant-level columns on d.

## NEW
# dead_stemborer_: count of dead stemborer larvae per maize plant (unassigned to species).
# lepidoptera_pupae_: count of lepidopteran pupae per maize plant (unassigned to species).
# coinfestation_: co-infestation category on the plant (faw, sb, both, or none).
# plant_part_: maize plant part where the pest was found (leaf funnel, closed tassel, ear, or stem) in the long table.
# pest_species values not in terminag: Sesamia calamistis, Thaumatotibia leucotreta (listed in dictionary as Cryptophlebus leucotreta).

## MISSING
# hhid (field pest-monitoring survey without household units).

## NOTES
# Two-season (2017) maize pest-partitioning field study in Cameroon (4 blocks, Vegetative I through Senescence; Abang et al. 2021, doi:10.1111/jen.12827).
# Two embedded Excel subtotal rows (Season = NA, rows 281 and 522) are removed, leaving 520 plant-level observations.
# All six lepidopteran pest species counts (Spodoptera frugiperda, Busseola fusca, Eldana saccharina, Mussidia nigrivenella, Thaumatotibia leucotreta, Sesamia calamistis) are stored in long table dd alongside plant_part_ instead of wide species columns.

## NOT PROCESSED
# Total_SB: verified equal to the sum of the 5 stemborer species + Dead_SB + Pupae across all 520 rows (redundant).
# FAW_P_A, SBP_A, BfP_A, ES_P_A, Mn_P_A, CL_P_A, SC_P_A: binary presence/absence flags redundant with the counts.
# metadata.csv: column dictionary only.


carob_script <- function(path) {

"
Spatiotemporal infestation of maize by FAW and stemborer in Cameroon

Biological invasion is a global concern with species deployed out of native range, the most recent in Africa being the invasion by Fall armyworm (FAW) Spodoptera frugiperda J.E. Smith (Lepidoptera: Noctuidae). One of the most mentioned ecological consequences is the capacity of invaders to change native communities' structure. Busseola fusca Fuller (Lepidoptera: Noctuidae) has long been the most destructive pests of maize in Africa. Few or no studies have attempted to look at the interaction between FAW and native maize pests over a cropping cycle for sustainable management.
"

	uri <- "doi:10.25502/4wdn-e812/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA; Drexel",
		publication = "doi:10.1111/jen.12827",
		project = "Spatio-temporal partitioning and parasitoids sharing by Fall armyworm and indigenous maize insect borers in Cameroon",
		design = "Maize plants scored across growth stages for FAW and stemborers in 4 blocks over 2 cropping seasons (2017); no applied treatment in dataset",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "pest_incidence",
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 90,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "spatio-temporal-partitioning-dataset140720201.csv"]
	r <- read.csv(f, check.names=FALSE, fileEncoding="latin1")
	# Remove 2 embedded Excel subtotal rows (Season = NA, rows 281 and 522)
	r <- r[!is.na(r$Season), , drop=FALSE]

	clean_part <- function(x) {
		x <- tolower(trimws(as.character(x)))
		x[!is.na(x) & x == ""] <- NA_character_
		x
	}
	season <- ifelse(r$Season == 1, "first", ifelse(r$Season == 2, "second", NA_character_))
	co <- tolower(trimws(as.character(r$Co_Infest)))
	co[!is.na(co) & co == ""] <- NA_character_

	d <- data.frame(
		record_id = seq_len(nrow(r)),
		country = "Cameroon",
		# Coordinates from CKAN coverage_x/coverage_y (Yaounde / Nkolbisson area)
		latitude = 3.87427,
		longitude = 11.462,
		geo_from_source = TRUE,
		trial_id = paste0("2017_", season),
		season = season,
		growth_stage = trimws(r$Growth_stage),
		block_id = as.character(r$Block),
		crop = "maize",
		stress = "pests",
		dead_stemborer_ = as.integer(r$Dead_SB),
		lepidoptera_pupae_ = as.integer(r$Pupae),
		coinfestation_ = co,
		on_farm = NA,
		is_survey = TRUE
	)

	part_faw <- clean_part(r$Plant_partFAW)
	part_sb <- clean_part(r$Plant_partSB)

	make_pest <- function(species, count, part) {
		data.frame(
			record_id = d$record_id,
			pest_species = species,
			pest_incidence = as.integer(count),
			plant_part_ = ifelse(count > 0, part, NA_character_)
		)
	}

	dd <- rbind(
		make_pest("Spodoptera frugiperda", r$FAW_Larvae, part_faw),
		make_pest("Busseola fusca", r$Bf_Larvae, part_sb),
		make_pest("Eldana saccharina", r$Es_Larvae, part_sb),
		make_pest("Mussidia nigrivenella", r$M_n_Larvae, part_sb),
		# Dictionary spells "Cryptophlebus leucotreta"; current valid name is Thaumatotibia leucotreta
		make_pest("Thaumatotibia leucotreta", r$Cryp_leuco_Larvae, part_sb),
		make_pest("Sesamia calamistis", r$Sc_Larvae, part_sb)
	)

	carobiner::write_files(path, meta, d, long=dd)
}
