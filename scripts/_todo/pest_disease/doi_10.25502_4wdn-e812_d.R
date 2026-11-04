# R script for "carob"
# license: GPL (>=3)

## NOTES
# The raw download in this workspace had only metadata.csv. The data file is
#   "Spatio-temporal partitioning dataset140720201.csv" (public on data.iita.org).
#   get_data should retrieve it (ok.txt was absent). 522 rows, seasons 1 and 2, blocks 1-4,
#   maize growth stages from Vegetative I through senescence. No treatment column.
# Season 1 and 2 are stored as first and second. That reading of the codes is not confirmed.
# d is one row per sample. Species counts stay on that row (zeros kept). A long table was
#   not used: the species are not a time series, and the file has no date per sample.
# Dictionary spelling "Cryptophlebus leucotreta" is stored as Cryptophlebia leucotreta
#   (the name in use when the file was made; now Thaumatotibia leucotreta).
# Coordinates are the dataset coverage point (coverage_x 3.87427, coverage_y 11.462),
#   the same lat/lon order as other IITA CKAN records in this batch. The site is not named.
# Study window in the metadata is 2017-03-30 to 2017-11-30. That is not a planting date.
# Plant-part text is lower-cased. Co-infestation "None"/"none" stored as none.

## ISSUES
# 1. No place name. The coverage point is in the Yaoundé area; confirm the field before naming it.
# 2. No yield, fertilizer, irrigation or planting/harvest dates.
# 3. Confirm Total_SB is the sum of the five stemborer species. It is kept as stemborer_total_.
# 4. Dead_SB and Pupae are not assigned to a species.
# 5. Design is not stated (blocks exist, no applied treatment). Treated here as a survey.
# 6. A few growth-stage cells are blank.
# 7. write_files: missing yield, yield_part, yield_moisture, N/P/K, planting_date, harvest_date, irrigated.
#    Spodoptera frugiperda is not in the pest vocabulary. Species counts use provisional names.

## NOT PROCESSED
# FAW_P_A, SBP_A, BfP_A, ES_P_A, Mn_P_A, CL_P_A, SC_P_A: presence/absence flags that repeat the counts.
# ID: row number.
# metadata.csv: column definitions only.

## NEW VARIABLES
# busseola_fusca_: Busseola fusca larvae
# eldana_saccharina_: Eldana saccharina larvae
# mussidia_nigrivenella_: Mussidia nigrivenella larvae
# cryptophlebia_leucotreta_: Cryptophlebia leucotreta larvae (dictionary: Cryptophlebus)
# sesamia_calamistis_: Sesamia calamistis larvae
# stemborer_total_: Total_SB
# dead_stemborer_: Dead_SB
# lepidoptera_pupae_: Pupae (dictionary: lepidoptera pupa)
# plant_part_faw_: plant part where FAW was recorded
# plant_part_sb_: plant part where stemborers were recorded
# coinfestation_: FAW, SB, both, or none

carob_script <- function(path) {

"
Spatiotemporal infestation of maize by FAW and stemborer in Cameroon

Biological invasion is a global concern with species deployed out of native range, the most recent in Africa being the invasion by Fall armyworm (FAW) Spodoptera frugiperda J.E. Smith (Lepidoptera: Noctuidae). One of the most mentioned ecological consequences is the capacity of invaders to change native communities’ structure. Busseola fusca Fuller (Lepidoptera: Noctuidae) has long been the most destructive pests of maize in Africa. Few or no studies have attempted to look at the interaction between FAW and native maize pests over a cropping cycle for sustainable management.
"

	uri <- "doi:10.25502/4wdn-e812/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = NA, minor = NA,
		data_organization = "IITA",
		publication = NA,
		project = "Spatio-temporal partitioning and parasitoids sharing by Fall armyworm and indigenous maize insect borers in Cameroon",
		design = "Maize plants scored for FAW and stemborers by growth stage; blocks 1-4; two seasons; no applied treatment in the file",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "pest_incidence",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 64,
		carob_effort = 0.6
	)

	f <- ff[grepl("\\.csv$", ff, ignore.case = TRUE)]
	f <- f[!grepl("metadata", basename(f), ignore.case = TRUE)]
	r <- read.csv(f, check.names = FALSE)

	part <- function(x) {
		x <- tolower(trimws(x))
		x[!is.na(x) & x == ""] <- NA_character_
		x
	}
	co <- tolower(trimws(r$Co_Infest))
	co[!is.na(co) & co == ""] <- NA_character_
	gs <- trimws(r$Growth_stage)
	gs[!is.na(gs) & gs == ""] <- NA_character_
	season <- ifelse(r$Season == 1, "first", ifelse(r$Season == 2, "second", NA_character_))

	d <- data.frame(
		country = "Cameroon",
		latitude = 3.87427,
		longitude = 11.462,
		geo_from_source = TRUE,
		season = season,
		growth_stage = gs,
		block_id = as.character(r$Block),
		crop = "maize",
		stress = "pests",
		pest_species = "Spodoptera frugiperda",
		pest_incidence = as.integer(r$FAW_Larvae),
		busseola_fusca_ = as.integer(r$Bf_Larvae),
		eldana_saccharina_ = as.integer(r$Es_Larvae),
		mussidia_nigrivenella_ = as.integer(r$M_n_Larvae),
		cryptophlebia_leucotreta_ = as.integer(r$Cryp_leuco_Larvae),
		sesamia_calamistis_ = as.integer(r$Sc_Larvae),
		stemborer_total_ = r$Total_SB,
		dead_stemborer_ = r$Dead_SB,
		lepidoptera_pupae_ = r$Pupae,
		plant_part_faw_ = part(r$Plant_partFAW),
		plant_part_sb_ = part(r$Plant_partSB),
		coinfestation_ = co,
		trial_id = season,
		on_farm = NA,
		is_survey = TRUE
	)
	# identical scores on different plants would otherwise look like duplicate rows
	d$record_id <- seq_len(nrow(d))

	carobiner::write_files(path, meta, d)
}
