# R script for "carob"
# license: GPL (>=3)

## NOTES
# Cane yield in yield.csv is t/ha, multiplied by 1000. yield_part is stems. Moisture is not stated.
# crop_cycle such as 14_15 is the readme's plantation-or-ratoon year and harvest year, stored as 2014 and 2015.
# Country is geographicCoverage Réunion. Localities are the locality column.
# Treatment codes are kept as recorded. The dataset description names CcWp as chemical weeding of the inter-row. The readme does not expand CeE, CeL or CcRo.
# Borer counts and block ground-cover scores are in the long table. Cover scores have no plot id (several plots share a block).
# Publication id in the Dataverse record is 10.64898/2026.01.26.701649v1.

## ISSUES
# No coordinates, fertilizer rates, or irrigation flag. Treatment codes CeE, CeL and CcRo are not defined in the readme. CcWp is chemical weeding of the inter-row.
# The species-level cover file and the coverage-dynamics file are not mapped.
# Cane yield in t/ha exceeds 150 t/ha on some plots.
# write_files: missing yield_moisture, geo_from_source, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer; unknown variables TFI_, block_, canavalia_cover_, borer_plot_, stalk_id_, internode_number_, bored_internodes_, stalk_damaged_; out of bounds yield (90833.333, 184000).

## NEW VARIABLES
# block_: block number on the cover-score rows
# borer_plot_: plot code on the stalk-borer rows
# stalk_id_: stalk counted for borer damage
# bored_internodes_: internodes bored by Chilo sacchariphagus on one stalk (count)
# internode_number_: internodes on that stalk (count)
# stalk_damaged_: 1 if the stalk had at least one bored internode, else 0
# TFI_: treatment frequency index (herbicide), as recorded
# canavalia_cover_: ground cover by Canavalia ensiformis in the inter-row (%)

## NOT PROCESSED
# ground cover by growth type and species.csv: weed species cover and presence (EPPO codes)
# ground coverage dynamics data.csv: the same cover scores against sugarcane age

carob_script <- function(path) {

"
Jack bean (Canavalia ensiformis) intercrop to reduce weeds and stalk borer damage in sugarcane

The aim of this study was to assess the efficiency of two intercrop species, <i>Canavalia ensiformis</i> and <i>Desmodium intortum</i>, to reduce weed growth, herbicide use and damage by the stalk borer <i>Chilo sacchariphagus</i> in sugarcane cropping system in Reunion. 
We compared six inter-row management techniques: four treatments combined the two intercrops <i>Canavalia ensiformis</i> or <i>Desmodium intortum</i> sown either early (between 0 and 2.1 months after sugarcane emergence) or late (between 1.3 and 3.7 months after sugarcane emergence), a treatment where no weeding was carried out on the inter-row, and a treatment with chemical weeding of the inter-row (CcWp). In all treatments, the sugarcane row was weeded chemically and manually. The six treatments were compared in a multilocal randomized block design with tree localities, during one or two crop cycles depending on the locality.
"

	uri <- "doi:10.18167/DVN1/NVW8ZV"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = "doi:10.64898/2026.01.26.701649",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield;weed_cover",
		carob_LLM = "Cursor Grok 4.7",
		carob_date = "2026-10-31",
		carob_completion = 50,
		carob_effort = 0.6
	)

	f1 <- ff[basename(ff) == "yield.csv"]
	r1 <- read.csv(f1, sep=";", check.names=FALSE)
	cyc <- strsplit(r1$crop_cycle, "_")
	start_y <- paste0("20", sapply(cyc, function(z) z[1]))
	end_y <- paste0("20", sapply(cyc, function(z) z[2]))

	d <- data.frame(
		country = "Réunion",
		location = r1$locality,
		crop = "sugarcane",
		treatment = r1$treatment,
		plot_id = as.character(r1$plot),
		rep = as.integer(r1$bloc),
		trial_id = paste(r1$locality, r1$crop_cycle, sep="_"),
		planting_date = start_y,
		harvest_date = end_y,
		yield = as.numeric(r1$yield) * 1000,
		yield_part = "stems",
		borer_trial = TRUE,
		on_farm = FALSE,
		is_survey = FALSE
	)
	# country from geographicCoverage. planting_date and harvest_date are the two years in crop_cycle (readme).

	f2 <- ff[basename(ff) == "TFI and manual weeding.csv"]
	r2 <- read.csv(f2, sep=";", check.names=FALSE)
	r2$trial_id <- paste(r2$locality, r2$crop_cycle, sep="_")
	r2 <- data.frame(
		trial_id = r2$trial_id,
		treatment = r2$treatment,
		TFI_ = as.numeric(r2$TFI),
		weeding_times = as.integer(r2$manual_weeding)
	)
	d <- merge(d, r2, by=c("trial_id", "treatment"), all.x=TRUE)
	d$weeding_done <- d$weeding_times > 0
	d$record_id <- as.integer(seq_len(nrow(d)))

	f3 <- ff[basename(ff) == "global ground cover.csv"]
	r3 <- read.csv(f3, check.names=FALSE)
	r3$trial_id <- paste(r3$locality, r3$crop_cycle, sep="_")
	# notation_date is d/m/yyyy (day 24 cannot be a month)
	gdate <- as.character(as.Date(r3$notation_date, "%d/%m/%Y"))
	long_g <- data.frame(
		trial_id = r3$trial_id,
		treatment = r3$treatment,
		rep = as.integer(r3$bloc),
		date = gdate,
		ground_cover = as.numeric(r3$global_score_WP),
		weed_cover = as.numeric(r3$weed_score_WP),
		canavalia_cover_ = as.numeric(r3$canavalia_score_IR)
	)

	f4 <- ff[basename(ff) == "borer damage.csv"]
	r4 <- read.csv(f4, sep=";", check.names=FALSE)
	r4$trial_id <- paste(r4$locality, r4$crop_cycle, sep="_")
	long_b <- data.frame(
		trial_id = r4$trial_id,
		treatment = r4$treatment,
		plot_id = as.character(r4$plot),
		rep = as.integer(r4$bloc),
		stalk_id_ = as.character(r4$stalk),
		internode_number_ = as.numeric(r4$ENT),
		bored_internodes_ = as.numeric(r4$ENA),
		stalk_damaged_ = as.numeric(r4$ta)
	)

	# Cover scores are by block, and several plots share a block, so they are not given a plot_id.
	long <- rbind(
		data.frame(
			trial_id = long_g$trial_id,
			date = long_g$date,
			block_ = long_g$rep,
			ground_cover = long_g$ground_cover,
			weed_cover = long_g$weed_cover,
			canavalia_cover_ = long_g$canavalia_cover_,
			borer_plot_ = NA_character_,
			stalk_id_ = NA_character_,
			internode_number_ = NA_real_,
			bored_internodes_ = NA_real_,
			stalk_damaged_ = NA_real_
		),
		data.frame(
			trial_id = long_b$trial_id,
			date = NA_character_,
			block_ = long_b$rep,
			ground_cover = NA_real_,
			weed_cover = NA_real_,
			canavalia_cover_ = NA_real_,
			borer_plot_ = long_b$plot_id,
			stalk_id_ = long_b$stalk_id_,
			internode_number_ = long_b$internode_number_,
			bored_internodes_ = long_b$bored_internodes_,
			stalk_damaged_ = long_b$stalk_damaged_
		)
	)

	carobiner::write_files(path, meta, d, long=long)
}
