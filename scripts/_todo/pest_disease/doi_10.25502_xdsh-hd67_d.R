# R script for "carob"
# license: GPL (>=3)

## ISSUES
# - Genotype "I090521" appears twice at every location and sampling time (30 plants each) with two different fixed Hairiness scores (2 vs 4), indicating one of the two entries is likely a mislabeled genotype (12 genotypes total per site, 360 plants per site-time). Both entries are kept distinct via plot_id and leaf_hairiness_.
# - CGM_Sev (observed 2-4) and Hairiness (observed 2-4) scales are not defined in the data dictionary (likely standard IITA 1-5 scales).
# - At Bambui, Foumbot, and Meyomessala, two different MAP ages (3MAP with 9MAP or 6MAP) were both recorded in January 2017, indicating two planting cohorts at those sites; planting dates are not reported.
# - Meyomessala January 6MAP has only 200 rows (7 genotypes, with 20 plants for I071026) instead of 360.

## NEW
# aez_: Cameroon agroecological zone code (AEZII, AEZIII, AEZIV, AEZV)
# leaf_hairiness_: shoot/leaf pubescence score (values 2, 3, or 4; scale not specified)
# months_after_planting_: months after planting at assessment (3, 6, or 9)
# plant_number_: sequential plant number sampled per block (1-10)
# typhlodromalus_present_: logical presence/absence of the predatory mite Typhlodromalus aripo on the plant
# pest_species: Mononychellus tanajoa

## MISSING
# yield, yield_moisture, irrigated, N_fertilizer, P_fertilizer, K_fertilizer, planting_date, harvest_date

## NOTES
# Multi-location cassava trial across 7 sites in Cameroon (dry season 2016-2017; October 2016 and January 2017) evaluating cassava green mite (Mononychellus tanajoa) abundance/severity and presence of the predatory mite Typhlodromalus aripo across 12 genotype entries (3 blocks x 10 plants per entry).
# Wide table d has one row per trial cohort x genotype entry (distinguishing the two I090521 hairiness levels) x block; long table dd preserves all 4,520 individual plant-level observations linked by record_id without aggregation.
# Sites are georeferenced at town/station level (Wikipedia/GeoNames; ~5 km uncertainty); the single CKAN coordinate (3°51.791'N, 11°27.706'E, Yaoundé/IITA) is not applied across all 7 sites.

## NOT PROCESSED
# cgm-data.csv: ID (row number).
# cgm-data.csv: CGM_Incidence (exactly CGM_Abundance * 100 / 30 on every row, reaching 1820; apparent spreadsheet formula error copied from T_aripo_perc).
# cgm-data.csv: T_aripo_perc (redundant helper column = T_aripo * 100 / 30, i.e., 0 or 3.3333%).
# cgm-data.csv: Taripo_30plants (redundant summary = sum of T_aripo_perc across the 30 plants of each genotype entry, recorded only on row 1 of each 30-plant group).
# data-dictionary.csv: codebook only.


carob_script <- function(path) {

"
Distribution and impact of the predatory mite Typhlodromalus aripo on cassava green mite in diverse agroecological landscape in Central Africa.

Biological control of Mononychellus tanajoa.
"

	uri <- "doi:10.25502/xdsh-hd67/d"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = NA, minor = NA,
		data_organization = "IITA; ICARDA",
		publication = NA,
		project = "Agriculture Investment and Market Development Project (PIDMA)",
		design = "multilocation; 12 cassava genotype entries in 3 blocks; 10 plants sampled per block",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "pest_incidence;pest_severity",
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 90,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "cgm-data.csv"]
	r <- read.csv(f, check.names = FALSE)

	loc <- carobiner::fix_name(trimws(r$Loc), "title")
	aez <- trimws(r$AEZone)
	mon <- trimws(r$Month)
	# Year is "2016-2017"; October (6MAP) precedes January (9MAP) by 3 months
	obs_date <- ifelse(mon == "October", "2016-10", ifelse(mon == "January", "2017-01", NA_character_))
	map_num <- as.integer(sub("MAP", "", trimws(r$MAP)))
	# Distinguish planting cohorts at sites where two MAP ages were sampled in the same month (January)
	cohort <- ifelse(mon == "October" & map_num == 6L, "2016-04",
		ifelse(mon == "January" & map_num == 9L, "2016-04",
		ifelse(mon == "January" & map_num == 6L, "2016-07",
		ifelse(mon == "January" & map_num == 3L, "2016-10", "unknown"))))
	trial_id <- paste(loc, cohort, sep = "_")
	var <- trimws(r$Genotypes)
	hair <- as.integer(r$Hairiness)
	blk <- as.character(r$Block)
	# Include hairiness in plot_id to keep the two distinct I090521 entries (Hairiness 2 vs 4) separate
	plot_id <- paste(trial_id, var, paste0("H", hair), paste0("B", blk), sep = "_")

	# Wide table: one row per plot (trial cohort x genotype entry x block)
	d <- unique(data.frame(
		trial_id = trial_id,
		location = loc,
		aez_ = aez,
		block_id = blk,
		plot_id = plot_id,
		variety = var,
		treatment = var,
		leaf_hairiness_ = hair
	))
	d <- d[order(d$trial_id, d$variety, d$leaf_hairiness_, d$block_id), ]
	rownames(d) <- NULL
	d$record_id <- seq_len(nrow(d))

	# Town/station-level georeferences in Cameroon (Wikipedia/GeoNames; ~5 km uncertainty)
	geo <- data.frame(
		location = c("Bambui", "Bertoua", "Ekona", "Foumbot", "Mbalmayo", "Meiganga", "Meyomessala"),
		adm1 = c("Nord-Ouest", "Est", "Sud-Ouest", "Ouest", "Centre", "Adamaoua", "Sud"),
		latitude = c(6.0500, 4.5773, 4.2306, 5.5080, 3.5167, 6.5300, 2.7203),
		longitude = c(10.2333, 13.6846, 9.3368, 10.6325, 11.5000, 14.3700, 11.7068),
		geo_from_source = FALSE,
		geo_uncertainty = 5000,
		geo_source = "Wikipedia/GeoNames"
	)
	d <- merge(d, geo, by = "location", all.x = TRUE)
	d$country <- "Cameroon"
	d$season <- "dry"
	d$crop <- "cassava"
	d$yield_part <- "roots"
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$stress <- "pests"

	# Long table: all 4,520 plant-level observations linked to d by record_id
	rec_id <- d$record_id[match(plot_id, d$plot_id)]
	stopifnot(!any(is.na(rec_id)))
	dd <- data.frame(
		record_id = rec_id,
		date = obs_date,
		months_after_planting_ = map_num,
		plant_number_ = as.integer(r$Plant_number),
		pest_species = "Mononychellus tanajoa",
		pest_incidence = as.integer(r$CGM_Abundance),
		pest_severity = as.character(r$CGM_Sev),
		typhlodromalus_present_ = r$T_aripo == 1L
	)

	carobiner::write_files(path, meta, d, long = dd)
}
