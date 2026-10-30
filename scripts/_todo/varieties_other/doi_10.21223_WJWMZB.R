# R script for "carob"
# license: GPL (>=3)

## NOTES
# Umbeluzi Research Station, Mozambique, planted 20-23 December 2018. Row-column design, two replications, plots of 25 m2. The Dataverse keyword is Sweetpotato.
# Five water and fertilizer treatments. NPK is mentioned with no rate, so no N, P or K column is filled.
# Coordinates are in Image_description_Moz1.xlsx and match the bounding box (longitude 32.358583, latitude -26.046829).
# Each workbook sheet is one flight date (19 March 2019 and 15 June 2019). The first row under the grouped header is Plot, Trial, Genotype, then mean and stdev pairs. Means are kept. Stdev is not.

## ISSUES
# No yield in the files. A single irrigated flag does not describe the timed drought treatments.
# Replication is described but not labeled on the rows. Genotypes repeat within a treatment.
# write_files: missing carob_contributor, yield, yield_moisture, yield_part, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, harvest_date. unknown variables: canopy_temperature_, ndvi_, ndvi_green_, ci_, cover_percent_.

## NOT PROCESSED
# Image_description_Moz1.xlsx: image file names, altitude, sensor and pixel size. Coordinates were taken from it.
# Standard-deviation columns on both dates.
# Soil analysis is mentioned in the description and is not in these files.

## NEW VARIABLES
# canopy_temperature_: mean canopy temperature (C).
# ndvi_: mean NDVI.
# ndvi_green_: mean green NDVI.
# ci_: the CI column. Unit is not stated.
# cover_percent_: canopy cover (%).


carob_script <- function(path) {

"
Dataset for: Remote sensing and agronomic data for drought trials evaluated at Umbeluzi Research Station in 2019 (1st experiment).

The trial had 24 clones. Check clones were 199062.1, Nhacoongo 1, Caromex, Xiadlaxakau, TIS2534, Irene and Xitsekele. The remaining 17 are parents from Gurue and Umbeluzi. The trials were planted at Umbeluzi from 20 to 23 December 2018. Each plot was 25m2. In this trial, there were 5 treatments namely, (i) optimum irrigation & fertilizer (ii) optimum irrigation (iii) early season drought (iv) mid-season drought & (v) late season drought. The treatments are defined as follows;  (i)	Irrigation + fertilizer – no water stress and fertilizer (NPK) applied at 35 days after planting in the treatment. (irrigation to water holding capacity of the soil)  (ii)	Irrigation – no water stress in the treatment (irrigation to water holding capacity of the soil)  (iii)	Early season drought – water withheld at 40 days after planting for 15 days. Drought intensity targetted is moderate to severe (40 to 70 % stress). Irrigation resumed at 50 to 56 days after planting.    (iv)	Mid-season drought – water withheld at 65 days after planting for 15 days. Drought intensity targetted is moderate to severe (40 to 70 % stress). Irrigation resumed at 80 to 86 days after planting.  (v)	Terminal drought - water withheld at 120 days after planting. No irrigation applied thereafter. Drought intensity targetted is moderate to severe (40 to 70 % stress). 

Experimental design: row column design (6 rows x 8 columns per each treatment), 2 replications per treatment. Soil analysis done for each field. The trial was mainly supported by irrigation.
"

	uri <- "doi:10.21223/WJWMZB"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "row-column",
		data_type = "experiment",
		treatment_vars = "variety;treatment",
		response_vars = "canopy_temperature_;ndvi_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.45
	)

	f <- ff[basename(ff) == "Umbeluzi_2019_Moz1.xlsx"]
	one <- function(sheet, date) {
		r <- carobiner::read.excel(f, sheet=sheet)
		r <- r[-1, ]
		data.frame(
			plot_id = trimws(as.character(r[[1]])),
			treatment = trimws(as.character(r[[2]])),
			variety = trimws(as.character(r[[3]])),
			date = date,
			canopy_temperature_ = as.numeric(r[[4]]),
			ndvi_ = as.numeric(r[[6]]),
			ndvi_green_ = as.numeric(r[[8]]),
			ci_ = as.numeric(r[[10]]),
			cover_percent_ = as.numeric(r[[12]])
		)
	}
	x <- rbind(
		one("190319", "2019-03-19"),
		one("150619", "2019-06-15")
	)
	x$record_id <- as.integer(as.factor(paste(x$treatment, x$plot_id, x$variety)))

	checks <- c("199062.1", "Nhacoongo 1", "Caromex", "Xiadlaxakau", "TIS2534", "Irene", "Xitsekele")
	d <- x[!duplicated(x$record_id), c("record_id", "plot_id", "treatment", "variety")]
	d$variety_type <- ifelse(d$variety %in% checks, "check", NA_character_)
	d$crop <- "sweetpotato"
	d$country <- "Mozambique"
	d$adm1 <- "Maputo"
	d$adm2 <- "Boane"
	d$location <- "Umbeluzi"
	d$longitude <- 32.358583
	d$latitude <- -26.046829
	d$geo_from_source <- TRUE
	d$planting_date <- "2018-12"
	d$trial_id <- "Umbeluzi_2019"
	d$on_farm <- FALSE
	d$is_survey <- FALSE

	dlong <- x[, c("record_id", "date", "canopy_temperature_", "ndvi_", "ndvi_green_", "ci_", "cover_percent_")]

	carobiner::write_files(path, meta, d, long=dlong)
}
