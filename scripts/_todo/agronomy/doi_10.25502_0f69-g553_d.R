# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm vegetable seedling trial, Murang'a (spelled Murange in the title), Kenya, season 1 2022.
# Three farmer groups: Githiga (nightshade, harvest in bunches), Jijenge (kale, kg), Njothi (amaranth, kg).
# Dictionary: HS = tray seedling, FP = ground seedling. Farmer names are blank; the row ID within each file is the replicate.
# Weekly harvests (week 1 and week 3 of Dec, Jan, Feb, Mar) are in the long table. Dec is taken as 2022 and Jan-Mar as 2023 because the columns run Dec then Jan-Mar in a trial titled season 1 2022.
# Bunch counts were not converted to kg. The dictionary line "1kg, ksh.35" was not used as a bunch weight or as a price.
# African nightshade (Solanum scabrum) mapped to terminag crop "managu".
# Georeferenced at Murang'a County centroid (-0.7210, 37.1526; geo_from_source = FALSE, geo_uncertainty = 15000).

## ISSUES
# No plot area, so kg and bunches cannot be turned into kg/ha yield.
# Exact village coordinates for Githiga, Jijenge and Njothi in Murang'a County are not in the file; Murang'a County centroid used.
# Calendar year of the January-March harvests is inferred (see NOTES).
# N/P/K and irrigation are not reported.
# HS_Total / FP_Total columns do not exist in this season. 
# write_files: missing yield, yield_moisture, N/P/K, irrigated, harvest_date; unknown seedling_type_, harvest_week_, harvest_kg_, harvest_bunches_.

## NOT PROCESSED
# Farmers_name: empty in every row.
# dictionary.csv: HS/FP definitions and the 35 KSh/kg note.

## NEW VARIABLES
# seedling_type_: tray seedling (HS) or ground seedling (FP).
# harvest_kg_: weekly harvest in kg where the column says Kg.
# harvest_bunches_: weekly harvest in bunches where the column says Bunches.
# harvest_week_: 1 or 3, the week within the month.


carob_script <- function(path) {

"
RCI veg seedling trial data.Murange, Kenya season 1 2022

WP1 of RCI to improve UPU crop productivity; assessment of tray vegetable seedling benefits
"

	uri <- "doi:10.25502/0f69-g553/d"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "RCI",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "seedling_type_",
		response_vars = "harvest_kg_;harvest_bunches_",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 55,
		carob_effort = 0.6,
		notes = NA
	)

	files <- data.frame(
		file = c("githiga-group-nightsahde.csv", "jijenge-group-kales.csv", "njothi-group-amaranth.csv"),
		location = c("Githiga", "Jijenge", "Njothi"),
		crop = c("managu", "kale", "amaranth")
	)
	month_date <- c(Dec="2022-12", Jan="2023-01", Feb="2023-02", Mar="2023-03")

	pieces <- list()
	longs <- list()
	k <- 0
	for (i in seq_len(nrow(files))) {
		f <- ff[basename(ff) == files$file[i]]
		r <- read.csv(f, check.names=FALSE)
		r <- r[, names(r) != ""]
		for (j in seq_len(nrow(r))) {
			k <- k + 1
			for (seed in c("HS", "FP")) {
				rid <- paste(files$location[i], r$ID[j], seed, sep="_")
				pieces[[length(pieces) + 1]] <- data.frame(
					record_id = rid,
					location = files$location[i],
					crop = files$crop[i],
					rep = as.integer(r$ID[j]),
					seedling_type_ = c(HS="tray seedling", FP="ground seedling")[seed],
					treatment = c(HS="tray seedling", FP="ground seedling")[seed]
				)
				cn <- grep(paste0("^", seed, "_Week"), names(r), value=TRUE)
				for (nm in cn) {
					parts <- strsplit(nm, "_")[[1]]
					week <- as.integer(sub("Week", "", parts[2]))
					mon <- parts[length(parts)]
					val <- r[[nm]][j]
					longs[[length(longs) + 1]] <- data.frame(
						record_id = rid,
						date = unname(month_date[mon]),
						harvest_week_ = week,
						harvest_kg_ = if (grepl("_Kg_", nm)) val else NA_real_,
						harvest_bunches_ = if (grepl("_Bunches_", nm)) val else NA_real_
					)
				}
			}
		}
	}

	d <- do.call(rbind, pieces)
	long <- do.call(rbind, longs)
	d <- d[!duplicated(d$record_id), ]
	ids <- unique(d$record_id)
	d$record_id <- match(d$record_id, ids)
	long$record_id <- match(long$record_id, ids)
	d$country <- "Kenya"
	d$adm1 <- "Murang'a"
	d$latitude <- -0.7210
	d$longitude <- 37.1526
	d$geo_from_source <- FALSE
	d$geo_uncertainty <- 15000
	d$trial_id <- paste(d$location, "2022", sep="_")
	d$planting_date <- "2022"
	d$yield_part <- "leaves"
	d$on_farm <- TRUE
	d$is_survey <- FALSE

	carobiner::write_files(path, meta, d, long=long)
}
