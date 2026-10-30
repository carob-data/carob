# R script for "carob"
# license: GPL (>=3)

## NOTES
# Same seedling comparison as the 2022 Murang'a trial, season 2 2023.
# Githiga kale is in bunches. Jijenge amaranth and Njothi spinach are in kg.
# HS = tray seedling, FP = ground seedling (dictionary). Farmer names are blank; row ID is the replicate.
# Weekly harvests are July, August and September 2023 (week 1 and week 3). The file also has an HS/FP total, which was not mapped because it is the sum of those weeks (not checked plot by plot).
# Bunch counts were not converted to kilograms. "1kg = ksh.35" in the dictionary was not applied.

## ISSUES
# No plot area, so these harvests are not kg/ha.
# No coordinates. Georeference Githiga, Jijenge and Njothi, Murang'a, Kenya.
# N/P/K and irrigation are not reported.
# write_files: missing yield, longitude, latitude, N/P/K, irrigated, harvest_date.

## NOT PROCESSED
# Farmers_name: empty.
# HS_Total_Yield_in_Bunches, FP_Total_Yield_in_Bunches, HS_Total_Yield_in_Kg, FP_Total_Yield_in_Kg: period totals of the weekly columns.
# dictionary.csv.

## NEW VARIABLES
# seedling_type_: tray seedling or ground seedling.
# harvest_kg_, harvest_bunches_, harvest_week_: see the 2022 script. Same meaning.


carob_script <- function(path) {

"
RCI veg seedling trial data.Murange, Kenya season 2 2023

WP1 of RCI to improve UPU crop productivity; assessment of tray vegetable seedling benefits
"

	uri <- "doi:10.25502/1qa0-sc25/d"
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
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.5,
		notes = NA
	)

	files <- data.frame(
		file = c("githiga-group-kales.csv", "jijenge-group-amaranth.csv", "njothi-group-spinach.csv"),
		location = c("Githiga", "Jijenge", "Njothi"),
		crop = c("kale", "amaranth", "spinach"),
		stringsAsFactors = FALSE
	)
	month_date <- c(Jul="2023-07", July="2023-07", Aug="2023-08", Sep="2023-09")

	pieces <- list()
	longs <- list()
	for (i in seq_len(nrow(files))) {
		r <- read.csv(ff[basename(ff) == files$file[i]], stringsAsFactors=FALSE, check.names=FALSE)
		r <- r[, names(r) != ""]
		for (j in seq_len(nrow(r))) {
			for (seed in c("HS", "FP")) {
				rid <- paste(files$location[i], r$ID[j], seed, sep="_")
				pieces[[length(pieces) + 1]] <- data.frame(
					record_id = rid,
					location = files$location[i],
					crop = files$crop[i],
					rep = as.integer(r$ID[j]),
					seedling_type_ = c(HS="tray seedling", FP="ground seedling")[seed],
					treatment = c(HS="tray seedling", FP="ground seedling")[seed],
					stringsAsFactors = FALSE
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
						harvest_bunches_ = if (grepl("Bunches", nm)) val else NA_real_,
						stringsAsFactors = FALSE
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
	d$trial_id <- paste(d$location, "2023", sep="_")
	d$planting_date <- "2023"
	d$yield_part <- "leaves"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$geo_from_source <- FALSE

	carobiner::write_files(path, meta, d, long=long)
}
