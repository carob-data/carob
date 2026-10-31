# R script for "carob"
# license: GPL (>=3)

## NOTES
# Malaise-trap counts in maize and wheat at El Batán (Estado de México) and CENEB (Ciudad Obregón, Sonora), 2021-2024. Geographic coverage is Mexico. The file has no coordinates.
# Each row is one taxon in one trap. A count of 0 is a recorded count. Distance 0 is the flower strip.
# The crop is not separable by row. Maize and wheat were both grown, so crop is not stored.
# Days is the number of days the trap was active.
# 3581 rows repeat the same site, date, plot, replicate, taxon, distance, count, and functional group. One copy of each is kept.

## ISSUES
# The rows do not say whether the trap was in maize or wheat, so crop is not stored. There is no yield, fertilizer, irrigation, planting date, or coordinates.
# write_files: missing variables: crop, yield, yield_moisture, yield_part, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: flower_strip_distance_, trap_days_, functional_group_, taxon_, insect_count_. contributor:

## NEW VARIABLES
# flower_strip_distance_: distance of the trap from the flower strip, in meters.
# trap_days_: days the trap was active.
# functional_group_: functional group assigned to the taxon.
# taxon_: taxonomic group.
# insect_count_: number of individuals of that taxon.

## NOT PROCESSED
# Overview sheet.


carob_script <- function(path) {

"
Replication Data for: Arthropods caught using malaise traps in maize and wheat in El Batan and CENEB

The database contains data on arthropods caught using Malaise traps in maize and wheat fields planted with flower strips across two agro-ecological regions of Mexico: El Batán in Estado de México, and Campo Experimental Norman E. Borlaug (CENEB) in Ciudad Obregón, Sonora. Over three years (2021-2024), arthropods were sampled regularly at five distances from the flower strips using Malaise traps, and were assigned to taxonomic and functional groups.
"

	uri <- "doi:10.71682/10549348"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "flower_strip_distance_",
		response_vars = "insect_count_",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 0.4
	)

	f <- ff[grepl("\\.xlsx$", basename(ff), ignore.case=TRUE)]
	r <- carobiner::read.excel(f, sheet="Data")
	num <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA", "-")] <- NA_character_
		bad <- !is.na(x) & grepl("[A-Za-z]", x)
		x[bad] <- NA_character_
		as.numeric(x)
	}
	site <- trimws(as.character(r$Site))
	loc <- site
	loc[site == "El_Batan"] <- "El Batán"
	loc[site == "CENEB"] <- "Ciudad Obregón"
	dist <- num(r$Distance)
	count <- num(r$Number)
	when <- as.character(as.Date(r$Date))
	d <- data.frame(
		country = "Mexico",
		location = loc,
		trial_id = paste(site, when, r$Plot, r$Replicate, r$Taxonomical_group, dist, sep="_"),
		plot_id = as.character(r$Plot),
		rep = as.integer(num(r$Replicate)),
		flower_strip_distance_ = dist,
		treatment = as.character(dist),
		trap_days_ = num(r$Days),
		date = when,
		functional_group_ = trimws(as.character(r$Functional_group)),
		taxon_ = trimws(as.character(r$Taxonomical_group)),
		insect_count_ = count,
		stress = "pests",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	d <- d[!is.na(d$flower_strip_distance_), ]
	d$functional_group_[d$functional_group_ %in% c("", "NA")] <- NA_character_
	d$taxon_[d$taxon_ %in% c("", "NA")] <- NA_character_
	# The workbook repeats 3581 trap records.
	d <- d[!duplicated(d), ]
	carobiner::write_files(path, meta, d)
}
