# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Rows do not indicate whether a Malaise trap was placed in a maize or a wheat field, so crop cannot be assigned per row.
# The raw Data sheet contains 3,581 exact duplicate rows (deduplicated here) and 1,309 additional pairs sharing the same Site, Date, Plot, Distance, Replicate, and Taxonomical_group but with different counts (often one non-zero and one zero); those pairs are kept because the cause (e.g. morphospecies rows vs. data-entry artifact) cannot be resolved without the authors.
# Coordinates are not in the workbook; station coordinates for CIMMYT El Batan and CENEB were georeferenced from the station names.

## NEW
# flower_strip_distance_: distance of the Malaise trap from the flower strip (m).
# trap_days_: number of days the Malaise trap remained active for sampling (days).
# functional_group_: ecological functional group assigned to the captured arthropod taxon.

## MISSING
# crop, yield, yield_moisture, yield_part, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.

## NOTES
# Malaise-trap arthropod counts at five distances from flower strips in maize and wheat fields at CIMMYT El Batan (Estado de Mexico) and CENEB (Ciudad Obregon, Sonora), 2021-2024.
# Taxonomic family/group is stored in insect_species and the number of captured individuals (including 0) in insect_abundance. Distance 0 m is inside the flower strip.

## NOT PROCESSED
# Overview sheet: workbook metadata and column definitions only.


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
		response_vars = "insect_abundance",
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 90,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "Mexico Arthropod Biodiversity Dataset (2021-2024).xlsx"]
	r <- carobiner::read.excel(f, sheet="Data", na=c("", "NA"))

	# Georeferenced CIMMYT experiment stations (El Batan and CENEB)
	sites <- data.frame(
		site_key = c("El_Batan", "CENEB"),
		country = c("Mexico", "Mexico"),
		adm1 = c("México", "Sonora"),
		adm2 = c("Texcoco", "Cajeme"),
		location = c("El Batán", "Ciudad Obregón"),
		site = c("CIMMYT El Batán", "CENEB"),
		latitude = c(19.530, 27.369),
		longitude = c(-98.849, -109.931),
		elevation = c(2249, 38),
		geo_uncertainty = c(1500, 1500),
		geo_source = c("CIMMYT El Batan research station", "CIMMYT CENEB research station")
	)

	# Clean trailing spaces and non-breaking spaces in taxonomic and functional group names
	clean_txt <- function(x) {
		x <- gsub("\u00a0", " ", as.character(x), fixed=TRUE)
		x <- trimws(x)
		x[!nzchar(x)] <- NA_character_
		x
	}

	site_key <- clean_txt(r$Site)
	d <- data.frame(
		site_key = site_key,
		trial_id = paste(site_key, as.integer(r$Year), sep="_"),
		plot_id = as.character(as.integer(r$Plot)),
		rep = as.integer(r$Replicate),
		date = as.character(as.Date(r$Date)),
		flower_strip_distance_ = as.numeric(r$Distance),
		treatment = paste0(as.numeric(r$Distance), " m"),
		trap_days_ = as.numeric(r$Days),
		functional_group_ = clean_txt(r$Functional_group),
		insect_species = clean_txt(r$Taxonomical_group),
		insect_abundance = as.character(as.integer(r$Number)),
		stress = "pests",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	# Drop 3,581 exact duplicate rows present in the raw workbook
	d <- d[!duplicated(d), ]
	d <- merge(d, sites, by="site_key", all.x=TRUE, sort=FALSE)
	d$site_key <- NULL

	carobiner::write_files(path, meta, d)
}
