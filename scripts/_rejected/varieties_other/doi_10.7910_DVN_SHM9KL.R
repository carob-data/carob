# R script for "carob"
# license: GPL (>=3)

## ISSUES

# rejected:

# Aggregate ("replication") dataset that combines eight location x year trials,
# each of which is also available as a separate Dataverse dataset. 


## Column names
# and formats differ between files and are harmonized here. Grain yield for the
# Melkassa 2020 file is skipped because yield is reported as g/plant and cannot be converted to kg/ha



carob_script <- function(path) {

"
Replication Data for: Advanced drought tolerant sorghum hybrids evaluated at multiple locations in Ethiopia during 2018 to 2020

Data on agronomic traits of maturity, plant height, grain yield and plant aspect score collected for advanced drought tolerant sorghum hybrids evaluated at multiple locations (Kobo, Melkassa, Mieso and Shiraro) in Ethiopia during 2018 to 2020
"

	uri <- "doi:10.7910/DVN/SHM9KL"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "PURDUE",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;flowering_days;maturity_days;plant_height",
		carob_contributor = "Robert Hijmans",
		carob_date = "2026-07-16",
		carob_completion = 80,
		carob_effort = 3
	)

	ff <- ff[!grepl("Melkassa", basename(ff))]

	process <- function(f) {
		r <- carobiner::read.excel(f, fix_names=TRUE, na="NA")
		nm <- names(r)
		ph  <- if ("PHTMean" %in% nm) r$PHTMean else if ("PHT" %in% nm) r$PHT else NA
		yld <- if ("Yield.Kg.Ha" %in% nm) r$Yield.Kg.Ha else NA
		ped <- if ("Pedigree" %in% nm) r$Pedigree else NA
		rp  <- if ("Replicate" %in% nm) r$Replicate else if ("rep" %in% nm) r$rep else NA
		data.frame(
			year = as.character(r$Year),
			location = as.character(r$Site),
			plot_id = as.character(r$Plot),
			rep = as.integer(rp),
			treatment = as.character(r$Genotype),
			variety = as.character(r$Genotype),
			variety_pedigree = as.character(ped),
			flowering_days = as.numeric(r$DTF),
			maturity_days = as.numeric(r$DTM),
			plant_height = as.numeric(ph),
			yield = as.numeric(yld),
			crop = "sorghum"
		)
	}

	d <- do.call(rbind, lapply(ff, process))

	## harmonize site codes and years
	d$location[d$location %in% c("KB")] <- "Kobo"
	d$location[d$location %in% c("MS")] <- "Mieso"
	d$location[d$location %in% c("SH", "Sheraro")] <- "Shiraro"
	d$location[d$location %in% c("MK")] <- "Melkassa"
	d$year[nchar(d$year) == 2] <- paste0("20", d$year[nchar(d$year) == 2])

	d$variety_type <- "drought tolerant hybrid"
	d$planting_date <- d$year
	d$harvest_date <- d$year
	d$trial_id <- paste(d$location, d$year, sep="_")
	d$country <- "Ethiopia"
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA

	geo <- data.frame(
		location = c("Kobo", "Melkassa", "Mieso", "Shiraro"),
		longitude = c(39.643, 39.326, 40.5638, 37.773),
		latitude = c(12.1038, 8.417, 9.1779, 14.396),
		geo_from_source = FALSE
	)
	d <- merge(d, geo, by="location", all.x=TRUE)

	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)

	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA

	d$year <- NULL

	carobiner::write_files(path, meta, d)
}
