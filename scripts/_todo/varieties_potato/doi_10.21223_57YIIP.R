# R script for "carob"
# license: GPL (>=3)

## NOTES
# Plot rows are processed from PTYL200507_MUG.xls (Mugerero) using the shared CIP HIDAP helper process_cip_lbvars().
# process_cip_lbvars() computes yield and yield_marketable (kg/ha from TTWP/MTWP and plot_size), yield_moisture (100 - AVDM), plant_density, tuber_density, seed_density, spacing, soil_pH, soil_SOM, and joins 02_Material_list.xlsx (variety names and pedigree).
# Daily weather is read from Weather_daily (temperature only).

## ISSUES
# The abstract says "Completely Randomized Block Design". The Installation sheet says "Randomized Complete Block Design (RCBD)".
# Weather_daily includes 2006 as well as 2005; rows are kept as recorded.
# No fertilizer or irrigation rates reported. The site is not labeled on-farm or station.
# Longitude is published as 29.3 (one decimal).

## NOT PROCESSED
# 01_data_yield_Mugurero_2005.xlsx: flat export of the same Fieldbook.
# Hobo_data, Weather_data, Weather_Original, Weather_monthly, Summary by clone, Charts, Ranks by Clone, TTYA/MTWP/MTYA summary sheets.
# Weather_daily n_sample and TSD.
# 03_Data_dictionary.xlsx: documentation or a codebook, not an observation table.

## NEW VARIABLES
# none

carob_script <- function(path) {

"
Dataset for: Evaluation of Potato Clones for Performance in Burundi, 2005

An evaluation of 24 potato clones was carried out in the cities of Mugerero , Burundi, in 2005. The study utilized a Completely Randomized Block Design to assess the clones' performance under local conditions.
"

	uri <- "doi:10.21223/57YIIP"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "Randomized complete block design",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;yield_marketable;yield_moisture",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 85,
		carob_effort = 0.4
	)

	proc <- carobiner::get_function("process_cip_lbvars", path, group)
	f <- ff[basename(ff) == "PTYL200507_MUG.xls"]
	d <- proc(f)
	d$treatment <- d$variety
	d$on_farm <- NA
	d$irrigated <- NA
	d$geo_uncertainty <- 12000
	d$geo_source <- "HIDAP Minimal sheet"

	r4 <- carobiner::read.excel(f, sheet="Weather_daily")
	wth <- data.frame(
		country = as.character(r4$Country),
		adm1 = as.character(r4$Department),
		location = as.character(r4$Locality),
		date = sprintf("%04d-%02d-%02d", as.integer(r4$Year), as.integer(r4$Month), as.integer(r4$Day)),
		tmin = as.numeric(r4$TMIN),
		tmax = as.numeric(r4$TMAX),
		temp = as.numeric(r4$TMEAN),
		latitude = d$latitude[1],
		longitude = d$longitude[1],
		geo_from_source = TRUE
	)
	wth <- unique(wth[!is.na(wth$date), ])

	carobiner::write_files(path, meta, d, wth=wth)
}
