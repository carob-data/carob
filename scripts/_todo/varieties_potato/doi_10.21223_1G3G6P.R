# R script for "carob"
# license: GPL (>=3)

## NOTES
# Plot rows are processed from PTYL200606_MPA.xls (Mparambo) and PTYL200607_MUG.xls (Mugerero) using the shared CIP HIDAP helper process_cip_lbvars().
# process_cip_lbvars() computes yield and yield_marketable (kg/ha from TTWP/MTWP and plot_size), yield_moisture (100 - AVDM), plant_density, tuber_density, seed_density, spacing, soil_pH, soil_SOM, and joins 03_Material_list.xlsx (variety names and pedigree).
# Daily weather is read from Weather_daily (TMIN, TMAX, TMEAN; no rain column).

## ISSUES
# The abstract says "Completely Randomized Block Design". The Installation sheet says "Randomized Complete Block Design (RCBD)".
# Mugerero Weather_daily includes 2005 as well as 2006; rows are kept as recorded.
# No fertilizer or irrigation rates in Crop_management. Sites are not labeled on-farm or station.
# Mugerero longitude is published as 29.3 (one decimal).

## NOT PROCESSED
# 01_data_yield_Mparambo_2006.xlsx and 02_data_yield_Mugerero_2006.xlsx: flat exports of the same Fieldbook rows.
# Hobo_data, Weather_data, Weather_Original, Weather_monthly, Summary by clone, Charts, Ranks by Clone: summaries or raw logger dumps.
# Weather_daily n_sample and TSD: sample size and temperature standard deviation.
# 04_Data_dictionary.xlsx: documentation or a codebook, not an observation table.

## NEW VARIABLES
# none

carob_script <- function(path) {

"
Dataset for: Evaluation of Potato Clones for Performance in Burundi, 2006

An evaluation of 11 potato clones was carried out in the cities of Mugerero and Mparambo, Burundi, in 2006. The study utilized a Completely Randomized Block Design to assess the clones' performance under local conditions.
"

	uri <- "doi:10.21223/1G3G6P"
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
		carob_effort = 0.5
	)

	proc <- carobiner::get_function("process_cip_lbvars", path, group)

	one_site <- function(f) {
		d <- proc(f)
		d$treatment <- d$variety
		d$on_farm <- NA
		d$irrigated <- NA
		d$geo_uncertainty <- ifelse(d$location == "Mugerero", 12000, 20)
		d$geo_source <- "HIDAP Minimal sheet"

		r3 <- carobiner::read.excel(f, sheet="Weather_daily")
		wth <- data.frame(
			country = as.character(r3$Country),
			adm1 = as.character(r3$Department),
			location = as.character(r3$Locality),
			date = sprintf("%04d-%02d-%02d", as.integer(r3$Year), as.integer(r3$Month), as.integer(r3$Day)),
			tmin = as.numeric(r3$TMIN),
			tmax = as.numeric(r3$TMAX),
			temp = as.numeric(r3$TMEAN),
			latitude = d$latitude[1],
			longitude = d$longitude[1],
			geo_from_source = TRUE
		)
		list(d=d, wth=wth)
	}

	f1 <- ff[basename(ff) == "PTYL200606_MPA.xls"]
	f2 <- ff[basename(ff) == "PTYL200607_MUG.xls"]
	a <- one_site(f1)
	b <- one_site(f2)
	d <- carobiner::bindr(a$d, b$d)
	wth <- carobiner::bindr(a$wth, b$wth)
	wth <- unique(wth[!is.na(wth$date), ])

	carobiner::write_files(path, meta, d, wth=wth)
}
