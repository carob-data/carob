# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. The "Installation" sheet gives 3 replications and 20 plants per plot (one row of 20 plants,
#    0.3 x 0.8 m, 4.8 m2), but the fieldbook has 4 replications (REP 1-4) and 22 tubers planted
#    per plot (NTP). The plot size of 4.8 m2 is used, as in the TTYNA column of the fieldbook.
# 2. Plant stand was low in many plots: on average 16 of 22 plants emerged and 15 were harvested
#    (as few as 3). The cause is not reported. Yield is not adjusted for missing plants
#    (TTWP / plot size).
# 3. All dates in the "Crop_management" sheet are 2006-09-01 (after the trial); not used.
#    Planting and harvest dates are from the "Minimal" sheet (2005-07-21 to 2005-10-21).

## NOTES
# 1. Coordinates from the "Minimal" sheet, confirmed by the contributor: Mugerero -3.25, 29.3 (801 m).
# 2. AVDM (average dry matter, %) has a single value per clone (not per plot).
#    It is used to compute yield_moisture (100 - AVDM).
# 3. It is not stated whether the trial was on-station or on-farm, or whether it was
#    irrigated, so on_farm and irrigated are NA. No information on fertilizer use.
#    The "Soil_analysis" sheet is empty.
# 4. In the material list, the codes "CIP720097" (Uganda11) and "KP90116.89" (Rukuzi) include a
#    non-breaking space, so they are not matched by process_cip_lbvars; their names are set here.
#    KP90116.89 is not a CIP number.
# 5. Weather (daily air temperature, no rainfall), from the "Weather_daily" sheet: continuous
#    records from 2005-07-21 to 2005-10-19 (during the trial). The sheet also has records for
#    2006 (the 2006 trial, doi:10.21223/1G3G6P); these are not included. The hourly
#    "Weather_Original" sheet is not used.
# 6. Some parent codes in the material list are stored as numbers (e.g. 88.052) and are read
#    with floating point noise; they are rounded to 3 decimals.

carob_script <- function(path) {

"
Dataset for: Evaluation of Potato Clones for Performance in Burundi, 2005

An evaluation of 24 potato clones was carried out at Mugerero (Bubanza, 801 m), Burundi, in 2005, using a Randomized Complete Block Design with four replications (22 plants per plot, 4.8 m²).
"

	uri <- "doi:10.21223/57YIIP"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		# design as reported in the "Installation" sheet
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;yield_marketable",
		notes = NA,
		carob_contributor = "Vilma Hualla",
		carob_LLM = "Claude Opus 5.5",
		carob_date = "2026-11-26",
		carob_completion = 85,
		carob_effort = 1
	)

	## the CIP fieldbook has the standard sheets "Minimal", "Installation" and
	## "Fieldbook", so we use the shared function
	process <- carobiner::get_function("process_cip_lbvars", path, group)

	f <- ff[grep("^PTYL", basename(ff))]
	d <- process(f)

	## AVDM: average dry matter (%) per clone, repeated for each plot (NOTES 2)
	fb <- carobiner::read.excel(f, sheet="Fieldbook", na=c("", "#N/A"))
	d$yield_moisture <- 100 - as.numeric(fb$AVDM)

	## names not matched because of non-breaking spaces in the material list (NOTES 4)
	d$variety[d$variety == "CIP720097"] <- "Uganda11"
	d$variety[d$variety == "KP90116.89"] <- "Rukuzi"
	d$variety[d$variety == "REICHE"] <- "Reiche"

	## parent codes stored as numbers in Excel are read with floating point
	## noise (e.g. "88.052000000000007" for 88.052; NOTES 6)
	fixnum <- function(x) {
		i <- which(grepl("^[0-9]+\\.[0-9]{8,}$", x))
		x[i] <- as.character(round(as.numeric(x[i]), 3))
		x
	}
	d$female_parent <- fixnum(d$female_parent)
	d$male_parent <- fixnum(d$male_parent)
	i <- !is.na(d$female_parent) & !is.na(d$male_parent)
	d$variety_pedigree[i] <- paste(d$female_parent[i], d$male_parent[i], sep=" x ")

	## not stated in the data (NOTES 3)
	d$on_farm <- NA
	d$irrigated <- NA

	## Weather (NOTES 5): daily temperature during the 2005 trial
	w <- carobiner::read.excel(f, sheet="Weather_daily")
	w <- w[w$Year == 2005, ]
	wth <- data.frame(
		country = d$country[1],
		adm1 = d$adm1[1],
		location = d$location[1],
		latitude = d$latitude[1],
		longitude = d$longitude[1],
		date = sprintf("2005-%02d-%02d", as.integer(w$Month), as.integer(w$Day)),
		tmin = as.numeric(w$TMIN),
		tmax = as.numeric(w$TMAX),
		temp = as.numeric(w$TMEAN)
	)

	carobiner::write_files(path, meta, d, wth=wth)
}
