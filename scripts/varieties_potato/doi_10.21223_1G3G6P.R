# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Mparambo: low emergence (on average 16 of 30 plants; as few as 5), with the number
#    of plants harvested close to the number emerged. Mugerero: on average 21 plants
#    emerged and 18 harvested. The cause of the losses is not reported. Yield is not
#    adjusted for missing plants (TTWP / plot size).
# 2. In Mugerero, 10 plots have more plants harvested (NPH) than emerged (NPE).
# 3. Mparambo dates differ between sheets: "Minimal" gives 2006-06-29 to 2006-10-11, while
#    "Crop_management" gives planting 2006-07-11 and harvest 2006-10-17, identical to the
#    Mugerero file (probably copied). The dates from "Minimal" are used.
#    "Vine cutting / killing" is dated on the planting day in both files; not used.

## NOTES
# 1. Coordinates from the "Minimal" sheets, confirmed by the contributor:
#    Mparambo -2.8333, 29.0667 (892 m); Mugerero -3.25, 29.3 (801 m).
# 2. AVDM (average dry matter, %) has a single value per clone and site (not per plot).
#    It is used to compute yield_moisture (100 - AVDM). In Mparambo, AVDM is "#N/A" for
#    CIP395192.1.
# 3. It is not stated whether the trials were on-station or on-farm, or whether they were
#    irrigated, so on_farm and irrigated are NA. No information on fertilizer use.
#    The "Soil_analysis" sheets are empty.
# 4. Weather (daily air temperature, no rainfall), from the "Weather_daily" sheets.
#    For 2006, day and month are swapped when the day is 12 or less (e.g. "1/8/2006" is
#    2006-08-01): after swapping, the records are continuous from 2006-07-10 to 2006-10-11
#    at both sites, with no duplicates. The Mugerero sheet also has records for
#    2005-07-21 to 2005-10-19 (the 2005 trial, doi:10.21223/57YIIP); these are not included.
#    The hourly "Weather_Original" sheets have the same date problems and also cover other
#    periods (Mparambo up to 2007-12); not used.

carob_script <- function(path) {

"
Dataset for: Evaluation of Potato Clones for Performance in Burundi, 2006

An evaluation of 11 potato clones was carried out at Mparambo (Cibitoke, 892 m) and Mugerero (Bubanza, 801 m), Burundi, in 2006, using a Randomized Complete Block Design with four replications (30 plants per plot, 7.2 m²).
"

	uri <- "doi:10.21223/1G3G6P"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		# design as reported in the "Installation" sheet of each fieldbook
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

	## the two CIP fieldbooks (one per site) have the standard sheets
	## "Minimal", "Installation" and "Fieldbook", so we use the shared function
	process <- carobiner::get_function("process_cip_lbvars", path, group)

	f <- ff[grep("^PTYL", basename(ff))]
	d <- lapply(f, function(x) {
		out <- process(x)
		## AVDM: average dry matter (%) per clone and site, repeated for each plot (NOTES 2)
		fb <- carobiner::read.excel(x, sheet="Fieldbook", na=c("", "#N/A"))
		out$yield_moisture <- 100 - as.numeric(fb$AVDM)
		out
	})
	d <- do.call(rbind, d)

	d$variety[d$variety == "REICHE"] <- "Reiche"

	## not stated in the data (NOTES 3)
	d$on_farm <- NA
	d$irrigated <- NA

	## Weather (NOTES 4): daily temperature. In 2006, day and month are swapped
	## when the day is 12 or less
	get_wth <- function(x) {
		w <- carobiner::read.excel(x, sheet="Weather_daily")
		w <- w[w$Year == 2006, ]
		mm <- as.integer(w$Month)
		dd <- as.integer(w$Day)
		i <- dd <= 12
		tmp <- mm[i]
		mm[i] <- dd[i]
		dd[i] <- tmp
		s <- d[d$trial_id == gsub(".xls$", "", basename(x)), ][1, ]
		data.frame(
			country = s$country,
			adm1 = s$adm1,
			location = s$location,
			latitude = s$latitude,
			longitude = s$longitude,
			date = sprintf("2006-%02d-%02d", mm, dd),
			tmin = as.numeric(w$TMIN),
			tmax = as.numeric(w$TMAX),
			temp = as.numeric(w$TMEAN)
		)
	}
	wth <- do.call(rbind, lapply(f, get_wth))
	wth <- wth[order(wth$location, wth$date), ]

	carobiner::write_files(path, meta, d, wth=wth)
}
