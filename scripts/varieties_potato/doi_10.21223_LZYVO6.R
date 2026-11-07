# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Plant stand was low in many plots: emergence was good (on average 24-29 of 30 plants),
#    but far fewer plants were harvested (on average 12-16; as few as 1-2). The cause is not
#    reported. Losses differ between clones, and per-plant yield is negatively correlated with
#    the number of plants harvested, which suggests a biological cause (e.g. disease) rather
#    than theft. Yield is therefore not adjusted for missing plants (TTWP / plot size).
# 2. In Gisozi, 4 plots have more plants harvested (NPH) than emerged (NPE).
# 3. 7 plots (5 in Ryansoro, 2 in Mparambo) have no harvest data (NA yield); plants
#    emerged, but no reason for the missing harvest records is given.

## NOTES
# 1. AVDM (average dry matter, %) has a single value per clone and site (not per plot).
#    It is used to compute yield_moisture (100 - AVDM).
# 2. It is not stated whether the trials were on-station or on-farm, or whether they were
#    irrigated (Mparambo is in the lowland Imbo plain), so on_farm and irrigated are NA.
# 3. No information on fertilizer use.
# 4. Gisozi fieldbook: "CIP395611.22" changed to "CIP388611.22" (Reiche). The former is not
#    in the material list of the same file; the latter is listed there and used at the other
#    two sites. To be confirmed with the data provider.
# 5. Weather (temperature only, no rainfall):
#    - Ryansoro: hourly records 2007-08-18 to 2007-11-10 (during the trial) are included
#      in the weather table, with dates reconstructed (see code).
#    - Mparambo: hourly records are mostly from 2006-10 to 2007-08, before planting, and the
#      dates are not consistent; not included.
#    - Gisozi: only two readings per day (08:00 and 15:00); not included.
#    - The "Weather_daily" and "Weather_monthly" sheets are summaries of these records
#      (with the same date problems) and are not used.

carob_script <- function(path) {

"
Dataset for: Evaluation of Potato Clones for Performance in Burundi, 2007

An evaluation of 8 potato clones was carried out in the cities of Ryansoro, Gisozi and Mparambo, Burundi, in 2007. The study utilized a Completely Randomized Block Design to assess the clones' performance under local conditions.
"

	uri <- "doi:10.21223/LZYVO6"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
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
		carob_LLM = "Claude Opus 5.5; Claude Fable 5",
		carob_date = "2026-10-28",
		carob_completion = 85,
		carob_effort = 1
	)

	## the three CIP fieldbooks (one per site) have the standard sheets
	## "Minimal", "Installation" and "Fieldbook", so we use the shared function
	process <- carobiner::get_function("process_cip_lbvars", path, group)
	
	f <- ff[grep("^PTYL", basename(ff))]
	d <- lapply(f, function(x) {
		out <- process(x)
		## AVDM: average dry matter (%) per clone and site, repeated for each plot (NOTES 1)
		fb <- carobiner::read.excel(x, sheet="Fieldbook", na=c("", "#N/A"))
		out$yield_moisture <- 100 - as.numeric(fb$AVDM)
		out
	})
	d <- do.call(rbind, d)

	## Variety names, codes and parents are set from 04_Material_list.xlsx by
	## process_cip_lbvars. In the Gisozi fieldbook, clone "CIP395611.22" is not in
	## the material list, which instead lists CIP388611.22 (Reiche), used at the
	## other two sites. We assume a typing error (NOTES 4)
	i <- d$variety == "CIP395611.22"
	d$variety[i] <- "Reiche"
	d$variety_code[i] <- "CIP388611.22"
	d$variety[d$variety == "REICHE"] <- "Reiche"
	## the mistyped rows also missed the pedigree from the material list
	j <- which(d$variety == "Reiche" & !is.na(d$variety_pedigree))[1]
	d$female_parent[i] <- d$female_parent[j]
	d$male_parent[i] <- d$male_parent[j]
	d$variety_pedigree[i] <- d$variety_pedigree[j]

	## not stated in the data (NOTES 2)
	d$on_farm <- NA
	d$irrigated <- NA

	## Weather (NOTES 5): hourly air temperature recorded at Ryansoro during the trial.
	## In this sheet the dates are inconsistent (day and month are swapped in some rows),
	## but the records are continuous, 24 per day, starting 2007-08-18 00:00,
	## so the date is reconstructed from the row order.
	fr <- f[grep("_RYA", basename(f))]
	w <- carobiner::read.excel(fr, sheet="Weather_Original")
	wth <- data.frame(
		country = "Burundi",
		adm1 = "Gitega",
		location = "Ryansoro",
		latitude = -3.7833,
		longitude = 29.7667,
		date = as.character(as.Date("2007-08-18") + (seq_len(nrow(w)) - 1) %/% 24),
		time = substr(as.character(w$Time), 1, 5),  # "HH:MM"
		temp = as.numeric(w$Temperature)
	)
	carobiner::write_files(path, meta, d, wth=wth)
}

