# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Some plots have no harvest data (2 plots in Mparambo, 5 in Ryansoro). They are removed
#    because yield cannot be missing.
# 2. Plant stand was low in some plots (as few as 1-2 of 30 plants harvested). Yield is
#    reported per plot area (not adjusted for missing plants), as in the source (TTYNA).
# 3. AVDM (average dry matter, %) has a single value per clone and site, so it is probably
#    not a plot-level measurement. It is not included.
# 4. It is not stated whether the trials were on-station or on-farm, or whether they were
#    irrigated (Mparambo is in the lowland Imbo plain), so on_farm and irrigated are NA.
# 5. No information on fertilizer use.
# 6. process_cip_lbvars() does not find "Plot size (m2) " (trailing space in the
#    Installation sheet); yields are recomputed in this script. The function could use
#    trimws() on the factor names to avoid this.
# 7. Gisozi fieldbook: "CIP395611.22" changed to "CIP388611.22" (Reiche). The former is not
#    in the material list of the same file; the latter is listed there and used at the other
#    two sites. To be confirmed with the data provider.
# 8. Weather (temperature only, no rainfall):
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
		carob_LLM = "Claude Opus 5.5",
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
		fb <- carobiner::read.excel(x, sheet="Fieldbook")
		# the function looks for a "plot" column, but these fieldbooks use "PLOT"
		out$plot_id <- as.character(fb$PLOT)
		# In these fieldbooks the label is "Plot size (m2) " (with a trailing space),
		# so the function does not find the plot size and returns NA yields.
		# Yields are recomputed here from plot weights (kg/plot) and plot size (m2).
		ins <- carobiner::read.excel(x, sheet="Installation")
		plot_size <- as.numeric(ins$Value[trimws(ins$Factor) == "Plot size (m2)"])
		out$yield <- as.numeric(fb$TTWP) * 10000 / plot_size
		out$yield_marketable <- as.numeric(fb$MTWP) * 10000 / plot_size
		out
	})
	d <- do.call(rbind, d)

	## In the Gisozi fieldbook, clone "CIP395611.22" is not in the material list of that
	## same file, which instead lists CIP388611.22 (Reiche), used at the other two sites.
	## We assume a typing error (see ISSUES 7).
	d$variety[d$variety == "CIP395611.22"] <- "CIP388611.22"

	## plots without harvest data (see ISSUES 1)
	d <- d[!is.na(d$yield), ]

	## not stated in the data (see ISSUES 4)
	d$on_farm <- NA
	d$irrigated <- NA

	## variety names from the material list; keep the CIP number as variety_code
	vars <- data.frame(
		variety_code = c("CIP381381.20", "CIP382171.4", "CIP388611.22", "CIP720118"),
		name = c("Victoria", "Ruhanyura", "Reiche", "Ndinamagara")
	)
	d$variety_code <- d$variety
	d <- merge(d, vars, by="variety_code", all.x=TRUE)
	i <- !is.na(d$name)
	d$variety[i] <- d$name[i]
	d$name <- NULL

	## Weather (see ISSUES 8): hourly air temperature recorded at Ryansoro during the trial.
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
