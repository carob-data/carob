# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm alternative cropping systems at Cooch Behar, 2015-16. The stat sheet names the country column Westbengal, so the country is India and adm1 is West Bengal.
# The 2016-17 CSE-2017 sheet in this dataset names R-W-J as rice, wheat and jute, R-M as rice and maize, and R-L-J as rice, lentil and jute. Those letters are used for 2015-16.
# Grain and seed yields are the GY columns, already t/ha, stored as kg/ha. Jute is stored as stems because the file does not say fiber or stick.
# A season column of 0 t/ha with no crop letter is an empty season in a two-crop system, not a measured zero.

## ISSUES
# The letter B in R-L-B and R-M-B is not named in either workbook.
# R-M-J rabi yields are about 0.6 t/ha at about 38 Rs/kg. R-M, which is labeled rice-maize, has rabi yields of about 6 t/ha. That rabi letter is not stored as maize.
# No coordinates, fertilizer rates, planting dates, or grain moisture.
# Author should georeference the Cooch Behar nodes.
# write_files: missing variables: yield_moisture, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: component_code_. invalid terms: season: prekharif. NA detected: yield_part.

## NEW VARIABLES
# component_code_: one letter of the cropping-system code for this season.


## NOT PROCESSED
# 2015-16 Sheet1 and stat: pivot and a second copy of the same yields.
# Prices, costs, and returns in AUD. Farm-gate price in Rs/kg is kept.
# 2016-17 CSE-2017 and Cal-syst: costs and returns, no crop yield.
# 2016-17 Incrop-17: maize q/bigha mixed with overall means and a second layout. Vegetable columns are returns, not yields.
# 2016-17 tillage codes CT and ZT. They are not on the 2015-16 yield rows.


carob_script <- function(path) {

"
Farmer participatory on-farm trials with CA technologies comparing with farmers’ practices (CT), were conducted in several fields in each community. Likewise, farmer-participatory alternative cropping systems trials were conducted comparing to exiting systems and to find out suitable and more profitable cropping systems, prioritized to increase visibility and to avoid implementation and management problems that emerge when utilizing small plots with significant edge effects. Most trials were replicated in several fields within each community and were farmer-managed with backstopping from project staff and NARES partners. Project partners and staff coordinated monitoring and data acquisition. Where possible, collaborating farmers were selected by the community, and the project worked with existing farmer groups, with groups of both men and women farmers.
"

	uri <- "hdl:11529/10547998"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 45,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "Rabi crops 2015-16-ACST-All nodes-Chochbehar.xlsx"]
	r <- carobiner::read.excel(f, sheet="CROPPING SYSTEM")
	r <- r[!is.na(r$`Cropping systems`), ]

	# Letters named in CSE-2017 in the 2016-17 workbook.
	known <- c(R="rice", W="wheat", M="maize", L="lentil", J="jute")
	part <- c(rice="grain", wheat="grain", maize="grain", lentil="seed", jute="stems")
	sys <- trimws(as.character(r$`Cropping systems`))
	bits <- strsplit(sys, "-", fixed=TRUE)
	farmer <- trimws(as.character(r$`NAME OF THE FARMER`))
	ycol <- c("GY (t ha-1)-kharif", "GY (t ha-1)-rabi", "GY (t ha-1)-kharif 1")
	pcol <- c("FARM GATE PRICE (RS./KG)...5", "FARM GATE PRICE (RS./KG)...10", "FARM GATE PRICE (RS/KG)")
	sname <- c("kharif", "rabi", "prekharif")

	blocks <- vector("list", 3)
	for (i in 1:3) {
		letter <- vapply(bits, function(z) if (length(z) >= i) z[i] else NA_character_, "")
		y <- as.numeric(r[[ycol[i]]])
		keep <- !is.na(letter) & !is.na(y)
		crop <- unname(known[letter])
		# M is maize in the two-crop system R-M. In R-M-J and R-M-B the rabi yield and price do not match that maize.
		crop[letter == "M" & sys != "R-M"] <- NA_character_
		yp <- unname(part[crop])
		price <- as.numeric(r[[pcol[i]]])
		blocks[[i]] <- data.frame(
			country = "India",
			adm1 = "West Bengal",
			location = "Cooch Behar",
			trial_id = "Cooch Behar 2015-16",
			plot_id = paste(farmer, sys),
			season = sname[i],
			treatment = sys,
			component_code_ = letter,
			crop = crop,
			yield_part = yp,
			yield = y * 1000,
			crop_price = price,
			currency = "INR",
			on_farm = TRUE,
			is_survey = FALSE,
			geo_from_source = FALSE
		)
		blocks[[i]] <- blocks[[i]][keep, ]
	}

	d <- carobiner::bindr(blocks[[1]], blocks[[2]], blocks[[3]])
	d$record_id <- as.integer(seq_len(nrow(d)))

	carobiner::write_files(path, meta, d)
}
