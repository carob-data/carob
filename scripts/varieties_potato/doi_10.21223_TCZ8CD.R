# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. The description says 10 clones; each fieldbook has 11 (33 plots per site).
# 2. Admin names in the Minimal sheets are "Fayzabad" and "Khatlon" for Admin1-3. District names are kept
#    as documented in the fieldbooks (Fayzabad, Muminabad; also spelled Fayzobod, Muminobod). Fayzabad district
#    is in the Districts of Republican Subordination (not given in the source); Muminabad is in Khatlon.

## NOTES
# 1. All plants of each plot (40 planted, 7 m2) were harvested; yield is TTWP / 7 m2 (not adjusted for missing plants).
# 2. The Weather_data, Soil_analysis and Hobo_data sheets are empty; Crop_management has dates only.
# 3. No fertilizer or irrigation information.

## NEW VARIABLES
# Not included (as in doi:10.21223/15FGBK): yield adjusted for missing plants (TTYA, MTYA;
#   plants planted and harvested are recorded instead) and the weight and number of marketable
#   tubers by size category (MTWCI, MTWCII, NMTCI, NMTCII; limits not documented).
# Not included because they can be computed from the variables above: per-plant values (TTWPL,
#   MTWPL, TNTPL, NMTPL), non-marketable weight and number, average tuber weight (ATW), TTYNA, MTYNA, MTYA.

carob_script <- function(path) {

"
Dataset for: Assessment of Heat-Tolerant Potato Varieties in Tajikistan (2011)

The evaluation of 10 potato clones was conducted in the Tajikistan region, specifically in the localities of Muminabad and Fayzabad, in 2011. The experiment followed a randomized complete block design (RCBD) with three repetitions, aiming to assess heat tolerance in these areas.
"

	uri <- "doi:10.21223/TCZ8CD"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = "Enhanced food and income security in SWCA through potato varieties with improved tolerance to abiotic stress (07.7860.5-001.00)",
		# design as reported in the "Installation" sheet
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;yield_marketable",
		notes = NA,
		carob_contributor = "Vilma Hualla",
		carob_LLM = "Cursor Grok 4.7; Claude Opus 5.5",
		carob_date = "2026-11-09",
		carob_completion = 85,
		carob_effort = 1
	)

	## standard CIP fieldbook (sheets "Minimal", "Installation" and "Fieldbook"),
	## processed with the shared function; variety names and parents come from
	## the material list (text file)
	process <- carobiner::get_function("process_cip_lbvars", path, group)

	f <- ff[grep("^PTYL", basename(ff))]
	d <- lapply(f, function(x) {
		d <- process(x)
		r <- carobiner::read.excel(x, sheet="Fieldbook")
		harvest_area <- 7

		## other plot measurements (NEW VARIABLES); per-plot values per ha of harvested area
		## yield adjusted for missing plants: tuber weight per harvested plant x plants planted per ha
		#d$yield_adj_ <- as.numeric(r$TTWP) / as.numeric(r$NPH) * d$seed_density
		#d$yield_marketable_c1_ <- as.numeric(r$MTWCI) * 10000 / harvest_area
		#d$yield_marketable_c2_ <- as.numeric(r$MTWCII) * 10000 / harvest_area
		d$tuber_density_marketable <- as.numeric(r$NMTP) * 10000 / harvest_area
		#d$tuber_density_c1_ <- as.numeric(r$NMTCI) * 10000 / harvest_area
		#d$tuber_density_c2_ <- as.numeric(r$NMTCII) * 10000 / harvest_area
		d
	})
	d <- do.call(rbind, d)
	d$plot_area <- 7

	## variety names in upper case in the material list
	d$variety[d$variety == "REICHE"] <- "Reiche"
	d$variety[d$variety == "BASADRE"] <- "Basadre"

	## admin names (ISSUES 2)
	d$adm1[d$location == "Fayzabad"] <- "Districts of Republican Subordination"
	d$adm2[d$location == "Fayzabad"] <- "Fayzabad"
	d$adm1[d$location == "Muminabad"] <- "Khatlon"
	d$adm2[d$location == "Muminabad"] <- "Muminabad"
	d$adm3 <- NULL

	## irrigation and fertilizer not reported in the fieldbook; no dry matter.
	## The shared function adds empty columns for these; they are removed
	d$irrigated <- NA
	d$yield_moisture <- d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- NULL

	carobiner::write_files(path, meta, d)
}
