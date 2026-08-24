# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Sheet "Summary" row 27 (Country cell reads "summa", i.e. a truncated
#    "summary"/average label) is a mean-across-trials row for variety H511, not
#    an observed field trial - it is dropped; rows 28-34 (Country=="Kenya") are
#    kept as the 7 real observed records. Confirmed by re-reading the raw sheet
#    (row 27 is not "Kenya" and its DAF/DAM/yields do not match any individual
#    replicate, consistent with an average).
# 2. The publication (Gummadi et al. 2020, PLOS ONE) reports the calibration
#    data came from three seasons (SR 2000, LR 2001, SR 2001) at KARI Embu, but
#    the Summary sheet does not say which season each of the 7 rows belongs to
#    - planting_date/harvest_date are left NA rather than guessed.
# 3. "Observed Biomass Yield (Kg/ha)" is mapped to dmy_total (dry matter), based
#    on the publication's Methods statement that "grain and dry matter yields at
#    harvest" were used for calibration; moisture of the grain yield is not
#    reported anywhere (yield_moisture/yield_isfresh left NA).
# 4. No coordinates for the KARI Embu research farm itself could be found (data,
#    dataset metadata, or publication); coordinate supplied by the carob
#    contributor for the named farm (see longitude/latitude comment below);
#    geo_from_source=FALSE since it is not sourced from the dataset/publication.
# 5. Only the "Summary" sheet is used. "All"/"Sheet1"/"APSIM"/"DSSAT"/"ETH-DSSAT"/
#    "ETH-APSIM" sheets, and the "Simulated ..." / duplicate DSSAT-block columns
#    of "Summary" itself, hold APSIM/DSSAT model output (simulated, not
#    observed) and are out of scope for this script.


carob_script <- function(path) {

"
APSIM and DSSAT Calibration

Calibration and validation of crop simulation models for 3 maize cultivars in Embu county Kenya
"

	uri <- "doi:10.7910/DVN/0ECMP0"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRISAT",
		publication = "doi:10.1371/journal.pone.0241147",
		# AgMIP = Agricultural Model Intercomparison and Improvement Project,
		# explicitly named in the publication as the protocol this study follows
		project = "AgMIP",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;dmy_total;flowering_days;maturity_days",
		notes = NA,
		carob_contributor = "Oscar Bautista",
		carob_LLM = "Claude Sonnet 5",
		carob_date = "2026-08-24",
		carob_completion = 30,
		carob_effort = 0.5
	)


	f1 <- ff[basename(ff) == "APSIM-DSSAT-Calibration.xlsx"]

	r <- carobiner::read.excel(f1, sheet="Summary")
	r <- r[27:33, ]

	d <- data.frame(
		country = r[[1]],
		adm1 = "Embu",
		location = "KARI Embu Research Farm", # per publication Methods
		crop = "maize",
		variety = r[[2]],
		treatment = r[[2]],
		flowering_days = as.numeric(r[[3]]),
		maturity_days = as.numeric(r[[5]]),
		dmy_total = as.numeric(r[[7]]), # Kg/ha, already; dry matter, see ## ISSUES 3
		yield = as.numeric(r[[9]]) # Kg/ha grain yield
	)

## separate individual trials. For example trials in different locations or years.
## do _not_ separate by treatments within a trial. For a survey, each row gets a unique trial_id
## each of the 7 rows is its own distinct observed trial (see ## ISSUES 2: season/year unknown)
	d$trial_id <- as.character(seq_len(nrow(d)))

## about the data (TRUE/FALSE)
	d$on_farm <- FALSE   # KARI Embu research farm, not a farmer's field
	d$is_survey <- FALSE
	
	d$irrigated <- as.logical(NA) # not reported
	d$longitude <- 37.4578
	d$latitude <- -0.5027
	d$geo_uncertainty <- NA
	d$geo_source <- "KARI Embu research farm, google maps"
	d$geo_from_source <- FALSE

	d$planting_date <- as.character(NA)
	d$harvest_date  <- as.character(NA)

	d$P_fertilizer <- as.numeric(NA)
	d$K_fertilizer <- as.numeric(NA)
	d$N_fertilizer <- as.numeric(NA)
	d$S_fertilizer <- as.numeric(NA)
	d$lime <- as.numeric(NA)

### Yield
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA) # not reported
	d$yield_isfresh <- as.numeric(NA) # not reported

# all scripts must end like this
	carobiner::write_files(path, meta, d)
}
