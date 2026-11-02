# R script for "carob"
# license: GPL (>=3)

## NOTES
# Weed-control (herbicide) trial on volunteer (glyphosate-resistant) corn in dry-seeded rice, IRRI Los Banos.
# Only the "FIELD EXPERIMENT" sheet is mapped (2 field trials, 2013; RCBD with 3 reps each).
# Methods from the included paper (Chauhan & Opena 2013, doi:10.4236/ajps.2013.412294):
#   rice cv. Rc222 hand-sown at 100 kg/ha in 20 cm rows; corn broadcast at 50 seeds/m2 before rice planting
#   (not in weed-free plots); N 2 x 30 kg/ha at 14 and 28 DAP; site rotovated twice; irrigated as required.
#   At 35 DAP rice and corn/weeds were sampled from a central 1 m2 area -> g/m2 * 10 = kg/ha; counts/m2 * 10000 = /ha.
# Rice tiller count/m2 -> stem_density; rice leaf area (cm2/m2) / 10000 -> LAI.
# Weed TOTAL in the sheet excludes corn; volunteer corn is kept separately and as a species in the long table.
# Long table: weed density/biomass per species at 13 DAP (count 1 day before bispyribac application, 1 m2 quadrat)
#   and at 35 DAP; species not recorded in a plot are omitted.

## ISSUES
# No rice yield: crop responses are early-season only (35 DAP rice tillers, leaf area, stem/leaf biomass).
# Screenhouse "POT EXPERIMENT" and "BLUE TRAY EXPERIMENT" sheets are not mapped (pots/trays).
# Trial 2 has a LEFT/RIGHT "POSITION" (6 rows per treatment); its meaning is not explained (paper: 3 reps).
# Rice stand count at 14 DAS: sheet multiplies the mean of 4 counts by 5; assumed counts per 1 m row
#   and 5 rows/m (20 cm rows) -> plants/m2. Confirm before using plant_density.
# Weed-free (WF) treatment: description says no corn seeds plus oxadiazon fb bispyribac (RR); paper only says weed-free.
# Trial 1 has no rice stem/leaf biomass (empty in sheet).
# herbicide_amount is only set for single-product treatments; sequential rates are in herbicide_rates_.
# "butachlor" is not in the herbicide vocabulary (kept).
# P and K fertilizer not reported. No coordinates in paper/data (IRRI farm, Los Banos); georeference.
# stem_density (tillers) up to 714/m2, above valid_max (3.1e6/ha); left as recorded.
# write_files: missing yield, longitude/latitude, P/K fertilizer, harvest_date (crop sampled at 35 DAP, not harvested);
#   NA in treatment vars herbicide_amount (sequential) and herbicide_timing (weedy control);
#   weed_density/weed_biomass in both wide (totals, excl. corn) and long (per species) tables.

## NEW VARIABLES
# position_: LEFT/RIGHT position in trial 2 (meaning unclear)
# corn_density_: volunteer corn density at 35 DAP (plants/ha)
# corn_biomass_: volunteer corn dry biomass at 35 DAP (kg/ha)
# corn_broadcast_: whether volunteer corn seeds were broadcast in the plot (logical)
# herbicide_rates_: herbicide rates in order of application (kg ai/ha, ";"-separated)


carob_script <- function(path) {

"
Management of Volunteer Corn Seedlings in Dry-Seeded Rice

The demand for corn is increasing in Asia for feed and biofuel. It is grown in the rice-corn cropping system. During harvest of corn, however, seeds drop on the soil surface and become problems as volunteer corn seedlings in the subsequent dry-seeded rice crop, in which the suppressive effect of standing water is absent. A study was conducted in screenhouse and field conditions to evaluate the effect of rice herbicides on the management of volunteer corn seedlings. In the screenhouse experiment, bispyribac-sodium at 0.030 and 0.045 kg ai/ha provided complete control of corn seedlings. Fenoxaprop + ethoxysulfuron and penoxsulam + cyhalofop did not provide effective control of corn seedlings. In the field, the sole application of bispyribac and sequential application of oxadiazon and bispyribac suppressed corn biomass by 60% - 82% and 89% - 91%, respectively, as compared with the nontreated control. The results of this study demonstrate that, in the absence of other management strategies, volunteer corn seedlings in dry-seeded rice systems can be managed by using bispyribac-sodium.
"

	uri <- "doi:10.7910/DVN/27235"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IRRI",
		publication = "doi:10.4236/ajps.2013.412294",
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "herbicide_product;herbicide_amount;herbicide_timing",
		response_vars = "weed_density;weed_biomass;stem_density;LAI;dmy_stems;dmy_leaves",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 55,
		carob_effort = 1
	)

	f <- ff[basename(ff) == "Effect of pre and post emergence herbicide on survival and growth of rice and 2nd generation round up ready corn.xls"]
	rd <- function(range) carobiner::read.excel(f, sheet="FIELD EXPERIMENT", range=range, col_names=FALSE)

	# rice stand count at 14 DAS
	ra1 <- rd("A4:J27")
	ra2 <- rd("A43:J90")
	# corn and weed count 1 day before bispyribac application (13 DAP)
	rb1 <- rd("P4:AC27")
	rb2 <- rd("P43:AC90")
	# 35 DAP sampling (rows are in a different order than above)
	rc1 <- rd("AF4:BD27")
	rc2 <- rd("AF44:BD91")

	stand <- function(r, trial) {
		data.frame(
			trial_id = trial,
			position_ = r[[2]],
			rep = as.integer(r[[3]]),
			treatment = r[[4]],
			# mean of 4 counts * 5; see ISSUES
			plant_density = as.numeric(r[[10]]) * 10000
		)
	}
	d <- rbind(stand(ra1, "1"), stand(ra2, "2"))

	dap35 <- function(r, trial) {
		data.frame(
			trial_id = trial,
			position_ = r[[1]],
			rep = as.integer(r[[2]]),
			treatment = r[[3]],
			stem_density = as.numeric(r[[4]]) * 10000,
			LAI = as.numeric(r[[5]]) / 10000,
			dmy_stems = as.numeric(r[[6]]) * 10,
			dmy_leaves = as.numeric(r[[7]]) * 10,
			corn_density_ = as.numeric(r[[8]]) * 10000,
			corn_biomass_ = as.numeric(r[[9]]) * 10,
			weed_density = as.numeric(r[[24]]) * 10000,
			weed_biomass = as.numeric(r[[25]]) * 10
		)
	}
	d35 <- rbind(dap35(rc1, "1"), dap35(rc2, "2"))
	d <- merge(d, d35, by=c("trial_id", "position_", "rep", "treatment"), all=TRUE)
	d$plot_id <- paste(d$trial_id, d$position_, d$rep, d$treatment, sep="_")

	trt <- data.frame(
		treatment = c("WEEDY", "OXA", "BISRR", "BIS1.5RR", "OXAfbBISRR", "OXAfbBIS1.5RR", "WF", "BUTPROP"),
		herbicide_product = c("none", "oxadiazon", "bispyribac-sodium", "bispyribac-sodium",
			"oxadiazon;bispyribac-sodium", "oxadiazon;bispyribac-sodium", "oxadiazon;bispyribac-sodium", "butachlor;propanil"),
		herbicide_amount = c(0, 0.75, 0.03, 0.045, NA, NA, NA, 0.6),
		herbicide_rates_ = c("0", "0.75", "0.03", "0.045", "0.75;0.03", "0.75;0.045", "0.75;0.03", "0.6"),
		herbicide_timing = c(NA, "1", "14", "14", "1;14", "1;14", "1;14", "6"),
		corn_broadcast_ = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE)
	)
	d <- merge(d, trt, by="treatment", all.x=TRUE)
	d$herbicide_used <- d$herbicide_product != "none"
	d$herbicide_times <- ifelse(grepl("fb", d$treatment) | d$treatment == "WF", 2L, ifelse(d$treatment == "WEEDY", 0L, 1L))

	d$crop <- "rice"
	d$variety <- "Rc222"
	d$planting_date <- ifelse(d$trial_id == "1", "2013-01-23", "2013-03-20")
	d$planting_method <- "direct seeding"
	d$seed_rate <- 100
	d$row_spacing <- 20
	d$land_prep_method <- "rotovating"
	d$N_fertilizer <- 60
	d$N_splits <- 2L
	d$irrigated <- TRUE
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$country <- "Philippines"
	d$location <- "IRRI, Los Banos"
	d$yield_part <- "grain"
	d$record_id <- 1:nrow(d)

	# weed and corn counts per species
	sp13 <- c("Zea mays", "Eleusine indica", "Digitaria", "Leptochloa chinensis", "Echinochloa colona", "Cyperus rotundus",
		"Cyperus iria", "Eclipta prostrata", "Phyllanthus niruri", "Portulaca oleracea")
	w13 <- function(r, trial) {
		do.call(rbind, lapply(seq_along(sp13), function(i) {
			data.frame(trial_id = trial, position_ = r[[1]], rep = as.integer(r[[2]]), treatment = r[[3]],
				weed_species = sp13[i], weed_density = as.numeric(r[[3 + i]]) * 10000)
		}))
	}
	L13 <- rbind(w13(rb1, "1"), w13(rb2, "2"))
	L13$DAP <- 13L

	sp35 <- c("Eleusine indica", "Echinochloa colona", "Digitaria", "Cyperus rotundus", "Cyperus iria",
		"Phyllanthus niruri", "Eclipta prostrata")
	w35 <- function(r, trial) {
		x <- lapply(seq_along(sp35), function(i) {
			data.frame(trial_id = trial, position_ = r[[1]], rep = as.integer(r[[2]]), treatment = r[[3]],
				weed_species = sp35[i], weed_density = as.numeric(r[[8 + 2*i]]) * 10000,
				weed_biomass = as.numeric(r[[9 + 2*i]]) * 10)
		})
		corn <- data.frame(trial_id = trial, position_ = r[[1]], rep = as.integer(r[[2]]), treatment = r[[3]],
			weed_species = "Zea mays", weed_density = as.numeric(r[[8]]) * 10000, weed_biomass = as.numeric(r[[9]]) * 10)
		do.call(rbind, c(list(corn), x))
	}
	L35 <- rbind(w35(rc1, "1"), w35(rc2, "2"))
	L35$DAP <- 35L

	dlong <- carobiner::bindr(L13, L35)
	dlong <- dlong[!is.na(dlong$weed_density), ]
	dlong <- merge(dlong, d[, c("trial_id", "position_", "rep", "treatment", "record_id")],
		by=c("trial_id", "position_", "rep", "treatment"))
	dlong <- dlong[, c("record_id", "DAP", "weed_species", "weed_density", "weed_biomass")]

	carobiner::write_files(path, meta, d, long=dlong)
}
