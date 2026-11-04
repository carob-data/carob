# R script for "carob"
# license: GPL (>=3)

## NOTES
# Dry direct-seeded rice (DSR) N-management trial at IRRI (Hongyan Liu, CESD), 2016 dry and wet season.
# 5 N management levels (N0-N4) x 2 varieties (V1, V2) x 4 reps; 40 plots per season.
# "2016 DS.xlsx" (Sheet1) and "2016 WS.xlsx" (summary) are used; the RDM documentation sheets give units.
# GY in t/ha -> kg/ha; TB (g/m2) -> kg/ha (*10); HI and GFP fractions -> %; N content % -> mg/g (*10).
# Stage-wise observations (PI, BT, FL, FL+15 days, PM, MT) are in a long table.
# The WS sheet has no rep column; the plot layout (plot numbers) matches the DS sheet, so rep is taken
#   from the DS plot -> rep mapping.

## ISSUES
# N treatments N0-N4 are not described (rates, splits, timing); only N_fert_level is set. N_fertilizer missing.
# Varieties are coded V1/V2 only; names unknown.
# P/K fertilizer, seed rate, irrigation regime not documented.
# planting_date/harvest_date taken from "Date Start"/"Date End" in the RDM documentation; confirm these are sowing/harvest dates.
# Location only given as "Upland" (IRRI upland farm, Los Banos, presumably); no coordinates. Georeference.
# yield moisture basis unknown.
# PN_m2 (panicles/m2) mapped to spike_density (panicles/ha); confirm convention for rice.
# SLW unit documented as g/cm2 but values (40-70) suggest a different unit; kept as recorded in long table.
# grain_N is out of bounds for some plots (0.88-1.41 % = 8.8-14.1 mg/g; valid_min 10); left as recorded.
# write_files: dmy_total is in both wide (maturity, TB_m2) and long (by stage) records; growth_stage is
#   not recognized as a time variable for the long table (no DAS/dates given).
# Per-sample organ weights (STEM, GLA, DW_GLA, Y_L, Rachis, filled/unfilled grain weights and counts), and
#   WS "PN" at PI/FL/FL-15/PM (unclear what is counted) and an unnamed PH column (10 values) are not mapped.

## NEW VARIABLES
# spikelets_per_panicle_: number of spikelets per panicle
# spikelet_density_: number of spikelets per m2
# specific_leaf_weight_: SLW, as recorded (documented as g/cm2)
# yellow_leaf_dw_: yellow (senesced) leaf dry weight (g, sample basis unclear)



carob_script <- function(path) {

"
N management trial

N management trial in DSR
"

	uri <- "doi:10.7910/DVN/1CZPHH"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IRRI",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fert_level;variety",
		response_vars = "yield;dmy_total;harvest_index;spike_density;seed_weight;grain_fill", 
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 60,
		carob_effort = 0.8
	)

	f1 <- ff[basename(ff) == "2016 DS.xlsx"]
	f2 <- ff[basename(ff) == "2016 WS.xlsx"]

	# two header rows; columns are referenced by position (header names in comments)
	r1 <- carobiner::read.excel(f1, sheet="Sheet1", col_names=FALSE, skip=2)
	r2 <- carobiner::read.excel(f2, sheet="summary", col_names=FALSE, skip=2)

	d1 <- data.frame(
		season = "dry",
		N_fert_level = r1[[1]],
		variety = r1[[2]],
		rep = as.integer(r1[[3]]),
		plot_id = as.character(r1[[4]]),
		yield = r1[[39]] * 1000,                # GY t/ha
		dmy_total = r1[[53]] * 10,              # TB_m2 g/m2
		harvest_index = r1[[54]] * 100,
		spike_density = r1[[55]] * 10000,       # PN_m2 panicles/m2
		spikelet_density_ = r1[[56]],
		spikelets_per_panicle_ = r1[[57]],
		grain_fill = r1[[58]] * 100,
		seed_weight = r1[[59]],                 # 1000-grain weight g
		# straw and grain N %, stored as text in the sheet
		residue_N = as.numeric(r1[[61]]) * 10,
		grain_N = as.numeric(r1[[62]]) * 10,
		planting_date = "2016-01-12",
		harvest_date = "2016-05-04"
	)

	rep_lookup <- unique(d1[, c("plot_id", "rep")])

	d2 <- data.frame(
		season = "wet",
		N_fert_level = r2[[1]],
		variety = r2[[2]],
		plot_id = as.character(r2[[3]]),
		yield = r2[[17]] * 1000,                # GY t/ha
		dmy_total = r2[[18]] * 10,              # TB g/m2
		harvest_index = r2[[19]] * 100,
		spike_density = r2[[20]] * 10000,       # PN /m2
		spikelets_per_panicle_ = r2[[21]],
		spikelet_density_ = r2[[22]],
		grain_fill = r2[[23]] * 100,
		seed_weight = r2[[24]],                 # 1000-GW g
		planting_date = "2016-06-15",
		harvest_date = "2016-10-04"
	)
	d2 <- merge(d2, rep_lookup, by="plot_id", all.x=TRUE)

	d <- carobiner::bindr(d1, d2)
	d$treatment <- paste(d$N_fert_level, d$variety, sep="_")
	d$trial_id <- paste0("2016", d$season)
	d$record_id <- 1:nrow(d)

	d$crop <- "rice"
	d$yield_part <- "grain"
	d$planting_method <- "direct seeding"
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$country <- "Philippines"
	d$location <- "IRRI, Los Banos"
	d$site <- "Upland farm"

	# stage-wise observations
	stack <- function(r, plotcol, cols, stages, var, mult=1) {
		do.call(rbind, lapply(seq_along(cols), function(i) {
			data.frame(plot_id = as.character(r[[plotcol]]),
				growth_stage = stages[i], variable = var, value = r[[cols[i]]] * mult)
		}))
	}
	st5 <- c("MT", "PI", "FL", "FL+15", "PM")
	st4 <- c("PI", "FL", "FL+15", "PM")
	st6 <- c("MT", "PI", "BT", "FL", "FL+15", "PM")
	L1 <- rbind(
		stack(r1, 4, 5:6, c("PI", "FL"), "plant_height"),
		stack(r1, 4, 7:10, st4, "dmy_total", 10),
		stack(r1, 4, 11:14, st4, "LAI"),
		stack(r1, 4, 15:18, st4, "specific_leaf_weight_"),
		stack(r1, 4, 19:22, st4, "yellow_leaf_dw_"),
		stack(r1, 4, 23:27, st5, "SPAD"),
		stack(r1, 4, 28:32, st5, "NDVI"),
		stack(r1, 4, 33:38, st6, "frac_int_radiation")
	)
	L1$trial_id <- "2016dry"
	L2 <- rbind(
		stack(r2, 3, 25:29, st5, "SPAD"),
		stack(r2, 3, 30:34, st5, "NDVI"),
		stack(r2, 3, 35:36, c("PI", "FL"), "plant_height"),
		stack(r2, 3, 42:47, st6, "frac_int_radiation")
	)
	L2$trial_id <- "2016wet"
	L <- rbind(L1, L2)
	dlong <- reshape(L, direction="wide", idvar=c("trial_id", "plot_id", "growth_stage"), timevar="variable")
	names(dlong) <- gsub("^value\\.", "", names(dlong))
	dlong <- merge(dlong, d[, c("trial_id", "plot_id", "record_id")], by=c("trial_id", "plot_id"))
	dlong$trial_id <- dlong$plot_id <- NULL
	rownames(dlong) <- NULL

	carobiner::write_files(path, meta, d, long=dlong)
}
