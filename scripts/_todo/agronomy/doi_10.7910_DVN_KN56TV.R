# R script for "carob"
# license: GPL (>=3)

## NOTES
# Cropping-system experiment (aus rice - aman rice - rabi maize/sunflower) at PSTU, Dumki, Patuakhali, Bangladesh, 2012-2014.
# Treatments: aus sowing date (A1-A4: 20 Mar, 5 Apr, 20 Apr, 5 May; this shifts the whole system), rabi crop
#   (C1 maize Pacific984, C2 sunflower Hysun33), mulch (M1 5 t/ha rice straw during rabi crop, M2 none). 4 reps.
#   Aus rice BRRI dhan65, aman rice BRRI dhan44 (from abstract).
# Only the 8 "Yield & yield cont char" files are mapped (one row per plot and crop-season).
# Plot codes: R<rep><date level><C level><M level>; date level is A (aus), D (aman) or S (rabi); in some files A/D are mixed up.
# Rice grain yield t/ha at 14% moisture; maize at 12%; sunflower at 9% (description file).
# Maize/sunflower yield components are averages of the per-plant/sub-sample columns (10 plants, 3 sub-samples).
# Planting dates: aus = the sowing date treatment; aman (D1-D4: 20 Jun, 5 Jul, 20 Jul, 5 Aug) and rabi
#   (S1-S4: 30 Nov, 15 Dec, 30 Dec, 15 Jan) are the target dates from the description table ("> 10 d after previous harvest").
# "Yield & yield cont char_2012-14" and "_2013-15" are sunflower (achene data) for 2012-13 and 2013-14 (assumed from file
#   names); their codes all say C1 (maize), so crop is taken from the file.

## ISSUES
# Not mapped: biomass/partitioning, LAI, plant height, tiller density (by stage; good candidates for a long table),
#   plot water depth, tensiometer, weather (site 2012-14 and long-term 1973-2012), economic analysis, maize/sunflower
#   head/cob diameter and sunflower seed set per plant.
# Aman/rabi planting dates are target dates, not observed; aus dates are seedbed sowing dates (transplanted, dates not given).
# Fertilizer, irrigation and spacing are not documented in the files (see thesis). No coordinates; georeference
#   PSTU, Dumki, Patuakhali.
# Duplicate plot codes in the rabi files (e.g. "R1S2C1M1" twice, in place of rep 4); the second occurrence is set to rep 4.
# Sunflower 2013-15 file: first columns (fresh weights, stover) are identical to the 2012-14 file; possible copy error.
# "Aus 2014": headers "1000 GW" and "Grain Yield" are swapped relative to the values (col 5 ~4.5 = yield, col 6 ~19 = TGW);
#   mapped by values.
# Rice straw yield assumed dry weight (dmy_residue); Aman 2012 straw in kg/ha and HI as fraction, other rice files t/ha and %.
# season "aus" is not in the season vocabulary.
# Aman 2012 plots are coded by date only (no rabi crop yet), so rabi_crop_ is NA there.
# write_files: missing coordinates, fertilizer, irrigated, harvest_date; NA in treatment var rabi_crop_; new variables.

## NEW VARIABLES
# system_timing_: date level of the system (1-4, = aus sowing date A1-A4 and the subsequent aman/rabi dates)
# rabi_crop_: rabi crop of the cropping system (maize or sunflower)
# REY_: rice equivalent yield (kg/ha)
# spikelets_per_panicle_: florets (spikelets) per panicle
# spikelet_fertility_: % filled spikelets
# cobs_per_plant_, kernels_per_cob_, cob_length_ (cm), cob_diameter_ (cm), achenes_per_head_


carob_script <- function(path) {

"
Raw data PhD thesis:The feasibility of high yielding aus-aman-rabi cropping systems in the polders of the low salinity coastal zone of Bangladesh

The cropping intensity and productivity of the coastal zone of Bangladesh is much lower than that of most of the country, despite enormous investment in the construction of polders to protect the lands from tidal flooding and saline water intrusion. In the coastal zone, farmers typically grow a single crop of rice (aman) during the rainy season using low yielding, photoperiod sensitive local varieties, followed by a low yielding pulse or sesame crop. Much of the cultivable land remains fallow for 3-7 months each year. The main reasons for the low productivity in the polders are waterlogging as a result of high rainfall and poor drainage during the rainy season, the late rice harvest (December/January), and the lack of readily available fresh water during the dry season. The waterlogging prevents the production of high yielding and earlier maturing aman varieties (HYV aman). The late harvest of the local aman varieties and saturated soil prevent the cultivation of high yielding rabi crops such as maize and sunflower, and delay establishment of traditional rabi crops such as mungbean and sesame, which are thus often damaged or destroyed by pre-monsoon rains and cyclones. However, by draining excess water at low tide during and at the end of the rainy season, it should be possible to grow modern HYV rice varieties and establish high yielding rabi crops at the optimum time. Furthermore, the south central coastal zone (Barisal division) is supplied by large volumes of fresh river water throughout the year. Therefore, the research undertaken in this thesis was designed to evaluate, under good water management, the feasibility of intensifying to high yielding aus-aman-rabi cropping systems in the low salinity coastal zone. A replicated cropping system experiment was conducted for two years at Patuakhali using a short duration modern aus variety (BRRI dhan65), a medium duration modern aman variety (BRRI dhan44), and high yielding/value rabi crops. Performance of the systems was compared for 4 aus sowing dates (20 March, 5 and 20 April, 5 May), 2 rabi crop species (maize – Pacific984, sunflower – Hysun33) and 2 mulch treatments (0, 5 t/ha of rice straw during the rabi crops). The longest total system in-field crop duration was about 330 d. This allowed for an average turn-around time between crops of about 10 d. Annual cropping system rice equivalent yield (REY) ranged from 13.7-20.0 t/ha. The highest REY was for systems with early (20 March and 5 April) aus sowings, at 17-20 t/ha with maize, and 16-20 t/ha with sunflower (2 to 5 times that of current farmer practice). Rabi crop yield was the main determinant of trends in total system rice equivalent yield, which declined as establishment date of the rabi crops was delayed beyond 15 December. All cropping systems were profitable in terms of gross margin, net return and benefit-cost ratio. The most profitable system (with 5 April aus sowing and maize) had a gross margin of Tk. 101,000 per ha, net return Tk. 73,000 per ha and BCR 1.29. While maximum yield and profitability occurred with the first 2 aus sowings, these systems had the shortest turnaround time. Early aus sowing advanced the sowing of all crops and thus reduced the risk of rabi crop damage by pre-monsoon rainfall and cyclones. This work shows that, with good water management, it is possible to implement highly productive and profitable aus-aman-rabi cropping systems in the low salinity coastal zone of Bangladesh.
"

	uri <- "doi:10.7910/DVN/KN56TV"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "planting_date;rabi_crop_;mulch",
		response_vars = "yield;dmy_residue;harvest_index;seed_weight", 
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 35,
		carob_effort = 1.2
	)

	getf <- function(name) ff[basename(ff) == name]

	## rice
	rice <- function(fname, season, year, cols, straw_mult, hi_mult) {
		r <- carobiner::read.excel(getf(fname), col_names=FALSE)
		r <- r[grepl("^R[1-4]", r[[1]]), ]
		data.frame(
			code = r[[1]],
			season = season,
			year = year,
			spike_density = as.numeric(r[[cols[1]]]) * 10000,
			spikelet_fertility_ = as.numeric(r[[cols[2]]]),
			spikelets_per_panicle_ = as.numeric(r[[cols[3]]]),
			seed_weight = as.numeric(r[[cols[4]]]),
			yield = as.numeric(r[[cols[5]]]) * 1000,
			dmy_residue = as.numeric(r[[cols[6]]]) * straw_mult,
			harvest_index = as.numeric(r[[cols[7]]]) * hi_mult
		)
	}
	# column order: panicle density, fertility, florets/panicle, 1000 GW, grain yield, straw yield, HI
	d1 <- rice("Yield & yield cont char_Aman 2012.xlsx", "aman", 2012, 2:8, 1, 100)
	d2 <- rice("Yield & yield cont char_Aman 2013.xlsx", "aman", 2013, c(2,3,4,6,5,7,8), 1000, 1)
	d3 <- rice("Yield & yield cont char_Aus 2013.xlsx", "aus", 2013, 2:8, 1000, 1)
	# header says 1000 GW in col 5 and grain yield in col 6, but the values are the other way around
	d4 <- rice("Yield & yield cont char_Aus 2014.xlsx", "aus", 2014, c(2,3,4,6,5,7,8), 1000, 1)
	drice <- rbind(d1, d2, d3, d4)
	drice$crop <- "rice"
	drice$variety <- ifelse(drice$season == "aus", "BRRI dhan65", "BRRI dhan44")
	drice$yield_moisture <- 14

	## maize
	maize <- function(fname, year) {
		r <- carobiner::read.excel(getf(fname), col_names=FALSE)
		r <- r[grepl("^R[1-4]", r[[1]]), ]
		stopifnot(all(r[[1]] == r[[22]]), all(r[[1]] == r[[79]]))
		# typo in a cob length value ("22.6y") in 2013-14
		r[[44]] <- sub("y$", "", r[[44]])
		data.frame(
			code = r[[1]],
			season = "rabi",
			year = year,
			cobs_per_plant_ = rowMeans(sapply(r[, 2:17], as.numeric)),
			kernels_per_cob_ = rowMeans(sapply(r[, 23:32], as.numeric)),
			cob_length_ = rowMeans(sapply(r[, 38:47], as.numeric)),
			cob_diameter_ = rowMeans(sapply(r[, 53:62], as.numeric)),
			# 100-kernel weight -> 1000-kernel weight
			seed_weight = rowMeans(sapply(r[, 68:70], as.numeric)) * 10,
			REY_ = as.numeric(r[[77]]) * 1000,
			dmy_residue = as.numeric(r[[83]]),
			yield = as.numeric(r[[84]]),
			harvest_index = as.numeric(r[[86]]) * 100
		)
	}
	d5 <- maize("Yield & yield cont char_2012-13.xlsx", 2012)
	d6 <- maize("Yield & yield cont char_2013-14.xlsx", 2013)
	dmaize <- rbind(d5, d6)
	dmaize$crop <- "maize"
	dmaize$variety <- "Pacific984"
	dmaize$yield_moisture <- 12

	## sunflower
	sunflower <- function(fname, year) {
		r <- carobiner::read.excel(getf(fname), col_names=FALSE)
		r <- r[grepl("^R[1-4]", r[[1]]), ]
		stopifnot(all(r[[1]] == r[[33]]), all(r[[1]] == r[[45]]))
		data.frame(
			code = r[[1]],
			season = "rabi",
			year = year,
			achenes_per_head_ = rowMeans(sapply(r[, 34:36], as.numeric)),
			seed_weight = rowMeans(sapply(r[, 37:39], as.numeric)),
			REY_ = as.numeric(r[[43]]) * 1000,
			dmy_residue = as.numeric(r[[49]]),
			yield = as.numeric(r[[50]]),
			harvest_index = as.numeric(r[[52]]) * 100
		)
	}
	d7 <- sunflower("Yield & yield cont char_2012-14.xlsx", 2012)
	d8 <- sunflower("Yield & yield cont char_2013-15.xlsx", 2013)
	dsun <- rbind(d7, d8)
	dsun$crop <- "sunflower"
	dsun$variety <- "Hysun33"
	dsun$yield_moisture <- 9

	d <- carobiner::bindr(drice, dmaize, dsun)
	d$trial_id <- paste(d$season, d$year, d$crop, sep="_")
	d$rep <- as.integer(substr(d$code, 2, 2))
	# duplicated codes in the rabi files take the place of rep 4
	dup <- duplicated(d[, c("trial_id", "code")])
	d$rep[dup] <- 4L
	d$system_timing_ <- as.integer(substr(d$code, 4, 4))
	cl <- substr(d$code, 6, 6)
	ml <- substr(d$code, 8, 8)
	d$rabi_crop_ <- ifelse(cl == "1", "maize", ifelse(cl == "2", "sunflower", NA))
	d$rabi_crop_[d$crop != "rice"] <- d$crop[d$crop != "rice"]
	d$treatment <- paste0("A", d$system_timing_, ifelse(cl == "", "", paste0("C", cl, "M", ml)))
	d$plot_id <- paste0("R", d$rep, d$treatment)

	# mulch was applied to the rabi crops only
	d$mulch <- ifelse(d$crop == "rice", 0, ifelse(ml == "1", 5000, 0))
	d$mulch_type <- ifelse(d$mulch > 0, "straw", NA)

	md <- c("03-20", "04-05", "04-20", "05-05")
	ad <- c("06-20", "07-05", "07-20", "08-05")
	rd <- c("11-30", "12-15", "12-30", "01-15")
	i <- d$system_timing_
	d$planting_date <- ifelse(d$season == "aus", paste0(d$year, "-", md[i]), 
		ifelse(d$season == "aman", paste0(d$year, "-", ad[i]), 
		paste0(d$year + (i == 4), "-", rd[i])))
	d$crop_rotation <- ifelse(is.na(d$rabi_crop_), NA, paste0("rice;rice;", d$rabi_crop_))
	d$code <- d$year <- NULL

	d$yield_part <- ifelse(d$crop == "sunflower", "seed", "grain")
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$country <- "Bangladesh"
	d$adm1 <- "Barisal"
	d$adm2 <- "Patuakhali"
	d$adm3 <- "Dumki"
	d$location <- "Patuakhali Science and Technology University"
	d$record_id <- 1:nrow(d)

	carobiner::write_files(path, meta, d)
}
