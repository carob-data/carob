# R script for "carob"
# license: GPL (>=3)

## NOTES
# Flooded vs aerobic rice at IRRI (Los Banos), dry seasons 2011 and 2012. Four experiments:
#   C11 2011: flooded transplanted, IR72 / Rc222 / SacG7 x continuous flooding or a vegetative-stage drought
#     (flooding stopped 10-12 DAT for about 2 weeks; protocol); 6 plots, 1 per treatment (sub-reps A/B).
#   C11 2012: flooded transplanted, IR72 / Rc222 / SacG7, 3 reps; 1 plant/hill transplanted at 15 DAS, 20 x 20 cm.
#   UJ 2011: aerobic Rc222, flash-flooding irrigation (field UD3) vs pivot sprinklers (field UJ), 4 reps each.
#   UE 2012: aerobic Rc222 under pivot sprinklers, 6 plots.
# Protocols are in the companion dataset doi:10.7910/DVN/QDITFO.
# Wide table: one record per plot; harvest values are the mean of the final-harvest samples of that plot
#   (5 plants or subsamples A-C/A-B). For C11 2012 yield is from the 5.76 m2 harvest area ("Harvest_area_C11_DS2012").
# Long table: weekly plot means of LAI, shoot dry matter and tiller density by DAP (DAS for direct seeded crops).
# UE 2012 sowing date derived from the first sampling (2012-02-08 at 12 DAS).

## ISSUES
# The C11 2011 treatments are not replicated (one plot per variety x watering); sub-reps A/B are within-plot samples.
# C11 2011 protocol lists variety "I 45" but the data say SACG7/SacG7; kept as in the data.
# C11 2011 sowing/transplanting dates and the planting dates of the aerobic UJ fields are partly missing:
#   UJ/UD3 sowing dates are in the data (2011-01-29/30); C11 2011 sowing date not given.
# Planting method for UJ/UE taken as direct seeding (aerobic crops "direct-seeded" in the abstract).
# 1000-grain weight is "HKFiGr14" (C11 2011) and "KFiGr14" (2012); ~25-30 g, mapped to seed_weight.
#   UE 2012 KFiGr14 values are ~26000 and were divided by 1000 (assumed mg); UJ 2011 has no 14% grain weight.
# UE 2012 FilledPC_nb is exactly 0.5 in 4 of 6 plots (placeholder?), grain_fill not mapped for UE.
# UJ 2011 has no HI and filled-grain fraction.
# UD3 "flash flooding" mapped to irrigation_method "flood".
# No coordinates (IRRI fields C11, UZ, UD3, UJ, UE), fertilizer not documented.
# Not mapped: organ-level development, leaf, sheath, internode, tiller, N analysis, tensiometer, meteo files;
#   treatment means files (they summarise the mapped plot data).
# Remaining messages: missing NPK and coordinates, new variables, out of bounds stem_density (high tiller densities),
#   dmy_total and stem_density in both wide (final harvest) and long (weekly) tables.

## NEW VARIABLES
# field_: IRRI experimental field code
# water_management_: flooded / aerobic as described by the authors


carob_script <- function(path) {

"
Modifications in development and growth of a dual-adapted tropical rice variety grown as either a flooded or an aerobic crop

As the human population continues to grow and competition for natural resources increases, there is a consequent need to produce more food with fewer resources. Flooded rice crops use large quantities of water, which become scarcer for farmers. New methods of saving irrigation water have been tested and released but they often suffer from a yield trade-off. A comparative study was carried out with a recent dual-adapted variety cultivated as either a flooded transplanted crop or a direct-seeded and soil water-saturated crop, referred to as an aerobic crop, to dissect the crop physiological differences induced by crop management. Experiments were conducted at IRRI's experiment station in Los Banos, Philippines, in the dry seasons of 2011 and 2012. Grain yields were 2 tons/ha or 25% lower in the aerobic crop as a result of a complex series of modifications and adjustments in plant architecture and yield components. Four main changes-- higher plant density, slower rate of leaf appearance, lower nitrogen content, and reduced size of all organs--induced by the aerobic crop environment were responsible for three chains of modifications that resulted in lower biomass accumulation and finally lower grain yield. Leaf appearance rates were bilinear in both environments, initially similar in both environments and slower after an inflection point that occurred at the appearance of the 8th leaf in aerobic crops instead of the 11th leaf in flooded crops. As a consequence, two fewer leaves were produced by main tillers in aerobic crops, whose individual leaf area was much lower. In 2012, plant nitrogen content was about 1% lower in the aerobic crop than in the flooded crop during the entire crop duration. Biomass accumulation of the variety used was biphasic in both environments, with high radiation use efficiency during the two phases of effective biomass accumulation in flooded crops,  and the standard radiation use efficiency expected with C3 species in aerobic crops during the same phases.
"

	uri <- "doi:10.7910/DVN/28202"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety;water_management_;drought_stress;planting_method",
		response_vars = "yield;dmy_total;harvest_index", 
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 45,
		carob_effort = 1.2
	)

	rd <- function(f) read.csv(ff[basename(ff) == f])

	# plot means of the final harvest samples
	hmean <- function(x, kw) {
		x <- x[!is.na(x$Grain14_gm2), ]
		x$kw <- x[[kw]]
		if (is.null(x$HI)) x$HI <- NA
		if (is.null(x$FilledPC_nb)) {
			x$fp <- NA
		} else {
			# fraction in 2012, but check in case of %
			x$fp <- x$FilledPC_nb
			if (max(x$fp, na.rm=TRUE) <= 1) x$fp <- x$fp * 100
		}
		a <- stats::aggregate(x[, c("Grain14_gm2", "Shoot_gm2", "HI", "Tiller_density", "kw", "fp")],
			x[, "Plot_NO", drop=FALSE], mean, na.rm=TRUE)
		a$harvest_date <- as.character(as.Date(tapply(x$Date, x$Plot_NO, max)[as.character(a$Plot_NO)], "%m/%d/%Y"))
		data.frame(plot_id=as.character(a$Plot_NO), harvest_date=a$harvest_date, yield=a$Grain14_gm2 * 10,
			dmy_total=a$Shoot_gm2 * 10, harvest_index=a$HI * 100, stem_density=a$Tiller_density * 10000,
			seed_weight=a$kw, grain_fill=a$fp)
	}
	# weekly plot means
	wmean <- function(x, trial) {
		a <- stats::aggregate(x[, c("LAI", "Shoot_gm2", "Tiller_density")], x[, c("Plot_NO", "DAS")], mean, na.rm=TRUE)
		data.frame(trial_id=trial, plot_id=as.character(a$Plot_NO), DAP=a$DAS, LAI=a$LAI,
			dmy_total=a$Shoot_gm2 * 10, stem_density=a$Tiller_density * 10000)
	}

	## C11 2011
	b1 <- rd("Biomasses_C11_DS2011.csv")
	d1 <- hmean(b1, "HKFiGr14")
	tr <- unique(b1[, c("Plot_NO", "Variety", "Watering")])
	d1 <- merge(d1, data.frame(plot_id=as.character(tr$Plot_NO), variety=tr$Variety, wat=tr$Watering), by="plot_id")
	d1$trial_id <- "C11_DS2011"
	d1$field_ <- "C11"
	d1$water_management_ <- "flooded"
	d1$drought_stress <- ifelse(d1$wat == "Drought", "vegetative stage", "none")
	d1$planting_method <- "transplanting"
	d1$irrigation_method <- "continuous flooding"
	d1$wat <- NULL
	l1 <- wmean(b1, "C11_DS2011")

	## C11 2012
	b2 <- rd("Biomasses_C11_DS2012.csv")
	d2 <- hmean(b2, "KFiGr14")
	h2 <- rd("Harvest_area_C11_DS2012.csv")
	h2 <- data.frame(plot_id=as.character(h2$Plot_NO), variety=h2$Variety, yield_area=h2$Grain14_gm2 * 10,
		spike_density=h2$Panicle_density * 10000, planting_date=as.character(as.Date(h2$Sowing_date, "%m/%d/%Y")),
		harvest_date_area=as.character(as.Date(h2$Harvest_date, "%m/%d/%Y")))
	rp <- unique(b2[, c("Plot_NO", "Rep_NO")])
	d2 <- merge(d2, h2, by="plot_id")
	d2 <- merge(d2, data.frame(plot_id=as.character(rp$Plot_NO), rep=rp$Rep_NO), by="plot_id", all.x=TRUE)
	d2$yield <- d2$yield_area
	d2$harvest_date <- d2$harvest_date_area
	d2$yield_area <- d2$harvest_date_area <- NULL
	d2$transplanting_date <- as.character(as.Date(d2$planting_date) + 15)
	d2$trial_id <- "C11_DS2012"
	d2$field_ <- "C11"
	d2$water_management_ <- "flooded"
	d2$drought_stress <- "none"
	d2$planting_method <- "transplanting"
	d2$irrigation_method <- "continuous flooding"
	d2$row_spacing <- 20
	d2$plant_spacing <- 20
	l2 <- wmean(b2, "C11_DS2012")

	## UJ 2011
	b3 <- rd("Biomasses_UJ_DS2011.csv")
	d3 <- hmean(b3, "HKFiGr_DW")
	pd <- unique(b3[, c("Plot_NO", "Rep_NO", "Sowing_date", "Field")])
	pd <- pd[!is.na(pd$Sowing_date) & pd$Sowing_date != "", ]
	d3 <- merge(d3, data.frame(plot_id=pd$Plot_NO, rep=pd$Rep_NO, field_=ifelse(grepl("^UD", pd$Plot_NO), "UD3", "UJ"),
		planting_date=as.character(as.Date(pd$Sowing_date, "%m/%d/%Y"))), by="plot_id")
	d3$trial_id <- "UJ_DS2011"
	d3$variety <- "Rc222"
	d3$water_management_ <- "aerobic"
	# UD3 "flash flooding", UJ "pivot sprinklers" (protocol)
	d3$irrigation_method <- ifelse(d3$field_ == "UD3", "flood", "center pivot sprinkler")
	# "HKFiGr_DW" is the dry weight of a counted grain sample (not at 14%), not used
	d3$seed_weight <- NA
	d3$drought_stress <- "none"
	d3$planting_method <- "direct seeding"
	l3 <- wmean(b3, "UJ_DS2011")

	## UE 2012
	b4 <- rd("Biomasses_UE_DS2012.csv")
	d4 <- hmean(b4, "KFiGr14")
	d4$trial_id <- "UE_DS2012"
	d4$field_ <- "UE"
	d4$variety <- "Rc222"
	d4$water_management_ <- "aerobic"
	d4$irrigation_method <- "center pivot sprinkler"
	# KFiGr14 is ~26000 in UE (other files ~26 g); assumed mg
	d4$seed_weight <- d4$seed_weight / 1000
	# FilledPC_nb is exactly 0.5 in 4 of 6 plots, apparently a placeholder
	d4$grain_fill <- NA
	d4$drought_stress <- "none"
	d4$planting_method <- "direct seeding"
	d4$planting_date <- "2012-01-27"
	l4 <- wmean(b4, "UE_DS2012")

	d <- carobiner::bindr(d1, d2, d3, d4)
	num <- sapply(d, is.numeric)
	d[num] <- lapply(d[num], function(i) replace(i, is.nan(i), NA))
	d$variety[d$variety == "SACG7"] <- "SacG7"
	d$rep <- as.integer(d$rep)
	d$treatment <- paste(d$variety, d$water_management_, d$drought_stress, sep="_")
	d$record_id <- seq_len(nrow(d))

	d$crop <- "rice"
	d$yield_part <- "grain"
	d$yield_moisture <- 14
	d$season <- "dry"
	d$irrigated <- TRUE
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$country <- "Philippines"
	d$adm1 <- "Laguna"
	d$location <- "IRRI, Los Banos"
	d$longitude <- as.numeric(NA)
	d$latitude <- as.numeric(NA)
	d$geo_from_source <- NA

	dlong <- rbind(l1, l2, l3, l4)
	num <- sapply(dlong, is.numeric)
	dlong[num] <- lapply(dlong[num], function(i) replace(i, is.nan(i), NA))
	dlong <- merge(d[, c("trial_id", "plot_id", "record_id")], dlong, by=c("trial_id", "plot_id"))
	dlong$trial_id <- dlong$plot_id <- NULL

	carobiner::write_files(path, meta, d, long=dlong)
}
