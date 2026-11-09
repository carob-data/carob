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



## NOT PROCESSED
# Annotated_C11_DS2011_1.csv: not read. columns: Date, DAS, SumT, Variety, Watering, Tiller_NO, _FREQ_, Minitiated, Mappeared, Mligulated, Msenesced, Mlive_tiller, .... Observation records are missing from d until this file is standardized.
# Annotated_C11_DS2012.csv: not read. columns: Date, DAS, SumT, Variety, Tiller_NO, _FREQ_, Mappeared, Mligulated, Msenesced, Mlive_tiller, Mplant_height, Mculm_length, .... Observation records are missing from d until this file is standardized.
# Biomasses_mean_C11_DS2011.csv: not read. columns: Variety, Watering, DAS, SumT, _FREQ_, MLAI, MTiller_density, MDead_leaves_gm2, MDead_tiller_gm2, MDead_mat_gm2, MCulm_gm2, MSheath_gm2, .... Observation records are missing from d until this file is standardized.
# Biomasses_mean_C11_DS2012.csv: not read. columns: Variety, DAS, SumT, SumGR, _FREQ_, MLAI, Mtiller_density, Mdead_leaves_gm2, Mdead_tiller_gm2, Mdead_mat_gm2, Mculm_gm2, Msheath_gm2, .... Observation records are missing from d until this file is standardized.
# Biomasses_mean_UE_DS2012.csv: not read. columns: DAS, SumT, SumGR, MLAI, Mdead_leaves_gm2, Mdead_tiller_gm2, Mdead_mat_gm2, Mculm_gm2, Msheath_gm2, Mstem_gm2, Mleaf_gm2, Mpanicle_gm2, .... Observation records are missing from d until this file is standardized.
# Biomasses_mean_UJ_DS2011.csv: not read. columns: Field, Date, SumT, SumGR, MLAI, Mdead_leaves_gm2, MDead_tiller_gm2, Mdead_mat_gm2, Mculm_gm2, Msheath_gm2, Mstem_gm2, Mleaf_gm2, .... Observation records are missing from d until this file is standardized.
# Conventional_area_UE_DS2012.csv: not read. columns: Date, SN, Plot_NO, Leaf_area. Observation records are missing from d until this file is standardized.
# Culm_panicle_C11_DS2012.csv: not read. columns: Date, SN, Plot_NO, Variety, Plant_NO, Tiller_NO, Culm_length, Culm_length_n1, Culm_length_n2, Culm_length_n3, Culm_length_n4, Culm_length_n5, .... Observation records are missing from d until this file is standardized.
# Culm_panicle_UE_DS2012.csv: not read. columns: Date, SN, Plot_NO, Plant_NO, Tiller_NO, Organ, Length. Observation records are missing from d until this file is standardized.
# Development_C11_DS2011.csv: not read. columns: Date, Rep_NO, Plot_NO, Plant_NO, Live_tiller, Tiller_NO, Appeared, Ligulated, Senesced, Plant_height, Sheath_length_n, Sheath_length_n1, .... Observation records are missing from d until this file is standardized.
# Development_C11_DS2012.csv: not read. columns: Date, SN, Plot_NO, Variety, Plant_NO, Tiller_NO, Live_tiller, Appeared, Ligulated, Senesced, Plant_height, SumT, .... Observation records are missing from d until this file is standardized.
# Development_mean_C11_DS2011.csv: not read. columns: Date, DAS, SumT, Variety, Watering, Tiller_NO, _FREQ_, Minitiated, Mappeared, Mligulated, Msenesced, Mlive_tiller, .... Observation records are missing from d until this file is standardized.
# Development_mean_C11_DS2012.csv: not read. columns: Date, DAS, SumT, Variety, Tiller_NO, _FREQ_, Mappeared, Mligulated, Msenesced, Mlive_tiller, Mplant_height, Mculm_length, .... Observation records are missing from d until this file is standardized.
# Development_mean_UE_DS2012.csv: not read. columns: Date, DAS, SumT, Mappeared, Mligulated, Msenesced, Mlive_tiller, Mplant_height, Msampling_length, Mtiller_density, Mplant_density, Mapp_sen, .... Observation records are missing from d until this file is standardized.
# Development_mean_UJ_DS2011.csv: not read. columns: Field, Date, SumT, Mappeared, Mligulated, Msenesced, Mlive_tiller, Msheath_length_n, Mculm_length, Mtiller_density, Mplant_density, Msampling_length, .... Observation records are missing from d until this file is standardized.
# Development_UE_DS2012.csv: not read. columns: Date, SN, Plot_NO, Plant_NO, Tiller_NO, Sampling_length, Live_tiller, Appeared, Ligulated, Senesced, Plant_height. Observation records are missing from d until this file is standardized.
# Development_UJ_DS2011.csv: not read. columns: Date, Subsample, Plot_NO, Sampling_length, Plant_NO, Dead_tiller, Live_tiller, Tiller_NO, Appeared, Ligulated, Senesced, Plant_height, .... Observation records are missing from d until this file is standardized.
# Dissections_C11_DS2011.csv: not read. columns: Date, Plot_NO, Plant_NO, Appeared, Ligulated, Live_tiller, Plant_height, Initiated, Tip_length, Leaf_length_n1, Sheath_length_n1, Stem_length, .... Observation records are missing from d until this file is standardized.
# Harvest_C11_DS2012.csv: not read. columns: Plot_NO, Variety, Sowing_date, Harvest_date, Hill_count, Panicle_count, FiGr_DW, UnFiGr_DW, KFiGr_DW, MC_PC, Sampling_area, Panicle_per_plant, .... Observation records are missing from d until this file is standardized.
# Harvest_mean_C11_DS2011.csv: not read. columns: Variety, Watering, _FREQ_, Mdead_leaves_gm2, Mdead_tiller_gm2, Mdead_mat_gm2, Mculm_gm2, Msheath_gm2, Mstem_gm2, Mleaf_gm2, Mpanicle_gm2, Mgrain14_gm2, .... Observation records are missing from d until this file is standardized.
# Harvest_mean_C11_DS2012.csv: not read. columns: Variety, _FREQ_, Mdead_leaves_gm2, Mdead_tiller_gm2, Mdead_mat_gm2, Mculm_gm2, Msheath_gm2, Mstem_gm2, Mleaf_gm2, Mpanicle_gm2, Mgrain14_gm2, Mlive_mat_gm2, .... Observation records are missing from d until this file is standardized.
# Harvest_mean_UE_DS2012.csv: not read. columns: Mdead_leaves_gm2, Mdead_tiller_gm2, Mdead_mat_gm2, Mculm_gm2, Msheath_gm2, Mstem_gm2, Mleaf_gm2, Mpanicle_gm2, Mgrain14_gm2, Mlive_mat_gm2, Mshoot_gm2, Mvegetative_mat_gm2, .... Observation records are missing from d until this file is standardized.
# Harvest_mean_UJ_DS2011.csv: not read. columns: Field, Mdead_leaves_gm2, MDead_tiller_gm2, Mdead_mat_gm2, Mculm_gm2, Msheath_gm2, Mstem_gm2, Mleaf_gm2, Mpanicle_gm2, Mgrain14_gm2, Mlive_mat_gm2, Mshoot_gm2, .... Observation records are missing from d until this file is standardized.
# Internode_C11_DS2011.csv: not read. columns: Date, Plot_NO, Plant_NO, Tiller_NO, Leaf_NO, Culm_length, Culm_length_n1, Variety, Watering, Internode_length. Observation records are missing from d until this file is standardized.
# Internode_mean_C11_DS2011.csv: not read. columns: Variety, Watering, Tiller_NO, Leaf_NO, _FREQ_, Minternode_length, Linternode_length, dinternode_length. Observation records are missing from d until this file is standardized.
# Internode_mean_C11_DS2012.csv: not read. columns: Variety, Tiller_NO, Organ, _FREQ_, Mlength, Llength. Observation records are missing from d until this file is standardized.
# Internode_mean_UE_DS2012.csv: not read. columns: Organ, Mlength, Llength, dlength. Observation records are missing from d until this file is standardized.
# Internode_mean_UJ_DS2011.csv: not read. columns: Plot_NO, Mculm_length, Mculm_length_n1, Mculm_length_n2, Mculm_length_n3, Mculm_length_n4, Mculm_length_n5, Mculm_length_rmng, Lculm_length, Lculm_length_n1, Lculm_length_n2, Lculm_length_n3, .... Observation records are missing from d until this file is standardized.
# Leaf_area_C11_DS2012.csv: not read. columns: Expt, SN, Date, Variety, Plot_NO, Plant_NO, Tiller_NO, Leaf_NO, Leaf_DW, Leaf_area, Leaf_width, Leaf_length, .... Observation records are missing from d until this file is standardized.
# Leaf_area_mean_C11_DS2011.csv: not read. columns: Variety, Watering, Tiller_NO, Leaf_NO, _FREQ_, Mleaf_area, Mleaf_length, Mleaf_width, Lleaf_area, Lleaf_length, Lleaf_width, dleaf_area, .... Observation records are missing from d until this file is standardized.
# Leaf_area_mean_C11_DS2012.csv: not read. columns: Variety, Tiller_NO, Leaf_NO, _FREQ_, Mleaf_area, Mleaf_length, Mleaf_width, Lleaf_area, Lleaf_length, Lleaf_width, dleaf_area, dleaf_length, .... Observation records are missing from d until this file is standardized.
# Leaf_area_mean_UE_DS2012.csv: not read. columns: Leaf_NO, Mleaf_area, Mleaf_length, Mleaf_width, Lleaf_area, Lleaf_length, Lleaf_width, dleaf_area, dleaf_length, dleaf_width, Msheath_length, Msheath_length_2. Observation records are missing from d until this file is standardized.
# Leaf_area_mean_UJ_DS2011.csv: not read. columns: Field, Leaf_NO, Mleaf_area, Mleaf_width, Mleaf_length, Lleaf_area, Lleaf_width, Lleaf_length, dleaf_area, dleaf_width, dleaf_length, Leaf_area_sum. Observation records are missing from d until this file is standardized.
# Leaf_area_UE_DS2012.csv: not read. columns: Expt, SN, Date, Plot_NO, Plant_NO, Tiller_NO, Leaf_NO, Code, Leaf_DW, Plot_plant, Tiller_leaf, Leaf_area, .... Observation records are missing from d until this file is standardized.
# Leaf_area_UJ_DS2011.csv: not read. columns: Date, Plot_NO, Plant_NO, Leaf_NO, Leaf_area, Leaf_width, Mleaf_width, Leaf_length, Field, Rep_NO, SumT, SumGR. Observation records are missing from d until this file is standardized.
# Leaf_C11_DS2011.csv: not read. columns: Date, Plot_NO, Plant_NO, Leaf_area, Leaf_width, Leaf_length, Tiller_NO, Leaf_NO, Variety, Watering. Observation records are missing from d until this file is standardized.
# Meteo_C11_DS2011.csv: not read. columns: Date, GR, AirT, RH, SoilT, SumT_air, SumT_dry, SumT_soil, SumT. Observation records are missing from d until this file is standardized.
# Meteo_C11_DS2012.csv: not read. columns: Date, N, GR, RH, AirT, T2, T3, T4, T5, T6, SoilT, SumGR, .... Observation records are missing from d until this file is standardized.
# Meteo_UE_DS2012.csv: not read. columns: Date, AirT, SoilT, GR, SumGR, SumT. Observation records are missing from d until this file is standardized.
# Meteo_UJ_DS2011.csv: not read. columns: Date, JD, AirT, SoilT, SumT_air, SumT_soil, GR, RH, SumGR, SumT. Observation records are missing from d until this file is standardized.
# Nanalysis_C11_DS2012.csv: not read. columns: Field, Date, SN, Plot_NO, Variety, Watering, Organ, N_PC, DAS. Observation records are missing from d until this file is standardized.
# Nanalysis_mean_C11_DS2012.csv: not read. columns: Variety, DAS, _FREQ_, MN_PC. Observation records are missing from d until this file is standardized.
# Nanalysis_UE_DS2012.csv: not read. columns: Field, Date, SN, Plot_NO, Variety, Code, N_PC. Observation records are missing from d until this file is standardized.
# Sheath_C11_DS2012.csv: not read. columns: Date, SN, Plot_NO, Variety, Plant_NO, Tiller_NO, Leaf_NO, Sheath_length. Observation records are missing from d until this file is standardized.
# Sheath_mean_C11_DS2011.csv: not read. columns: Variety, Watering, Tiller_NO, Leaf_NO, _FREQ_, Msheath_length_n, Lsheath_length_n, dsheath_length_n. Observation records are missing from d until this file is standardized.
# Sheath_mean_C11_DS2012.csv: not read. columns: Variety, Tiller_NO, Leaf_NO, _FREQ_, Msheath_length, Lsheath_length, dsheath_length. Observation records are missing from d until this file is standardized.
# Sheath_mean_UE_DS2012.csv: not read. columns: Leaf_NO, Msheath_length, Lsheath_length, dsheath_length. Observation records are missing from d until this file is standardized.
# Sheath_mean_UJ_DS2011.csv: not read. columns: Field, Ligulated, Msheath_length, Lsheath_length. Observation records are missing from d until this file is standardized.
# Sheath_UE_DS2012.csv: not read. columns: Date, SN, Plot_NO, Plant_NO, Tiller_NO, Leaf_NO, Sheath_length. Observation records are missing from d until this file is standardized.
# Table descriptions_for upload.xlsx: not read. sheet Sheet1 (of 3): field, year, season, experiment, table, classification, description. Observation records are missing from d until this file is standardized.
# Tensiometer_C11_DS2011.csv: not read. columns: Date, DAS, Variety, Tensio_NO, Water_potential, SumT. Observation records are missing from d until this file is standardized.
# Tensiometer_mean_C11_DS2011.csv: not read. columns: Variety, DAS, SumT, _FREQ_, Mwater_potential, Lwater_potential. Observation records are missing from d until this file is standardized.
# Tiller_C11_DS2012.csv: not read. columns: Date, SN, Plot_NO, Variety, Plant_NO, Tiller_NO, Organ, Length, Dry_weight, SumT. Observation records are missing from d until this file is standardized.
# Tiller_length_UE_DS2012.csv: not read. columns: Date, Plot_NO, Organ, Length. Observation records are missing from d until this file is standardized.
# Tiller_mean_C11_DS2012.csv: not read. columns: Variety, Tiller_NO, Date, SumT, Organ, _FREQ_, Mdry_weight, Ldry_weight. Observation records are missing from d until this file is standardized.

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
