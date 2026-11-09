# R script for "carob"
# license: GPL (>=3)

## NOTES
# Only the aerobic plant density experiments (field UZ, IRRI, 2012DS and 2013DS) are mapped: variety RC222,
#   dry direct seeding in rows 20 cm apart, thinned to 1 plant/hill at the treatment distance within the row
#   (2012: 7, 10, 15, 20 cm; 2013: 6, 10, 20 cm in the protocol, 16 cm also in the randomization file). 3 reps (RCBD).
# Harvest: processed files "Harvest_area_UZ1density_DS201x.csv" (5 m2 harvest area; grain at 14% moisture).
# Long table: plot-level weekly biomass means ("Biomass_mean_plot_UZ1density_DS201x.csv"), keyed by DAP (= DAS).
# 2012DS sowing date from the harvest file; 2013DS sowing date (2012-12-11) derived from the first sampling date (2012-12-26) at 15 DAS.
# plant_density = 1 plant per hill at 20 cm x plant distance.

## ISSUES
# The C11 (flooded) and UJ/UE (aerobic) development/meteo files and the 2011 greenhouse experiment (BG10)
#   are not mapped (no yield in this dataset; harvest of these trials appears to be in doi:10.7910/DVN/28202).
# No coordinates in the data (IRRI, Los Banos, field UZ); fertilizer management not documented.
# stem_density from tiller counts exceeds the vocabulary maximum at the highest densities (up to ~1700 tillers/m2).
# Not mapped: leaf-level area/SLA, sheath, phenology (leaf appearance), N concentration (2013) and meteo files.
# Remaining messages: missing NPK, coordinates; out of bounds stem_density; new variables.

## NEW VARIABLES
# spikelet_density_: total spikelets per m2
# grains_per_panicle_: filled grains per panicle
# dmy_panicle_: panicle dry matter (kg/ha), long table



## NOT PROCESSED
# 2011 BG10 Meteo.csv: not read. columns: Date, Day, GR, AirT, T1, T2, T3, T4, SoilT. Observation records are missing from d until this file is standardized.
# 2011 BG10 Phenology.csv: not read. columns: Date, Pot, Variety, Treatment, Live Tiller, App, Lig, Sen, LG App Blade, Plant HT. Observation records are missing from d until this file is standardized.
# 2011 BG10 Phenology_SumT.csv: not read. columns: Date, Pot, Live_Tiller, Appeared, Ligulated, Senesced, LG_App_Blade, Plant_Height, Variety, Treatment, Plot, Size, .... Observation records are missing from d until this file is standardized.
# 2012DS UZ1density Biomass.csv: not read. columns: Date, Plot_NO, Plant_distance, KFiGr_DW, KUnFiGr_DW, Culm_DW, DMatA_DW, DMatB_DW, DMatC_DW, FiGr_DW, Leaf_DW, Panicle_DW, .... Observation records are missing from d until this file is standardized.
# 2012DS UZ1density Harvest Area.csv: not read. columns: Plot_NO, Plant_distance, Sowing_date, Harvest_date, Hill_count, Panicle_count, FiGr_DW, UnFiGr_DW, KFiGr_DW, KUnFiGr_DW, MC_PC, Sampling_area. Observation records are missing from d until this file is standardized.
# 2012DS UZ1density Harvest Weekly.csv: not read. columns: Year, Date, DAS, Plant_distance, Plot_NO, SumT, Sample_plant_count, Culm_DW, Dead_mat_DW, DMatA_DW, DMatB_DW, DMatC_DW, .... Observation records are missing from d until this file is standardized.
# 2012DS UZ1density Leaf Area.csv: not read. columns: Expt, SN, Date, Plot_NO, Plant_NO, Tiller_NO, Leaf_NO, Code, Leaf_DW, Leaf_area, Leaf_width, Mleaf_width, .... Observation records are missing from d until this file is standardized.
# 2012DS UZ1density Meteo.csv: not read. columns: Date, Day, GR, AirT, RH, T10, T11, T12, T13, T14, T15, T16, .... Observation records are missing from d until this file is standardized.
# 2012DS UZ1density Phenology.csv: not read. columns: Date, SN, Plant_distance, Plot_NO, Plant_NO, Tiller_NO, Live_tiller, Appeared, Ligulated, Senesced, Plant_height. Observation records are missing from d until this file is standardized.
# 2012DS UZ1density Random.csv: not read. columns: Plot_NO, Plant_distance. Observation records are missing from d until this file is standardized.
# 2012DS UZ1density Sheath.csv: not read. columns: Date, SN, Plant_distance, Plot_NO, Plant_NO, Tiller_NO, Leaf_NO, Sheath_length. Observation records are missing from d until this file is standardized.
# 2012DS UZ1density Tiller.csv: not read. columns: Date, Plot_NO, Plant_distance, Plant_NO, Culm_DW, CuN1, CuN2, CuN3, CuN4, CuN5, CuNR, Panicle_DW, .... Observation records are missing from d until this file is standardized.
# 2013DS UZ1density Area Weight.csv: not read. columns: SN, Date, Plot_NO, Leaf_area, Leaf_DW. Observation records are missing from d until this file is standardized.
# 2013DS UZ1density Culm Panicle.csv: not read. columns: Date, Expt, SN, Plot_NO, Plant_NO, Tiller_NO, Organ, Culm_length, PI, PI_date, Panicle_length, FL_PC_pan. Observation records are missing from d until this file is standardized.
# 2013DS UZ1density Harvest Area.csv: not read. columns: Plot_NO, Rep_NO, Plant_distance, Sampling_area, Date, Hill_NO, Panicle_count, FiGr_DW, UnFiGr_rachis_DW, UnFiGr_DW, Rachis_DW, MC_PC, .... Observation records are missing from d until this file is standardized.
# 2013DS UZ1density Harvest Weekly.csv: not read. columns: Year, Date, DAS, Plant_distance, Plot_NO, SumT, Sample_plant_count, Culm_DW, Dead_mat_DW, DMatA_DW, DMatB_DW, DMatC_DW, .... Observation records are missing from d until this file is standardized.
# 2013DS UZ1density Leaf Area.csv: not read. columns: Expt, SN, Date, Plot_NO, Plant_NO, Tiller_NO, Leaf_NO, Leaf_width, Mleaf_width, Leaf_length, Leaf_area, Leaf_DW. Observation records are missing from d until this file is standardized.
# 2013DS UZ1density Meteo.csv: not read. columns: Date, GR, AirT, SoilT. Observation records are missing from d until this file is standardized.
# 2013DS UZ1density Nitrogen.csv: not read. columns: Expt, SN, Date, Plot, Organ, Plant_distance, N, , . Observation records are missing from d until this file is standardized.
# 2013DS UZ1density Other Tiller.csv: not read. columns: SN, Date, Plot_NO, Culm_DW, DMatA_DW, DMatB_DW, DMatC_DW, Leaf_DW, Panicle_DW, Sheath_DW. Observation records are missing from d until this file is standardized.
# 2013DS UZ1density Others Area Complement.csv: not read. columns: Date, Plot_NO, Leaf_area_B, Leaf_area_others. Observation records are missing from d until this file is standardized.
# 2013DS UZ1density Others Area.csv: not read. columns: SN, Date, Plot_NO, Leaf_area_B, Leaf_area_others, Leaf_area_rmng. Observation records are missing from d until this file is standardized.
# 2013DS UZ1density Phenology.csv: not read. columns: Date, Expt, SN, DAS, Rep_NO, Plot_NO, Plant_NO, Tiller_NO, Live_tiller, Appeared, Ligulated, Senesced, .... Observation records are missing from d until this file is standardized.
# 2013DS UZ1density Random.csv: not read. columns: Plot_NO, Variety, Plant_distance. Observation records are missing from d until this file is standardized.
# 2013DS UZ1density Sheath.csv: not read. columns: Date, Expt, SN, Plot_NO, Plant_NO, Tiller_NO, Leaf_NO, Sheath_length. Observation records are missing from d until this file is standardized.
# 2013DS UZ1density Tiller.csv: not read. columns: SN, Date, Plot_NO, Culm_DW, Leaf_DW, Panicle_DW, Sheath_DW. Observation records are missing from d until this file is standardized.
# 2013DS UZ1density Yield Components.csv: not read. columns: Plot_NO, Panicle_count_T0, Panicle_count, FiGr_DW_T0, FiGr_DW, UnFiGr_DW_T0, UnFiGr_DW, actual_KFiGr, KFiGr_DW, actual_KUnFiGr, KUnFiGr_DW, Rachis_DW_T0, .... Observation records are missing from d until this file is standardized.
# Biomass_mean_plot_UZ1density_DS2012.csv: not read. columns: Plot_NO, Plant_distance, DAS, SumT, _TYPE_, _FREQ_, MSLA, MLAI, Mdead_leaves_gm2, Mdead_tillers_gm2, Mdead_mat_gm2, Mculm_gm2, .... Observation records are missing from d until this file is standardized.
# Biomass_mean_plot_UZ1density_DS2013.csv: not read. columns: Plot_NO, Plant_distance, SN, _TYPE_, _FREQ_, DAS, SumT, MSLA, MLeaf_Area, MLAI, Mdead_leaves_gm2, Mdead_tiller_gm2, .... Observation records are missing from d until this file is standardized.
# Biomass_mean_UZ1density_DS2012.csv: not read. columns: Plant_distance, SN, _TYPE_, _FREQ_, MSLA, MLAI, Mdead_leaves_gm2, Mdead_tillers_gm2, Mdead_mat_gm2, Mculm_gm2, Msheath_gm2, Mstem_gm2, .... Observation records are missing from d until this file is standardized.
# Biomass_mean_UZ1density_DS2013.csv: not read. columns: Plant_distance, SN, _TYPE_, _FREQ_, DAS, SumT, MSLA, MLeaf_Area, MLAI, Mdead_leaves_gm2, Mdead_tiller_gm2, Mdead_mat_gm2, .... Observation records are missing from d until this file is standardized.
# Development_C11_DS2011.csv: not read. columns: Date, Rep_NO, Plot_NO, Plant_NO, Live_tiller, Tiller_NO, Appeared, Ligulated, Senesced, Plant_height, Sheath_length_n, Sheath_length_n1, .... Observation records are missing from d until this file is standardized.
# Development_C11_DS2012.csv: not read. columns: Date, SN, Plot_NO, Variety, Plant_NO, Tiller_NO, Live_tiller, Appeared, Ligulated, Senesced, Plant_height, SumT, .... Observation records are missing from d until this file is standardized.
# Development_mean_C11_DS2011.csv: not read. columns: Date, DAS, SumT, Variety, Watering, Tiller_NO, _FREQ_, Minitiated, Mappeared, Mligulated, Msenesced, Mlive_tiller, .... Observation records are missing from d until this file is standardized.
# Development_mean_C11_DS2012.csv: not read. columns: Date, DAS, SumT, Variety, Tiller_NO, _FREQ_, Mappeared, Mligulated, Msenesced, Mlive_tiller, Mplant_height, Mculm_length, .... Observation records are missing from d until this file is standardized.
# Development_mean_UE_DS2012.csv: not read. columns: Date, DAS, SumT, Mappeared, Mligulated, Msenesced, Mlive_tiller, Mplant_height, Msampling_length, Mtiller_density, Mplant_density, Mapp_sen, .... Observation records are missing from d until this file is standardized.
# Development_mean_UJ_DS2011.csv: not read. columns: Field, Date, SumT, Mappeared, Mligulated, Msenesced, Mlive_tiller, Msheath_length_n, Mculm_length, Mtiller_density, Mplant_density, Msampling_length, .... Observation records are missing from d until this file is standardized.
# Development_UE_DS2012.csv: not read. columns: Date, SN, Plot_NO, Plant_NO, Tiller_NO, Sampling_length, Live_tiller, Appeared, Ligulated, Senesced, Plant_height. Observation records are missing from d until this file is standardized.
# Development_UJ_DS2011.csv: not read. columns: Date, Subsample, Plot_NO, Sampling_length, Plant_NO, Dead_tiller, Live_tiller, Tiller_NO, Appeared, Ligulated, Senesced, Plant_height, .... Observation records are missing from d until this file is standardized.
# Harvest_area_UZ1density_DS2012.csv: not read. columns: Plot_NO, Plant_distance, Sowing_date, Harvest_date, Hill_count, Panicle_count, FiGr_DW, UnFiGr_DW, KFiGr_DW, KUnFiGr_DW, MC_PC, Sampling_area, .... Observation records are missing from d until this file is standardized.
# Harvest_area_UZ1density_DS2013.csv: not read. columns: Plot_NO, Rep_NO, Plant_distance, Sampling_area, Date, Hill_NO, Panicle_count, FiGr_DW, UnFiGr_rachis_DW, UnFiGr_DW, Rachis_DW, MC_PC, .... Observation records are missing from d until this file is standardized.
# Leaf_area_mean_UZ1density_DS2012.csv: not read. columns: Plant_distance, Leaf_NO, _TYPE_, _FREQ_, Mleaf_area, Mleaf_length, Mleaf_width, MSLA, Lleaf_area, Lleaf_length, Lleaf_width, LSLA, .... Observation records are missing from d until this file is standardized.
# Leaf_area_mean_UZ1density_DS2013.csv: not read. columns: Plant_distance, Leaf_NO, _TYPE_, _FREQ_, Mleaf_area, Mleaf_length, Mleaf_width, MSLA, Lleaf_area, Lleaf_length, Lleaf_width, LSLA, .... Observation records are missing from d until this file is standardized.
# Meteo_C11_DS2011.csv: not read. columns: Date, GR, AirT, RH, SoilT, SumT_air, SumT_dry, SumT_soil, SumT. Observation records are missing from d until this file is standardized.
# Meteo_C11_DS2012.csv: not read. columns: Date, N, GR, RH, AirT, T2, T3, T4, T5, T6, SoilT, SumGR, .... Observation records are missing from d until this file is standardized.
# Meteo_UE_DS2012.csv: not read. columns: Date, AirT, SoilT, GR, SumGR, SumT. Observation records are missing from d until this file is standardized.
# Meteo_UJ_DS2011.csv: not read. columns: Date, JD, AirT, SoilT, SumT_air, SumT_soil, GR, RH, SumGR, SumT. Observation records are missing from d until this file is standardized.
# Phenology_mean_plot_UZ1density_DS2012.csv: not read. columns: Plot_NO, Plant_distance, DAS, _TYPE_, _FREQ_, Mappeared, Mligulated, Msenesced, Mlive_tiller, Mplant_height, Lappeared, Lligulated, .... Observation records are missing from d until this file is standardized.
# Phenology_mean_plot_UZ1density_DS2013.csv: not read. columns: Plot_NO, Plant_distance, SN, _TYPE_, _FREQ_, Mappeared, Mligulated, Msenesced, Mlive_tiller, Mplant_height, Date, DAS, .... Observation records are missing from d until this file is standardized.
# Phenology_mean_UZ1density_DS2012.csv: not read. columns: SN, Plant_distance, _TYPE_, _FREQ_, Mappeared, Mligulated, Msenesced, Mlive_tiller, Mplant_height, Date, DAS, SumT, .... Observation records are missing from d until this file is standardized.
# Phenology_mean_UZ1density_DS2013.csv: not read. columns: SN, Plant_distance, _TYPE_, _FREQ_, Mappeared, Mligulated, Msenesced, Mlive_tiller, Mplant_height, Date, DAS, SumT, .... Observation records are missing from d until this file is standardized.
# Sheath_mean_UZ1density_DS2012.csv: not read. columns: Plant_distance, Leaf_NO, _TYPE_, _FREQ_, Msheath_length, Lsheath_length, dsheath_length. Observation records are missing from d until this file is standardized.
# Sheath_mean_UZ1density_DS2013.csv: not read. columns: Plant_distance, Leaf_NO, _TYPE_, _FREQ_, Msheath_length, Lsheath_length, dsheath_length. Observation records are missing from d until this file is standardized.
# SLA_mean_UZ1density_DS2012.csv: not read. columns: Plant_distance, Leaf_NO, Date, SumT, _TYPE_, _FREQ_, MSLA, LSLA, dSLA. Observation records are missing from d until this file is standardized.
# SLA_mean_UZ1density_DS2013.csv: not read. columns: Plant_distance, Leaf_NO, SN, _TYPE_, _FREQ_, MSLA, Date, SumT, LSLA, dSLA. Observation records are missing from d until this file is standardized.
# Variable Dictionary.xlsx: documentation or a codebook, not an observation table.

carob_script <- function(path) {

"
Replication Data for: Leaf emergence, tillering, plant growth, and yield in response to plant density in a high-yielding aerobic rice crop

The traditional transplanted and flooded rice cropping system is threatened by water shortage and labor cost increase in many Asian countries and is currently evolving toward direct-seeded and water-saving systems. However, yield penalties generally accompany this evolution and ongoing research targets their mitigation. In a previous research, slower plant development rate was observed in aerobic crops, possibly caused by the increased plant density. This possible factor was tested in experiments conducted at IRRI's farm, Los Banos, Philippines, in 2012 and 2013. One elite variety was direct-seeded at three plant distances (6, 10, and 20 cm × 20 cm) and sprinkler-irrigated to keep the soil water potential above−10 kPa. Weekly measurements were made to dissect crop and plant physiological differences. Data from previous experiments on tillering in the greenhouse of four varieties and in fields in aerobic and flooded conditions were added to strengthen the results from the current experiment. The last upper leaves of the main stem appeared faster at higher plant density, thus increase plant density was not the cause for the slower leaf appearance in aerobic than in flooded crops. Plants at higher plant density also had earlier panicle initiation, earlier earing, and 0.8 leaf less than plants at low density. The number of outgrowing tillers was linked to the number of leaves on the main stem by a variety-specific relationship that is independent from plant density and water management. Tiller density was regulated by the cessation of the outgrowth of tillers triggered by the onset of stem elongation at 20-cm plant distance and occurred earlier at shorter distances probably in response to the high root density. Grain yields were significantly higher at higher plant density (6-cm and 10-cm plant distances) and correlated with a higher tiller density. Thus for aerobic crops, seedling density higher than 50 plants per sq meter remains the way to get high yields for the type of variety used in this study.
"

	uri <- "doi:10.7910/DVN/QDITFO"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=3,
		data_organization = "IRRI",
		publication = "doi:10.1016/j.fcr.2016.09.018",
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "plant_spacing;plant_density",
		response_vars = "yield", 
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 45,
		carob_effort = 1
	)

	harv <- function(year) {
		f1 <- ff[basename(ff) == paste0("Harvest_area_UZ1density_DS", year, ".csv")]
		r <- read.csv(f1)
		hd <- if (year == "2012") r$Harvest_date else r$Date
		data.frame(
			plot_id = as.character(r$Plot_NO),
			rep = as.integer(r$Rep_NO),
			plant_spacing = r$Plant_distance,
			harvest_date = as.character(as.Date(hd, "%m/%d/%Y")),
			yield = r$Grain14_gm2 * 10,
			spike_density = r$Panicle_density * 10000,
			seed_weight = r$KFiGr_DW_14,
			grain_fill = r$Fertility_rate * 100,
			spikelet_density_ = r$Tspikelets_density,
			grains_per_panicle_ = r$FiGr_per_panicle,
			trial_id = paste0("UZ1density_DS", year)
		)
	}
	d1 <- harv("2012")
	d1$planting_date <- "2012-01-06"
	d2 <- harv("2013")
	d2$planting_date <- "2012-12-11"
	d <- rbind(d1, d2)
	d$season <- "dry"
	d$treatment <- paste0("20x", d$plant_spacing, " cm")
	d$row_spacing <- 20
	d$plant_density <- 1e8 / (d$row_spacing * d$plant_spacing)
	d$record_id <- seq_len(nrow(d))

	d$crop <- "rice"
	d$variety <- "RC222"
	d$yield_part <- "grain"
	d$yield_moisture <- 14
	d$planting_method <- "direct seeding"
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	# abstract: sprinkler-irrigated to keep soil water potential above -10 kPa
	d$irrigated <- TRUE
	d$irrigation_method <- "sprinkler"
	d$country <- "Philippines"
	d$adm1 <- "Laguna"
	d$location <- "IRRI, Los Banos"
	d$site <- "UZ field"
	d$longitude <- as.numeric(NA)
	d$latitude <- as.numeric(NA)
	d$geo_from_source <- NA

	bio <- function(year) {
		f2 <- ff[basename(ff) == paste0("Biomass_mean_plot_UZ1density_DS", year, ".csv")]
		r <- read.csv(f2)
		data.frame(
			trial_id = paste0("UZ1density_DS", year),
			plot_id = as.character(r$Plot_NO),
			DAP = r$DAS,
			LAI = r$MLAI,
			dmy_total = r$Mshoot_gm2 * 10,
			dmy_leaves = r$Mleaf_gm2 * 10,
			dmy_stems = r$Mstem_gm2 * 10,
			dmy_panicle_ = r$Mpanicle_gm2 * 10,
			stem_density = r$Mtiller_density * 10000
		)
	}
	dlong <- rbind(bio("2012"), bio("2013"))
	dlong <- merge(d[, c("trial_id", "plot_id", "record_id")], dlong, by=c("trial_id", "plot_id"))
	dlong$trial_id <- NULL
	dlong$plot_id <- NULL

	carobiner::write_files(path, meta, d, long=dlong)
}
