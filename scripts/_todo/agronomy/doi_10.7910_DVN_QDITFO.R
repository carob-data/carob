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
		r <- read.csv(ff[basename(ff) == paste0("Harvest_area_UZ1density_DS", year, ".csv")])
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
		r <- read.csv(ff[basename(ff) == paste0("Biomass_mean_plot_UZ1density_DS", year, ".csv")])
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
