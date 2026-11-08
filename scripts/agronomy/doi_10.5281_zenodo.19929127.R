# R script for "carob"
# license: GPL (>=3)

## NOTES
# Three-season maize field trial (2023-2025) on an Oxisol in Siaya, western Kenya
# (PyMiCCS project): 9 treatments x 3 replicates; biochar, basanite (rock powder),
# rock-enhanced biochar and their co-application, with/without NPK fertilizer.
# Treatment N:P:K rates and amendment rates are from the dataset readme. For the amended
# treatments P and K rates changed from 0/0 (season 1) to 10/20 kg/ha (seasons 2-3).
# Root morphology and soil bulk density were measured once for the whole trial; they are
# repeated on each season (trial_id) row of the wide table.
# The associated manuscript (Meyer zu Drewer et al.) is not yet published; no DOI found
# (checked 2026-11-07).


## ignored the "per plot" and "per plant" data

## ISSUES
# - No planting or harvest dates in the data; the trial ran 2023-2025 but which
#   season fell in which year (Siaya has two cropping seasons per year) is not stated,
#   so no dates are recorded.
# - Whether the trial was on a farm or a research site, and whether it was irrigated,
#   is not stated; on_farm and irrigated are NA.
# - Yield moisture basis is not stated (grain and stover yields in t/ha).
# - Root biomass (g dry weight) and the other root morphology measures are per plant
#   (excavated root system); plot area and plant density are unknown so they cannot be
#   expressed per ha.
# - The grain/stover yields of "Root Shoot factor" rows: not clear if the factor uses
#   total biomass; kept as the reported ratio.

## NEW VARIABLES
# striga_count_       number of emerged striga plants counted per plot (plot area unknown)
# root_biomass_       root biomass per plant (g dry weight)
# root_length_        root length per plant (cm)
# root_angle_         root opening angle (degrees)
# root_shoot_ratio_   root:shoot ratio (dimensionless)
# root_area_          root surface area per plant (cm2)

## NOT PROCESSED
# - Data_Meyer_zu_Drewer_P-sorption.csv: in-vitro laboratory batch experiment
#   (P sorption in centrifuge tubes), not field data; out of scope for carob.
# - Data_Meyer_zu_Drewer_Kenya.csv, parameter "Soil pH in-situ season 1" (243 rows, two
#   depths plus the amendment application zone): recorded with replicates 1-9 per
#   treatment that cannot be linked to the 3 field plots.
# - Data_Meyer_zu_Drewer_Kenya.csv, parameters "TFP Grain/Stover" and "TPP Grain":
#   total factor / total phosphorus productivity, derived as yield divided by the
#   fertilizer input already recorded (verified, e.g. TFP 34.8 = 3130 kg / 90 kg N), redundant.
# - readme_Meyer-zu-Drewer_fin.rtf: codebook (treatment and parameter definitions), no data.


carob_script <- function(path) {

"
Data for: Effect of biochar and basanite on maize yield,
fertilizer efficiency and phosphorus sorption in a tropical Oxisol (PyMiCCS Kenya field trial)

# Field trial and in-vitro batch experiment data; Effect of biochar and 
basanite on maize yield, fertilizer efficiency and phosphorus sorption in a tropical Oxisol &nbsp; 
This dataset contains the raw experimental data underlying the manuscript 'Effect of biochar and basanite on maize yield,
fertilizer efficiency and phosphorus sorption in a tropical Oxisol' by Meyer zu Drewer, Roobroeck, Maritim, Lotz, Kammann, 
Hartmann, Broszat, Schmidt, and Hagemann.; 
The data were collected during a three-season maize field trial on an Oxisol in Siaya, western Kenya (2023 - 2025), 
and during a complementary six-week in-vitro phosphorus sorption batch experiment.
"

	uri <- "doi:10.5281/zenodo.19929127"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=6, minor=NA,
	          data_organization = "ITIN",   # Ithaka Institute
	          publication = NA,
	          project = "PyMiCCS",
	          design = NA,
	          data_type = "experiment",
	          treatment_vars = "soil_amendment;soil_amendment_type;N_fertilizer;P_fertilizer;K_fertilizer",
	          response_vars = "yield;dmy_residue",
	          notes = NA,
	          carob_contributor = "Stella Muthoni",
	          carob_LLM = "Claude Fable 5",
	          carob_date = "2026-08-30",
	          carob_completion = 95,
	          carob_effort = 3
	)
	
	f1 <- ff[basename(ff) == "Data_Meyer_zu_Drewer_Kenya.csv"]          # field trial, long format
	# Data_Meyer_zu_Drewer_P-sorption.csv and the readme are not read; see NOT PROCESSED
	
	r1 <- read.csv(f1, sep=";", skip=1)
	
	## treatment table from the readme: N:P:K rates (kg/ha) and amendment rates (t/ha).
	## Season 1 rates; for seasons 2-3 the amended treatments got P/K 10/20 instead of 0/0
	trt <- data.frame(
	  treatment = c("Zero Control", "Control", "PK-half", "PK-full", "RE-BC_5", "RE-BC_10", "Co-Ap_10", "BC_5", "RP_5"),
	  N_fertilizer = c(0, 90, 90, 90, 90, 90, 90, 90, 90),
	  P_fertilizer = c(0, 0, 10, 20, 0, 0, 0, 0, 0),
	  K_fertilizer = c(0, 0, 20, 40, 0, 0, 0, 0, 0),
	  soil_amendment_type = c("none", "none", "none", "none", "rock-enhanced biochar",
	                          "rock-enhanced biochar", "biochar; basanite", "biochar", "basanite"),
	  # t/ha -> kg/ha ("Co-Ap_10" is a 1:1 biochar:basanite mix at 10 t/ha in total)
	  soil_amendment = c(0, 0, 0, 0, 5, 10, 10, 5, 5) * 1000,
	  # biochar and basanite are soil amendments, not organic fertilizer
	  OM_used = FALSE
	)
	
	trt2 <- trt
	trt2$P_fertilizer[5:9] <- 10
	trt2$K_fertilizer[5:9] <- 20
	trt <- rbind(data.frame(season="1", trt), data.frame(season="2", trt2), data.frame(season="3", trt2))

	## season from the parameter name
	r1$season <- gsub("\\D", "", r1$parameter)

	## grain and stover yield, wide
	s <- r1[grepl("season", r1$parameter) & !grepl("Soil pH|Striga|TFP|TPP", r1$parameter), ]
	s$parameter <- gsub(" 1st season| 2nd season| 3rd season", "", s$parameter)
	s <- reshape(s, timevar="parameter", idvar=c("season", "treatment", "subgroup", "replicate"), direction="wide")

	d <- data.frame(
		treatment = s$treatment,
		rep = as.integer(s$replicate),
		trial_id = s$season,
		yield = s$`value.Grain Yield t ha` * 1000,     # t/ha -> kg/ha
		dmy_residue = s$`value.Stover Yield t ha` * 1000
	)
	d <- merge(d, trt, by.x=c("treatment", "trial_id"), by.y=c("treatment", "season"), all.x=TRUE)
	
	## soil bulk density and root morphology - measured once for the whole trial;
	## repeated on each season row (see NOTES)
	bd <- r1[r1$parameter == "Soil Bulk Density g cm3", c("treatment", "replicate", "value")]
	names(bd) <- c("treatment", "rep", "soil_bd")
	d <- merge(d, bd, by=c("treatment", "rep"), all.x=TRUE)
	
	roots <- reshape(r1[grepl("^Root", r1$parameter), c("parameter", "treatment", "replicate", "value")],
	                 timevar="parameter", idvar=c("treatment", "replicate"), direction="wide")
	roots <- data.frame(
		treatment = roots$treatment,
		rep = as.integer(roots$replicate),
		root_biomass_ = roots$`value.Root Biomass g dw`,    # g dry weight per plant
		root_length_ = roots$`value.Root length cm`,
		root_angle_ = roots$`value.Root opening angle`,
		root_shoot_ratio_ = roots$`value.Root Shoot factor`,
		root_area_ = roots$`value.Root surface area cm2`
	)
	d <- merge(d, roots, by=c("treatment", "rep"), all.x=TRUE)
	
	d$plot_id <- paste(d$trial_id, d$treatment, d$rep, sep="_")
	
	d$crop <- "maize"
	d$country <- "Kenya"
	d$adm1 <- "Siaya"
	## Georeference Siaya county (adm1) per https://carob-data.org/contribute/georeference.html
	d$longitude <- 34.2488
	d$latitude <- -0.0546
	d$geo_uncertainty <- 44931
	d$geo_source <- "GADM 4.1, adm1"
	d$geo_from_source <- FALSE
	
	d$is_survey <- FALSE
	# not stated whether the trial was on a farm or a research site, nor whether irrigated
	d$on_farm <- NA
	d$irrigated <- NA
	d$yield_part <- "grain"
	
	############################################################
	# long records: striga counts (weekly) and soil pH (by depth and method)
	############################################################
	
	## striga counts, weekly in seasons 2 and 3
	striga <- r1[grepl("^Striga count", r1$parameter), c("parameter", "treatment", "replicate", "subgroup", "value")]
	d_striga <- data.frame(
		trial_id = gsub("\\D", "", striga$parameter),
		plot_id = paste(gsub("\\D", "", striga$parameter), striga$treatment, striga$replicate, sep="_"),
		# DAP assumed as week number x 7 (counting weeks are given, not dates)
		DAP = as.integer(gsub("\\D", "", striga$subgroup)) * 7L,
		striga_count_ = striga$value
	)
	
	## ex-situ soil pH at two depths (seasons 1-2), per plot and depth (not averaged).
	## In-situ pH is not processed (replicates cannot be linked to plots, see NOT PROCESSED)
	ph <- r1[grepl("^Soil pH ex-situ", r1$parameter), c("parameter", "treatment", "replicate", "subgroup", "value")]
	d_ph <- data.frame(
		trial_id = gsub("\\D", "", ph$parameter),
		plot_id = paste(gsub("\\D", "", ph$parameter), ph$treatment, ph$replicate, sep="_"),
		# depth of the sampled layer's base: "0-15 cm" -> 15, "15-30 cm" -> 30
		depth = ifelse(grepl("15-30", ph$subgroup), 30, 15),
		soil_pH = ph$value
	)
	
	d_long <- carobiner::bindr(d_striga, d_ph)
	
	carobiner::write_files(path, meta, wide=d, long=d_long)
}
