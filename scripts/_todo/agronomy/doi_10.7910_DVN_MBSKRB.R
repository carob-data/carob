# R script for "carob"
# license: GPL (>=3)

## NOTES
# Plot-level field data (sheet "All") of 15 Moroberekan x Swarna BC2F3:4 qDTY3.2 lines, Swarna and Moroberekan
# under well-watered (WW) and drought (S) treatments in 7 experiments (13DS, 13WS E/L, 14DS E/L, 14WS, 15DS)
# at the IRRI experimental station. Methods from Grondin et al. 2018 (Rice 11:43, included pdf):
# 21-day seedlings transplanted in puddled lowland fields, 0.25 x 0.20 m, RCBD with 4 reps; 40-40-40 kg/ha NPK
# basal + 40 kg N/ha ammonium sulfate at panicle initiation; drought imposed 47-75 DAS under a rainout shelter
# (14WS only partly sheltered). E/L = early/late maturity groups that had separate drought timing.
# GY is kg/ha at 14% moisture (sheet "HI" shows GY (g/m2) = GY/10). SDW (shoot dry mass without grain, g/m2)
# converted to kg/ha as dmy_residue. Tiller number and plant height measured on 3 plants per plot.
# Sheets "HI", "%_Shoot_Reduc", "%_Tiller_Reduc" are derived from "All" and not used.

## ISSUES
# Dry seasons run mid-December to mid-April, so DS sowing may be in the previous calendar year; planting_date
#   is only the season year. Exact sowing/transplanting dates are not given.
# Tiller is assumed to be tillers per plant (hill) at maturity (mean of 3 plants); kept as new variable.
# Root variables (RLD, "Fine" and root length per layer, % shallow/deep) units are not in the data file;
#   RLD assumed cm/cm3 (paper), "Fine_*" assumed % fine roots, "Length" assumed cm per core segment. Verify.
# SDW max 9575 g/m2 (one plot) is likely a typo; left as recorded.
# CT (canopy temperature) only measured in drought plots; the measurement date(s) are not given.
# Fertilizer "40-40-40 kg/ha NPK" taken as elemental N, P, K; may be N-P2O5-K2O. Verify.
# Lysimeter (greenhouse) and gel-imaging (growth chamber) data from the paper are not in this dataset.
# Duke University (co-author institute) not in the data_organization vocabulary; only IRRI used.
# write_files remaining: missing harvest_date; NA yield (1 record); unknown (new) variables.

## NEW VARIABLES
# tillers_plant_: number of tillers per plant at maturity
# canopy_temperature_: canopy temperature (C), drought plots only
# maturity_group_: E (early) or L (late) maturity group, which determined the timing of the drought treatment
# root_length_density_0_30_ and root_length_density_30_60_: root length density (cm/cm3) at 0-30 and 30-60 cm
# fine_root_0_30_ and fine_root_30_60_: fine roots (%), 0-30 and 30-60 cm
# root_length_0_30_ and root_length_30_60_: root length (cm) per soil core, 0-30 and 30-60 cm
# root_shallow_ and root_deep_: % of root length at 0-30 and 30-60 cm


carob_script <- function(path) {

"
Physiological mechanisms contributing to the QTL qDTY3.2 effects on improved performance of rice Moroberekan x Swarna BC2F3:4 lines under drought

Traditional rice (Oryza sativa) varieties are valuable resources for the improvement of drought resistance.
qDTY3.2 is a drought-yield quantitative trait locus that was identified in a population derived from the traditional variety
Moroberekan and the drought-susceptible variety Swarna. In this study, our aim was to characterize the physiological
mechanisms associated with qDTY3.2. Our approach was to phenotype fifteen BC2F3:4 lines for shoot and root drought
resistance-related traits as compared to Swarna in the field under well-watered and drought stress conditions. Four
BC2F3:4 lines contrasting for yield under drought were selected for detailed characterization of shoot morphology,
water use related traits, flowering time and root system architecture in the field as well as in controlled environments
(lysimeters in a greenhouse, and gel imaging platform in a growth chamber).
"

	uri <- "doi:10.7910/DVN/MBSKRB"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IRRI",
		publication = "doi:10.1186/s12284-018-0234-1",
		project = NA,
		design = "RCBD; 4 replications",
		data_type = "experiment",
		treatment_vars = "variety;stress",
		response_vars = "yield;dmy_residue;flowering_days",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 80,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "MoroxSwarna_RawData_All.xlsx"]
	r <- carobiner::read.excel(f, sheet="All")

	d <- data.frame(
		trial_id = paste0(r$Season, "_", r$Trt),
		season = ifelse(grepl("DS", r$Season), "dry", "wet"),
		planting_date = paste0("20", substr(r$Season, 1, 2)),
		maturity_group_ = ifelse(grepl("\\(E\\)", r$Season), "E", ifelse(grepl("\\(L\\)", r$Season), "L", NA)),
		plot_id = as.character(r$Plot),
		variety = trimws(r$Designation),
		treatment = r$Trt,
		stress = ifelse(r$Trt == "S", "drought", "none"),
		yield = r$GY,
		tillers_plant_ = r$Tiller,
		canopy_temperature_ = r$CT,
		flowering_days = r$DTF,
		dmy_residue = r$SDW * 10,
		root_length_density_0_30_ = r$RLD_0_30,
		root_length_density_30_60_ = r$RLD_30_60,
		fine_root_0_30_ = r$Fine_0_30,
		fine_root_30_60_ = r$Fine_30_60,
		root_length_0_30_ = r[["Length 0-30"]],
		root_length_30_60_ = r[["Length 30-60"]],
		root_shallow_ = r[["%shallow root"]],
		root_deep_ = r[["%deep roots"]]
	)

	d$crop <- "rice"
	d$yield_part <- "grain"
	d$yield_moisture <- 14
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- TRUE
	d$flooded <- d$stress == "none"
	d$planting_method <- "transplanting"
	d$transplanting_days <- 21
	d$row_spacing <- 25
	d$plant_spacing <- 20

	# Grondin et al. 2018: 40-40-40 kg/ha NPK basal + 40 kg N/ha as ammonium sulfate (21% N, 24% S)
	d$N_fertilizer <- 80
	d$P_fertilizer <- 40
	d$K_fertilizer <- 40
	d$S_fertilizer <- 40 / 0.21 * 0.24
	d$fertilizer_type <- "NPK; DAS"

	d$country <- "Philippines"
	d$adm1 <- "Laguna"
	d$location <- "IRRI, Los Banos"
	# paper: 14 11' N, 121 15' E
	d$latitude <- 14 + 11/60
	d$longitude <- 121.25
	d$geo_from_source <- TRUE

	carobiner::write_files(path, meta, d)
}
