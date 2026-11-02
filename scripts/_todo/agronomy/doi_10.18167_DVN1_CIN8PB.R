# R script for "carob"

## NOTES
# Measured maize calibration observations at five SSA sites (two seasons each). Grain yield and biomass are t DM/ha.
# N rates are from Falconnier et al. 2020 (GCB) Table 1 (field N, not the 0/80/160 kg/ha model scenarios).
# Coordinates, planting/anthesis/maturity dates, and rainfed management are from the per-site Field-mgmt xlsx files.
# Model scenario files and 0/80/160 N treatments are not in the observation tables; not processed.

## ISSUES
# Variety names are coded only as site_CUL (OPV, Ethiopia hybrid); true cultivar names not in the files.
# P and K not reported except Ghana experiment names "80N_30P" (P treated as 30 kg P/ha elemental; confirm vs P2O5).
# Mali has "decomposed crop residues" organic amendment with no amount; OM_amount not mapped.
# Daily LAI, soil water/N layers, and Weather sheets in the xlsx files are not processed.
# In-season crop N (CroN-ma) and P (CroP-ma) not mapped.
# write_files: P_fertilizer/K_fertilizer NA except Benin (0) and Ghana P=30.

carob_script <- function(path) {

"
Calibration data for: Modelling climate change impacts on maize yields under low nitrogen input conditions in sub-Saharan Africa

The dataset contains experimental data on maize growth for crop model calibration, at five sites in sub-Saharan Africa. The observed climate data for the experimental years at the sites is provided, along with basic soil characteristics. Crop management (planting date, date of field operations) is also described, as welll as initial conditions prior to planting. Eventually, observations of in-season leaf area index (when available), soil water (and N when available), aboveground biomass and grain yield are harvest are provided.
"

	uri <- "doi:10.18167/DVN1/CIN8PB"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = "doi:10.1111/gcb.15261",
		project = "AgMIP",
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer",
		response_vars = "yield;dmy_total;LAI",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 60,
		carob_effort = 1.2
	)

	f <- ff[basename(ff) == "AgMIP_Smallholder_Observation_summary_average.txt"]
	r <- read.table(f, header=TRUE, sep="\t", check.names=FALSE)
	r$Yield[r$Yield == "na"] <- NA
	r$`Biom-ma`[r$`Biom-ma` == "na"] <- NA
	r$MaxLAI[r$MaxLAI == "na"] <- NA

	# Site metadata from Field-mgmt xlsx (coords) and Falconnier et al. 2020 (N rates)
	sites <- data.frame(
		Site.name = c("BEOU", "ETBA", "GHKP", "MANT", "RWBU"),
		country = c("Benin", "Ethiopia", "Ghana", "Mali", "Rwanda"),
		location = c("Ouri Yori", "Bako", "Kpong", "Ntarla", "Bugesera"),
		latitude = c(10.82, 9.12, 6.16, 12.58, -2.35),
		longitude = c(1.07, 37.1, 0.06, -5.7, 30.27),
		elevation = c(213, 1850, 22, 302, 1400),
		N_fertilizer = c(0, 87, 80, 85, 64),
		variety_type = c("OPV", "hybrid", "OPV", "OPV", "OPV")
	)

	s <- sites[match(r$Site.name, sites$Site.name), ]

	d <- data.frame(
		trial_id = r$Experiment.name,
		crop = "maize",
		country = s$country,
		location = s$location,
		latitude = s$latitude,
		longitude = s$longitude,
		elevation = s$elevation,
		geo_from_source = TRUE,
		planting_date = r$Planting.date,
		flowering_date = r$Ant.date,
		maturity_date = r$Mat.date,
		harvest_date = r$Mat.date,
		yield = as.numeric(r$Yield) * 1000,
		dmy_total = as.numeric(r$`Biom-ma`) * 1000,
		LAI = as.numeric(r$MaxLAI),
		N_fertilizer = s$N_fertilizer,
		variety_type = s$variety_type,
		yield_part = "grain",
		yield_moisture = 0,
		irrigated = FALSE,
		on_farm = FALSE,
		is_survey = FALSE
	)

	# Ghana experiment names include 30P; Benin had no fertilizer
	d$P_fertilizer <- ifelse(d$country == "Ghana", 30, ifelse(d$country == "Benin", 0, NA))
	d$K_fertilizer <- ifelse(d$country == "Benin", 0, NA)

	carobiner::write_files(path, meta, d)
}
