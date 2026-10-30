# R script for "carob"

## NOTES
# Plot-level measured wheat observations (Bacanora vs DH) at Buenos Aires, Ciudad Obregon, and Valdivia.
# Grain yield is kg DM/ha. Irrigation events are labeled "Model_estimate_to_mitigate_water_stress"; irrigated=TRUE but amounts not used.
# Fertilizer N is urea-ammonium nitrate (FE010) summed from Fertilizer_events. Simulations/model output folders were not used.

## ISSUES
# Irrigation mm are modeled, not measured; irrigation_amount not mapped.
# P and K not in Fertilizer_events (N only); omitted.
# Some seasons have a single plot (means?) rather than three replicates.
# Daily in-season crop observations (Obs_crop_daily_*) not processed as long records.
# Weather station files have "use limited, no secondary distribution" notes; included as source weather.
# write_files: P_fertilizer and K_fertilizer not in source; plant_density/seed_density exceed vocab max (source up to 370 plants/m2).

carob_script <- function(path) {

"
Data from the AgMIP-Wheat high-yielding traits experiment for modeling potential production of wheat: field experiments and multi-model simulations

The dataset reported here was created to analyze the value of physiological traits identified by the International Wheat Yield Partnership (IWYP) to improve wheat potential in high-yielding environments. This dataset consists of 11 growing seasons at three high-yielding locations in Buenos Aires (Argentina), Ciudad Obregon (Mexico), and Valdivia (Chile) with the spring wheat cultivar Bacanora and a high-yielding genotype selected from a doubled haploid (DH) population developed from the cross between the Bacanora and Weebil cultivars from the International Maize and Wheat Improvement Center (CIMMYT). This dataset was used in the Agricultural Model Intercomparison and Improvement Project (AgMIP) Wheat Phase 4 to evaluate crop model performance when simulating high-yielding physiological traits and to determine the potential production of wheat using an ensemble of 29 wheat crop models. The field trials were managed for non-stress conditions with full irrigation, fertilizer application, and without biotic stress. Data include local daily weather, soil characteristics and initial soil conditions, cultivar information, and crop measurements (anthesis and maturity dates, total above-ground biomass, final grain yield, yield components, and photosynthetically active radiation interception). Simulations include both daily in-season and end-of-season results for 25 crop variables simulated by 29 wheat crop models. The R code and formatted data used for the statistical analyses are included.
"

	uri <- "doi:10.7910/DVN/VKWKUP"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT",
		publication = "doi:10.18174/odjar.v9i0.18573",
		project = "AgMIP-Wheat",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety;N_fertilizer",
		response_vars = "yield;dmy_total",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 70,
		carob_effort = 1.5
	)

	fobs <- ff[basename(ff) == "CIM_AgMIP_Obs_crop_summary_plots.txt"]
	ffld <- ff[basename(ff) == "CIM_AgMIP_Fields.txt"]
	fplt <- ff[basename(ff) == "CIM_AgMIP_Planting_events.txt"]
	ffrt <- ff[basename(ff) == "CIM_AgMIP_Fertilizer_events.txt"]
	fwth <- ff[basename(ff) == "CIM_AgMIP_Weather_daily.txt"]
	fwst <- ff[basename(ff) == "CIM_AgMIP_Weather_stations.txt"]

	obs <- read.table(fobs, header=TRUE, sep="\t", skip=2, check.names=FALSE)
	fld <- read.table(ffld, header=TRUE, sep="\t", skip=2, check.names=FALSE)
	plt <- read.table(fplt, header=TRUE, sep="\t", skip=2, check.names=FALSE)
	frt <- read.table(ffrt, header=TRUE, sep="\t", skip=2, check.names=FALSE)

	# Seasonal N (kg/ha) from fertilizer events; same N for Bacanora and DH within a site-year
	nsum <- aggregate(list(N_fertilizer=as.numeric(frt$FEAMN)), list(EID=frt$EID), sum, na.rm=TRUE)

	g <- fld[match(obs$EID, fld$EID), ]
	p <- plt[match(obs$EID, plt$EID), ]
	n <- nsum$N_fertilizer[match(obs$EID, nsum$EID)]

	d <- data.frame(
		trial_id = obs$EID,
		rep = as.integer(obs$RP),
		variety = obs$CUL_NAME,
		planting_date = as.character(obs$PDATE),
		emergence_date = as.character(obs$PLDAE),
		flowering_date = as.character(obs$ADAT),
		maturity_date = as.character(obs$MDAT),
		harvest_date = as.character(obs$HDATE),
		# mg/seed equals g per 1000 seeds
		seed_weight = as.numeric(obs$GWGM),
		plant_density = as.numeric(obs$PLPAE) * 10000,
		plant_height = as.numeric(obs$PHTM) * 100,
		yield = as.numeric(obs$GWAM),
		dmy_total = as.numeric(obs$CWAM),
		country = g$FL_LOC_1,
		location = gsub("_", " ", g$FL_NAME),
		latitude = as.numeric(g$FL_LAT),
		longitude = as.numeric(g$FL_LONG),
		elevation = as.numeric(g$FLELE),
		row_spacing = as.numeric(p$PLRS),
		seed_density = as.numeric(p$PLPOP) * 10000,
		N_fertilizer = n,
		crop = "wheat",
		yield_part = "grain",
		yield_moisture = 0,
		irrigated = TRUE,
		geo_from_source = TRUE,
		on_farm = FALSE,
		is_survey = FALSE
	)

	d$treatment <- d$variety

	wd <- read.table(fwth, header=TRUE, sep="\t", skip=2, check.names=FALSE)
	# Weather file prefixes map to Fields coordinates
	loc <- ifelse(grepl("BUA", wd$WST_DATASET), "Buenos Aires",
		ifelse(grepl("CIO", wd$WST_DATASET), "Ciudad Obregon", "Valdivia"))
	wmeta <- data.frame(
		location = c("Buenos Aires", "Ciudad Obregon", "Valdivia"),
		country = c("Argentina", "Mexico", "Chile"),
		latitude = c(-34.60, 27.43, -39.80),
		longitude = c(-58.50, -109.92, -73.25),
		elevation = c(26, 38, 19)
	)
	wm <- wmeta[match(loc, wmeta$location), ]
	wth <- data.frame(
		country = wm$country,
		location = wm$location,
		longitude = wm$longitude,
		latitude = wm$latitude,
		elevation = wm$elevation,
		date = as.character(wd$W_DATE),
		srad = as.numeric(wd$SRAD),
		tmax = as.numeric(wd$TMAX),
		tmin = as.numeric(wd$TMIN),
		prec = as.numeric(wd$RAIN),
		vapr = as.numeric(wd$VPRSD),
		# km/d to m/s
		wspd = as.numeric(wd$WIND) / 86.4
	)
	wth <- unique(wth)
	wth <- wth[!is.na(wth$date), ]

	carobiner::write_files(path, meta, d, wth=wth)
}
