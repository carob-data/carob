# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Only NZ_AgMIP_dataset.xlsx is read. The .txt files are display-rounded exports of the same
#    sheets (verified cell by cell) and are ignored.
# 2. Harvest_events: Wakanui treatments (FARwak2016-2018) are coded "FARlee201x..." in TREAT_ID;
#    corrected to "FARwak201x..." to match Treatments/Planting_events/Plots.
# 3. Obs_crop_daily_plots: two PLTID errors -- "FARlee2014100..4..405" (missing "F") and plot 310 in
#    FARwak2018 labelled EM150 although Plots sheet says LM150. Time records are therefore linked to
#    plots by trial + plot number (EID + PLOTno), not by PLTID/TREAT_ID.
# 4. Obs_crop_summary_means and Obs_crop_daily_means are treatment means of the plot sheets and are
#    not used (except phenology dates, which exist only at treatment level). Daily_means also holds
#    leaf area per leaf rank (LAL1D-LAL19D, <10 values each) that is not in the plot sheet -- dropped.
# 5. Soil profile values are partly defaults/estimates (Comments sheet: pH set to 7 and C:N to 10 when
#    missing; SOC estimated from SOM). Soil chemistry is therefore not included; only texture.

carob_script <- function(path) {

"
Winter wheat yield potential experiment (NZ2020). Six on-farm, researcher-managed trials of cv. Wakanui at
Leeston (2013-2015) and Wakanui (2016-2018), Mid Canterbury, New Zealand, testing sowing date (February,
early March, late March, April) and target plant density (50-250 plants/m2). Split-plot, 4 replicates.
Data organised in the AgMIP/ICASA field-experiment template (v3.2) by the Foundation for Arable Research
and Plant & Food Research. Includes final harvest components, in-season growth records and daily weather.
"

	uri <- "doi:10.7910/DVN/XA4VA2"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "INRAE; New Zealand Institute for Plant & Food Research; Technical University of Munich; UIA; Foundation for Arable Research; UFL; CIMMYT; Pir Mehr Ali Shah Arid Agriculture University; IASU; MSU; National Institute of Agricultural Research (INIA); University of Göttingen; PURDUE; Chinese Academy of Science, Beijing; UOR; ZALF; UFL; UFL; UHoh; CSIRO; ZALF; AARU; Natural Resources Institute Finland (Luke), Helsinki; Helmholtz Zentrum München-German Research Center for Environmental Health; Technic University of Madrid; ROTH; MSU; Wageningen University; Chinese Academy of Science; Zhejiang University; HEBAU; HEBAU",
		publication = NA,
		project = "20t/ha by 2020 programme",
		design = "split plot; 4 replicates",
		data_type = "experiment",
		treatment_vars = "planting_date;plant_density",
		response_vars = "yield;dmy_total;harvest_index",
		notes = NA,
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2026-10-19",
		carob_completion = 100,
		carob_effort = 7
		)

	f <- ff[basename(ff) == "NZ_AgMIP_dataset.xlsx"]

	## row 3 of each sheet holds the ICASA codes used as column names
	r1 <- carobiner::read.excel(f, sheet = "Fields", skip = 2)
	r2 <- carobiner::read.excel(f, sheet = "Treatments", skip = 2)
	r3 <- carobiner::read.excel(f, sheet = "Planting_events", skip = 2)
	r4 <- carobiner::read.excel(f, sheet = "Harvest_events", skip = 2)
	r5 <- carobiner::read.excel(f, sheet = "Residue", skip = 2)
	r6 <- carobiner::read.excel(f, sheet = "Obs_crop_summary_means", skip = 2)
	r7 <- carobiner::read.excel(f, sheet = "Obs_crop_summary_plots", skip = 2)
	r8 <- carobiner::read.excel(f, sheet = "Fertilizer_events", skip = 2)
	r9 <- carobiner::read.excel(f, sheet = "Irrigation_events", skip = 2)
	r10 <- carobiner::read.excel(f, sheet = "Obs_crop_daily_plots", skip = 2)
	r11 <- carobiner::read.excel(f, sheet = "Weather_stations", skip = 2)
	r12 <- carobiner::read.excel(f, sheet = "Weather_daily", skip = 2)

	r4$TREAT_ID <- sub("^FARlee(201[678])", "FARwak\\1", r4$TREAT_ID)   # ISSUE 3

	#plot-level harvest data
	d <- data.frame(
		trial_id = r7$EID,
		TREAT_ID = r7$TREAT_ID,
		plot_id = as.character(r7$PLOTno),
		rep = as.integer(r7$RP),
		crop = "wheat",
		variety = "Wakanui",
		yield = r7$GWAM,
		yield_part = "grain",
		yield_moisture = 0,
		yield_isfresh = FALSE,
		dmy_total = r7$CWAM,
		dmy_stems = r7$SWAM,
		dmy_leaves = r7$LWAM,
		harvest_index = r7$HIAM
		#grain_weight = r7$GWGM   # mg/grain == g/1000 grains
	)

	## treatment information
	treat <- data.frame(
		TREAT_ID = r2$TREAT_ID,
		treatment = r2$TRT_NAME,
		irrigated = r2$IRRIG == "Y"
	)
	d <- merge(d, treat, by = "TREAT_ID", all.x = TRUE)

	## planting
	pl <- data.frame(
		TREAT_ID = r3$TREAT_ID,
		planting_date = as.character(as.Date(r3$PDATE)),
		plant_density = r3$PLPOP * 10000,   # plants/m2 -> plants/ha
		row_spacing = r3$PLRS,              # cm
		plot_area = r3$PLTA                 # m2
	#planting_depth = r3$PLDP / 10     # mm -> cm
		
	)
	d <- merge(d, pl, by = "TREAT_ID", all.x = TRUE)

	# harvest date and phenology (treatment level only)
	d$harvest_date <- as.character(as.Date(r4$HADAT[match(d$TREAT_ID, r4$TREAT_ID)]))
	i <- match(d$TREAT_ID, r6$TREAT_ID)
	d$flowering_date <- as.character(as.Date(r6$ADAT[i]))
	d$maturity_date <- as.character(as.Date(r6$MDAT[i]))

	## location
	d$country <- "New Zealand"
	d$adm1 <- "Canterbury"   
	i <- match(d$trial_id, r1$EID)
	d$location <- r1$FL_LOC_3[i]
	d$longitude <- r1$FL_LONG[i]
	d$latitude <- r1$FL_LAT[i]
	d$elevation <- r1$FLELE[i]
	d$geo_from_source <- TRUE
	d$on_farm <- TRUE        
	d$is_survey <- FALSE

	#soil (ISSUE 6) and previous crop (ISSUE 10)
	d$soil_texture <- "clay loam"
	d$previous_crop <- tolower(r5$PC_NOTES[match(d$trial_id, r5$EID)])

	#fertilizer (experiment level)
	r8$FEDATE <- as.Date(r8$FEDATE)
	ftype <- c(FE005 = "urea", FE003 = "Ammonium nitrate sulfate")   
	fs <- lapply(split(r8, r8$EID), function(x) {
		x <- x[order(x$FEDATE), ]
		data.frame(
			trial_id = x$EID[1],
			N_fertilizer = sum(x$FEAMN),
			N_splits = nrow(x),
			fertilizer_date = paste(x$FEDATE, collapse = ";"),
			fertilizer_type = paste(unique(ftype[x$FECD]), collapse = ";")
		)
	})
	d <- merge(d, do.call(rbind, fs), by = "trial_id", all.x = TRUE)
	d$P_fertilizer <- d$K_fertilizer <- NA

	#irrigation (experiment level)
	r9$IDATE <- as.Date(r9$IDATE)
	is <- lapply(split(r9, r9$EID), function(x) {
		x <- x[order(x$IDATE), ]
		data.frame(
			trial_id = x$EID[1],
			irrigation_number = nrow(x),
			irrigation_amount = sum(x$IRVAL),   # mm
			irrigation_dates = paste(x$IDATE, collapse = ";"),
			irrigation_method = "sprinkler"     # IR004
		)
	})
	d <- merge(d, do.call(rbind, is), by = "trial_id", all.x = TRUE)
	d$irrigation_number[!d$irrigated] <- 0L
	d$irrigation_amount[!d$irrigated] <- 0

	d$TREAT_ID <- NULL
	d$record_id <- seq_len(nrow(d))

	#in-season time records (plot level)
	key_d <- paste(d$trial_id, d$plot_id)
	key_x <- paste(r10$EID, r10$PLOTno)        # ISSUE 4
	tr <- data.frame(
		record_id = d$record_id[match(key_x, key_d)],
		date = as.character(as.Date(r10$date)),
		dmy_total = r10$CWAD,
		dmy_stems = r10$SWAD,
		LAI = as.numeric(r10$LAID),
		NDVI = r10$NDVI
	)
	tr <- tr[rowSums(!is.na(tr[, -(1:2)])) > 0, ]   # drop dates with only unmapped variables (ISSUE 7)

	#weather
	wid <- substr(r12$WST_DATASET, 1, 6)
	i <- match(wid, r11$WST_ID)
	wth <- data.frame(
		location = r11$WST_SITE[i],
		longitude = r11$WST_LONG[i],
		latitude = r11$WST_LAT[i],
		elevation = r11$WST_ELEV[i],
		date = as.character(as.Date(r12$W_DATE)),
		srad = r12$SRAD,        # MJ/m2/d
		tmax = r12$TMAX,
		tmin = r12$TMIN,
		prec = r12$RAIN,        # mm/d
		vapr = r12$VPRSD,       # kPa
		wspd = r12$WIND / 86.4  # km/d -> m/s
	)
  
	wth <- unique(wth)
	
	carobiner::write_files(path, meta, d, long = tr, wth = wth)
}
