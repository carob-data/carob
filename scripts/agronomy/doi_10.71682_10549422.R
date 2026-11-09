	uri <- "doi:10.71682/10549422"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT; AARU",
		publication = NA,
		project = NA,
		carob_date = "2026-05-15",
		design = "split plot (N rate main plot, variety split plot)",
		data_type = "experiment",
		treatment_vars = "N_fertilizer;variety;nitrification_inhibitor",
		response_vars = "NO3_trapped;NH4_trapped;flux_N2O;flux_CO2;soil_NO3;soil_NH4",
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_LLM = "Claude Fable 5",
		carob_completion = 90,
		carob_effort = 4
	)

	f1 <- ff[basename(ff) == "Leaching_Batan_2024_BNI.xlsx"]
	f2 <- ff[basename(ff) == "Leaching_Obregon_2025_BNI.xlsx"]
	f3 <- ff[basename(ff) == "Min_N_Batan_2024-BNI.xlsx"]
	f4 <- ff[basename(ff) == "Min_N_Obregon_2025_BNI.xlsx"]
	f5 <- ff[basename(ff) == "N2O_El_Batan_2024_BNI.xlsx"]
	f6 <- ff[basename(ff) == "N2O_Obregon_2025_BNI.xlsx"]

	r1 <- carobiner::read.excel(f1, sheet="leaching_Batan_2024-BNI")
	r2 <- carobiner::read.excel(f2, sheet="Leaching_Obregon_2025_BNI ")
	r3 <- carobiner::read.excel(f3, sheet="MIN N Batan from dataset")
	r4 <- carobiner::read.excel(f4, sheet="Min N Obregon-2025-BNI")
	r5 <- carobiner::read.excel(f5, sheet="Batan_2024_N2O")
	r6 <- carobiner::read.excel(f6, sheet="Obregon_2025_N2O")

	### plots: one row per location x trial (ENSAYO) x plot.
	### plot numbers are reused between trials 904 and 905, so "trial" must be a key
	p1 <- data.frame(location="El Batan", trial=r1$Ensayo, plot=r1$`Plot number`,
		rep=r1$Replication, N=r1$`N rate (kg/ha)`, bni=r1$`BNI component`,
		variety=ifelse(r1$`BNI component` == "+BNI", paste(r1$Variety, "+ Lr#n-SA"), r1$Variety))
	p2 <- data.frame(location="Obregon", trial=r2$ENSAYO, plot=r2$`Plot number`,
		rep=r2$Replication, N=r2$`N level`, bni=r2$`BNI component`,
		variety=ifelse(r2$`BNI component` == "+BNI", paste(r2$Variety, "+ Lr#n-SA"), r2$Variety))
	p3 <- data.frame(location="El Batan", trial=r3$ENSAYO, plot=r3$`PLOT NUMBER`,
		rep=r3$REPLICATION, N=r3$`N RATE (kg/ha)`, bni=r3$`BNI COMPONENT`,
		variety=ifelse(r3$`BNI COMPONENT` == "+BNI", paste(r3$VARIETY, "+ Lr#n-SA"), r3$VARIETY))
	p4 <- data.frame(location="Obregon", trial=r4$ENSAYO, plot=r4$`PLOT NUMBER`,
		rep=r4$REPLICATION, N=r4$`N RATE (kgN ha)`, bni=r4$`BNI COMPONENT`,
		variety=ifelse(r4$`BNI COMPONENT` == "+BNI", paste(r4$VARIETY, "+ Lr#n-SA"), r4$VARIETY))
	# some chamber plots occur only in the N2O files (at Obregon these include
	# Vorobey and Borlaug plots of the 12-variety field); rep is unknown there.
	# variety numbers per the "Varieties" sheet; even numbers carry the BNI
	# translocation (+ Lr#n-SA)
	vmap <- c("1"="Sonora-64", "2"="Sonora-64 + Lr#n-SA", "3"="Vorobey", "4"="Vorobey + Lr#n-SA",
		"5"="Borlaug", "6"="Borlaug + Lr#n-SA", "7"="Roelfs", "8"="Roelfs + Lr#n-SA",
		"9"="Quaiu #1", "10"="Quaiu #1 + Lr#n-SA", "11"="Navojoa", "12"="Navojoa + Lr#n-SA")
	p5 <- data.frame(location="El Batan", trial=r5$ENSAYO, plot=r5$Plot_No_Act,
		rep=NA, N=r5$`N-fertilizer rate`,
		bni=ifelse(r5$ENSAYO == 905, "+SNI", ifelse(r5$`Variety number` %% 2 == 0, "+BNI", "-BNI")),
		variety=vmap[as.character(r5$`Variety number`)])
	p6 <- data.frame(location="Obregon", trial=r6$ENSAYO, plot=r6$Plot_No_Act,
		rep=NA, N=r6$`N-fertilizer rate`,
		bni=ifelse(r6$ENSAYO == 905, "+SNI", ifelse(r6$`Variety number` %% 2 == 0, "+BNI", "-BNI")),
		variety=vmap[as.character(r6$`Variety number`)])

	plots <- unique(rbind(p1, p2, p3, p4, p5, p6))
	# where a plot occurs with and without a known rep, keep the known rep
	plots <- plots[order(plots$location, plots$trial, plots$plot, is.na(plots$rep)), ]
	plots <- plots[!duplicated(plots[, c("location", "trial", "plot")]), ]

	d <- data.frame(
		location = plots$location,
		trial_id = paste0(ifelse(plots$location == "El Batan", "Batan_", "Obregon_"), plots$trial),
		plot_id = as.character(plots$plot),
		rep = as.integer(plots$rep),
		treatment = plots$bni,
		variety = plots$variety,
		# "+SNI": synthetic nitrification inhibitor applied to the non-BNI variety
		nitrification_inhibitor = plots$bni == "+SNI",
		N_fertilizer = plots$N,
		# abstract: N applied as ammonium sulfate
		fertilizer_type = ifelse(plots$N > 0, "DAS", "none"),
		planting_date = ifelse(plots$location == "El Batan", "2024-06", "2024-12")
	)
	d$record_id <- as.integer(seq_len(nrow(d)))

	### leaching: season total NO3-N (and NH4-N, Obregon) trapped by resin at 60 cm;
	### mean of the resin subsamples per plot (kg/ha)
	l1 <- aggregate(list(NO3_trapped = r1$`Total NO3-N kg/ha`),
		list(trial_id = paste0("Batan_", r1$Ensayo), plot_id = as.character(r1$`Plot number`)), mean, na.rm=TRUE)
	l2 <- aggregate(list(NO3_trapped = r2$`Total NO3 N kg/ha`, NH4_trapped = r2$`Total NH4 N kg /ha`),
		list(trial_id = paste0("Obregon_", r2$ENSAYO), plot_id = as.character(r2$`Plot number`)), mean, na.rm=TRUE)
	leach <- carobiner::bindr(l1, l2)
	d <- merge(d, leach[, c("trial_id", "plot_id", "NO3_trapped", "NH4_trapped")],
		by=c("trial_id", "plot_id"), all.x=TRUE)

	# "Harvesting Date" in the Obregon leaching file; the Batan file only has a resin
	# sampling date (2024-11-01), which is not necessarily the crop harvest
	d$harvest_date <- ifelse(d$location == "Obregon", "2025-05-08", NA)

	### soil mineral N (mg/kg dry soil), repeated samplings; to the long table
	s3 <- data.frame(
		location = "El Batan",
		trial = r3$ENSAYO,
		plot_id = as.character(r3$`PLOT NUMBER`),
		date = as.character(as.Date(r3$`SAMPLING DATE`)),
		depth_top = as.numeric(sub("-.*", "", r3$`SOIL DEPTH (cm)`)),
		depth_bottom = as.numeric(sub(".*-", "", r3$`SOIL DEPTH (cm)`)),
		sample_position_ = c(B="bed", F="furrow")[r3$`FURROW/BED`],
		soil_NO3 = r3$`NO3-N  mg/kg DW`,
		soil_NH4 = r3$`NH4-N  mg/kg DW`
	)
	# Obregon sampling dates are a mix of d/m/Y strings and excel date numbers
	dt <- r4$`SAMPLING DATE`
	i <- grepl("/", dt)
	dt[i] <- as.character(as.Date(dt[i], "%d/%m/%Y"))
	dt[!i] <- as.character(as.Date(as.numeric(dt[!i]), origin="1899-12-30"))
	s4 <- data.frame(
		location = "Obregon",
		trial = r4$ENSAYO,
		plot_id = as.character(r4$`PLOT NUMBER`),
		date = dt,
		depth_top = as.numeric(sub("-.*", "", r4$`SOIL DEPTH (cm)`)),
		depth_bottom = as.numeric(sub(".*-", "", r4$`SOIL DEPTH (cm)`)),
		sample_position_ = c(B="bed", F="furrow")[r4$`FURROW/BED`],
		soil_NO3 = as.numeric(r4$`mg NO3-N/kg DW`),
		soil_NH4 = as.numeric(r4$`mg NH4-N /kgDW`)
	)

	### N2O/CO2 chamber fluxes; one row per chamber x date
	gas <- function(r, location, datefun) {
		# QA/QC: M = missing (excluded); Z = noise, use 50% of the minimal detectable
		# flux for N2O (instruction in the file metadata); NA ReEval treated as missing
		r <- r[!is.na(r$ReEval) & r$ReEval != "M", ]
		fN2O <- ifelse(r$ReEval == "Z", 0.5 * r$MDF_N2O, r$LM.flux_N2O)
		sm <- rowMeans(r[, grep("^Soil_moisture", names(r))], na.rm=TRUE)
		st <- rowMeans(r[, grep("^Soil_temp", names(r))], na.rm=TRUE)
		data.frame(
			location = location,
			trial = r$ENSAYO,
			plot_id = as.character(r$Plot_No_Act),
			date = datefun(r$DATE),
			chamber_no_ = r$Chamber_No,
			# nmol N2O/m2/s -> mg N2O/m2/h
			flux_N2O = fN2O * 44.013 * 3.6 / 1000,
			# umol CO2/m2/s -> mg C/m2/h (terminag unit is mg C, not mg CO2)
			flux_CO2 = r$LM.flux_CO2 * 12.011 * 3.6,
			# TDR volumetric % -> cm3/cm3
			soil_VWC = sm / 100,
			soil_temp = st
		)
	}
	g5 <- gas(r5, "El Batan", function(x) as.character(as.Date(x)))
	g6 <- gas(r6, "Obregon", as.character)

	long <- carobiner::bindr(s3, s4, g5, g6)
	long$trial_id <- paste0(ifelse(long$location == "El Batan", "Batan_", "Obregon_"), long$trial)
	long <- merge(long, d[, c("trial_id", "plot_id", "record_id")], by=c("trial_id", "plot_id"), all.x=TRUE)
	long <- long[, !names(long) %in% c("location", "trial", "trial_id", "plot_id")]

	### georeference: station locations (estimated from station names, not in the data)
	geo <- data.frame(
		location = c("El Batan", "Obregon"),
		# CIMMYT El Batan station, Texcoco; CENEB station, Ciudad Obregon
		longitude = c(-98.846, -109.930),
		latitude = c(19.533, 27.370),
		geo_uncertainty = 2000
	)
	d <- merge(d, geo, by="location", all.x=TRUE)

	d$country <- "Mexico"
	d$adm1 <- ifelse(d$location == "El Batan", "México", "Sonora")
	d$geo_from_source <- FALSE
	d$crop <- "wheat"
	d$is_survey <- FALSE
	# both sites are CIMMYT experiment stations
	d$on_farm <- FALSE
	d$irrigated <- NA
	d$yield_part <- "none"   # no yield data in this dataset
	d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

	carobiner::write_files(path, meta, d, long=long)
