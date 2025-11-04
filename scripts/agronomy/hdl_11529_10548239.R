# R script for "carob"

carob_script <- function(path) {

"Nutrient Omission Trials (NOTs) conducted in two zones (West Showa and Jimma) in Ethiopia in 2015 and 2016. Trials comprise six nutrient management treatments, namely Control (zero fertilizer), PK, NK, PK, NPK, NPK+Ca+Mg+Zn+B. Trials were conducted on-farm with six plots per farm. Observations include soil analysis (0-20cm), biomass and grain yields (2016)"

	uri <- "hdl:11529/10548239"
	group <- "agronomy"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication= NA,
		project="TAMASA",
		data_type= "experiment",
		response_vars = "yield",
		treatment_vars = c("N_fertilizer;P_fertilizer;K_fertilizer;Ca_fertilizer;Mg_fertilizer;Zn_fertilizer;B_fertilizer"),
		carob_contributor= "Shumirai Manzvera",
		carob_date="2024-05-16"
	)

	f <- ff[basename(ff) == "TAMASA_NOTs_Database_Ethiopia_2015_2016.xlsx"]
	r1 <- carobiner::read.excel.hdr(f, sheet = "Raw_data Harvest parameters",skip = 1, hdr = 1)
#	r2 <- carobiner::read.excel(f, sheet = "Fertilizer rates & variety")

	d <- data.frame(
		crop="maize",
		country=r1$Site.location.information_Country,
		adm1=r1$Region,
		adm3=r1$District,
		latitude=r1$Latitude.Decimal.Degrees,
		longitude=r1$Longitude.Decimal.Degrees,
		geo_from_source=TRUE,
		elevation=r1$Altitude.m.a.s.l,
		trial_id = as.character(r1$Experiment.site.code),
		treatment=r1$Experimental.design_Treatment,
		yield = r1$Grain.Yield.kg.ha,
		fwy_residue = r1$Stover.yield.kg.ha,
		variety=r1$Maize.variety.name.MVnam,
		variety_type = r1$Maize.variety.type,
		planting_date = r1$Agronomic.management_Planting.date.PLNdat,
		harvest_date = as.character(as.Date(r1$Harvest.date.HDATE, format = '%d/%m/%Y')),
		soil_pH = r1$Initial.characterization.of.soils_pH.H2O,
		soil_SOC = r1$OC.pct, 
		soil_sand= r1$pctSAND, 
		soil_silt = r1$pctSILT, 
		soil_clay = r1$pctCLAY, 
		soil_ex_acidity = r1$Exch.Acidity.cmol.kg, 
		soil_ECEC = r1$ECEC.cmol.kg, 

		soil_N = r1$N.pct, 
		soil_P_Mehlich = r1$Meh.P.ppm, 
		soil_K = r1$K.ppm, 
		soil_Na = r1$Na.cmol.kg, 
		soil_Zn = r1$Zn.ppm, 
		soil_Cu = r1$Cu.ppm, 
		soil_Mn = r1$Mn.ppm, 
		soil_Fe = r1$Fe.ppm, 
		soil_Ca = r1$Ca.ppm, 
		soil_Mg = r1$Mg.ppm,
		
		leaf_N = r1$Ear_leaf_analysis_pct.N,
		leaf_P = r1$pct.P,
		leaf_K = r1$pct.K,
		leaf_Ca = r1$pct.Ca,
		leaf_Mg = r1$pct.Mg,
		leaf_Zn = r1$ppm.Zn,
		residue_N = r1$Stover.yield.kg.ha * r1$Stover_analysis_pct.N / 100,
		residue_P = r1$Stover.yield.kg.ha * r1$pct.P.1 / 100,
		grain_N = r1$Grain_analysis_pct.N,
		grain_P = r1$pct.P.2,
		grain_K = r1$pct.K.1
	)
	
	for (i in 2:nrow(d)) {
		if (is.na(d$trial_id[i])) d$trial_id[i] <- d$trial_id[i-1]
	}
	
#		moist=r1$Grain.moisture.content.pct,

	# swap some lon/lat
	i <- which(d$longitude < 10)
	lat <- d$longitude[i]
	d$longitude[i] <- d$latitude[i]
	d$latitude[i] <- lat
	d$geo_from_source[i] <- FALSE

	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- 0
	d$Ca_fertilizer <- d$Mg_fertilizer <- d$Zn_fertilizer <- d$B_fertilizer <- 0
	
	trts <- sapply(strsplit(d$treatment, " "), \(i)i[1])
	
	d$N_fertilizer[grep("N", trts)] <- 120 
	d$P_fertilizer[grep("P", trts)] <- 40/2.29
	d$K_fertilizer[grep("K", trts)] <- 40/1.2051

	d$fertilizer_type <- ""
	d$fertilizer_type[grep("N", trts)] <- "urea"
	d$fertilizer_type[grep("P", trts)] <- paste0(d$fertilizer_type[grep("P", trts)], ";TSP")
	d$fertilizer_type[grep("K", trts)] <- paste0(d$fertilizer_type[grep("K", trts)], ";KCl")
	i <- trts == "NPK+Ca+Mg+Zn+B"
	d$Ca_fertilizer[i] <- d$Mg_fertilizer[i] <- 10
	d$Zn_fertilizer[i] <- d$B_fertilizer[i] <- 5 
	d$fertilizer_type[i] <- paste0(d$fertilizer_type[i], ";gypsum;MgSO4;ZnSO4;borax")
	i <- grep("^;", d$fertilizer_type)
	d$fertilizer_type[i] <- substr(d$fertilizer_type[i], 2, 100)
	d$fertilizer_type[d$fertilizer_type==""] <- "none"
	
	#fixing bad data
	d$planting_date[d$planting_date == "2/6//2016"] <- "2/6/2016"
	d$planting_date[d$planting_date == "30/52016"] <- "30/5/2016"
	i <- grepl("^4", d$planting_date)
	d$planting_date[i] <- as.character(as.Date(as.numeric(d$planting_date[i]), origin = "1899-12-31"))
	d$planting_date[!i] <- as.character(as.Date(d$planting_date[!i], format = "%d/%m/%Y"))
  
	d$yield_part <- "grain"
	d$on_farm <- TRUE
	
	d$irrigated <- NA
	d$is_survey <- FALSE
	
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(path, meta, d)
}

