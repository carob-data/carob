# R script for "carob"

carob_script <- function(path) {

"Nutrient Omission Trials (NOTs) from three states in Nigeria. Six treatments, yield, soil and ear leaf, stover and grain nutrient contents (2016)"

	uri <- "hdl:11529/10548238"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)
   
	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT",
		publication= "doi:10.1016/j.fcr.2019.107585",
		project=NA,
		data_type= "experiment",
		response_vars = "yield",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;S_fertilizer;Ca_fertilizer;Zn_fertilizer;B_fertilizer;variety",
		carob_contributor= "Cedric Ngakou",
		carob_date="2024-06-06"
	)

	r0 <- carobiner::read.excel(ff[basename(ff)=="TAMASA_NOTs_Database_Nigeria_2015_2016.xlsx"], sheet="Fertilizer rates & variety", skip=1)
	r1 <- carobiner::read.excel(ff[basename(ff)=="TAMASA_NOTs_Database_Nigeria_2015_2016.xlsx"], fix_names = TRUE, sheet="Raw_Data_2015", skip=1)
	r2 <- carobiner::read.excel(ff[basename(ff)=="TAMASA_NOTs_Database_Nigeria_2015_2016.xlsx"], fix_names = TRUE, sheet="Raw_Data_2016", skip=1)

	# remove summary stats
	i <- which(is.na(r1$Country))-1
	r1 <- r1[1:i, ]
	r1$Plot.number.PLOTnumb <- NULL
	r1 <- unique(r1)
	
	# remove empty rows
	r2 <- r2[!is.na(r2$Country), ]
   
	## fertilizer
	d0 <- data.frame(
		country= "Nigeria",
		adm2= r0$LGAs,
		N_fertilizer = r0$`N (kg/ha)`,
		P_fertilizer = r0$`P2O5 (kg/ha)`/(2.29),
		K_fertilizer = r0$`K2O (kg/ha)`/(1.2051),
		S_fertilizer = r0$`S (kg/ha)`,
		Ca_fertilizer= r0$`Ca (kg/ha)`,
		Zn_fertilizer= r0$`Zn (kg/ha)`,
		Mg_fertilizer= r0$`Mg (kg/ha)`,
		B_fertilizer= r0$`B (kg/ha)`
	)

	fd <- data.frame(
		nutrient = r0$Nutrient,
		fertilizer_type = r0$`Ferilizer source`
	) |> na.omit()
	
	### season1 
	
	d1 <- data.frame(
		adm1= r1$State,
		adm2= r1$LGA,
		location= r1$Community.village,
		latitude= r1$GPS.Coordinate.Latitude,
		longitude= r1$GPS.Coordinate.Longitude,
		geo_from_source=TRUE,
		elevation= r1$GPS.Coordinate.Altitude,
		previous_crop= r1$Crops.grown.in.the.past.two.years,
		treatment= r1$Treatment,
		variety_type= r1$Maize.variety.type,
		variety= r1$Maize.variety.name.MVnam,
		plot_area= r1$Size.of.the.net.plot.m.x.m,
		yield= r1$Grain.Yield.kg.ha,
		fwy_residue= r1$Stover.Yield.kg.ha,
		dmy_total= r1$Above.ground.biomass.t.ha * 1000, ## to kg/ha

	## soil data 
		soil_pH= r1$pH.Water.1.1,
		soil_SOC= r1$OC.pct,
		soil_N= r1$N.pct,
		soil_P_Mehlich= r1$MehP.ppm,
		soil_sand= r1$Sand.pct,
		soil_clay= r1$Clay.pct,
		soil_silt = r1$Silt.pct,
		soil_Ca= r1$Ca.cmol.kg,
		soil_Mg= r1$Mg.cmol.kg,
		soil_K= r1$K.cmol.kg,
		soil_Na= r1$Na.cmol.kg,
		soil_CEC= r1$Exch.Acidity.cmol.kg,
		soil_ECEC= r1$ECEC.cmol.kg,
		soil_Zn= r1$Zn.ppm,
		soil_Cu= r1$Cu.ppm,
		soil_Mn= r1$Mn.ppm,
		soil_Fe= r1$Fe.ppm,
		soil_B= r1$B.ppm,
		 soil_S= r1$S.pmm, 

	### nutrient content 
		leaf_N= r1$pct.N.73,
		leaf_P= r1$pct.P.74,
		leaf_K= r1$pct.K.75,
		leaf_Ca= r1$pct.Ca.76,
		leaf_Mg= r1$pct.Mg.77,
		leaf_Cu= r1$ppm.Cu.79,
		leaf_Fe= r1$ppm.Fe.80,
		leaf_Mn= r1$ppm.Mn.81,
		leaf_Zn= r1$ppm.Zn.82,
		leaf_B= r1$ppm.B,
		residue_N= r1$pct.N.84,
		residue_P= r1$pct.P.85,
		residue_K= r1$pct.K.86,
		residue_Ca= r1$pct.Ca.87,
		residue_Mg= r1$pct.Mg.88,
		residue_Na= r1$ppm.Na.99,
		residue_Mn= r1$ppm.Mn.90,
		residue_Cu= r1$ppm.Cu.91,
		residue_Fe= r1$ppm.Fe.92,
		residue_Zn= r1$ppm.Zn.93,
		grain_N= r1$pct.N.94,
		grain_P= r1$pct.P.95,
		grain_K= r1$pct.K.96,
		grain_Ca= r1$pct.Ca.97,
		grain_Mg= r1$pct.Mg.98,
		grain_Na= r1$ppm.Na.99,
		grain_Mn= r1$ppm.Mn.100,
		grain_Cu= r1$ppm.Cu.101,
		grain_Fe= r1$ppm.Fe.102,
		grain_Zn= r1$ppm.Zn.103,
		trial_id = "1"
	)

	pd <- r1$Harvest.date.HDATE
	pd[pd == "41"] <- NA
	i <- grepl("^42", pd)
	pd[i] <- as.character(as.Date("1900-01-01") + as.integer(pd[i]) - 2)
	pd[!i] <- as.character(as.Date(pd[!i], "%d/%m/%Y"))
	d1$planting_date <- pd
	d1 <- unique(d1)

		
	## Season 2
	d2 <- data.frame(
		adm1= r2$State.Province,
		adm2= r2$LGA,
		location= r2$Community.village,
		latitude= r2$GPS.Coordinate.Latitude,
		longitude= r2$GPS.Coordinate.Longitude,
		elevation= r2$GPS.Coordinate.Altitude,
		treatment= r2$Treatment,
		variety_type= r2$Maize.variety.type,
		variety= r2$Maize.variety.name.MVnam,
		planting_date= as.character(as.Date(r2$Planting.date, "%d/%m/%Y")),
		harvest_date= as.character(as.Date(r2$Harvest.Date)),
		yield= r2$Grain.Yield.kg.ha,
		fwy_residue= r2$Stalk.Yield.kg.ha,
		dmy_total= r2$Above.ground.biomass.t.ha*1000,## in kg/ha
		leaf_N= r2$pct.N.46,
		leaf_P= r2$pct.P.47,
		leaf_K= r2$pct.K.48,
		leaf_Ca= r2$pct.Ca,
		leaf_Mg= r2$pct.Mg,
		leaf_Cu= r2$ppm.Cu,
		leaf_Fe= r2$ppm.Fe,
		leaf_Mn= r2$ppm.Mn,
		leaf_Zn= r2$ppm.Zn,
		leaf_B= r2$ppm.B,
		residue_N= r2$pct.N.57,
		residue_P= r2$pct.P.58,
		residue_K= r2$pct.K.59,
		grain_N= r2$pct.N.60,
		grain_P= r2$pct.P.61,
		grain_K= r2$pct.K.62,
		trial_id = "2"
	)
	
	d <- carobiner::bindr(d1, d2)
	d$adm2[d$adm2 == "T/Wada"] <- "Tudun Wada"

	i <- nrow(d0)
	d0 <- rbind(d0, d0[i,])
	d0[i+1, "adm2"] <- "Bakori"

	# fertilization rates
	d <- merge(d, d0, by="adm2", all.x = TRUE)	
	for (nut in c("N", "P", "K", "S", "Ca", "Zn", "B")) {
		name <- paste0(nut, "_fertilizer")
		d[name] <- d[name] * grepl(nut, d$treatment)
	}
	
	#see data.frame "fd"
	ft <- rep("", nrow(d))
	ft[grepl("N", d$treatment)] <- "urea;"
	i <- grepl("P", d$treatment)
	ft[i] <- paste0(ft[i], "SSP;")
	i <- grepl("K", d$treatment)
	ft[i] <- paste0(ft[i], "KCl;")
	i <- grepl("S", d$treatment)
	ft[i] <- paste0(ft[i], "gypsum;MgSO4;ZnSO4;borax")
	ft[ft == ""] <- "none"
	d$fertilizer_type <- gsub(";$", "", ft)
	
	d$plant_spacing <- 25 #from data description 
	d$row_spacing <- 75 # from data description
	d$plot_width <- 5  # from data description
	d$plot_length <- 6 # from data description
	d$plot_area <- 5 * 6
	### the below is not consistent with the above. Perhaps the below is the _harvested_ area?
	### d$plot_area <- as.numeric(gsub("3 x 3", 9, d$plot_area))

	d$country <- "Nigeria"
	d$crop <- "maize"
	d$yield_part <- "grain"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$inoculated <- FALSE
	
	## Fixing previous crop content
	p <- carobiner::fix_name(d$previous_crop, "lower")
	p <- gsub(", and | and |, | & ", ";", p)
	p <- gsub("g/nut|gnut", "groundnut", p)
	p <- gsub("mazie", "maize", p)
	d$previous_crop <- p
	d$variety_type[grep("HYBRID", d$variety_type, ignore.case=TRUE)] <- "hybrid"
	
	## maize is not harvested more than 1 year after planting
	d$harvest_date <- gsub("2017-09-30", "2016-09-30", d$harvest_date) 

	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(path, meta, d)
	
}

