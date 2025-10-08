# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Data of a long-term experiment on mineral and organic fertilization and crop productivity of a three-years cotton based rotation in N'Tarla - Mali

Dataset of agronomic and climate variables over the 1965-1990 period of experiment in N'Tarla, Mali. Cotton/Soghum/Groundnut rotation. 4 treatments were compared: control without fertilization, mineral fertilization, organic fertilization and combination of both. Agronomic variables : yield of cotton, sorghum and groundnut; C,N,K, P soil content, pH; description of organic manure - nutrients content and moisture. Climate variables :  rainfall, temperature, wind, radiation.
"

	uri <- "doi:10.18167/DVN1/GPZOHO"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIRAD; IER",
		publication = "doi:10.1016/j.fcr.2015.02.013",
		project = NA,
		carob_date = "2025-10-07",
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = "The DataNtarla_1965_90_dataversev1.xlsx file includes all the data."
	)
	

	
	f <- ff[basename(ff) == "DataNTarla_1965_90_dataversev1.xlsx"]
	
	r1 <- carobiner::read.excel(f, sheet="ITK_ferti")
	r2 <- carobiner::read.excel(f, sheet="OpeCulturales")
	r3 <- carobiner::read.excel(f, sheet="Climat")
	r4 <- carobiner::read.excel(f, sheet="AnalysesSol_granulo")
	r5 <- carobiner::read.excel(f, sheet="AnalysesSol_chimie")
	r6 <- carobiner::read.excel(f, sheet="Rendements_Culture")
	r7 <- carobiner::read.excel(f, sheet="DataFumier")
	
	
 #### process fertilizer
	d1 <- data.frame(
	   location = r1$Essai,
	   treatment = r1$Traitement,
	   crop = tolower(r1$Culture),
	   year = as.character(r1$Annee),
	   N_fertilizer = r1$N,
	   P_fertilizer = r1$P,
	   K_fertilizer = r1$K,
	   Ca_fertilizer = r1$Ca,
	   Mg_fertilizer = r1$Mg,
	   OM_amount = r1$Fumier*1000
	)
	
 #### process 	date
	
	dp <- data.frame(
	   location = r2$Essai,
	   year = paste("19", r2$Année, sep = ""),
	   crop = tolower(r2$Culture),
	   serie = as.numeric(gsub("NA", NA, r2$NumSerie)),
	   planting_date = ifelse(grepl("Semis|Ressemis", r2$Operation), as.character(r2$Date), NA)
	   
	)
	
	dh <- data.frame(
	   location = r2$Essai,
	   year = paste("19", r2$Année, sep = ""),
	   crop = tolower(r2$Culture),
	   serie = as.numeric(gsub("NA", NA, r2$NumSerie)),
	   harvest_date = ifelse(grepl("Recolte" , r2$Operation), as.character(r2$Date), NA)
	   
	)
	d2 <- merge(dp[!is.na(dp$planting_date),], dh[!is.na(dh$harvest_date),], by= c("location", "year", "crop", "serie"), all = TRUE)
	d2 <- d2[!is.na(d2$serie),]
	
	### process weather data 
	
	d3 <- data.frame(
	   date = as.character(r3$date),
	   tmax = r3$temp_max,
	   tmin = r3$temp_min,
	   prec = r3$pluviometrie,
	   srad = r3$Rg,
	   wspd = r3$vent,
	   country = "Mali",
	   location = "NTarla",
	   longitude = -5.688,
	   latitude = 12.619,
	   geo_from_source = TRUE # from paper
	)
	
	d3[d3==-999] <- NA
	### process soil 
	
	d4 <- data.frame(
	   location = r4$Essai,
	   treatment =  gsub("C", "Control", r4$traitement),
	   serie = r4$serie,
	   year = paste("19", r4$annee, sep = ""),
	   deph = r4$horizon,
	   soil_clay = r4$argiles,
	   soil_silt_coarse = r4$limons_grossiers,
	   soil_silt_fine = r4$limons_fins,
	   soil_sand_coarse = r4$sables_grossiers,
	   soil_sand_fine = r4$sables_fins
	)
	
	d5 <- data.frame(
	   location = r5$Essai,
	   treatment =  gsub("C", "Control", r5$traitement),
	   serie = r5$serie,
	   year = paste("19", r5$annee, sep = ""),
	   deph = r5$horizon,
	   soil_SOM = r5$MO,
	   soil_SOC = r5$C_ORG,
	   soil_N_total = r5$N_TOT*1000,
	   soil_P_total = r5$P_TOT,
	   ## from meq/100g to ppm using molecular weight en charge number
	   soil_Ca = r5$CA*(40/2)*10, 
	   soil_Mg = r5$MG*(24.31/2)*10,
	   soil_K = r5$K*(39/2)* 10,
	   soil_Na = r5$`NA`,
	   soil_Al = r5$AL,
	   soil_Mn = r5$MN,
	   #r6$H,
	   soil_S = r5$S_BASES,
	   soil_CEC = r5$CEC,
	   soil_pH = r5$PH
	)
	
	### merge soil data
	dd <- merge(d4, d5, by=c("year", "treatment", "deph", "serie", "location"), all.y = TRUE)
	
	### aggregate
	dd <- aggregate(cbind(soil_clay, soil_silt_coarse,soil_silt_fine, soil_sand_coarse,soil_sand_fine, soil_SOM, soil_SOC, soil_N_total, soil_P_total, soil_Ca, soil_Mg, soil_K, soil_Na, soil_Al, soil_Mn, soil_S, soil_CEC, soil_pH) ~ year+ treatment, data= dd, FUN= mean, na.action = na.pass)
	dd$soil_P_method <- "Olsen"
	
	### process yield data 
	
	d6 <- data.frame(
		location = r6$Essai,
		treatment = gsub("C", "Control", r6$traitement),
		crop = r6$culture,
		year = paste("19", r6$annee, sep = ""),
		serie = r6$serie,
		rep = as.integer(r6$bloc),
		yield = r6$rdt,
		trial_id = paste(r6$Essai , r6$serie, sep = "-"),
		country = "Mali",
		longitude = -5.688,
		latitude = 12.619,
		geo_from_source = TRUE # from paper
	)

	
	### merge fertilizer and yield
	d <- merge(d6, d1, by= c("location", "treatment", "crop", "year"), all.x = TRUE)
	
	### merge yield with soil data
	d <- merge(d, dd, by= c( "treatment", "year"), all.x = TRUE)
	
	
	## merge yield with date 
	df <- merge(d, d2, by= c("location", "year", "crop", "serie"), all.x = TRUE)
	
   df$planting_date[is.na(df$planting_date)] <- df$year[is.na(df$planting_date)]
	
   df$year <- df$serie <- NULL

   ### Fixing crop names
   P <- carobiner::fix_name(df$crop)
   P <- gsub("arachide", "groundnut", P)
   P <- gsub("coton", "cotton", P)
   P <- gsub("sorgho", "sorghum", P)
   df$crop <- P
   
   df$crop_rotation <- ifelse(grepl("sorghum", df$crop), "sorghum;groundnut;cotton",
                        ifelse(grepl("cotton", df$crop), "cotton;sorghum;groundnut", "groundnut;cotton;sorghum"))
   df$on_farm <- TRUE
   df$is_survey <- FALSE
   df$irrigated <- NA
   df$yield_part <- "grain"
   df$yield_moisture <- as.numeric(NA)
   


carobiner::write_files(path, meta, df, wth = d3)

}


