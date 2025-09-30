# R script for "carob"
# license: GPL (>=3)

## ISSUES
## The link between maize (intercropped with soybean) in rotation with rice and site information is absent.

carob_script <- function(path) {

"
Agronomic management of rice blast datasets (®GARP database)

Data from GARP project in Madagascar highlands. 2 trials at different altitudes (middle and high). Impact of conservation agriculture cropping systems on plant growth, nitrogen uptake, blast epidemic and yield
"


	uri <- "doi:10.18167/DVN1/Y5HADO"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD; UMPL",
		publication = "doi:10.1016/j.fcr.2017.01.024",
		project = NA,
		carob_date = "2025-09-28",
        design = "unitOfAnalysis: Plot and pocket",
		data_type = "experiment",
		treatment_vars = "N_fertilizer;land_prep_method",
		response_vars = "yield;fwy_total", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "Biomasse_rotation.txt"]
	f2 <- ff[basename(ff) == "RecolteRotation.txt"]
	f3 <- ff[basename(ff) == "Date_Phenologique.txt"]
	f4 <- ff[basename(ff) == "DiagnosticFoliaire.txt"]
	f5 <- ff[basename(ff) == "Parcelle.txt"]
	f6 <- ff[basename(ff) == "VarSiteAnnee.txt"]
	f7 <- ff[basename(ff) == "FertiSiteAnnee.txt"]
	f8 <- ff[basename(ff) == "Recolte.txt"]
	f9 <- ff[basename(ff) == "Prelevement.txt"]
	f10 <- ff[basename(ff) == "DonneesMeteo.txt"]
	f11 <- ff[basename(ff) == "Site.txt"]
	#f12 <- ff[basename(ff) == "Intervention.txt"]
	#f13 <- ff[basename(ff) == "PyriFoliaire.txt"]
	#f14 <- ff[basename(ff) == "PyriPaniculaire.txt"]


	r1 <- read.table(f1, header = TRUE, sep = "\t", dec = "," , fileEncoding = "latin1", na= "")
	r2 <- read.table(f2, header = TRUE, sep = "\t", dec = "," , fileEncoding = "latin1", na= "")
	r3 <- read.table(f3, header = TRUE, sep = "\t", dec = "," , fileEncoding = "latin1", na= "")
	r4 <- read.table(f4, header = TRUE, sep = "\t", dec = "," , fileEncoding = "latin1", na= "")
	r5 <- read.table(f5, header = TRUE, sep = "\t", dec = "," , fileEncoding = "latin1", na= "")
	r6 <- read.table(f6, header = TRUE, sep = "\t", dec = "," , fileEncoding = "latin1", na= "")
	r7 <- read.table(f7, header = TRUE, sep = "\t", dec = "," , fileEncoding = "latin1", na= "")
	r8 <- read.table(f8, header = TRUE, sep = "\t", dec = "," , fileEncoding = "latin1", na= "")
	r9 <- read.table(f9, header = TRUE, sep = "\t", dec = "," , fileEncoding = "latin1", na= "")
	r10 <- read.table(f10, header = TRUE, sep = "\t", dec = ",", fileEncoding = "latin1", na= "")
	r11 <- read.table(f11, header = TRUE, sep = "\t", dec = ",", fileEncoding = "latin1", na= "")
	#r12 <- read.table(f12, header = TRUE, sep = "\t", dec = "," , fileEncoding = "latin1", na= "")
	#r13 <- read.table(f13, header = TRUE, sep = "\t", dec = "," , fileEncoding = "latin1", na= "")
	#r14 <- read.table(f14, header = TRUE, sep = "\t", dec = "," , fileEncoding = "latin1", na= "")
	
	### process rotation crop (maize intercropped with soybean)
	
	### biomass
	d1 <- data.frame(
	   trial_id = as.character(r1$IDParc),
	   fwy_total = (r1$PoidsFrais/r1$Surface)*10000,
	   dmy_total = (r1$PoidsSec/r1$Surface)*10000,
	   ## this can't be right (residue <= 20% of biomass)
	   ## dmy_residue = (as.numeric(r1$BiomResidu)/r1$Surface)*10000,
	   plot_area = r1$Surface,
	   crop = r1$Culture
	)
	
	##### yield
	d2 <- data.frame(
	   trial_id = as.character(r2$IDParc),
	   crop = r2$Culture,
	   harvest_date = as.character(as.Date(r2$DateRecRotation, "%d/%m/%Y")), # "Rec" surely stands for "recolte"?
	   plot_area = r2$SurfaceRecolte,
	   plant_density = (r2$NbPlanteRecTot/r2$SurfaceRecolte)*10000,
	   yield = as.numeric(r2$PdGrainTonHa)*1000
	)	



	dr <- merge(d2, d1, by=c("trial_id", "crop", "plot_area"), all.x = TRUE ) 	
	dr$crop <- gsub("Maïs|maïs", "maize",  dr$crop)
	dr$crop <- gsub("Soja", "soybean",  dr$crop)
	dr$intercrops <- ifelse(grepl("maize", dr$crop), "soybean", "maize")	
	dr$row_spacing <- 120
	dr$plant_spacing <- ifelse(grepl("maize", dr$crop), 50, 40)

	dr$intercrop_type <- "mixed"
	dr$country <- "Madagascar"
	dr$on_farm <- TRUE 
	dr$is_survey <-  FALSE 
	dr$yield_part <- "grain" 
	dr$yield_moisture <- as.numeric(NA) 
	dr$irrigated <- NA 
	
	
	#### process crop rice (in rotation with maize)
	
		## date information
	d3 <- data.frame(
		trial_id = r3$IDParc,
		planting_date = as.character(as.Date(r3$Date_Semis, "%d/%m/%Y")),
		heading_date = as.character(as.Date(r3$Date_debut_epiaison, "%d/%m/%Y")),
		flowering_date = as.character(as.Date(r3$Date_50_Floraison, "%d/%m/%Y"))
	)
		
		#### leaf nutrient content
	d4 <- data.frame(
		trial_id = r4$IDParc,
		leaf_N = as.numeric(r4$N)*10, ## % to mg/g
		leaf_P = as.numeric(r4$P)*10,
		leaf_K = as.numeric(r4$K)*10,
		leaf_Ca = as.numeric(r4$Ca)*10,
		leaf_Mg = as.numeric(r4$Mg)*10,
		leaf_Na = as.numeric(r4$Na)*10,
		leaf_S = as.numeric(r4$S)*10,
		leaf_Cu = as.numeric(r4$Cu),
		leaf_Mn = as.numeric(r4$Mn)/100,
		leaf_Zn = as.numeric(r4$Zn),
		leaf_B = as.numeric(r4$B),
		leaf_N_NH4 = as.numeric(r4$NNH4)/10,
		leaf_N_NO3 = as.numeric(r4$NNO3)
	)
		
	dd	<- merge(d3, d4, by= "trial_id")

	
	#### Rice variety code
	d5 <- data.frame(
		trial_id = r5$IDParc,
		#r5$Bloc,
		land_prep_method = ifelse(grepl("Labour", r5$Systeme), "conventional", "none"),
		variety_code = r5$Variete,
		treatment_nb = gsub("FV", "Fv", r5$Fertilisation),
		site_Nbr = r5$NumSite,
		year = r5$Annee
	)	
	
	###### variety names
	d6 <- data.frame(
		variety_code = r6$NomStat,
		variety = r6$NomVar,
		year = r6$Annee,
		site_Nbr = r6$NumSite
	)	
	
	dv <- merge(d5, d6, by = c("variety_code", "site_Nbr", "year"), all.x = TRUE)
	
	dd <- merge(dd, dv, by= "trial_id", all.x = TRUE)
	
	### Treatment 
	d7 <- data.frame(
		treatment_nb = r7$Fertilisation,
		treatment = ifelse(grepl("sans azote", r7$Caracteristiques), "N0", r7$Caracteristiques) ,
		N_fertilizer = ifelse(grepl("30N \\+ 23N \\+ 23N|30N \\+ 46N", r7$Caracteristiques), 76, 
					ifelse(grepl("^30N \\+ 23N$", r7$Caracteristiques), 53, 
					ifelse(grepl("60N \\+ 46N \\+ 46N", r7$Caracteristiques), 152, 
					ifelse(grepl("30N \\+ 15N \\+ 15N|30N \\+ 30N", r7$Caracteristiques), 60, 
					ifelse(grepl("60N \\+ 30N \\+ 30N", r7$Caracteristiques), 120, 
					ifelse(grepl("^30N$", r7$Caracteristiques), 30, 0)))))),
		site_Nbr = r7$Site,
		year = r7$Annee
	)	

	dd <- merge(dd, d7, by=c("site_Nbr", "treatment_nb", "year"), all.x = TRUE)

	#### yield
	d8 <- data.frame(
		trial_id = r8$IDParc,
		harvest_date = r8$DateRecolte,
		plot_area = as.numeric(r8$SurfRecolteParc),
		yield = as.numeric(r8$RendementGrainParc)*1000,
		plant_density = (as.numeric(r8$NbPlantsParM2))*10000, #  
		harvest_days = r8$NbJAS	
	)

	d8$harvest_date <- gsub("mai", "05", d8$harvest_date)
	d8$harvest_date <- gsub("mars", "03", d8$harvest_date)
	d8$harvest_date <- gsub("avr", "04", d8$harvest_date)
	d8$harvest_date <- as.character(as.Date(d8$harvest_date, "%d-%m-%y")) 

	dd <- merge(dd, d8, by= c("trial_id"), all = TRUE)


	#### Adding K and P  fertilizer and organic fertilizer 
	dd$P_fertilizer <- 150*0.1923
	dd$K_fertilizer <- 80*0.498
	dd$fertilizer_type <- ifelse(dd$N_fertilizer==0, "TSP;KCl", "urea;TSP;KCl")
	dd$OM_used <- TRUE
	dd$OM_amount <- 5000 # 5t
	dd$OM_type <- "farmyard manure"

	####	biomass and LAI
	d9 <- data.frame(
		trial_id = r9$IDParc,
		fwy_total = as.numeric(r9$BiomPlanteTHa)*1000,
		fwy_leaves = as.numeric(r9$BiomFeuilles)*1000,
		LAI = as.numeric(r9$LAISunScan)
	)
	
	dd <- merge(dd, d9, by= "trial_id", all = TRUE) 
	## Keep only rows where the site of the experiment is specified.
	dd <- dd[!is.na(dd$site_Nbr),] 
	
	#### weather information 
	d10 <- data.frame(
		site_Nbr = r10$NumSite,
		date = as.character(as.Date(r10$DateDonneeMeteo, "%d/%m/%Y")),
		tmin = as.numeric(r10$Tmin),
		tmax = as.numeric(r10$Tmax),
		temp = as.numeric(r10$Tmoy),
		rhmn = as.numeric(r10$HRmin),
		rhmx = as.numeric(r10$HRmax),
		rhum = as.numeric(r10$HRmoy24),
		wspd = as.numeric(r10$Ventmoy24),
		wspdmx = r10$Ventmax, ## maximum wind speed
		srad = as.numeric(r10$RayonGlobal24),
		ETo = as.numeric(r10$ETo), # ?
		country = "Madagascar"
	)	
	d10$rhmx[d10$rhmx > 100] <- 100
	
	### site information
	d11 <- data.frame(
		site_Nbr = r11$NumSite,
		location = r11$Site,
		station_name = r11$StationMeteo,
		elevation = r11$Altitude,
		longitude = ifelse(grepl("Andranomanelatra", r11$Site), 47.106, 46.41) ,
		latitude = ifelse(grepl("Andranomanelatra", r11$Site), -19.79, -19.554),
		geo_from_source = TRUE  # From paper
	)
	
	### Adding location in weather data 
	dw <- merge(d10, d11, by= "site_Nbr", all.x = TRUE)
	dw$site_Nbr <- NULL
	### adding location in rice yield data
	d <- merge(dd, d11, by= "site_Nbr", all.x = TRUE)
	d$site_Nbr <-  d$treatment_nb <-  NULL
	
	## drop rows with missing yield
	d <- d[!is.na(d$yield),]
	
	d$crop <- "rice"
	d$crop_rotation <- "rice;maize" 
	d$country <- "Madagascar"
	d$on_farm <- TRUE 
	d$is_survey <-  FALSE 
	d$yield_part <- "grain" 
	d$yield_moisture <- as.numeric(NA) 
	d$irrigated <- NA 
	d$trial_id <- as.character(d$trial_id)
	d$year <- substr(d$year, 1, 4)
	
	### Adding soil information
	
	
	soil <- data.frame(
		location = c(rep("Andranomanelatra", 5), rep("Ivory", 6)),
		year = c("2012", rep("2014", 4), "2012", "2014", "2014", rep("2013", 3)),
		depth_bottom = c(5, 10, 20, 40, 60, 20, 40, 60, 20, 40, 60),
		depth_top = c(0, 5, 10, 20, 40, 0, 20, 40, 0, 20, 40),
		soil_pH = c(5.83, 5.08, 4.90, 5.11, 4.93, 5.43, 5.38, 5.64, 5.44, 5.39, 5.60),
		soil_SOM = c(92.7, 81.2, 70.3, 44.4, 29.7, 29.6, 19.6, 13.9, 32.2, 18.6, 12.2)/10,
		soil_N = c(4.15, 3.45, 2.83, 1.53, 1, 1.17, 1.84, 0.68, 1.20, 0.80, 0.61)*1000,
		soil_P = c(15.5, 7.4, 4.90, 2.10, 1.75, 5.90, 3.35, 3.10, 5.5, 3.6, 3.45),
		soil_K = c(0.66, 0.2, 0.15, 0.09, 0.07, 0.411, 0.15, 0.12, 0.32, 0.08, 0.05)*390,
		soil_CEC = c(9.64, 4.07, 3.10, 2.14, 1.56, 3.68, 3.19,3.19, 3.91, 3.42, 3.37),
		soil_P_method = "Olsen"
	)
	
	### some if the above are in cmolc kg−1. Keep that, but here change the values to mg/g by accounting for the molecular weights.

	
	d <- merge(d, soil, by=c("location", "year"), all.x = TRUE)
	d$year <- NULL

	### join maize rotation crop data  and rice data
	
	df <- carobiner::bindr(d, dr)
	
	df$plant_density[df$plant_density == 0] <- NA
	
	### Missing values in longitude and latitude are due to the missing site information in the maize raw data.
	
	carobiner::write_files(path, meta, df, wth = dw)

}


