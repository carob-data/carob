# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Variability of soybean response to rhizobia inoculant, vermicompost, and a legume-specific fertilizer blend in Siaya County of Kenya

Phase-II will set up sustainable, institutionalized quality control and regulatory systems for commercial products to enable the new country partners to continue screening new products through country-specific investments in human and technical capacity and effective interactions with national and regional regulatory bodies and policymakers. Simultaneously, confirmation screening of the tested products in the new countries and screening of new products will take place in all countries and newly validated products will enter the dissemination campaigns, led by public-private sector partnerships and accompanied by large-scale promotion campaigns, using country-specific models. Above interactions with policy makers, regulatory bodies, extension and private sector partners, and farming households will be enriched through specific communication materials and approaches. It is expected that through these concerted efforts, the use of effective commercial products will improve yields of target crops with 15-30% for at least 200,000 farming households across the target countries. It is also expected that new commercial products will pass through a self-sustaining quality control and regulation system in at least 4 of the 6 countries towards the end of the project.
"

	uri <- "doi:10.25502/3633-0d33/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = "doi:10.1016/j.still.2019.06.007",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;N_organic",
		response_vars = "yield;nodule_weight", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-08-04",
		carob_completion = 100,	
		carob_effort = 2
	)
	

	f1 <- ff[basename(ff) == "field-exp-site-7.csv"]
	f2 <- ff[basename(ff) == "field-exp-site-17.csv"]
	f3 <- ff[basename(ff) == "greenhouse-exp-1.csv"]
	f4 <- ff[basename(ff) == "greenhouse-exp-2.csv"]
	f5 <- ff[basename(ff) == "metadata.csv"]

	r1 <- read.csv(f1, na= "")
	r2 <- read.csv(f2, na= "")
	r3 <- read.csv(f3, na="")
	r4 <- read.csv(f4, na= "")
	r5 <- read.csv(f5, na = "")

#### process
	## Field trial experiment (site 7, vermicompost used)
	d1 <- data.frame(
		plot_id = as.character(r1$POT_NO),
		treatment = r1$TREATMENT,
		inoculated = r1$INOCULATION== "YES",
		fertilizer_used = r1$SYMPAL=="YES",
		N_fertilizer = 0,
		plot_area = 9 , #m2 from publication
		VC = r1$PHYMYX,
		rep = r1$BLOCK,
		nodule_weight	 = (r1$NFW_G_P/9)*10,
		dmy_residue = (r1$SDW_G_P/9)*10,
		#r1$EFFECTIVENESS,
		yield = r1$GRAIN_YIELD_KG_HA,
		fertilizer_type = "sympal",
		OM_type = "vermicompost",
		inoculant = "Rhizobia",
		trial_id = "site7"
	)
	
  d1 <- d1[!is.na(d1$plot_id),]
  OM_amount <- c("PA"=0 , "PB" =2.5*1000, "PC" = 5*1000, "PD"=7.5*1000 , "PE" =10*1000 )
  Nrate_Vc <- c("PA"=0 , "PB" =37, "PC" = 74, "PD"=111 , "PE" = 148)
  d1$N_organic <- Nrate_Vc[d1$VC]
  d1$OM_amount <-  OM_amount[d1$VC]
  d1$VC <- NULL
	
	### Field trial experiment site 17 (Vermocompost used )
	d2 <- data.frame(
	  plot_id = as.character(r2$POT_NO),
	  treatment = r2$TREATMENT,
	  inoculated = r2$INOCULATION =="YES",
	  fertilizer_used = r2$SYMPAL== "YES",
	  N_fertilizer = 0,
	  VC = r2$PHYMYX,
	  rep = r2$BLOCK,
	  plot_area = 9, #m2
	  nodule_weight	 = (r2$NFW_G_P/9)*10, # kg /ha
	  dmy_residue = (r2$SDW_G_P/9)*10,
	  #r2$EFFECTIVENESS,
	  yield = r2$GRAIN_YIELD_KG_HA,
	  OM_type = "vermicompost",
	  fertilizer_type = "sympal",
	  inoculant = "Rhizobia",
	  trial_id = "site17"
	)
	
	d2 <- d2[!is.na(d2$plot_id),]
	
	OM_amount <- c("PA"=0 , "PB" =2.5*1000, "PC" = 5*1000, "PD"=7.5*1000 , "PE" =10*1000 )
	Nrate_Vc <- c("PA"=0 , "PB" =37, "PC" = 74, "PD"=111 , "PE" = 148)
	d2$N_organic <- Nrate_Vc[d2$VC]
	d2$OM_amount <-  OM_amount[d2$VC]
	d2$VC <- NULL
	
	### d1 and d2 (site A and B data)
	d_field <- carobiner::bindr(d1,d2)
	
	#### first greenhouse experiment
	d3 <- data.frame(
	  rep = r3$REP,
	  fertilizer_used = FALSE,
	  plot_id = as.character(r3$POT_NO),
	  soil_C_total = r3$CARBON,
	  soil_N = r3$NITROGEN,
	  plot_area = 9,
	  nodule_weight = (r3$NFW_G_P/9)*10,
	  dmy_total = (r3$BDW_G_P/9)*10,
	  treatment = r3$TREATMENT,
	  #sample_id = as.character(r3$SOIL_NUMBER),
	  inoculated = grepl("^Inoculation$", r3$TREATMENT),
	  inoculant = "Rhizobia",
	  N_fertilizer = 0,
	  N_organic = 0,
	  fertilizer_type = "none",
	  OM_type = "none",
	  OM_amount = 0,
	  trial_id = "greenhouse-exp-1"
	)
	
	d3 <- d3[!is.na(d3$plot_id),]
	
	# Second greenhouse experiment
	d4 <- data.frame(
	  fertilizer_used = r4$Sympal== "YES",
	  plot_id = gsub("Pot ", "",  r4$Pot_number),
	  NS = r4$Nsource,
	  fertilizer_type = ifelse(grepl("U", r4$Nsource), "urea", "none"),
	  OM_type = ifelse(grepl("P", r4$Nsource), "vermicompost", "none"),
	  rep = r4$Reps,
	  plot_area = 9 ,
	  fwy_residue = (r4$SFW/9)*10,
	  dmy_residue = (r4$SDW_G_P/9)*10, #g/plot
	  #grain_N = r4$N_PARC,
	  nodule_weight = (r4$NFW/9)*10,
	  inoculated	 = r4$Legumefix=="YES",
	  inoculant = "Rhizobia",
	  trial_id = "greenhouse-exp-2"
	)
	
	d4 <- d4[!is.na(d4$plot_id),]
	VC <- c("P1"=0 , "P2"= 37, "P3" = 74, "P4" =111 , "P5"=148 ,"U1"= 0, "U2"= 0 , "U3" = 0 , "U4" = 0 , "U5"= 0)
	VC_amount <- c("P1"=0 , "P2"= 2.5*1000, "P3" = 5*1000, "P4" = 7.5*1000 , "P5"= 10*1000 ,"U1"= 0, "U2"= 0 , "U3" = 0 , "U4" = 0 , "U5"= 0)
	urea <- c("U1"=0 , "U2"= 37 , "U3" =74 , "U4" = 111, "U5"= 148 , "P1"= 0 , "P2"= 0 , "P3" = 0, "P4" = 0 , "P5"= 0 )
	
	d4$N_fertilizer <- urea[d4$NS]
	d4$N_organic <- VC[d4$NS]
	d4$OM_amount <- VC_amount[d4$NS]
	d4$NS <- NULL 
	### combine d3 and d4 (greenhouse experiment 1 and 2)
	d_ghexp <- carobiner::bindr(d3, d4)
	
	###
	d <- carobiner::bindr(d_field, d_ghexp)
	
	## from publication
	i <- grepl("TRUE", d$fertilizer_used)
	d$P_fertilizer <- d$K_fertilizer <- d$Ca_fertilizer <- d$S_fertilizer <- d$Mg_fertilizer <- d$Zn_fertilizer <- 0
	d$P_fertilizer[i] <- 30
	d$K_fertilizer[i] <- 38
	d$Ca_fertilizer[i] <- 21
	d$S_fertilizer[i] <- 12
	d$Mg_fertilizer[i] <- 1.8
	d$Zn_fertilizer[i] <- 0.243
	d$fertilizer_used <- ifelse(grepl("urea", d$fertilizer_type), TRUE, d$fertilizer_used)

	### adding soil
	
	soil <- data.frame(
	  trial_id = c("site7", "site17"),
	  soil_P = c(3.1, 12),
	  soil_pH = c(4.5, 5.4),
	  soil_N_total = c(0.6, 0.8),
	  soil_SOC = c(8, 10.5),
	  soil_Ca_exch = c(0.9, 5.5),
	  soil_Mg_exch = c(0.3, 1.5),
	  soil_K_exch = c(0.2, 0.3),
	  soil_texture = "clay"
	)
	
	d <- merge(d, soil, by= "trial_id", all.x = TRUE)
	
	#####
	d$crop <- "soybean"
	d$is_survey <- FALSE
	d$on_farm <- TRUE
	d$yield_moisture <- NA
	d$yield_part <- "grain"
	d$country <- "Kenya" 
	d$geo_from_source <- FALSE
	d$latitude <- -0.0546 
	d$longitude <-  34.2488 
	d$geo_source <-  "GADM 4.1, adm1"
	d$adm1 <- "Siaya"
	d$irrigated <- NA
	d$planting_date <- "2016-04" 
	d$harvest_date <- NA_character_
	d$yield_isfresh <- NA
	
	carobiner::write_files(path, meta, d)
}

