# R script for "carob"


carob_script <- function(path) {

"Vitamin A deficiency, drought, low soil nitrogen (low N) and Striga hermonthica parasitism of maize (Zea mays L.) cause malnutrition and food insecurity in sub-Saharan Africa. The objectives of this study were to determine combining abilities of extra-early provitamin A (PVA) lines, classify them into heterotic groups (HGs), identify testers, and determine yield stability of hybrids under contrasting environments in two trials. In trial 1, 20 extra-early PVA lines were inter-mated in a diallel mating scheme to obtain 190 F1 hybrids. The 190 F1 hybrids plus six checks were tested under Striga infestation, drought, and stress-free environments in Nigeria from 2015 to 2017. In trial 2, 35 extra-early yellow hybrids were evaluated under low-N, Striga-infested and stress-free environments in 2018. Provitamin A concentrations of 23.98 36 and 22.56 µg g-1 were obtained for TZEEIOR 202 and TZEEIOR 205. TZEEIOR 197 × 37 TZEEIOR 205 (20.1 μg g-1) and TZEEIOR 202 × TZEEIOR 205 (22.7 μg g-1) contained about double the PVA level of the commercial check, TZEEI 58 × TZEE-Y Pop STR C5 (11.4 μg 39 g -1). Both general (GCA) and specific (SCA) combining ability variances were statistically significant for most agronomic traits, although GCA was much larger than SCA effects, indicating that additive genetic effects primarily controlled the inheritance of those traits. TZEEIOR 97 and TZEEIOR 197 were identified as inbred testers. TZEEIOR 197 × TZEEIOR 205 (20.1 μg g-1) was identified as a single-cross tester as well as the most stable and highest yielding hybrid across environments. TZEEIOR 202 and TZEEIOR 205 should be invaluable resources for breeding for high PVA. PVA level was independent of hybrid yield potential, indicating that selection of superior hybrids with elevated PVA levels should be feasible."

	uri <- "doi:10.25502/g8y5-2377/d"
	group <- "varieties_maize"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = "doi:10.1002/csc2.20071",
		project = NA,
		data_type = "experiment",
		response_vars = "yield",
		treatment_vars = "variety_code;striga_trial;season", 
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2024-07-11"
	)
	

	f1 <- ff[basename(ff) == tolower("Drought-Stress.csv")]
	f2 <- ff[basename(ff) == tolower("Optimum.csv")]
	f3 <- ff[basename(ff) == tolower("Striga.csv")]
	
	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)
	
	# drought stress
	d1 <- data.frame(
		#publication: drought trials began November and ended in March the following year
		planting_date = paste0(r1$YEAR, "-11"),
		harvest_date = paste0(r1$YEAR+1, "-11"),
		location=r1$LOC,
		trial_id=paste0(r1$TRIAL, "_", r1$LOC),
		rep=r1$Rep,
		variety_code=r1$Pedigree,
		dy_poll=r1$POLLEN,
		silking_days=r1$DYSK,
		asi=r1$ASI,
		plant_height=r1$PLHT,
		ear_height=r1$EHT,
		rl=r1$RL,
		rlper=r1$RLPERCT,
		sl=r1$SL,
		slper=r1$SLPERCT,
		husk=r1$HC,
		p_asp=r1$PASP,
		e_asp=r1$EASP,
		p_harv=r1$PHARV,
		e_harv=r1$EHARV,
		e_rot=r1$EROT,
		moist=r1$MOIST,
		yield = r1$YIELD,
		#fertilizer rates from publication
		N_fertilizer=90,
		P_fertilizer=60,
		K_fertilizer=60,
		fertilizer_dap="0;21",
		irrigation_amount=17 * 4,
		season_constraint = "drought",
		striga_trial=FALSE,
		striga_infected=FALSE,
		season="dry season",
		irrigated=TRUE
	)
	
	# optimal
	d2 <- data.frame(	
	  planting_date = as.character(r2$YEAR),
	  harvest_date = as.character(r2$YEAR),
	  location=r2$LOC,
	  trial_id=paste0(r2$TRIAL, "_", r2$LOC),
	  rep=r2$Rep,
	  variety_code=r2$Pedigree,
	  dy_poll=r2$POLLEN,
	  silking_days=r2$DYSK,
	  asi=r2$ASI,
	  plant_height=r2$PLHT,
	  ear_height=r2$EHT,
	  rl=r2$RL,
	  sl=r2$SL,
	  husk=r2$HC,
	  p_asp=r2$PASP,
	  e_asp=r2$EASP,
	  p_harv=r2$PHARV,
	  e_harv=r2$EHARV,
	  e_rot=r2$EROT,
	  moist=r2$MOIST,
	  yield = r2$YIELD,
      #fertilizer rates from publication
	  N_fertilizer=90,
	  P_fertilizer=60,
	  K_fertilizer=60,
	  fertilizer_dap="14;28",
	  fertilizer_type="NPK;urea",
	  striga_trial=FALSE,
	  striga_infected=FALSE,
	  season="rainy season",
	  irrigated=FALSE
	)
	
	# striga
	d3 <- data.frame(
	  planting_date = as.character(r3$YEAR),
	  harvest_date = as.character(r3$YEAR),
	  location=r3$LOC,
	  trial_id=paste0(r3$TRIAL, "_", r3$LOC),
	  rep=r3$Rep,
	  variety_code=r3$Pedigree,
	  dy_poll=r3$POLLENIN,
	  silking_days=r3$DYSKIN,
	  asi=r3$ASIIN,
	  plant_height=r3$PLHTIN,
	  rlper=r3$RLINPERCT,
	  slper=r3$SLINPERCT,
	  ehtin=r3$EHTIN,
	  rl=r3$RLIN,
	  sl=r3$SLIN,
	  e_asp =r3$EASPIN,
	  p_harv=r3$PHARVIN,
	  e_harv=r3$EHARVIN,
	  e_rot=r3$EROTIN,
	  moist=r3$MOISTIN,
	  str_co1=r3$CO1,
	  str_co2=r3$CO2,
	  str_rat1=r3$RAT1,
	  str_rat2=r3$RAT2,
	  yield = r3$YIELDIN,
      #fertilizer rates from publication
	  N_fertilizer=30,
	  P_fertilizer=30,
	  K_fertilizer=30,
	  fertilizer_type="NPK",
	  fertilizer_dap="25",
	  soil_type="alfisols",
	  striga_trial=TRUE,
	  striga_infected=TRUE,
	  season="rainy season",
	  irrigated=FALSE
	)

	d <- carobiner::bindr(d1, d2, d3)
		
#field managment(see publication)
	d$herbicide_used <- TRUE
	d$herbicide_product <- ifelse(d$location%in% c("BG", "IK", "MO_OPT","IKDS"), "atrazine;paraquat dichloride","atrazine")
	  
#location (see publication)
  d$country <- "Nigeria"
  d$location <- gsub("IKDS|IK", "Ikenne", d$location)
  d$location <- gsub("MO|MO_OPT", "Mokwa", d$location)
  d$location <- gsub("BG", "Bagauda", d$location)
  d$longitude[d$location=="Ikenne"] <- 3.7150
  d$latitude[d$location=="Ikenne"] <- 6.8833
  d$elevation[d$location=="Ikenne"] <- 60
  d$longitude[d$location=="Mokwa"] <- 5.0667
  d$latitude[d$location=="Mokwa"] <- 9.3
  d$elevation[d$location=="Mokwa"] <- 457
  d$longitude[d$location=="Bagauda"] <- 8.3667
  d$latitude[d$location=="Bagauda"] <- 12
  d$elevation[d$location=="Bagauda"] <- 580
  
# about the data (TRUE/FALSE)
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$borer_trial <- FALSE
	d$crop <- "maize"
	d$yield_part <- "grain"
	
	carobiner::write_files(path, meta, d)
}



