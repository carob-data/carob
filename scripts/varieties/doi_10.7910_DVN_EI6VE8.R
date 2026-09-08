# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {
  
  "
Effects of artisanal parboiling steaming time and variety 
on grain quality, mineral and digestive properties of rice
"
  
  uri <- "doi:10.7910/DVN/EI6VE8"
  group <- "varieties"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
                                  data_organization = "AfricaRice",
                                  publication = "doi:10.1002/fsn3.600",
                                  project = NA,
                                  design = NA,
                                  data_type = "experiment",
                                  treatment_vars = "variety",
                                  response_vars = "grain_protein; grain_P, grain_K; grain_Mg; grain_Na; water_uptake; amylose; cooking_time", 
                                  notes = NA,
                                  carob_contributor = "Kora Simperegui",
                                  carob_date = "2026-09-07",
                                  carob_completion = 100,	
                                  carob_effort = 2
  )
  
  
  f <- ff[basename(ff) == "Effects of artisanal parboiling.xls"]
  
  r <- carobiner::read.excel(f)
  
  
  d <- data.frame(
    trial_id = paste(r$Location, r$Year, sep="-"),
    date = as.character(r$Year),
    country = "Benin",
    rep = as.integer(ifelse(r$Rep == "R1",1, ifelse(r$Rep == "R2", 2, 3))),
    crop = "rice", 
    variety = r$Variety,
    grain_protein = r$Protein,
    grain_P = r$Phosphorus*10, # from % to mg/g (x10000 % to mg/kg)/1000(kg to g)
    grain_K = r$Potassium/1000, # from  mg/kg to mg/g
    # grain_Ca = r$Calcium/1000, # Original unit is mg/mL. To convert mg/mL to mg/mg, I need the density of the solution which I don't have.
    grain_Mg = r$Magnesium/1000, # from  mg/kg to mg/g
    grain_Na = r$Sodium/1000, # from  mg/kg to mg/g
    is_survey = FALSE, 
    on_farm = FALSE, 
    planting_date = as.character(NA),
    
    #The following variable are not yet available in carob
    steamed_time = r$Time, #parboiling steaming time
    lipid = r$Lipid, #Lipid content (%) determined by the Soxhlet method (A.O.A.C., 1984) using petroleum ether as the extraction solvent (Anon, 1990)
    total_starch = r$Tstarch,	#Total starch fraction (%) determined by Megazyme total starch assay kit (K-TSTA, Megazyme Int. Co. Wicklow, Ireland) based on AOAC method 996.11 and AACC method 76-13.01.
    resistant_starch = r$Rstarch,	# Resistant starch (%) determined by Megazyme resistant starch assay kit (K-RSTAR, Megazyme Int. Co. Wicklow, Ireland) protocol (AOAC method 2002.02 and AACC method 32-40.01). 
    damage_starch = r$Dstarch,	#	Damage starch (%) determined by Megazyme total damage starch assay kit (K-SDAM, Megazyme Int. Co. Wicklow, Ireland) protocol (AACC approved method 76-32.01 and ICC Method No. 164)
    soluble_starch = r$Sstarch,	#	Soluble starch (%) determined from the difference between total starch and resistant starch.
    ash = r$Ash, #Ash content (%) determined by absorption spectrophotometer (Varian Vista, Victoria, Australia)
    amylose = r$amylose, #Apparent amylose content (%) determined by the standard iodine colorimetric method ISO 6647-2-2011 using an Auto Analyzer 3 (Ndindeng et al., 2015)
    brown = r$Brice, #		Brown rice (%)
    milled = r$Mrice, #		Milled rice (%)
    head_rice = r$Headrice,	# Head rice yield (%)
    broken_fraction = r$Bfraction, #	Broken fraction (%)
    grain_width = r$Width, # Grain width (mm) determined using the S21 Rice Statistic Analyzer (LKL Technologia, Brazil (Graham-Acquaah, Manful, Ndindeng, and Tchatcha (2015)
    grain_length = r$Length, # Grain length (mm) determined using the S21 Rice Statistic Analyzer (LKL Technologia, Brazil (Graham-Acquaah, Manful, Ndindeng, and Tchatcha (2015)
    cooking_time = r$sCT,	#	Cooking time (min) which was the time taken to cook the sample
    swelling_ratio = r$sR,	#	Swelling ratio which was determined as described by Bhattacharya and Sowbhagya (1971). 
    water_uptake = r$WUR	#	Water uptake ratio which was determined as described by Bhattacharya and Sowbhagya (1971). 
  )
  
  #adding the coordinates. Observation are all from Cotonou, Benin
  d$longitude <- 2.4182
  d$latitude <- 6.3758
  d$geo_from_source <- FALSE
  
  d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
  d$irrigated <- d$harvest_date <- d$yield <- d$yield_moisture <- d$yield_isfresh  <- NA
  d$yield_part <- "grain"
  
  carobiner::write_files(path, meta, d)
}



