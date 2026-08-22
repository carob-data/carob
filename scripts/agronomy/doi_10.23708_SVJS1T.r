# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {

"
Data for: Varietal mixtures of Sahelian smallholders conciliate enhanced yield and agrobiodiversity conservation

This study was co-designed with farmers' organizations in Senegal with the aim of assessing the impacts of the mixing of early- and late-flowering pearl millet landraces (Cenchrus americanus) on in situ yield, i.e. in poor soil conditions with limited fertilization on smallholder plots. 

To this end, during 2019 growing season, we performed experiments in two villages from Senegal, with 15 peasants per village. Two groups of treatments were randomly distributed within each block: T1: monovarietal pearl millet with landrace 1, or 2, 3 and 4, and T2: mixture of all four landraces (1 + 2 + 3 + 4) at equal proportions (25% each). In addition to grain and fodder yield, we measured seven morphological traits and three variables related to weeds (all species together without taxonomic identification) and pathogens. Soils were sampled before the start of the experiments and were analysed for total soil organic carbon (SOC) and total nitrogen (by CHN elemental analyser), available P (Olsen method), water pH and mineral nitrogen (N_NO3 and N_NH4).
"

	uri <- "doi:10.23708/SVJS1T"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IRD; Gaston Berger University; ENSAT; CIRAD",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield", 
		notes =NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-08-21",
		carob_completion = 90,	
		carob_effort = 6
	)

	f1 <- ff[basename(ff) == "DIVAGRO_Grain_fodder_yield_perlandrace_perunit.csv"]
	f2 <- ff[basename(ff) == "DIVAGRO_soil_measures.csv"]
	f3 <- ff[basename(ff) == "DIVAGRO_traits_perlandrace_perunit.csv"]

	r1 <- read.csv(f1,sep = ";")
	r2 <- read.csv(f2,sep = ";")
	r3 <- read.csv(f3,sep = ";")
   
	#standardizing in long format
	long1 <- data.frame(
	  plot_id=r1$Plot,
	  location=tolower(r1$Site),
	  variety=r1$Variety,
	  treatment="pure landrace",
    yield=r1$GY.T1.kg.ha.,
	  fodder_yield=r1$FY.T1.T.ha.
	)
	
	long2 <- data.frame(
	  plot_id=r1$Plot,
	  location=tolower(r1$Site),
	  variety=r1$Variety,
	  treatment="mixed landrace",
	  yield=r1$GY.T2.kg.ha.,
	  fodder_yield=r1$FY.T2.T.ha.
	)
	
	d1 <- rbind(long1,long2)

	soil <- data.frame(
	  plot_id=r2$Plot,
	  location=tolower(r2$Site),
	  soil_NO3=r2$N.NO3..mg.kg,
	  soil_NH4=r2$N.NH4..mg.kg,
	  soil_N=r2$N,
	  soil_SOC=r2$C,
	  soil_CN=r2$C...N,#C:N ratio
	  soil_P=r2$P.assim.mg.kg,
	  soil_pH=r2$pH
	)
	
	d <- merge(d1,soil,by=c("plot_id","location"), all.x = T)
	
	traits <- data.frame(
	  plot_id=r3$Plot,
	  treatment=ifelse(r3$Tmt=="T1","pure landrace","mixed landrace"),
	  location=tolower(r3$Site),
	  variety=r3$Variety,
	  plant_density=r3$Density*10000,
	  seed_weight=r3$W1000,
	  striga_damage=ifelse(r3$Striga==0,FALSE,TRUE)
	)
	
	d <- merge(d,traits,by=c("plot_id","location","variety","treatment"), all.x = T)
	
	d$country <- "Senegal"
	d$crop <- "pearl millet"
	d$trial_id <- paste(d$plot_id,d$location,d$variety,sep = "_")
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$longitude[d$location=="lissar"] <- -16.6205
	d$longitude[d$location=="mbane"] <- -15.7952
	d$latitude[d$location=="lissar"] <- 15.0562
	d$latitude[d$location=="mbane"] <- 16.2667
	d$geo_from_source <- FALSE
	d$planting_date <- "2019"
	d$harvest_date  <- NA
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
	d$yield_part <- "grain"
	d$yield_moisture <- NA
	d$yield_isfresh <- NA
  d$location[d$location=="lissar"] <- "Lissar"
  d$location[d$location=="mbane"] <- "Mbane"
  
	carobiner::write_files(path, meta, d)
}
