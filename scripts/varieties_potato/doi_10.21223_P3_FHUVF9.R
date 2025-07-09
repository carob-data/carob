# R script for "carob"


carob_script <- function(path) {
   
"Breeding efforts for enriching potato tubers with more Zn and Fe are in progress at the International Potato Center (CIP). Knowledge of genotype by environment interaction effects on the micronutrient concentrations of different genotypes is needed to identify cultivars that show high and stable concentrations and to inform breeding and selection schemes. Stability analysis for micronutrient content has been applied to biofortification and varietal dissemination strategies. Forty native potato cultivars were evaluated to: assess the magnitude and nature of Genotype (G), Environment (E), and GxE interaction effects for Zn and Fe concentrations in the tropical highlands of Peru and study the contribution of soil fertility to the micronutrient content of potato tubers. Tubers were taken from plots grown in randomized complete block designs with three replications of one hill per plot in each of 6 sites of the central Peruvian Andes in 2010: Ccasapata (3765 m.a.s.l); Sotopampa (3754 m.a.s.l); Ccollpaccasa (4067 m.a.s.l); Conayca (4178 m.a.s.l), la Victoria (3265 m.a.s.l) and Rangra (3323 m.a.s.l). Well-matured tubers were harvested at 150 and 180 days. Samples were prepared and analysed for Fe and Zn by inductively coupled plasma-optical emission spectrophotometry (ICP-OES) using an ARL 3580B ICP (ARL, Switzerland) (Burgos et al., 2007). Statistical analyses were performed using SAS software (SAS. 2003). ANOVA was performed using combined data for all environments. The Additive Main Effects and Multiplicative Interaction model (AMMI) was used for studying Genotype X Enviroment interaction, examining genotypic yield stability and adaptation (Crossa et al., 2002)."


	uri <- "doi:10.21223/P3/FHUVF9"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)
  
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=4,
		publication= NA,
		data_organization = "CIP;YANAPAI",
		carob_contributor="Cedric Ngakou",
		data_type="experiment",
		response_vars = "yield",
		treatment_vars = "variety",
		project=NA,
		carob_date="2023-12-12"
	)  
   
   ff <- ff[grep("PTYL", basename(ff))]

   process <- function(f) {
      r <- carobiner::read.excel(f, sheet="Fieldbook")
      r <- r[, c("REP", "INSTN", "TTYNA")]
      colnames(r) <- c("rep", "variety", "yield")

      m <- carobiner::read.excel(f, sheet="Minimal")
      n <- as.list(m$Value)
      names(n) <- m$Factor
      r$adm1 <- n$Admin1
      r$adm2 <- n$Admin2
      r$adm3 <- n$Admin3
	  r$location <- n$Locality

      r$latitude <- as.numeric(n$Latitude)  
      r$longitude <- as.numeric(n$Longitude)
	  r$geo_from_source <- TRUE
      r$planting_date <- n$`Begin date` 
      r$harvest_date <- n$`End date`

      k <- carobiner::read.excel(f, sheet="Soil_analysis")
      k <- k[,c("Abbreviture","Unit","Data1")]
      kk <- as.list(k$Data1)
      names(kk) <- k$Abbreviture
      r$soil_pH <- as.numeric(kk$SOILPH)
      r$soil_SOM <- as.numeric(kk$ORGM)
      r$soil_P_available <- as.numeric(kk$PHOS)
      r$soil_sand <- as.numeric(kk$SAND)
      r$soil_clay <- as.numeric(kk$CLAY)
      r$soil_silt <- as.numeric(kk$LIME)
      r$soil_CEC <- as.numeric(kk$SOILEC)
      r$soil_ex_Ca <- as.numeric(kk$EXCA2)
      r$soil_ex_Mg <- as.numeric(kk$EXMG2)
      r$soil_ex_K <- as.numeric(kk$EXK)
      r$soil_ex_Na <- as.numeric(kk$EXNA)
      r$soil_ex_Al <- as.numeric(kk$AL3H)
      r$soil_Zn <- as.numeric(kk$SOILZN)
      r$soil_B  <- as.numeric(kk$SOILB)
      r$soil_S  <- as.numeric(kk$SOILSULFATE)
      r$soil_N <- as.numeric(kk$SOILNITRATE)
      r$soil_Fe <- as.numeric(kk$SOILFE)
      r$soil_Ca <- as.numeric(kk$SOILCA)
      r
   }
   
   d <- lapply(ff, process) 
   d <- do.call(rbind, d)
   d$rep <- as.integer(d$rep)
   d$yield <- d$yield * 1000 ## kg/ha
   d <- d[!is.na(d$yield),] ## remove NA in yield
    
   d$country <- "Peru"
   d$trial_id <- as.character(as.integer(as.factor(d$adm3)))
   d$irrigated <- FALSE
   d$inoculated <- FALSE
   d$is_survey <- FALSE
   d$on_farm <- TRUE
   d$crop <- "potato"
   d$yield_part <- "tubers" 
   # fix lon coordinate
   d$longitude[d$longitude==-14.75289] <- -74.75289
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
   carobiner::write_files(meta, d, path=path)
   
}


