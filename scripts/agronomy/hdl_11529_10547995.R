# R script for "carob"

carob_script <- function(path) {
  
"Farmer participatory on-farm trials with CA technologies comparing with farmers’ practices (CT), were conducted in several fields in each community. Likewise, farmer-participatory alternative cropping systems trials were conducted comparing to existing systems and to find out suitable and more profitable cropping systems, prioritized to increase visibility and to avoid implementation and management problems that emerge when utilizing small plots with significant edge effects. Most trials were replicated in several fields within each community and were farmer-managed with backstopping from project staff and NARES partners. Project partners and staff coordinated monitoring and data acquisition. Where possible, collaborating farmers were selected by the community, and the project worked with existing farmer groups, with groups of both men and women farmers."
  
  uri <- "hdl:11529/10547995"
  group <- "agronomy"
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
    project=NA, 
    publication= NA, 
    data_organization = "CIMMYT;ICAR;RDRS;BARI", 
    data_type="on-farm experiment", 
    carob_contributor="Mitchelle Njukuya", 
    carob_date="2024-04-30",
		treatment_vars=NA, 
		response_vars="yield"	
  )
  
  
  ## process all -Rangpur files  
  proc_data <- function(f) {
    
    r1 <- carobiner::read.excel.hdr(f, sheet ="4- Stand counts & Phenology", skip=4, hdr=3)
   
    d1 <- data.frame(
      treatment=r1$Tmnt, 
      trial_id=paste0(r1$Node, "_", r1$Site.No), 
      location=r1$Node,
      variety=r1$Variety,
      row_spacing=r1$Row.spacing.cm,
      crop=tolower(r1$Types.of.Trial),
      planting_date=as.character(r1$Date.of.seeding.mm.dd.yy),
      harvest_date=as.character(r1$Datw.of.harvest.mm.dd.yy)
    )
    d1$row_spacing[d1$row_spacing == 0] <- NA
	
    r2 <- carobiner::read.excel.hdr(f, sheet ="6 - Fertilizer amounts ", skip=4, hdr=3)
    
    colnames(r2) <- gsub("K.kg.ha","K2O.kg.ha",colnames(r2))
    colnames(r2) <- gsub("Zn.kg.ha","ZnSO4.kg.ha",colnames(r2))
    
    d2 <- data.frame(
      treatment=r2$Tmnt,
      location=r2$Node,
      trial_id=paste0(r2$Node, "_", r2$Site.No),
      N_fertilizer=r2$N.kg.ha, 
      P_fertilizer=r2$P2O5.kg.ha / 2.29,
      K_fertilizer=r2$K2O.kg.ha /  1.2051,
      Zn_fertilizer=r2$ZnSO4.kg.ha
    ) 
    
    d2$fertilizer_type <- apply(r2[, grep("Application_Product.used", names(r2))], 1, 
                                \(i) paste(unique(i), collapse="; "))
    
    
    r3 <- carobiner::read.excel.hdr(f, sheet = "14 - Grain Harvest ", skip=4, hdr=3)	
    
    colnames(r3) <- gsub("Dry.Straw.yield.t.ha", "Straw.yield.t.ha", colnames(r3))
    
    
    d3 <- data.frame(
      treatment=r3$Tmnt,
      location=r3$Node,
      trial_id=paste0(r3$Node, "_", r3$Site.No), 
      yield=r3$Calculation_Grain.yield.t.ha * 1000,
      fwy_residue=r3$Straw.yield.t.ha * 1000,
      dmy_total=r3$Biomass.t.ha * 1000
    )
    
    ## merge all 
    dd <- merge(d1, d2, by=c("treatment", "trial_id","location"), all.x=TRUE)
    dd <- merge(dd, d3, by=c("treatment", "trial_id","location"), all.x=TRUE) }
  
  
  d <- lapply(grep(".xlsx", ff, value=TRUE), proc_data)
  d <- do.call(rbind, d)
  
  d$country <- "Bangladesh"
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- TRUE
  d$yield_part <- "grain" 
  d$yield_part[d$crop == "potato"] <- "tubers"
  d$yield_part[d$crop == "mustard"] <- "seed"
  d$fertilizer_type <- gsub("MOP", "KCl", d$fertilizer_type)
  d$fertilizer_type <- gsub("Urea", "urea", d$fertilizer_type)
  d$fertilizer_type <- gsub("Gypsum", "gypsum", d$fertilizer_type)
  d$fertilizer_type[is.na(d$fertilizer_type)] <- "unknown"

  d$crop <- gsub("rabi maize", "maize", d$crop)
  d$location <- gsub("Borodorgha|Borodargha","Borodarga", d$location)
  d$location <- gsub("Mohonpur","Mohanpur", d$location)
  d$location <- gsub("Kolkondo","Kolkonda", d$location)
  d$location <- gsub("Durgarpur","Durgapur", d$location)
  
  geo <- data.frame(
		location=c("Borodarga", "Mohanpur", "Kolkonda" , "Lakkhitari", "Durgapur"), 
        latitude=c(25.7011, 25.9342, 25.8683, 25.88404, 25.55461), 
        longitude=c(89.3436, 88.6928, 89.2044, 89.25188, 89.29195),
		geo_from_source=FALSE
	)
  
  d <- merge(d, geo, by="location", all.x = TRUE)  
  
  carobiner::write_files(meta, d, path=path)	
}


