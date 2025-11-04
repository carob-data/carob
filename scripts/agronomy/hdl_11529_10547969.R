# R script for "carob"

carob_script <- function(path) {
  
"Farmers’ participatory researchers managed long-term trails aimed to improve the productivity, profitability, and sustainability of smallholder agriculture in the EGP by activities carried out to address the objectives:
1. Understand farmer circumstances with respect to cropping systems, natural and economic resources base, livelihood strategies, and capacity to bear risk and undertake technological innovation.
2. Develop with farmers more productive and sustainable technologies that are resilient to climate risks and profitable for small holders.
3. Facilitate widespread adoption of sustainable, resilient, and more profitable farming systems. (2018-02-18)"
  
  
  uri <- "hdl:11529/10547969"
  group <- "agronomy"
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
    project=NA, 
    publication= "doi:10.1016/j.fcr.2019.04.005", 
    data_organization = "CIMMYT;UBKV", 
    data_type="on-farm experiment", 
	  response_vars = "yield",
	  treatment_vars = "land_prep_method;crop_rotation",
    carob_contributor="Mitchelle Njukuya", 
    carob_date="2024-05-21"
  )
  
  
  ## process all Wheat and Maize -Purnea files  
  proc_data <- function(f) {
    
    suppressWarnings(r1 <- carobiner::read.excel.hdr(f, sheet ="4- Stand counts & Phenology", skip=4, hdr=3))
    colnames(r1) <- gsub("Date.of.harvest.dd.mm.yy", "Datw.of.harvest.dd.mm.yy", colnames(r1))
   
    d1 <- data.frame(
      code=r1$Tmnt, 
      trial_id=paste0(r1$Node, "_", r1$Site.No), 
      location=r1$Node,
      variety=r1$Variety,
      row_spacing=r1$Row.spacing.cm,
      crop=tolower(r1$Crop),
      planting_date=as.character(r1$Date.of.seeding.dd.mm.yy),
      harvest_date=as.character(r1$Datw.of.harvest.dd.mm.yy)
    )
    
    r2 <- carobiner::read.excel.hdr(f, sheet ="6 - Fertilizer amounts ", skip=4, hdr=3)
    
    d2 <- data.frame(
      code=r2$Tmnt,
      location=r2$Node,
      trial_id=paste0(r2$Node, "_", r2$Site.No),
      N_fertilizer=r2$N.kg.ha, 
      P_fertilizer=r2$P2O5.kg.ha / 2.29,
      K_fertilizer=r2$K2O.kg.ha /  1.2051,
      Zn_fertilizer=r2$ZnSO4.kg.ha,
      B_fertilizer=r2$Boric.acid.kg.ha.1
    ) 
    
    d2$fertilizer_type <- apply(r2[, grep("Application_Product.used", names(r2))], 1, 
                             \(i) paste(unique(i), collapse="; "))
    
    
    r3 <- carobiner::read.excel.hdr(f, sheet ="14 - Grain Harvest ", skip=4, hdr=3)	
    
    colnames(r3) <- gsub("Calculation_Calculation_Grain.yield.t.ha|Calculation_Grain.yield.t.ha|Calculation_Grain.Yield.t.ha", "Grain.yield.t.ha", colnames(r3))
                        
    d3 <- data.frame(
      code=r3$Tmnt,
      location=r3$Node,
      trial_id=paste0(r3$Node, "_", r3$Site.No), 
      yield=r3$Grain.yield.t.ha * 1000,
      fwy_residue=r3$Straw.yield.t.ha * 1000,
      dmy_total=r3$Biomass.t.ha * 1000
    )
    
    ## merge all 
    dd <- merge(d1, d2, by=c("code", "trial_id", "location"), all.x=TRUE)
    dd <- merge(dd, d3, by=c("code", "trial_id", "location"), all.x=TRUE) 
  }
  
  f0 <- grep("xlsx$", ff, value=TRUE)
  d0 <- lapply(f0, proc_data)

  d <- do.call(rbind, d0)
  
  
	d$country <- "India"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- TRUE
	d$yield_part <- "grain" 
	d$yield_part[d$crop == "lentil"] <- "seed"

	d$fertilizer_type <- gsub("MoP|MOP", "KCl", d$fertilizer_type)
	d$fertilizer_type <- gsub("UREA", "urea", d$fertilizer_type)
	d$fertilizer_type <- gsub("N:P:K|10-26-26|1899-12-31 10:26:26|:26:26", "NPK", d$fertilizer_type)
	d$fertilizer_type <- gsub("[0-9]|-|:|,", '', d$fertilizer_type)
	d$fertilizer_type[grep("boron", d$fertilizer_type, TRUE)] <- "borax"
	d$fertilizer_type[is.na(d$fertilizer_type)] <- "unknown"
	
  
  
	code = c("CTTPR-RL-JTJ", "CTTPR-RL-ZTJ", "UPTPR-RL-ZTJ", "CTTPR-CTL-ZTJ", "CTTPR-ZTL-ZTJ", 
		"UPTPR-CTL-ZTJ", "UPTPR-ZTL-ZTJ", "CTTPR-CTM", "UPTPR-ZTM", "CTTPR-ZTM", "UPTPR-CTM", "CTTPR-CTW", 
		"UPTPR-ZTW", "CTTPR-CTW-ZTJ", "CTTPR-ZTW-ZTJ", "UPTPR-CTW-ZTJ", "UPTPR-ZTW-ZTJ")
  
	rotation = c("rice;rice_lentil;jute", "rice;rice_lentil;jute","rice;rice_lentil;jute", "rice;lentil;jute", 
			"rice;lentil;jute", "rice;lentil;jute", "rice;lentil;jute", "rice;maize","rice;maize", "rice;maize", "rice;maize","rice;wheat", "rice;wheat", "rice;wheat;jute", "rice;wheat;jute", "rice;wheat;jute", "rice;wheat;jute" )

	treatname = c("conventional tillage transplanted puddled rice;rice_lentil;jute", 
				"conventional tillage transplanted puddled rice;rice_lentil;jute",
                "unpuddled transplanted rice;Rice_lentil;jute", 
				"conventional tillage transplanted puddled rice;lentil;jute", 
				"conventional tillage transplanted puddled rice;lentil;jute",
                "unpuddled transplanted rice;lentil;jute", 
				"unpuddled transplanted rice;lentil;jute", 
				"conventional tillage transplanted puddled rice;maize",
                "unpuddled transplanted rice;maize", 
				"conventional tillage transplanted puddled rice;maize", 
				"unpuddled transplanted rice;maize",
				"conventional tillage transplanted puddled rice;wheat",
                "unpuddled transplanted rice;wheat", 
				"conventional tillage transplanted puddled rice;wheat;jute", 
				"conventional tillage transplanted puddled rice;wheat;jute", 
                "unpuddled transplanted rice;wheat;jute", 
				"unpuddled transplanted rice;wheat;jute" )
  
  
	land_prep = c("puddled", "puddled", "not puddled", "puddled", "puddled", "not puddled", "not puddled", "puddled", "not puddled", "puddled", "not puddled",	"puddled", "not puddled", "puddled", "puddled", "not puddled", "not puddled" )
  
	trt <- cbind(code=code, treatment=treatname, crop_rotation=rotation, land_prep_method=land_prep)
	rownames(trt) <- NULL

	d <- merge(d, trt, by="code")
	d$code <- NULL
	d$planting_method = "transplanted"

	d$location <- gsub("DURGANAGAR","Durganagar", d$location)
	d$location <- gsub("MANSAI","Mansai", d$location)
	d$location <- gsub("GHUGHUMARI","Ghughumari", d$location)
	d$location <- gsub("PATCHARA","Patchara", d$location)
 
  
	geo <- data.frame(
		location=c("Durganagar", "Mansai", "Falimari", "Ghughumari", "Patchara"), 
		latitude=c(24.7696, 25.2347, 26.0617, 26.3339, 24.4878), 
		longitude=c(79.0309, 87.6779, 89.4739, 89.3261, 82.0019),
		geo_from_source=FALSE
	)
  
  d <- merge(d, geo, by="location", all.x = TRUE)  
  
  
	d$yield_moisture <- as.numeric(NA) #needs to be checked

  carobiner::write_files(meta, d, path=path)	
}


