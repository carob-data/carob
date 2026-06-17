# R script for "carob"

# ## ISSUES 

#RH: perhaps we should capture more managment variables 


# ....


carob_script <- function(path) {
  
  "
Farmers’ participatory researchers managed long-term trails aimed to improve the productivity, profitability, and sustainability of smallholder agriculture in the EGP by activities carried out to address the objectives: 1. Understand farmer circumstances with respect to cropping systems, natural and economic resources base, livelihood strategies, and capacity to bear risk and undertake technological innovation. 2. Develop with farmers more productive and sustainable technologies that are resilient to climate risks and profitable for small holders. 3. Facilitate widespread adoption of sustainable, resilient, and more profitable farming systems. (2018-02-17)

  "
  
  uri <- "hdl:11529/10548076"
  group <- "conservation_agriculture"
  ff	<- carobiner::get_data(uri, path, group)
 
  meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
    project="Rabi (winter) crops-all nodes-Long term trial (LT)-Rangpur-Bangladesh",
    publication= NA,
    data_organization = "CIMMYT",
    data_type="on-farm experiment",
    carob_contributor="Fredy Chimire",
    carob_effort = NA,
    carob_date="2023-11-22"
  )
  
  
  ff <- ff[grep("Rangpur", basename(ff))] # Taking Wheat and maize files

  
  get_raw_data <- function(f) {
    r1 <- carobiner::read.excel.hdr(f, sheet ="4- Stand counts & Phenology", skip=4, hdr=2)
    r2 <- carobiner::read.excel.hdr(f, sheet ="14 - Grain Harvest ", skip=4, hdr=2)
    r3 <- carobiner::read.excel.hdr(f, sheet ="6 - Fertilizer amounts ", skip=4, hdr=2)
    
    nms <- c("Site.No", "Tmnt","Sun.dry.grain.yield.t.ha)", "Total.Biomass.t.ha", "Biomass.t.ha")
    r2 <- r2[, nms]
    
    #colnames(r3) <- gsub("Kg.ha_N.kg.ha", "N.kg.ha", colnames(r3))
    #nms <- c("Site.No", "Tmnt", "Urea.kg.ha","TSP.kg.ha","N.kg.ha","MOP.kg.ha", "P2O5.kg.ha", "K2O.kg.ha", "Gypsum.kg.ha", "ZnSO4.kg.ha", "Boric.acid.kg.ha", grep("Product.used", names(r3), value=TRUE))
    
    nms <- c("Site.No", "Tmnt", "N.kg.ha", "P2O5.kg.ha", "K2O.kg.ha", "Gypsum.kg.ha", "ZnSO4.kg.ha", "Boric.acid.kg.ha", grep("Product.used", names(r3), value=TRUE))
    
    r3 <- r3[, nms]
    
    r <- merge(r1, r2, by=c("Site.No", "Tmnt"))
    merge(r, r3, by=c("Site.No", "Tmnt"))
  }
  
  
  #### about the data #####
  
  process_data <- function(r) {
    
    d <- data.frame(trial_id = as.character(r$Trial.Code), season = r$Season,
                    crop=tolower(r$Crop), variety= r$Variety, 
                    treatment = r$Tmnt, 
                    yield = r$Sun.dry.grain.yield.t.ha * 1000,	
                    biomass_total = r$Total.Biomass.t.ha * 1000,
                    fwy_residue = (r$Total.Biomass.t.ha-r$Sun.dry.grain.yield.t.ha) * 1000,
                    N_fertilizer = r$N.kg.ha,
                    P_fertilizer = r$P2O5.kg.ha / 2.29,
                    K_fertilizer = r$K2O.kg.ha / 1.2051,
                    B_fertilizer = r$Boric.acid.kg.ha * 0.1748,
                    row_spacing = r$Row.spacing.cm,
                    site = paste("site ", r$Site.No),
                    country= "Bangladesh",
                    adm2 = "Rangpur", # district provided in the excel
                    S_fertilizer= 0,
                    Zn_fertilizer= r$ZnSO4.kg.ha,
                    lime =0, gypsum =r$Gypsum.kg.ha
    )
    
    d$irrigated <- TRUE
    d$fertilizer_type <- apply(r[, grep("Product.used", names(r), value=TRUE)], 1, 
                               function(i) {
                                 i <- gsub("Urea.*", "urea", i[!is.na(i)])
                                 i <- gsub("Gypsum.*", "gypsum", i[!is.na(i)])
                                 i <- gsub("MOP", "KCl", i[!is.na(i)])
                                 i <- gsub("Muriate", "KCl", i[!is.na(i)])
                                 paste(unique(i[!is.na(i)]), collapse="; ")
                               })
    
    i <- grep("Date.of.seeding", names(r))
    d$planting_date = as.character(as.Date(r[,i]))
    i <- grep("Dat..of.harvest", names(r))
    d$harvest_date = as.character(as.Date(r[,i]))			
    
    if (is.character(d$row_spacing)) {
      d$row_spacing <- gsub("30-40", "35", d$row_spacing)
      d$row_spacing <- gsub("40-50", "45", d$row_spacing)
      d$row_spacing <- as.numeric(d$row_spacing)
    }
    
    i <- grep("Date.of.50.anthesis", names(r))
    if (length(i) != 0) {
      d$anthesis_date = as.character(as.Date(r[,i]))
    }
    d	
  }
  
  
  fun <- function(f) {
    #print(basename(f)); flush.console()
    r <- get_raw_data(f)
    d <- process_data(r)
    
    if (grepl("Bhokraha", f)) {
      d$latitude <- 26.592 # https://www.gps-coordinates.net/
      d$longitude <- 87.103
      d$location <- "Bhokraha"
      
    } else if (grepl("Durgapur", f)) {
      d$latitude <- 26.592 # https://www.gps-coordinates.net/
      d$longitude <- 87.103
      d$location <- "Durgapur"}
    else {
      d$latitude <- 26.592 # https://www.gps-coordinates.net/
      d$longitude <- 87.103}
    
    
    crop <- tolower(strsplit(basename(f), "-")[[1]][1])
    d$yield_part <- "grain"
   
    d
  }
  
  dd <- lapply(ff, fun)
  dd <- do.call(rbind, dd)
  
  #carobiner::write_files(meta, dd, path=path)
}
