# R script for "carob"

## NOTES 
## yield is given in kg/plot but we do not know plot size 


carob_script <- function(path) {
   
"The objectives were to develop at least 3 superior cultivars to serve both producer and consumer needs. CIP used a bi-parental crossing block and an open pollinated poly-cross nursery at Gurué and Umbeluzi Research Stations (Mozambique to get seeds of a sweetpotato breeding population. A series of trials were conducted from 2011 to 2014 and 72 clones selected based on yield and nutrition for testing across sites (Chokwé in Gaza Province, Gurué in Zambezia and Umbeluzi in Maputo Province, Mozambique). Data were analyzed within and across locations. the clones were grouped into 3 classes; 27 clones OFSP, 25 purple fleshed and 20 dual purpose. The experimental layout of each MET was a randomized complete block design with four replications comprising 23 plants per plot for data recording. The data were analyzed using SAS (SAS Institute, Inc., 1997) for each MET separately and across respective MET set by an analysis of variance with a least significant difference comparison."
   
   uri <- "doi:10.21223/P3/DP0OYK"
   group <- "varieties" 
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=1,minor=7, 
      data_organization ="CIP", 
      publication ="doi:10.21273/HORTSCI.51.5.597",
      project =NA, 
      data_type = "experiment",
      response_vars = "yield;yield_marketable;fwy_residue",
      treatment_vars = "variety;variety_code", 
      carob_contributor = "Cedric Ngakou", 
      carob_date = "2024-10-10",
      design= "RCBD"
   )
   
   ff <- ff[grepl("Clones", basename(ff))]
   
  process <- function(f){
     r <- carobiner::read.excel(f, fix_names = TRUE)
     if(is.null(r$geno)) r$geno <- NA
     data.frame(
        location= r$l,
        rep= as.integer(r$r),
        variety= r$g,
        variety_code= r$geno,
        #plant_density= as.numeric(r$noph),
        yield_marketable= as.numeric(r$crw),
        yield= as.numeric(r$ncrw + r$crw) ,
        fwy_residue= as.numeric(r$vw),
        root_Fe= r$fe,
        root_Zn= r$zn,
        root_Ca= r$ca,
        root_Mg= r$mg,
        beta_carotene= r$bc,
        trial_id= gsub("xlsx", "", basename(f))
     )
  }
  
  d <- lapply(ff, process)  
  d <- do.call(rbind, d)   
   

   d$country <- "Mozambique"
   d$crop <- "sweetpotato"
   d$on_farm <- FALSE
   #plot_area <- NA
   d$irrigated <- NA
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "roots"
   
   ### Adding longitude and latitude 
   
   geo <- data.frame(
      location= c("Chokwe", "Gurue", "Umbeluzi"),
      latitude= c(-24.53142, -15.47157, -26.05315),
      longitude= c(32.98463, 36.98074, 32.35565)
   )
   
   d$geo_from_source <- FALSE
   
   d <- merge(d, geo, by="location", all.x = TRUE)
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
   carobiner::write_files (path, meta, d)
}


