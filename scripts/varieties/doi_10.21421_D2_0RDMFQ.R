# R script for "carob"


carob_script <- function(path) {
   
" This database includes the research work carried out on regional groundnut varieties focusing on the identification of stable Valencia genotypes and stratification of environments in the region for targeted breeding. The experiments were conducted in Malawi in 2013 to 2016 cropping seasons. Groundnut is a very important food and cash crop in Eastern and Southern Africa with varying multiple uses. Groundnut (Arachis hypogaea L., AABB, 2n=4x=40) is a self-pollinated crop and is an annual crop distinguished from most other legume crop species by producing aerial flowers and fruiting below ground. The groundnut breeding program in Malawi is the regional hub that supplies Eastern and Southern Africa Region with improved resilient varieties. The program makes over 150 new crosses per year targeting different market niches which are then advanced culminating in adaptability trials across the region. Based on their agronomic performance in advanced and regional yield trials for yield and other traits such as disease resistance, the superior genotypes will be selected and advanced further in National performance trials and farmer participatory variety selection in preparation for release and commercialization and or wider use by farmers. The environmental stratification achieved from this study will help in reduction of phenotyping cost by conducting experiments in fewer representative environments.The agronomic data (grain yield) of the Valencia genotypes evaluated is hereby presented, that helped in dissecting the stability status of the genotypes across environments and at the same time stratification of environments into mega-environments. The most adapted and stable genotypes identified can be advanced for release in the respective country regions and or use in breeding programs for further genetic enhancement. The mega-environments identified can be used for early generation screening and at the same time to acquire data for release in the region as a measure to save resources "  
  
    uri <- "doi:10.21421/D2/0RDMFQ"
   group <- "varieties" 
   ff  <- carobiner::get_data(uri, path, group)
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0, 
      data_organization = "ICRISAT", 
      publication ="doi:10.21475/ajcs.19.13.12.p2039", 
      project = NA, 
      data_type = "experiment",
      treatment_vars = "variety", 
      response_vars = "yield",
      carob_contributor = "Cedric Ngakou", 
      carob_date = "2024-07-28"
   )
   
   r <- carobiner::read.excel(ff[basename(ff)=="Data file of stability and adaptability of valencia Groundnut genotypes.xlsx"])
   
   d <- data.frame(
      rep =  as.integer(r$`Replication number`),
      variety = r$Genotype,
      yield = r$SY_Calc_kgha,
      location= r$SiteSeason,
      trial_id= r$Codes
   )
   
   d$crop <- "groundnut"
   d$irrigated <- as.logical(NA) 
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "seed"
   d$plant_spacing <- 10 #cm
   d$row_spacing <- 75 #cm
   d$plot_length <- 2.25 #m
   d$plot_width <- 6 #m
   d$plot_area <- 13.5 #m2
   
   d$planting_date[grep("13", d$location)] <- "2013"
   d$planting_date[grep("14", d$location)] <- "2014"
   d$planting_date[grep("15", d$location)] <- "2015"
   d$planting_date[grep("16", d$location)] <- "2016"
   d$location <- gsub("1|2|3|4|5|6| ", "", d$location)
   
## Adding longitude and latitude
   ## More clarification on location and country could be found on the publication (doi: 10.21475/ajcs.19.13.12.p2039)
   geo <- data.frame(
      location=c("Nachingwea", "Naliendele", "Tumbi", "Serere", "Namapa", "Nampula", "Ngabu", "Chitedze", "Baka", "Msekera"),
      latitude= c(-10.3291321, -10.3647106, 1.0700784, 1.4916989, -13.7173641, -14.966969, -16.4584355, -13.9788154, -12.4048405, -13.6452874),
      longitude=c(38.4689187, 40.1625663, 34.2424799, 33.3568368, 39.8185303, 39.2707752, 34.8866104, 33.6538127, 32.1760991, 32.5638197),
      country=c(rep("Tanzania",2), rep("Uganda",2), rep("Mozambique",2),rep("Malawi",2),rep("Zambia",2))
   )
   d$geo_from_source <- FALSE
   d <- merge(d,geo,by="location",all.x = TRUE)
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files (path, meta, d)    
}
