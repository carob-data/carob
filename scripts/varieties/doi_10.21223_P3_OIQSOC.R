# R script for "carob"


carob_script <- function(path) {
   
"The objectives of this study were to describe an Acelerated breeding scheme, ABS, for sweetpotato and to investigate the efficiency of this breeding scheme for selecting high-yielding and well-adapted orange-fleshed sweetpotato (OFSP) cultivars with high β-carotene content. More than 198, 500 seeds from two crossing blocks were germinated and rapidly multiplied for evaluations in observation trials at four breeding locations in Mozambique. Breeding clones with storage root yields above 10 t/ha were advanced to preliminary and advanced yield trials across four sites and for three years. As a result, 64 high-yielding OFSP breeding clones were selected and evaluated in four mega-environments following a randomized complete block design with three replicates at Angónia, Chókwè, Gurué, and Umbelúzi. Field agronomic data and storage root quality data were collected. Data from multi-environment trials were subjected to single site and combined analysis of variance as well as to stability analysis using AMMI and regression."

   uri <- "doi:10.21223/P3/OIQSOC"
   group <- "varieties"
   ff <- carobiner::get_data(uri, path, group)

   meta <- carobiner::get_metadata(uri, path, group, major=1,minor=7, 
		publication="doi:10.1017/S002185961600099X", 
		project=NA, 
		data_organization="CIP;IIAM;SLU", 
		carob_contributor="Cedric Ngakou", 
		carob_date="2023-11-29", 
		response_vars = "yield",
		treatment_vars = "variety", 		
		data_type="experiment"
   )
   
   r <- carobiner::read.excel(ff[basename(ff)=="Multilocational trials with 64 clones at 4 sites .xls"])
   d <- r[, c("Year", "Locality", "Geno Name", "RYTHa", "Biomass", "RVY")]
   colnames(d) <- c("planting_date", "location", "variety", "yield", "dmy_total", "dmy_leaves") 
   
   ## add columns
   d$country <- "Mozambique"
   d$crop <- "sweetpotato" 
   d$row_spacing <- 90  # from doi:10.1017/S002185961600099X
   d$plant_spacing <- 30    
   
   d$trial_id <- paste(d$location, d$treatment, sep = "_")
   d$yield_part <- "roots"
   d$on_farm <- TRUE
   d$irrigated <- FALSE
   d$is_survey <- FALSE
   d$irrigated <- TRUE # see doi:10.1017/S002185961600099X
   
   ## fix yield value
   d$yield <- gsub("\\*", NA, d$yield)
   d$dmy_leaves <- gsub("\\*", NA, d$dmy_leaves)
   d$dmy_total <- gsub("\\*", NA, d$dmy_total)
   d <- d[!is.na(d$yield), ] ## remove NA in yield	
   
   # data type
   d$yield <- as.integer(d$yield)  
   d$dmy_total <- as.integer(d$dmy_total) 
   d$dmy_leaves <- as.integer(d$dmy_leaves) 
   d$planting_date <- as.character(d$planting_date)
   d$yield <- d$yield*1000 # in kg/ha
   d$dmy_total <- d$dmy_total*1000 # in kg/ha
   d$dmy_leaves <- d$dmy_leaves*1000 # in kg/ha
   
   # set the values out of bounds into NA
   d$dmy_leaves[d$dmy_leaves>20000] <- NA
   d$dmy_total[d$dmy_total>100000] <- NA
   ## add long and lat coordinate 
   geo <- data.frame(
			location=c("Angonia", "Chokwe", "Gurue", "Umbeluzi"), 
            longitude=c(34.1444739, 32.8598472, 36.9410455, 32.56745), 
            latitude=c(-14.7689832, -24.4886204, -15.4544621, -25.966213),
			geo_from_source = FALSE
		)
   
   d <- merge(d, geo, by="location", all.x = TRUE)
   
  #season: "October 2009-March 2010"  # from doi:10.1017/S002185961600099X
  d$planting_date <- paste(d$planting_date, "10", sep = "-")  
  d$harvest_date <- "2010-03" 

	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- 0

	d <- unique(d) 
	
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(path, meta, d)
}


