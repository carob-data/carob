# R script for "carob"


carob_script <- function(path) {
   
"In the Agoro improved irrigation scheme, the existing practice is that paddy rice fields are alternated with fallow period of about 5 months before planting is resumed. During the fallow periods, there is prolific re-growth of mixed weeds, grasses and shrubs which become part of local grazing pastures during the dry season. However, this re-grown vegetation also significantly increases the work required to prepare the land again for rice the next season. A rotation of rice-sweetpotato-rice might ease field management for both crops, reduce the costs of rice production as well as provide early planting material for sweetpotato upland growing. A study was initiated in Agoro rice scheme during the 1st season of 2016 to determine the effect of sweetpotato-rice seed crop rotation on rice seed purity, sweetpotato pest and disease prevalence, sweetpotato root and vine yield, and the cost-benefit ratio of different rotation options for providing basic seed in a timely manner to sweetpotato vine multipliers. Treatments included crop rotations of sweetpotato-rice–sweetpotato; rice-sweetpotato-rice; rice-rice-rice and sweetpotato-sweetpotato-sweetpotato. The study was set up using Randomised Complete Block Design (RCBD) with four replicates with sweetpotato varieties NASPOT 11 (Cream fleshed), NASPOT 10 and Ejumula (both orange fleshed) and rice varieties New WITA 9, Okile and Komboka. Data were collected on incidence and severity of SPVD and Alternaria blight, weevil infestation, plant vigour, root and vine weight (for sweetpotato), plant height, number of productive tillers, flowering, grain yield and rice biomass dry weight at harvest (for rice). (2016)"
   
   uri <- "doi:10.21223/P3/RBR0FG"
   group <- "agronomy" 
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1, 
      data_organization ="CIP;NARO", 
      publication = "http://www.sweetpotatoknowledge.org/wp-content/uploads/2017/10/SA10-Rice-sp-Rice-rotation-agronomic-benefits-and-profitability-analysis.pdf",
      project =NA, 
      data_type = "experiment",
      response_vars = "yield;yield_marketable;fwy_residue",
      treatment_vars = "variety;crop_rotation", 
      carob_contributor = "Cedric Ngakou", 
      carob_date = "2024-10-02",
      notes= "rice data are not available",
	  design= "RCBD"
   )
   
   f <- ff[basename(ff)=="Final data_Sweetpotato-Rice Rotation.xls"]
   
   ### Processing data
      r <- carobiner::read.excel(f)
      
      d <- data.frame(
         season= r$Season,
         variety= r$Variety,
         treatment= r$Treatment,
         rep= as.integer(r$Replication),
         fwy_residue= (r$`Wt Vines (T/ha)`)*1000,
         yield_marketable= r$RCTHA*1000,
         yield= (r$`T-Roots-H/ha`)*1000,
         diseases= "alternaria blight",
         disease_severity= as.character(r$`ALT1, AS1`),
         virus_severity=  as.character(r$SPVD),
         severity_scale= "1-9",
         crop_rotation= ifelse(grepl("Control", r$Treatment), "none", "rice"),
         location= "NaCRRI",
         planting_date= gsub("A|B", "", r$Season),
         trial_id= r$Season
      )
   
   
   d$country <- "Uganda"
   d$crop <- "sweetpotato"
   d$longitude <- 32.61234
   d$latitude <- 0.528956
   d$geo_from_source <- FALSE
   d$on_farm <- TRUE
   d$irrigated <- NA
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$yield_part <- "roots"
   

   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files (path, meta, d)
}


