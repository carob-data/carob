# R script for "carob"
# license: GPL (>=3)


## ISSUES
# Trt_Code shows a third fertilizer level, NPK2 (16 rows), assuming it is the same as NPK
#
# Root weight columns do not speficy the crop
#
# No location below country level exists in the source.
# No planting date in the source - only harvestDate is available.

## NOTES
# "Sweet potato Monocrop" treatments had high density cassava. 
# This must be an error, and these cassava densities were set to zero.
#
# "75:20:90 NPK" treated as elemental kg/ha N:P:K (not oxide P2O5/K2O),


carob_script <- function(path) {
  
"
Cassava sweet potato intercropping in Tanzania

On-farm cassava/sweet potato intercropping (CIS) trials, rounds CIS-3, CIS-4,
and CIS-5, conducted in Tanzania under the ACAI project (IITA). 490 plots
across 108 trials tested sweet potato planting density (10,000/20,000/30,000
plants/ha) and planting time relative to cassava (simultaneous, or 2 weeks
after), with and without NPK fertilizer (75:20:90 kg/ha N:P:K), against
cassava and sweet potato sole-crop checks. Cassava marketable, diseased, and
small root fresh weights were recorded at harvest.
"

  uri <- "doi:10.25502/5qx6-wd06"
  group <- "agronomy"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "ACAI",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "intercropped;N_fertilizer",
		response_vars = "yield;yield_marketable;root_infection",
		notes = NA,
		carob_contributor = "Stella Muthoni",
		carob_date = "2026-07-15",
		carob_completion = 80,
		carob_effort = 3
  )
  
  f1 <- ff[basename(ff) == "cis_ckan.csv"]
  r <- read.csv(f1) |> unique() # removes one record
  
  ## f2 (cis_ckan_treatment.csv) and f3 (column_dictionary_acai_cis_forckan.csv)
  ## are reference/dictionary files, not data.
  
  tr1 <- do.call(rbind, strsplit(r$Trt_Code, "_"))
  tr2 <- do.call(rbind, lapply(strsplit(tr1[,2], "-"), \(x) {
	if (length(x) == 4) x 
	else if (length(x) == 3) c(x[1], NA, gsub("r", "", x[3]), x[2])
	else c(x[1], NA, NA, x[2])
   }))  
  
  trts <- data.frame(cbind(tr1[,2:1], tr2))
  names(trts) <- c("code", "trial", "crops", "planting", "rep", "fertilizer") 
  trts <- cbind(r["trial_ID"], trts)
  trts <- trts[order(trts$trial_ID), ]


  #table(trts$trial) # same as table(r$trial_Code). Not sure if it matters
  #CIS-3 CIS-4 CIS-5 
  #  243   153   101 
  
  #table(trts$crops) # matches table(r$cropping)
  # CS CSSP   SP 
  #139  311   47 
  
  # sweetpotato planted 0 or 2 weeks after cassava.
  # not reported for sole crop SP where it presumably should be 0WAP
  #table(trts$planting) 
  #0WAP 2WAP 
  # 213  145 

  #table(trts$fertilizer) # what is NPK2?
  #NPK0 NPK1 NPK2 
  # 377  104   16   
    
  
  d <- data.frame(
     trial_id = r$trial_ID,
	 rep = ifelse(is.na(trts$rep), 1, as.numeric(trts$rep)),
     plot_id = r$plot_ID,
     treatment = r$Trt_Code,
     country = r$country,
	 crop = ifelse(r$cropping == "Cassava Monocrop", "cassava",  
			 ifelse(r$cropping == "Sweet potato Monocrop", "sweetpotato", "cassava_sweetpotato")),
     intercropped = r$cropping == "Intercropped" 	 ,

 	 cassava_density = r$cassava_Density,
	 spotato_density = r$sweetPotatoPlantDensity,

  # assumed the weights provided are in tonne/ha... since planting density reported is per ha
	 yield = rowSums(r[, c("tuberizedMarketableRootsFW","tuberizedDiseasedRootsFW","tuberizedSmallRootsFW")], na.rm = FALSE) * 1000,
     yield_marketable = r$tuberizedMarketableRootsFW * 1000,
	 planting_date = NA,
     harvest_date = as.character(as.Date(r$harvestDate, format = "%d/%m/%Y")),
     yield_isfresh = TRUE,
     yield_moisture = NA,
     yield_part = "roots",    
     on_farm = TRUE,
     is_survey = FALSE,
     irrigated = NA,  
     longitude = NA,
     latitude = NA,
     geo_from_source = NA
  )
  
  # also matches `cropping = "Cassava Monocrop"`
  d$spotato_density[is.na(d$spotato_density)] <- 0
  # sweetpotato monocrop has no cassava, clearly an error in the data.
  d$cassava_density[d$crop == "sweetpotato"] <- 0

## fertilizer variables
## "control" -> 0 N/P/K; 
## "75:20:90 NPK" assumed to be elemental kg/ha N:P:K (not oxide form P2O5/K2O)
  d$N_fertilizer <- r$P_fertilizer <- r$K_fertilizer <- 0
  fu <- grepl("NPK", r$Fertilizer)
  d$N_fertilizer[fu] <- 75
  d$P_fertilizer[fu] <- 20
  d$K_fertilizer[fu] <- 90
  d$fertilizer_used <- fu
    
  # disease variable; % of total tuberized root weight that was diseased
  d$root_infection <- (r$tuberizedDiseasedRootsFW / d$yield) * 100

##  r$intercrops <- ifelse(r$intercropped, "sweetpotato", "none")
## r$intercrop_type <- ifelse(r$sweetPotatoPlanting == "together wth cassava", "mixed",
###                       ifelse(r$sweetPotatoPlanting == "2 weeks after cassava", "relay", NA)) # closest to planting timing
  
  carobiner::write_files(path, meta, d)
}
