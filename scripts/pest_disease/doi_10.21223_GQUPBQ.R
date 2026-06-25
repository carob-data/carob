# R script for "carob"


carob_script <- function(path) {
   
"In the 2020-2021 season, three clones with high levels of resistance to late blight were evaluated in adaptation and efficiency tests for tuber yield, dry matter content, frying and baking quality throughout Peru in six locations, compared Two varieties planted by farmers and very well accepted by end consumers, Canchan and Unica, are currently also used for frying in sticks, but without stability in all crops due to the genotype x environment interaction. The randomized complete block design was used with three replications of 150 plants, the fertilization dose was 200-220-180 Kg of NPK, using potassium sulfate as a source of potassium to improve frying quality. At harvest, samples were taken to determine the dry matter, reducing sugar content, traditional and blanched fixture color, and baking quality. The clone was equal to or superior to the check for the yield of tubers, it presented good quality of frying color in all localities compared to the control varieties that did not present good quality of frying color in all localities, It is expected to complete all the documents requested by the Peruvian Seed Authority (SENASA) to be registered as a new potato variety with resistance to late blight and quality for frying and/or baking. These experiments correspond to the second year"

	uri <- "doi:10.21223/GQUPBQ"
	group <- "pest_disease"
	ff  <- carobiner::get_data(uri, path, group)
   
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP",
		publication= NA,
		project=NA,
		data_type= "experiment",
		response_vars = "yield",
		treatment_vars = "variety;longitude;latitude",
		carob_contributor= "Cedric Ngakou",
		carob_effort = NA,
		carob_date="2024-06-10"
	)

      r <- carobiner::read.excel(ff[basename(ff)=="PTYield2020-2021_Combinado_CIP396026.101_exp.xlsx"], skip=9)
      
      d <- data.frame(
         country="Peru",
         crop="potato",
         variety= r$Clone,
         rep= as.integer(r$Rep),
         yield=r$MTYNA*1000, # to kg/ha
         AUDPC= r$AUDPC / 100,
         location=r$...39,
         on_farm= TRUE,
         irrigated= FALSE,
         inoculated= FALSE,
         yield_part= "tubers",
         trial_id= "1"         
      )

   ## drop row with bad value in location and variety
   ## All NA locations contain a bad value entered in the variety   
   d <- d[!is.na(d$location),] 

   ## Get planting and harvest date from data description of each file 
    dta <- data.frame(
		location=c("Uñigan","Licame","Yanac", "Chinchao", "Jauja","Majes"),
        latitude=c(-6.5516594, -7.7667731, -7.8154676, -9.6146422, -11.7751615, -16.3242988),
        longitude=c(-78.3891572, -77.834892, -78.0487063, -76.1287916, -75.5000068, -72.2878298),
        planting_date=c( "2020-11-05", "2020-11-18", "2020-11-23", "2020-11-07", "2020-11-20", "2021-02-17"),
        harvest_date=c("2021-05-10", "2021-05-08", "2021-05-10", "2021-04-14","2021-05-12", "2021-06-17")
	)  
   d$geo_from_source <- FALSE
	d <- merge(d, dta, by="location", all.x = TRUE)
   
   ## From data description 
	d$N_fertilizer <- 200   
	d$P_fertilizer <- 220/2.29
	d$K_fertilizer <- 180/1.2051

	d$pathogen <- "Phytophthora infestans"
	d$diseases <- "potato late blight"
   d$is_survey <- FALSE

   
	carobiner::write_files(path, meta, d)   
}

