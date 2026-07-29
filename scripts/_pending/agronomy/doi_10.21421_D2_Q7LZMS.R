# R script for "carob"
# license: GPL (>=3)

## ISSUES
#The dataset did not specify columns for each country. 
#NPK fertilizer rates and organic ammendment rates has not been specified
# The plant_height had out of bounds values (0, 651)
#heading_days, flowering_days, maturity_days,e_asp and ear_dam_co had some NA values in the dataset resulting in Warning message:NAs introduced by coercion

carob_script <- function(path) {

"
Effect of seed treatment and nutrient management on Millet and Sorghum establishment and yield in rainy season 2016

Over exploitation of soil and absence of fallow system expose soil to degradation. In addition climate variability is another threat. Under such conditions use of copping innovation may be the alternative. However in contact with other continents and regions, farmers in Sub-Saharan Africa and particularly in WCA use almost no production input. All this results in low yield a consequence of many years of mining agriculture to get good sustainable yield input must be used that would replace nutrient exported by crops. In addition soil organic pool build up is necessary to sustain soil quality and productivity.Poor plant stand is among the cause of low yield therefore optimal plant density is required which may be reached with adequate seed treatment.The combination of appropriate nutrient input, seed treatment and genotype will yield good and sustainable yield.        Experimental location on Google Maps - Ouahigouya, Burkina Faso    

      Experimental location on Google Maps - Maradi, Niger
"

	uri <- "doi:10.21421/D2/Q7LZMS"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRISAT",
		publication = "NA",
		project = NA,
		design = NA,
		data_type = NA,
		treatment_vars = "variety",
		response_vars = "seed_treatment;OM_used",
		carob_contributor = "Premrose Masunungure",
		carob_date = "2026-07-27",
		carob_completion = 70,	
		carob_effort = 6
	)
	

	f <- ff[basename(ff) == "Data file of Managment options to improve Millet and Sorghum productivity rainy season 2016.xlsx"]

	r <- carobiner::read.excel(f)

	r[r == "NA"] <- NA
	
	
	d <- data.frame(
	  country = c("Burkina Faso","Niger"),
	  plot_id = as.character(r$`Plot no`),
	  block_id = as.character(r$Block),
	  seed_treatment = r$`Seed treatment l`,
	  OM_used = r$`Organic amend l`,
	  fertilizer_used = r$`Minerals l`,
	  variety = r$`Millet genotype l`,
	  heading_days = as.numeric(r$E_50),
	  flowering_days = as.numeric(r$F_50),
	  maturity_days = as.numeric(r$M_50),
	  plant_height = r$`Hauteur moyenne plants`,
	  e_asp = as.numeric(r$`Proportion of Healthy heads`),
	  ear_dam_co = as.numeric(r$`Proportion of dammaged heads`),
	  dmy_residue = r$StoHvYld_M_gPlot,
	  yield = r$`Head yield`,
	  dmy_total = r$`Biomass total`,
	  crop = ifelse(r$`Millet genotype l` != "", "millet", "sorghum")
	)
	
	##the term striga_infested_hills is not abailable in carob
	#striga_infested_hills = r$`striga_nbre poquets_parcelle`,

	d$trial_id <- as.character(as.integer(as.factor(1)))
	
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA
	
	
	
	location <-  data.frame(
	  country = c("Burkina Faso", "Niger"),
	  adm1 = c("Nord", "Maradi"),
	  adm2 = c("Yatenga", NA),
	  adm3 = c("Ouahigouya", NA),
	  latitude = c(13.5891, 14.1162),
	  longitude = c(-2.436, 7.2992),
	  geo_uncertainty= c(18121, 157733 ),
	  geo_source = c("GADM 4.1, adm3", "GADM 4.1, adm1")
	)
	
	d <- merge(d, location, by = "country", all.x = TRUE)
	
	d$geo_from_source <- FALSE


	d$planting_date <- as.character(as.Date(NA))
	d$harvest_date  <- as.character(as.Date(NA))

	d$fertilizer_used <-NA
	d$fertilizer_used[d$'Minerals l'%in% c("No Min fert")] <- FALSE
	d$fertilizer_used[d$'Minerals l'%in% c("3g NPK per hill","6g NPK per hill","Recomm rate")] <- TRUE
	
	d$OM_used <-NA
	d$OM_used[d$'Organic amend l' %in% c("No Org Man", "Boadcast appl", "200g Man per hill")] <- TRUE
	
   
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA

	carobiner::write_files(path, meta, d)
}


