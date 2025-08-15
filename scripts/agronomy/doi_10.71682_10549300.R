# R script for "carob"
# license: GPL (>=3)


carob_script <- function(path) {
  
  
"
Evaluating sustainable cropping systems: A comparative study of agricultural practices in Central Malawi
  
This database contains grain, biomasss and nutritional yield data from on-farm trials in Kasungu, Mchinji, and Lilongwe districts of central Malawi, conducted over three cropping seasons (2014–15 to 2016-17). The trials tested sustainable and resilient cropping systems, including Conservation Agriculture (CA) with minimum tillage, glyphosate herbicide, and maize-legume rotations, compared to conventional ridge-and-furrow sole maize (True farmer practice). The experiments were conducted on 24 farms, with each farm serving as one replicate. Data collected included grain yield, biomass, protein, and energy yields, providing insights into system performance and sustainability.
"
  
  
  uri <- "doi:10.71682/10549300"
  group <- "agronomy"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT;AUH",
		publication = "doi:10.1016/j.fcr.2024.109565",
		project = NA,
		data_type = "on-farm experiment",
		treatment_vars = "land_prep_method;crop_rotation",
		response_vars = "yield;dmy_total;grain_protein", 
		completion = 100,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2025-08-14",
		notes = NA, 
		design ="Randomized Block"
  )
  
  ## read data 
  f <- ff[basename(ff) == "grain_and_nutritional_yields_v00.xlsx"]
  r <- carobiner::read.excel(f, sheet ="Rotation.Data", na="NA")

  
  #converting season years to planting dates
#  r$year <- gsub("2014-15","2014",r$year)
#  r$year <- gsub("2015-16","2015",r$year)
#  r$year <- gsub("2016-17","2016",r$year)
  
  d <- data.frame(
    country = "Malawi",
    adm1="Central Region",
    adm2=r$district,
    planting_date= substr(r$year, 1, 4),
    season="wet",
    treatment=r$treatment_name,
    plot_id=r$plot_id,
    land_prep_method=r$tillage,
    crop=tolower(r$crop),
    plant_density=as.numeric(r$plant_population),
    yield=as.numeric(r$grain_yield),
    dmy_total=as.numeric(r$biomass),
    grain_protein=(as.numeric(r$crop_protein)/as.numeric(r$grain_yield))*100,
    yield_part="grain",
    crop_rotation=r$treatment_name
  )

  d$harvest_date <- as.character(as.numeric(d$planting_date) + 1)
  
  coords <- data.frame(
    adm2 = c("Kasungu", "Mchinji", "Mitundu"),
    longitude = c(33.48333, 32.88019, 33.77885),
    latitude = c(-13.03333, -13.79841, -14.24695),
    elevation= c(1048, 1182, 1207)
  )
  d <- merge(d, coords, by = "adm2", all.x=TRUE)
  
  d$crop_rotation <- carobiner::replace_values(d$crop_rotation,
           c("CA_Maize_Cowpea_Rotation", "CA_Maize_Groundnut_Rotation", "CA_Maize_Soybean_Rotation", "CA_Sole_Maize_Herbicide", "CA_Sole_Maize_No_Herbicide", "True_Farmer_practice"), 
			c("maize;cowpea", "maize;groundnut", "maize;soybean", "maize", "maize", "maize"))
							
  varieties <- c(
    maize = "MH26",
    soyabean = "Nasoko",
    groundnut = "Chitala groundnut7",
    cowpea = "IT82E16"
  )
  
  d$variety <- varieties[d$crop]
  d$land_prep_method[d$land_prep_method == "CA"] <- "none"
  d$land_prep_method[d$land_prep_method == "CP"] <- "conventional"
  d$crop <- gsub("soyabean", "soybean", d$crop)
  d$trial_id <- paste0(d$adm2, "_", d$planting_date)
  
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  d$geo_from_source <-FALSE
  d$inoculated <- FALSE

# values as reported in the associated publication
  d$yield_moisture <- 12.5
  d$P_fertilizer <- 21/2.29
  d$K_fertilizer <- as.numeric(NA)
  d$N_fertilizer <- 92
  d$S_fertilizer <-  4
  d$herbicide_used <- TRUE
  d$herbicide_product <- "glyphosate"
  d$herbicide_amount <- 3
  d$weeding_implement <- "hoe"
  d$weeding_times <- as.integer(3)
  d$row_spacing <- 75
  d$plant_spacing <- 25
  d$pest_species <- "Busseola fusca;Phyllophaga spp."
  
  #removing duplicates
  d <- d[!duplicated(d), ]
   
  carobiner::write_files(path, meta, d)
}
