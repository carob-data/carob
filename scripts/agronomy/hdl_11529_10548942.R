# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {
#ISSUES
#1. Toria crop (Brassica campestris var.) not included in the crop values
  

"Data on mechanized crop establishment methods (Direct seeding by seed drill and transplanting by machine) and rice-fallow areas suitable for short duration pulses in Odisha
  
Two types of experiments conducted in multi-location on-farm trials to evaluate the mechanized crop establishment methods (Drill-Direct Seeded Rice and Mechanical Transplanted Rice) alternative to traditional crop establishment methods (Manual transplanted rice and broadcasting followed by beushening) in three districts of Odisha over three years (2017 to 2019). Two types of experiments were also conducted to evaluated the performance of short duration pulses or oilseeds in the rice-fallow areas for increasing the cropping intensity and system productivity. The yield data were collected manually from different treatments under each experiment over three years. We also combined multi-temporal Earth Observation (EO) data from Landsat-8 Operational Land Imager (OLI) and Sentinel-1 satellite sensors from 2018 to 2021 to identify rice-fallow areas and suitable rice-fallow areas for cultivation of short duration pulses and oilseeds. (2023-08-12)"

	uri <- "hdl:11529/10548942"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IRRI;CIMMYT;CU",
		publication = "doi.org/10.1016/j.fcr.2023.109078",
		project = NA,
		data_type = "on-farm experiment",
		treatment_vars = "land_prep_method",
		response_vars = "yield", 
		completion = 100,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2025-10-09",
		notes = NA, 
		design = NA
	)

	f <- ff[basename(ff) == "CSISA_IND_OD_Rice_Fallow_2017-19.xlsx"]
	r <- carobiner::read.excel(f)
	r2<- carobiner::read.excel(f, sheet ="Dry_season_crop")

	wet <- data.frame(
		country = "India",
		adm1="Odisha",
		adm2=tolower(r$District),
		adm3=tolower(r$Village),
		location=tolower(r$Blck),
		planting_date=as.numeric(r$Sow_date_ymd),
		variety=r$Var,
		treatment=r$Treat_Desc,
		seed_rate=r$Seed_rate,
		harvest_date=r$Harv_date_ymd,
		yield=r$GrYld_Tha*1000,
		crop_rotation=NA)
		
	wet$planting_date<- as.Date(wet$planting_date, origin = "1899-12-30")
	
	dry <- data.frame(
	  country = "India",
	  adm1="Odisha",
	  adm2=r2$District,
	  adm3=r2$Village,
	  location=r2$GP,
	  planting_date=r2$Year,
	  variety=r2$Var,
	  treatment=r2$Tret_detail,
	  seed_rate=NA,
	  harvest_date=NA,
	  yield=r2$GrYld_Tha*1000,
	  crop_rotation=tolower(r2$Crop_Sys))

	d<- rbind(wet,dry)
	d$crop <- "rice"
	d$trial_id <- paste(d$location, as.character(d$planting_date), sep = "_")
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- TRUE
	d$geo_from_source <- FALSE
	loc <- data.frame(adm2 = c("bhadrak","mayurbhanj","cuttack","Mayurbhanj","Bhadrak", "Cuttack"),
	                  longitude =c(86.498, 86.5738, 85.8886, 86.5738, 86.498, 85.8886),
	                  latitude=c(21.0666, 21.8103, 20.435, 21.8103, 21.0666, 20.435))
	d<- merge(d,loc, by="adm2",all.x = T)
  
	#Publication data
	d$P_fertilizer <- 40
  d$K_fertilizer <- 40
  d$N_fertilizer <- 80
  d$fertilizer_type <- "DAP;KCl;ZnSO4;urea"
  d$S_fertilizer <-  d$lime <- as.numeric(NA)
	d$yield_part <- "grain"
	d$yield_moisture <- 14
  d$harvest_date <- as.character(d$harvest_date)
  d$planting_date <- as.character(d$planting_date)
  d$crop_rotation <- gsub("-",";",d$crop_rotation)
  d$crop_rotation <- gsub("greengram","mung bean", d$crop_rotation)
  d$crop_rotation <- gsub("blackgram","black gram",d$crop_rotation)
  d <- unique(d)
  
	carobiner::write_files(path, meta, d)
}
