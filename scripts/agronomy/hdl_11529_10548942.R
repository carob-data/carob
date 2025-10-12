# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {

 
"Data on mechanized crop establishment methods (Direct seeding by seed drill and transplanting by machine) and rice-fallow areas suitable for short duration pulses in Odisha
  
Two types of experiments conducted in multi-location on-farm trials to evaluate the mechanized crop establishment methods (Drill-Direct Seeded Rice and Mechanical Transplanted Rice) alternative to traditional crop establishment methods (Manual transplanted rice and broadcasting followed by beushening) in three districts of Odisha over three years (2017 to 2019). Two types of experiments were also conducted to evaluated the performance of short duration pulses or oilseeds in the rice-fallow areas for increasing the cropping intensity and system productivity. The yield data were collected manually from different treatments under each experiment over three years. We also combined multi-temporal Earth Observation (EO) data from Landsat-8 Operational Land Imager (OLI) and Sentinel-1 satellite sensors from 2018 to 2021 to identify rice-fallow areas and suitable rice-fallow areas for cultivation of short duration pulses and oilseeds. (2023-08-12)"

	uri <- "hdl:11529/10548942"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IRRI;CIMMYT;CU",
		publication = "doi:10.1016/j.fcr.2023.109078",
		project = NA,
		data_type = "on-farm experiment",
		treatment_vars = "land_prep_method;planting_method",
		response_vars = "yield", 
		completion = 100,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2025-10-09",
		notes = NA, 
		design = NA
	)

	f <- ff[basename(ff) == "CSISA_IND_OD_Rice_Fallow_2017-19.xlsx"]
	r1 <- carobiner::read.excel(f, na="na")
	r2 <- carobiner::read.excel(f, sheet ="Dry_season_crop")

	wet <- data.frame(
		country = "India",
		adm1="Odisha",
		adm2=carobiner::fix_name(r1$District, "title"),
		adm3=carobiner::fix_name(r1$Blck),
		location=carobiner::fix_name(r1$Village, "title"),
		planting_date=as.character(r1$Sow_date_ymd),
		variety=r1$Var,
		treatment=r1$Treat_Desc,
		seed_rate=r1$Seed_rate,
		harvest_date=as.character(r1$Harv_date_ymd),
		yield=r1$GrYld_Tha*1000,
		crop_rotation=NA
	)
		
	dry <- data.frame(
	  country = "India",
	  adm1="Odisha",
	  adm2=carobiner::fix_name(r2$District, "title"),
	  adm3=carobiner::fix_name(r2$GP, "title"),
	  location=carobiner::fix_name(r2$Village, "title"),
	  planting_date=r2$Year,
	  variety=r2$Var,
	  treatment=r2$Tret_detail,
	  seed_rate=NA,
	  harvest_date=NA,
	  yield=r2$GrYld_Tha*1000,
	  crop_rotation=tolower(r2$Crop_Sys)
	)

	d <- rbind(wet, dry)
	d$crop <- "rice"
	d$trial_id <- paste(d$location, as.character(d$planting_date), sep = "_")
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- TRUE
	d$geo_from_source <- FALSE
	loc <- data.frame(adm2 = c("bhadrak","mayurbhanj","cuttack","Mayurbhanj","Bhadrak", "Cuttack"),
	                  longitude =c(86.498, 86.5738, 85.8886, 86.5738, 86.498, 85.8886),
	                  latitude=c(21.0666, 21.8103, 20.435, 21.8103, 21.0666, 20.435))
	d <- merge(d,loc, by="adm2", all.x=TRUE)
  
	#Publication data
	d$P_fertilizer <- 40
  d$K_fertilizer <- 40
  d$N_fertilizer <- 80
  d$fertilizer_type <- "DAP;KCl;ZnSO4;urea"
  d$S_fertilizer <-  d$lime <- as.numeric(NA)
	d$yield_part <- "grain"
	d$yield_moisture <- 14
  d$crop_rotation <- gsub("-",";",d$crop_rotation)
  d$crop_rotation <- gsub("greengram","mung bean", d$crop_rotation)
  d$crop_rotation <- gsub("blackgram","black gram",d$crop_rotation)
  d$crop_rotation <- gsub("toria","mustard",d$crop_rotation)
  
  #renaming values in planting_method
  trt <- c("Beushening",
           "Mechanical puddled transplanted Rice (PTR-M)",
           "Dry-direct seeded rice (DSR)",
           "Manual random puddled transplanted rice (PTR-R)",
           "Manual line puddled transplanted rice (PTR-L)")
  
  plant_mthd <- c("direct seeding", # beushening is direct seeding with post-emergence tillage
           "transplanted",
           "direct seeding",
           "transplanted",
           "transplanted")

  land_mthd <- c("post-emergence tillage",
           "mechanical puddling",
           "none",
           "manual puddling",
           "line puddling")

  it <- match(d$treatment, trt)
  d$planting_method <- plant_mthd[it]  
  d$land_prep_method <- land_mthd[it]  

  d$harvest_date[d$harvest_date == "2018-01-12"] <- "2018-12-12"
  d$harvest_date[d$harvest_date == "2018-07-12"] <- "2018-12-07"

  d <- unique(d)  
	carobiner::write_files(path, meta, d)
}
	 