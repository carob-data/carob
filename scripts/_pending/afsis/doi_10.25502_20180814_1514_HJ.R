# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Africa Soil Information System - Phase 1, Tuchila S1

The AFSIS project aimed to establish an Africa Soil Information system. Data was collected in sentinel sites across sub-Saharan Africa using the Land Degradation Surveilllance framework and inlcuded also multi-location diagnostic trials in selected sentiale sites to determine nutrient limitations and response to improved soil management practices (soil amendments).
"

	uri <- "doi:10.25502/20180814/1514/HJ"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA; ICRISAT; CIAT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "fertilizer_used;lime_used;OM_used",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-08-16",
		carob_completion = 90,	
		carob_effort = 6
	)
	

	f1 <- ff[basename(ff) == "tuchila_s1_field.csv"]
	f2 <- ff[basename(ff) == "tuchila_s1_plot.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)

	d1 <- data.frame(
	  site=r1$Site,
	  field_id=r1$FieldID,
	  latitude=r1$Flat,
	  longitude=r1$Flong,
	  location=r1$Village,
	  crop=tolower(r1$TCrop),
	  variety=r1$TCVariety,
	  previous_crop_residue_management=tolower(r1$CRM),
	  previous_crop=tolower(r1$PCrop1),
	  fertilization_method=r1$FertMet,
	  fertilizer_type=tolower(r1$FType2),
	  OM_type=paste(r1$MType1,r1$MType2,sep = ";"),
	  planting_date=r1$PlntDa,
	  thinning_date=r1$ThinDa,
	  emergence_date=r1$EmDate,
	  harvest_date=r1$HarvDa)
	  
	d2 <- data.frame(
	  field_id=r2$FieldID,
	  rep=as.integer(r2$Rep),
	  treatment=r2$TrtDesc,
	  harv_area=r2$Harea,
	  adj_harv_area=r2$AdjHarea,
	  dmy_residue=r2$AdjStoverYld,
	  #plant_density
	  fresh_cob_weight=r2$TCobFW,#kg
	  yield=r2$TGrainYld*1000,
	  seed_weight=r2$GrainDWbp,
	  bird_damage=as.character(r2$BirdD),
	  termite_damage=r2$TermiteD,
	  severity_scale="0-5"
	)
	
	d <- merge(d1,d2,by="field_id",all.x = T) 

	d$country <- "Malawi"
	d$on_farm <- NA
	d$is_survey <- FALSE 
	d$irrigated <- NA
	d$geo_from_source <- TRUE
  d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
	d$yield_part <- "grain"
	d$yield_moisture <- NA
	d$yield_isfresh <- NA
	
	d$fertilizer_used <- !d$treatment=="Control"
	d$lime_used <- d$treatment=="NPK+Lime"
	d$OM_used <- d$treatment=="NPK+MN"
	
	d$trial_id <- paste(d$site,1:nrow(d),sep = "_")
	d$planting_date <- as.character(as.Date(d$planting_date, format = "%m/%d/%y"))
	d$thinning_date <- as.character(as.Date(d$thinning_date, format = "%m/%d/%y"))
	d$emergence_date <- as.character(as.Date(d$emergence_date, format = "%m/%d/%y"))
	d$harvest_date <- as.character(as.Date(d$harvest_date, format = "%m/%d/%y"))
	
	d$plot_area <- ifelse(!is.na(d$adj_harv_area) & d$adj_harv_area, d$adj_harv_area,d$harv_area)

	#OM_type fix
	d$OM_type[d$OM_type == "None;None"] <- NA
	d$OM_type[d$OM_type == "Goat manure;Compost"] <- "animal dung;compost"
	d$OM_type[d$OM_type == "Manure;None"] <- "farmyard manure"	
	
	d$previous_crop <- gsub("\\s*-\\s*", ";", d$previous_crop)
	d$previous_crop <- gsub("ground nuts|groundnuts", "groundnut", d$previous_crop)
	d$previous_crop <- gsub("sweet potatoes","sweetpotato", d$previous_crop)
	d$fertilizer_type <-gsub("urea-d-compound-can","urea;D-compound;CAN", d$fertilizer_type) 
	d$fertilizer_type <-gsub("urea-can","urea;CAN", d$fertilizer_type) 
  
	d$harv_area <- NULL
	d$adj_harv_area <- NULL
	d[d == ""] <- NA
	
	 carobiner::write_files(path, meta, d)
}
