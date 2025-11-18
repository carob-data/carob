# R script for "carob"
# license: GPL (>=3)

## ISSUES
# I could not find the full meaning of IFC institute, or any info about Joon, Rajeev

carob_script <- function(path) {



"Data on performance of machine-led direct seeded rice (DSR) techniques compared to conventional rice transplanting in eastern India
  
Direct Seeded Rice (DSR) using seed-cum-fertilizer drill is an emerging rice establishment technique in India. How the two versions of DSR (zero-till & dust mulch) are comparable with the most common conventionally transplanted rice is less known. To fill the knowledge gap, CSISA in collaboration with Krishi Vigyan Kendra (KVK) conducted on-farm agronomic trial in eastern India covering different agro-climatic zones. The trial was conducted with 10 KVKs continuously for five years starting from 2017. Across the years and geographies, it was done on 1,315 farmers' fields. The dataset formed the foundational evidence to inform initial policy handles and invites researchers to plan further research. (2024-12-30)
"

	uri <- "hdl:11529/10549173"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT;CU;IRRI;IFC",
		publication =NA,
		project = NA,
		data_type = "on-farm experiment",
		treatment_vars = "land_prep_method;planting_method",
		response_vars = "yield", 
		completion = 100,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2025-11-18",
		notes = NA, 
		design = NA
	)
	
	f <- ff[basename(ff) == "CSISA_IND_DSR_Trial_2017-21_Data.xlsx"]
	r <- carobiner::read.excel(f)

	d <- data.frame(
		country = "India",
		planting_date=as.character(r$Year),
		crop="rice",
		adm1=r$State,
		adm2=r$District,
		adm3=r$Block,
		adm4=r$Village,
		longitude=r$Longitude,
		latitude=r$Latitude,
		treatment=r$Treatment,
		variety=r$Variety,
		seed_rate=r$SeedRate,
		transplanting_date=as.character(r$TransplantingDate),
		weeding_times=as.integer(r$ManualWeeding),
		irrigation_number=as.integer(r$IrrigationNumber),
		harvest_date=as.character(r$HarvestDate),
		dmy_total=r$BiomassYield,
		yield=r$GrainYield*1000,
		fertilizer_dap=as.character(r$TotalDAP),
		N_fertilizer=r$TotalNitrogen,
		P_fertilizer=r$TotalPhosphorus,
		K_fertilizer=r$TotalPotas,
		seed_weight=as.numeric(r$TestWeight)
		)
		
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- TRUE
	d$geo_from_source <- TRUE

#Not sure if the below conversions will be double counting	
#extracting elemental K from MOP
	elem_k <- (r$TotaMoP*0.60)/1.2051
  d$K_fertilizer <- r$TotalPotas+elem_k
#extracting elemental N from Urea
  d$N_fertilizer <- (r$TotalUrea*0.46)+ r$TotalNitrogen
  
  d$S_fertilizer <- d$lime <- as.numeric(NA)
	d$yield_part <- "grain"
	d$yield_moisture <- 0 #rice grain moisture is not stated in the dataset...how proceed?
	d$land_prep_method <- ifelse(d$treatment=="PTR(Check)","conventional","none")
	d$planting_method <- ifelse(d$treatment=="PTR(Check)","transplanted","direct seeding")
  d$latitude <- ifelse(d$adm4=="Ekamba",25.64,d$latitude)
  d$longitude<- ifelse(d$adm4=="Ekamba",86.14,d$longitude)
  d$trial_id <- paste(d$location, as.character(d$planting_date), sep = "_")
  d <- unique(d)
  
	carobiner::write_files(path, meta, d)
}
