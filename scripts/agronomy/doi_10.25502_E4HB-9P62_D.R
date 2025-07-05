# R script for "carob"

"Title: N2Africa agronomy trials - Rwanda, 2010
  
Description: N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project. The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries."

carob_script <- function(path) {
  
	uri <- "doi:10.25502/E4HB-9P62/D"
	group <- "agronomy"
	ff	 <- carobiner::get_data(uri, path, group)
  
	## data set level data 
	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		project="N2Africa",
		publication=NA,
		data_organization = "IITA;WUR",
		carob_contributor="Rachel Mukami",
		carob_date="2022-08-07",
		data_type = "on-farm experiment",
		response_vars = "yield",
		treatment_vars = "inoculated;N_fertilizer;P_fertilizer;K_fertilizer"		
    )
  
  
	
	# reading the data.csv data
	f <- ff[basename(ff) == "data.csv"]
	d <- read.csv(f)
	
	d$trial_id <- d$experiment_id
	d$rep <- d$replication_no
	d$main_treatment <- carobiner::fix_name(d$main_treatment,"lower")
	d$main_treatment <- carobiner::replace_values(d$main_treatment,
			c("-r","+r"), c("not inoculated","inoculated"))
	d$inoculated <- ifelse(d$main_treatment == "inoculated",TRUE,FALSE)
	v <- carobiner::fix_name(d$sub_treatment_inoc)
	v <- gsub("\\+", "/", v)
	v <- gsub(" ","",v)
	v <- gsub("KCL","KCl",v)
	v <- gsub("Urea","urea",v)
	v <- gsub("UREA","urea",v)
	v <- gsub("PK6","PK 6",v)
	v <- carobiner::replace_values(v,
			c("NONE","YEZUMUTIMA","SOPROSOY","SB24","SB19"),
			c("none","Yezumutima","Soprosoy","SB 24", "SB 19"))
	d$sub_treatment_inoc <- v
	d$treatment <- paste("main treatment: ",d$main_treatment," |","inoculant treatment : ",d$sub_treatment_inoc) # ignoring sub treatment fert as it has no input
	d$date_harvest_yyyy[d$trial_id == "RW030_VAR_CB_SR_2010"] <- 2011
	d$planting_date <- as.Date(paste(d$planting_date_yyyy,d$planting_date_mm,d$planting_date_dd, sep = "-"))
	d$harvest_date <- as.Date(paste(d$date_harvest_yyyy,d$date_harvest_mm,d$date_harvest_dd, sep = "-"))
	d$seed_weight <- as.numeric(d$dry_weight_100_seeds)*10
	d$yield <- d$grain_yield_ha_calc
	d$fwy_residue <- d$tot_stover_yield_haulm_husks_calc
	
	d[, c("above_ground_dry_biomass", "root_dry_weight_roots_no_nodules","nodule_dry_weight")] <- 
		lapply(d[, c("above_ground_dry_biomass", "root_dry_weight_roots_no_nodules","nodule_dry_weight")], as.numeric)
	
	d$dmy_total <- (d$above_ground_dry_biomass + d$root_dry_weight_roots_no_nodules+d$nodule_dry_weight)
	
	d$fertilizer_type <- ifelse(d$sub_treatment_inoc %in% c("DAP", "TSP", "TSP/KCl","none", "TSP/KCl/urea", "PK 6/urea"), d$sub_treatment_inoc, NA)
	d$fertilizer_type[d$fertilizer_type == "PK 6/urea"] <- "urea"
	
	vv <- carobiner::fix_name(d$variety)
	vv <- gsub(" ","",vv)
	vv <- gsub("SB","SB ",vv)
	vv <- gsub("\\+","/",vv)
	vv <- gsub("PK","PK ",vv)
	vv <- gsub("RWR","RWR ",vv)
	vv <- gsub("RWAV","RWV",vv)
	vv <- gsub("RWV","RWV ",vv)
	vv <- carobiner::replace_values(vv,
		c("YEZUMUTIMA","SOPROSOY","PK 6nonino","GASIRIDA","MAMESA","CAB2","GASILIDA","MAMASA"),
		c("Yezumutima","Soprosoy","PK 6","Gasilida","Mamesa","CAB 2","Gasilida","Mamesa"))
	d$variety <- vv
	d$variety[d$sub_treatment_inoc == "PK 6/urea"] <- "PK 6"
	# Fertilizer rates: TSP and DAP will be applied using a uniform rate of 30 kg P per hectare; KCl at 30 kg K/ha 
	# and Urea split (50-50) applied at a rate of 60 kg N/ha
	
	d$P_fertilizer <- 0
	i <- d$fertilizer_type %in% c("DAP", "TSP", "TSP/KCl", "TSP/KCl/urea")
	d$P_fertilizer[i] <- 30 
	
	d$K_fertilizer <- 0
	i <- grep("KCl", d$fertilizer_type) 
	d$K_fertilizer[i] <- 30 
	
	d$N_fertilizer <- 0
	i <- grep("urea", d$fertilizer_type) 
	d$N_fertilizer[i] <- 60 
	
	# Since DAP was applied at a rate of 30 kg P per hectare, we only know the amount of phosphorus applied, 
	# hence we calculate total amount of DAP whose composition in 18:46:0
	tot_DAP <- 30/0.46
	d$N_fertilizer <- ifelse(d$fertilizer_type == "DAP",tot_DAP*0.18,d$N_fertilizer)
	d$N_splits <- 2L
	
	x <- d[,c("trial_id","rep","treatment","variety","planting_date","harvest_date","seed_weight","yield","fwy_residue", "dmy_total", "fertilizer_type","N_fertilizer","N_splits","K_fertilizer","P_fertilizer","inoculated")]
	x <- unique(x) #removing duplicates

	# reading the general.csv data
	f <- ff[basename(ff) == "general.csv"]
	d1 <- read.csv(f)
	d1$trial_id <- d1$experiment_id
	d1$location <- d1$action_site
	d1$adm2 <- d1$mandate_area_name
	x1 <- d1[,c("trial_id","location","adm2","country","crop")]
	
	# reading the soil_properties.csv data
	f <- ff[basename(ff) == "soil_properties.csv"]
	d2 <- read.csv(f)
	d2$trial_id <- d2$experiment_id
	d2$soil_pH <- d2$ph
	d2$soil_sand <- d2$sand
	d2$soil_clay <- d2$clay
	d2$soil_silt <- d2$silt
	d2$soil_N <- d2$tot_nitrogen
	d2$soil_K <- d2$k
	d2$soil_SOC <- d2$tot_carbon
	x2 <- d2[,c("trial_id","soil_pH","soil_sand","soil_clay","soil_silt","soil_N","soil_K","soil_SOC")]
	 
	
	# combining into 1 data set
	w <- Reduce(function(...) merge(..., all=T), list(x,x1,x2))
	w$on_farm <- TRUE

	## Long	and Lat based on Location
	w$longitude[w$location=="Nyamiyaga"] <- 30.1349783
	w$latitude[w$location=="Nyamiyaga"] <- -1.6873919
	w$longitude[w$location=="Musenyi"] <- 30.0208922
	w$latitude[w$location=="Musenyi"] <- -2.1788972
	w$longitude[w$location=="Mareba"] <- 29.7324604
	w$latitude[w$location=="Mareba"] <- -1.6709261
	w$longitude[w$location=="Kinoni"] <- 29.7396606
	w$latitude[w$location=="Kinoni"] <- -1.4680746
	w$longitude[w$location=="Nemba"] <- 29.7869531
	w$latitude[w$location=="Nemba"] <- -1.6422655
	w$longitude[w$location=="Kawangire"] <- 30.4493534
	w$latitude[w$location=="Kawangire"] <- -1.8199256
	w$longitude[w$location=="Nyamirama"] <- 30.33748
	w$latitude[w$location=="Nyamirama"] <- -1.6353
	w$longitude[w$location=="Nyarubaka"] <- 29.8437483
	w$latitude[w$location=="Nyarubaka"] <- -2.0856999
	w$longitude[w$location=="Nyamata"] <- 30.1208728
	w$latitude[w$location=="Nyamata"] <- -2.1508074
	w$longitude[w$location=="Rwaza"] <- 29.6752674
	w$latitude[w$location=="Rwaza"] <- -1.5507172
	w$longitude[w$location=="Kivuruga"] <- 29.7555888
	w$latitude[w$location=="Kivuruga"] <- -1.5959515
	w$longitude[w$location=="Musambira"] <- 29.8455035
	w$latitude[w$location=="Musambira"] <- -2.0458359
	w$longitude[w$location=="Rukara"] <- 30.504675
	w$latitude[w$location=="Rukara"] <- -1.7956844
	w$longitude[w$location=="Rwinkwavu"] <- 30.6150693
	w$latitude[w$location=="Rwinkwavu"] <- -1.9674691
	w$longitude[w$location=="Cyabingo"] <- 29.6987288
	w$latitude[w$location=="Cyabingo"] <- -1.5866286
	w$longitude[w$location=="NYARUBAKA"] <- 29.8437493
	w$latitude[w$location=="NYARUBAKA"] <- -2.0856999
	w$longitude[w$location=="Mbonwa"] <- 30.0574684
	w$latitude[w$location=="Mbonwa"] <- -2.2146658
	w$longitude[w$location=="Nemba- Rubona"] <- 29.77145 
	w$latitude[w$location=="Nemba- Rubona"] <- -2.4828572
	
	v <- carobiner::fix_name(w$crop)
	v <- ifelse(grepl("bush",ignore.case = T,v),"common bean",v)
	v <- ifelse(grepl("soy",ignore.case = T,v),"soybean",v)
	v <- ifelse(grepl("climbing",ignore.case = T,v),"common bean",v)
	w$crop <- v
	w$crop[grepl("BB",w$trial_id)] <- "common bean"
	w$fertilizer_type <- gsub("/","; ",w$fertilizer_type)
	
	# data type fixed
	w$harvest_date <- as.character(w$harvest_date)
	w$planting_date <- as.character(w$planting_date)
	w$fwy_residue <- as.numeric(w$fwy_residue)
 
	d <- w[,c("trial_id","country","adm2","location","latitude", "longitude","rep", "treatment","crop", "variety", "planting_date","harvest_date","inoculated","seed_weight","dmy_total","fwy_residue","yield","fertilizer_type", "N_fertilizer","N_splits","P_fertilizer","K_fertilizer","soil_pH","soil_sand","soil_clay","soil_silt","soil_N", "soil_K","soil_SOC", "on_farm")]

	d$yield_part <- "seed"
	d$N_fertilizer[is.na(w$N_fertilizer)] <- 0
	w$geo_from_source <- FALSE
  
	w$soil_pH[w$soil_pH < 3] <- NA
	w$soil_N[w$soil_N < 1] <- NA
	w$soil_K[w$soil_K < 1] <- NA

	w$is_survey <- FALSE
	w$irrigated <- NA
	w$yield_part <- "seed"

	i <- (w$planting_date == "2010-03-04") & (w$harvest_date == "2010-03-18")
	w$harvest_date[i] <- "2010-06-18"
    	
	carobiner::write_files(meta, w, path=path)
}

