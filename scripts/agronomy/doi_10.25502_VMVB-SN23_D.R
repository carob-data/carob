# R script for "carob"



carob_script <- function(path){

"Title: N2Africa agronomy trials - Kenya, 2011
Description: N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries."
  
  uri <- "doi:10.25502/VMVB-SN23/D"
  group <- "agronomy"
  ff <- carobiner::get_data(uri, path, group)
  
  # The metadata at the dataset level
	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		project="N2Africa",
		publication="doi:10.21955/gatesopenres.1115299.1",
		data_organization = "IITA;ICRAF;WUR",
		carob_contributor="Rachel Mukami; Effie Ochieng'",
		carob_date="2022-08-03",
		data_type = "on-farm experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;inoculated",		
		response_vars = "yield"
	)
  
  # reading the data.csv data
  f <- ff[basename(ff) == "data.csv"]
  d <- read.csv(f)
  d <- d[d$grain_yield_ha_calc > 0,] # dropping entries without complete data under trial_id AT_KE010_LR_2011_MAIZE_ROT_BUTERE as they all have zero yield input.
  d$trial_id <- d$experiment_id
  d$rep <- d$replication_no
  d$planting_date <- as.character(as.Date(paste(d$planting_date_yyyy,d$planting_date_mm,d$planting_date_dd, sep = "-")))
  d$date_harvest_yyyy[d$trial_id == "AT_KE003_LR_2011_INPUT_SB_BUTULA"] <- 2011
  d$harvest_date <- as.character(as.Date(paste(d$date_harvest_yyyy,d$date_harvest_mm,d$date_harvest_dd, sep = "-")))
  d$harvest_date[d$harvest_date=="0-08-04"] <- "2011-08-04"
  
  d$yield <- d$grain_yield_ha_calc
  d$fwy_residue <- d$tot_stover_yield_haulm_husks_calc
  d$dmy_roots <- d$root_dry_weight_roots_no_nodules
  d$dmy_total <- (d$above_ground_dry_biomass + d$root_dry_weight_roots_no_nodules + d$nodule_dry_weight)
  d$seed_weight <- as.numeric(d$dry_weight_100_seeds)*10 # to get the 1000 grain weight
  
  y <- carobiner::fix_name(d$main_treatment)
  a <- grepl("lime", ignore.case = TRUE, y)
  y[a] <- carobiner::fix_name(y[a],"title")
  b <- grepl("kenya", ignore.case = TRUE, y)
  y[b] <- carobiner::fix_name(y[b],"title")
  c <- grepl("inoculated", ignore.case = TRUE, y)
  y[c] <- carobiner::fix_name(y[c],"title")
  j <- grepl("sprayed", ignore.case = TRUE, y)
  y[j] <- carobiner::fix_name(y[j],"title")
  y <- carobiner::replace_values(y,
     c("GASIRIDA","KK071","KK072","KK15","KK8","MAMESA","NEWROSCOCO","OKWODHO","RWV2070","UMUBANO"),
	 c("Gasilida","KK 071","KK 072","KK 15","KK 8","Mamesa","Newrosecoco","Okwodho","RWV 2070","Umubano"))
  y[y %in% c("Minus Lime","Withou Lime")] <- "Without Lime"
  d$main_treatment <- y
  i <- grepl("inoc",d$sub_treatment_inoc,ignore.case = TRUE)
  d$sub_treatment_inoc[i] <- carobiner::fix_name(d$sub_treatment_inoc[i],"title")
  d$sub_treatment_inoc <- carobiner::replace_values(d$sub_treatment_inoc,
                                                    c("Inoculation","Not Inociulated"),
                                                    c("Inoculated","Not Inoculated"))
  d$sub_treatment_inoc <- gsub("KCL","KCl",d$sub_treatment_inoc)
  d$sub_treatment_fert <- carobiner::fix_name(d$sub_treatment_fert)
  d$sub_treatment_fert <- carobiner::replace_values(d$sub_treatment_fert,
                                                    c("Gasirida","Mac44"),
                                                    c("Gasilida","Mac 44"))
  ii <- grepl("mac",d$sub_treatment_fert,ignore.case = TRUE)
  d$sub_treatment_fert[ii] <- carobiner::fix_name(d$sub_treatment_fert[ii],"upper")
  d$sub_treatment_fert[d$sub_treatment_fert %in% c("MRP-DUST","MRP-PALLET","MRP-PALLETS")] <- "MRP"
  d$sub_treatment_fert[d$sub_treatment_fert %in% c("None","NONE")] <- "none"
  d$sub_treatment_fert <- carobiner::replace_values(d$sub_treatment_fert,
                                                    c("TSP/KCL","TSP/KCL/Urea","SYMPAL","MRP"),
                                                    c("TSP/KCl","TSP/KCl/urea","sympal","unknown"))
  d$treatment <- paste0("Main treatment: ",d$main_treatment, " | ","Fertilizer treatment: ",d$sub_treatment_fert, " | ","Inoculant treatment: ",d$sub_treatment_inoc)
  
  # standardizing the all the fertilizer types
  ## MRP is an unknown fertilizer
  ft <- ifelse(d$sub_treatment_inoc %in% c("TSP/KCl","KCl"), d$sub_treatment_inoc,
               ifelse(d$sub_treatment_fert %in% c("DAP","TSP","TSP/KCl","TSP/KCl/urea","sympal","none","unknown"),d$sub_treatment_fert,NA))
  d$fertilizer_type <- ft
  d$fertilizer_type <- gsub("/", "; ", ft)
  
  # Fertilizer rates: TSP and DAP will be applied using a uniform rate of 30 kg P per hectare; KCl at 30 kg K/ha 
  # and Urea split (50-50) applied at a rate of 60 kg N/ha in Kenya and Rwanda trials
    
  
  # Fertilizer rates: TSP and DAP will be applied using a uniform rate of 30 kg P per hectare; KCl at 30 kg K/ha 
  # and Urea split (50-50) applied at a rate of 60 kg N/ha in Kenya and Rwanda trials
  
  d$N_splits <- ifelse(grepl("urea",d$fertilizer_type), 2L, 0L)
  d$N_fertilizer <- ifelse(grepl("urea", d$fertilizer_type), 60, 0)
  
  # Since DAP was applied at a rate of 30 kg P per hectare, we only know the amount of phosphorus applied, 
  # hence we calculate total amount of DAP whose composition in 18:46:0 in order to derive N amount
  tot_DAP <- 30/0.46
  d$N_fertilizer <- ifelse(d$fertilizer_type == "DAP",tot_DAP*0.18,d$N_fertilizer)
  d$P_fertilizer <- ifelse(grepl("TSP", d$fertilizer_type), 30, 0)
  d$P_fertilizer[d$fertilizer_type=="DAP"] <- 30
  d$K_fertilizer <- ifelse(grepl("KCl", d$fertilizer_type), 30, 0)
  d$inoculated <- ifelse(d$sub_treatment_inoc == "Inoculated",TRUE,
                         ifelse(d$sub_treatment_inoc == "Not Inoculated",FALSE,NA))
  vv <- carobiner::fix_name(d$variety)
  gg <- grepl("kenya",vv,ignore.case = TRUE)
  vv[gg] <- carobiner::fix_name(vv[gg],"title")
  ss <- grepl("umubano",vv,ignore.case = TRUE)
  vv[ss] <- carobiner::fix_name(vv[ss],"title")
  cc <- grepl("gasir",vv,ignore.case = TRUE)
  vv[cc] <- carobiner::fix_name(vv[cc],"title")
  aa <- grepl("mac",vv,ignore.case = TRUE)
  vv[aa] <- carobiner::fix_name(vv[aa],"upper")
  vv[vv %in% c("saga","Saga")] <- "SC Saga"
  vv <- carobiner::replace_values(vv,
       c("EAI3600","Gasirida","KK071","KK072","KK15","KK8","MAC44","Maksoy 4M","MAMESA","Namsoy 2N",
         "NEWROSCOCO","OKWODHO","RWV2070","SB19","Sequel","Squire","TGx-1987-62 F","TGx1740-2F","TGx1987-62F",
         "TGx1987-6F","TGx-1987-62F"),
       c("EAI 3600","Gasilida","KK 071","KK 072","KK 15","KK 8","MAC 44","Maksoy","Mamesa","Namsoy",
         "Newrosecoco","Okwodho","RWV 2070","SB 19","SC Sequel","SC Squire","TGx-1987-62F","TGx 1740-2F",
         "TGx 1987-62F","TGx 1987-62F","TGx 1987-62F"))
  vv <- gsub("x","X",vv)

# variety names are listed in the publication attached above  
  d$variety <- vv 
  
  d <- d[,c("trial_id","rep","variety","treatment","inoculated","planting_date","harvest_date","fertilizer_type","N_splits","N_fertilizer","P_fertilizer","K_fertilizer","yield","fwy_residue","seed_weight","dmy_roots","dmy_total")]
  
  # ignoring the rust_score.csv data it's redundant and carob doesn't cater to rust and economic related variables
  
  # reading the soil_properties.csv data
  f <- ff[basename(ff) == "soil_properties.csv"]
  d2 <- read.csv(f)
  d2$trial_id <- d2$experiment_id
  x <- carobiner::fix_name(sapply(strsplit(d2$action_site, "-"), \(i) i[1]), "title")
  m <- carobiner::fix_name(d2$mandate_area_name, "title")
  d2$location <- paste0(x, " (", m, ")")
## observation of what?
#  d2$observation_date <- as.character(as.Date(paste(d2$date_checked_yyyy, d2$date_checked_mm, d2$date_checked_dd, sep = "-")))
  d2$country <- "Kenya"
  d2$latitude <- d2$gps_latitude_dec
  d2$longitude <- d2$gps_longitude_dec
  d2$elevation <- as.numeric(d2$gps_altitude_dec)
  
  d2 <- d2[,c("trial_id", "country","location","latitude","longitude","elevation")]
  
  # combining into 1 final dataset
  z <- Reduce(function(...) merge(..., all=T), list(d,d2))
  z$on_farm <- TRUE
  
  # lats and longs extracted from geonames.org
  z$latitude <- ifelse(grepl("Bungoma",z$location), 0.5635,
		ifelse(grepl("Butere",z$location),0.20694,
		ifelse(grepl("Kakamega",z$location),0.28422,
        ifelse(grepl("Migori",z$location),-0.982,
        ifelse(grepl("Mumias",z$location),0.33474,
		ifelse(grepl("Kakamega South",z$location),0.17124,
        ifelse(z$location == "Kisumu West",-0.091702,
		ifelse(grepl("Butula",z$location),0.33796, -0.0693307))))))))
    
  z$longitude <- ifelse(grepl("Bungoma",z$location),34.56055,
		ifelse(grepl("Butere",z$location),34.49006,
        ifelse(grepl("Kakamega",z$location),34.75229,
		ifelse(grepl("Migori",z$location),34.409,
        ifelse(grepl("Mumias",z$location),34.48796,
		ifelse(grepl("Kakamega South",z$location),34.59466,
		ifelse(grepl("Kisumu West",z$location),34.767956,
		ifelse(grepl("Butula",z$location),34.3355,35.2146883))))))))

	# filling in the crop column based on trial_id
	z$crop <- ""
	z$crop[ grep("_CB", z$trial_id) ] <- "common bean" # climbing
	z$crop[ grep("_BB", z$trial_id) ] <- "common bean" # bush
	z$crop[ grep("_SB", z$trial_id) ] <- "soybean"
	z$crop[ grep("rust", ignore.case = TRUE,z$trial_id) ] <- "soybean"  # RUST trial was conducted on soybean as per publication
	z <- z[!is.na(z$yield),] # dropping entries without yield output
	#rearranging the data 
	
	z <- z[, c("trial_id","country","location","latitude","longitude","elevation","rep","treatment","crop","variety", "planting_date","harvest_date","inoculated","seed_weight","dmy_roots","dmy_total","fertilizer_type","N_fertilizer","N_splits",
	"P_fertilizer","K_fertilizer","fwy_residue","yield","on_farm")] 
	
	z$geo_from_source <- FALSE
	
	z$yield_part <- "seed"
	z$is_survey <- FALSE
	z$irrigated <- NA
	
	z$elevation[z$elevation < -99] <- NA
	
	z$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, z, path=path)
}

