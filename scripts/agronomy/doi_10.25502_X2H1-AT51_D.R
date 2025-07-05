# R script for "carob"

carob_script <- function(path){

"N2Africa agronomy trials - Ethiopia, 2013
  
Description: N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships toenable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project. The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries." 

	uri <- "doi:10.25502/X2H1-AT51/D"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)
  
	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		project="N2Africa",
		publication= "doi:10.1080/23311932.2020.1722353",
		data_organization = "IITA;WUR",
		carob_contributor="Rachel Mukami",
		carob_date="2022-08-13",
		data_type = "on-farm experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;inoculated",		
		response_vars = "yield"		
    )
  

	# The activities.csv, nutr_deficiency_pest_disease.csv,pesticide_biocide_use.csv datasets don't 
	# contain additional info as their important information is already represented in datasets below.
	
	# processing crop_observations.csv
	f <- ff[basename(ff) == "crop_observations.csv"]
	d <- read.csv(f)
	d$rep <- d$replication_no
	d$variety <- tolower(carobiner::fix_name(d$variety))
	d$variety <- carobiner::replace_values(
		d$variety,c("nassir","awasa dume","awash dume","argen","dimitu","awasa-04"),
		c("nasir","hawassa dume","hawassa dume","argene","dimtu","hawasa04"))
	d$variety <- carobiner::replace_values(
		d$variety,c("dinknesh","dinkenesh","didesa (v1)","didessa","ethio-ugozilavia(v2)","tumssa","habiru","awash-1","awash1"),
		c("dinkinesh","dinkinesh","didesa","didesa","ethio-ugozilavia","tumsa","habru","awash 1","awash 1"))
	d$variety <- carobiner::fix_name(d$variety,"title")
	
	d$treatment <- tolower(carobiner::fix_name(d$main_treatment))
	d$treatment <- carobiner::fix_name(d$treatment,"title")
	d$treatment[d$treatment == "#name?"] <- NA
	d$treatment <- carobiner::replace_values(
		d$treatment,c("Argen","Awash1","Dinkenesh","Dimitu"),
		c("Argene","Awash 1","Dinkinesh","Dimtu"))
	
	#p or +p shows presence of phosphorus, while +r or or i shows presence of inoculants
	d$treatment <- 
		ifelse(d$treatment %in% c("+p+ +r","25kgdap&inoculant","+i +p","+i + +p",
					"+p and +i","With P, I","+p,+i","I, P"),"+P,+I",
		ifelse(d$treatment %in% c("-P+ -R","-I -P","-I + -P","-P and -I",
					"W/out I, P","Withoutinputs"),"-P,-I",
		ifelse(d$treatment %in% c("-P+ +r","+i -P","+i + -P","-P and +i"), "-P,+I",
		ifelse(d$treatment %in% c("+p+ -R","-I +p","-I + +p","+p and -I","-I,+p"), "+P,-I",
		ifelse(d$treatment %in% c("25kgdap","P"),"+P",
		ifelse(d$treatment == "Variety + +i & +p",paste(d$variety,"+I,+P",sep = ","),
		ifelse(d$treatment %in% c("Inoculant","I"),"+I",
		ifelse(d$treatment == "Variety",d$variety,d$treatment))))))))
		
	d$planting_date <- as.character(as.Date(d$date_planting,"%m/%d/%Y"))
	d$harvest_date <- as.character(as.Date(d$date_harvest, "%m/%d/%Y"))
	# based on harvested plants from harvested area/ha
	d$plant_density <- d$no_plants/(d$area_harvest_plot_m2/10000) 
	d$plant_density[d$plant_density == "NaN"] <- NA
	d$yield <- as.numeric(d$grain_yield_kgperha)
	d <- d[d$yield > 0,]
	d$fwy_residue <- as.numeric(d$total_yield_stover_kg_per_ha)
	d$dmy_total <- as.numeric(d$calc_weight_a_ground_biomass_kg)
	d$seed_weight <- d$dry_weight_100_seed_g * 10
	d <- d[,c("trial_id","rep","treatment","variety","planting_date","harvest_date","plant_density","yield",
			"fwy_residue","dmy_total","seed_weight")]

	# processing field_history.csv
	f1 <- ff[basename(ff) == "field_history.csv"]
	d1 <- data.frame(read.csv(f1))
	d1$previous_crop <- tolower(d1$crop_n2a_plot_p_season)
	d1$previous_crop <- ifelse(d1$previous_crop == "bread wheat","wheat",
						ifelse(d1$previous_crop == "tef","teff",
						ifelse(d1$previous_crop == "noug (guziota scarba)", "noug", d1$previous_crop)))
	d1$previous_crop[d1$previous_crop == ""] <- NA
	d1 <- d1[,c("trial_id","previous_crop")]
	
	# processing general.csv
	f2 <- ff[basename(ff) == "general.csv"]
	d2 <- data.frame(read.csv(f2))
	d2$adm3 <- d2$district_county
	d2$adm3 <- carobiner::replace_values(
	d2$adm3,c("Bichena","Gobu Sayo","Jamma"),c("Enemay","Gobu Seyo","Jama")) # bechena is a town in enemay district(woreda),

	# most village inputs seem to be small towns in Ethiopia according to http://www.blogabond.com/LocationBrowse.aspx?CountryCode=ET&l=Ethiopia&showAll=1
	# so they'll be allotted locations.
	d2$location <- tolower(d2$village)
	d2$site <- tolower(d2$site)
	d2$site[d2$site == ""] <- NA
	d2$elevation <- as.numeric(d2$gps_altitude)
	d2$latitude <- d2$gps_latitude
	d2$longitude <- d2$gps_longitude
	d2$latitude[d2$adm3 == "Yilmana Densa"] <- 11.6838594
	d2$longitude[d2$adm3 == "Yilmana Densa"] <- 38.6728606
	d2$latitude[d2$adm3 == "Enemay"] <- 10.45
	d2$longitude[d2$adm3 == "Enemay"] <- 38.2
	d2$latitude[d2$location == "t/sangota"] <- 9.1
	d2$longitude[d2$location == "t/sangota"] <- 37.2
	d2$latitude[d2$adm3 == "Shalla"] <- 7.282938
	d2$longitude[d2$adm3 == "Shalla"] <- 38.324853
	d2$crop <- tolower(d2$type_of_experiment)
	d2$crop <- ifelse(d2$crop %in% c("commonbean_babytrial","common bean_input","commonbean_input","Commonbean_input", "common bean_var","commonbean_variety"),"common bean",
			ifelse(d2$crop %in% c("chickpea_input","chickpea_variety"),"chickpea",
			ifelse(d2$crop %in% c("fababean_input","fababean_variety"),"faba bean","soybean")))
	d2 <- d2[,c("trial_id","country","adm3","location","site","elevation","latitude","longitude","crop")]
	d3 <- merge(d2,d1,by = "trial_id")
	
	# processing rainfall.csv
	f4 <- ff[basename(ff) == "rainfall.csv"]
	d4 <- data.frame(read.csv(f4))
	d4 <- d4[d4$rain_mm > 0,]
	
	# averaging rain amount because we cannot specifically 
	# point individual rain inputs to specific reps under specific trial_ids
	b1 <- tapply(d4$rain_mm, d4$trial_id, mean)
	d4 <- data.frame(trial_id = names(b1),rain = b1)
	rownames(d4) <- NULL

	# processing soil_data.csv
	f5 <- ff[basename(ff) == "soil_data.csv"]
	d5 <- data.frame(read.csv(f5)) 
	d5$trial_id <- toupper(d5$farm_id)
	d5$soil_pH <- d5$ph
	d5$soil_SOC <- d5$tc_perc
	d5$soil_N <- d5$n_perc
	d5$soil_sand <- as.numeric(d5$sand_perc)
	d5$soil_clay <- as.numeric(d5$clay_perc)
	d5 <- d5[,c("trial_id","soil_pH","soil_SOC","soil_N","soil_sand","soil_clay")]

	# compiling into a single final dataset
	
	d6 <- merge(d3,d5,by = "trial_id",all.x = TRUE)
	d7 <- merge(d6,d4,by = "trial_id",all.x = TRUE)
	f <- merge(d,d7,by = "trial_id",all.x = TRUE)
	
	# Fertilizer rates: DAP will be applied using a rate of 25 kg DAP per hectare; DAP has 18:46:0 composition
	# calculating amount of P in DAP applied assuming that any +P input refers to DAP application; 
	
	P2O5 <- 25 * 0.46
	# to acquire the amount of P in P2O5, P has atomic weight 31 while O has atomic weight 16.
	P <- P2O5*((2*31)/(2*31+5*16))	
	N <- 25 * 0.18
	
	f$fertilizer_type <- "none"
	f$P_fertilizer <- ifelse(
		f$treatment %in% c("+p,+i","+p,-i","+p","hachalu,+i,+p","wayu,+i,+p","wolki,+i,+p","dagim,+i,+p","lalo,+i,+p","local,+i,+p"),P,0)
	f$N_fertilizer <- ifelse(
		f$treatment %in% c("+p,+i","+p,-i","+p","hachalu,+i,+p","wayu,+i,+p","wolki,+i,+p","dagim,+i,+p","lalo,+i,+p","local,+i,+p"),N,0)
	f$K_fertilizer <- 0
	f$inoculated <- grepl("\\+i", f$treatment)
	f$fertilizer_type[f$N_fertilizer > 0] <- "DAP"
	f$fertilizer_type[f$P_fertilizer > 0] <- "DAP"
	f$country <- "Ethiopia"
	f$row_spacing <- 40 
	f$plant_spacing <- 10
	f$on_farm <- TRUE
	f$yield_part <- "seed"
	
	f <- f[,c("trial_id","country","adm3","location","site","planting_date","harvest_date",
			"rep","treatment","crop","variety","previous_crop","yield","fwy_residue","dmy_total",
			"seed_weight","plant_density","soil_pH","soil_SOC","soil_N","soil_sand","soil_clay","rain",
			"fertilizer_type","P_fertilizer","N_fertilizer","K_fertilizer","inoculated","row_spacing",
			"plant_spacing","on_farm","elevation","latitude","longitude")]

	i <- which(f$location == "oda haro" & f$adm3 == "Bako Tibe")
	f$longitude[i] <- 37.2 
	f$latitude[i] <- 9
	
	f$yield_part <- "seed"

	f$is_survey <- FALSE
	f$irrigated <- NA
	
	f$geo_from_source <- FALSE
	
	carobiner::write_files(meta, f, path=path)
}

