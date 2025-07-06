# R script for "carob"


carob_script <- function(path){

"Title: N2Africa agronomy trials - Kenya, 2011
 
Description: N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project.The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries."
  
	uri <- "doi:10.25502/6G5B-RM44/D"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		project="N2Africa",
		publication= NA,
		data_organization = "IITA;ICRAF;WUR",
		carob_contributor="Rachel Mukami; Effie Ochieng'",
		carob_date="2022-07-29",
		data_type="on-farm experiment",
		response_vars= "yield",
		treatment_vars="N_fertilizer;P_fertilizer;K_fertilizer"
	)

	f <- ff[basename(ff) == "data.csv"]
	d <- data.frame(read.csv(f))
	d$rep <- d$replication_no
	d$date_harvest_yyyy[d$experiment_id %in% c("AT_KE011_SR_2010_INPUT_SB","AT_KE012_SR_2010_INPUT_SB")] <- 2011
	d$planting_date_yyyy[d$experiment_id == "AT_KE017_SR_2010_SB_ROT"] <- 2010
	d$planting_date <- as.character(as.Date(paste(d$planting_date_mm,d$planting_date_dd,d$planting_date_yyyy,sep = "/"),"%m/%d/%Y"))
	d$harvest_date <- as.character(as.Date(paste(d$date_harvest_mm,d$date_harvest_dd,d$date_harvest_yyyy,sep = "/"),"%m/%d/%Y"))

	d$main_treatment <- tolower(carobiner::fix_name(d$main_treatment))
	d$main_treatment <- carobiner::replace_values(
		d$main_treatment,c("inocualted","not noculated","not inoculted"),c("inoculated","not inoculated","not inoculated"))

	dd <- carobiner::fix_name(d$sub_treatment_fert)
	dd <- gsub("KCL","KCl",dd)
	dd <- gsub("UREA","urea",dd)
	i <- grepl("Kenya", dd, ignore.case=TRUE)
	dd[i] <- carobiner::fix_name(dd[i], "title")

	# MRP fertilizer is an unknown fertilizer so it will be replaced with "unknown" fertilizer name in all occurrences of MRP
	dd <- carobiner::replace_values(
		dd,c("MRP","None","UMUBANO","GASIRIDA","RWV1129"),c("unknown","none","Umubano","Gasilida","RWV 1129"))
	d$sub_treatment_fert <- dd

	de <- toupper(carobiner::fix_name(d$sub_treatment_inoc))
	de <- gsub("KCL","KCl",de)
	de <- gsub("UREA","urea",de)
	i <- grepl("Kenya", de, ignore.case=TRUE)
	de[i] <- carobiner::fix_name(de[i], "title")
	de <- carobiner::replace_values(de,
		c("MRP","TSP/KCl urea","TGX1835-10F","KENAY MAVUNO","KAT B 9","KAT X 56","NEW ROSCOCO","NONE","UMUBANO","GASIRIDA","RWV1129","NAMSOY 4M"),
		c("unknown","TSP/KCl/urea","TGX 1835-10F","Kenya Mavuno","KAT B9","KAT X56","New Roscoco","none","Umubano","Gasilida","RWV 1129","NAMSOY"))
	d$sub_treatment_inoc <- de
	d$treatment <- paste("main treatment: ", d$main_treatment," | ",
				"inoculant treatment: ", d$sub_treatment_inoc," | ",
				 "fertilizer treatment: ", d$sub_treatment_fert)

	ft <- ifelse(d$sub_treatment_inoc %in% c("unknown", "DAP", "TSP/KCl", "TSP/KCl/urea", "TSP","none"), d$sub_treatment_inoc, 
		ifelse( d$sub_treatment_fert %in% c("unknown","TSP/KCl","TSP/KCl/urea","DAP","TSP","KCl","none"), d$sub_treatment_fert,NA))
	d$fertilizer_type <- gsub("/", "; ", ft)

	v <- carobiner::fix_name(d$variety)
	i <- grepl("Kenya", v, ignore.case=TRUE)
	v[i] <- carobiner::fix_name(v[i], "title")
	v <- carobiner::replace_values(v,c("RWV1129","UMUBANO","GASIRIDA","TGX1740-2F","SB19","SB97","SB3","SB25","NEW ROSCOCO","Kenay Mavuno","KAT B 9","KAT X 56"),
		c("RWV 1129","Umubano","Gasilida","TGX 1740-2F","SB 19","SB 97","SB 3","SB 25","New Roscoco","Kenya Mavuno","KAT B9","KAT X56"))
	d$variety <- v

	d$dmy_roots <- d$root_dry_weight_roots_no_nodules
	d$dmy_total <- d$above_ground_dry_biomass + d$dmy_roots + d$nodule_dry_weight
	d$fwy_residue <- d$tot_stover_yield_haulm_husks_calc
	d$yield <- d$grain_yield_ha_calc

	d$inoculated <- ifelse(d$main_treatment == "inoculated",TRUE,FALSE)
	d$plant_density <- round((10000*d$no_plants_harvest_plot)/d$plot_area_harvest)
	d$seed_weight <- d$dry_weight_100_seeds*10 # converting to g per 1000 seeds
	d <- d[d$yield > 0,]

	d <- d[,c("experiment_id","rep","variety","planting_date","harvest_date","treatment","fertilizer_type","inoculated","plant_density","seed_weight","dmy_roots","dmy_total","fwy_residue","yield")]

	f1 <- ff[basename(ff) == "soil_properties.csv"]
	d1 <- read.csv(f1)
	d1$soil_pH <- d1$ph
	d1$soil_K <- d1$k
	d1$soil_sand <- d1$sand
	d1$soil_clay <- d1$clay
	d1$soil_SOC <- d1$tot_carbon
	d1$soil_N <- d1$tot_nitrogen

	d1 <- d1[,c("experiment_id","soil_pH","soil_K","soil_sand","soil_clay","soil_SOC","soil_N")]

	f2 <- ff[basename(ff) == "general.csv"]
	d2 <- read.csv(f2)
	d2$country <- "Kenya"
	as <- carobiner::fix_name(sapply(strsplit(d2$action_site, "-"), \(i) i[1]), "title")
	as <- sapply(strsplit(as, "\\("), \(i) i[1])
	as <- sapply(strsplit(as, "_"), \(i) i[1])
	as <- carobiner::fix_name(as)
	ma <- carobiner::fix_name(d2$mandate_area_name, "title")
	d2$location <- paste0(as, " (", ma, ")")
	d2$latitude <- d2$gps_latitude_dec
	d2$longitude <- d2$gps_longitude_dec
	d2$elevation <- as.numeric(d2$gps_altitude_dec)
	d2$geo_from_source <- TRUE
	d2$crop <- ""
	d2$crop[ grep("_CB", d2$experiment_id) ] <- "common bean" # climbing
	d2$crop[ grep("_BB", d2$experiment_id) ] <- "common bean" # bush
	d2$crop[ grep("_SB", d2$experiment_id) ] <- "soybean"

	d2 <- d2[,c("experiment_id","country","location","crop","latitude","longitude","elevation")]

	## Merging the data frames to one; 

	d4 <- Reduce(function(...) merge(..., all=T), list(d,d1,d2))
	names(d4)[1] <- "trial_id"
	d4$on_farm <- TRUE
	d4 <- d4[complete.cases(d4$yield), ]

	# Fertilizer rates: TSP and DAP will be applied using a uniform rate of 30 kg P per hectare; KCl at 30 kg K/ha 
	# and Urea split (50-50) applied at a rate of 60 kg N/ha in Kenya and Rwanda trials

	d4$N_fertilizer <- ifelse(grepl("urea", d4$fertilizer_type), 60, 0)
	# Since DAP was applied at a rate of 30 kg P per hectare, we only know the amount of phosphorus applied, 
	# hence we calculate total amount of DAP whose composition in 18:46:0
	tot_DAP <- 30/0.46
	d4$N_fertilizer <- ifelse(d4$fertilizer_type == "DAP",tot_DAP*0.18,d4$N_fertilizer)
	d4$P_fertilizer <- ifelse(grepl("TSP", d4$fertilizer_type), 30, 0)
	d4$P_fertilizer[d4$fertilizer_type=="DAP"] <- 30

	d4$K_fertilizer <- ifelse(grepl("KCl", d4$fertilizer_type), 30, 0)
	d4$N_splits <- ifelse(grepl("urea",d4$fertilizer_type), 2L, 0L)
	
	d <- d4[, c("trial_id","country","location","latitude", "longitude", "elevation","rep", "treatment","crop", "variety", "planting_date","harvest_date","inoculated","plant_density","seed_weight","dmy_roots","dmy_total", "fwy_residue","yield","fertilizer_type","N_fertilizer","N_splits","P_fertilizer","K_fertilizer", "soil_pH", "soil_K", "soil_sand", "soil_clay", "soil_SOC", "soil_N", "on_farm")]



	d$soil_pH[d$soil_pH < 2] <- NA
	
	d$plant_density[d$plant_density == 0] <- NA
	d$soil_K[d$soil_K==0] <- NA
	d$soil_N[d$soil_N==0] <- NA

	d$yield_part <- "seed"
	d$is_survey <- FALSE
	d$irrigated <- NA

	d$geo_from_source <- TRUE


	carobiner::write_files(meta, d, path=path)
}

