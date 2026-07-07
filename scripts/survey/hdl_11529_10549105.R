# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. raw data has messy coordinates, which are interconnected within the 3 countries, so i decided to ditch them and intended to use the point radius method, but the raw dataset and the publication only provides adm1 divisions for India only, none for Nepal and Bangladesh, and ultimately i can not estimate the adm1 divisions because the survey spans across atleast 3 adm1 divisions in Nepal(floodplains) and Bangladesh(Terai region), which renders the point radius method unusable. 
#2. out of bounds values originate from the raw dataset
#3. dates records are messy
#4. messy and NA values in yield originate from raw dataset

carob_script <- function(path) {

"
Farmer field survey dataset underpinning 'Data-driven strategies to improve nitrogen use efficiency of rice farming in South Asia'.

This database integrates farmer field surveys with secondary environmental data for 45,643 farmer fields that cultivated rice between 2016 and 2021. This dataset integrates original survey datasets that have, or will be, published on the CIMMYT Dataverse. Collectively, the dataset covers six regions across South Asia: Andhra Pradesh (n=1,744), Bihar and eastern Uttar Pradesh (n=9,693), Odisha (n=1,947), Punjab and Haryana (n=5,833), Bangladesh's floodplains (n=20,329), and the Terai region of Nepal (n=6,097). The northwestern Indian states of Punjab and Haryana are grouped together as they are commonly considered part of a distinct rice-producing area known as the 'Trans-Gangetic Plains'. Similarly, Bihar and the neighbouring districts of eastern Uttar Pradesh—Ballia, Chandauli, Deoria, Ghazipur, Gorakhpur, Kushinagar, Maharajganj, Mau, and Siddharthnagar—are grouped together as a distinct rice production environment commonly known as the 'Middle Indo-Gangetic Plains'. It should also be noted that the surveyed area of Bangladesh's floodplains primarily includes the Tista Meander Floodplain and Ganges River Floodplains and may not represent other floodplain areas in Bangladesh.
"

	uri <- "hdl:11529/10549105"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "CIMMYT; CU; IRRI; ICRISAT; ICAR; IRRI",
		publication = "doi_10.1038_s41893-024-01496-3",
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-07-06",
		carob_completion = 90,	
		carob_effort = 4
	)
	

	f1 <- ff[basename(ff) == "NUE_survey_dataset.csv"]
	f2 <- ff[basename(ff) == "NUE_survey_dataset_v2.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	#cannot bind at this point, colnames are mismatched
	
	standardize_nue <- function(r) {

	  d0 <- data.frame(
	    hhid = r$Merged_ID,
	    date = as.character(r$year),
	    adm1 = r$Region,
	    latitude = r$LAT,
	    longitude = r$LONG,
	    #season = as.character(tolower(r$SEASON)),# invalid term(carob)
	    plot_area = r$CRLPARHA,
	    sex = as.character(r$GEN),#empty values originate from raw data
	    education = as.character(r$EDU),
	    hh_size = r$HHMEM,
	    planting_date = r$SDATE,
	    crop = "maize",
	    transplanting_date = r$TDATE,
	    harvest_date = r$HDATE,
	    variety = tolower(r$VARTYPE),
	    #planting_method = as.character(tolower(r$EST_binary)),
	    seed_rate = r$SRATEHA,
	    irrigated = r$IRRIAVA,
	    irrigation_number = r$IRRINU,
	    yield = r$yield_kg_ha,
	    soil_clay = r$SoilGrids_clay,
	    soil_N = r$SoilGrids_nitrogen,
	    soil_pH = r$SoilGrids_phh2o,
	    soil_sand = r$SoilGrids_sand,
	    soil_silt = r$SoilGrids_silt,
	    soil_SOC = r$SoilGrids_soc,
	    prec = r$total_precip,
	    maturity_days = r$Crop_duration,
	    crop_price = r$FGPRICE_quintal,
	    soil_bd = r$SoilGrids_bdod
	  )
	  
	  d0$N_fertilizer <- r$Total_N / r$CRLPARHA
	  d0$P_fertilizer <- r$P2O5_ha / 2.29
	  d0$K_fertilizer <- r$K2O_ha / 1.2051
	  d0$B_fertilizer <- r$BOR_ha
	  d0$Zn_fertilizer <- r$Zn_ha
	  d0$OM_amount <- r$Organic_ha
	  
	  # Sulphur fertilizer, NA-preserving sum
	  vals <- cbind(r$TOTSSP*0.12, r$TOTTSP*0.02, r$TOTGYP*0.19)
	  d0$S_fertilizer <- rowSums(vals, na.rm = TRUE)
	  d0$S_fertilizer[rowSums(!is.na(vals)) == 0] <- NA
	  
	  return(d0)
	}
	
	##applying function
	d1 <- standardize_nue(r1)
	d2 <- standardize_nue(r2)
	
	##binding
	d <- rbind(d1, d2)
	
	d$sex <- gsub("",NA,d$sex)
	d$fertilizer_type <- "SSP;TSP;KCl;gypsum;ZnSO4;NPK;NPKS;"

	d$adm1 <- gsub("AP","Andhra Pradesh",d$adm1)

	#mapping countries
	countries <- c(
	  "Andhra Pradesh"="India",
	  "Bihar"="India",
	  "Odisha"="India",
	  "Punjab_Haryana"="India",
	  "Bangladesh"="Bangladesh",
	  "Nepal"="Nepal"
	)
	
	d$country <- countries[d$adm1]
	
	#mapping currencies
	currencies <- c(
	  "India"="INR",
	  "Bangladesh"="BDT",
	  "Nepal"="NPR"
	)
		
	d$currency <- currencies[d$country]

	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$irrigated <- d$irrigated=="1"
	d$geo_from_source <- TRUE
	
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA
	#d$planting_method <- gsub("direct_seed","direct seeding",d$planting_method) #invalid term(carob)
	
	d <- unique(d) #since some differentiating columns have been removed through standardizing, obs. changed from 91286 to 79809
	
	carobiner::write_files(path, meta, d)
}
