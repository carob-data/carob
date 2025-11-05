# R script for "carob"


carob_script <- function(path) {
	
"AfricaRISING- Sustainable Intensification of Maize-Legume-Livestock Integrated Farming Systems in East and Southern Africa

The datasets contain field level trial data on low-input agriculture systems in Zambia

Sustainable intensification of mixed crop-livestock systems is a key pathway towards better food security, improved livelihoods, and a healthy environment. As part of the US government’s Feed the Future initiative to address hunger and food security issues in sub-Saharan Africa, the US Agency for International Development (USAID) is supporting three multi-stakeholder agricultural research projects to sustainably intensify key African farming systems. In East and Southern Africa, the project is being implemented in Tanzania and Malawi, and Zambia. In Tanzania, the project is being implemented in Babati and Kongwa districts in Manyara region of northern Tanzania and Kiteto district in Dodoma region, central Tanzania. The action sites were selected to acknowledge agro-ecological differences, allow appropriate targeting of technologies and strategies, and complement the development efforts of another USAID-supported program, the Tanzania Staples Value Chain (NAFAKA) project. In Malawi, the project is being implemented in Ntechu and Dedza districts in central Malawi where maize-based productions systems are dominant. Agroecological considerations guided the identification of research action sites. The pilot site for the study will be Eastern and Lusaka Provinces in Zambia."
	
	uri <- "doi:10.7910/DVN/FVEISH"
	group <- "agronomy"
	ff	<- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
	  project="AfricaRISING",
	  publication= NA,
	  data_organization = "CIMMYT;ZARI;GT;CRS",
	  data_type="experiment", 
	  treatment_vars = "intercrops; land_prep_method; N_fertilizer; P_fertilizer; K_fertilizer",
	  response_vars="yield",
	  carob_contributor="Siyabusa Mkuhlani",
	  carob_date="2024-07-23"
	)
	
	#Read and process first data set
	#There are rotation and intercrop treatments but there was no mention of the other crops in the intercrop and rotation.
	#On-farm
	f1 <- ff[basename(ff) == "001_arzamcimmytcamotheronfarm2016.xlsx"]
	r1 <-carobiner::read.excel(f1, sheet="Data")
	d1 <- data.frame(
	  trial_id = (paste0(r1$SiteName, r1$Farmerno)), 
	  planting_date = as.character(r1$Year-1), #Subtracting 1 because the harvest year is the following year after planting.
	  country = r1$Country,
	  adm1='Eastern province',
	  adm2=r1$District,
	  location=r1$Camp,
	  rep=as.integer(r1$Rep),
	  crop=tolower(r1$Cropgrow), 
	  treatment=r1$Description,
	  dmy_residue=r1$Biomassyield,
	  yield=r1$Grainyield,
	  yield_part = "grain",
	  intercrops = NA)
	
	#Split the fertilizer data column  
	d1$fertilizer_type <- 'D-compound; urea'
	d1[c("basalfert", "topdressing")] <- do.call(rbind, strsplit(as.character(r1$Fertilizerrate), ":"))
	#Most common basal fertilizer is 10:20:10 and top dressing as urea.
	d1$N_fertilizer <- 0.1*as.numeric(d1$basalfert) + as.numeric(d1$topdressing)
	d1$P_fertilizer <- 0.2*as.numeric(d1$basalfert)*0.436
	d1$K_fertilizer <- 0.1*as.numeric(d1$basalfert)*0.83
	
	#Remove the basal fert and top dressing columns as they are no longer useful
	d1 <- d1[, !(names(d1) %in% c("basalfert", "topdressing"))]
	
	d1$land_prep_method = NA
	d1$land_prep_method[d1$treatment== "CP sole" ] <- "conventional"
	d1$land_prep_method[d1$treatment== "DS sole" | d1$treatment== "DS inter" | d1$treatment== "DS Inter" | d1$treatment== "DS rot" | d1$treatment== "DS Rot"] <- "reduced tillage"
	d1$land_prep_method[d1$treatment== "RI sole" | d1$treatment== "RI rot" ] <- "ripping"
	d1$land_prep_method[d1$treatment== "ATDS sole" | d1$treatment== "ATDS rot" ] <- "deep ploughing"
	
	d1$planting_method<- NA
	d1$planting_method[d1$treatment== "CP sole" | d1$treatment== "RI sole" | d1$treatment== "RI rot" | d1$treatment== "ATDS sole" | d1$treatment== "ATDS rot"] <- "manual"
	d1$planting_method[d1$treatment== "DS sole" | d1$treatment== "DS inter" | d1$treatment== "DS Inter" | d1$treatment== "DS rot" | d1$treatment== "DS Rot"] <- "mechanized"
	
	d1$treatment <- ifelse(d1$treatment == "CP sole" , "Conventional ploughing",
	                ifelse(d1$treatment == "DS sole" , "Direct seeding sole",
	                ifelse(d1$treatment == "DS inter" , "Direct seeding intercrop",
	                ifelse(d1$treatment == "DS rot" , "Direct seeding rotation",
	                ifelse(d1$treatment == "RI sole" , "Ripper sole",
	                ifelse(d1$treatment == "RI rot" , "Ripper rotation",
	                ifelse(d1$treatment == "ATDS sole" , "Tractor drawn plough sole", "Tractor drawn plough rotation")))))))
	
	#Read and process second data set
	#There is an intercrop treatment but there was no mention of the other crops in the intercrop.
	#On-farm
	f2 <- ff[basename(ff) == "002_arzamcimmytdoubleduponfarm2016.xlsx"]
	r2 <-carobiner::read.excel(f2, sheet="data")
	d2 <- data.frame(
	  trial_id = (paste0(r2$Site, r2$No)), 
	  planting_date = as.character(r2$year-1), #Subtracting 1 because the harvest year is the following year after planting.
	  country = r2$Country,
	  adm1='Eastern province',
	  adm2=r2$District,
	  location=r2$Site,
	  rep=as.integer(r2$Rep),
	  crop=tolower(r2$Crop), 
	  treatment=r2$Treat,
	  dmy_residue=r2$Biomassyield,
	  yield=r2$Grainyield,
	  yield_part = "grain",
	  planting_method = "manual")
	
	#Split the fertilizer data column  
	d2$fertilizer_type <- 'D-compound'
	#No basal fertilizer is applied.
	d2$N_fertilizer <- 0.1*as.numeric(r2$Fertrate) 
	d2$P_fertilizer <- 0.2*as.numeric(r2$Fertrate)*0.436
	d2$K_fertilizer <- 0.1*as.numeric(r2$Fertrate)*0.83
	
	d2$land_prep_method<-ifelse(d2$treatment == "CA", "reduced tillage", "conventional")
	d2$intercrops <- NA
	
	#Read and process third data set
	#There is an intercrop treatment but there was no mention of the other crops in the intercrop.
	#On-Station
	f3 <- ff[basename(ff) == "003_arzamcimmytexpandedsteponstation2016.xlsx"]
	r3 <-carobiner::read.excel(f3, sheet="Data")
	d3 <- data.frame(
	  trial_id = (paste0(r3$Location, r3$No)), 
	  planting_date = as.character(r3$Year-1), #Subtracting 1 because the harvest year is the following year after planting.
	  country = r3$Country,
	  adm1='Eastern province',
	  adm2=r3$District,
	  location=r3$Location,
	  rep=as.integer(r3$Rep),
	  crop=tolower(r3$Crop), 
	  treatment=r3$Residuemgm,
	  dmy_residue=r3$Biomassyield+r3$Coreyield,
	  yield=r3$Grainyield,
	  yield_part = "grain")
	
	#Split the fertilizer data column  
	d3$fertilizer_type <- 'D-compound; urea'
	d3[c("basalfert", "topdressing")] <- do.call(rbind, strsplit(as.character(r3$Fertilizerrate), ":"))
	#Most common basal fertilizer is 10:20:10 and top dressing as urea.
	d3$N_fertilizer <- 0.1*as.numeric(d3$basalfert) + as.numeric(d3$topdressing)
	d3$P_fertilizer <- 0.2*as.numeric(d3$basalfert)*0.436
	d3$K_fertilizer <- 0.1*as.numeric(d3$basalfert)*0.83
	
	#Remove the basal fert and top dressing columns as they are no longer useful
	d3 <- d3[, !(names(d3) %in% c("basalfert", "topdressing"))]
	
	#Add reasonable treatment name
	d3$treatment <- ifelse(d3$treatment == "residues",'conservation agriculture',"conventional agriculture")
	
	d3$land_prep_method<-ifelse(r3$Residuemgm == "residues", "reduced tillage", "conventional")
	d3$planting_method<- 'manual'
	d3$intercrops <- NA
	
	#Read and process fourth data set
	#On-farm
	f4 <- ff[basename(ff) == "004_arzamcimmytgliricidiaonfarm2016.xlsx"]
	r4 <-carobiner::read.excel(f4, sheet="data")
	d4 <- data.frame(
	  trial_id = (paste0(r4$Village, r4$Farmerno)), 
	  planting_date = as.character(r4$Year-1), #Subtracting 1 because the harvest year is the following year after planting.
	  country = r4$Country,
	  adm1='Eastern province',
	  adm2=r4$Districts,
	  location=r4$Village,
	  rep=as.integer(r4$plotrep),
	  crop=tolower(r4$Crop), 
	  treatment=r4$cropstrat,
	  dmy_residue=NA,
	  yield=r4$Grainyield,
	  yield_part = "grain")
	
	#Split the fertilizer data column  
	d4$fertilizer_type <- 'D-compound; urea'
	d4[c("basalfert", "topdressing")] <- do.call(rbind, strsplit(as.character(r4$Fertilizerrate), ":"))
	#Applied fertilizer not stated but fist sheet provides how much mineral fertilizer  of 165kg basal and 200 kg urea was applied.
	d4$N_fertilizer <- NA
	d4$P_fertilizer <- NA
	d4$K_fertilizer <- NA
	
	#There is a mineral fertilizer and no fertilizer treatment 
	d4$N_fertilizer[r4$fertstrat== 'mineral fert']<-  0.1*165 + 0.46*200
	d4$P_fertilizer[r4$fertstrat== 'mineral fert'] <- 0.2*165*0.436
	d4$K_fertilizer[r4$fertstrat== 'mineral fert'] <- 0.1*165*0.83
	
	d4$N_fertilizer[r4$fertstrat== 'no fert']<-  0
	d4$P_fertilizer[r4$fertstrat== 'no fert'] <- 0
	d4$K_fertilizer[r4$fertstrat== 'no fert'] <- 0
	
	d4$land_prep_method<- "conventional"
	d4$planting_method<- 'manual'
	
	d4$intercrops<- NA
	d4$intercrops[r4$treatment== "Gliricidia intercrop" ] <- "Gliricidia"
	
	#Read and process fifth data set
	#On-station
	f5 <- ff[basename(ff) == "005_arzamcimmytgliricidiaonstation2016.xlsx"]
	r5 <-carobiner::read.excel(f5, sheet="Data")
	d5 <- data.frame(
	  trial_id = (paste0(r5$Location, r5$No)), 
	  planting_date = as.character(r5$Year-1), #Subtracting 1 because the harvest year is the following year after planting.
	  country = r5$Country,
	  adm1='Eastern province',
	  adm2=r5$District,
	  location=r5$Location,
	  rep=as.integer(r5$Rep),
	  crop=tolower(r5$Crop), 
	  treatment=r5$fertrate,
	  dmy_residue=NA,
	  yield=r5$grainyield,
	  yield_part = "grain",
	  intercrops=r5$Intercrop)
	
	#Split the fertilizer data column  
	d5$fertilizer_type <- 'D-compound; urea'
	d5[c("basalfert", "topdressing")] <- do.call(rbind, strsplit(as.character(r5$fertrate), ":"))
	
	#Most common basal fertilizer in Zambia is 10:20:10 and top dressing as urea.
	d5$N_fertilizer <- 0.1*as.numeric(d5$basalfert) + as.numeric(d5$topdressing)
	d5$P_fertilizer <- 0.2*as.numeric(d5$basalfert)*0.436
	d5$K_fertilizer <- 0.1*as.numeric(d5$basalfert)*0.83
	
	#Remove the basal fert and top dressing columns as they are no longer useful
	d5 <- d5[, !(names(d5) %in% c("basalfert", "topdressing"))]
	
	d5$land_prep_method<- NA
	d5$planting_method<- 'manual'
	
	#Read and process sixth data set
	#On-farm
	#There is a treatment column with Treatment codes as no. only and not the actual details.
	#The sub treat column has abbreviations but not very clear which ones are intercrops and rotations.
	f6 <- ff[basename(ff) == "006_arzamcimmytgmcconfarm2016.xlsx"]
	r6 <-carobiner::read.excel(f6, sheet="Data")
	d6 <- data.frame(
	  trial_id = (paste0(r6$Village, r6$Farmerno)), 
	  planting_date = as.character(r6$Year-1), #Subtracting 1 because the harvest year is the following year after planting.
	  country = r6$Country,
	  adm1='Eastern province',
	  adm2=r6$District,
	  location=r6$Village,
	  rep=as.integer(r6$Rep),
	  crop=tolower(r6$Crop), 
	  treatment=r6$Treat,
	  dmy_residue=r6$Biomassyield,
	  yield=r6$Grainyield,
	  yield_part = "grain")
	
	#Split the fertilizer data column  
	d6$fertilizer_type <- 'D-compound; urea'
	d6[c("basalfert", "topdressing")] <- do.call(rbind, strsplit(as.character(r6$Fertilizationrate), ":"))
	#Most common basal fertilizer is 10:20:10 and top dressing as urea.
	d6$N_fertilizer <- 0.1*as.numeric(d6$basalfert) + as.numeric(d6$topdressing)
	d6$P_fertilizer <- 0.2*as.numeric(d6$basalfert)*0.436
	d6$K_fertilizer <- 0.1*as.numeric(d6$basalfert)*0.83
	
	#Remove the basal fert and top dressing columns as they are no longer useful
	d6 <- d6[, !(names(d6) %in% c("basalfert", "topdressing"))]
	
	#information not provided
	d6$land_prep_method<- NA
	d6$planting_method<- NA
	d6$intercrops <- NA
	
	#Read and process seventh data set
	#On-farm
	#There is a treatment column with codes only and not the actual details.
	#Appears like subtreat 1, has no fertilizer input. This can be a no fert treatement but it is not staterd.
	f7 <- ff[basename(ff) == "007_arzamcimmytgmcconstation2016.xlsx"]
	r7 <-carobiner::read.excel(f7, sheet="Data")
	d7 <- data.frame(
	  trial_id = (paste0(r7$Location, r7$No)), 
	  planting_date = as.character(r7$Year-1), #Subtracting 1 because the harvest year is the following year after planting.
	  country = r7$Country,
	  adm1='Eastern province',
	  adm2=r7$District,
	  location=r7$Location,
	  rep=as.integer(r7$Rep),
	  crop=tolower(r7$Crop), 
	  treatment=r7$Treat,
	  dmy_residue=r7$Biomassyield+r7$Coreyield,
	  yield=r7$Grainyield,
	  yield_part = "grain",
	  intercrops=tolower(r7$'Companion crop'))
	
	#Split the fertilizer data column  
	d7$fertilizer_type <- 'D-compound; urea'
	d7[c("basalfert", "topdressing")] <- do.call(rbind, strsplit(as.character(r7$Fertilizerrate), ":"))
	#Most common basal fertilizer is 10:20:10 and top dressing as urea.
	d7$N_fertilizer <- 0.1*as.numeric(d7$basalfert) + as.numeric(d7$topdressing)
	d7$P_fertilizer <- 0.2*as.numeric(d7$basalfert)*0.436
	d7$K_fertilizer <- 0.1*as.numeric(d7$basalfert)*0.83
	
	#Remove the basal fert and top dressing columns as they are no longer useful
	d7 <- d7[, !(names(d7) %in% c("basalfert", "topdressing"))]
	
	#information not provided
	d7$land_prep_method<- NA
	d7$planting_method<- NA
	
	d<-rbind(d1,d2,d3,d4,d5,d6,d7)
	d$longitude <- NA
	d$latitude <- NA
	
	#geo-referencing
	#could not find coordinates of most locations, in that case we used the district centroid. 
	#Auto geo-referencing not yielding any coordinate points.
	d <- carobiner::geocode_duplicates(d, c("country", "location") )
	pts <- data.frame(
	  location=c("Chanje",  "Hoya",  "Kapara", "Kawalala", "Mtaya",  "Vuu", "Msekera Research Station", "Chilikumtima Farm", "Chindimba", 
	             "Kajuuluke Farm", "Tyson Farm",  "Chikata", "Itaye","Jabesi", "Langa",  "Mapili",  "Malaya",  "Mduwa", "Ngapu","Mizeck",
	             "Masaiti farm", "Kamukwezi school",  "Mukaya",  "Musalemela farm","Mwase Farm",  "Wasogoza Farm",  "Chimpapi", "Julius",
	             "Mlaka",   NA, "Katambala", "Musonepo",  "Msekera",    "chigona",  "kafwadala",  "magoda", "mapato", "chuma", 
	             "chikungu farm",  "kalimba", "kafwanda",  "vyalaviandu", "kamtemeni", "kwaule",  "mtavu",  "fobo", "luambwa",  "kajepu",                  
	             "kanyondo",  "mahobe",   "holoholo", "chikungu farms",  "kafyanda", "0"),
	  longitude=c(32.8513, 33.174511,32.52324,  31.80801,32.65,    33.2333,  32.6447, 33.1782,  31.1333333, 
	              32.74398,32.74398, 32.6447001, 32.6447001,    32.6447001, 31.9333300, 32.6447001,    32.6447001,32.6447001,  32.6447001, 32.6447001, 33.1745097,33.1745097,  33.1745097, 33.1745097, 32.9166700,   33.1745097, 33.1745097,33.1745097,
	              32.743987,  32.743987, 32.3      ,  32.743987, 32.52431, 33.2833,    33.1745097,  32.6447001, 32.65, 32.65, 
	              31.63333, 32.1833300,    33.1782,  33.1782,   32.6447001, 32.7439873, 33.450,  33.1745097,33.18594, 33.1745097,
	              33.1745097,  33.1745097,  33.1745097,  31.63333, 33.1782,  32.6447001),
	  
	  latitude=c(-13.3874,-12.284898,-13.51473,-14.19957,-14.01667, -12.3333, -13.64451, -12.29292, -13.65,                                                                
	             -12.53213,-12.53213,-13.6445104, -13.6445104,-13.6445104, -14.3166700, -13.6445104, -13.6445104,-13.6445104,-13.6445104,-13.6445104,
	             -12.2848978,-12.2848978,-12.2848978,-12.2848978, -13.0166700, -12.2848978,-12.2848978,-12.2848978, 
	             -12.532131, -12.532131, -11.1666667,-12.532131, -13.6407, -12.4000,  -12.2848978, -13.6445104, -13.63333, -13.63333,
	             -14.53333, -13.2333300, -12.29292,-12.29292, -13.6445104, -12.532131, -12.417, -12.2848978,-11.831, -12.2848978,  
	             -12.2848978, -12.2848978,  -12.2848978, -14.53333, -12.29292,-13.6445104),
		geo_from_source = FALSE
	)
	
	d <- merge(d, pts, by=c("location"), all.x=TRUE)
	d <- d[, !(names(d) %in% c("longitude.x", "latitude.x"))]
	names(d)[names(d) == 'longitude.y'] <- 'longitude'
	names(d)[names(d) == 'latitude.y'] <- 'latitude'
	
	d$crop <- gsub("groundnuts", "groundnut", d$crop) 
	d$crop <- gsub("pigeonpea", "pigeon pea", d$crop) 
	d$crop <- gsub("soyabean", "soybean", d$crop) 
	d$intercrops <- gsub("pigeonpea", "pigeon pea", d$intercrops)
	d$intercrops <- gsub("Gliricidia", "gliricidia", d$intercrops)
	
	#### about the data #####
	#The data is a mixture of on farm and on station
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	
	d <- d[!is.na(d$yield), ]
	
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, d, path=path)
}


