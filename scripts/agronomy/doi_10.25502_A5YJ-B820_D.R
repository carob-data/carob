# R script for "carob"

carob_script <- function(path) {
   
"The use of mineral fertilizer and organic inputs with an improved and local variety of cassava allow firstly to identify nutrient limitations to cassava production, and secondly to investigate the effects of variety and combined application of mineral and organic inputs on cassava growth and yields in the highland conditions of the Democratic Republic of Congo (DR Congo). Data on growth parameters, yields and yield components of the improved and local varieties of cassava, economic analysis and soil parameters, collected during two growing cycles of cassava are presented. 
The data support a research article which is under review “Increased cassava growth and yields through improved variety use and fertilizer application in the highlands of South Kivu, Democratic Republic of Congo” [1]. Data on plant height and diameter was measured throughout the growing period of the crop while the data on the storage root, stem, tradable storage root and non-tradable storage root was determined at 12 months after planting (MAP) of the field experiments. The economic analysis was performed using a simplified financial analysis where the additional benefits were calculated relative to the respective control treatments while the total costs included the purchasing prices of fertilizer and the additional net benefits, the revenue from the increased storage root yields due to fertilizer application. The value cost ratio (VCR) was calculated as the additional net benefits over the cost of fertilizer purchase."
   
   uri <-  "doi:10.25502/A5YJ-B820/D"
   group <- "agronomy" 
	ff <- carobiner::get_data(uri, path, group)
  
	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		publication= "doi:10.1016/j.fcr.2023.109056",
		data_organization = "IITA",
		carob_contributor="Cedric Ngakou",
		carob_date="2023-10-01",
		data_type="experiment",
		project=NA, 
		response_vars= "yield",
		treatment_vars="N_fertilizer;P_fertilizer;K_fertilizer"		
	)
	
	
	# read the dataset
	r1 <- read.csv(ff[basename(ff)=="Variety&Fertilizer_Effect_Data.csv"])  
	r2 <- read.csv(ff[basename(ff)=="Nutrient response_Data.csv"])  
	
	### process Variety&Fertilizer file()
	
	vrs <- c("ID", "Site", "Village", "Season", "Replicate", "Fertilizer", "Variety","Total_yield_Root_stem", "H12MAP", "FW_Stem")
	d <- rbind(r1[,vrs], r2[,vrs])
 
	colnames(d) <- c("ID", "adm2", "location", "season", "rep", "treatment", "variety", "yield","plant_height", "dmy_stems")
	
	d$yield <- d$yield*1000  ## kg/ha
	d$dmy_stems <- d$dmy_stems*1000 # kg/ha 
	# add columns
	d$country <- "Democratic Republic of the Congo"
	d$crop <- "cassava" 
	
	d$trial_id <- paste(d$ID,d$location,sep = "-")
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$ID <- NULL
	d$adm1 <- "Sud-Kivu"
	d$yield[d$yield==""] <- NA
	d <- d[!is.na(d$yield),]
	## add fertilizer
	### 100-22-83 # get from 
	d$N_fertilizer <- NA
	d$P_fertilizer <- NA
	d$K_fertilizer <- NA
	j <- d$treatment == "None"
	d$N_fertilizer[j] <- 0
	d$P_fertilizer[j] <- 0
	d$K_fertilizer[j] <- 0
	j <- grepl("NP",d$treatment) |grepl("NK",d$treatment)
	d$N_fertilizer[j] <- 100
	j <- grepl("NP",d$treatment) |grepl("PK",d$treatment)
	d$P_fertilizer[j] <- 22/2.29
	j <- grepl("NK",d$treatment) |grepl("PK",d$treatment)
	d$K_fertilizer[j] <- 83/1.2051
### add long and lat coordinate
	geoc <- data.frame(location=c("Kasheke","Cibanda","Cibandja","Muhongoza","Munanira"),
				latitude=c(-2.1518846,-2.1065462,-2.1065462,-2.0976667,-2.1057639),
				longitude=c(28.8560076,28.9186227,28.9186227,28.9069167,28.9202472),
				geo_from_source = FALSE)
	
	d <- merge(d,geoc,by="location")

	d$dmy_stems[d$dmy_stems>20000] <- NA
	d$plant_height[d$plant_height>250] <- NA
	d$yield_part <- "roots" 
	d$planting_date <- ifelse(d$season == "LR2014", "2014", "2015")
	
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, d, path=path)
	
}


