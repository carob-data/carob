# R script for "carob"

carob_script <- function(path) {
  
"The AFSIS project aimed to establish an Africa Soil Information system. Data was collected in sentinel sites across sub-Saharan Africa using the Land Degradation Surveillance framework and included also multi-location diagnostic trials in selected sentinel sites to determine nutrient limitations and response to improved soil management practices (soil amendments)." 

	uri <- "doi:10.25502/20180814/0923/HJ"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		project="AfSIS", 
		publication= "doi:10.1016/j.agee.2016.05.012",
		data_organization = "IITA;ICRISAT;ABC", 
		carob_contributor="Cedric Ngakou", 
		carob_date="2023-04-04",
		data_type="experiment",
		response_vars = "yield",
		treatment_vars = "N_fertilizer;K_fertilizer;P_fertilizer;Zn_fertilizer;S_fertilizer;lime"
    )

	f1 <- ff[basename(ff) == "Kasungu_DT2011_field.csv"]
	f2 <- ff[basename(ff) == "Kasungu_DT2011_plot.csv"]	
#	fx <- ff[basename(ff) == "Kasungu_DT2011_plant.csv"]
	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
#	r3 <- read.csv(f3)
		
	## process file(s)
	d1 <- r1[, c("Site", "Cluster", "Field", "Flat", "Flong", "Village", "Season", "Soil.texture.class", "TCrop", "PCrop1", "FType1", "MType1")]
	colnames(d1) <- c("site", "cluster", "field", "latitude", "longitude", "location", "season", "soil_type", "crop", "previous_crop", "fertilizer_type", "OM_type")

	d2 <- r2[, c("Cluster", "Field", 'Rep', 'TrtDesc', 'Adj.StoverYld', 'Grn.yld.adj')]
	colnames(d2) <- c("cluster", "field", "rep", "treatment", "fwy_residue", "yield")

	#merge d1 and d2
	d2 <- d2[!is.na(d2$yield), ]
	d <- merge(d1, d2, by=c("cluster", "field"))
	d$trial_id <- paste0(d$location, "-", d$cluster)
	d$cluster <- d$field <- NULL
	
	# fix fertilizer_type name
	p <- d$fertilizer_type
	p <- gsub("\\+\\+|\\+|-|&", ";", p)
	p <- gsub(";", ";", p)
	p <- gsub(";  ", ";", p)
	p <- gsub("D Compound|D.Comp", "D-compound", p)
	p <- gsub("Urea", "urea", p)
	p[p == ""] <- "none"
	d$fertilizer_type <- p

	d$OM_type[d$OM_type ==""] <- NA
	d$OM_type[!is.na(d$OM_type)] <- "farmyard manure"
	d$OM_used <- !is.na(d$OM_type)

	d$yield <- d$yield*1000 # kg/ha

	# fix name 
	p <- carobiner::fix_name(d$previous_crop)
	p <- gsub("Sweetpotatoes", "sweetpotato", p)
	p <- gsub("SWEET POTATOES", "sweetpotato", p)
	p <- gsub("No", "none", p)
	p <- gsub("G/nuts", "groundnut", p)
	p <- gsub("Cassava", "cassava", p)
	p <- gsub("Maize", "maize", p)
	p <- gsub("Soyabean", "soybean", p)
	p <- gsub("Tobacco", "tobacco", p)
	d$previous_crop <- p

	
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	#d$elevation <- NA
	
	##### Fertilizers #####
	# fertilizer application partly extracted from 10.1016/j.agee.2016.05.012 

	d$N_fertilizer <- 0
	d$N_fertilizer[grepl("N", d$treatment)] <- 100 
	d$N_fertilizer[grepl("45N", d$treatment)] <- 45 
	d$N_fertilizer[grepl("90N", d$treatment)] <- 90 
	d$N_fertilizer[grepl("120N", d$treatment)] <- 120 
	d$N_fertilizer[grepl("150N", d$treatment)] <- 150

	d$K_fertilizer <- 0
	d$K_fertilizer[grepl("K", d$treatment)] <- 60 
	
	d$P_fertilizer <- 0
	d$P_fertilizer[grepl("P", d$treatment)] <- 30
	d$P_fertilizer[grepl("15P", d$treatment)] <- 15
	d$P_fertilizer[grepl("40P", d$treatment)] <- 40
							
	d$Zn_fertilizer <- ifelse(d$treatment=="NPK+Mn", 3, 0)						
	d$S_fertilizer <- ifelse(d$treatment=="NPK+Mn", 5, 0)

	d$lime <- ifelse(d$treatment=="NPK+Lime", 500, 0)						
   
	d$country <- "Malawi"
	d$location <- carobiner::fix_name(d$location, "title")
	d$planting_date <- "2015-12-29"
	d$harvest_date <- "2016-06-01"
	d$season <- as.character(d$season) 
	d$crop <- "maize"
	d$yield_part <- "grain"
	d$geo_from_source <- TRUE
	d$site <- trimws(d$site)

	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(path, meta, d)
}

