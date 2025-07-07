# R script for "carob"


carob_script <- function(path) {
  
"The AFSIS project aimed to establish an  Africa Soil Information system. Data was collected in sentinel sites across sub-Saharan Africa using the Land Degradation Surveillance framework and included also multi-location diagnostic trials in selected sentinel sites to determine nutrient limitations and response to improved soil management practices (soil amendments)"
  
	uri <- "doi:10.25502/20180814/1154/HJ"
	group <- "agronomy"
	ff	 <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		publication=NA,#  10.1016/j.agee.2016.05.012
		data_organization = "IITA;ICRISAT;ABC",
		carob_contributor="Cedric Ngakou",
		carob_date="2023-02-22",
		data_type="experiment",
		project=NA,
		response_vars = "yield",
		treatment_vars = "N_fertilizer;K_fertilizer;P_fertilizer;Zn_fertilizer;S_fertilizer;Ca_fertilizer;Mg_fertilizer"
	)
	
	f1 <- ff[basename(ff) == "Koloko_DT2009_field.csv"] # get Field dataset 
	f2 <- ff[basename(ff) == "Koloko_DT2009_plant.csv"] # get plant dataset
	f3 <- ff[basename(ff) == "Koloko_DT2009_plot.csv"] # get plot dataset
	
	# read the dataset
	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)
	
	#process field dataset
	d1 <- r1[,c("Cluster","FieldID","Field","Country","Village","Site","Flat","Flong","TCVariety","TCrop","PCrop1","PlntDa","HarvDa")]
	colnames(d1) <- c("cluster","FieldID","Field","country","location","site","latitude","longitude","variety","crop","previous_crop","planting_date","harvest_date")
	
	#process plot data 
	d2 <- r3[,c("Cluster","FieldID","Field","Site","Rep","TrtDesc","TGrainYld","TStoverYld","Season")]
	colnames(d2) <- c("cluster","FieldID","Field","site","rep","treatment","yield","fwy_residue","season")
	
	#merge d1 and d2
	d <- merge(d1,d2,by=c("cluster","FieldID","Field","site"), all.x = TRUE)
	
	# keep the relevant columm
	d <- d[,c("country","location","site","latitude","longitude","variety","crop","previous_crop","planting_date","harvest_date","rep","treatment","yield","fwy_residue","season")]
	
	#fertilizer apply	 more information can be found here  10.1016/j.agee.2016.05.012
	d$N_fertilizer <- ifelse(d$treatment=="Control",0,
                        ifelse(d$treatment=="PK",0,60))
	
	d$K_fertilizer <- ifelse(d$treatment=="Control", 0,
                       ifelse(d$treatment=="NP", 0, 20))
	
	d$P_fertilizer <- ifelse(d$treatment=="Control", 0,
                       ifelse(d$treatment=="NK", 0, 30))
	
	d$Zn_fertilizer <- ifelse(d$treatment=="NPK+MN", 3, 0)
	
	d$S_fertilizer <- ifelse(d$treatment=="NPK+MN", 5, 0)
	d$Ca_fertilizer <- ifelse(d$treatment=="NPK+MN",10,0)
	d$Mg_fertilizer <- ifelse(d$treatment=="NPK+MN",5,0)
	d$N_splits <- ifelse(d$treatment > 0, 3L, 0L)
	
	#Add columns 
	d$OM_type <- NA
	d$OM_used <- FALSE
	
	d$trial_id <- as.character(as.integer(as.factor(d$location)))
	d$OM_type[grepl("+MN",d$treatment)] <- "farmyard manure"
	d$OM_used[grepl("farmyard manure",d$OM_type)] <- TRUE
	# previous crop name normalization 

	p <- carobiner::fix_name(d$previous_crop,"lower")
	p <- gsub("kolokoland",NA,p)
	p <- gsub("millet","pearl millet",p)
	p[p == ""] <- "none"
	p[p =="groundnuts(aracide)"] <- "groundnut"
	d$previous_crop <- p
	# fix crop name
	d$crop <- "sorghum"
	# fix yield unit
	d$yield <- d$yield*1000
	d$fwy_residue <- d$fwy_residue*1000
		# data type
	d$season <- as.character(d$season)
	# change date format
	d$planting_date <- format(as.Date(d$planting_date, format = "%m/%d/%Y"), "%Y-%m-%d")
	
	d$harvest_date <- format(as.Date(d$harvest_date, format = "%m/%d/%Y"), "%Y-%m-%d")
	# fill whitespace in observation 
	d[d==""] <- NA
	d$yield_part <- "grain"
	d <- d[!is.na(d$yield), ]
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$geo_from_source <- TRUE
	
	carobiner::write_files(meta, d, path=path)
}

