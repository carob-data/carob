# R script for "carob"

# this is already included in doi_10.7910_DVN_UNLRGC

carob_script <- function(path) {

"The AFSIS project aimed to establish an  Africa Soil Information system. Data was collected in sentinel sites across sub-Saharan Africa using the Land Degradation Surveillance framework and included also multi-location diagnostic trials in selected sentinel sites to determine nutrient limitations and response to improved soil management practices (soil amendments)"

	uri <- "doi:10.25502/20180814/1554/HJ"
	group <- "agronomy"

	ff <- carobiner::get_data(uri, path, group)
 
	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		publication=NA,
		project = "AfSIS",
		data_organization = "IITA",
		carob_contributor="Cedric Ngakou",
		carob_date="2023-02-15",
		data_type="experiment",
		response_vars = "yield",
		treatment_vars = "N_fertilizer;K_fertilizer;P_fertilizer;Zn_fertilizer;S_fertilizer;lime_used"
	)
    
	f1 <- ff[basename(ff) == "Tuchila_S2_Field.csv"] ## get Field dataset 
	f2 <- ff[basename(ff) == "Tuchila_S2_Plot.csv"] ## get plot dataset
	#fx <- ff[basename(ff) == "Tuchila_S2_Plant.csv"] ## get plant dataset
 
   # read the dataset
	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	#rx <- read.csv(fx)
  
	d1 <- data.frame(
		location = r1$Village,
		latitude = r1$Flat,
		longitude = r1$Flong, 
		variety_type = r1$TCVariety,
		previous_crop = r1$PCrop1,
		OM_type = tolower(r1$MType1),
		OM_amount = r1$MAmt1, 
		fertilizer_type = r1$FType1,
		site = r1$Site,
		country = "Malawi",
		crop = "maize",
		yield_part = "grain",
		trial_id = r1$FieldID
	)
  
	p <- carobiner::fix_name(gsub("/", ";", d1$previous_crop), "lower")
	p <- gsub("maize\\+pigion peas", "maize;pigeon pea", p)
	p <- gsub("maize\\+peas", "maize;pigeon pea", p)
	p <- gsub("pigion pea", "pigeon pea", p)
	d1$previous_crop <- p
	d1$OM_used <- !d1$OM_type %in% c("", "None")
	d1$fertilizer_type <- gsub("Urea", "urea", d1$fertilizer_type)
	d1$fertilizer_type <- gsub("\\+", ";", d1$fertilizer_type)
	d1$fertilizer_type <- gsub(" ", "", d1$fertilizer_type)
	d1$OM_type <- gsub("dungs", "dung", d1$OM_type)
	d1$OM_type <- gsub("composite", "unknown", d1$OM_type)

	d2 <- data.frame(
		rep = r2$Rep,
		treatment = r2$TrtDesc,
		yield = r2$TGrainYld * 1000,
		fwy_residue = r2$TStoverYld * 1000,
		season = as.character(r2$Season),
		trial_id = r2$FieldID
	)

	d2$N_fertilizer <- d2$P_fertilizer <- d2$K_fertilizer <- d2$Zn_fertilizer <- d2$S_fertilizer <- 0
	d2$N_fertilizer[grep("NPK", d2$treatment)] <- 100
	d2$N_splits <- NA
	d2$N_splits[d2$N_fertilizer > 0] <- 3L

	d2$P_fertilizer[grep("P", d2$treatment)] <- 30
	d2$K_fertilizer[grep("K", d2$treatment)] <- 60
	d2$Zn_fertilizer[grep("MN", d2$treatment)] <- 3
	d2$S_fertilizer[grep("MN", d2$treatment)] <- 5
	d2$lime_used <- grepl("Lime", d2$treatment)

	d <- merge(d1, d2, by="trial_id")


#	d$OM_type <- ifelse(d$OM_used, "farmyard manure", "none")
#	d$OM_amount <- ifelse(d$OM_used, 1000, 0)

	d$planting_date <- as.character(NA)
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	
	# Fixing and completing goereferencing
	d$latitude <- d$latitude * -1 # Source was in north hemisphere... for Malawi.
	d$geo_from_source <- ifelse(!is.na(d$latitude) & !is.na(d$longitude), TRUE, FALSE)
	d$longitude[d$location == "Liphutu"] <- 35.314
	d$latitude[d$location == "Liphutu"] <- -15.898
	d$longitude[d$location == "Tambala"] <- 35.577
	d$latitude[d$location == "Tambala"] <- -14.450


	d <- d[!is.na(d$yield), ]

	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, d, path=path)
}
