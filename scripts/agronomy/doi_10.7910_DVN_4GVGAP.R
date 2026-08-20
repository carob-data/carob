# R script for "carob"
# license: GPL (>=3)

## ISSUES



carob_script <- function(path) {

"
Replication Data for: Soil Biological Indices in Short and Long-Term Experiments in Kenya

Improving soil health by utilizing the appropriate tillage, cropping systems and soil management practices is important for enhanced soil quality and agricultural productivity. Of key importance is effect of these factors on the persistence of soil faunal diversity, biomass and their performance in the soil. Studies on the interactive effects of tillage, cropping systems and management practices on soil biology, diversity, abundance and activities in tropical agriculture is still scanty. With that regard, this study was crucial in investigating the effects of different tillage and cropping systems with varying rates of organic and inorganic inputs (i.e., maize stubble, manure, inorganic fertilizers and lime) on the abundance, biomass, taxonomic diversity and extracellular phosphatase enzyme activities of soil microbes, macro and mesofauna. Both Conservation agriculture and conventional tillage with rotations, intercrops and continuous cereal and legumes were compared within two long-term experiments, representing the prevailing conditions on sub-humid regions of the tropics. Also, data for one short term trial at Kenya Agricultural and Livestock Research Organization (KALRO) Kakamega station is included.
"

	uri <- "doi:10.7910/DVN/4GVGAP"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIAT; UONBI",
		publication = "https://cgspace.cgiar.org/items/5439980c-e546-4f98-bed7-409a86445423",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "land_prep_method;crop_rotation;intercrops;residue_prevcrop_used",
		response_vars = "soil_MBC;soil_MBN;soil_MBP;taxon",
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-08-18",
		carob_completion = 70,	
		carob_effort = 3
	)
	

	f1 <- ff[basename(ff) == "01. Codes.xls"]
	f2 <- ff[basename(ff) == "02. Macrofauna.csv"]
	f3 <- ff[basename(ff) == "03. Mesofauna.csv"]
	f4 <- ff[basename(ff) == "04. Microbial Biomass and Enzyme Activities.csv"]
	f5 <- ff[basename(ff) == "05. Bacteria 2016.csv"] # not sure how to capture this (treatment missing)
	f6 <- ff[basename(ff) == "06. Fungi 2017.csv"] # not sure how to capture this (treatment missing)
	f7 <- ff[basename(ff) == "07. Bacteria 2017.csv"] # not sure how to capture this (treatment missing)

	#r1 <- carobiner::read.excel(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)
	r4 <- read.csv(f4)
	#r5 <- read.csv(f5)
	#r6 <- read.csv(f6)
	#r7 <- read.csv(f7)
	
	### process 
	
	### merge Macrofauna and Mesofauna data (r2 and r3)
	col <- names(r2)[!grepl("Site|Plot|Treat|Rep|Depth", names(r2))]
	colnames(r2) <- c("Site", "Plot", "Treat","Rep", "Depth", paste0("Macrofauna_", col))
	
	
	names(r3) <- gsub("Descript", "Treat", names(r3))
	col <- names(r3)[!grepl("Site|Plot|Treat|Rep|Depth", names(r3))]
	colnames(r3) <- c("Site", "Plot", "Treat","Rep", "Depth", paste0("Mesofauna_", col))
	rr <-  merge(r2[!duplicated(r2[, c("Site", "Plot", "Treat", "Rep", "Depth")]), ], r3, by = c("Site","Plot","Treat","Rep","Depth"), all = TRUE)
	
	### Adding record_id
	id <- unique(rr[, c("Plot", "Rep", "Treat", "Depth", "Site")])
	id$record_id <- as.integer(1:nrow(id))
	rr <- merge(rr, id, by= c("Plot", "Rep", "Treat", "Depth", "Site"), all.x = TRUE)
	
	
	### long format
	col <- names(rr)[grepl("Macrofauna|Mesofauna", names(rr))]
	
	d_lon <- rr[, col]
	d_lon$record_id <- rr$record_id
	d_lon$Depth <- rr$Depth
	d_lon <- reshape(d_lon, varying = col, v.names = "count", direction = "long")
	domain <- as.data.frame(do.call(rbind, strsplit(col[d_lon$time], "_")))
	d_lon$taxon <- tolower(domain$V2)
	tx = c("acari" = "acarina", "collemb" = "collembola", "diplur" = "dipluran", "symp" = "symphyla", "enchyt" = "enchytraeids", "proturan" = "proturan", "arachnid" = "arachnida", "richness" = "species richness", "abund" = "species abundance", "olig" = "oligochaeta", "coleo" = "coleoptera", "hym" = "hymenoptera", "dipt" = "diptera", "isopt" = "isoptera", "lepid" = "lepidoptera", "diplop" = "diplopoda", "chilop" = "chilopoda", "arane" = "araneae", "hemip" = "hemiptera", "orthop" = "orthoptera", "blatt" = "blattodea", "arachnid" = "arachnida", "odonata" = "odonata", "isopod" = "isopoda", "richness total" = "total species richness")
	d_lon$taxon <- tx[d_lon$taxon]
	d_lon$tax_domain <- domain$V1
	d_lon$depth_top <- as.numeric(gsub("-", "", substr(d_lon$Depth, 1, 2)))
	d_lon$depth_bottom <- as.numeric(gsub("-", "", substr(d_lon$Depth, 3, 5)))
	d_lon$id <- d_lon$time <- d_lon$Depth <- NULL
	
	
	### wide format
	d1 <- data.frame(
	  location = carobiner::fix_name(rr$Site, "title"),
	  country = "Kenya",
	  trial_id = rr$Plot,
	  treatment = rr$Treat,
	  land_prep_method = ifelse(grepl("CT|FarmerP", rr$Treat), "conventional", "none"),
	  intercrops = ifelse(grepl("ZTMSi", rr$Treat), "soybean",
	                    ifelse(grepl("CTMBi|ZTMBi|CTMBi", rr$Treat), "common bean", "none")),
	  crop_rotation = ifelse(grepl("ZTMSr|CTMSr", rr$Treat), "maize;soybean", "none"),
	  residue_prevcrop_used = grepl("\\+CR", rr$Treat),
	  residue_prevcrop = ifelse(grepl("\\+CR", rr$Treat), 2000, 
	                   ifelse(grepl("-CR", rr$Treat), 0, NA)) ,
	  crop = ifelse(grepl("SB", rr$Treat), "common bean", "maize"),
	  rep = rr$Rep,
	  record_id = rr$record_id
	
	)
	
	### Microbial Biomass and Enzyme Activities
	d2 <- data.frame(
		record_id = as.integer(max(rr$record_id)+1: nrow(r4)),
		location = r4$Site,
		country = "Kenya",
		rep = r4$Rep,
		trial_id = r4$Plot,
		treatment = r4$Treatment.description,
		#r4$Fert,
		N_fertilizer = as.numeric(gsub("N", "", r4$N)),
		P_fertilizer = as.numeric(gsub("P", "", r4$P)),
		residue_prevcrop_used = grepl("\\+R", trimws(r4$Residue)),
		residue_prevcrop = ifelse(grepl("\\+R", trimws(r4$Residue)),2000, 0) ,
		lime = ifelse(grepl("-L", trimws(r4$Lime)), 0, 2000),
		land_prep_method = ifelse(grepl("CT", r4$Tillage), "conventional", "reduced tillage"),
		crop_rotation = tolower(gsub("-", ";", ifelse( grepl("rotation", r4$CS), gsub(" rotation| \\(T-M\\)|\\(T-M-M\\)", "", r4$CS), "none"))),
		intercrops = ifelse(grepl("intercrop", r4$CS), "common bean", 
		             ifelse(grepl("intercropping", r4$CS), "soybean", "none")),
		crop = "maize",
		soil_ACP = r4$ACP, #Acid phosphatase enzyme activity
		soil_ALP = r4$ALP, #Alkaline phosphatase enzyme activity
		soil_MBC = r4$MBC,
		soil_MBN = r4$MBN, # soil microbial biomass Nitrogen
		soil_MBP = r4$MBP # soil microbial biomass Nitrogen
	)
	
	ste <- c("CT1" = "Nyabeda", "INM" = "Madeya", "KAL" = "Kalro")
	d2$location <- ste[d2$location]
	d <- carobiner::bindr(d1, d2)
	
	### Adding geo coordinate
	
	geo <- data.frame(
	  location = c("Embu-Lt", "Kalro", "Nyabeda", "Madeya" ),
	  longitude = c(37.450, 34.7507, 34.411, 36.824),
	  latitude = c(-0.532, 0.2828, 0.133, -1.289),
	  geo_from_source = FALSE,
	  geo_source = "Google Maps"
	  
	)
	
	d <- merge(d, geo, by= "location", all.x = TRUE)
 
	d$is_survey <- FALSE
	d$on_farm <- TRUE
	d$yield <- NA
	d$yield_moisture <- NA
	d$yield_part <- "none"
	d$irrigated <- NA
	d$K_fertilizer <- as.numeric(NA)
	d$planting_date <- NA_character_
	d$harvest_date <- NA_character_
  d$yield_isfresh <- TRUE
  
  d <- unique(d)
  
	carobiner::write_files(path, meta, d, long = d_lon)
}


