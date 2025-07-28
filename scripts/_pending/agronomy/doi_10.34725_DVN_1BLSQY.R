# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Some locations are unknown, which is the reason for NA in longitude and latitude.
# The excel file has five sheets. Sheets "Grain" and "Straw" are included, more completly, in "Rawdata", Same for GrainForSAS and StrawForSAS.

carob_script <- function(path) {

"
Replication data for: Cereal yield response to conservation agriculture practices in drylands of West Africa: A quantitative synthesis.

To address the decline in crop productivity in the drylands ofWest Africa, many initiatives have focused on combatin g soil degradation. Various practices including (1) parkland trees associated with crops, (2) coppicing trees, (3) green manure, (4) mulching, (5) crop rotation and intercropping, and (6) traditional soil/water conservation have been tested. The present study attempts to provide a comprehensive, quantitative synthesis of existing reports on the effect of conservation agriculture (CA) practices on crop yield response in Burkina Faso, Mali, Niger and Senegal. Out of a total of 155 reports found, 63 fulfilled all the appropriate criteria to be included in the meta-analysis of the effect of various conservation agriculture practices on the yield response of maize, millet and sorghum. The study revealed significant variability in cereal yield response (and hence risk) with all the practices examined. Despite the variability, the mean effects of the six CA practices on crop yield were more positive than negative except with parkland trees. However, for this last practice, species like Faidherbia albida exerts more positive impact on crop yield. Yield increases relative to the control were higher with green manure and mulching than with coppicing trees and parklands. Increases in yield in the six CA practices were higher on low to medium productivity sites for maize, millet and sorghum. Coppicing trees and rotations improved yields when the rainfall is greater than 800mm whereas the opposite happens with parkland and soilewater conservation measures. Mulching performed better when the rainfall is less than 600 mm. The variability (and hence yield risks) calls for more understanding of the processes and application of appropriate tree management to reduce crop yield losses while still providing products (fruits, leaves, wood, etc.) and services (soil carbon building up) for long-term sustainability of the production systems in drylands of West Africa.
"

	uri <- "doi:10.34725/DVN/1BLSQY"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)
	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=0,
		data_organization ="ICRAF", #"World Agroforetry Centre",
		publication = "doi:10.1016/j.jaridenv.2011.10.011",
		project = NA,
		carob_date = "2025-07-22",
		design = NA,
		data_type = "compilation",
		treatment_vars = "xxx",
		response_vars = "yield;fwy_total", 
		carob_contributor = "Cedric Ngakou",
		completion = 50, # explain in notes why 50%, what is missing
		notes = ""
	)
	

	f <- ff[basename(ff) == "Data of CAWT Septembe 2010_2.xls"]

	r <- carobiner::read.excel(f, sheet="Rawdata")
   
	r$Treatment <-	ifelse(grepl("Parkland", r$Technology), paste(r$Species, ":", r$Treatment, sep = " "), r$Treatment)
	
	r$crop_rotation <- ifelse(grepl("Rot-Ass", r$Technology), r$Treatment, "none")
	r$crop_rotation <- tolower(gsub("-|/", ";", r$crop_rotation))
	r$crop_rotation <- ifelse(grepl("mix|ass", r$crop_rotation), "none", r$crop_rotation)
	r$intercrops <- ifelse(grepl("Rot-Ass", r$Technology) & grepl("Mix|Ass", r$Treatment), r$Treatment, "none")
	
	
	d <- data.frame(
	   CA_practices= r$Technology,
		year = r$Year,
		country = r$Country,
		adm1 = r$Region,
		location = r$Site,
		elevation = r$Elevation,
		T_treatment = r$Treatment,
		C_treatment= paste("no", " ","CA practices", "(", r$Treatment, ")", sep = ""),
		crop = tolower(r$Crop),
		crop_rotation= r$crop_rotation,
		intercrops= r$intercrops,
		Treatment_yield= r$`Treatment yield`*1000,
		control_yield= r$`Control yield`*1000,
		reference= r$Author,
		on_farm= ifelse(grepl("On-farm", r$`Research type`), TRUE, FALSE),
		rain= r$Rainfall,
		yield_part= tolower(r$Component),
		trial_id= paste0(r$PubN,"_", r$Species) 
	)
	
	df <- reshape(d, varying = list(c("T_treatment", "C_treatment"), c("Treatment_yield", "control_yield")), v.names = c("treatment", "yield"), 
				  times = c(1,2),
				  direction= "long")

	df$CA_practices <- ifelse(grepl("no", df$treatment), paste( df$CA_practices,"(", "control", ")", sep = ""), df$CA_practices)

	df <- df[order(df$year, df$country), ]

	# fixing country
	i <- grepl("BF and Niger", df$country) & grepl("Kouaré", df$location)
	df$country[i] <- "Burkina Faso"

	i <- grepl("BF and Niger", df$country) & grepl("Gaya|Goberi|Sadoré", df$location)
	df$country[i] <- "Niger"
	### removing rows with missing country name
	df <- df[!is.na(df$country), ]

	df$planting_date <- as.character(df$year)
	df$time <- df$year <- df$id <- NULL

	### Adding variety
	df$variety <- NA
	df$variety <- ifelse(grepl("irat", df$crop), sub(".*(irat \\d+)", "\\1", df$crop), df$variety)
	df$variety <- ifelse(grepl("djigari", df$crop), "djigari", 
				  ifelse(grepl(" gorom gorom", df$crop), "gorom gorom", 
				  ifelse(grepl("icsv 4000", df$crop), "icsv 4000", 
				  ifelse(grepl("cmi 6", df$crop), "cmi 6", 
				  ifelse(grepl("fbc6", df$crop), "fbc6", 
				  ifelse(grepl("csm660", df$crop), "csm660", 
				  ifelse(grepl("sotuba", df$crop), "local", 
				  ifelse(grepl("roma", df$crop), "roma",
				  ifelse(grepl("local", df$crop), "local", 
				  ifelse(grepl("csm661", df$crop), "csm661", 
				  ifelse(grepl("niéléni", df$crop), "local", df$variety)))))))))))

	### Fixing crop rotation

	P <- carobiner::fix_name(df$crop_rotation)
	P <- gsub("sorghum after cowpea", "sorghum;cowpea", P)
	P <- gsub("millet after cowpea", "millet;cowpea", P)
	P <- gsub("  res. incorp|rot. | local| fodder|local |rotation | iar 7", "", P)
	P <- gsub("continuous maize", "maize;maize", P)
	P <- gsub("mucuna", "velvet bean", P)
	P <- gsub("cereal;cereal res. incorp|fallow", "none", P)
	P <- gsub("goundnut", "groundnut", P)
	P <- gsub("dolichos", "lablab", P)
	df$crop_rotation <- P
	df$crop_rotation <- ifelse(grepl("control", df$CA_practices), "none", df$crop_rotation)
	### Fixing  intercrops 
	P <- carobiner::fix_name(df$intercrops)
	P <- gsub("Ass. Maize/Cowpea fodder|Ass. Maize/Cowpea local|Mix maize-cowpea", "cowpea", P)
	P <- gsub("Ass. Maize/Mucuna|Mix maize-mucuna", "velvet bean", P)
	P <- gsub("Mix maize-legume", "legume", P)
	df$intercrops <- P
	df$intercrops <- ifelse(grepl("control", df$CA_practices), "none", df$intercrops)
	### millet-sorghum crop in Soil Water Conservation (SWC) is confusing (no information about the main crop and the cropping system) 
	df <- df[!grepl("millet-sorghum", df$crop), ] 

	## Fixing crop names
	df$crop <- ifelse(grepl("sorghum", df$crop), "sorghum",
			   ifelse(grepl("maize", df$crop), "maize", 
			   ifelse(grepl("cowpea", df$crop), "cowpea", 
			   ifelse(grepl("eggplant", df$crop), "eggplant",
			   ifelse(grepl("tomatoe", df$crop), "tomato", df$crop)))))

	P <- carobiner::fix_name(df$crop)
	P <- gsub("h. sabariffa", "sabariffa", P)
	P <- gsub("vegetation recovery", "vegetable", P)
	P <- gsub("grass", "grass pea", P)
	P <- gsub("chilli pepper", "chili pepper", P)
	P <- gsub("sabariffa", "roselle", P)
	P <- gsub("mucuna", "velvet bean", P)
	P <- gsub("amarante", "amaranth", P)
	df$crop <- P

	### Fixing yield part
	P <- carobiner::fix_name(df$yield_part)
	P <- gsub("pods", "pod", P)
	P <- gsub("tuber", "tubers", P)
	P <- gsub("calice", "calyx", P)
	P <- gsub("stover|haulms", "straws", P) ## not sure
	P <- gsub("biomass", "aboveground biomass", P) ## not sure
	df$yield_part <- P

	# ### Add biomass columns  
	## CN:  Is this useful? 
	d_biomass <- df[grepl("straws|aboveground biomass|leaves", df$yield_part),]
	d_biomass$fwy_total <- d_biomass$yield
	d_biomass$yield <- NULL
	d_main <- df[!grepl("straws|aboveground biomass|leaves", df$yield_part),] ## main harvestable part

	df <- merge(d_main, d_biomass, by= intersect(names(d_main), names(d_biomass)), all = TRUE)


	### Adding geo coordinate 


	geo <- data.frame(
	   
	   country= c("Senegal", "Cameroon",rep("Mali", 2), "Senegal", "Mali", rep("Burkina Faso", 3), 
				  rep("Mali", 4), "Niger", "Senegal", rep("Mali", 5), "Burkina Faso", "Mali", "Niger", "Niger", "Niger", "Burkina Faso", "Burkina Faso", rep("Burkina Faso", 18), rep("Mali", 11), rep("Niger", 6), rep("Senegal", 3)),
	   
	   location = c("Station", "Mouda", "Lagassagou", "Samanko", "Sob", "Katibougou", "Saria", "Taonsongo", 
					"Bourzanga", "Segala", "Oussoubidiagna", "Djikoye", "N'Tarla", "Illela", "Lab", "Sotuba", "Cinzana", "Tori", "Gouani", "Seno", "Kouaré", "Konobougou", "Tahoua", "Maradi", "Tillabéri", "Farako-ba", "Ziga", 
					"Bazèga", "Farako ba", "Gampela", "Karaba", "Klésso", "Kongoussi", "Kribina, Tiekouna, Siniena", "Nioniogo", "Nobéré", "Noh", "Northen Plateau central",
					"Pougyango", "Ramongho","Ranawa", "Rissiam", "Saponé", "Watinoma", "Yako", "Diakan1", "Diakan2", "Diarrakoungo", "Farsama", "Katibougou ad Sho", "Lafiabougou and Samaya", "Madipaya", "N'Goukan", "Nafagan",
					"Ouassoubilolato", "Siramana", "Sotuba and Samaya", "Bogodjotou", "Gaya", "Goberi", "Guidan Tanyo", "N'Dounga", "Sadoré", "Nioro", "Sare Yorobana"),
	   
	   
	   
	   longitude = c(-16.4033, 14.4233, -3.629, -8.084, -16.4416, -8.0923, -2.1668, 
			  -0.5439, -1.5463, -10.9727, -10.4611, -10.8423, -5.6889, 5.039, -17.4705, -7.9257, -5.967, -3.708, -7.5729, -1.1603, 0.298, -6.7591, 4.873, 7.1025, 1.3432, -4.3423, 
			  -1.1172, -1.5168, -4.305, -1.366, -2.792, -3.985, -1.525, -4.8001, -1.047, -1.202, -0.264, -0.871, -0.871, -0.871, -0.871, -1.578,
			  -1.559,  -1.579, -1.561, -11.446, -11.446, -4.778, -4.778, -7.559, -8.043, -11.447, -5.473, -5.474, -11.447, -8.12, -7.917, 1.798, 3.457, 2.846, 7.1040, 2.249, 2.124, -15.050, -15.0822),
	   
	   latitude = c(14.479, 10.6517, 13.8368, 12.5306, 14.4886, 12.5021, 12.2791, 12.1829, 13.674, 14.563, 
			   14.2575, 14.049, 12.6193, 14.3468, 14.7212, 12.6595, 13.25, 13.6186, 12.2751, 14.8861, 11.9429, 12.9192, 14.8397, 13.5012, 14.5198, 11.0825, 12.5703, 12.0163, 11.165, 12.435, 11.387, 10.945, 13.326, 10.6359, 12.566, 11.5584,
			   11.221, 12.533, 12.533, 12.533, 12.533, 13.303, 12.2457, 12.335, 12.238, 14.439, 14.439, 12.354, 12.354,
			   12.866, 12.635, 14.438, 12.377, 12.377, 14.439, 11.777, 12.662, 13.122, 11.893, 13.010, 13.500, 13.351,
			   13.513, 13.501, 13.540)
	)

	df <- merge(df, geo, by= c("country", "location"), all.x  = TRUE)

	i <- grepl("Bamako", df$adm1) & is.na(df$latitude)
	df$longitude[i] <- -8.0037
	df$latitude[i] <- 12.6406 

	i <- grepl("Niamey", df$adm1) & is.na(df$latitude)   
	df$longitude[i] <- 2.1248
	df$latitude[i] <- 13.5114
	   
	i <- grepl("Plateau central", df$adm1) & is.na(df$latitude)   
	df$longitude[i] <- -1.4692
	df$latitude[i] <- 12.3655 
	df$geo_from_source <- FALSE


	df$is_survey <- FALSE
	df$irrigated <- NA
	
	## not correct. See r$Treatment
	df$N_fertilizer <- df$P_fertilizer <- df$K_fertilizer <- as.numeric(NA) 

	## remove duplicate (from raw data) 
	df <- unique(df)

	carobiner::write_files(path, meta, df)

}


