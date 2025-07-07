# R script for "carob"

# to do: FNAppln and PKAppln
# also see: "doi:10.7910/DVN/C6DIIC"


carob_script <- function(path) {
  
"The AFSIS project aimed to establish an Africa Soil Information system. Data was collected in sentinel sites across sub-Saharan Africa using the Land Degradation Surveillance framework and included also multi-location diagnostic trials in selected sentinel sites to determine nutrient limitations and response to improved soil management practices (soil amendments)."
  
	uri <- "doi:10.25502/20180814/1446/HJ"
	group <- "agronomy" 

	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		project= "AfSIS", 
		publication = "doi:10.1016/j.agee.2016.05.012",
		data_organization = "IITA;ICRISAT;ABC",
		data_type="on-farm experiment",
		carob_contributor = "Eduardo Garcia Bendito",
		carob_date="2024-02-28",
		response_vars = "yield",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;lime;Ca_fertilizer;Zn_fertilizer;S_fertilizer;Mg_fertilizer;OM_amount;lime"
	)
	
	ffield <- ff[basename(ff) == "Sidindi_LR2010_Field.csv"]
	fplant <- ff[basename(ff) == "Sidindi_LR2010_plant.csv"]
	fplot <- ff[basename(ff) == "Sidindi_LR2010_Plot.csv"]

	field <- read.csv(ffield, na.strings="")
	plot <- read.csv(fplot, na.strings="")
	plant <- read.csv(fplant, na.strings="")
	plant <- aggregate(Plant.height..cm. ~ FieldID + PlotID, data = plant, 
										 FUN = function(x) mean(x, na.rm = TRUE))

	r <- merge(plot, field, by = "FieldID")
	r <- merge(r, plant, by = c("FieldID", "PlotID"))
	
	d <- data.frame(
		on_farm = TRUE,
		is_survey = FALSE,
		irrigated = FALSE,
		treatment = r$TrtDesc,
		rep = r$Rep,
		country = "Kenya",
		adm1 = "Siaya",
		location = r$Site.x,
		site = trimws(r$Village),
		longitude = r$Flong,
		latitude = r$Flat,
		geo_from_source = TRUE,
		crop = trimws(tolower(r$TCrop)),
		variety = r$TCVariety,
		trial_id = gsub("Sidi2010a", "", r$FieldID),
		yield_part = "grain",
		plant_height = r$Plant.height..cm.,
		dmy_storage = r$Grn_yld_adj * 1000
	)

	# fresh weight
	d$yield <- d$dmy_storage / 0.85

	##### Fertilizers #####
	# according to https://www.sciencedirect.com/science/article/pii/S0167880916302584
	# "NPK", "NPK+Lime", "NPK+Manure", "NPK+MN", "PK", "NK", "NP"
	d$N_fertilizer <- d$K_fertilizer <- d$P_fertilizer <- 0
	d$N_fertilizer[grep("N", r$TrtDesc)] <- 100
	d$K_fertilizer[grep("K", r$TrtDesc)] <- 60
	d$Ca_fertilizer <- d$Mg_fertilizer <- d$S_fertilizer <- d$Zn_fertilizer <- 0
	d$P_fertilizer[grep("P", r$TrtDesc)] <- 30
	d$Ca_fertilizer[r$TrtDesc == "NPK+MN"] <- 10
	d$Mg_fertilizer[r$TrtDesc == "NPK+MN"] <- 5
	d$S_fertilizer[r$TrtDesc == "NPK+MN"] <- 5
	d$Zn_fertilizer[r$TrtDesc == "NPK+MN"] <- 3
	d$lime <- 0
	d$lime[r$TrtDesc == "NPK+Lime"] <- 500
	d$OM_amount <- 0
	d$OM_amount[r$TrtDesc == "NPK+Manure"] <- 10 * 1000
	d$OM_used <- d$OM_amount == 500
	d$OM_type <- NA
	d$OM_type[r$TrtDesc == "NPK+Manure"] <- "farmyard manure"

	## normalize names 
	d$fertilizer_type <- NA
	d$fertilizer_type[r$FType1 != ""] <- "DAP"
	d$fertilizer_type[r$FType2 != ""] <- "urea"
	d$fertilizer_type[r$FType1 != "" & r$FType2 != ""] <- paste0("DAP; urea")
	i <- d$N_fertilizer == 0 & d$K_fertilizer == 0 & d$P_fertilizer == 0
	d$fertilizer_type[i] <- "none"
	

	r$CobFW[r$CobFW == "."] <- NA
	d$fwy_residue <- 10000 * r$TStoverYld / r$Harea
	
	d$seed_weight <- r$X100GrainDW * 10 # Adjusting to 1000 grains
	d$seed_weight[d$seed_weight == 0] <- NA
	
	d$previous_crop <- trimws(tolower(r$PCrop1))
	d$previous_crop[d$previous_crop == "sweet potato"] <- "sweetpotato"
	d$previous_crop <- gsub(", ", "; ", d$previous_crop)
	d$previous_crop <- gsub("beans", "bean", d$previous_crop)
	d$previous_crop <- gsub("bean", "common bean", d$previous_crop)
	d$previous_crop[d$previous_crop == ""] <- NA
	
	d$crop_rotation <- apply(r[, c("PCrop1", "P2Crop", "P3Crop", "P4Crop")], 1, \(i) paste(na.omit(i), collapse="; "))
	d$crop_rotation[d$crop_rotation==""] <- NA
	d$crop_rotation <- gsub("sweet potato", "sweetpotato", d$crop_rotation)
	d$crop_rotation <- gsub("sweetpotatoes", "sweetpotato", d$crop_rotation)
	d$crop_rotation <- gsub("beans", "common bean", d$crop_rotation)
	d$crop_rotation <- gsub("bananas", "banana", d$crop_rotation)
	d$crop_rotation <- gsub(" ;", ";", d$crop_rotation)
	d$crop_rotation <- gsub(",", ";", d$crop_rotation)
	d$crop_rotation <- gsub("cowpeas", "cowpea", d$crop_rotation)
	d$crop_rotation <- gsub("ground nuts", "groundnut", d$crop_rotation)


## the dates are a mess. Different formats and inconsistencies with e.g. harvesting after planting, and in different years). Would need to check with author. Perhaps the planting date is correct 
	pdate1 <- as.Date(r$PlntDa, "%d/%m/%Y")
	pdate2 <- as.Date(r$PlntDa, "%m/%d/%Y")
	pdate1[is.na(pdate1)] <- pdate2[is.na(pdate1)]
	d$planting_date <- as.character(pdate1)
	
	d <- d[!is.na(d$yield), ]
	d$seed_weight[d$seed_weight > 600] <- NA

	carobiner::write_files(meta, d, path=path)
}


