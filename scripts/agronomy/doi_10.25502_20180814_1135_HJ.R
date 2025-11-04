# R script for "carob"


carob_script <- function(path) {

"The AFSIS project aimed to establish an Africa Soil Information system. Data was collected in sentinel sites across sub-Saharan Africa using the Land Degradation Surveillance framework and included also multi-location diagnostic trials in selected sentinel sites to determine nutrient limitations and response to improved soil management practices (soil amendments)."

	uri <- "doi:10.25502/20180814/1135/HJ"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		publication=NA,
		data_organization = "IITA;ICRISAT;ABC",
		carob_contributor="Eduardo Garcia Bendito",
		carob_date="2021-10-06",
		data_type="on-farm experiment",
		project="AFSIS",
		response_vars = "yield",
		treatment_vars = "N_fertilizer;K_fertilizer;P_fertilizer;Zn_fertilizer;S_fertilizer;lime_used"
 	)

	f1 <- ff[basename(ff) == "Kiberashi_DT2010_field.csv"]
	r1 <- read.csv(f1)

	d1 <- data.frame(
		country = "Tanzania",
		location = r1$Site,
		site = r1$Village,
		latitude = r1$Flat,
		longitude = r1$Flong,
		geo_from_source = TRUE,
		on_farm = TRUE,
		is_survey = FALSE,
		crop = tolower(r1$TCrop),
		variety = r1$TCVariety,
	## should be crop names
	##intercrops = r1$CS
		previous_crop = r1$PCrop1,
		yield_part = "grain",
		landscape_position = r1$Postn,
		harvest_date = as.character(as.Date(r1$HarvDa, "%m/%d/%Y")),	
		trial_id = r1$FieldID
	)

	p <- carobiner::fix_name(gsub("/", ";", d1$previous_crop), "lower")
	p <- gsub("mangoes", "mango", p)
	p <- gsub("beans", "common bean", p)
	d1$previous_crop <- p

	# 2nd get agronomic data
	f2 <- ff[basename(ff) == "Kiberashi_DT2010_plot.csv"]
	r2 <- read.csv(f2)
	
## note that TGrainYld_adj (not used) is 25% higher than TGrainYld.	
	
	d2 <- data.frame(
		yield = r2$TGrainYld * 1000,
		fwy_residue = r2$TStoverYld * 1000,
		treatment = r2$TrtDesc,
		rep = r2$Rep,
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
	d2$OM_used <- TRUE
	d2$OM_type <- "farmyard manure"
	d2$OM_amount <- 1000
	d2$lime_used <- grepl("Lime", d2$treatment)
	
	d <- merge(d1, d2, by="trial_id", all.x = TRUE)
	
	d <- d[!is.na(d$yield), ]
	d$irrigated <- NA
	d$planting_date <- as.character(NA)
	
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, d, path=path)
}
