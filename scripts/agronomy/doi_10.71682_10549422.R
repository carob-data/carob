# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Nitrous oxide (N2O) emission, leaching and soil mineral N data for BNI and Non-BNI Roelfs wheat variety for Batan and Obregon 2024-2025 cycle

Data from experiments evaluating Biological Nitrification Inhibition (BNI) wheat. A pair of Roelfs wheat variety (with and without the BNI trait) was planted in June 2024 in Batan and December 2024 in Obregon in a split plot randomized design. Main plot was nitrogen fertilizer rate (0, 75, 150, 225 kg/ha) in the form of ammonium sulphate while split plots were variety. Additionally, synthetic nitrification inhibition (SNI) was applied to non BNI variety for N rates of 75, 150, 225 kg/ha to act as an external control. Nitrous oxide emissions (N2O) were measured throughout the season with static chambers using laser gas analyzers (Aeris Ultra Mira). Leaching was measured by burying 7 g of ionic resin in plastic tubes (5 cm diameter) at 60 cm below the wheat. Mineral N was measured at the beginning, mid, and end of each season.
"


	uri <- "doi:10.71682/10549422"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT; AARU",
		publication = NA,
		project = NA,
		carob_date = "2026-05-15",
		design = "RCB",
		data_type = "experiment",
		treatment_vars = "N_fertilizer;variety_type",
		response_vars = "NO3_trapped;NH4_trapped;flux_CO2;flux_N2O", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_completion = 90,
		carob_effort = 2
	
	)
	

	f1 <- ff[basename(ff) == "Leaching_Batan_2024_BNI.xlsx"]
	f2 <- ff[basename(ff) == "Leaching_Obregon_2025_BNI.xlsx"]
	f3 <- ff[basename(ff) == "Min_N_Batan_2024-BNI.xlsx"]
	f4 <- ff[basename(ff) == "Min_N_Obregon_2025_BNI.xlsx"]
	f5 <- ff[basename(ff) == "N2O_El_Batan_2024_BNI.xlsx"]
	f6 <- ff[basename(ff) == "N2O_Obregon_2025_BNI.xlsx"]

	r1 <- carobiner::read.excel(f1, sheet="leaching_Batan_2024-BNI")
	r2 <- carobiner::read.excel(f2, sheet="Leaching_Obregon_2025_BNI ")
	r3 <- carobiner::read.excel(f3, sheet="MIN N Batan from dataset")
	r4 <- carobiner::read.excel(f4, sheet="Min N Obregon-2025-BNI")
	r5 <- carobiner::read.excel(f5, sheet="Batan_2024_N2O")
	r6 <- carobiner::read.excel(f6, sheet="Obregon_2025_N2O")

	### process 
	
### location :Batan
	d1 <- data.frame(
	  plot_id = as.character(r1$`Plot number`),
	  harvest_date = as.character(r1$`Sampling date`),
	  rep = as.integer(r1$Replication),
	  N_fertilizer = r1$`N rate (kg/ha)`,
	  variety_type = r1$`BNI component`,
	  variety = r1$Variety,
	  NO3_trapped = r1$`Total NO3-N kg/ha`, ## Total NO3-N trapped in the season (kg/ha)
	  location = "El Batan",
	  planting_date ="2024-06",
	  trial_id = "1"
	)
	
	
	### location : Obregon
	d2 <- data.frame(
		plot_id = as.character(r2$`Plot number`),
		harvest_date = as.character(r2$`Harvesting Date`),
		rep = as.integer(r2$Replication),
		N_fertilizer = r2$`N level`,
		variety_type = r2$`BNI component`,
		variety = r2$Variety,
		NO3_trapped = r2$`Total NO3 N kg/ha`, #Total NO3-N trapped in the season (kg/ha)
		NH4_trapped = r2$`Total NH4 N kg /ha`,
		location = "Obregon",
		planting_date = "2024-12",
		trial_id = "2"
	)
	
	##### soil information location: Batan
	d3 <- data.frame(
	  plot_id = as.character(r3$`PLOT NUMBER`),
	  rep = as.integer(r3$REPLICATION),
	  depth = 10,
	  N_fertilizer = r3$`N RATE (kg/ha)`,
	  variety_type = r3$`BNI COMPONENT`,
	  variety = r3$VARIETY,
	  soil_NO3 = r3$`NO3-N  mg/kg DW`,
	  soil_NH4 = r3$`NH4-N  mg/kg DW`
	)
	
	### merge d1 and d3
	d1_agg <- aggregate(.~ trial_id + harvest_date +location+ plot_id + variety_type +rep +N_fertilizer+ variety + planting_date,d1, function(X) mean(X) )
	d1_agg$record_id <- as.integer(1:nrow(d1_agg))
	d3_agg <- aggregate(. ~ plot_id + variety_type +rep +N_fertilizer+ variety ,d3, function(X) mean(X) )
	dd <- merge(d1_agg , d3_agg, by= c("N_fertilizer", "variety", "variety_type", "rep", "plot_id"), all.x = TRUE)
	
	### soil information. location: Obregon
	d4 <- data.frame(
	  plot_id = as.character(r4$`PLOT NUMBER`),
	  rep = as.integer(r4$REPLICATION),
	  depth = 90,                      
	  N_fertilizer = r4$`N RATE (kgN ha)`,
	  variety_type = r4$`BNI COMPONENT`,
	  variety = r4$VARIETY,
	  soil_NO3 = r4$`mg NO3-N/kg DW`,
	  soil_NH4 = r4$`mg NH4-N /kgDW`
	)
	
	### merge d2 and d4
	d2_agg <- aggregate(. ~ trial_id+ harvest_date +location+ plot_id + variety_type +rep +N_fertilizer+ variety + planting_date,d2, function(X) mean(X) )
	d2_agg$record_id <- as.integer(nrow(dd)+1:nrow(d2_agg))
	d4_agg <- aggregate(. ~ plot_id + variety_type +rep +N_fertilizer+ variety+ depth, d4, function(X) mean(X) )
	dd1 <- merge(d2_agg , d4_agg, by= c("N_fertilizer", "variety", "variety_type", "rep", "plot_id"), all.x = TRUE)
	d <- carobiner::bindr(dd, dd1)
	
	####
	d5 <- data.frame(
	  plot_id =  as.character(r5$Plot_No_Act),
	  date = as.character(r5$DATE),
	  N_fertilizer = r5$`N-fertilizer rate`,
	  flux_CO2 = r5$LM.flux_CO2*44.01 * 3.6, #mg/m2/h
	  flux_N2O = r5$LM.flux_N2O*44.013 * 3.6
	  
	)
	
	
	d5 <- aggregate(d5[, c("flux_CO2", "flux_N2O")], d5[, c("plot_id", "date", "N_fertilizer")], mean)
  d5 <- merge(d5, dd[, c("record_id", "plot_id", "N_fertilizer")], by = c("plot_id", "N_fertilizer"), all.x = TRUE)
  long1 <- d5[, c("record_id", "date", "flux_CO2", "flux_N2O")]	
  long1 <- long1[!is.na(long1$record_id),]
 ##
	d6 <- data.frame(
	  plot_id =  as.character(r6$Plot_No_Act),
	  date = as.character(r6$DATE),
	  N_fertilizer = r6$`N-fertilizer rate`,
	  flux_CO2 = r6$LM.flux_CO2*44.01 * 3.6, #mg/m2/h
	  flux_N2O = r6$LM.flux_N2O*44.013 * 3.6
	)
	
	
	### long format
	d6 <- aggregate(d6[, c("flux_CO2", "flux_N2O")], d6[, c("plot_id", "date", "N_fertilizer")], mean)
	d6 <- merge(d6, dd1[, c("record_id", "plot_id", "N_fertilizer")], by = c("plot_id", "N_fertilizer"), all.x = TRUE)
	long2 <- d6[, c("record_id", "date", "flux_CO2", "flux_N2O")]	
	long2 <- long2[!is.na(long2$record_id),]
	
	long <- carobiner::bindr(long1, long2)
	
	#### adding long and lat
	geo <- data.frame(
	  location = c("El Batan","Obregon"),
	  longitude = c(-103.3464,  -109.936),
	  latitude = c(20.721, 27.488)
	)
	
	d <- merge(d, geo, by = "location", all.x = TRUE)
	
	###
	d$crop <- "wheat"
	d$is_survey <- FALSE
	d$on_farm <- TRUE
	d$yield <- NA
	d$yield_moisture <- NA_real_
	d$yield_part <- "none"
	d$country <- "Mexico"
	d$geo_from_source <- FALSE
	d$irrigated <- NA
	d$yield_isfresh <- NA
	d$K_fertilizer <- d$P_fertilizer <- as.numeric(NA)
	

	carobiner::write_files(path, meta, d, long = long)
}

