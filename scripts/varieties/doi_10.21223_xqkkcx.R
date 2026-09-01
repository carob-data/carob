# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them
## added variable growth_habbit
### data set does not have planting dates eg recorded as DAP.
## Missing harvest_date
## added t_asp meaning tuber aspect score

carob_script <- function(path) {

"
Dataset for: Novel trait discovery for LB resistance in wild potato background (Second trial_Kenya)

319 genotypes from twenty-six biparental populations from crosses of wild potato species S. megistacrolobum, S. microdontum, S. tarijense and self-compatible (SC) 2x hybrid derived from crossing 2x landraces with SC sources of S. chacoense were exposed to late blight under natural infection. Statistical Augmented row-column design without replication of 3 hill plots was properly established, five varieties with known late blight response were included as controls. After 30 days after planting (DAP), the percentage of leaf area affected by late blight infection was recorded by plot throughout the season to subsequently compute the area under the disease progress curve (AUDPC).
"
	
	uri <- "doi:10.21223/xqkkcx"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)
	
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "row-column",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "disease_severity;disease;yield_marketable", 
		carob_contributor = "Illiana Kwenda",
		carob_date = "2026-08-25",
		carob_completion = 90,	
		carob_effort = 6
	)
	

	f1 <- ff[basename(ff) == "01_Fieldbook_CWR_2023_LateBlight_dv_Kenya.xlsx"]
	f2 <- ff[basename(ff) == "02_Material List_CWR_2023_LateBlight_dv_Kenya.xlsx"]
	f3 <- ff[basename(ff) == "03_Crop Managment_CWR_2023_LateBlight_dv_Kenya.xlsx"]
	f4 <- ff[basename(ff) == "04_Data_dictionary_CWR_2023_LateBlight_dv_Kenya.xlsx"]

	r1 <- carobiner::read.excel(f1, na="nd")
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	r4 <- carobiner::read.excel(f4)
	
	r1 <- r1[!is.na(r1$Plot), ]

	d1 <- data.frame(
	  plot_id = as.character(r1$Plot),
	  trial_id = r1$UniqueID,
	  flowering_days = as.numeric(r1$`Flowering time_DAP`),
	  flesh_color = r1$TBFSH1,
	  yield = as.numeric(r1$MTWP)*1000,
	  yield_marketable = as.numeric(r1$MTWP)*1000,
	  growth_habit = r1$PGH,
	  #tuber_size = r1$Tub_size,
	  p_asp = r1$Plant_vigor,
	  treatment = r1$CloneID,
	  variety = r1$CloneID,
	  variety_type = "crosses of wild potato species",
	  crop = "potato"
	)

	long <- data.frame(
	  LB1 = r1$LB1,
	  LB2 = r1$LB2,
	  LB3 = r1$LB3,
	  LB4 = r1$LB4,
	  LB5 = r1$LB5,
	  LB6 = r1$LB6,
	  LB7 = r1$LB7
	)
	
	d1$record_id <- long$record_id <- seq_len(nrow(d1))
	
	cols <- grep("LB", names(long), value = TRUE)
	long2 <- reshape(long, varying = cols, v.names = "disease_severity", timevar = "date", direction = "long")
	long2$disease <- "potato late blight"
	long2$pathogen <- "Phytophthora infestans"
	long2$disease_severity <- as.character(long2$disease_severity)
	long2 <- long2[!is.na(long2$disease_severity), ]
	long2$id <- NULL
	
	
	d2 <- data.frame(
	  sample_id = as.character(r2$Ord),
	  variety = r2$CIPN,
	  variety_pedigree = r2$`Male Pedigri_Female`,
	  #accession_name = r2$Female_AcceNumb,
	  variety_code = r2$Female_codename 
	  #seed_source = r2$`Family cip`
	)

 
	d3 <- data.frame(
	  sample_id = r3$Ord, ### plant sample plan
	  date = r3$Date,
	  DAP = as.integer(r3$DAP),
	  intervention_category = r3$`Category of intervention`,
	  method = r3$`Type of intervention`#the specific activity / measurement/assessments that was used to collect data
	)
 
 d1$planting_date <- d3$date[d3$method == "Siembra"]
 d1$harvest_date <- NA
 
 lb_dates <- d3[d3$intervention_category %in% cols, c("intervention_category", "date")]
 long$date <- lb_dates$date[match(paste0("LB", long$disease), lb_dates$intervention_category)]
 
 
 d <- merge(d1, d2, by = "variety", all.x = TRUE)
 

  d$on_farm <- TRUE
	d$is_survey <-FALSE 
	d$irrigated <- NA
	

## The dataset does not  have exact location where the experiment was done. 
	d$longitude <- NA
	d$latitude <- NA
  d$geo_source <- NA
	d$geo_from_source <- FALSE

  d$country = "Kenya"
  d$on_farm <- NA
  d$is_survey <- FALSE 
  d$irrigated <- NA
  d$yield_part <- "tubers"
  d$yield_isfresh <- TRUE
  d$yield_moisture <- as.numeric(NA)
   
  carobiner::write_files(path, meta, d, long=long)
}


