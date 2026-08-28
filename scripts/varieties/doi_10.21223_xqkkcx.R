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

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/xqkkcx"
	group <- "varieties"
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

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	r4 <- carobiner::read.excel(f4)
	
	r1 <- r1[!is.na(r1$Plot), ]
	cols <- c("NPH", "Flowering time_DAP", "MTWP")
	
	for (x in cols) r1[[x]] <- as.numeric(sub("nd", NA, r1[[x]]))
	

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
	  crop = "potato",
	  LB1 = r1$LB1,
	  LB2 = r1$LB2,
	  LB3 = r1$LB3,
	  LB4 = r1$LB4,
	  LB5 = r1$LB5,
	  LB6 = r1$LB6,
	  LB7 = r1$LB7,
	  record_id = seq_len(nrow(r1))
	)
	
	
	cols <- c("LB1", "LB2", "LB3", "LB4", "LB5", "LB6", "LB7")
	
	long <- d1[, c("record_id", cols)]
	
	long <- reshape(long, varying = cols, v.names = "disease_severity", timevar = "disease", direction = "long")
	
	long$disease <- "potato late blight"
	long$pathogen <- "Phytophthora infestans"
	long$disease_severity <- as.character(long$disease_severity)
	long <- long[!is.na(long$disease_severity), ]
	long$id <- NULL

	
	d1 <- d1[, !(names(d1) %in% cols)]
	
	
	d2 <- data.frame(
	  plot_id = as.character(r2$Ord),
	  #variety = r2$CIPN,
	  variety_pedigree = paste("Female:", r2$`Male Pedigri_Female`, "Male:", r2$`Male Pedigri_Male`, sep = "; "),
	  #accession_name = paste("Female:", r2$Female_AcceNumb, "Male:", r2$Male_AcceNumb, sep = "; "),
	  accession_id = paste("Female:", r2$Female_codename, "Male:", r2$Male_codename, sep = "; "),
	  seed_source = r2$`Family cip`
	)
	
	
	d3 <- data.frame(
	  plot_id = r3$Ord, ### where the plant sample plan
	  date = r3$Date,
	  DAP = as.integer(r3$DAP),
	  intervention_category = r3$`Category of intervention`,
	  method = r3$`Type of intervention`#the specific activity / measurement/assessments that was used to collect data
	)
 
 d3$method[d3$method == "Siembra"] <- "planting"
 
 d3$method[d3$method %in% paste0("LB", 1:7)] <- "potato late blight"#late blight assessment
 
 d3$method[d3$method %in% c("NPE","PltHrv","PlVig", "PlUni")] <- "p_asp" #plant assessment
 
 d3$method[d3$method %in% c("TubUni", "TubApp", "TubSiz",
                                                  "NMTb I", "NMTb II", "NNMTb")] <- "t_asp"## tuber assessment/tuber aspect scores
 
 
 d <- merge(d1, d2, by = "plot_id", all.x = TRUE)
 
 d <- carobiner::bindr(d, d3)
 d$record_id <- seq_len(nrow(d))
 
  d$on_farm <- TRUE
	d$is_survey <-FALSE 
	d$irrigated <- NA
	

## The dataset does not  have exact location where the experiment was done. But i have assumed that the experiment was conducted at International potato Centre (CIP) in Nairobi Kenya. But this need to be confirmed. It only sates the country which is Kenya
	d$longitude <- 36.72120
	d$latitude <- -1.26933
	d$geo_source <- "Google maps"
	d$geo_from_source <- FALSE


	d$planting_date <- NA ### not indicated in the dataset
	d$harvest_date  <- NA ### not indicated in the dataset


  d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)

  d$country = "Kenya"
  d$on_farm <- TRUE
  d$is_survey <- FALSE 
  d$irrigated <- NA
  d$yield_part <- "tubers"
  d$yield_isfresh <- TRUE
  d$yield_moisture <- as.numeric(NA)
   
  carobiner::write_files(path, meta, d, long=long)
}


