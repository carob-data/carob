# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them
#added variables: vine_weight, vit_A_content, carotene_B_cont, node_length, vine_length, root_storage_bulking

carob_script <- function(path) {

"
Dataset for: Transgressive Segregation for Continuous Storage Root Formation and Bulking in F1 Sweetpotato Population

The data set for the 'Transgressive Segregation for Continuous Storage Root Formation and Bulking in F1 Sweetpotato Population' Agronomy, Open access journal has on-station plant breeding trials conducted at Namulonge in Wakiso districts respectively between 2017 and 2018 leading to an original research article.   The on-station data are in Excel format for Sweetpotato Breeding Protocol (manual) http://www.sweetpotatoknowledge.org/). The on-station data on Continuous storage root formation and bulking scores, root numbers, vegetative growth data, and root weight from plots over 4 harvesting times used to estimate growth overtime and compute root yield, and biomass yield was the basis for the article. The pdf file has details on background information, site description, materials and methods, analysis, results and discussion, and relevant tables and illustrations. Supporting raw data and analysis tables include spreadsheets for singles harvests (at 90, 120, 150 and 180 Days after planting), the combined data sheet, and its respective analysis tables, summary means and breeding value estimations.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/P3/ZUQHHT"
	group <- "varieties_sweetpotato"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "MAK; CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "pest_severity;yield_marketable", 
		carob_contributor = "Illiana Kwenda",
		carob_date = "2026-08-25",
		carob_completion = 80,	
		carob_effort = 3
	)
	

	f1 <- ff[basename(ff) == "Data.xls"]
	f2 <- ff[basename(ff) == "Data_dictionary.xls"]
	f3 <- ff[basename(ff) == "Material_list.xls"]

	r1a <- carobiner::read.excel(f1, sheet="4_Crosses H1")
	r1b <- carobiner::read.excel(f1, sheet="5_Crosses H2")
	r1c <- carobiner::read.excel(f1, sheet="6_Crosses H3")
	r1d <- carobiner::read.excel(f1, sheet="7_Crosses H4")
	#r1e <- carobiner::read.excel(f1, sheet="8_All data crosses ")
	r1e <- readxl::read_excel(f1, sheet = "8_All data crosses ", col_types = "text")
	r1f <- carobiner::read.excel(f1, sheet="9_Parents_check")
	#r1g <- carobiner::read.excel(f1, sheet="10_Aoutput H1")
	#r1h <- carobiner::read.excel(f1, sheet="11_Aoutput H2")
	#r1i <- carobiner::read.excel(f1, sheet="12_Aoutput H3")
	#r1j <- carobiner::read.excel(f1, sheet="13_Aoutput H4")
	#r1k <- carobiner::read.excel(f1, sheet="14_Aoutput across H")
	#r1l <- carobiner::read.excel(f1, sheet="15_All means_ across analysis")
	#r1m <- carobiner::read.excel(f1, sheet="16_Means for GCA analysis H1")
	#r1n <- carobiner::read.excel(f1, sheet="17_Means H2 for GCA analysis")
	#r1o <- carobiner::read.excel(f1, sheet="18_Means H3 for GCA")
	#r1p <- carobiner::read.excel(f1, sheet="19_Means H4 for GCA")
	#r1q <- carobiner::read.excel(f1, sheet="20_Means crosses across H ")
	#r1r <- carobiner::read.excel(f1, sheet="21_Presented tables")
	#r1s <- carobiner::read.excel(f1, sheet="22_Used parent output")
	#r1t <- carobiner::read.excel(f1, sheet="23_Heterosis")
	#r1u <- carobiner::read.excel(f1, sheet="24_Mean parents and crosses")
	#r2 <- carobiner::read.excel(f2)
	r3a <- carobiner::read.excel(f3, sheet="Material_List")
	r3b <- carobiner::read.excel(f3, sheet="Crosses_Coding")

	
## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
	  country = "Uganda",
	  adm1 = "Central region",
	  adm2 = "Wakiso",
	  adm3 = "Kyaddondo",
	  location = "Namulonge",
	  rep = as.integer(r1a$Rep),
	  plot_id = r1a$Plot,
	  treatment = r1a$Entry,
	  variety = r1a$Entry,
	  variety_type = "cross",
	  #variety_name = r1a$Female,
	  vine_weight = r1a$VWt,
	  dmy_roots = as.numeric(r1a$SRY),
	  pest_severity = r1a$Weevil,
	  harvest_index = as.numeric(r1a$HT), # harvesting time
	  vit_A_content = r1a$VAC,
	  carotene_B_cont = r1a$BCC,
	  #internode_count = r1a$Int_D,
	  node_length = r1a$Int_L,
	  yield_marketable = as.numeric(r1a$MkR_w),
	  node_count = as.numeric(r1a$UGN), ### underground nodes
	  vine_length = r1a$VL,
	  root_storage_bulking = r1a$CSRFAB
	)
	
	d2 <- data.frame(
	  country = NA,
	  adm1 = NA,
	  adm2 = NA,
	  adm3 = "Wakiso",
	  location = "Namulonge",
	  rep = as.integer(r1b$Rep),
	  plot_id = r1b$Plot,
	  treatment = r1b$Entry,
	  variety = r1b$Entry,
	  variety_type = "cross",
	  #variety_name = r1b$Female,
	  vine_weight = r1b$VWt,
	  dmy_roots = as.numeric(r1b$SRY),
	  pest_severity = r1b$Weevil,
	  harvest_index = as.numeric(r1b$HT), # harvesting time
	  vit_A_content = r1b$VAC,
	  carotene_B_cont = r1b$BCC,
	  #internode_count = r1b$Int_D,
	  node_length = r1b$Int_L,
	  yield_marketable = as.numeric(r1b$MkR_w),
	  node_count = as.numeric(r1b$UGN), ### underground nodes
	  vine_length = r1b$VL,
	  root_storage_bulking = r1b$CSRFAB
	)
	
	
	
	d3 <- data.frame(
	  country = NA,
	  adm1 = NA,
	  adm2 = NA,
	  adm3 = "Wakiso",
	  location = "Namulonge",
	  rep = as.integer(r1c$Rep),
	  plot_id = r1c$Plot,
	  treatment = r1c$Entry,
	  variety = r1c$Entry,
	  variety_type = "cross",
	  #variety_name = r1c$Female,
	  vine_weight = r1c$VWt,
	  dmy_roots = as.numeric(r1c$SRY),
	  pest_severity = r1c$Weevil,
	  harvest_index = as.numeric(r1c$HT), # harvesting time
	  vit_A_content = r1c$VAC,
	  carotene_B_cont = r1c$BCC,
	  #internode_count = r1c$Int_D,
	  node_length = r1c$Int_L,
	  yield_marketable = as.numeric(r1c$MkR_w),
	  node_count = as.numeric(r1c$UGN), ### underground nodes
	  vine_length = r1c$VL,
	  root_storage_bulking = r1c$CSRFAB
	)
	
	d4 <- data.frame(
	  country = NA,
	  adm1 = NA,
	  adm2 = NA,
	  adm3 = "Wakiso",
	  rep = as.integer(r1d$Rep),
	  plot_id = r1d$Plot,
	  treatment = r1d$Entry,
	  variety = r1d$Entry,
	  variety_type = "cross",
	  #variety_name = r1d$Female,
	  vine_weight = r1d$VWt,
	  dmy_roots = as.numeric(r1d$SRY),
	  pest_severity = r1d$Weevil,
	  harvest_index = as.numeric(r1d$HT), # harvesting time
	  vit_A_content = r1d$VAC,
	  carotene_B_cont = r1d$BCC,
	  #internode_count = r1d$Int_D,
	  node_length = r1d$Int_L,
	  yield_marketable = as.numeric(r1d$MkR_w),
	  node_count = as.numeric(r1d$UGN), ### underground nodes
	  vine_length = r1d$VL,
	  root_storage_bulking = r1d$CSRFAB
	)
	
	
	d5 <- data.frame(
	  country = NA,
	  adm1 = NA,
	  adm2 = NA,
	  adm3 = "Wakiso",
	  location = "Namulonge",
	  rep = as.integer(r1e$Rep),
	  plot_id = r1e$Plot,
	  treatment = r1e$Entry,
	  variety = r1e$Entry,
	  variety_type = "cross",
	  #variety_name = r1e$Female,
	  vine_weight = r1e$VWt,
	  dmy_roots = as.numeric(r1e$SRY),
	  pest_severity = r1e$Weevil,
	  harvest_index = as.numeric(r1e$HT), # harvesting time
	  vit_A_content = r1e$VAC,
	  carotene_B_cont = r1e$BCC,
	  #internode_count = r1e$Int_D,
	  node_length = r1e$Int_L,
	  yield_marketable = as.numeric(r1e$MkR_w),
	  node_count = as.numeric(r1e$UGN), ### underground nodes
	  vine_length = r1e$VL,
	  root_storage_bulking = r1e$CSRFAB
	)
	

	d6 <- data.frame(
	  country = NA,
	  adm1 = NA,
	  adm2 = NA,
	  adm3 = "Wakiso",
	  location = "Namulonge",
	  rep = as.integer(r1f$Rep),
	  plot_id = r1f$Plot,
	  treatment = r1f$`Parental genotypes`,
	  variety = NA,
	  variety_type = "cross",
	  #variety_name = r1f$Female,
	  vine_weight = r1f$VWt,
	  dmy_roots = as.numeric(r1f$SRY),
	  pest_severity = r1f$Weevil,
	  harvest_index = as.numeric(r1f$HT), # harvesting time
	  vit_A_content = r1f$VAC,
	  carotene_B_cont = r1f$BCC,
	  #internode_count = r1f$Int_D,
	  node_length = r1f$Int_L,
	  yield_marketable = r1f$MkR_w,
	  node_count = as.numeric(r1f$UGN), ### underground nodes
	  vine_length = r1f$VL,
	  root_storage_bulking = r1f$CSRFAB
	)
	

	d7 <- data.frame(
	  variety = r3a$Cross_ID,
	  variety_code = r3a$Accession_Code,
	  date = r3a$Date_Created
	)

	d <- carobiner::bindr(d1, d2, d3, d4, d5, d6)
	
	d <- merge(d, d7, by = "variety", all.x = TRUE)
	d <- d[!duplicated(d), ]
	
	d$crop <- "sweetpotato"
	d$trial_id <- "1"
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- NA
  d$country <- "Uganda"
		
## see carobiner::geocode
	d$longitude <- 32.615
	d$latitude <- 0.525
	d$geo_from_source <- FALSE
	d$geo_source <- "Google Maps"

# The dataset does not indicate the planting dates but only stated that the experiment was done in 2017 and 2018
	d$planting_date <- NA
	d$harvest_date <- NA

  d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- NA
  d$fertilizer_type <- NA


  d$yield_part <- "tubers"
  d$yield_moisture <- as.numeric(NA)
  d$yield_isfresh <- TRUE
  

	carobiner::write_files(path, meta, d)
}


