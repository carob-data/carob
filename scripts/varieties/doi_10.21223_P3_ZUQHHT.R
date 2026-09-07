# R script for "carob"
# license: GPL (>=3)

## ISSUES
#added variables: vitamin_A, B_carotene, node_length, vine_length, root_storage_bulking

carob_script <- function(path) {

"
Dataset for: Transgressive Segregation for Continuous Storage Root Formation and Bulking in F1 Sweetpotato Population

The data set for the 'Transgressive Segregation for Continuous Storage Root Formation and Bulking in F1 Sweetpotato Population' Agronomy, Open access journal has on-station plant breeding trials conducted at Namulonge in Wakiso districts respectively between 2017 and 2018 leading to an original research article.   The on-station data are in Excel format for Sweetpotato Breeding Protocol (manual) http://www.sweetpotatoknowledge.org/). The on-station data on Continuous storage root formation and bulking scores, root numbers, vegetative growth data, and root weight from plots over 4 harvesting times used to estimate growth overtime and compute root yield, and biomass yield was the basis for the article. The pdf file has details on background information, site description, materials and methods, analysis, results and discussion, and relevant tables and illustrations. Supporting raw data and analysis tables include spreadsheets for singles harvests (at 90, 120, 150 and 180 Days after planting), the combined data sheet, and its respective analysis tables, summary means and breeding value estimations.
"

	uri <- "doi:10.21223/P3/ZUQHHT"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "MAK; CIP",
		publication = "10.12688/gatesopenres.12895.4",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;yield_marketable", 
		carob_contributor = "Illiana Kwenda",
		carob_date = "2026-08-25",
		carob_completion = 80,	
		carob_effort = 3
	)
	
	f1 <- ff[basename(ff) == "Data.xls"]
	f2 <- ff[basename(ff) == "Data_dictionary.xls"]
	f3 <- ff[basename(ff) == "Material_list.xls"]

	#r1a <- carobiner::read.excel(f1, sheet="4_Crosses H1")
	#r1b <- carobiner::read.excel(f1, sheet="5_Crosses H2")
	#r1c <- carobiner::read.excel(f1, sheet="6_Crosses H3")
	#r1d <- carobiner::read.excel(f1, sheet="7_Crosses H4")
	r1e <- carobiner::read.excel(f1, sheet = "8_All data crosses ", col_types = "text")
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

	d1 <- data.frame(
	  rep = as.integer(r1e$Rep),
	  plot_id = r1e$Plot,
	  variety = as.character(r1e$Entry),
	  variety_pedigree = paste(r1e$Female, "x", r1e$Male),
	  fwy_residue = as.numeric(gsub(",", ".", gsub("S", NA, r1e$VWt))),
	  yield = as.numeric(r1e$SRY),
	  pest_severity = r1e$Weevil,
	  harvest = r1e$HT,### harvest time in months after planting	
	  vitamin_A = r1e$VAC,#units of measurement were µg RE/100g of FW
	  beta_carotene = r1e$BCC,#units of measurement were mg/100g of FW
	  #internode_count = r1e$Int_D,
	  #node_length = r1e$Int_L,
	  yield_marketable = as.numeric(r1e$MkR_w)
	  #node_count = as.numeric(r1e$UGN), ### underground nodes
	  #vine_length = r1e$VL,
	  #root_storage_bulking = r1e$CSRFAB
	)
	
	
	
	d2 <- data.frame(
	  rep = as.integer(r1f$Rep),
	  plot_id = r1f$Plot,
	  variety = r1f$`Parental genotypes`,
	  #variety_name = r1f$Female,
	  fwy_residue = r1f$VWt,
	  yield = r1f$SRY,
	  pest_severity = r1f$Weevil,
	  harvest = r1f$HT,##harvest time in months after planting
	  vitamin_A = r1f$VAC,#units of measurement were µg RE/100g of FW
	  beta_carotene = r1f$BCC,#units of measurement were mg/100g of FW
	  #internode_count = r1f$Int_D,
	  #node_length = r1f$Int_L,
	  yield_marketable = r1f$MkR_w
	  #node_count = as.numeric(r1f$UGN), ### underground nodes
	  #vine_length = r1f$VL,
	  #root_storage_bulking = r1f$CSRFAB  ## storage root yield in t/ha-1
	)

	d7 <- data.frame(
	  variety = r3a$Cross_ID,
	  variety_code = r3a$Accession_Code,
	  date = r3a$Date_Created
	)

	d <- carobiner::bindr(d1, d2)	
	d <- merge(d, d7, by = "variety", all.x = TRUE)
	
	d$trial_id < "1"
	d$crop <- "sweetpotato"	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- NA
  d$country <- "Uganda"
  
## Publication indicated that the study was conducted at the National Crops Resources Research Institute (NaCRRI), Namulonge, Uganda	
  d$adm1 = "Wakiso"###  obtained from publication 
  d$adm2 = "Kyaddondo"#obtained from GADM
  d$adm3 = "Busukuma"#obtained from GADM
  d$location = "Namulonge" ### obtained from publication refers to the local village name
	
## see carobiner::geocode
	d$longitude <- 32.603
	d$latitude <- 0.530
	d$geo_from_source <- FALSE
	d$geo_source <- "Google Maps"

# The dataset does not indicate the harvesting date dates only indicated that harvesting was done from January to April no exact dates provided in the publication
	d$planting_date <- c("2016-09-22", "2017-03-10")
	d$harvest_date <- NA### not indicated in the publication and can not be derived from either r1e or r1f since its not clearly stated which year belongs to neither of the two
	d$DAP <- c(90, 120, 150, 180)[d$harvest]

  d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- NA
  d$fertilizer_type <- NA

  d$yield_part <- "roots"
  d$yield_moisture <- NA
  d$yield_isfresh <- TRUE
  
  carobiner::write_files(path, meta, d)
}


