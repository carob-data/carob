# R script for "carob"

carob_script <- function(path) {
  
"CIMMYT’s durum wheat breeding program performed parallel selection in conventional tilled (CT) and zero tillage (ZT) soils with the aim to compare the effect of selection under either CT or ZT on the performance of selected progenies. From 16 initial crosses, 234 lines were selected under CT and 250 under ZT. All 484 lines were subsequently tested for yield and growth traits during three seasons (winter 2012-2013, 2013-2014 and 2014-2015) near Ciudad Obregon, Sonora, Mexico in three different testing environments. Those included ZT and CT with full irrigation and CT with reduced irrigation. The experiment was set-up as an alpha lattice design with three replications for each testing environment. Within each replication, genotypes were arranged randomly in three blocks of 160 and 170 genotypes. The dataset includes the following data: days to heading (DH), plant height (PHT), grain yield (GY) and two NDVI values (NDVI1 and NDVI2). Throughout the experiment, NDVI readings were recorded at regular intervals and growth curves were created based on the obtained data. For analysis, two values were selected, one measurement during early vegetative growth (NDVI1), around four weeks after planting, and the second at maximum growth (NDVI2)."  

	uri <- "hdl:11529/10548473"
	group <- "agronomy"


	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor = 0,
		project=NA,
		#data_citation="Verhulst, Nele; Ammar, Karim; Honsdorf, Nora; Govaerts, Bram; Crossa, Jose; Vargas, Mateo, 2020. Parallel selection of durum wheat in conventional and zero tillage. https://hdl.handle.net/11529/10548473, CIMMYT Research Data & Software Repository Network, V1, UNF:6:C+bZEedQtaH9ce54/PaaNA== [fileUNF]",
		publication=NA,
		data_organization = "CIMMYT",
		data_type="experiment", 
		carob_contributor="Fredy Chimire",
		carob_date="2024-03-24",
		treatment_vars="land_preparation;irrigation_fulfilment",
		response_vars="yield"
	)
  
	f <- ff[basename(ff) == "Database_902-Selection-Honsdorf_et_al.xlsx"]
	r <- readxl::read_excel(f, sheet = "Data")
  
	d <- data.frame(
		crop = "durum wheat", 
		yield_part = "grain", 
		yield = r$YLD*1000, 
		variety = as.character(r$Genotype),
		treatment = r$TEnv,
		trial_id = "1",
		planting_date = as.character(r$Year), 
		rep = as.integer(r$Rep),
		plant_height = as.numeric(r$PHT),
		irrigated = TRUE,
		country = "Mexico",
		adm1 = "Sonora",
		location = "Ciudad Obregon",
		site = "CIMMYT CENEB",
		latitude = 27.3716, 
		longitude = -109.9315,
		geo_from_source = FALSE
	)

  	d$land_preparation[d$treatment == "CT"] <- "conventional"
	d$land_preparation[d$treatment == "ZT"] <- "none"
	d$land_preparation[d$treatment == "RI"] <- "conventional"
	d$irrigation_fulfilment  = "full"
	d$irrigation_fulfilment[d$treatment == "RI"] <- "reduced"
  
  	d$treatment[d$treatment == "CT"] <- "conventional tillage"
	d$treatment[d$treatment == "ZT"] <- "zero tillage"
	d$treatment[d$treatment == "RI"] <- "reduced irrigation with conventional tillage"

  
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
  
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, d, path=path)
}

