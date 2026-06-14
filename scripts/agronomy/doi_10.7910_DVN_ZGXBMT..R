# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1.NAs in NPK fertilizer columns arise from the merge for crop data that has no fertilizer data 
#2.need to add soil_loam in carob
carob_script <- function(path) {

"
Impact of NPK and micronutrients fertilization on beans in Nicaragua

This dataset contains information of experiments carried out beans in two regions of Nicaragua (Northern of the Central Region and Southern of the Pacific Region), as well as a compilation of soils data from different regions in Nicaragua collected from 2015 to 2019. The experiments were designed to explore the effects of macronutrients (N, P, K) and micronutrients (Zn + B + Mg) in the yield of beans. The experiments were carried out in the farmer's field during the 2019 production cycle (Apante sowing), the dataset contains yield and aerial biomass of the experiments.
"


	uri <- "doi:10.7910/DVN/ZGXBMT"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=4,
		data_organization = "CIAT",
		publication = NA,
		project = NA,
		carob_date = "2026-06-13",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer",
		response_vars = "yield", 
		carob_contributor = "Blessing Dzuda",
		completion = 100,	
		notes = NA)
	

	f1 <- ff[basename(ff) == "01. Codebook.xls"]
	f2 <- ff[basename(ff) == "02. Soils Data.xlsx"]
	f3 <- ff[basename(ff) == "03. Bean Data - NPK.xlsx"]
	f4 <- ff[basename(ff) == "04. Bean Data - Micronutrients.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	r4 <- carobiner::read.excel(f4)

	soil <- data.frame(
	  adm1=r2$departamento,
	  adm2=r2$municipio,
	  location=r2$localidad,
	  id=tolower(r2$id_banco),
	  soil_pH=r2$pH,
	  soil_SOM=r2$MO,
	  N_fertilizer=r2$N,
	  P_fertilizer=r2$P,
	  K_fertilizer=r2$K,
	  Ca_fertilizer=r2$Ca,
	  Mg_fertilizer=r2$Mg,
	  Fe_fertilizer=r2$Fe,
	  Cu_fertilizer=r2$Cu,
	  Zn_fertilizer=r2$Zn,
	  Mn_fertilizer=r2$Mn,
	  soil_ex_acidity=r2$Ac_in,
	  soil_bd=r2$Da,
	  soil_clay=r2$Arcilla,
	  #soil_loam=r2$Limo,
	  soil_sand=r2$Arena,
	  soil_texture=r2$Textura)
	
	
	r4$npta <- NULL#removing columns before binding(variables not in carob)
	r4$vpta <- NULL
	r5 <- rbind(r3,r4)
	
	data <- data.frame(
	   country="Nicaragua",
	   adm1=r5$departamento,
	  adm2=r5$municipio,
	  location=r5$localidad,
	  id=tolower(r5$id_banco),
	  rep=as.integer(r5$rep),
	  crop="common bean",
	  yield=r5$rto_grano_kgha,
	  dmy_total=r5$rto_biom_kgha#assuming its dry because common bean is harvested when the plant/pods are dry
	)
	
	cols <- c("adm1", "adm2", "location", "id")
	
	data[cols] <- lapply(data[cols], tolower)
	soil[cols] <- lapply(soil[cols], tolower)
	soil$adm2[soil$adm2 == "posotega"]     <- "posoltega"
	
	d <- merge(data, soil, by = cols, all.x = TRUE)#some rows in soil were discarded after the merge because
	# we only want data that corresponds to sites where the trial was done
	d[cols] <- lapply(d[cols], tools::toTitleCase)
	
  	
  d$planting_date <- "2019"
  d$trial_id <- paste(d$planting_date,d$location,sep = "_")
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$geo_from_source <- FALSE
	
	loc <- data.frame(
	  location = c("El Charcon", "Limones", "Linda Vista", "El Tablon", "El Triunfo", "Rio Mena"),
	  latitude  = c(12.128, 12.160, 12.154, 12.242, 11.792, 11.147),
	  longitude = c(-86.197, -86.183, -86.303, -86.604, -84.609, -85.423)
	)
	
	d <- merge(d,loc,by="location", all.x = T)
	 d$soil_texture[d$soil_texture=="Franco Arcilloso"] <- "clay loam"
	 d$soil_texture[d$soil_texture=="Arcilloso"] <- "clay"
	 
   d$inoculated <- FALSE
	d$yield_part <- "seed"
	d$yield_moisture <- as.numeric(NA)
  d$yield_isfresh <- TRUE
  d$id=NULL
  
	carobiner::write_files(path, meta, d)
}
