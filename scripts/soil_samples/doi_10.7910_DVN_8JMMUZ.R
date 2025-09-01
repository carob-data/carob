# R script for "carob"


carob_script <- function(path) {
  
"The aim of this project component was to conduct a landscape assessment of soil and land health in near Hallu, Tanzania for the CIAT-Soils activity for the CGIAR Research Program on Climate Change, Agriculture and Food Security (CCAFS). The selection of the LDSF site near Hallu was informed by Selian Agriculture Research Institute (SARI) scientists and chosen to compliment existing CIAT AfricaRISING LDSF sites (near Long and Matufa). This activity was part of a CIAT-SARI activity to access eco-efficiency of farming systems for CCAFS. The Land Degradation Surveillance Framework (LDSF) was employed to conduct a systematic biophysical baseline of key land and soil health metrics. The LDSF is designed to provide a biophysical baseline at landscape level, and a monitoring and evaluation framework for assessing processes of land degradation and the effectiveness of rehabilitation measures (recovery) over time. (2015-11-17)"
  
	uri <- "doi:10.7910/DVN/8JMMUZ"
	group <- "soil_samples"

	ff <- carobiner::get_data(uri, path, group)
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		project=NA,
		publication= NA,
		data_organization = "CIAT",
		data_type = "survey", 
		response_vars = "none",
		treatment_vars = "none",
		carob_contributor="Andrew Sila", 
		carob_date="2023-09-28"
	)
  
  # No need to read the table with MIR data
#RH: why not?
##	 fm <- ff[basename(ff) == "5 Hallu_MIR.csv"]
## Also, what can we extract from LDSF?
## 7 Hallu_LDSF_data_raw.csv"


	f1 <- ff[basename(ff) == "2 Hallu_Crop_Nutrition_Results.xls"]
	r1 <- data.frame(carobiner::read.excel(f1))
	r1 <- r1[-c(1:5),]
	colnames(r1) <- unlist(r1[1,])
	r1$S[r1$S == "< 0.50"] <- 0.1
	
	f2 <- ff[basename(ff) == "3 Hallu_Carbon_and_Nitrogen.csv"]
	r2 <- data.frame(read.csv(f2))
	r2$CIAT_Kenya_Laboratory.ID <- gsub(" ", "", r2$CIAT_Kenya_Laboratory.ID)
	
	f3 <- ff[basename(ff) == "7 Hallu_LDSF_data_raw.csv"]
	r3 <- data.frame(read.csv(f3))
	r3 <- r3[,c('Country', 'Site', 'Cluster', 'Plot', 'Latitude', 'Longitude')]
	r3 <- aggregate(r3[, c("Latitude", "Longitude")], r3[, c("Country", "Site", "Cluster")], mean)
	
	r <- merge(r2, r1, by.x = 'CIAT_Kenya_Laboratory.ID', by.y = 'Field')
	r <- merge(r, r3, by=c("Country", "Site", "Cluster"), all.x=TRUE)
	
	
	d <- data.frame(
		location = r$Site,
		longitude = r$Longitude,
		latitude = r$Latitude,
		geo_from_source= TRUE,
		country = r$Country,
		#trial_id = "1",
		soil_pH = as.numeric(r$pH),
		soil_EC = as.numeric(r$`EC(S)`)/1000, #uS/cm to mS/cm
		soil_Al = as.numeric(r$Al),
		soil_B = as.numeric(r$B),
		soil_Ca = as.numeric(r$Ca),
		soil_Cu = as.numeric(r$Cu),
		soil_Fe = as.numeric(r$Fe),
		soil_K = as.numeric(r$K),
		soil_Mg = as.numeric(r$Mg),
		soil_Mn = as.numeric(r$Mn),
		soil_Na = as.numeric(r$Mn),
		soil_P_total = as.numeric(r$P),
		soil_S = as.numeric(r$S),
		soil_Zn = as.numeric(r$Zn),
		soil_ex_acidity = as.numeric(r$Hp),
		soil_PSI = as.numeric(r$PSI),
		soil_C = as.numeric(r$Total.C),
		soil_N = as.numeric(r$Tota.N)
	)
	
	i <- r["Soil Depth"] == "Sub Soil"
	r$depth_top <- ifelse(i, 20, 0)
	r$depth_bottom  <- ifelse(i, 50, 20)
	
	carobiner::write_files(path, meta, d)
}


