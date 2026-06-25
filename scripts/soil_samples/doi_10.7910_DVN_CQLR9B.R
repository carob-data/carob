# R script for "carob"


carob_script <- function(path) {
  
"This was Task 1.2. “Soil survey to characterize two sentinel sites” as part of Work Package 1 “Identification of the key biophysical production constraints to crops and livestock at farm and landscape levels” The aim of this task was to conduct an assessment of soil and land health in the Babati district in Tanzania covering the action villages of the USAID- AfricaRISING project. The Land Degradation Surveillance Framework (LDSF) was employed to conduct a systematic biophysical baseline of key land and soil health metrics, which was to be combined with agronomic survey data collected as part of the wider project. The LDSF is designed to provide a biophysical baseline at landscape level, and a monitoring and evaluation framework for assessing processes of land degradation and the effectiveness of rehabilitation measures (recovery) over time. Babati Agricultural District Office and Tanzania National Agricultural Research System as well as Sokoine University of Agriculture (SUA) staff were involved in two one-week trainings in the LDSF methodology at the Long and Matufa LDSF sites. Those trained include: Majid Suleiman (District Agricultural Officer), Edgar Wakurwa (District Agronomist), Mbwambo (Extension Officer), Prosper Massawe from Selian Agricultural Research Institute and Boniface Massawe, from SUA. George Sayula of SARI also helped coordinate the field efforts. Local farmers were also involved in the surveys and farmer knowledge on management practices and land use history was incorporated into the LDSF database. (2015-09-10)"
  
	uri <- "doi:10.7910/DVN/CQLR9B"
	group <- "soil_samples"
	ff  <- carobiner::get_data(uri, path, group)
  
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIAT",
		publication= "doi:10.1007/s1070",
		project="Africa Rising",
		data_type= "survey",
		response_vars = "none",
		treatment_vars = "none",
		carob_contributor= "Andrew Sila",
		carob_effort = NA,
		carob_date="2024-05-16"
	)
  
	f1 <- ff[basename(ff) == "02. Mafuta_Crop Nutrition Results for dataverse.xlsx"]
	r1 <- carobiner::read.excel(f1, TRUE)
	f2 <- ff[basename(ff) == "03. Long Crop Nutrition Results for dataverse.xlsx"]
	r2 <- carobiner::read.excel.hdr(f2, 10, hdr=2, fix_names=TRUE)

	f3 <- ff[basename(ff) == "04. Matufa LDPSA and CN for dataverse.xlsx"]
	r3 <- carobiner::read.excel(f3, TRUE)
	f4 <- ff[basename(ff) == "05. Long LDPSA and CN for dataverse.xlsx"]
	r4 <- carobiner::read.excel(f4, TRUE)
	f5 <- ff[basename(ff) == "07. Matufa LDSF data raw for dataverse.csv"]
	r5 <- read.csv(f5)
	f6 <- ff[basename(ff) == "08. Long_LDSF_data raw for dataverse .csv"]
	r6 <- read.csv(f6)
	

	names(r2)[grep("CIAT.Lab.ID1", names(r2))] <- "CIAT.lab.ID"
	names(r2) <- gsub("_", ".", names(r2))
	names(r2) <- gsub("\\.\\.", ".", names(r2))
	cnls <- carobiner::bindr(r1, r2)

	#rbind r3 and r4 to get CN and PSA data
	names(r4)[names(r4) == "CIAT Lab ID1"] <- "CIAT lab ID"
	cnpsa <- carobiner::bindr(r3, r4)
	cnpsa <- cnpsa[, c("Selian_SSN", "ldsfID", "Clay.pct", "Silt.pct", "Sand.pct", "Carbon.Content.pct", "Nitrogen.Content.pct")]
	
	r <- merge(cnls, cnpsa, by.x = "Selian.SSN", by.y = "Selian_SSN")
	r$Site[r$Site=="Mafuta"] <- "Matufa"

	# Get lat and lon from r5 and r6
	ldsf <- carobiner::bindr(r5, r6)[,c("Site", "Cluster", "Plot", "Latitude", "Longitude")]
	
	r <- merge(r, ldsf, by=c("Site", "Cluster", "Plot"))
	r$Na.ppm[r$Na.ppm == "< 0.60"] <- "0.3"

	d <- data.frame(
		#trial_id = "1",
		soil_pH = as.numeric(r$pH),
		soil_EC = as.numeric(r$EC.S.uS.cm)/1000,
		soil_Al = as.numeric(r$Al.ppm),
		soil_B = as.numeric(r$B.ppm),
		soil_Ca = as.numeric(r$Ca.ppm),
		soil_Cu = as.numeric(r$Cu.ppm),
		soil_Fe = as.numeric(r$Fe.ppm),
		soil_K = as.numeric(r$K.ppm),
		soil_Mg = as.numeric(r$Mg.ppm),
		soil_Mn = as.numeric(r$Mn.ppm),
		soil_Na = as.numeric(r$Na.ppm),
		soil_P_total = as.numeric(r$P.ppm),
		soil_S = as.numeric(r$S.ppm),
		soil_Zn = as.numeric(r$Zn.ppm),
		soil_ex_acidity = as.numeric(r$Hp),
		soil_PSI = as.numeric(r$PSI),
		soil_clay = as.numeric(r$Clay.pct),
		soil_silt = as.numeric(r$Silt.pct),
		soil_sand = as.numeric(r$Sand.pct),
		soil_C = r$Carbon.Content.pct,
		soil_N = r$Nitrogen.Content.pct,
		country = r$Country,
		location = r$Site,
		longitude = r$Longitude,
		latitude = r$Latitude,
		geo_from_source= TRUE
	)

    carobiner::write_files(path, meta, d)
}
