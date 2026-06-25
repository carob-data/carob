# R script for "carob"


carob_script <- function(path) {
  
"The ICRAF-ISRIC Soil VNIR Spectral Library contains visible near infrared spectra of 785 soil profiles (4,438 samples) soils selected from the Soil Information System of the International Soil Reference and Information Centre (ISRIC). The samples consist of all physically archived samples at ISRIC in 2004 for which soil attribute data was available. The spectra were measured at the World Agroforestry Center (ICRAF) Soil and Plant Spectral Diagnostic Laboratory. The samples are from 58 countries spanning Africa, Asia, Europe, North America, and South America. Associated attribute data, such as geographical coordinates, horizon (depth), and physical and chemical properties, are provided as separate tables. Using different methods for linking related files as explained in the pdf document accompanying this dataset (0ICRAF-ISRICSoilVNIRSpectralLibrary.pdf), the visNIR spectra can be linked with all tables holding both physical and chemical properties."
  
	uri <- "doi:10.34725/DVN/MFHA9C"
	group <- "soil_samples"
	ff  <- carobiner::get_data(uri, path, group)
  
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRAF;ISRIC",
		publication= "doi:10.1016/j.earscirev.2016.01.012.",
		project="FAO",
		data_type= "survey",
		response_vars = "none",
		treatment_vars = "none",
		carob_contributor= "Andrew Sila",
		carob_effort = NA,
		carob_date="2024-05-20"
	)
  
	f1 <- ff[basename(ff) == "Chemical_properties.csv"]
	r1 <- read.csv(f1)
	f2 <- ff[basename(ff) == "Physical_properties.csv"]
	r2 <- read.csv(f2)
	f3 <- ff[basename(ff) == "Country.csv"]
	r3 <- read.csv(f3)
	f4 <- ff[basename(ff) == "Region.csv"]
	r4 <- read.csv(f4)
	f5 <- ff[basename(ff) == "Classification.csv"]
	r5 <- read.csv(f5)
	f6 <- ff[basename(ff) == "Site_description.csv"]
	r6 <- read.csv(f6)

	# Get lat and lon from r6 and convert into degrees
	r6$LONM[is.na(as.numeric(r6$LONM))] <- 0
	r6$LONS[is.na(as.numeric(r6$LONS))] <- 0
	r6$Longitude <-  r6$LOND +  as.numeric(r6$LONM)/60 +  as.numeric(r6$LONS)/3600
	r6$Longitude[r6$LONEW == "W"] <- -r6$Longitude[r6$LONEW == "W"]
	
	r6$LATM[is.na(as.numeric(r6$LATM))] <- 0
	r6$LATS[is.na(as.numeric(r6$LATS))] <- 0
	r6$Latitude <- r6$LATD + as.numeric(r6$LATM)/60 +  as.numeric(r6$LATS)/3600
	r6$Latitude[r6$LATNS == "S"] <- -r6$Latitude[r6$LATNS == "S"]
	r6$LOC <- iconv(r6$LOC, "latin1", "UTF8")
	

	# Merge country and region
	r34 <- merge(r3, r4, by="REGION")
	r56 <- merge(r5, r6, by=c("ISO", "ID"))
	#Merge country and site description
	r3456 <- merge(r34, r56, by="ISO")
	
## need to investigate the non-matches (see all=FALSE)
## perhaps typos can be fixed?
	r12 <- merge(r1, r2, by=c("ISO", "ID", "HORI", "BTOP", "BBOT", "SAMPLENO"), all=TRUE)

## the number of records of r is 5318, but r12 has 5248 records. Is that correct?
## Please investigate
	r <- merge(r12, r3456, by = c("ISO", "ID"), all=TRUE)
		
	d <- data.frame(
		trial_id = paste0(r$ISO, r$ID),
		soil_pH = as.numeric(r$PHH2O),
		soil_pH_KCl = as.numeric(r$PHKCL),
		soil_pH_CaCl2 = as.numeric(r$PHCACL2),
		soil_SOC = as.numeric(r$ORGC),
		soil_Ca = as.numeric(r$CA),
		soil_K = as.numeric(r$K),
		soil_Mg = as.numeric(r$MG),
		soil_Na = as.numeric(r$NA.),
		soil_EC = as.numeric(r$EC),
		soil_Ex_Al = as.numeric(r$EXAL),
		soil_Ex_acidity = as.numeric(r$EXACID),
		soil_CEC = as.numeric(r$CECSOIL),
		soil_clay = as.numeric(r$CLAY),
		soil_silt = as.numeric(r$TSI),
		soil_sand = as.numeric(r$TSA),
		country = r$COUNTRY,
		location = r$LOC,
		longitude = r$Longitude,
		latitude = r$Latitude
	)

	d$country <- carobiner::replace_values( d$country,
		c("Cote d'Ivoire", "Russian Federation", "Syrian Arab Republic", "Slovakia (Slovak Republic)", "Congo, the Democratic Republic of"),
		c("Côte d'Ivoire", "Russia", "Syria", "Slovakia", "Congo")
	)

	# There are about 608 records missing latlon values out of the total 4830
	#i <- which(is.na(d$longitude) | is.na(d$latitude))
	#nolonlat <- d[, c("country", "location")]
	# locs <-  NULL
	# loc <- strsplit(latlon_missing_l, ",", fixed = TRUE)
	# for (l in 1:length(loc)){
	#   locs <- c(locs,loc[[l]][1])
	# }
	#carobiner::geocode(latlon_miss[1,1], latlon_miss[1,2])

    carobiner::write_files(path, meta, d)
}
