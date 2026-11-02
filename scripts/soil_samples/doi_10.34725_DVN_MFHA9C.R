# R script for "carob"
 
## Some latitude values from countries such as Greenland fall outside the bounds defined in terminag
# Samoa is a country in the Ocean that why it's showing that the points are in the ocean


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
		carob_modified_by = "Cedric Ngakou",
		carob_last_modified = "2026-10-28",
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
		trial_id = paste0(r$ISO, r$ID, "-",r$HORI, "-", r$SAMPLENO),
		soil_pH = as.numeric(r$PHH2O),
		soil_pH_KCl = as.numeric(r$PHKCL),
		soil_pH_CaCl2 = as.numeric(r$PHCACL2),
		soil_SOC = as.numeric(r$ORGC),
		soil_Ca = as.numeric(r$CA),
		soil_K = as.numeric(r$K),
		soil_Mg = as.numeric(r$MG),
		soil_Na = as.numeric(r$NA.),
		soil_EC = as.numeric(r$EC),
		soil_Al_exch = as.numeric(r$EXAL),
		soil_ex_acidity = as.numeric(r$EXACID),
		soil_CEC = as.numeric(r$CECSOIL),
		soil_clay = as.numeric(r$CLAY),
		soil_silt = as.numeric(r$TSI),
		soil_sand = as.numeric(r$TSA),
		country = r$COUNTRY,
		location = r$LOC,
		longitude = r$Longitude,
		latitude = r$Latitude,
		geo_from_source = TRUE
	)

	d$country <- carobiner::replace_values( d$country,
		c("Cote d'Ivoire", "Russian Federation", "Syrian Arab Republic", "Slovakia (Slovak Republic)", "Congo, the Democratic Republic of"),
		c("Côte d'Ivoire", "Russia", "Syria", "Slovakia", "Democratic Republic of the Congo")
	)

	d[d==""] <- NA
	### Fixing geo coordinate errors 
	i <- grepl("Harsum", d$location) & grepl("Germany", d$country)
	d$longitude[i] <- 9.9665
	
	i <- grepl("Orestiás", d$location) & grepl("Greece", d$country)
	d$longitude[i] <- 26.5267
	d$latitude[i] <- 41.50998
	d$geo_from_source[i] <- FALSE 
	d$geo_source[i] <- "Google Maps"
	
	i <- grepl("Drama", d$location) & grepl("Greece", d$country)
	d$longitude[i] <- 24.1467
	d$latitude[i] <- 41.151
	d$geo_from_source[i] <- FALSE 
	d$geo_source[i] <- "Google Maps"
	
	i <- grepl("Stefan Voda village", d$location) & grepl("Romania", d$country)
	d$longitude[i] <- 27.0778
	d$latitude[i] <- 44.3438
	d$geo_from_source[i] <- FALSE 
	d$geo_source[i] <- "Google Maps"
	
	i <- grepl("Svartberget research catchment", d$location) & grepl("Sweden", d$country)
	d$longitude[i] <- 19.766
	d$latitude[i] <- 64.2444
	d$geo_from_source[i] <- FALSE 
	d$geo_source[i] <- "Google Maps"
	
	i <- grepl("Cerrillos",d$location) & grepl("Uruguay", d$country)
	d$longitude[i] <- -56.361
	d$latitude[i] <- -34.607
	d$geo_from_source[i] <- FALSE 
	d$geo_source[i] <- "Google Maps"
	
	i <- grepl("Florida city", d$location) & grepl("Uruguay", d$country)
	d$longitude[i] <-  -56.2177
	d$latitude[i] <- -34.0938
	d$geo_from_source[i] <- FALSE 
	d$geo_source[i] <- "Google Maps"
	
	i <- grepl("San Gabriel", d$location) & grepl("Uruguay", d$country)
	d$longitude[i] <- -55.885
	d$latitude[i] <- -34.03689
	d$geo_from_source[i] <- FALSE 
	d$geo_source[i] <- "Google Maps"
	
	i <- grepl("Santa Rosa village", d$location) & grepl("Uruguay", d$country)
	d$longitude[i] <- -56.0387
	d$latitude[i] <- -34.4988
	d$geo_from_source[i] <- FALSE 
	d$geo_source[i] <- "Google Maps"
	
	i <- grepl("New South Wales", d$location) & grepl("Australia", d$country)
	d$longitude[i] <- 145.6028
	d$latitude[i] <- -32.3424
	d$geo_from_source[i] <- FALSE 
	d$geo_source[i] <- "Google Maps"
	
	i <- grepl("Paraiba, Jacuma", d$location) & grepl("Brazil", d$country)
	d$longitude[i] <- -34.8132
	d$latitude[i] <- -7.2612
	d$geo_from_source[i] <- FALSE 
	d$geo_source[i] <- "Google Maps"
	
	i <- grepl("Sumatra", d$location) & grepl("Indonesia", d$country)
	
	d$longitude[i] <- 100.253
	d$latitude[i] <- 0.9577
	d$geo_from_source[i] <- FALSE 
	d$geo_source[i] <- "Google Maps"
	
	i <- grepl("Moluccas", d$location) & grepl("Indonesia", d$country)
	d$longitude[i] <- 128.1068
	d$latitude[i] <- -3.6245
	d$geo_from_source[i] <- FALSE 
	d$geo_source[i] <- "Google Maps"
	
	i <- grepl("Kandy District", d$location) & grepl("Sri Lanka", d$country)
	d$longitude[i] <- 80.634
	d$latitude[i] <- 7.2902
	d$geo_from_source[i] <- FALSE 
	d$geo_source[i] <- "Google Maps"
	
	i <- grepl("Sematan", d$location) & grepl("Malaysia", d$country)
	d$longitude[i] <- 109.76899
	d$latitude[i] <- 1.8092
	d$geo_from_source[i] <- FALSE 
	
	d$geo_source[i] <- "Google Maps"
	
	i <- grepl("Kula Siru-Kuala Pueh", d$location) & grepl("Malaysia", d$country)
	d$longitude[i] <- 102.9430
	d$latitude[i] <- 5.0759
	d$geo_from_source[i] <- FALSE 
	d$geo_source[i] <- "Google Maps"
	
	i <- grepl("Chacocente", d$location) & grepl("Nicaragua", d$country)
	d$longitude[i] <- -86.1943
	d$latitude[i] <- 11.5423
	d$geo_from_source[i] <- FALSE 
	d$geo_source[i] <- "Google Maps"
	
	i <- grepl("Crucero-Masachapa", d$location) & grepl("Nicaragua", d$country)
	d$longitude[i] <- -86.516
	d$latitude[i] <- 11.7834
	d$geo_from_source[i] <- FALSE 
	d$geo_source[i] <- "Google Maps"
	
	i <- grepl("Sorsogon", d$location) & grepl("Philippines", d$country)
	d$longitude[i] <- 123.9651
	d$latitude[i] <- 12.9806
	d$geo_from_source[i] <- FALSE 
	d$geo_source[i] <- "Google Maps"
	
	i <- grepl("Chanthaburi Province", d$location) & grepl("Thailand", d$country)
	d$longitude[i] <- 102.1128
	d$latitude[i] <- 12.7887
	d$geo_from_source[i] <- FALSE 
	d$geo_source[i] <- "Google Maps"
	
	i <- grepl("polima Island", d$location) & grepl("Samoa", d$country)
	d$longitude[i] <- -172.1513
	d$latitude[i] <- -13.82305
	d$geo_from_source[i] <- FALSE 
	d$geo_source[i] <- "Google Maps"
	
	i <- grepl("Idoha State", d$location) & grepl("United States", d$country)
	d$longitude[i] <- -114.245
	d$latitude[i] <- 43.5056
	d$geo_from_source[i] <- FALSE 
	d$geo_source[i] <- "Google Maps"
	
	i <- grepl("Coastal Savanna Zone", d$location) & grepl("Ghana", d$country)
	d$longitude[i] <- -1.77
	d$latitude[i] <- 4.88
	d$geo_from_source[i] <- FALSE 
	d$geo_source[i] <- "Google Maps"
	
	i <- grepl("Southeast coast", d$location) & grepl("Greenland", d$country)
	d$longitude[i] <- -37.6394
	d$latitude[i] <- 65.6143
	d$geo_from_source[i] <- FALSE 
	d$geo_source[i] <- "Google Maps"
	
	i <- grepl("Koumani", d$location) & grepl("Greece", d$country)
	d$longitude[i] <- 21.7501
	d$latitude[i] <- 37.7906
	d$geo_from_source[i] <- FALSE 
	d$geo_source[i] <- "Google Maps"
	
	i <- grepl("Niedersachsen", d$location) & grepl("Germany", d$country)
	d$longitude[i] <- 8.6212
	d$latitude[i] <- 53.7694
	d$geo_from_source[i] <- FALSE 
	d$geo_source[i] <- "Google Maps"
	

	### drop rows with missing geocoordinates and location
	d <- d[!(is.na(d$longitude) & is.na(d$latitude) & is.na(d$location)), ]
  
  coords <- data.frame( 
     location = c("W of Sinaia town", "Rio de Janeiro, RJ, Itumirim road no. 654, 50m on the right side and 300m from the Magerca Road", "SE 6-20-15-W4, Alberta", "Schleswig-Holstein, Kiel, Brueggerholz near Boksee", "Schleswig-Holstein, Fitzbek near Neumuenster", "Eifel, Stromberg", "Niedersachsen, 2 km SW of Beierstedt; 10 km SW of Schöningen", "Dröper, 10km SSE of Osnabrück and E of Georgsmarienhütte", "Departement Vosges, 16km SSE of St. Die, Col du Luschpach", "Departement Vosges, 6km W of St. Die, Fôret de Montagne", "Departement Vosges, 10km N of Epinal, 2 km W of Domèvre-sur-Durbion", "Departement Meurthe-et-Moselle, 5km SW of Nancy, Forêt de Haye", "Department Seine-et-Oise, 10km W of Versailles, Grignon", "Departement Seine-et-Oise, 4km W of Rambouillet, Fôret des Yvelines", "Departement Aisne, 14km W of Laon, Forêt Dom. de St. Gobain", "Carrière de St. Marcel les Valence, NE of Valence", "Domaine de Restinclières, near Prades-le-Lez, about 10km N of Montpellier, l'Herault", "Haws Hill, Hadstock, Essex", "Chapelhouse Farm, Poulton, Cheshire", "Roadside cutting in Gwydyr Forest, 1km N of Ty-hyllt", "Beside Aled Isaf reservoir", "Beside Llyn Alad Reservoir", "Cliptone Forest, Notts", "The Buck Gates, Thoresby Park, Notts", "North Leigh, Oxon", "Winsor Hill above Princes Risborough, Bucks", "Guria, Caucasus, Ozurguet (Krasnodar)", "Coastal Savanna Zone", "Central Agricultural Research Station, Kwadaso - Kumasi", "Blomsterdalen near Angmassalik, Southeast coast", "Near Angmassalik, Southeast coast", "Ilias Prefecture, 50km NE of Pyrgos, 2km S of Koumani, Foloy Forest", "Kiambu District, KSS excursion site no. 1", "Nyandarua District, KSS excursion site no. 2", "near Suswa, 5km part satellite station", "Muranga District, Kimakia forest, KSS excursion site no. 3", "Kilifi District", "Kwale District", "Kisii District", "7km S of Cheborge, on Sotik-Roret Road", "Kisii District,35km SSW of Kisii,2km E of Magombo market,near Kenyenya", "Kericho District, near Gorgor", "southern route to Ganze, 1.5km after junction to Sokoke", "Kilifi District, road Kaioleni-Mariakani, 2km after Kombeni drift", "13km SE from Koumantou, in the direction of the village of Sele", "6km E of N'Tentou on the ancient road Bougouni-Sélingué", "Gaza, Limpopo-valley", "Prov. of Limburg, near Elkenrade", "Wadi Quriyat, 1.4km N of Ministry farm, on Lajrid Plain", "Huwayyah S, 1.5km S of graded Buraimi-Mahdah road, 10km NE of Buraimi", "10km N of Olanesti, NV Rîmnicu Vîlcea", "3km W of Cîmpina town", "W of Rîmnicu Sarat, Putreda village", "East Bucharest, Fundulea village", "30km N of Bucharest, Snagov village", "NV Brasov, Vladeni village", "NE Cimpulung, Podul Dimbovitei village", "California State, Yolo County", "Indiana State, Tippecanoe County, 6km E of Romney, in the Purdue-Cunningham Research Forest", "Gisenyi, Kibilira, Ntobwe, Cellule Rusebeya, road cut", "South Dakota State, Brookings County", "Montana State, Teton County, about 16km N of Conrad and 1.6km E of Highway 15, in Western Triangle Research Station", "Iowa State, Decatur County, 383,4m S and 73,2 m W of NE corner Sec. 17T.70N., R.25W", "Vindelälven, Vindeln, Västerbotten", "Kloten", "Asa", "North Carolina State, Catawba County, about 8,5 km SE of Newton", "North Carolina State, Robeson County, 400m SE of Parkton", "Massachusetts State, Hampshire County, City of Northamptom", "Minnesota State, Itasca County, 4.8km S and 6.4km W of Grand Rapids", "Texas State, Falls County", "Texas State, Hidalgo County", "Iowa State, Hamilton County", "Idaho State, Elmore County, Payette Canyon, NW quarter of NE quarter of Section 36, T.4S., R.9E.", "Illinois State, Jasper County, NE 160 NW 40 NW 10 SE 2 1/2, 4.3km N and 12.2km E SW corner plot 309 Sec. 3, T.6N., R.9W.", "Mantecal, Experimental Station \"Ezequil Zamora\"", "Upolu island, near Sale'a'aumua", "Rehoboth"), 
     long = c(25.53, -43.6013, -111.70, 10.08466, 9.85, 6.50, 10.87, 8.05, 7.05, 6.87, 6.42, 6.00, 1.93, 1.78, 3.37, 4.9382, 3.86, 0.27846, -2.99, -3.82, -3.65, -4.52, -1.12, -1.04, -1.37, -0.84, 42.00, 0.10, -1.65, -37.00, -37.00, -21.80, 36.5847, 36.6055, 36.35, 37.30, 39.85, 39.45, 34.7667, 35.00, 34.48, 35.20, 39.72, 39.45, -6.8333, -7.10, 32.20, 5.90, 58.90, 56.2336, 24.23, 25.73, 27.00, 26.52, 26.17, 25.37, 25.20, -121.90162, -86.90, 29.60, -96.79, -111.95, -93.95, 19.70, 15.30, 14.70, -81.15, -79.00, -72.64, -93.60, -96.95, -98.18, -93.70, -116.00, -88.15, -69.15, -171.45, 17.0910), 
     lat = c(45.35, -22.9526, 53.10, 54.19435, 54.00, 50.10, 52.07, 52.23, 48.16, 48.29, 48.26, 48.65, 48.85, 48.63, 49.60, 44.9730, 43.73, 52.07786, 53.83, 53.10, 53.15, 53.35, 53.17, 53.22, 51.82, 51.73, 41.92, 5.70, 6.71, 65.61, 65.60, 37.70, -0.9017, -0.4765, -1.15, -0.95, -3.63, -4.18, -0.6817, -0.70, -0.85, -0.35, -3.50, -3.70, 11.3333, 11.30, -24.70, 50.83, 23.25, 24.1929, 45.20, 45.13, 45.40, 44.45, 44.71, 45.77, 45.42, 38.68665, 40.50, -1.97, 44.37, 48.17, 40.74, 64.19, 59.90, 57.15, 35.60, 34.90, 42.33, 47.20, 31.25, 26.40, 42.38, 44.20, 39.01, 7.57, -13.95, -23.3190),
     geo_s = "Google Maps",
     geo_from = FALSE)
  
  # Join coordinates 
  d <- merge(d, coords, by = "location", all.x = TRUE, sort = FALSE)
  i <- is.na(d$longitude)
  d$longitude[i] <- d$long[i]
  d$latitude[i] <- d$lat[i]
  d$geo_source[i] <- d$geo_s[i]
  d$geo_from_source[i] <- d$geo_from[i]
  d$lat <- d$long <- d$geo_s <- d$geo_from <- NULL
	
  d$is_survey <- TRUE
  d$on_farm <- FALSE
  
  ### drop duplicate rows (6 rows)
  d <- unique(d)
  
  carobiner::write_files(path, meta, d)
}



