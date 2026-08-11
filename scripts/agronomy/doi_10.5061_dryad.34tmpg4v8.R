# R script for "carob"
# license: GPL (>=3)

## ISSUES



carob_script <- function(path) {

"
Data from: Soybean yield is positively linked to organic matter, but planting date remains more influential

Establishing connections between soil health indicators and crop performance will help ensure that tests recommended to farmers relate to outcomes of interest. This study assessed the relationship of soybean [Glycine max (L.) Merr] yield with three common soil health indicators: soil organic matter (SOM), permanganate oxidizable carbon (POXC), and autoclaved citrate extractable nitrogen (ACE‐N). These tests were assessed alongside other factors (soil test phosphorus, soil test potassium [STK], mapped clay, planting date, summer precipitation, and location). Soil samples were collected from 457 producer‐managed fields between 2019 and 2021 in Arkansas, Michigan, North Carolina, and Wisconsin. Planting date and yield were reported by producers, while mapped clay and rainfall were determined using publicly available data. Simple linear regression was used to assess the relationship between soil health indicators and yield: the natural log of SOM and POXC were positively associated with soybean yield (R2 = 0.07, p &lt; 0.001; R2 = 0.03, p &lt; 0.001), while ACE‐N was not (p = 0.872). Multiple linear regression was used to further test the relationship of SOM and POXC with yield, while accounting for other factors that contribute to soybean yield. Models explained 27% of variation in yield, with significant factors including SOM or POXC, soybean planting date, STK, and mapped clay. Based on standardized coefficients, planting date was the most influential factor associated with yield. Broadly, our results indicate that improvements in yield are linked to higher SOM, but management decisions like planting early are critical for achieving high yields.
"

	uri <- "doi:10.5061/dryad.34tmpg4v8"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=NA,
		data_organization = "NDSU; UWM; NCSU; MSU; UAKS",
		publication = "doi:10.1002/saj2.20779",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "planting_date;soil_SOM",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-07-22",
		carob_completion = 100,	
		carob_effort = 2
	)
	

	f1 <- ff[basename(ff) == "Dataset_forRepositor12_6_24.xlsx"]
	#f2 <- ff[basename(ff) == "README.md"]

	r1 <- carobiner::read.excel(f1, sheet="Data")
	#r2 <- carobiner::read.excel(f1, sheet="Headers")
	
	d1 <- data.frame(
		#year = r1$Year,
		adm1 = r1$State,
		yield = r1$`Soybean Yield`,
		planting_date = as.character(as.Date(paste0(r1$Year, "-01-01")) + r1$`Planting Date` - 1),
		prec = r1$`Summer Precip`,
		soil_pH = r1$pH,
		soil_P = r1$STP,
		soil_K = r1$STK,
		soil_clay = r1$`Percent Clay`,
		soil_SOM = r1$`OM-LOI`, # amount of organic matter in soil sample
		soil_N = r1$`ACE-N`,
		soil_POXC = r1$POXC, # Permanganate oxidizable carbon
		soil_P_method = "Bray-1",
		adm2 = gsub(" \\(IL\\)| \\(IA\\)", "",  r1$County),
		country = "United States",
		crop = "soybean", 
		is_survey = FALSE, 
		on_farm = TRUE, 
		trial_id = paste(r1$County, r1$Year, sep = "-"), 
		yield_moisture = NA_real_, 
		yield_part = "grain", 
		geo_from_source = FALSE, 
		irrigated = NA, 
		yield_isfresh = NA,
		harvest_date = NA_character_
	)
	
	adm <- c("WI" = "Wisconsin", "AR"= "Arkansas", "MI"= "Michigan", "NC" = "North Carolina")
	d1$adm1 <- adm[d1$adm1]
	d1$adm2 <- gsub("St. Clair", "Saint Clair", d1$adm2)
	d1$adm2 <- gsub("St. Croix", "Saint Croix", d1$adm2)
	d1$adm2 <- gsub("St. Francis", "Saint Francis", d1$adm2)
	
	geo <- data.frame(
	  adm1 = c(rep("Arkansas", 33), rep("Michigan", 20), rep("North Carolina", 25), rep("Wisconsin", 40)),
	  adm2 = c("Arkansas", "Boone","Butler", "Chicot", "Clark", "Clay", "Cleveland", "Columbia", "Craighead", "Crawford", "Cross", "Desha", "Drew", "Greene", "Independence", "Jefferson", "Lafayette", "Lawrence", "Lincoln", "Mississippi", "Monroe", "Phillips", "Poinsett", "Polk", "Pope", "Prairie", "Randolph", "Saint Francis", "Stone", "Union", "Washington", "Woodruff", "Yell", "Barry", "Bay", "Branch", "Cass", "Chippewa", "Clinton", "Crawford", "Huron", "Ingham", "Kalamazoo", "Kent", "Lapeer", "Livingston", "Midland", "Monroe", "Ottawa", "Saginaw", "Saint Clair", "Sanilac", "Shiawassee", "Alexander", "Anson", "Beaufort", "Bertie", "Clay", "Cleveland", "Davie", "Greene", "Hyde", "Iredell", "Johnston", "Lincoln", "Nash", "Onslow", "Perquimans", "Polk", "Randolph", "Richmond", "Robeson", "Sampson", "Surry", "Union", "Washington", "Wilson", "Yadkin", "Adams", "Barron", "Brown", "Buffalo", "Calumet", "Chippewa", "Clark", "Columbia", "Crawford", "Dane", "Dodge", "Door", "Dunn", "Fond du Lac", "Green", "Iowa", "Jefferson", "Kenosha", "Lafayette", "Lincoln", "Manitowoc", "Marathon", "Monroe", "Ozaukee", "Pierce", "Polk", "Portage", "Rock", "Saint Croix", "Sauk", "Shawano", "Sheboygan", "Trempealeau", "Walworth", "Washburn", "Washington", "Waupaca", "Waushara", "Boone", "Jo Daviess"),
	  longitude = c(-91.3742, -93.0927, -86.6801, -91.2937, -93.1764, -90.4174, -92.1859, -93.2274, -90.6316, -94.2437, -90.7707, -91.2537, -91.7195, -90.5583, -91.5695, -91.9317, -93.6061, -91.1099, -91.7341, -90.0527, -91.2037, -90.8488, -90.6616, -94.2296, -93.0341, -91.5519, -91.0273, -90.7476, -92.1572, -92.5971, -94.2146, -91.2424, -93.4116, -85.3091, -83.9932, -85.0603, -85.9938, -84.5924, -84.6032, -84.6115, -83.0219, -84.3751, -85.5310, -85.5505, -83.2213, -83.9131, -84.3893, -83.5410, -85.9968, -84.0544, -82.6831, -82.8206, -84.1479, -81.1775, -80.1031, -76.8636, -76.9794, -83.7498, -81.5551, -80.5467, -77.6753, -76.2492, -80.8733, -78.3666, -81.2252, -77.9899, -77.4342, -76.4420, -82.1709, -79.8060, -79.7497, -79.1039, -78.3731, -80.6900, -80.5310, -76.5772, -77.9199, -80.6676, -89.7698, -91.8482, -88.0041, -91.7555, -88.2179, -91.2801, -90.6122, -89.3344, -90.9325, -89.4175, -88.7072, -87.3198, -91.8965, -88.4883, -89.6017, -90.1359, -88.7761, -88.0407, -90.1326, -89.7351, -87.8098, -89.7590, -90.6179, -87.9506, -92.4219, -92.4403, -89.5014, -89.0700, -92.4520, -89.9495, -88.7658, -87.9458, -91.3590, -88.5391, -91.7912, -88.2305, -88.9647, -89.2432, -93.0927, -90.2120	),
	  latitude = c(34.2912, 36.3082, 31.7529	,33.2676, 34.0511, 36.3674, 33.8976, 33.2140, 35.8309, 35.5883, 35.2960, 33.8328, 33.5885, 36.1168, 35.7419, 34.2693, 33.2415, 36.0422, 33.9573, 35.7638, 34.6784, 34.4277, 35.5740, 34.4858, 35.4478, 34.8294, 36.3413, 35.0219, 35.8591, 33.1714, 35.9798, 35.1861, 35.0036, 42.5948, 43.7072, 41.9168, 41.9162, 46.3040, 42.9441, 44.6838, 43.8342, 42.5969, 42.2451, 43.0311, 43.0906, 42.6033, 43.6465, 41.9289, 42.9594, 43.3351, 42.9439, 43.4238, 42.9542, 35.9227, 34.9736, 35.4942, 36.0664, 35.0573, 35.3336, 35.9301, 35.4854, 35.5323, 35.8103, 35.5181, 35.4875, 35.9686, 34.7376, 36.2062, 35.2797, 35.7105, 35.0055, 34.6428, 34.9901, 36.4141, 34.9876, 35.8230, 35.7059, 36.1605, 43.9691, 45.4237, 44.4524, 44.3807, 44.0818, 45.0695, 44.7348, 43.4673, 43.2405, 43.0683, 43.4171, 44.9447, 44.9465, 43.7537, 42.6817, 43.0005, 43.0222, 42.5795, 42.6612, 45.3373, 44.1198, 44.8983, 43.9457, 43.3852, 44.7195, 45.4615, 44.4760, 42.6735, 45.0339, 43.4263, 44.7892, 43.7211, 44.3042, 42.6712, 45.8991, 43.3697, 44.4705, 44.1131, 36.3082, 42.3671),
	  geo_uncertainty = c(44180, 29914, 32178, 36189, 42519, 36518, 31793, 32704, 38974, 37108, 29763, 45580, 32973, 34234, 37303, 49635, 31793, 34764, 30212, 41757, 42214, 42381, 37404, 39694, 36750, 40254, 38498, 38890, 33634, 52875, 40830, 32558, 48304, 27457, 35032, 25994, 25803, 93413, 27390, 27251, 41475, 27243, 27538, 35026, 30173, 27989, 26772, 31321, 31929, 34344, 45995, 38311, 26709, 19640, 30011, 38767, 32820, 25204, 29855, 21471, 19297, 58016, 35554, 34691, 29341, 32922, 34912, 24984, 20850, 32759, 33720, 37770, 49993, 28119, 32876, 27218, 24787, 22548, 38748, 33999, 32478, 42361, 25388, 37629, 41700, 41325, 32724, 41372, 34392, 66283, 35690, 35637, 27809, 33144, 27517, 23270, 29606, 34337, 31400, 50055, 35046, 23148, 31408, 40174, 35821, 31329, 31651, 37800, 47107, 26306, 35995, 27403, 35002, 24590, 36730, 32037, 29914, 38972),
	  geo_source = "GADM 4.1, adm2"
	)
  
	d <- merge(d1, geo, by = c("adm1", "adm2"), all.x = TRUE)  
	
	d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- as.numeric(NA) 
	
	carobiner::write_files(path, meta, d)
}

