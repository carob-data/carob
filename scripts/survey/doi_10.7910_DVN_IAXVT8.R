# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
The origin, quality and market price of rice in three sub-Saharan African Countries

The study was conducted in three SSA countries – Benin, Cameroon, and Ghana – where local partners were willing to collect data and rice samples in selected sites. Each country was mapped into geographical zones or administrative districts within which representative markets (either urban or rural) were selected. Each market was considered close to the port when the distance of the market to the nearest port was  400 km. The markets in Benin were from the northern (far from the port) to the southern (close to the port) part of the country. Those in Cameroon were only from the southern (close to the port) part of the country, while those from Ghana were only from the northern (far from the port) part of the country. The northern parts of these countries that are far from the ports are also major rice production zones, while the southern parts have major cities with higher purchasing power. Thirty-three (33) markets (19 rural and 14 urban) were surveyed in Benin, four in Cameroon (1 rural and 3 urban), and eight in Ghana (3 rural and 5 urban). The structured questionnaire in Table 1 was prepared for interviewing rice retailers in the markets randomly selected using the snowball sampling method. In addition to the information on rice types and origin in Table 2, name, age and sex, level of education, and number of years as a rice retailer were also recorded. At the end of the interview and assessment of all rice brands retailed, 300 g of each rice type was bought and appropriately labeled (See Table 1) and taken to the national laboratory for moisture content measurement and coding. Coded samples were then properly packaged and shipped to Africa Rice Center, Cotonou Benin (now Africa Rice Station) for laboratory analysis.A total of 637 (146 local and 491 imported) rice samples were received for quality analyses. The moisture content of samples at the time of purchase was measured with an electronic moisture meter and expressed as a percentage. Impurities were evaluated through manual selection and weighing of foreign matter in 200 g of each sample and expressed as a percentage. The proportion of intact grains (head rice) in 100 g of each sample was separated with a rice grader and expressed as a percentage. Grain dimensions was measured using the S21 Rice Statistical Analyzer (LKL Technologia Brazil) according to Graham-Acquaah, Manful, Ndindeng, and Tchatcha (2015. Color (Lightness (L) green-red (a) values, blue-yellow (b) values) measurements of 20 g grains per sample were done using a color meter (CR-400, Minolta Co., Ltd., Tokyo, Japan). Color intensity was computed by taking the square root of the sum of squares of absolute a and b values. Alkali spreading value (of six intact grains); an indicator of gelatinization temperature was determined in 1.7% KOH solution incubated at 30 ᵒC for 23hrs (Little, Hilder, and Dawson, 1958). Peak Viscosity, Breakdown viscosity, and final viscosity were determined using a Rapid Visco Analyzer (RVA) model super4 (Newport Scientific, Warriewood, Australia) and Thermocline for Windows (TCW3) software (Ndindeng et al, 2015). Apparent amylose content was determined following the standard iodine colorimetric method ISO 6647-2-2011 with an Auto Analyzer 3 (Seal Analytical, Germany) according to Ndindeng et al (2015). Cooking time was measured by cooking grains to obtain translucent endosperms when gently pressed between two petri dishes. Swelling ratio which defines volume increase and water uptake ratio which defines mass increase when grains are cooked using the pre-determined cooking time were respectively evaluated in 8 g and 5 g of grains for each sample. Details on the step by step execution of the analytical methods are found in Graham-Acquaah et al (2015), and Ndindeng et al (2015).
"

   uri <- "doi:10.7910/DVN/IAXVT8"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "AfricaRice",
		publication = NA,
		project = NA,
		carob_date = "2025-11-13",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "Origin, quality and market price of rice in Benin, Cameroon and Ghana.xls"]
	#f2 <- ff[basename(ff) == "Dictionnary.txt"]

	r1 <- carobiner::read.excel(f1)
	
	d1 <- data.frame(
	   record_id = as.integer(1:nrow(r1)),
		date = as.character(r1$Year),
		country = r1$Country,
		location = r1$Comty,
		#market_name= r1$Market,
		market_type = r1$Type,
		latitude = r1$Latitude,
		longitude = r1$Longitude,
		farmer_gender = r1$Gender,
		variety = r1$Origin,
		crop_price = r1$Price,
		currency = "USD",
		yield_moisture = r1$Moisture,
		crop = "rice",
		trial_id = paste(r1$Market, r1$Year, sep = "-"), 
		on_farm = FALSE, 
		is_survey = TRUE, 
		yield_part = "none",
		irrigated = NA, 
		geo_from_source = TRUE
	)
	
	
	
	### convert long and lat into decimal 
	x <- d1$longitude
	y <- d1$latitude
	convert_to_decimal <- function(x) {
	    # normalize Unicode symbols
	   x <- gsub("[˚ºᵒ]|°", "°", x)    
	   x <- gsub("[’′ʹ]|'", "'", x)    
	   x <- gsub("″|”|“|''", "\"", x) 
	   x <- gsub("°", " ", x)                           # replace ° with space
	   x <- gsub("'", " ", x)                           # replace ' with space
	   x <- gsub("\"", " ", x)                          # replace " with space
	   x <- gsub("\\s+", " ", x)
	   
	   # Extract direction (N/S/E/W)
	   dir <- ifelse(grepl("[NnSsEeWw]$", x), substr(x, nchar(x), nchar(x)) , "")
	   x <- trimws(substr(x, 1, nchar(x)-1))
	   
	   # Split into D M S (degrees, minutes, seconds)
	   parts <- strsplit(x, " ")
	   result <- sapply(parts, function(p) {
	      p <- p[p != ""]  # remove empty parts
	      d <- as.numeric(p[1])
	      m <- ifelse(length(p) >= 2, as.numeric(p[2]), 0)
	      s <- ifelse(length(p) >= 3, as.numeric(p[3]), 0)
	      d + m/60 + s/3600
	   })
	   # Apply sign for South and West
	   result[dir %in% c("S", "W")] <- -result[dir %in% c("S", "W")]
	   
	   return(result)
	}
	
	# get long and lat in decimal
	
	d1$longitude <- convert_to_decimal(x)
	d1$latitude <- convert_to_decimal(y)
	
	i <- grepl("Come", d1$location) & grepl("Benin", d1$country)
	 d1$longitude[i] <- 1.886
	 d1$latitude[i] <- 6.4101
	 d1$geo_from_source[i] <- FALSE
	 
	d1$N_fertilizer <- d1$P_fertilizer <- d1$K_fertilizer <- as.numeric(NA)
	
	
carobiner::write_files(path, meta, d1)

}


