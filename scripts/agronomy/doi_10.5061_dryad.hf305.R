# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {

"
Data from: Diversification practices reduce organic to conventional yield gap

Agriculture today places great strains on biodiversity, soils, water and the atmosphere, and these strains will be exacerbated if current trends in population growth, meat and energy consumption, and food waste continue. Thus, farming systems that are both highly productive and minimize environmental harms are critically needed. How organic agriculture may contribute to world food production has been subject to vigorous debate over the past decade. Here, we revisit this topic comparing organic and conventional yields with a new meta-dataset three times larger than previously used (115 studies containing more than 1000 observations) and a new hierarchical analytical framework that can better account for the heterogeneity and structure in the data. We find organic yields are only 19.2% (±3.7%) lower than conventional yields, a smaller yield gap than previous estimates. More importantly, we find entirely different effects of crop types and management practices on the yield gap compared with previous studies. For example, we found no significant differences in yields for leguminous versus non-leguminous crops, perennials versus annuals or developed versus developing countries. Instead, we found the novel result that two agricultural diversification practices, multi-cropping and crop rotations, substantially reduce the yield gap (to 9 ± 4% and 8 ± 5%, respectively) when the methods were applied in only organic systems. These promising results, based on robust analysis of a larger meta-dataset, suggest that appropriate investment in agroecological research to improve organic management systems could greatly reduce or eliminate the yield gap for some crops or regions.
"

	uri <- "doi:10.5061/dryad.hf305"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=NA,
		data_organization ="UCB", #University of California, Berkeley
		publication = "doi:10.1098/rspb.2014.1396",
		project = NA,
		carob_date = "2025-07-06",
		design = NA,
		data_type ="compilation",
		treatment_vars = "OM_used; N_fertilizer; P_fertilizer",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f <- ff[basename(ff) == "DataForSubmitting.csv"]

	r <- read.csv(f, na= c("", "NA"))
	
	d1 <- data.frame(
		year =  substr(r$year.org, 1, 4),
		reference= r$Author,
		country = gsub(" ", "", r$Country),
		latitude = as.numeric(gsub("31.18.87", "31.18", r$Y_coord)),
		longitude= as.numeric(gsub("121.77.11", "121.77", r$X_coord)),
		crop = tolower(r$Crop.species),
		location = r$Study.site,
		yield = r$Mean.Org,
		unit= r$Yield.unit,
		treatment = gsub(" ", "", r$Organic.treatment), 
		#crop_rotation= r$Rotations,
		OM_used= ifelse(is.na(r$Green.manure.org) & !is.na(r$Animal.manure.org), r$Animal.manure.org, r$Green.manure.org),
		
		irrigated= ifelse(grepl("yes", r$Irrigation), TRUE,
		           ifelse(grepl("no", r$Irrigation), FALSE, r$Irrigation)),
		land_prep_method= r$Tillage,
		N_organic= as.numeric(r$Org.N.input.kg.ha),
		P_organic= as.numeric(r$Org.P.input.kg.ha),
		N_fertilizer= 0,
		P_fertilizer= 0,
		trial_id = gsub("\\.| ", "", paste(r$Study., r$Comparison., r$Author, sep = "_")), 
		on_farm= ifelse(grepl("on-farm", r$Study.type), TRUE, FALSE),
		is_survey=ifelse(grepl("survey", r$Study.type), TRUE, FALSE) 
		)
	
	### Fixing OM used 
	d1$OM_used= ifelse(grepl("yes", d1$OM_used) , TRUE, 
	                ifelse(grepl("no", d1$OM_used), FALSE, d1$OM_used))
	d1$OM_used[which(d1$N_organic > 0)] <- TRUE
	
	d2 <- data.frame(
	  year = substr(r$year.conv, 1, 4),
	  reference= r$Author,
	  country = gsub(" ", "", r$Country),
	  latitude = as.numeric(gsub("31.18.87", "31.18", r$Y_coord)),
	  longitude= as.numeric(gsub("121.77.11", "121.77", r$X_coord)),
	  crop = tolower(r$Crop.species),
	  location = r$Study.site,
	  yield = r$Mean.conv,
	  unit= r$Yield.unit,
	  treatment = gsub(" ", "", r$Conv.treatment),
	  #crop_rotation= r$Rotations,
	  OM_used= FALSE,
	  irrigated= ifelse(grepl("yes", r$Irrigation), TRUE,
	             ifelse(grepl("no", r$Irrigation), FALSE, r$Irrigation)),
	  land_prep_method= r$Tillage,
	  N_fertilizer= as.numeric(r$Conv.N.input.kg.ha),
	  P_fertilizer= as.numeric(r$Conv.P.input.kg.ha),
	  N_organic= 0,
	  P_organic= 0,
	  trial_id = gsub("\\.| ", "", paste(r$Study., r$Comparison., r$Author, sep = "_")),  
	  on_farm= ifelse(grepl("on-farm", r$Study.type), TRUE, FALSE),
	  is_survey=ifelse(grepl("survey", r$Study.type), TRUE, FALSE)  
	)
	
	d <- carobiner::bindr(d1, d2)

	d$yield_part <- "none"
	d$K_fertilizer <- 0 
	d$geo_from_source <- TRUE
 ### Fixing crop names
 
	P <- carobiner::fix_name(d$crop)
	P <- gsub("alfalfa", "lucerne", P)
	P <- gsub("barely", "barley", P)
	P <- gsub("^bean$", "common bean", P)
	P <- gsub("chard", "beetroot", P)
	P <- gsub("chili", "chili pepper", P)
	P <- gsub("elephant foot yam", "yam", P)
	P <- gsub("fennel", "", P)
	P <- gsub("grapes", "grape", P)
	P <- gsub("muskmellon", "cantaloupe", P)
	P <- gsub("oat", "oats", P)
	P <- gsub("pea grain", "pea", P)
	P <- gsub("pidgeon pea", "pigeon pea", P)
	P <- gsub("raisins", "grape", P)
	P <- gsub("spring wheat", "wheat", P)
	P <- gsub("sweet corn", "maize", P)
	P[P==""] <- NA
	d$crop <- P
	## Fixing crop yield unit

	d  <- d[!grepl("kg/ square centimeter|trays/ha|lb/plant|boxes/ha|kg/plant|kg Fw/plant|kg/tree|bales/ha|^g$|^kg$", d$unit), ]

	d$unit[d$reference == "Teasdale et al."] <- "kg/ha"

	
	d$yield2 <- ifelse(grepl("Kg/ha|kg/ha|ka/ha", d$unit), d$yield, 
				ifelse(grepl("Mg/ha|t/ha", d$unit), d$yield*1000, 
				ifelse(grepl("t/A|t/acre", d$unit), d$yield*1000*2.471, 
				ifelse(grepl("g/m\\^2", d$unit), d$yield*10, #!! d$yield*1000, 
				ifelse(grepl("kg/m2", d$unit), d$yield*10000,
				ifelse(grepl("dt/ha", d$unit), d$yield*100,
				ifelse(grepl("bu/acre|bushel/a|Bu/acre|bu/ac", d$unit) & grepl("maize", d$crop), d$yield*62.74,
				ifelse(grepl("bu/acre|bushel/a|Bu/acre|bu/ac", d$unit) & grepl("soybean|wheat", d$crop), d$yield*67.25, 
				ifelse(grepl("bu/acre|bushel/a|Bu/acre|bu/ac", d$unit) & grepl("oat", d$crop), d$yield*35.86, 
				ifelse(grepl("lb/A", d$unit), d$yield*1.12085, -99))))))))))

	
	d$planting_date <- gsub("1", NA, d$year) 
	d$OM_used <- as.logical(d$OM_used)
	d$irrigated <- as.logical(d$irrigated)
	d$unit <- d$year <- NULL
	
	d$location <- iconv(d$location, from = "latin1", to = "UTF-8", sub = "byte")
	### Fixing land_prep_method 
	d$land_prep_method <- ifelse(grepl("no-till", d$land_prep_method), "minimum tillage",
						  ifelse(grepl("reduced", d$land_prep_method), "reduced tillage", "tillage"))
	### fixing country 
	
	P <- carobiner::fix_name(d$country)
	P <- gsub("CostaRica","Costa Rica", P)
	P <- gsub("CzechRepublic","Czech Republic", P)
	P <- gsub("UnitedStates","United States", P)
	d$country  <- P
	
	i <- grepl("Greece", d$country) & grepl("Oropos", d$location)
	d$latitude[i] <- 38.3037
	d$longitude[i] <- 23.7552   
	d$geo_from_source[i] <- FALSE
	
	i <- grepl("Croatia", d$country) & grepl("Svica", d$location)
	d$latitude[i] <- 44.872 
	d$longitude[i] <- 15.172 
	d$geo_from_source[i] <- FALSE
	
	i <- grepl("India", d$country) & grepl("Madhya Pradesh|experimental farm", d$location)
	d$latitude[i] <- 23.577
	d$longitude[i] <-  76.898  
	d$geo_from_source[i] <- FALSE
	
	i <- grepl("India", d$country) & grepl("Dist. Nashik", d$location)
	d$latitude[i] <- 20.0041 
	d$longitude[i] <-  73.789 
	d$geo_from_source[i] <- FALSE
	
	i <- grepl("Norway", d$country) 
	d$longitude[i] <-  abs(d$longitude[i])

	i <- grepl("Denmark", d$country) & grepl("Research Centre Aarslev", d$location)
	d$latitude[i] <-  55.18 
	d$longitude[i] <- 10.27

	
	i <- grepl("United States", d$country) & grepl("Swan Lake Research Farm", d$location)
	d$latitude[i] <-  41.7529
	d$longitude[i] <- -74.7806
	d$geo_from_source[i] <- FALSE
	
	
	i <- grepl("United States", d$country) & grepl("Goldsboro", d$location)
	d$latitude[i] <-  35.384
	d$longitude[i] <-  -77.998
	d$geo_from_source[i] <- FALSE
	
	i <- grepl("United States", d$country) & grepl("University of Minnesota", d$location)
	d$latitude[i] <-  44.9741
	d$longitude[i] <-  -93.227
	d$geo_from_source[i] <- FALSE
	
	i <- grepl("Estonia", d$country) & grepl("Jogeva,Estonia|Breeding", d$location)
	d$latitude[i] <-  58.742
	d$longitude[i] <-  26.387
	d$geo_from_source[i] <- FALSE
	
	
	i <- grepl("Italy", d$country) & grepl("Naples University", d$location)
	d$latitude[i] <-  40.8461 
	d$longitude[i] <- 14.257
	d$geo_from_source[i] <- FALSE
	
	d$location <- gsub(" ", "", d$location)
	d$reference <- gsub(" ", "", d$reference)
	
	geo <- data.frame(
		country = c("United States", "Switzerland", "Croatia", "Poland", rep("United States", 3), rep("Turkey", 3), "United States", "Czech Republic", "Bulgaria"), 
		location = c("Mississippi,USA","Basel", "Pula" , "Poland", "Greenfield,Iowa","Yolocounty", "cornbelt,USA", "MalatyaProvince,Turkey", "AydinProvince;Turkey", 
					 "AegeanRegionofTurkey", "AgriculturalFieldLaboratoryatMeadNebraska", "ExperimentalstationofCzechUniversityofAgricultureinPrague", "TrakiaUniversity"), 
		lon = c(-90.874, 7.5902, 13.875, 19.1955, -94.4432, -121.309, -96.7463, 38.5038, 27.831, 26.885, -96.490, 14.374, 25.572), 
		lat = c(32.3181, 47.5545, 44.8736, 52.0191, 41.3051, 38.553, 43.5590, 38.357, 37.829, 39.0873, 41.228, 50.1301, 42.401),
		geo_from= c(rep(FALSE, 13))
	) 
	
	d <- merge(d, geo, by= c("location", "country"), all.x = TRUE)
	
	d$longitude[!is.na(d$lon)] <- d$lon[!is.na(d$lon)]
	d$latitude[!is.na(d$lat)] <- d$lat[!is.na(d$lat)]
	d$geo_from_source[!is.na(d$geo_from)] <- d$geo_from[!is.na(d$geo_from)]
	
	d$lon <- d$lat <- d$geo_from <- NULL
	
	
	carobiner::write_files(path, meta, d)
}

