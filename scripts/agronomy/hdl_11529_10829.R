# R script for "carob"


carob_script <- function(path) {

"The present data is based on on-farm demonstration sites set in Malawi to demonstrate the best options available at the moment for the management of drought-tolerant maize varieties and conservation agriculture practices in Balaka, Machinga and Zomba communities. Crop yields in southern Africa are generally low compared to the world average and the average of developing regions. Thus, this calls for the identification of more sustainable strategies that are capable of increasing yields. Amongst the possible strategies is conservation agriculture (CA). This data is a subset of a larger data set from southern Africa that seeks to demonstrate the effects of CA technologies as compared to the traditional farmers' practices. The CA treatments included: 1. Farmers check. Traditional land preparation (ridges) and maize management. Residues may be grazed, removed, burned or incorporated into the ridges. 2. Conservation Agriculture – sole maize. No tillage, no burning. Previous year’s ridges retained (but not reformed). Residue retained (mulch). 3. Conservation Agriculture – maize/legume intercrop. No tillage, no burning. Previous year’s ridges retained (but not reformed). Residue retained (mulch). The data set presents yields for maize and the legumes from these sites over 10 seasons (2005-2015). (2016-12-08)"

	uri <- "hdl:11529/10829"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		project=NA,
		publication=NA,
		data_organization = "CIMMYT",
		data_type="on-farm experiment",
		carob_contributor="Mitchelle Njukuya",
		carob_date="2023-06-21",
		modified_by="Effie Ochieng';Robert Hijmans",
		treatment_vars=NA,
		response_vars="yield"
	)


	
	f <- ff[basename(ff) == "Summary files Malawi 2005-15..xlsx"]
	
	# to avoid warnings
	ctp <- c('numeric', 'numeric', 'text', 'text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'text', 'numeric', 'text', 'text', 'text', 'numeric', 'numeric', 'numeric') 
	r1 <- carobiner::read.excel(f, sheet = "Maize working data", col_types=ctp)
	r2 <- carobiner::read.excel(f, sheet = "Legume yields")
	r3 <- carobiner::read.excel(f, sheet = "Intercropped legume yields")	
  
  ## process file(s)
 
  ## use a subset (like this to avoid subsetting later)
	d1 <- r1[, c("Harvest Year", "District", "Village", "Plot No.", "crop grown", "Tmnt.", "Stalk yield (kg/ha)", "Grain yield (kg/ha)", "Farmer")]
	colnames(d1) <- c("harvest_date", "adm1", "location", "rep", "crop", "treatment", "fwy_residue", "yield", "fname")

	d2 <- r2[,  c("Site", "Tmnt.", "Crop grown", "Final stand (pl/ha)", "Grain yield (kg/ha)", "total Biomass yield (kg/ha)",  "Year", "Site name", "Plot No.")]
	colnames(d2) <- c("location", "treatment", "crop", "plant_density", "yield", "dmy_total", "harvest_date", "fname", "rep")

	# third data set
	d3 <- r3[, c("Year", "Site name", "Tmnt.", "Crop grown", "Final stand (pl/ha)", "total Biomass yield (kg/ha)", "Grain yield (kg/ha)", "Farmer")]
	colnames(d3) <- c("harvest_date", "location", "treatment", "crop", "plant_density", "dmy_total", "yield", "fname")

#joining tables
	d <- carobiner::bindr(d1,  d2,  d3)
	
	d$harvest_date <- as.character(d$harvest_date)
	d$yield_part <- "grain"
	d$country <- "Malawi"
	d$on_farm <- TRUE 
	d$is_survey <- FALSE


	cleanname <- function(x) {
		f <- tolower(x)
		f[f=="nasoweka h."] <- "nasoweka" 
		f = gsub("\\.", " ", f)
		f = gsub("  ", " ", f)
		f = gsub("(mrs)", "", f)
		f = gsub("kamfamveka", "kafamveka", f)
		sapply(strsplit(f, " "), \(i) i[length(i)])
	}
	d$fname <- cleanname(d$fname)
	d$trial_id <- as.character(as.integer(as.factor(paste0(d$harvest_date, d$fname))))
	d$fname <- NULL
	d$rep <- as.integer(d$rep)
	
	i <- which(is.na(d$location))

	d$crop <- carobiner::fix_name(d$crop,  case = "lower")
	d$crop <- carobiner::replace_values(d$crop, "soya", "soybean")
	d$crop[d$crop %in% c("pigeonpea", "p/peas")] <- "pigeon pea"
	d$crop[grep("nuts", d$crop)] <- "groundnut"
	d$crop[grep("cowpe", d$crop)] <- "cowpea"

	tr <- carobiner::fix_name(d$treatment, case = "lower")
	tr <- gsub("\\+", "_", tr)
	tr <- gsub(" ", "_", tr)
	tr <- gsub("__", "_", tr)
	tr <- gsub("/", "_", tr)
	tr <- gsub("variety_", "variety-", tr)
	tr <- gsub("ppea", "pigeonpea", tr)
	tr <- gsub("pp", "pigeonpea", tr)
	tr <- gsub("p_peas", "pigeonpea", tr)
	tr <- gsub("groudnnuts", "groundnuts", tr)
	tr <- gsub("gnuts", "groundnuts", tr)
	tr <- gsub("g_nuts", "groundnuts", tr)
	tr <- gsub("mucuna", "velvetbean", tr)
	tr <- gsub("cp", "cowpea", tr)
	tr <- gsub("sb", "soybean", tr)
	tr <- gsub("mz", "maize", tr)
	tr <- gsub("leg_", "legume_", tr)
	tr <- gsub("leg$", "legume", tr)
	tr <- gsub("ca", "CA", tr)
	tr <- gsub("_rot$", "_rotation", tr)
	tr <- gsub("maizepig", "maize_pig", tr)
	tr <- gsub("maizesole", "maize_sole", tr)
	tr <- gsub("conventional_controls", "conventional_control", tr)
	tr <- gsub("__", "_", tr)
	tr <- gsub("1", "-1", tr)
	tr <- gsub("2", "-2", tr)
	tr <- gsub("--1", "-1", tr)
	tr <- gsub("--2", "-2", tr)
	tr <- gsub("CA-", "CA", tr)
	d$treatment <- tr
	##unique(tr)
	## what are these?
	##"ds_maize" "ds_intercrop" "sc_719", "zm523", "mh_30", "pan53", "dkc5053")
 

	d$intercrops <- NA
	i <- d$crop != "maize"
	d$intercrops[i & grepl("maize", d$treatment)] <- "maize"
	i <- d$crop == "maize"
	d$intercrops[i & grepl("pigeon", d$treatment)] <- "pigeon pea"
	d$intercrops[i & grepl("velvet", d$treatment)] <- "velvet bean"
	d$intercrops[i & grepl("cowpea", d$treatment)] <- "cowpea"
	d$intercrops[i & grepl("soybean", d$treatment)] <- "soybean"
	d$intercrops[i & grepl("legume", d$treatment)] <- "legume"


	d$adm1 <- carobiner::replace_values(d$adm1, 
		c("Nkhotkota", "Mchinga"), 
		c("Nkhotakota", "Machinga"))

	g <- unique(d[, c("adm1", "location")])
	g <- g[!is.na(g$adm1), ]
	colnames(g)[1] <- c("ad1")
	
	d <- merge(d, g, by="location", all.x=TRUE)
	i <- is.na(d$adm1)
	d$adm1[i] <- d$ad1[i]
	d$ad1 <- NULL
	
	# why?
	d$location <- carobiner::replace_values(d$location, 
		c("Champhira", "Lemu", "Matandika"), 
		c("Champhila", "Balaka", "Machinga"))

	geo <- data.frame(
		location = c("Malula", "Enyezini", "Chisepo", "Chipeni", "Zidyana", "Mwansambo",
			"Balaka", "Machinga", "Linga", "Herbert", "Songani", "Chinguluwe", "Champhila", "Kaluluma"), 
		latitude = c(-14.967, -11.465, -13.633, -13.818, -13.214, -13.697, -14.979, -15.067, -13.063, -13.54,
			-15.3, -13.811, -12.405, -12.581), 
		longitude = c(34.992, 33.865, 33.472, 33.385, 34.317, 33.556, 34.956, 35.225, 33.436, 33.027, 35.483,
			33.499, 33.643, 33.518),
		geo_from_source=FALSE
	)

	d <- merge(d, geo, by ="location", all.x = TRUE)

	d$planting_date <- as.character(NA)
	d$plant_density[d$plant_density < 1] <- NA
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	d$irrigated <- NA
	
	d <- d[!is.na(d$yield), ]
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta,  d,  path=path)
}





  # efyrouwa: finding the coordinates, this part should not be in the script or be commented out
  ##RH  this loop should not be necessary; should be done by carobiner
  # 	loc <- unique(d[c("country", "site")])
  #  for (i in 1:nrow(l)) {
  #     ll <- carobiner::geocode(country = l$country[i],location = l$site[i], service = "geonames", username = "efyrouwa")
  #     ii <- unlist(jsonlite::fromJSON(ll))
  #     c <- as.integer(ii["totalResultsCount"][[1]])
  #     l$latitude[i] <- as.numeric(ifelse(c == 1, ii["geonames.lat"][1], ii["geonames.lat1"][1]))
  #     l$longitude[i] <- as.numeric(ifelse(c == 1, ii["geonames.lng"][1], ii["geonames.lng1"][1]))
  #   }
  #  code below will print the cordinates then copy paste to make make a data frame
  #   sss <- dput(l) 

