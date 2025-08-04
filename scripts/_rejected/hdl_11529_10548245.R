# R script for "carob"

## not processed because the data in hdl:11529/10548245 are also included in hdl:11529/10548238 
## although the yield values are not the same; this needs to be looked into and perhaps reported.

carob_script <- function(path) {

	uri <- "hdl:11529/10548245"
	group <- "agronomy"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "TAMASA",
		data_type = "experiment",
		treatment_vars ="Control;PK;NK;NP;NPK;NPK+S+Ca+Zn+B;NA", 
		carob_contributor = "Shumirai Manzvera",
		carob_date = "2024-04-06"
	)
	


	meta$dataset_id <- paste0(meta$dataset_id, "_nodata")
	carobiner::write_files(path, meta)
}


.ignore <- function(path) {

# ISSUES:
#treatment_vars need to be explicitly included in the dataset

"Nutrient Ommission Trials (NOTs) from three states in Nigeria. Six treatments, yield, soil and ear leaf nutrient contents."

	uri <- "hdl:11529/10548245"
	group <- "agronomy"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "TAMASA",
		data_type = "experiment",
		treatment_vars ="Control;PK;NK;NP;NPK;NPK+S+Ca+Zn+B;NA", 
		carob_contributor = "Shumirai Manzvera",
		carob_date = "2024-04-06"
	)
	
	f <- ff[basename(ff) == "NG-2015_NOTs_soil_leaf_yield_database_PC.xlsx"]
	r <- carobiner::read.excel.hdr(f, sheet = "Raw_Data",skip = 1, hdr = 1)
	r <- r[1:(which(is.na(r$Country))-1), ]

	
	d <- data.frame(
		crop="maize",
		country=r$Country,
		adm1=r$State.Province,
		adm2=r$LGA.District,
		location=r$Community.village,
		latitude=r$GPS.Coordinate.Latitude,
		longitude=r$GPS.Coordinate.Longitude,
		elevation=r$GPS.Coordinate.Altitude,
		trial_id = r$Experimental.site.code.EXPsit,
		treatment=r$Treatment,
		yield = r$Grain.yield.kg.ha,
		variety = r$Maize.variety.type,
		planting_date = as.character(as.Date(r$Planting.date.PLNdat, format = '%d/%m/%Y')),
		harvest_date=r$Harvest.date.HDATE,
	 
		soil_SOC = r$OC.pct, 
		soil_sand= r$Sand.pct, 
		soil_silt = r$Silt.pct, 
		soil_clay = r$Clay.pct, 
		soil_ex_acidity = r$Exch.Acidity.cmol.kg, 
		soil_ECEC = r$ECEC.cmol.kg,
	 	
		soil_P_Mehlich = r$MehP.ppm, 
		soil_K = r$K.cmol.kg, 
		soil_Na = r$Na.cmol.kg, 
		soil_Zn = r$Zn.ppm, 
		soil_Cu = r$Cu.ppm,
		soil_Mn = r$Mn.ppm, 
		soil_Fe = r$Fe.ppm, 
		soil_Ca = r$Ca.cmol.kg, 
		soil_Mg = r$Mg.cmol.kg,
	
		leaf_N = r$Ear_leaf.analysis_pct.N,
		leaf_P = r$pct.P,
		leaf_K = r$pct.K,
		leaf_Ca = r$pct.Ca,
		leaf_Mg = r$pct.Mg,
		leaf_Zn = r$ppm.Zn,
 
		previous_crop= tolower(r$Crops.grown.in.the.past.two.years)
	)

	d <- d[d$country == "Nigeria",]

	d$previous_crop <- gsub(" |  ", "", d$previous_crop)
	d$previous_crop <- gsub(",|and|&", ";", d$previous_crop)
	d$previous_crop <- gsub("g/nut|gnut", "groundnut", d$previous_crop)
	d$previous_crop <- gsub("101", NA, d$previous_crop)
	
#HD<-	carobiner::fix_name(d$harvest_date )
#HD<- gsub("1900-03-24", "2015-03-24", HD)
#d$harvest_date<- HD

	i <- grep("^4", d$harvest_date)
	d$harvest_date[i] <- as.character(as.Date(as.numeric(d$harvest_date[i]), origin = "1900-01-01") - 2)
	i <- grep("/2015$", d$harvest_date)
	d$harvest_date[i] <- as.character(as.Date(d$harvest_date[i], format = "%d/%m/%Y"))


	d$on_farm <- TRUE
	d$yield_part <- "grain"
	
#	ft <- lapply(gsub("149", NA, r$Fertilizer.type), \(i) c(unlist(strsplit(i, ", ")), NA)[1:2])
#	ft <- do.call(rbind, ft)
#	ft[,1] <- gsub("Urea", "urea", ft[,1])
#	fa <- lapply(gsub("149", NA, r$Fertilizer.application.rate), \(i) c(as.numeric(unlist(strsplit(i, ", "))), 0)[1:2])
#	fa <- do.call(rbind, fa)
##	ftab <- vocal::accepted_values("fertilizer_type")
#	ftab[ftab$name=="NPK", c("N", "P", "K")] <- c(20, 20, 20)	
#	get_elements <- carobiner::get_function("get_elements_from_product", path, group)
#	elements <- get_elements(ftab, x$product)
#	use <- elements * x$amount
#	fert <- aggregate(use, x["id"], sum, na.rm=TRUE)
#	colnames(x) <- c("f1", "f2", "a1", "a2")
#	x$id <- 1:nrow(x)
	
	
	carobiner::write_files(path, meta, d)
}

