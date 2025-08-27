# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Replication data for Yield and Response of Bean Breeding Lines for Drought Tolerance to Field Diseases

Field evaluation of breeding lines developed for drought tolerance for adaptation, yield, response to field diseases and agronomic quality
"

	uri <- "doi:10.7910/DVN/ZUDPEP"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIAT;TARI",
		publication = "doi:10.5539/jas.v11n13p81",
		project = NA,
		carob_date = "2025-08-25",
		design = NA,
		data_type = "on-station experiment",
		treatment_vars = "variety",
		response_vars = "yield;yield_marketable", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	
	ff <- ff[grepl("xls", basename(ff))]

	
	process <- function(f){
	   
	   r <- suppressWarnings(carobiner::read.excel(f, sheet="Observation", fix_names = TRUE))
	   names(r) <- gsub("PLOT_NO.8", "PLOT_NO", names(r))
	   names(r) <- gsub("REP_NO", "REP", names(r))
	   names(r) <- gsub("DF.25|Flowering.date|DF.27|DF.22|DF.29|DF.20", "DF", names(r))
	   names(r) <- gsub("DF.28|DF.26|DF.23|DF.30|DF.21", "DFd", names(r))
	   names(r) <- gsub("DPM.27|Maturity.date|DPM.29|DPM.31|DPM.22", "DPM", names(r))
	   names(r) <- gsub("DPM.28|DPM.30|DPM.25|DPM.32|DPM.23", "DPMd", names(r))
	   names(r) <- gsub("Planting.date|PLANT", "planting.date", names(r))
	   names(r) <- gsub("PlotArea_m2", "PlotArea", names(r))
	   names(r) <- gsub("ENTRY_NO.3", "DESIGNATION", names(r))
	   if (is.null(r$DFd)){ r$DFd <- NA}
	   if (is.null(r$DPMd)){ r$DPMd <- NA} 
	   if (is.null(r$DF)){ r$DF <- NA}
	   if (is.null(r$DPM)){ r$DPM <- NA}
	   if (is.null(r$CYDHA)){ r$CYDHA <- NA}
	   if (is.null(r$RUSTFL)){ r$RUSTFL <- NA}
	   if (is.null(r$CBBFL)){ r$CBBFL <- NA}
	   if (is.null(r$PlotArea)){ r$PlotArea <- NA}
	   if (is.null(r$planting.date)){ r$planting.date <- substr(r$SEASON, 1, 4)}
	   
	  d <- data.frame(
	     location= r$SITE,
	     season= r$SEASON,
	     variety= r$DESIGNATION, 
	     rep= as.integer(r$REP),
	     plot_id= as.character(r$PLOT_NO), 
	     flowering_days= as.character(r$DFd), 
	     maturity_days= as.character(r$DPMd), 
	     flowering_date= gsub("1031", NA, as.character(r$DF)),  
	     maturity_date= as.character(r$DPM), 
	     planting_date=  as.character(r$planting.date) , 
	     yield= r$YDHA,
	     yield_marketable= r$CYDHA,
	     plot_area= r$PlotArea, 
	     rust= r$RUSTFL,
	     blight = r$CBBFL,
	     trial_id= gsub(".xls", "", basename(f)),
	     crop= "common bean",
	     row_spacing= 50,
	     plant_spacing= 10,
	     on_farm= FALSE,
	     is_survey= FALSE,
	     irrigated= NA,
	     yield_moisture= as.numeric(NA),
	     yield_part= "grain",
	     stress= "disease"
	     
	  )  
	}
	
	dd <- lapply(ff, process)
	d1 <- do.call(carobiner::bindr, dd)

	## drop all rows with missing yield and yield_marketable
	d1 <- d1[!is.na(d1$yield),]

	d1$location[is.na(d1$location)] <-  d1$trial_id[is.na(d1$location)]
	d1$location <- gsub("DABs Set3 \\(DABs PYT\\) 2018A_Kitengulex", "Kitengule", d1$location)

	### Fixing date
	##
	d1$flowering_days <- ifelse(is.na(d1$flowering_days) & nchar(d1$flowering_date) < 3, d1$flowering_date, d1$flowering_days)
	d1$flowering_date[nchar(d1$flowering_date)< 3] <- NA
	d1$flowering_date <- ifelse(is.na(d1$flowering_date) & nchar(d1$flowering_days) > 2, d1$flowering_days, d1$flowering_date)
	d1$flowering_days[nchar(d1$flowering_days) > 2] <- NA	
	d1$flowering_days <- as.numeric(d1$flowering_days)
	P <- carobiner::fix_name(d1$flowering_date)
	P <- gsub("3010/2015", "30/10/2015", P)
	P <- gsub("27/102015", "27/10/2015", P)
	P <- gsub("31/102015", "31/10/2015", P)
	P <- gsub("29/102015", "29/10/2015", P)
	P <- gsub("1900-03-08", NA, P)
	d1$flowering_date <- P
	i <- which(nchar(d1$flowering_date) == 5)
	d1$flowering_date[i] <- as.character(as.Date(as.numeric(d1$flowering_date[i]), origin = "1899-12-31"))
	i <- grepl("/", d1$flowering_date)
	d1$flowering_date[i] <- as.character(as.Date(d1$flowering_date[i], format = "%d/%m/%Y"))
	## 
	d1$maturity_days <- ifelse(is.na(d1$maturity_days) & nchar(d1$maturity_date) < 4, d1$maturity_date, d1$maturity_days)
	d1$maturity_date[nchar(d1$maturity_date) < 4] <- NA
	d1$maturity_date <- ifelse(is.na(d1$maturity_date) & nchar(d1$maturity_days) > 2, d1$maturity_days, d1$maturity_date)
	d1$maturity_days[nchar(d1$maturity_days) > 3] <- NA	
	d1$maturity_days <- as.numeric(d1$maturity_days)
	P <- carobiner::fix_name(d1$maturity_date)
	P <- gsub("27/811/2015", "27/11/2015", P)
	P <- gsub("dried|102", NA, P)
	d1$maturity_date <- P
	i <- which(nchar(d1$maturity_date) == 5)
	d1$maturity_date[i] <- as.character(as.Date(as.numeric(d1$maturity_date[i]), origin = "1899-12-31"))
	i <- grepl("/", d1$maturity_date)
	d1$maturity_date[i] <- as.character(as.Date(d1$maturity_date[i], format = "%d/%m/%Y"))

	##
	d1$planting_date <- ifelse(is.na(d1$planting_date) & !is.na(d1$season), substr(d1$season, 1, 4), d1$planting_date)
	d1$planting_date <- gsub("20180329", "2018-03-29", d1$planting_date)
	i <- grepl("/", d1$planting_date)
	d1$planting_date[i] <- as.character(as.Date(d1$planting_date[i], format = "%d/%m/%Y"))

	### Adding geo coordinate

	geo <- data.frame(
	   location= c("Kawanda", "Kachwekano", "Kitengule"),
	   longitude= c(32.541, 29.950, 35.2534),
	   latitude= c(0.422, -1.255, -6.2308),
	   country= c(rep("Uganda", 2), "Tanzania"),
	   geo_from_source= FALSE
	) 

	d <- merge(d1, geo, by="location", all.x = TRUE)	

	# fertilizer apply 17:17:17 (125kg/ha) from paper
	d$N_fertilizer <-  125*0.17
	d$P_fertilizer <- 125*0.17/2.29 
	d$K_fertilizer <- 125*0.17/1.2051 
	d$season  <- NULL
	d$record_id <- as.integer(1:nrow(d))

	### Adding disease
	dse <- d[, grepl("rust|blight|record_id", names(d))]

	x <- reshape(dse, varying = list(c("rust", "blight")), v.names = "disease_severity", timevar = "disease",
		times = c(1,2),
		direction = "long")
	x$disease <- c("rust", "bacterial blight")[x$disease]
	x$severity_scale <- "1-9"
	x <- x[!is.na(x$disease_severity),]
	x$disease_severity <- as.character(x$disease_severity)
	x$id <- NULL

	d <- d[, !grepl("rust|blight", names(d))]

## more investigation needed to understand what causes very high yields that are not credible
	d$yield[d$yield > 10000] <- NA
	
	carobiner::write_files(path, meta, d, long = x)

}


