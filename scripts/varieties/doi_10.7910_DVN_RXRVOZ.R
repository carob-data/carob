# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Replication Data for Agronomic quality and Resistance of Andean beans and advanced breeding lines to root rots in Uganda

Bean root rot improvement
"
   
	uri <- "doi:10.7910/DVN/RXRVOZ"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ABC",
		publication = "doi:10.4314/acsj.v28i3.5",
		project = NA,
		carob_date = "2025-09-15",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;yield_marketable", 
		carob_contributor = "Cedric Ngakou",
		completion = 90,	
		notes = "NE_BL_Set3 root rot.xlsx file has not been processed as its content is not suitable for carob."
	)
	
	ff1 <- ff[!grepl("root", basename(ff))]

	process <- function(f){
	   
	   if(basename(f)=="ADP PYT _Root rot data.xls"){
	      r <- carobiner::read.excel(f, sheet="Field experiments_1&2", fix_names = TRUE)
	   } else {
	      r <- carobiner::read.excel(f, sheet="Observation", fix_names = TRUE)
	   }
	  
	   names(r) <- gsub("REP_NO", "REP", names(r))
	   names(r) <- gsub("PLANTING.DATE", "Planting.date", names(r))
	   if(basename(f)=="NE_BL_Set3 PYT1_2014A.xls" ){ 
	    r$DFd <- r$DF.22 
	    r$DF <- r$DF.23 
	    r$DPM <- r$DPM.25 
	    r$DPMd <- r$DPM.24
	    } else { 
	   names(r) <- gsub("DF.21|DF.26|DF.23|DF.24", "DFd", names(r))
	   names(r) <- gsub("DF.22|DF.27|DF.25", "DF", names(r))
	   names(r) <- gsub("DPM.23|DPM.28|DPM.25|DPM.26", "DPMd", names(r))
	   names(r) <- gsub("DPM.24|DPM.29|DPM.27", "DPM", names(r))}
	   if(is.null(r$CYDHA)) r$CYDHA <- NA 
	   if(is.null(r$Season)) r$Season <-  NA
	   if(is.null(r$Planting.date)) r$Planting.date <-  NA
	   if(is.null(r$DPMd)) r$DPMd <-  NA
	   if(is.null(r$DFd)) r$DFd <-  NA
	   
	   
	   d <- data.frame(
	      variety = r$DESIGNATION,
	      plot_id = as.character(r$PLOT_NO),
	      rep = as.integer(r$REP),
	      season = r$Season,
	      blight = r$CBBFL,
	      rust = r$RUSTFL,
	      flowering_days = r$DF, 
	      flowering_date = as.character(r$DFd),
	      maturity_days = r$DPM, 
	      maturity_date = as.character(r$DPMd),
	      yield = r$YDHA,
	      yield_marketable = r$CYDHA,
	      plot_area = r$PlotArea_m2,
	      planting_date = as.character(r$Planting.date),
	      trial_id = gsub(".xls", "", basename(f)),
	      crop = "common bean",
	      country = "Uganda",
	      location = "Kawanda",
	      longitude = 32.541 ,
	      latitude = 0.422,
	      row_spacing = 50,
	      plant_spacing = 10,
	      on_farm = FALSE,
	      is_survey = FALSE,
	      yield_part = "seed",
	      yield_moisture = as.numeric(NA),
	      irrigated = NA,
	      geo_from_source = FALSE,
	      stress = "disease"
	   )
	}
	
	dd <- lapply(ff1, process)
	d <- do.call(carobiner::bindr, dd)
	
	## drop rows with missing yield
	d <- d[!is.na(d$yield),]
	d$record_id <- as.integer(1:nrow(d))
	
### Fixing date 
	
d$planting_date <- ifelse(grepl("ADP PYT _Root rot data", d$trial_id), substr(d$season, 1, 4), 
                   ifelse(grepl("B11_BL_Set4_PYT1_2014A|NE_BL_Set3 PYT1_2014A|N11_BL_Set1 PYT1_2014A", d$trial_id), "2014-02-05", 
                   ifelse(grepl("B11_BL_Set4_PYT2_2014B", d$trial_id), "2014-10-24", d$planting_date)))	

i <- grepl("/", d$planting_date)	
d$planting_date[i] <- as.character(as.Date(gsub("0ct", 10, d$planting_date[i]), "%d/%m/%Y"))	
	
i <- grepl("/", d$flowering_date)	
d$flowering_date[i] <- as.character(as.Date(d$flowering_date[i], "%d/%m/%Y"))	

i <- !grepl("-", d$flowering_date) & !is.na(d$flowering_date)
d$flowering_date[i] <- as.character(as.Date(as.numeric(d$flowering_date[i]), origin= "1899-12-31"))

i <- grepl("/", d$maturity_date)	
d$maturity_date[i] <- as.character(as.Date(d$maturity_date[i], "%d/%m/%Y"))	

i <- !grepl("-", d$maturity_date) & !is.na(d$maturity_date)
d$maturity_date[i] <- as.character(as.Date(as.numeric(d$maturity_date[i]), origin= "1899-12-31"))

d$flowering_date[grepl("1900-01-07|1918-05-25|1918-05-17",d$flowering_date)] <- NA
d$maturity_date[d$maturity_date=="1934-07-01"] <- NA
d$season <- NULL

# fertilizer apply 17:17:17 (125kg/ha) from paper
d$N_fertilizer <-  125*0.17
d$P_fertilizer <- 125*0.17/2.29 
d$K_fertilizer <- 125*0.17/1.2051 

#### Adding disease 
ds <- d[, grepl("blight|rust|record_id", names(d))]

x <- reshape(ds, varying =c("blight", "rust"), v.names = "disease_severity", timevar ="disease",
     times = c("bacterial blight", "rust"),
     direction = "long")
x$severity_scale <- "1-9"
x <- x[!is.na(x$disease_severity),]
x$id <- NULL
rownames(x) <- NULL


d <- d[, !grepl("blight|rust", names(d))] 

### setting very low flowering days  into NA   
d$flowering_days[d$flowering_days < 15] <- NA

## more investigation needed to understand what causes very high yields that are not credible
d$yield[d$yield > 9000] <- NA

carobiner::write_files(path, meta, d, long = x)

}

