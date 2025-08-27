# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Replication Data for Characterization of Phaseolus coccineus interspecific germplasm accessions for disease resistance, grain market class and yield attributes

P. coccineus interspecific lines (Agronomic quality, resistance levels in Fusarium and Pythium root rot, seed attributes)
"

	uri <- "doi:10.7910/DVN/D1DWVN"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIAT",
		publication = "doi:10.4314/acsj.v26i1.9",
		project = NA,
		carob_date = "2025-08-27",
		design = NA,
		data_type ="on-station experiment",
		treatment_vars = "variety",
		response_vars = "yield;yield_marketable", 
		carob_contributor = "Cedric Ngakou",
		completion = 90,	
		notes = "ALB Fusarium_Pythium root rot file has not been processed as it does not contain suitable data for carob."
	)
	
   ff1 <- ff[grepl("BPYT|PYT", basename(ff))]
	
	f1 <- ff[basename(ff) == "ALB SEED ATTRIBUTES.xlsx"]
	r1 <- carobiner::read.excel(f1, fix_names = TRUE)

 process <- function(f){
    
    r <- carobiner::read.excel(f, sheet="Observation", fix_names = TRUE)
    names(r) <- gsub("REPLICATION", "REP", names(r))
    if(is.null(r$CYDHA)) r$CYDHA <- NA
    if(is.null(r$DF)) r$DF <- NA
    if(is.null(r$DPM)) r$DPM <- NA
    if(is.null(r$Plant.Stand)) r$Plant.Stand <- NA
    
    d <- data.frame(
       variety= r$DESIGNATION,
       plot_id= as.character(r$PLOT_NO),
       rep= as.integer(r$REP),
       plant_density= (r$Plant.Stand/r$PlotArea_m2)*10000,
       blight= r$CBBFL,
       rust= r$RUSTFL,
       flowering_days= r$DF,
       maturity_days= r$DPM,
       yield= r$YDHA,
       yield_marketable= r$CYDHA,
       plot_area= r$PlotArea_m2,
       planting_date= ifelse(grepl("2011", basename(f)), "2011", "2012"),
       trial_id= gsub(".xls", "", basename(f)),
       crop= "common bean",
       country= "Uganda",
       location= "Kawanda",
       longitude= 32.541 ,
       latitude= 0.422,
       row_spacing= 50,
       plant_spacing= 10,
       on_farm= FALSE,
       is_survey= FALSE,
       yield_part= "seed",
       yield_moisture= as.numeric(NA),
       irrigated= NA,
       geo_from_source= FALSE,
       stress= "disease"
    )
 }
	
 dd <- lapply(ff1, process)
 d <- do.call(carobiner::bindr, dd)
 
 ## drop rows with missing yield
 
 d <- d[!is.na(d$yield),]
 
 ## Adding seed weight
 d1 <- data.frame(
    variety= r1$Entry,
    seed_weight= as.numeric(gsub("[^0-9\\.]", "", r1$Weight.of.100.seed))*10
 )
 
 d <- merge(d, d1, by= "variety", all.x = TRUE)
 
 ## Adding soil information
 soil <- data.frame(
    planting_date= c("2011", "2012"),
    soil_pH= c(5.3, (5.1)),
    soil_SOM= c(9.8, 5.3),
    soil_N= c(0.4, 0.3),
    soil_P= c(5.9, 4.8),
    soil_Ca= c(1769, 1888.2),
    soil_Mg= c(361.8, 522),
    soil_K= c(240.4, 214.3),
    soil_Fe= c(147.7, 80.6),
    soil_Zn= c(4.9, 4)
 )
 
 d <- merge(d, soil, by= "planting_date", all.x = TRUE)
 
 d$planting_date <- ifelse(grepl("2011A", d$trial_id), paste0(d$planting_date, "-", "03"), 
                    ifelse(grepl("2011B", d$trial_id), paste0(d$planting_date, "-", "09"), paste0(d$planting_date, "-", "03")))
 
 
 # fertilizer apply 17:17:17 (125kg/ha) from paper
 d$N_fertilizer <-  125*0.17
 d$P_fertilizer <- 125*0.17/2.29 
 d$K_fertilizer <- 125*0.17/1.2051 
 
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
 
 ## correcting yield errors probably happen during the data entry process
 i <- which(grepl("19933.333|15853.333|13940", d$yield))
 d$yield[i] <- d$yield[i]/10  

 
carobiner::write_files(path, meta, d, long = x)

}



