# R script for "carob"


carob_script <- function(path) {
   
"Introgression lines (ILs) of groundnut with enhanced resistance to rust and late leaf spot (LLS) recorded increased pod and haulm yield in multilocation testing. Marker-assisted backcrossing (MABC) approach was used to introgress a genomic region containing a major QTL that explains >80% of phenotypic variance (PV) for rust resistance and 67.98% PV for LLS resistance. ILs in the genetic background of TAG-24, ICGV 91114 and JL 24 were evaluated for two seasons (Rainy 2013 and 2014) to select 20 best ILs based on resistance, productivity parameters and maturity duration. Multilocation evaluation of the selected ILs was conducted including disease hot spots. Disease incidence at these two locations is by natural infection wherein, both late leaf spot and rust occur together. The incidence of rust is severe at Aliyarnagar during the season, while LLS is moderate. In all the locations, infector rows of susceptible variety around the experimental plot and in between test entries ensured uniform spread of disease. Only the disease scores at Aliyarnagar, were considered for ANOVA as the scoring at Dharwad-Karnataka was recorded on single replication. Background genotype, environment and genotype X environment interactions are important for expression of resistance governed by the QTL region. Six best ILs namely ICGV13192, ICGV 13193, ICGV 13200, ICGV 13206, ICGV 13228 and ICGV 13229 were selected with 39–79% higher mean pod yield and 25-89% higher mean haulm yield over their respective recurrent parents. Pod yield increase was contributed by increase in seed mass and number of pods per plant."

	uri <- "doi:10.21421/D2/FPFACD"
	group <- "varieties" 
	ff  <- carobiner::get_data(uri, path, group)
	
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2, 
		data_organization = "ICRISAT", 
		publication ="doi:10.1111/pbr.12358", 
		project = NA, 
		data_type = "experiment", 
		response_vars = "yield;disease_severity",
		treatment_vars = "variety", 
		carob_contributor = "Cedric Ngakou", 
		carob_date = "2024-08-04"
	)

	ff <- ff[grep(".xlsx", basename(ff))]

	process <- function(f){
		r <- carobiner::read.excel.hdr(f, skip=0, na=c("NA",NA))
		if (is.null(r$Rust.severity.105))  r$Rust.severity.105 <- NA
		if (is.null(r$Late.leaf.spot.severity.75))  r$Late.leaf.spot.severity.75 <- NA
		data.frame(
			variety= r$Genotype,
			yield= r$Pod.yield,
			rep= as.integer(r$Replication.number),
			fwy_residue= r$Haulm.yield,
			seed_weight = as.numeric(r$Hundred.seed.weight)* 10, ## 1000 seed weight,
			location= substr(gsub("xlsx", "", basename(f)), 64, 70),
			shelling_percentage = r$Shelling.outturn,
			longitude= ifelse(grepl("Dharwad",basename(f)), 75.1333, 77.0992),
			latitude= ifelse(grepl("in Aliy",basename(f)), 27.4240, 15.3707),		  
			geo_from_source=FALSE,
			rust75= r$Rust.severity.75,
			rust90= r$Rust.severity.90,
			rust105= r$Rust.severity.105,
			LLS75= r$Late.leaf.spot.severity.75,
			LLS90= r$Late.leaf.spot.severity.90,
			LLS105= r$Late.leaf.spot.severity.105
		)	 
	}

	d <- lapply(ff, process)
	d <- do.call(rbind,d)
	
	#  Removing two rows with NA in yield
	d <- d[!is.na(d$yield),]

	d$record_id <- 1:nrow(d)
	d$country <- "India"

	d$location[d$location=="in Aliy"] <- "Aliyar Nagar"
	d$adm1 <- "Karnataka"
	d$adm1[d$location=="Aliyar Nagar"] <- "Tamil Nadu"
	d$crop <- "groundnut"
	d$planting_date <- "2013"
	d$planting_date[d$rep==1] <- "2014"
	d$trial_id <- "1"
	d$irrigated <- FALSE ## rainy season
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$inoculated <- FALSE
	d$yield_part <- "pod"

	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

	## Rust and LLS disease score at different time after planting
	dvars <- grep("^rust|^LLS", colnames(d), value=TRUE)
	dd <- d[, c("record_id", dvars)]
	# since the data has not provided the planting date we are using Days After Planting (DAP)
	days <- rep(c(75, 90, 105), 2) |> as.integer()
	x <- reshape(dd, direction="long", varying =dvars, v.names="disease_severity", timevar="step")
	x$DAP <- days[x$step]
	x$diseases <- ifelse(x$step < 4, "rust", "late leaf spot")
	x$severity_scale <- "1-9"	
	x$disease_severity <- as.character(x$disease_severity)
	x$id <- x$step <- NULL

	d[dvars] <-  NULL

	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files (path, meta, d, long=x)
}
