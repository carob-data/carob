# R script for "carob"

# to do: extract more variables

carob_script <- function(path) {

"International Durum Screening Nursery (IDSN) distributes diverse CIMMYT-bred spring durum wheat germplasm adapted to irrigated and variable moisture stressed environments. Disease resistance and high industrial pasta quality are essential traits possessed in this germplasm. It is distributed to 100 locations, and contains 150 entries. (2002)"

	uri <- "hdl:11529/10905"
	group <- "varieties_wheat"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		data_organization = "CIMMYT",
		publication= NA,
		project="International Durum Screening Nursery",
		data_type= "experiment",
		response_vars = "yield",
		treatment_vars = "variety_code",
		carob_contributor= "Robert Hijmans",
		carob_effort = NA,
		carob_date="2024-03-26"
	)

	sets <- gsub("_RawData.xls", "", grep("RawData", basename(ff), value=TRUE))

	proc_wheat <- carobiner::get_function("proc_wheat", path, group)

	maxid <- 0
	dd <- vector("list", length(sets))
	for (i in seq_along(sets)) {
		fs <- grep(sets[i], ff, value=TRUE)
		d <- proc_wheat(fs)
		d$wide$record_id <- d$wide$record_id + maxid
		d$long$record_id <- d$long$record_id + maxid
		maxid <<- max(d$wide$record_id)
		dd[[i]] <- d
	}
	dlong <- do.call(carobiner::bindr, lapply(dd, \(x) x[["long"]]))
	dwide <- do.call(carobiner::bindr, lapply(dd, \(x) x[["wide"]]))
	dlong$record_id <- as.integer(dlong$record_id)
	dwide$record_id <- as.integer(dwide$record_id)
	d <- list(long=dlong, wide=dwide)

	d$wide$crop <- "durum wheat"

	d$wide$soil_pH[d$wide$soil_pH < 2 | d$wide$soil_pH > 10] <- NA

	pds <- c(paste0(90:96, "-", 91:97), "00-01")
	rpl <- c(1990:1996, 2000)
	for (i in 1:length(pds)) d$wide$planting_date <- gsub(pds[i], rpl[i], d$wide$planting_date)
	d$wide$planting_date[d$wide$planting_date == "202000-04"] <- "1999"


	i <- which(d$wide$location == "Alameda Del Obispo" & d$wide$planting_date == "1994-11-23")
	d$wide$yield[i] <- 	d$wide$yield[i] / 100

	carobiner::write_files(path, meta, d$wide, d$long)
}


