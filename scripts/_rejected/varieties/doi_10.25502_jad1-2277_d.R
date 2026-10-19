# R script for "carob"
# license: GPL (>=3)

##REJECTED
#1. Dataset rejected because it is modeled data and not field observations

carob_script <- function(path) {

"
Soybean_Climate Change Impact Data_Agronomy, Kano

N2AFRICA is a large scale, science-based “research-in-development” project focused on putting nitrogen fixation to work for smallholder farmers growing legume crops in Africa.
"

	uri <- "doi:10.25502/jad1-2277/d"
	group <- "rejected"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA; SLARI",
		publication = NA,
		project = "N2AFRICA",
		design = NA,
		data_type = NA,
		treatment_vars = "variety",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-10-01",
		carob_completion = 100,	
		carob_effort = 7
	)
	

	f1 <- ff[basename(ff) == "baseline-data.csv"]
	f2 <- ff[basename(ff) == "climate-change-impact-data.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)

  weather <- data.frame(
    date=as.character(r1$YEAR),
    location=r1$LOC,
    tmin=r1$TMIN,
    tmax=r1$TMAX,
    srad=r1$SRAD,
    prec=r1$RAIN
  )
	
  d <- data.frame(
    date=r2$YEAR,
    location=r2$LOC,
    variety=r2$VARIETY,
    flowering_days=r2$FLOWERING,
    maturity_days=r2$MATURITY,
    yield=r2$YIELD,
  )

	carobiner::write_files(path, meta, d)
}

