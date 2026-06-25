# R script for "carob"



carob_script <- function(path) {
  
	uri <- "doi:10.7910/DVN/C3YBNN"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)
	
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		project=NA, 
		publication= "NA", 
		data_organization = "ABC", 
		data_type="compilation", 
		response_vars = "yield",
		treatment_vars = "none",  # many
		carob_contributor="Robert Hijmans", 
		carob_effort = NA,
		carob_date="2024-07-16"
	)
	
	r <- read.csv(ff[basename(ff) == "01a. ERA_Compiled.csv"])
	pcode <- read.csv(ff[basename(ff) == "Practice_Codes.csv"])[, c("Code", "Subpractice")]
	outcode <- read.csv(ff[basename(ff) == "Outcome_Codes.csv"])[, c("Code", "Subindicator")]

	xw <- read.csv(file.path(path, "misc/xwalk/ERA_crosswalk.csv"))[ , 1:2]
	xw <- xw[xw$carob != "", ]
	i <- na.omit(match(names(r), xw$ERA))
	d <- r[, xw$ERA[i]]
	i <- match(names(d), xw$ERA)
	names(d) <- xw$carob[i]

	vtrt <- paste0(rep(c("C", "T"), each=13), 1:13)
	trt <- r[, vtrt]
	trt2 <- sapply(trt, \(x) pcode[match(x, pcode[,1]), "Subpractice"])
		


    tcd <- r[, c( "CID", "C.Descrip", "C.NI", "C.NO", "TID", "T.Descrip", "T.NI", "T.NO")]

Diversity


T.Feed.Source
C.Feed.Source
Species
Partial.Outcome.Name
Analysis.Function 
Version
Irrigation.C
Irrigation.T
Irrig.Meth.C
Irrig.Meth.T
	
	carobiner::write_files(meta, d, path=path)	
}


