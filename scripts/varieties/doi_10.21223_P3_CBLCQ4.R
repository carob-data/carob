# R script for "carob"
# license: GPL (>=3)


## ISSUES
## Alternaria blight disease severity was recorded twice (alt1 and alt2), but the recording times are missing.

carob_script <- function(path) {

"
Dataset for: Multilocation phenotyping of the 'Bearegard by Tanzania' mapping populations in Uganda

The BT population is a bi-parental cross comprising 317 F1 genotypes that were evaluated along with the two parents ('Beauregard' and 'Tanzania') and one international check clone 'CIP1999062.1'. The trial was conducted at three locations in Uganda (Namulonge, Serere and Kachwekano) for two seasons 2016 B and 2017 A in an alpha lattice experimental design.  Each genotype was planted with 16 plants per plot which were replicated three times per site. Data for every season at each location was checked for consistency using the command spconsis of package st4gi in R.  Any value that was three times the interquartile range was detected as an outlier. Standardized residuals for each observation were calculated using the Proc GLM and the Proc Univariate procedures in SAS. Genotype observations with residuals ± 4 were set to missing values.
"

	uri <- "doi:10.21223/P3/CBLCQ4"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization ="CIP;NCSU", #"International Potato Center; North Carolina State University",
		publication = NA,
		project = NA,
		carob_date = "2025-07-28",
		design = "alpha lattice experimental",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;fwy_total;yield_marketable;fwy_leaves", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f <- ff[basename(ff) == "3.0 BTphenotyping trial Uganda.xlsx"]

	r <- suppressWarnings(carobiner::read.excel(f, na="."))

	d <- data.frame(
		rep= as.integer(r$rep),
		location= r$Location,
		plot_id= as.character(r$Plot),
		planting_date= substr(r$Season,1, 4),
		flesh_color= as.numeric(r$fcol),
		variety= r$Genotype,
		yield= r$RYTHA*1000, ##  kg/ha
		fwy_leaves= r$FYTHA*1000,
		yield_marketable= r$CYTHA*1000,
		harvest_index= r$HI,
		fwy_total= r$BIOM*1000,
		diseases="alternaria blight",
		disease_severity= as.character(r$alt1),
		severity_scale= "1-9",
		#root_damage= r$damr,
		##shelling_percentage= r$SHI, ??? sweetpotato!
		country= "Uganda",
		crop= "sweetpotato",
		yield_part= "roots",
		is_survey= FALSE,
		on_farm= TRUE,
		geo_from_source= FALSE,
		irrigated= NA
		
	)

	geo <- data.frame(
	   location= c("Namulonge", "Serere", "Kachwekano"),
	   longitude= c(32.620, 33.534, 29.950),
	   latitude= c(0.534, 1.5020, -1.255 ),
	   trial_id= c("1", "2", "3")
	)
	
	dd <- merge(d, geo, by="location")	

	cols <- c("white", "cream", "dark cream", "pale yellow", "dark yellow", "pale orange", "intermediate orange", "dark orange", "strongly pigmented with anthocyanins")
	d$flesh_color[d$flesh_color > 9] <- NA
	d$flesh_color <- cols[d$flesh_color]	

	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

	## keep rows with yield > 0  
	#d <- d[d$yield>0, ]
		
	carobiner::write_files(path, meta, d)

}


