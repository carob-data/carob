# R script for "carob"


# a lot more to be done

carob_script <- function(path) {
  
"Summary
The experiment tests the effects of different amounts of lime (and thus soil pH), P and K on the yields of a sequence of arable crops, on a silty clay loam soil at Rothamsted Experimental Farm, from 1962-1996. Mg, Mn and S were tested on split plots at different times. Target soil pH was 4.5, 5.5, 6.5 and 7.5. The dataset contains plot and sub-plot yields for all plots and years, 1962-1996, details of all treatments applied (lime, P, K, Mg, Mn and S), and agronomic details (crop variety, sowing and harvest date, basal fertilizer).

There was a parallel experiment at Woburn Experimental Farm. This experiment was under grass from 1997-2019 and no treatments were applied. In April 2019 the experiment was revived, and is currently being used to investigate the effect of different lime, P and Zn treatments on macro and micro nutrient uptake of wheat varieties. For more details contact Dr SM Haefele.

This dataset was partially funded by the BBSRC Institute Strategic Programme Soil to Nutrition (BBS/E/C/000I0310).

Methods
Period 1 (1962-1980): four lime levels with and without fertilizer P and K. Magnesium was tested on split plots 1974-1978. Period 2 (1981-1996): four lime levels with four P levels. Manganese was tested on split plots 1987-1990; Sulphur was tested on split plots 1991-1996. 2 replicates."


	uri <- "doi:10.23637/rcs10-rltlyields-01"
	group <- "fertilizer"
	ff <- carobiner::get_data(uri, path, group)
  
	meta <- carobiner::get_metadata(uri,path,group,major=1,minor = 0,
		project="NA",
		publication=NA,
		data_organization = "Rothamsted",
		carob_contributor="Robert Hijmans",
		carob_effort = NA,
		carob_date="2024-04-23",
		data_type="experiment"
	)
  
	f <- ff[basename(ff) == "crop_data.csv"] 
	r <- read.csv(f, na=c("", "*"))
  
	d <- data.frame(
		trial_id = "1",
		country = "United Kingdom",
		adm1 = "England",
		location = r$site,
		rep = r$block,
		crop = r$crop,
		variety = r$variety,
		planting_date = r$sow_date,
		harvest_date = r$harvest_date,
		yield = r$yield * 1000
	)

	# Rothamsted Research Farm
	d$longitude <- -0.377
	d$latitude <- 51.81 
	
	d$crop <- gsub("spring |winter ", "", d$crop)
	d$crop <- carobiner::replace_values(d$crop, 
			c("fallow", "potatoes", "beans", "oilseed rape", "lupins", "linseed"), 
			c("none", "potato", "faba bean", "rapeseed", "white lupin", "flax"))

##	d$yield[which(d$yield==0)] <- NA # ? not clear what these are
		
	carobiner::write_files (meta, d, path=path)
 
}
