# R script for "carob"

# ## ISSUES

## The N, P K computations in the spreadsheet are wrong. 


carob_script <- function(path) {
	
	"
	The file contains on-farm and on-station agronomy data sets generated in Eastern and Southern Africa between 2010 and 2016 under the Sustainable Intensification of Maize-Legume systems in Eastern and Southern Africa (SIMLESA) project. The five countries involved were Ethiopia, Kenya, Tanzania, Malawi and Mozambique. The experimental trials tested the performance of various Conservation Agriculture based maize-legume intercrops or rotations relative to the commonly practiced conventional till farmer practice systems. In most of the tested cropping systems, the conventional farmer practice involved tillage using the moldboard plough, the manual hand hoe or in some cases the ridge and furrow system. Conservation Agriculture involved reduced soil disturbance, provision of soil cover and the use of leguminous crop rotations or intercrops. Crop establishment techniques involved hoe prepared planting basins or stations, ripping using animal traction, direct seeding using the dibble stick, jab planters, or animal traction direct seeding equipment. Crop management in terms of planting date, plant populations, fertilization, pest and weed control, were similar across all tested cropping systems. Experiments were mostly run in the same locations over the seven year period in order to also get an understanding of how the systems performance changed over time. On-station data has 43 variables and 1114 observations On-farm data has 43 variables and 5242 observations (2017-12-17)

	"
	
	uri <- "hdl:11529/2223085"
	group <- "conservation_agriculture"
	ff	<- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=2,
		project=NA,
		publication= NA,
		data_organization = "CIMMYT",
		data_type="experiment",
		carob_contributor="Fredy Chimire",
		carob_effort = NA,
		carob_date="2023-09-10"
		modified_by="Robert Hijmans (on-going)",
		last_modified="2023-11-04"
	)
	
	
	
	
	f <- ff[basename(ff) == "Agronomy full data set 13 dec 2019 with nonames.xlsx"]
	
	# start with reading On-farm data sheet
	ctype <- rep("guess", 64)
	ctype[c(18, 38)] = "text"
	r1 <- carobiner::read.excel(f, sheet = "onfarmwith nonames", col_types=ctype, fix_names=TRUE)
	# Reading On-station data sheet
	ctype <- rep("guess", 44)
	ctype[c(16, 17, 22, 41, 43, 44)] = "text"
	r2 <- carobiner::read.excel.hdr(f, sheet = "On-station", skip= 2, hdr=2, col_types=ctype) 
	# Drop the last 6 rows
	i <- which(r2$Country.name == "Average")
	r2 <- r2[1:(i-1), ] 
	r2 <- r2[!is.na(r2$Country.name), ] 
	

	d1 <- data.frame(country=r1$country,
		longitude=r1$Longitude,
		latitude=r1$Latitude,
		location=r1$site,
		planting_date = r1$Maize.Planting.Date,
		harvest_date = r1$Maize.harvesting.dat,
		rainfall=r1$rainfall,
		treatment=r1$original_treatment,
		yield=r1$maize_grain,
		soil_clay=r1$Clay_0_20_cm,
		soil_sand=r1$sand_0_20_cm,
		soil_silt=r2$silt_0_20_cm,
		on_farm=TRUE
	)
			
			
#"Basal.fertilizer.type"
	
	
	d2 <- data.frame(country=r2$Country.name,
		location=r2$SIMLESA.Site.name,
		season=r2$Season,
		planting_date = r2$Planting.date_Maize,
		harvest_date = r2$Date.harvesting_Maize,
		rainfall=r2$Total.rainfall,
		treatment=r2$SITE_Treatment.name.code,
		yield=r2$Maize.grain.yield_.kg.DM.ha,
		variety=r2$Maize.cultivar,
		tillage=r2$Tillage.Practice,
		soil_clay=r2$Clay.0.20.cm_.pct,
		soil_sand=r2$Sand.0.20.cm_.pct,
		soil_silt=r2$Silt.0.20.cm_.pct,
		rep=r2$Replicate,
		on_farm=FALSE,
		lg_crop = r2$Legume.species,
		lg_planting_date = r2$DATES.of.OPERATIONS_Planting.date_Legume.if.any,
		lg_harvest_date = r2$Date.harvesting_Legume.if.any,
		lg_variety = r2$Legume.cultivar,
		lg_yield = r2$Legume.grain.yield_.kg.DM.ha,
		lg_fwy_residue = r2$Legume.residue.production_.kg.DM.ha
	)
	
	
	#	put dates in the correct format for the onfarm sheet
	
	##RH not correct. There are cases like 15_09_2013 to consider.
	## also many dates end up being in 1901 or thereabouts.

#	d$`Maize Planting Date` <- as.Date("1899-12-30") + as.numeric(d$`Maize Planting Date`)
#	d$`Maize harvesting dat` <- as.Date("1899-12-30") + as.numeric(d$`Maize harvesting dat`)
	

	# find geo coordinates for onstation
	locs <- unique(d2[, c("country", "location")]) 
	locs <- na.omit(locs) # remove null values


# list("ISPM Chimoio"=c(-19.08114,33.39414) ,"Melkassa"=c(8.4, 39.33333),
#														"ARI-Ilonga"=c(-9.06667,36.85),"SARI"=c(-6,35))
	
	# sources for gps coordinates 
	#https://mozambique.worldplaces.me/view-place/61801532-ispm-santo-antonio-chimoio.html
	#https://www.gps-coordinates.net/
#	d1 <- merge(d1,geocodes1,by=c("country","site"))
	
	d <- carobiner::bindr(d2, d2) 
	
	d$crop <- "maize"
	d$yield_part <- "grain"
	d$is_survey <- FALSE
	d$is_experiment <- TRUE
	d$irrigated <- FALSE
 
	carobiner::write_files(meta, d, path=path)
}


