# REJECTED 
# Reason: no .xlsx/.xls/.csv/.dta/.rds files in download (draft cannot auto-map columns)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Not measured data, degital soil map from a model


carob_script <- function(path) {

"
Digital mapping of soil properties in the West of Honduras, Central America.

Digital soil property maps were generated at 30 meters resolution for the West of Honduras in order to develop the AGRI v.1 tool (Monserrate et al., 2016). AGRI (from its Spanish words AGua para RIego) is a tool that combines information about climate, relief, soils, land cover, and hydrology to identify suitable water sources for implementing small irrigation projects. The soil properties mapped were sand (%), silt (%), clay (%), texture class, field capacity (v/v), wilting point (v/v), water holding capacity (v/v), and curve numbers. A database of 1887 points from González et al. (2008) were used to generate the maps of sand, silt, and clay. This database was also used to determine field capacity, wilting point and water-holding capacity for each point by applying pedotransfer functions according to Saxton & Rawls (2006). A regression kriging approach was performed by combining 80% of point data with the terrain attributes aspect, mid-slope position, normalized height, plan and profile curvature, slope and topographic wetness index generated from a digital elevation model SRTM of 30 meters resolution. The combination of sand, silt, and clay maps resulted on texture class map. The curve number was mapped using the texture and land cover maps according to Soil Conservation Service of the United States of America (USDA-SCS, 1985). The maps performance was evaluated by the normalized root mean square error (RMSEn) expressed in percentage and using 20% of data point not used for mapping. Clay, sand, silt, field capacity, water holding capacity and wilting point presented error of 16%, 17%, 13%, 19%, 10% and 18% respectively.
"

	uri <- "doi:10.7910/DVN/QVXA7U"
	group <- "draft"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=4,
		data_organization = "CIAT",
		publication = "",
		project = NA,
		carob_date = "2026-08-24",
		design = NA,
		data_type = NA,
		treatment_vars = "",
		response_vars = "", 
		carob_contributor = "Your Name",
		completion = 0,	
		notes = "",
		# The percentage of relevant variables that have been standardized (between 0 and 100%) 
		carob_completion = 0,	
		# The number of hours spent creating this script
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "1a. Sand.jpg"]
	f2 <- ff[basename(ff) == "1b. sand.asc"]
	f3 <- ff[basename(ff) == "2a. Clay.jpg"]
	f4 <- ff[basename(ff) == "2b. Clay.asc"]
	f5 <- ff[basename(ff) == "3a. Silt.jpg"]
	f6 <- ff[basename(ff) == "3b. Silt.asc"]
	f7 <- ff[basename(ff) == "4a. Texture class.jpg"]
	f8 <- ff[basename(ff) == "4b. Texture class.asc"]
	f9 <- ff[basename(ff) == "4c. Texture class legend.txt"]
	f10 <- ff[basename(ff) == "5a. Field capacity.jpg"]
	f11 <- ff[basename(ff) == "5b. Field capacity.asc"]
	f12 <- ff[basename(ff) == "6a. Wilting point.jpg"]
	f13 <- ff[basename(ff) == "6b. Wilting point.asc"]
	f14 <- ff[basename(ff) == "7a. Water holding capacity.jpg"]
	f15 <- ff[basename(ff) == "7b. Water holding capacity.asc"]
	f16 <- ff[basename(ff) == "8a. Curve number.jpg"]
	f17 <- ff[basename(ff) == "8b. Curve number.asc"]
	f18 <- ff[basename(ff) == "clay.asc"]
	f19 <- ff[basename(ff) == "curve_number.asc"]
	f20 <- ff[basename(ff) == "field_capacity.asc"]
	f21 <- ff[basename(ff) == "sand.asc"]
	f22 <- ff[basename(ff) == "silt.asc"]
	f23 <- ff[basename(ff) == "texture_class legend.txt"]
	f24 <- ff[basename(ff) == "texture_class.asc"]
	f25 <- ff[basename(ff) == "water_holding_capacity.asc"]
	f26 <- ff[basename(ff) == "wilting_point.asc"]

	#r1 <- read.???(f1)
	#r2 <- read.???(f2)
	#r3 <- read.???(f3)
	#r4 <- read.???(f4)
	#r5 <- read.???(f5)
	#r6 <- read.???(f6)
	#r7 <- read.???(f7)
	#r8 <- read.???(f8)
	#r9 <- read.???(f9)
	#r10 <- read.???(f10)
	#r11 <- read.???(f11)
	#r12 <- read.???(f12)
	#r13 <- read.???(f13)
	#r14 <- read.???(f14)
	#r15 <- read.???(f15)
	#r16 <- read.???(f16)
	#r17 <- read.???(f17)
	#r18 <- read.???(f18)
	#r19 <- read.???(f19)
	#r20 <- read.???(f20)
	#r21 <- read.???(f21)
	#r22 <- read.???(f22)
	#r23 <- read.???(f23)
	#r24 <- read.???(f24)
	#r25 <- read.???(f25)
	#r26 <- read.???(f26)
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
