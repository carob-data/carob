# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
The integrated BEM and e-Agrology dataset encompasses historical data from 2012 to 2022, compiled in Mexico. This dataset contains detailed information on farmers' field data, plots, and specific details of plots related to various crops grouped in nearly five hundred variables, covering different stages of the agronomic cycle. By sharing it with the community, invaluable insights can be extracted, aiding in the dissemination of knowledge. Additionally, it supports farmers in improving their production practices
"

	uri <- "hdl:11529/10548986"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=1,
		data_organization = "CIMMYT",
		publication = "NA",
		project = NA,
		carob_date = "2025-06-27",
		design = "unitOfAnalysis: Plot",		
		data_type = "survey",		
		treatment_vars = "none",
		response_vars = "none", 
		carob_contributor = "Robert Hijmans",
		completion = 10,	
		notes = ""
	)
	

	f1 <- ff[basename(ff) == "1.-Farmer_Plot_Logbook_2012-2022_02.xlsx"]
	f2 <- ff[basename(ff) == "2.-Sowing_harvest_yields_2012-2022_01.xlsx"]
	f3 <- ff[basename(ff) == "3._Labor_harvest_activities_2012-2022_01.xlsx"]
	f4 <- ff[basename(ff) == "4.-Agricultural supplies_2012-2022_01.xlsx"]
	f5 <- ff[basename(ff) == "5.-Irrigacion_2012-2022_01.xlsx"]
	f6 <- ff[basename(ff) == "6.-Costs and revenues_2012-2022_01.xlsx"]
	f7 <- ff[basename(ff) == "Spanish-English Glossary.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	r4a <- carobiner::read.excel(f4, sheet="Organic Fertilization")
	r4b <- carobiner::read.excel(f4, sheet="Agricultural supplies")
	r4c <- carobiner::read.excel(f4, sheet="Products applied to the seed")
	r5 <- carobiner::read.excel(f5)
	r6 <- carobiner::read.excel(f6)
	r7 <- carobiner::read.excel(f7)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		farmer = r1[["FARMER ID"]],
		logbook = r1[["LOGBOOK ID"]],
		adm1 = carobiner::fix_namer1[["STATE WHERE THE FARMER LIVES"]], "title")
		adm2 = carobiner::fix_namer1[["MUNICIPALITY WHERE THE FARMER LIVES"]], "title"),
#		year = r1[["YEARS OF EXPERIENCE IN THE AGRICULTURAL SECTOR"]],
		farm_size = r1[["TOTAL AREA OF THE PLOTS SOWN BY PRODUCER (HA)"]],
		plot_size = r1[["TOTAL PLOT AREA (HA) DATA OBTAINED BY SURVEY WITH THE FARMER"]],
		plot_size2 = r1[["AREA (HA) APPROACH DATA CALCULATED BY THE DELIMITATION OF THE PLOT IN SATELLITE IMAGE"]],
		latitude = r1[["LATITUDE N"]],
		longitude = r1[["LONGITUDE W"]],
		prior_yield = r1[["GRAIN YIELD IN THE PREVIOUS YEAR"]]
		##crop = tolower(r1[["CROP RESIDUE HEIGHT IN THE PREVIOUS COMPARABLE CYCLE (CM)"]])
	)
##r1: "EDUCATION LEVEL", "GENDER", "DOES THE PRODUCER BELONG TO ANY ETHNIC GROUP", "WHAT ETHNICITY DO FARMER BELONG TO?", "PLOT ID", "STATE", "MUNICIPALITY", "LAND OWNERSHIP", "PROPERTY TYPE", "TOTAL PLOT AREA (HA) DATA OBTAINED BY SURVEY WITH THE FARMER", "AREA (HA) APPROACH DATA CALCULATED BY THE DELIMITATION OF THE PLOT IN SATELLITE IMAGE", "AGROECOLOGICAL HUB", "AVERAGE ANNUAL PRECIPITATION (MM)", "AVERAGE ANNUAL TEMPERATURE (°C)", "MAIN INCLINE", "CROP RESIDUE COVER IN THE PREVIOUS COMPARABLE CYCLE (%)", "PREDOMINANT TOPOGRAFIC RELIEF", "TECHNIFICATION LEVEL", "SOIL TEXTURE", "SOIL EROSION LEVEL", "LOGBOOK ID", "PLOT TYPE", "YEAR", "WINTER/SUMMER  SEASON", "HYDRIC REGIME", "CONSERVATION AGRICULTURE", "FERTILIZATION", "PLANTING ARREGEMENTS", "WATER CONSERVATION", "INTEGRATED PEST MANAGEMENT", "IMPROVED VARIETIES", "SOIL CONSERVATION", "COMMERCIALIZATION", "POSTHARVEST", "OTHER INNOVATION", "INNOVATION PRACTICES_ID", "CONVENTIONAL  PRACTICES_ID"


	d2 <- data.frame(
		farmer = r2[["FARMER.ID"]],
		logbook = r2[["LOGBOOK.ID"]],
		plot = r2[["PLOT.ID"]],
		## not always consistent with d1
		#adm1 = r2[["STATE"]],
		#adm2 = r2[["MUNICIPALITY"]],
		year = r2[["YEAR"]],
		crop = tolower(r2[["CROP"]]),
		planting_date = r2[["CROP.SOWING.DATE"]],
		variety = r2[["VARIETY"]],
		yield = r2[["ACTUAL.YIELD.(UNIT/HA)"]],
		unit = r2[["UNIT.OF.MEASURE/HA"]],
		irrigated = r2$HYDRIC.REGIME == "IRRIGATION",
		harvest_date = r2[["HARVEST.DATE.(FIRST.ACTIVITY.IN.THE.PLOT.BY.PRODUCTION.CYCLE)"]]
	)
##r2: "LOGBOOK.ID", "PLOT.TYPE", "WINTER/SUMER. SEASON", "HYDRIC.REGIME", "PRACTICES_TYPE", "INNOVATION/CONVENTIONAL_PRACTICES_ID", "ID.SOWING", "CROP.NUMBER.(REGISTRATION.SEQUENCE)", "CROP", "SOWING.ID.(PRIMARY.KEY)", "ACTIVITY.PERFORMED", "SEED.TYPE", "SEED.COLOR", "SOWING.DENSITY.(KG/HA)", "SOWING.DENSITY.(UNIT)", "SOWING.DENSITY.(QUANTITY/HA)", "HOW.DID.YOU.DETERMINE.WHEN.IT.WAS.TIME.TO.SOWING?", "SPECIFY.HOW.DID.YOU.DETERMINE.WHEN.IT.WAS.TIME.TO.SOWING", "DID.YOU.REPLANT/RESEED.THIS.CROP?", "CROP.SOWN.IN:", "SOWING.ARRANGEMENT", "BED.WIDTH.(CM)", "DID.YOU.CONSIDER.THE.SLOPE.OF.THE.LAND.WHEN.LAYING.OUT.THE.FURROWS?", "PART.OF.THE.FURROW.OR.BED.WHERE.THE.CROP.WAS.ESTABLISHED", "DISTANCE.BETWEEN.PLANTS.OR.PLANTING.HOLES.(CM)", "DISTANCE.BETWEEN.FURROWS.(CM)", "DISTANCE.BETWEEN.ROWS.(CM)", "DISTANCE.BETWEEN.DOUBLE.AND.TRIPLE.ROWS.(CM)", "NUMBER.OF.SEEDS.PER.BLOW", "SOWING.DEPTH.(CM)", "SOIL.MOISTURE.WHEN.SOWING", "HOW.DID.YOU.GET.THE.SEED?", "UNIT.OF.SEEDS.PURCHASED", "QUANTITY.OF.SEEDS.PURCHASED", "PRODUCT.OBTAINED", "UNIT.OF.MEASURE/HA", "ACTUAL.YIELD.(UNIT/HA)", "HARVEST.DATE.(LAST.ACTIVITY.IN.THE.PLOT.BY.PRODUCTION.CYCLE", "TOTAL.PLOT.AREA.(HA).DATA.OBTAINED.BY.SURVEY.WITH.THE.FARMER", "AREA.(HA).APPROACH.DATA.CALCULATED.BY.THE.DELIMITATION.OF.THE.PLOT.IN.SATELLITE.IMAGE", "AREA.(HA).SOWING.WHIT.INNOVATION.OR.CONVENTIONAL.PRACTICES"


## ignoring "PART", "BALE", "OTHER" for now
	d2 <- d2[d2$unit %in% c("KG", "TON"), ]
	d2$yield <- ifelse(d2$unit == "TON", d2$yield * 1000, d2$yield)
	d2$unit <- NULL

	d <- merge(d1, d2, by=c("farmer", "logbook"), all.y=TRUE)

	d3 <- data.frame(
		plot = r3[["GENERAL INFORMATION ABOUT AGRICULTURAL PLOT"]]
	)
##r3: "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "COMMON ACTIVITIES BY ACTIVITY GROUP", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...49", "...50", "...51", "...52", "...53", "...54", "...55", "...56", "...57", "...58", "WATER AND SOIL CONSERVATION.", "...60", "...61", "...62", "...63", "MECHANICAL SOIL PREPARATION", "...65", "...66", "...67", "...68", "...69", "...70", "...71", "PHYSICAL WEED CONTROL", "...73", "SOWING ACTIVITIES", "...75", "...76", "...77", "...78", "...79", "...80", "HARVEST ACTIVITIES", "...82", "...83", "...84", "...85", "...86", "...87", "...88", "...89", "...90", "...91", "...92", "...93", "...94", "...95", "...96", "...97"


	d4a <- data.frame(
		logbook = r4a[["LOGBOOK ID"]],
		farmer = r4a[["FARMER ID"]],
		plot = r4a[["PLOT ID"]],
		adm1 = r4a[["STATE"]],
		year = r4a[["YEAR"]],
		crop = tolower(r4a[["DID YOU MAKE THIS APPLICATION TO CROP1?"]]),
		treatment = r4a[["THE SEED ALREADY THE COMMITMENT TREATED?"]]
	)
##r4a: "LOGBOOK ID", "PLOT TYPE", "WINTER/SUMMER  SEASON", "HYDRIC REGIME", "INNOVATION/CONVENTIONAL_PRACTICES_ID", "PRACTICES_TYPE", "ID_APLICACIONESDEINSUMOS", "APPLICATION DATE", "APPLICATION TIME", "ACTIVITY PERFORMED", "DID YOU MAKE THIS APPLICATION TO CROP2?", "DID YOU MAKE THIS APPLICATION TO CROP3?", "DO YOU USE ANY TOOLS TO ADJUST YOUR FERTILIZER RATE?", "DO YOU APPLY FERTILIZER AT THE TIME OF PLANTING?", "DID YOU APPLY THE FERTILIZER AT APPROPRIATE TIMES? (USING CROP DEMAND CURVES):", "TYPE OF WEED TO BE CONTROLLED", "NUMBER OF PESTS YOU WANT TO CONTROL", "REASON FOR APPLICATION...24", "PEST TYPE (1)", "PEST TYPE (2)", "TYPE OF PEST (3)", "HOW DO I DETECT THE PEST?", "DID YOU USE FUNCTIONAL BIODIVERSITY IN THIS APPLICATION?", "ACTIVITY CARRIED OUT AS A USE OF FUNCTIONAL BIODIVERSITY.", "REASON FOR APPLICATION...31", "DISEASE PRESENTED", "DAMAGED PLANT PART", "PERCENTAGE (%) OF DAMAGE TO THE PLANT BY THE PEST OR DISEASE", "PERCENTAGE OF CROP DAMAGED (%)", "NUMBER OF PRODUCTS APPLIED", "PRODUCT REGISTRATION SEQUENCE", "PRODUCT CATEGORY", "KIND OF PRODUCT", "PRODUCT NAME APPLIED", "NITROGEN (N)", "PHOSPHORUS (P)", "POTASSIUM (K)", "OTHER NUTRIENTS (OPTIONAL)", "ACTIVE INGREDIENT", "AMOUNT OF PRODUCT APPLIED", "UNITS/HA", "PLACE OF APPLICATION", "LOG TYPE ID SECTION (FOREIGN KEY)"


	d4b <- data.frame()
##r4b: "LOGBOOK.ID", "INNOVATION/CONVENTIONAL_PRACTICES_ID", "PRACTICES_TYPE", "ID DE LA APLICACION (CLAVE PRIMARIA)", "APPLICATION DATE", "PRODUCT APPLIED_TYPE", "AMOUNT OF PRODUCT APPLIED (T_HA)", "PRODUCT MOISTURE IN THE APPLICATION", "APPLICATION METHOD"


	d4c <- data.frame()
##r4c: "LOGBOOK.ID", "INNOVATION/CONVENTIONAL_PRACTICES_ID", "PRACTICES_TYPE", "ACTIVITY", "ID DE LA APLICACION (CLAVE PRIMARIA)", "PRODUCT NAME APPLIED", "ACTIVE INGREDIENT", "AMOUNT OF PRODUCT APPLIED", "UNITS/HA"


	d5 <- data.frame(
		farmer = r5[["FARMER ID"]],
		plot = r5[["PLOT ID"]],
		adm1 = r5[["STATE"]],
		adm2 = r5[["MUNICIPALITY"]],
		year = r5[["YEAR"]]
	)
##r5: "LOGBOOK ID", "PLOT TYPE", "WINTER/SUMER  SEASON", "HYDRIC REGIME", "INNOVATION/CONVENTIONAL_PRACTICES_ID", "PRACTICES_TYPE", "ID_IRRIGATION", "IRRIGATION TYPE", "IRRIGATION SYSTEM", "WATER SOURCE", "IRRIGATION DATE (GRAVITY,FOR SPRINKLING AND DRIP, FIRST DATE)", "IRRIGATION DATE (GRAVITY,FOR SPRINKLING AND DRIP, LAST DATE)", "IRRIGATION RUN (M) (GRAVITY)", "HOW MANY TIMES A DAY DO YOU DO THIS IRRIGATION? (DRIP)", "HOW MANY DAYS DID I CARRY OUT THIS IRRIGATION DURING THE PERIOD INDICATED IN THE START AND END DATES? (DRIP)", "IRRIGATION SYSTEM (SPRINKLER)", "HOW MANY TIMES A DAY DO YOU DO THIS IRRIGATION? (ASPERSION)", "HOW MANY DAYS DID I CARRY OUT THIS IRRIGATION DURING THE PERIOD INDICATED IN THE START AND END DATES? (ASPERSION)", "TOTAL WATER CONSUMPTION PER APPLICATION (LT)", "IRRIGATION TIME  (HR/HA)", "AVERAGE IRRIGATION TIME  (HR/HA)", "IRRIGATION SHEET (MM) IF YOU KNOW THE IRRIGATION SHEET, REPORT IT IMMEDIATELY, IF NOT, REPORT WATER CONSUMPTION IN THE NEXT QUESTION", "WATER CONSUMPTION (LT", "TOTAL WATER CONSUMPTION PER IRRIGATION (LT", "DO I USE BOMBS?", "PUMP POWER SOURCE", "LITERS OF FUEL CONSUMED (LT", "NUMBER OF PEOPLE WHO PARTICIPATED IN THE WORK", "DAYS USED", "DURATION OF WORK", "TIME IN WHICH THE ACTIVITY WAS COMPLETED (HR"


	d6 <- data.frame(
		plot = r6[["GENERAL INFORMATION ABOUT AGRICULTURAL PLOT"]]
	)
##r6: "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "INNOVATION", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23", "...24", "CONVENTIONAL", "...26", "...27", "...28", "...29", "...30", "...31", "...32", "...33", "...34", "...35", "...36", "...37", "...38", "...39"


	d7 <- data.frame()
##r7: "SPANISH WORD", "ENGLISH TRANSLATION"


### Fertilizers 
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
## normalize names 

	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$geo_from_source <- TRUE
	
	carobiner::write_files(path, meta, d)
}
