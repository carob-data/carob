# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
The integrated BEM and e-Agrology dataset encompasses historical data from 2012 to 2022, compiled in Mexico. This dataset contains detailed information on farmers' field data, plots, and specific details of plots related to various crops grouped in nearly five hundred variables, covering different stages of the agronomic cycle. By sharing it with the community, invaluable insights can be extracted, aiding in the dissemination of knowledge. Additionally, it supports farmers in improving their production practices
"

	uri <- "hdl:11529/10548986"
	group <- "agronomy"
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
		completion = 15,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "1.-Farmer_Plot_Logbook_2012-2022_02.xlsx"]
	f2 <- ff[basename(ff) == "2.-Sowing_harvest_yields_2012-2022_01.xlsx"]
	f3 <- ff[basename(ff) == "3._Labor_harvest_activities_2012-2022_01.xlsx"]
	f4 <- ff[basename(ff) == "4.-Agricultural supplies_2012-2022_01.xlsx"]
	f5 <- ff[basename(ff) == "5.-Irrigacion_2012-2022_01.xlsx"]
	f6 <- ff[basename(ff) == "6.-Costs and revenues_2012-2022_01.xlsx"]
	f7 <- ff[basename(ff) == "Spanish-English Glossary.xlsx"]

	r1 <- suppressWarnings(carobiner::read.excel(f1))
	r2 <- suppressWarnings(carobiner::read.excel(f2))
	r3 <- suppressWarnings(carobiner::read.excel(f3, skip=1))
	r4a <- suppressWarnings(carobiner::read.excel(f4, sheet="Agricultural supplies"))
	r4b <- carobiner::read.excel(f4, sheet="Organic Fertilization")
	r4c <- carobiner::read.excel(f4, sheet="Products applied to the seed")
	r5 <- suppressWarnings(carobiner::read.excel(f5))
	r6 <- suppressWarnings(carobiner::read.excel(f6, skip=1))
	#r7 <- carobiner::read.excel(f7)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		hhid = r1[["FARMER ID"]],
		logbook = r1[["LOGBOOK ID"]],
		adm1 = carobiner::fix_name(r1[["STATE WHERE THE FARMER LIVES"]], "title"),
		adm2 = carobiner::fix_name(r1[["MUNICIPALITY WHERE THE FARMER LIVES"]], "title"),
#		year = r1[["YEARS OF EXPERIENCE IN THE AGRICULTURAL SECTOR"]],
		farmland = r1[["TOTAL AREA OF THE PLOTS SOWN BY PRODUCER (HA)"]],
		latitude = r1[["LATITUDE N"]],
		longitude = r1[["LONGITUDE W"]],
		prior_yield = r1[["GRAIN YIELD IN THE PREVIOUS YEAR"]]
		##crop = tolower(r1[["CROP RESIDUE HEIGHT IN THE PREVIOUS COMPARABLE CYCLE (CM)"]])
	)
##r1: "EDUCATION LEVEL", "GENDER", "DOES THE PRODUCER BELONG TO ANY ETHNIC GROUP", "WHAT ETHNICITY DO FARMER BELONG TO?", "PLOT ID", "STATE", "MUNICIPALITY", "LAND OWNERSHIP", "PROPERTY TYPE", "TOTAL PLOT AREA (HA) DATA OBTAINED BY SURVEY WITH THE FARMER", "AREA (HA) APPROACH DATA CALCULATED BY THE DELIMITATION OF THE PLOT IN SATELLITE IMAGE", "AGROECOLOGICAL HUB", "AVERAGE ANNUAL PRECIPITATION (MM)", "AVERAGE ANNUAL TEMPERATURE (°C)", "MAIN INCLINE", "CROP RESIDUE COVER IN THE PREVIOUS COMPARABLE CYCLE (%)", "PREDOMINANT TOPOGRAFIC RELIEF", "TECHNIFICATION LEVEL", "SOIL TEXTURE", "SOIL EROSION LEVEL", "LOGBOOK ID", "PLOT TYPE", "YEAR", "WINTER/SUMMER  SEASON", "HYDRIC REGIME", "CONSERVATION AGRICULTURE", "FERTILIZATION", "PLANTING ARREGEMENTS", "WATER CONSERVATION", "INTEGRATED PEST MANAGEMENT", "IMPROVED VARIETIES", "SOIL CONSERVATION", "COMMERCIALIZATION", "POSTHARVEST", "OTHER INNOVATION", "INNOVATION PRACTICES_ID", "CONVENTIONAL  PRACTICES_ID"


	d2 <- data.frame(
		hhid = r2[["FARMER.ID"]],
		logbook = r2[["LOGBOOK.ID"]],
		plot_id = r2[["PLOT.ID"]],
		pratice = r2[["PRACTICES_TYPE"]],
		## not always consistent with d1
		#adm1 = r2[["STATE"]],
		#adm2 = r2[["MUNICIPALITY"]],
		plot_size = r2[["TOTAL.PLOT.AREA.(HA).DATA.OBTAINED.BY.SURVEY.WITH.THE.FARMER"]],
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


## removing yield for "PART", "BALE", "OTHER" for now
	d2$yield[!(d2$unit %in% c("KG", "TON"))] <- NA
	d2$yield <- ifelse(d2$unit == "TON", d2$yield * 1000, d2$yield)
	d2$unit <- NULL

	d2$yield_part <- NA
	d2$yield_part[d2$crop == "fodder oats"] <- "aboveground biomass"

	d2$crop <- gsub("\\+|,", ";", d2$crop)
	d2$crop <- gsub("mix . \\(|)", "", d2$crop)
	d2$crop <- gsub("canola", "rapeseed", d2$crop)
	d2$crop <- gsub("hyacinth bean", "lablab", d2$crop)
	d2$crop <- gsub(" ;", ";", d2$crop)

	cropnms <- data.frame(
		from=c("agave maguey", "alfalfa", "bean", "borla", "canavalia", "chili", "fava bean", "fodder oats", "habanero pepper", "husk tomato", "mucuna", "mungbean", "peanut", "potatoe", "soy", "sugar cane"), 
		to=c("agave", "lucerne", "common bean", "cockscomb", "jack bean", "chili pepper", "faba bean", "oats", "chili pepper", "tomatillo", "velvet bean", "mung bean", "groundnut", "potato", "soybean", "sugarcane")
	)
	d2$crop <- carobiner::replace_values(d2$crop, cropnms$from, cropnms$to)


	i <- grepl("/", d2$planting_date)
	d2$planting_date[i] <- as.character(as.Date(d2$planting_date[i], "%d/%m/%Y"))
	
	bad <- c("2055-05-30", "2222-06-23", "2323-06-06", "0221-07-06", "1899-12-31", "1980-04-10", "201-07-18")
	d2$planting_date[d2$planting_date %in% bad] <- NA
	
	i <- !grepl("-", d2$harvest_date)
	d2$harvest_date[i] <- as.character(as.Date("1900-01-01") - 2 + as.numeric(d2$harvest_date[i]))



	d3 <- data.frame(
		hhid = r3[["FARMER.ID"]],
		logbook = r3[["LOGBOOK.ID"]],
		plot_id = r3[["PLOT.ID"]],
		pratice = r3[["PRACTICES_TYPE"]]
	)
	
##r3: "FARMER.ID", "PLOT.ID", "LOGBOOK.ID", "PLOT.TYPE", "STATE", "YEAR", "WINTER/SUMMER  SEASON", "HYDRIC REGIME", "INNOVATION/CONVENTIONAL_PRACTICES_ID", "PRACTICES_TYPE", "ACTIVITY.ID", "GROUP.ACTIVITY", "TILLAGE.LAND", "TILLAGE.BEM", "PERFORMED.ACTIVITY", "PERFORMED.ACTIVITY_DATE", "POWER.SOURCE", "ANIMAL.EMPLOYED", "HOURS.ACTIVITY.LASTED.HR.HA", "NO.IMPLEMENTS.USED", "IMPLEMENT.USED_ANIMAL1", "THE.IMPLEMENT.IS.1", "HOW.WAS.THE.IMPLEMENT.OBTAINED?", "IMPLEMENT.CODE.TYPE", "IMPLEMENT.CODE", "IMPLEMENT.USED_ANIMAL2", "OTHER.IMPLEMENT", "THE.IMPLEMENT.IS.2", "IMPLEMENT.USED_MOTORIZED1", "THE.IMPLEMENT.IS.M1", "TRACTOR.TYPE", "HORSEPOWER", "IMPLEMENT.BRAND", "MODEL.DESCRIPTION", "YEARS.OF.AGE.IMPLEMENT", "HOURS.ACTIVITY.LASTED.HR.HA_MOTOR", "WHO.PERFORMS.OPERATION?", "FUEL.CONSUMPTION.L.HA_MOTOR1", "HOW.WAS.THE.IMPLEMENT.OBTAINED?_MOTOR", "IMPLEMENT.CODE_MOTOR", "MACHINERY.POINT.NAME", "TECHNICIAN.NAME", "OTHER.ORIGIN.IMPLEMENT", "WAS.AN.OPERATOR.HIRED?", "FUEL.CONSUMPTION.L.HA_MOTOR2", "WERE.IMPLEMENTS.USED?_MAN", "NO.IMPLEMENTS.USED_MAN", "IMPLEMENT.USED_MAN1", "IMPLEMENT.USED_MAN2", "IMPLEMENT.USED_MAN3", "IMPLEMENT.USED_MAN4", "IMPLEMENT.USED_MAN5", "NO.PEOPLE.WHO.PARTICIPATED.IN.THE.WORK", "WORKED.SHIFTS", "DURATION.OF.SHIFT.WORK.DAY.HR", "HOURS.ACTIVITY.LASTED.HR.HA_MAN1", "INPUT.APPLICATION.TOOL", "APPLICATION.TIME", "CAT.OF.WORK.CARRIED.OUT_CONSER_OF_BIOW&S", "HOW.OFTEN.IS.THE.OPERATION.PERFORMED?_CONSER_OF_BIOW&S", "TIME.UNIT_CONSER_OF_BIOW&S", "ACTIVITY.DESC_CONSER_OF_BIOW&S", "PERFORMED.ACTIVITY_CONSER_OF_BIOW&S", "MGMT.TYPE_SOIL_PREP", "CROP.RESIDUE.DESTINATION_LAST_SEASON", "FIELD.GRAZING.PERC_LAST_SEASON", "BURNED.PERC_LAST_SEASON", "INCORPORATED.PERC_LAST_SEASON", "CHOPPED.AND.LEFT.FOR.COVERAGE.PERC_LAST_SEASON", "DETAINED.FOR.COVERAGE.PERC_LAST_SEASON", "REMOVED.FROM.PLOT.BALED.IN.NUNCHES.PERC_LAST_SEASON", "TYPE.OF.WEED.TO.CONTROL", "REASON.FOR.PHYSICAL.WEED.CONTROL", "SOWING.ID", "CROP.NO_SOWING", "CROP.NAME_SOWING", "SEEDING.MACHINE.TYPE", "NO.SEEDING.MACHINE.COMPONENTS_SOWING", "APPLICATION.TYPE.NAMEINPUTS", "LOG.ID_SOWING", "GENERAL.HARVEST.ID", "NO.HARVEST.ACTIVITIES.CARRIED.OUT", "ACTIVITY.NUMBER", "WAS.THIS.HARVESTING.ACTIVITY.CARRIED.OUT.FOR.CROP.1?", "WAS.THIS.HARVESTING.ACTIVITY.CARRIED.OUT.FOR.CROP.2?", "WAS.THIS.HARVESTING.ACTIVITY.CARRIED.OUT.FOR.CROP.3?", "CROP.RELATED.TO.HARVEST.ACTIVITY", "DAYS.SPENT.IN.ACTIVITY_MANUAL_HARVEST", "FACTORS.THAT.IMPACTED.THE.HARVEST", "ENVIROMENTAL.FACTORS", "MGMT.FACTORS", "FIELD.GRAZING.PERC_HARVEST", "BURNED.PERC_HARVEST", "INCORPORATED.PERC_HARVEST", "CHOPPED.AND.LEFT.FOR.COVERAGE.PERC_HARVEST", "DETAINED.FOR.COVERAGE.PERC_HARVEST", "REMOVED.FROM.PLOT.BALED.IN.NUNCHES.PERC_HARVEST"

	d4a <- data.frame(
		hhid = r4a[["FARMER ID"]],
		plot_id = r4a[["PLOT ID"]],
		logbook = r4a[["LOGBOOK ID"]],
		pratice = r4a[["PRACTICES_TYPE"]],
		#adm1 = r4a[["STATE"]],
		#year = r4a[["YEAR"]],
		#crop = tolower(r4a[["DID YOU MAKE THIS APPLICATION TO CROP1?"]]),
		#treatment = r4a[["THE SEED ALREADY THE COMMITMENT TREATED?"]]

		N_fertilizer = r4a[["NITROGEN (N)"]],
		P_fertilizer = r4a[["PHOSPHORUS (P)"]],
		K_fertilizer = r4a[["POTASSIUM (K)"]],
		other = r4a[["OTHER NUTRIENTS (OPTIONAL)"]], 
		ai = r4a[["ACTIVE INGREDIENT"]], 
		amount = r4a[["AMOUNT OF PRODUCT APPLIED"]], 
		units = r4a[["UNITS/HA"]], 
		fertilization_method = r4a[["PLACE OF APPLICATION"]], 
		fertilizer_date = r4a[["APPLICATION DATE"]],
		time = r4a[["APPLICATION TIME"]],
		fertilizer_type = r4a[["PRODUCT NAME APPLIED"]]
	)

	i <- grepl("/", d4a$fertilizer_date)
	d4a$fertilizer_date[i] <- as.character(as.Date(d4a$fertilizer_date[i], "%d/%m/%Y"))
	d4a$fertilizer_date[d4a$fertilizer_date == "213-05-28"] <- "2013-05-28"

	i <- rowSums(is.na(d4a[, c("N_fertilizer", "P_fertilizer", "K_fertilizer", "other")])) < 4
	f <- d4a[i,]
	f <- f[f$units == "KG_HA", ]

	f$fertilizer_type <- gsub("  ", " ", f$fertilizer_type)
	f$fertilizer_type <- gsub("MEZLA|MEZLCA|MECLA|ME.CLA|MEZCAL", "MEZCLA", f$fertilizer_type)

	f$fertilizer_type <- gsub("UREA", "urea", f$fertilizer_type)
	f$fertilizer_type <- gsub("urea \\+", "urea;", f$fertilizer_type)
	
	f$fertilizer_type <- gsub("urea DE LENTA LIBERACION|urea RECUBIERTA COLANTA GRADO", "SR_urea", f$fertilizer_type)
	f$fertilizer_type <- gsub("urea 46.00.00", "urea", f$fertilizer_type)
	f$fertilizer_type <- gsub("urea PRILADA|urea ESTABILIZADA|urea MINERALIZADA", "urea", f$fertilizer_type)
	
	f$fertilizer_type <- gsub("CLORURO DE ZINC", "ZnCl2", f$fertilizer_type)
	f$fertilizer_type <- gsub("CLORURO DE CALCIO", "CaCl2", f$fertilizer_type)
	f$fertilizer_type <- gsub("CLORURO DE AMONIO", "NH4Cl", f$fertilizer_type)
	f$fertilizer_type <- gsub("CLORURO DE POTA.IO|KCL", "KCl", f$fertilizer_type)

	f$fertilizer_type <- gsub("SULFATO DE AMONIO", "DAS", f$fertilizer_type)
	f$fertilizer_type <- gsub("SU.FATO.*IN.|SULFATO DE ZN", "ZnSO4", f$fertilizer_type)

	f$fertilizer_type <- gsub("ZNSO4|ZINC", "ZnSO4", f$fertilizer_type)
	f$fertilizer_type <- gsub("PROZnSO4|SULTAFO DE ZnSO4", "ZnSO4", f$fertilizer_type)

	f$fertilizer_type <- gsub("SULFATO FERRO.O|SULFATO FERRROSO|SULFATO FEROSO|SULFATO DE FERROSO|SULFATO DE FIERRO", "FeSO4", f$fertilizer_type)
	f$fertilizer_type <- gsub("SUL..TO DE COBRE", "CuSO4", f$fertilizer_type)


	f$fertilizer_type <- gsub("SULFATO DE BORO", "BSO4", f$fertilizer_type)
	f$fertilizer_type <- gsub("SULFATO DE MANGANESO", "MnSO4", f$fertilizer_type)

	f$fertilizer_type <- gsub("SULFATO DE MAGNE.IO|SULMAG", "MgSO4", f$fertilizer_type)
	f$fertilizer_type <- gsub("SULFATO DE POTASIO", "POS", f$fertilizer_type)

	f$fertilizer_type <- gsub("TIOSULFURO DE AMONIO", "ATS", f$fertilizer_type)
	f$fertilizer_type <- gsub("SULF..O DE AMONIO", "AS", f$fertilizer_type)

	f$fertilizer_type <- gsub("FOSFATO DIAMONICO \\(DAP)", "DAP", f$fertilizer_type)
	f$fertilizer_type <- gsub("FOSFATO DIAMONICO", "DAP", f$fertilizer_type)
	f$fertilizer_type <- gsub("FOSFATO MONOAMONICO", "MAP", f$fertilizer_type)

	f$fertilizer_type <- gsub("SUPERFOSFATO SIMPLE", "SSP", f$fertilizer_type)
	f$fertilizer_type <- gsub("SUPERFOSFATO TRIPLE", "TSP", f$fertilizer_type)
	f$fertilizer_type <- gsub("FOSFONITRATO", "NP", f$fertilizer_type)
	f$fertilizer_type <- gsub("ACIDO FOSFORICO", "PA", f$fertilizer_type)

	f$fertilizer_type <- gsub("AMONIACO ANHIDRO", "NH3", f$fertilizer_type)

	f$fertilizer_type <- gsub("NITRATO DE AMONIO", "AN", f$fertilizer_type)
	f$fertilizer_type <- gsub("NITRATO DE POTASIO", "KNO", f$fertilizer_type)
	f$fertilizer_type <- gsub("NITRATO DE CALCIO", "CN", f$fertilizer_type)

	f$fertilizer_type <- gsub("K-MAG", "KMgS", f$fertilizer_type)

	f$fertilizer_type[grep("YARA.*TAR|YA.*ST|STAR YARA|SATR YaraMila", f$fertilizer_type)] <- "YaraMila Star"
	f$fertilizer_type[grep("YARA.*AC|YARA.*21|^ACT.VA|^ACTIUVA|^ACTICA", f$fertilizer_type)] <- "YaraMila ACTYVA"
	f$fertilizer_type[grep("YARA.*H.DRAN", f$fertilizer_type)] <- "YaraMila HYDRAN" 
	f$fertilizer_type[grep("YARA COMPLEX", f$fertilizer_type)] <- "YaraMila COMPLEX" 
	f$fertilizer_type[grep("^UNIK", f$fertilizer_type)] <- "YaraMila UNIK16" 
	
	f$fertilizer_type[grep("YARA.*NITRO|^NITROMAG|^NITRO MAG", f$fertilizer_type)] <- "YaraBela NITROMAG" 
	f$fertilizer_type <- gsub("YARAMILLA|YARAMILA|YARA MILLA|YARA MILA", "YaraMila", f$fertilizer_type)
	f$fertilizer_type <- gsub("YARAMIL|YARA MILL", "YaraMila", f$fertilizer_type)
	f$fertilizer_type <- gsub("YARA MIL", "YaraMila", f$fertilizer_type)
	f$fertilizer_type <- gsub("YARA BELLA|YARA BELA|YARABELA", "YaraBela", f$fertilizer_type)
	f$fertilizer_type <- gsub("YARALIVA NITRABOR", "YaraLiva NITRABOR", f$fertilizer_type)
	f$fertilizer_type <- gsub("^YAR$|^YARA$", "Yara", f$fertilizer_type)
	f$fertilizer_type[grep("YaraBela COMPLEX", f$fertilizer_type)] <- "YaraMila COMPLEX" 

	f$fertilizer_product <- NA 
	i <- grep("Yara..la.[A-Z]", f$fertilizer_type)
	f$fertilizer_product[i] <- f$fertilizer_type[i]

	f$fertilizer_type <- gsub("YaraBela NITROMAG", "NCaMg", f$fertilizer_type) #27 (half NO, half NH), 3*0.6, 6*0.7 
	f$fertilizer_type <- gsub("YaraMila Star", "NPKMgSZn", f$fertilizer_type) #21, 17*0.44, 3*.6, .4*.7, 12, .15
	f$fertilizer_type <- gsub("YaraMila ACTYVA", "NPKMgSZn", f$fertilizer_type) #23, 10*0.44, 5*.6, .3*.7, 12, .3
	f$fertilizer_type <- gsub("YaraMila HYDRAN", "NPKBMgSZn", f$fertilizer_type) #19, 4*0.44, 19*.6, .1, .3*.7, 1.9, .1
	f$fertilizer_type <- gsub("YaraMila COMPLEX", "NPKBFeMgMnSZn", f$fertilizer_type) #12, 11*0.44, 18*.6, .015, .2, 2.7*.7, .02, .4*20, .02
	f$fertilizer_type <- gsub("YaraMila UNIK16", "NPK 16-16-16", f$fertilizer_type) 


	i <- grep(" \\([0-9]", f$fertilizer_type)
	f$fertilizer_type[i] <-  gsub(" \\([0-9]", " ", f$fertilizer_type[i])
	f$fertilizer_type[i] <-  gsub(")", "", f$fertilizer_type[i])
	i <- grep("MEZCLA FISICA [0-9]|MEZCLA [0-9]", f$fertilizer_type)
	f$fertilizer_type[i] <-  gsub("MEZCLA FISICA", "NPK", f$fertilizer_type[i])
	f$fertilizer_type[i] <-  gsub("MEZCLA|FORMULA", "NPK", f$fertilizer_type[i])
	f$fertilizer_type <-  gsub("1 MEZCLA FISICA ESTANDAR|1 MEZCLA FISICA PREEMIUM", "NPK", f$fertilizer_type)
	f$fertilizer_type <-  gsub("^MF", "NPK", f$fertilizer_type)
	f$fertilizer_type[f$fertilizer_type == "MEZCLA FISICA"] <- "NPK"
	f$fertilizer_type[f$fertilizer_type == "MEZCLA MAICERA"] <- "NPK"
	f$fertilizer_type[f$fertilizer_type == "MEZCLA MAS MAIZ"] <- "NPK"

	i <- grep("^[0-9][0-9]-", f$fertilizer_type)
	f$fertilizer_type[i] <-  paste0("NPK ", f$fertilizer_type[i])
	f$fertilizer_type <- gsub("N-P-K|N-PK|N\\+P\\+K", "NPK", f$fertilizer_type)

	f$fertilizer_type <- gsub("TRIPLE 12", "NPK 12-12-12", f$fertilizer_type)
	f$fertilizer_type <- gsub("TRIPLE 15", "NPK 15-15-15", f$fertilizer_type)
	f$fertilizer_type <- gsub("TRIPLE 16", "NPK 16-16-16", f$fertilizer_type)
	f$fertilizer_type <- gsub("TRIPLE 17", "NPK 17-17-17", f$fertilizer_type)
	f$fertilizer_type <- gsub("TRIPLE18.*|TRIPLE 18.*", "NPK 18-18-18", f$fertilizer_type)

	f$fertilizer_type <- gsub(" MAS ", "; ", f$fertilizer_type)
	f$fertilizer_type <- gsub("urea/DAP", "urea;DAP", f$fertilizer_type)
	f$fertilizer_type <- gsub(",MAP Y DAS|, MAP Y DAS", ";MAP;DAS", f$fertilizer_type)
	f$fertilizer_type <- gsub("MEZCLA FISICA A BASE DE urea Y DAS|urea MAS DAS", "urea;DAS", f$fertilizer_type)


	i <- grepl("[0-9]", f$other) & grepl("[A-Z,a-z]", f$other)
	f$other[!i] <-  NA
	u <- unique(f[!is.na(f$other), c("fertilizer_type", "other")])
	u <- u[order(u[,1]), ]


	f$B_fertilizer <- f$Ca_fertilizer <- f$S_fertilizer <- f$Zn_fertilizer <- NA
	f$B_fertilizer[grepl("BORONAT", f$other)] <- 10
	f$Ca_fertilizer[grepl("BORONAT", f$other)] <- 9.5
	f$B_fertilizer[grepl("MEZCLA FISICA MAPEO", f$other)] <- .8
	f$Zn_fertilizer[grepl("MEZCLA FISICA MAPEO", f$other)] <- 1.5
	f$B_fertilizer[grepl("MEZCLA FISICA MAPEO", f$other)] <- .8
	f$Zn_fertilizer[grepl("MEZCLA FISICA MAPEO", f$other)] <- 1.5

	f$S_fertilizer[grepl("12", f$other)] <- 11
	f$S_fertilizer[grepl("DAS", f$other)] <- 24
	f$S_fertilizer[grepl("MAP", f$other)] <- 2
	f$S_fertilizer[grepl("SSP", f$other)] <- 12



#unique(grep("CLORURO", f$fertilizer_type, value=T))

#u <- sort(unique(f$fertilizer_type))
#unique(grep("SUPERFOSFATO", f$fertilizer_type, value=T))
#tab <- sort(table(f$fertilizer_type))
	
	f$N_fertilizer = f$amount * f$N_fertilizer / 100
	f$P_fertilizer = f$amount * f$P_fertilizer / 100
	f$K_fertilizer = f$amount * f$K_fertilizer / 100
	v1 <- c("hhid", "plot_id", "logbook", "pratice")

	f_amount <- aggregate(f[, c("N_fertilizer", "P_fertilizer", "K_fertilizer")], f[,v1], sum, na.rm=TRUE)
	f_splits <- aggregate(f[, c("N_fertilizer")], f[,v1], \(x) sum(!is.na(x)))
	names(f_splits)[ncol(f_splits)] <- "N_splits"
	f_dates <- aggregate(f[, c("fertilizer_date", "fertilizer_type", "fertilization_method")], f[,v1], \(i) paste(i, collapse=";"))

	fert <- merge(f_amount, f_splits, by=v1)
	fert <- merge(fert, f_dates, by=v1)

	d <- merge(d1, d2, by=c("hhid", "logbook"), all.y=TRUE)
	d <- merge(d, fert, by=v1, all.x=TRUE) 


##r4a: "LOGBOOK ID", "PLOT TYPE", "WINTER/SUMMER  SEASON", "HYDRIC REGIME", "INNOVATION/CONVENTIONAL_PRACTICES_ID", "PRACTICES_TYPE", "ID_APLICACIONESDEINSUMOS", "APPLICATION DATE", "APPLICATION TIME", "ACTIVITY PERFORMED", "DID YOU MAKE THIS APPLICATION TO CROP2?", "DID YOU MAKE THIS APPLICATION TO CROP3?", "DO YOU USE ANY TOOLS TO ADJUST YOUR FERTILIZER RATE?", "DO YOU APPLY FERTILIZER AT THE TIME OF PLANTING?", "DID YOU APPLY THE FERTILIZER AT APPROPRIATE TIMES? (USING CROP DEMAND CURVES):", "TYPE OF WEED TO BE CONTROLLED", "NUMBER OF PESTS YOU WANT TO CONTROL", "REASON FOR APPLICATION...24", "PEST TYPE (1)", "PEST TYPE (2)", "TYPE OF PEST (3)", "HOW DO I DETECT THE PEST?", "DID YOU USE FUNCTIONAL BIODIVERSITY IN THIS APPLICATION?", "ACTIVITY CARRIED OUT AS A USE OF FUNCTIONAL BIODIVERSITY.", "REASON FOR APPLICATION...31", "DISEASE PRESENTED", "DAMAGED PLANT PART", "PERCENTAGE (%) OF DAMAGE TO THE PLANT BY THE PEST OR DISEASE", "PERCENTAGE OF CROP DAMAGED (%)", "NUMBER OF PRODUCTS APPLIED", "PRODUCT REGISTRATION SEQUENCE", "PRODUCT CATEGORY", "KIND OF PRODUCT", "PRODUCT NAME APPLIED", "NITROGEN (N)", "PHOSPHORUS (P)", "POTASSIUM (K)", "OTHER NUTRIENTS (OPTIONAL)", "ACTIVE INGREDIENT", "AMOUNT OF PRODUCT APPLIED", "UNITS/HA", "PLACE OF APPLICATION", "LOG TYPE ID SECTION (FOREIGN KEY)"


	d4b <- data.frame(
		logbook = r4b[["LOGBOOK.ID"]],
		pratice = r4b[["PRACTICES_TYPE"]]
	)
##r4b: "LOGBOOK.ID", "INNOVATION/CONVENTIONAL_PRACTICES_ID", "PRACTICES_TYPE", "ID DE LA APLICACION (CLAVE PRIMARIA)", "APPLICATION DATE", "PRODUCT APPLIED_TYPE", "AMOUNT OF PRODUCT APPLIED (T_HA)", "PRODUCT MOISTURE IN THE APPLICATION", "APPLICATION METHOD"


	d4c <- data.frame(
		logbook = r4c[["LOGBOOK.ID"]],
		pratice = r4c[["PRACTICES_TYPE"]]
	)
##r4c: "LOGBOOK.ID", "INNOVATION/CONVENTIONAL_PRACTICES_ID", "PRACTICES_TYPE", "ACTIVITY", "ID DE LA APLICACION (CLAVE PRIMARIA)", "PRODUCT NAME APPLIED", "ACTIVE INGREDIENT", "AMOUNT OF PRODUCT APPLIED", "UNITS/HA"


	d5 <- data.frame(
		hhid = r5[["FARMER ID"]],
		plot = r5[["PLOT ID"]],
		logbook = r5[["LOGBOOK ID"]],
		pratice = r5[["PRACTICES_TYPE"]],		
		#adm1 = r5[["STATE"]],
		#adm2 = r5[["MUNICIPALITY"]],
		year = r5[["YEAR"]]
	)
##r5: "LOGBOOK ID", "PLOT TYPE", "WINTER/SUMER  SEASON", "HYDRIC REGIME", "INNOVATION/CONVENTIONAL_PRACTICES_ID", "PRACTICES_TYPE", "ID_IRRIGATION", "IRRIGATION TYPE", "IRRIGATION SYSTEM", "WATER SOURCE", "IRRIGATION DATE (GRAVITY,FOR SPRINKLING AND DRIP, FIRST DATE)", "IRRIGATION DATE (GRAVITY,FOR SPRINKLING AND DRIP, LAST DATE)", "IRRIGATION RUN (M) (GRAVITY)", "HOW MANY TIMES A DAY DO YOU DO THIS IRRIGATION? (DRIP)", "HOW MANY DAYS DID I CARRY OUT THIS IRRIGATION DURING THE PERIOD INDICATED IN THE START AND END DATES? (DRIP)", "IRRIGATION SYSTEM (SPRINKLER)", "HOW MANY TIMES A DAY DO YOU DO THIS IRRIGATION? (ASPERSION)", "HOW MANY DAYS DID I CARRY OUT THIS IRRIGATION DURING THE PERIOD INDICATED IN THE START AND END DATES? (ASPERSION)", "TOTAL WATER CONSUMPTION PER APPLICATION (LT)", "IRRIGATION TIME  (HR/HA)", "AVERAGE IRRIGATION TIME  (HR/HA)", "IRRIGATION SHEET (MM) IF YOU KNOW THE IRRIGATION SHEET, REPORT IT IMMEDIATELY, IF NOT, REPORT WATER CONSUMPTION IN THE NEXT QUESTION", "WATER CONSUMPTION (LT", "TOTAL WATER CONSUMPTION PER IRRIGATION (LT", "DO I USE BOMBS?", "PUMP POWER SOURCE", "LITERS OF FUEL CONSUMED (LT", "NUMBER OF PEOPLE WHO PARTICIPATED IN THE WORK", "DAYS USED", "DURATION OF WORK", "TIME IN WHICH THE ACTIVITY WAS COMPLETED (HR"


	d6 <- data.frame(
		hhid = r6[["FARMER ID"]],
		plot = r6[["PLOT ID"]],
		logbook = r6[["LOGBOOK.ID"]]
	)
	
# FARMER ID, PLOT ID, LOGBOOK.ID, PLOT TYPE, STATE, MUNICIPALITY, YEAR, WINTER/SUMMER  SEASON, HYDRIC REGIME, INNOVATION PRACTICES_ID, MECHANICAL SOIL PREPARATION....11, SOWING (ACTIVITY AND SEED)...12, SOIL ANALYSIS...13, WATER ANALYSIS...14, CULTURAL WORK AND PHYSICAL CONTROL OF WEEDS....15, APPLICATION OF INPUTS...16, IRRIGATION...17, HARVEST DONE BY HAND...18, MECHANICAL HARVEST...19, COMMERCIALIZATION...20, INDIRECT EXPENSES...21, TOTAL COSTS ($/HA)...22, INCOME ($/HA)...23, HARVESTED CROPS...24, CONVENTIONAL  PRACTICES_ID, MECHANICAL SOIL PREPARATION....26, SOWING (ACTIVITY AND SEED)...27, SOIL ANALYSIS...28, WATER ANALYSIS...29, CULTURAL WORK AND PHYSICAL CONTROL OF WEEDS....30, APPLICATION OF INPUTS...31, IRRIGATION...32, HARVEST DONE BY HAND...33, MECHANICAL HARVEST...34, COMMERCIALIZATION...35, INDIRECT EXPENSES...36, TOTAL COSTS ($/HA)...37, INCOME ($/HA)...38, HARVESTED CROPS...39
	
	
## 

	d$country <- "Mexico"
	d$on_farm <- TRUE
	d$is_survey <- d$pratice == "CONVENTIONAL"
	d$geo_from_source <- TRUE
	
	carobiner::write_files(path, meta, d)
}
