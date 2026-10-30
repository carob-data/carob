# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Development of LB-Resistant Potato Varieties Through Participatory Selection in Jumla, 2013

New LB-resistant potato varieties with improved yield, resistance, and nutritional traits identified and proposed for release through multistakeholder participatory selection (Participatory variety selection of varieties)
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/ZLUQTA"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "LI-BIRD; CIP",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = NA,
		
		# data_type can be e.g. "on-farm experiment", "survey", "compilation"
		data_type = NA,
		
		# treatment_vars has semi-colon separated variable names that represent the
		# treatments if the data is from an experiment. E.g. "N_fertilizer;P_fertilizer;K_fertilizer"
		treatment_vars = "",
		
		# response variables of interest such as yield, fwy_residue, disease incidence, etc. Do not include variables
		# that describe management for all treatments or other observations that were not related to the aim of 
		# the trial (e.g. the presence of a disease).
		response_vars = "", 

		# notes for the end-user
		notes = "",

		carob_contributor = "Your Name",
		carob_date = "2026-10-30",
		# The percentage of relevant variables that have been standardized (between 0 and 100%) 
		carob_completion = 0,	
		# The number of hours spent creating this script
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "01_data_F1_Select_crit.xlsx"]
	f2 <- ff[basename(ff) == "02_data_F2_02_data_F2 Clones_Select_Flowering.xlsx"]
	f3 <- ff[basename(ff) == "03_data_F3_Clones_Selec_Harvest.xlsx"]
	f4 <- ff[basename(ff) == "04_data_yield_F4_ Harvest_Mother.xlsx"]
	f5 <- ff[basename(ff) == "05_data_yield_F5_harvest_Baby.xlsx"]
	f6 <- ff[basename(ff) == "06_data_F8_Dormancy.xlsx"]
	f7 <- ff[basename(ff) == "07_data_Data_organoleptic_Mother.xlsx"]
	f8 <- ff[basename(ff) == "08_data_Data_organoleptic_Baby.xlsx"]
	f9 <- ff[basename(ff) == "09_Material_list.xlsx"]
	f10 <- ff[basename(ff) == "10_Data_dictionary.xlsx"]
	f11 <- ff[basename(ff) == "PTPV201203_JUMLA.xls"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	r4 <- carobiner::read.excel(f4)
	r5 <- carobiner::read.excel(f5)
	r6 <- carobiner::read.excel(f6)
	r7 <- carobiner::read.excel(f7)
	r8 <- carobiner::read.excel(f8)
	r9 <- carobiner::read.excel(f9)
	r10 <- carobiner::read.excel(f10)
	r11a <- carobiner::read.excel(f11, sheet="Minimal")
	r11b <- carobiner::read.excel(f11, sheet="Installation")
	r11c <- carobiner::read.excel(f11, sheet="Material List")
	r11d <- carobiner::read.excel(f11, sheet="Crop_management")
	r11e <- carobiner::read.excel(f11, sheet="Soil_analysis")
	r11f <- carobiner::read.excel(f11, sheet="Hobo_Data")
	r11g <- carobiner::read.excel(f11, sheet="Weather data")
	r11h <- carobiner::read.excel(f11, sheet="Var List")
	r11i <- carobiner::read.excel(f11, sheet="F1_Selec_Crit")
	r11j <- carobiner::read.excel(f11, sheet="F2 Clones_Select_Flowering")
	r11k <- carobiner::read.excel(f11, sheet="F3_Clones_Selec_Harvest")
	r11l <- carobiner::read.excel(f11, sheet="F4_ Harvest_Mother")
	r11m <- carobiner::read.excel(f11, sheet="F5_harvest_Baby")
	r11n <- carobiner::read.excel(f11, sheet="F6_Mother_Organoleptic")
	r11o <- carobiner::read.excel(f11, sheet="F7_Baby_Organoleptic")
	r11p <- carobiner::read.excel(f11, sheet="F8_Dormancy")
	r11q <- carobiner::read.excel(f11, sheet="F9_Clones_Select_Storage")
	r11r <- carobiner::read.excel(f11, sheet="Data_organoleptic_Mother")
	r11s <- carobiner::read.excel(f11, sheet="Data_organoleptic_Baby")
	r11t <- carobiner::read.excel(f11, sheet="Summary By Clone M&B")
	r11u <- carobiner::read.excel(f11, sheet="PCA_Mother_results")
	r11v <- carobiner::read.excel(f11, sheet="PCA_Baby_results")
	r11w <- carobiner::read.excel(f11, sheet="MSGLO_SF")
	r11x <- carobiner::read.excel(f11, sheet="BSGLO_SF")
	r11y <- carobiner::read.excel(f11, sheet="MTWP_HM")
	r11z <- carobiner::read.excel(f11, sheet="TTWP_HM")
	r11NA <- carobiner::read.excel(f11, sheet="TTYNA_HM")
	r11NA <- carobiner::read.excel(f11, sheet="TTYA_HM")
	r11NA <- carobiner::read.excel(f11, sheet="MTYNA_HM")
	r11NA <- carobiner::read.excel(f11, sheet="MTYA_HM")
	r11NA <- carobiner::read.excel(f11, sheet="MTWP_HB")
	r11NA <- carobiner::read.excel(f11, sheet="TTWP_HB")
	r11NA <- carobiner::read.excel(f11, sheet="TTYNA_HB")
	r11NA <- carobiner::read.excel(f11, sheet="TTYA_HB")
	r11NA <- carobiner::read.excel(f11, sheet="MTYNA_HB")
	r11NA <- carobiner::read.excel(f11, sheet="MTYA_HB")
	r11NA <- carobiner::read.excel(f11, sheet="Ranks by Clone")
	r11NA <- carobiner::read.excel(f11, sheet="PCA_Global_Results")
	r11NA <- carobiner::read.excel(f11, sheet="Mother&Baby Charts")

## select the variables of interest and assign them to the correct name

	d1 <- data.frame()
##r1: "ORD", "PHASE", "Identified criteria", "SCMEN", "OR_M", "SCWM", "OR_WM", "SGLOB", "PMEN", "PWM", "PGLOB"


	d2 <- data.frame(
		plot_id = r2[["PLOT"]],
		rep = r2[["REP"]]
	)
##r2: "INSTN", "Code", "MSM", "MSWM", "MSGLO", "BSM", "BSWM", "BSGLO", "Plant_Vigor", "PGH", "Flower", "SE"


	d3 <- data.frame(
		plot_id = r3[["PLOT"]],
		rep = r3[["REP"]]
	)
##r3: "INSTN", "Code", "MSM", "MSWM", "MSGLO", "BSM", "BSWM", "BSGLO"


	d4 <- data.frame(
		plot_id = r4[["PLOT"]],
		rep = r4[["REP"]]
	)
##r4: "INSTN", "Code", "NTP", "NPH", "PPH", "NMTP", "NNoMTP", "NMTPL", "MTWP", "NoMTWP", "TNTP", "TNTPL", "TTWP", "TTWPL", "TTYNA", "TTYA", "MTWPL", "MTYNA", "MTYA", "ATW"


	d5 <- data.frame(
		plot_id = r5[["PLOT"]],
		rep = r5[["REP"]]
	)
##r5: "INSTN", "Code", "NTP", "NPH", "PPH", "NMTP", "NNoMTP", "NMTPL", "MTWP", "NoMTWP", "TNTP", "TNTPL", "TTWP", "TTWPL", "TTYNA", "TTYA", "MTWPL", "MTYNA", "MTYA", "ATW"


	d6 <- data.frame(
		plot_id = r6[["PLOT"]],
		rep = r6[["REP"]]
	)
##r6: "INSTN", "NTE", "ITW", "FTWSPT", "PWL_SPT"


	d7 <- data.frame(
		soil_texture = tolower(r7[["texture"]])
	)
##r7: "INSTN", "PanelNo", "Sex", "appearance", "taste"


	d8 <- data.frame(
		soil_texture = tolower(r8[["texture"]])
	)
##r8: "INSTN", "PanelNo", "Sex", "appearance", "taste"


	d9 <- data.frame(
		variety = r9[["Clone or variety name"]]
	)
##r9: "Numeration", "Institutional number", "Code of clone", "Family Institutional number", "Female Institutional number", "Female code", "Male Institutional number", "Male code"


	d10 <- data.frame(
		crop = tolower(r10[["Crop"]])
	)
##r10: "numeration", "Variable ID", "Variable name", "Variable label", "Variable synonyms", "Variable description", "Context of use", "Growth stage", "Variable status", "Variable Xref", "Institution", "Scientist", "Date", "Language", "Trait ID", "Trait name", "Trait class", "Trait description", "Trait synonyms", "Main trait abbreviation", "Alternative trait abbreviations", "Entity", "Attribute", "Trait status", "Trait Xref", "Method ID", "Method name", "Method class", "Method description", "Formula", "Method reference", "Scale ID", "Scale name", "Scale class", "Decimal places", "Lower limit", "Upper limit", "Scale Xref"


	d11a <- data.frame()
##r11a: "Factor", "Value"


	d11b <- data.frame()
##r11b: "Factor", "Mother", "Baby 1", "Baby 2", "Baby 3", "Baby 4"


	d11c <- data.frame(
		variety = r11c[["Clone or variety name"]]
	)
##r11c: "Numeration", "Control", "Institutional number", "Code of clone", "Family Institutional number", "Female Institutional number", "Female code", "Male Institutional number", "Male code", "Seed source1", "References to silmultaneous trials", "References to previous trials", "Order of clone"


	d11d <- data.frame()
##r11d: "Intervention category", "Intervention type", "Date", "Responsable", "Operator", "Observations", "Active Ingredient", "Product concentration", "Dose of application", "Quantity used", "Unit Cost", "Total cost"


	d11e <- data.frame()
##r11e: "Variables", "Abbreviture", "Unit", "Data1", "Data2", "Data3", "Obs"


	d11f <- data.frame(
		year = r11f[["Year"]]
	)
##r11f: "Month", "Day", "Time", "Intervale Time", "Temperature °C", "Relative humidity (%)"


	d11g <- data.frame(
		year = r11g[["Year"]]
	)
##r11g: "Day", "Month", "Time", "Intervale time", "Rainfall (mm)", "Max. temperature", "Min. temperature", "Average temperature (°C)", "Relative humidity (%)", "Dew point (°C)", "Solar Radiation (w/m2)", "Temperature Soil 1", "Temperature Soil 2", "Temperature Soil 3", "Temperature Soil 4", "Walter Content 1", "Walter Content 2", "Walter Content 3", "Walter Content 4"


	d11h <- data.frame()
##r11h: "Factor.Variables", "Abbreviations", "Fieldbook", "Summarize", "Analyze", "Selection direction", "Selection weight"


	d11i <- data.frame()
##r11i: "ORD", "PHASE", "Identified criteria", "SCMEN", "OR_M", "SCWM", "OR_WM", "SGLOB", "PMEN", "PWM", "PGLOB"


	d11j <- data.frame(
		plot_id = r11j[["PLOT"]],
		rep = r11j[["REP"]]
	)
##r11j: "INSTN", "Code", "MSM", "MSWM", "MSGLO", "BSM", "BSWM", "BSGLO", "Plant_Vigor", "PGH", "Flower", "SE"


	d11k <- data.frame(
		plot_id = r11k[["PLOT"]],
		rep = r11k[["REP"]]
	)
##r11k: "INSTN", "Code", "MSM", "MSWM", "MSGLO", "BSM", "BSWM", "BSGLO", "Num_Stolon", "Leng_Stolon", "Tuber_Apper", "Tub_Unif", "Tub_Size"


	d11l <- data.frame(
		plot_id = r11l[["PLOT"]],
		rep = r11l[["REP"]]
	)
##r11l: "INSTN", "Code", "NTP", "NPH", "PPH", "NMTP", "NNoMTP", "NMTPL", "MTWP", "NoMTWP", "TNTP", "TNTPL", "TTWP", "TTWPL", "TTYNA", "TTYA", "MTWPL", "MTYNA", "MTYA", "ATW"


	d11m <- data.frame(
		plot_id = r11m[["PLOT"]],
		rep = r11m[["REP"]]
	)
##r11m: "INSTN", "Code", "NTP", "NPH", "PPH", "NMTP", "NNoMTP", "NMTPL", "MTWP", "NoMTWP", "TNTP", "TNTPL", "TTWP", "TTWPL", "TTYNA", "TTYA", "MTWPL", "MTYNA", "MTYA", "ATW"


	d11n <- data.frame()
##r11n: "Panel No:", "1", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23"


	d11o <- data.frame()
##r11o: "Panel No:", "1", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10", "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20", "...21", "...22", "...23"


	d11p <- data.frame(
		plot_id = r11p[["PLOT"]],
		rep = r11p[["REP"]]
	)
##r11p: "INSTN", "STYPE", "NTE", "ITW", "NTSP_EV1", "AVSNT_EV1", "PTYSP_EV1", "TAPH_EV1", "NTSP_EV2", "AVSNT_EV2", "PTYSP_EV2", "TAPH_EV2", "FTWSPT", "FTWRSPT", "PWL_SPT", "PWL_RSPT"


	d11q <- data.frame(
		plot_id = r11q[["PLOT"]],
		rep = r11q[["REP"]]
	)
##r11q: "INSTN", "Code", "STYPE", "SCMEN", "SCWM", "SGLOB", "PMEN", "PWM", "PGLOB", "...12", "...13", "...14"


	d11r <- data.frame(
		soil_texture = tolower(r11r[["texture"]])
	)
##r11r: "INSTN", "PanelNo", "Sex", "appearance", "taste"


	d11s <- data.frame(
		soil_texture = tolower(r11s[["texture"]])
	)
##r11s: "INSTN", "PanelNo", "Sex", "appearance", "taste"


	d11t <- data.frame(
		soil_texture = tolower(r11t[["texture_n_mother"]])
	)
##r11t: "INSTN", "PPH_n_mother", "PPH_mean_mother", "PPH_sd_mother", "PPH_n_baby", "PPH_mean_baby", "PPH_sd_baby", "TNTP_n_mother", "TNTP_mean_mother", "TNTP_sd_mother", "TNTP_n_baby", "TNTP_mean_baby", "TNTP_sd_baby", "TNTPL_n_mother", "TNTPL_mean_mother", "TNTPL_sd_mother", "TNTPL_n_baby", "TNTPL_mean_baby", "TNTPL_sd_baby", "TTWP_n_mother", "TTWP_mean_mother", "TTWP_sd_mother", "TTWP_n_baby", "TTWP_mean_baby", "TTWP_sd_baby", "TTYNA_n_mother", "TTYNA_mean_mother", "TTYNA_sd_mother", "TTYNA_n_baby", "TTYNA_mean_baby", "TTYNA_sd_baby", "TTYA_n_mother", "TTYA_mean_mother", "TTYA_sd_mother", "TTYA_n_baby", "TTYA_mean_baby", "TTYA_sd_baby", "MTWP_n_mother", "MTWP_mean_mother", "MTWP_sd_mother", "MTWP_n_baby", "MTWP_mean_baby", "MTWP_sd_baby", "MTYA_n_mother", "MTYA_mean_mother", "MTYA_sd_mother", "MTYA_n_baby", "MTYA_mean_baby", "MTYA_sd_baby", "MTYNA_n_mother", "MTYNA_mean_mother", "MTYNA_sd_mother", "MTYNA_n_baby", "MTYNA_mean_baby", "MTYNA_sd_baby", "appearance_n_mother", "appearance_mean_mother", "appearance_sd_mother", "appearance_n_baby", "appearance_mean_baby", "appearance_sd_baby", "taste_n_mother", "taste_mean_mother", "taste_sd_mother", "taste_n_baby", "taste_mean_baby", "taste_sd_baby", "texture_mean_mother", "texture_sd_mother", "texture_n_baby", "texture_mean_baby", "texture_sd_baby", "MSGLO_n_SF", "MSGLO_mean_SF", "MSGLO_sd_SF", "BSGLO_n_SF", "BSGLO_mean_SF", "BSGLO_sd_SF", "MSGLO_n_SH", "MSGLO_mean_SH", "MSGLO_sd_SH", "BSGLO_n_SH", "BSGLO_mean_SH", "BSGLO_sd_SH"


	d11u <- data.frame()
##r11u: "Standard deviations:"


	d11v <- data.frame()
##r11v: "Standard deviations:"


	d11w <- data.frame()
##r11w: "################################################################################"


	d11x <- data.frame()
##r11x: "################################################################################"


	d11y <- data.frame()
##r11y: "################################################################################"


	d11z <- data.frame()
##r11z: "################################################################################"


	d11NA <- data.frame()
##r11NA: "################################################################################"


	d11NA <- data.frame()
##r11NA: "################################################################################"


	d11NA <- data.frame()
##r11NA: "################################################################################"


	d11NA <- data.frame()
##r11NA: "################################################################################"


	d11NA <- data.frame()
##r11NA: "################################################################################"


	d11NA <- data.frame()
##r11NA: "################################################################################"


	d11NA <- data.frame()
##r11NA: "################################################################################"


	d11NA <- data.frame()
##r11NA: "################################################################################"


	d11NA <- data.frame()
##r11NA: "################################################################################"


	d11NA <- data.frame()
##r11NA: "################################################################################"


	d11NA <- data.frame(
		soil_texture = tolower(r11NA[["texture_mean_mother"]])
	)
##r11NA: "INSTN", "TTWP_mean_mother", "TTYNA_mean_mother", "TTYA_mean_mother", "MTWP_mean_mother", "MTYA_mean_mother", "MTYNA_mean_mother", "appearance_mean_mother", "taste_mean_mother", "TTWP_mean_baby", "TTYNA_mean_baby", "TTYA_mean_baby", "MTWP_mean_baby", "MTYA_mean_baby", "MTYNA_mean_baby", "appearance_mean_baby", "taste_mean_baby", "texture_mean_baby", "TTWP_Rank_mother", "TTYNA_Rank_mother", "TTYA_Rank_mother", "MTWP_Rank_mother", "MTYA_Rank_mother", "MTYNA_Rank_mother", "appearance_Rank_mother", "taste_Rank_mother", "texture_Rank_mother", "TTWP_Rank_baby", "TTYNA_Rank_baby", "TTYA_Rank_baby", "MTWP_Rank_baby", "MTYA_Rank_baby", "MTYNA_Rank_baby", "appearance_Rank_baby", "taste_Rank_baby", "texture_Rank_baby", "RSS", "SWD", "EIM"


	d11NA <- data.frame()
##r11NA: "Standard deviations:"


	d11NA <- data.frame()


## separate individual trials. For example trials in different locations or years. 
## do _not_ separate by treatments within a trial. For a survey, each row gets a unique trial_id
	d$trial_id <- as.character(as.integer(as.factor( ____ )))
	
## about the data (TRUE/FALSE)
	d$on_farm <- 
	d$is_survey <- 
	d$irrigated <-
	
## crop rotation. If available, add all crops, including "d$crop". Use an underscore for intercrops 
    d$crop_rotation <- "crop1;crop2;crop3_crop4"
	
## each site must have corresponding longitude and latitude
## if the raw data do not provide them you can estimate them from the location/adm data 
## see carobiner::geocode
	d$longitude <- 
	d$latitude <- 
# are the coordinates from the source (data/publication) or estimated by you?	
	d$geo_from_source <- TRUE/FALSE


## time can be year ("2023", four characters), year-month ("2023-07", 7 characters) or date ("2023-07-21", 10 characters).
## if dates come as character values, you can use as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- as.character(as.Date(   ))
	d$harvest_date  <- as.character(as.Date(    ))

### Fertilizers 
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
   d$P_fertilizer <- 
   d$K_fertilizer <-
   d$N_fertilizer <- 
   d$S_fertilizer <- 
   d$lime <- 
## normalize names 
   d$fertlizer_type <- 

## for legumes   
   d$inoculated <- TRUE or FALSE
   d$inoculant <- "name of inoculant"
   
### in general, add comments to your script if computations are
### based on information gleaned from metadata, a publication, 
### or when they are not immediately obvious for other reasons

### Yield

	yield <- r$yield_tonha * 1000
	#what plant part does yield refer to?
	d$yield_part <- "tubers"
	d$yield_moisture <- r$moisture * 100

#NOTE: yield is the _fresh weight_ production (kg/ha) of the "yield_part 
# Also record fresh and/or dry weight production of other organs (or "residue" or "total")
# if the data allow for that 

	d$fwy_storage <- r$yield_tonha * 1000
	d$dmy_storage <- (1-r$moisture) * r$yield_tonha * 1000
	d$dmy_totat <- r$dry_biomass
	
# all scripts must end like this
	carobiner::write_files(path, meta, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)

