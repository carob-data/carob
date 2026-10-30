# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Evaluation of Potato Clones Through Farmer Selection in Buthhang

A select group of 8 potato clones will be evaluated through a Farmer Participatory Variety Selection (PVS) in Buthhang during the 2009 campaign. Yield components and post-harvest traits will be determined
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/M97ZQS"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP-SWCA",
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
	f2 <- ff[basename(ff) == "02_data_F2 Clones_Select_Flowering.xlsx"]
	f3 <- ff[basename(ff) == "02_data_F2_Select_crit.xlsx"]
	f4 <- ff[basename(ff) == "03_data_F3_Clones_Selec_Harvest.xlsx"]
	f5 <- ff[basename(ff) == "04_data_yield_F4_ Harvest_Mother.xlsx"]
	f6 <- ff[basename(ff) == "05_data_yield_F5_harvest_Baby.xlsx"]
	f7 <- ff[basename(ff) == "06_data_F9_Clones_Select_Storage.xlsx"]
	f8 <- ff[basename(ff) == "07_data_Data_organoleptic_Mother.xlsx"]
	f9 <- ff[basename(ff) == "08_data_Data_organoleptic_Baby.xlsx"]
	f10 <- ff[basename(ff) == "09_Material_list.xlsx"]
	f11 <- ff[basename(ff) == "10_Data_dictionary.xlsx"]
	f12 <- ff[basename(ff) == "PTPV200908_BUMTHANG.xls"]

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
	r11 <- carobiner::read.excel(f11)
	r12a <- carobiner::read.excel(f12, sheet="Minimal")
	r12b <- carobiner::read.excel(f12, sheet="Installation")
	r12c <- carobiner::read.excel(f12, sheet="Material List")
	r12d <- carobiner::read.excel(f12, sheet="Crop_management")
	r12e <- carobiner::read.excel(f12, sheet="Soil_analysis")
	r12f <- carobiner::read.excel(f12, sheet="Hobo_Data")
	r12g <- carobiner::read.excel(f12, sheet="Weather data")
	r12h <- carobiner::read.excel(f12, sheet="Var List")
	r12i <- carobiner::read.excel(f12, sheet="F1_Selec_Crit")
	r12j <- carobiner::read.excel(f12, sheet="F2 Clones_Select_Flowering")
	r12k <- carobiner::read.excel(f12, sheet="F3_Clones_Selec_Harvest")
	r12l <- carobiner::read.excel(f12, sheet="F4_ Harvest_Mother")
	r12m <- carobiner::read.excel(f12, sheet="F5_harvest_Baby")
	r12n <- carobiner::read.excel(f12, sheet="F6_Mother_Organoleptic")
	r12o <- carobiner::read.excel(f12, sheet="F7_Baby_Organoleptic")
	r12p <- carobiner::read.excel(f12, sheet="F8_Dormancy")
	r12q <- carobiner::read.excel(f12, sheet="F9_Clones_Select_Storage")
	r12r <- carobiner::read.excel(f12, sheet="Data_organoleptic_Mother")
	r12s <- carobiner::read.excel(f12, sheet="Data_organoleptic_Baby")
	r12t <- carobiner::read.excel(f12, sheet="Summary By Clone M&B")
	r12u <- carobiner::read.excel(f12, sheet="PCA_Mother_results")
	r12v <- carobiner::read.excel(f12, sheet="PCA_Baby_results")
	r12w <- carobiner::read.excel(f12, sheet="MSGLO_SF")
	r12x <- carobiner::read.excel(f12, sheet="BSGLO_SF")
	r12y <- carobiner::read.excel(f12, sheet="MSGLO_SH")
	r12z <- carobiner::read.excel(f12, sheet="BSGLO_SH")
	r12NA <- carobiner::read.excel(f12, sheet="MTWP_HM")
	r12NA <- carobiner::read.excel(f12, sheet="TTWP_HM")
	r12NA <- carobiner::read.excel(f12, sheet="TTYNA_HM")
	r12NA <- carobiner::read.excel(f12, sheet="TTYA_HM")
	r12NA <- carobiner::read.excel(f12, sheet="MTYNA_HM")
	r12NA <- carobiner::read.excel(f12, sheet="MTYA_HM")
	r12NA <- carobiner::read.excel(f12, sheet="MTWP_HB")
	r12NA <- carobiner::read.excel(f12, sheet="TTWP_HB")
	r12NA <- carobiner::read.excel(f12, sheet="TTYNA_HB")
	r12NA <- carobiner::read.excel(f12, sheet="TTYA_HB")
	r12NA <- carobiner::read.excel(f12, sheet="MTYNA_HB")
	r12NA <- carobiner::read.excel(f12, sheet="MTYA_HB")
	r12NA <- carobiner::read.excel(f12, sheet="Ranks by Clone")
	r12NA <- carobiner::read.excel(f12, sheet="PCA_Global_Results")
	r12NA <- carobiner::read.excel(f12, sheet="Mother&Baby Charts")

## select the variables of interest and assign them to the correct name

	d1 <- data.frame()
##r1: "ORD", "PHASE", "Identified criteria", "SCMEN", "OR_M", "SCWM", "OR_WM", "SGLOB", "PMEN", "PWM", "PGLOB"


	d2 <- data.frame(
		plot_id = r2[["PLOT"]],
		rep = r2[["REP"]],
		country = r2[["Country"]],
		adm1 = carobiner::fix_name(r2[["Admin1"]], "title"),
		adm2 = carobiner::fix_name(r2[["Admin2"]], "title")
	)
##r2: "INSTN", "Locality", "NTP", "NPE", "NPH", "PPH", "NNoMTP", "NMTP", "TNTP", "NoMTWP", "TTWP", "TTWPL", "TTYA", "TTYNA", "MTWP", "MTWPL", "MTYA", "MTYNA", "ATW", "ATMW", "AVDM"


	d3 <- data.frame(
		plot_id = r3[["PLOT"]],
		rep = r3[["REP"]]
	)
##r3: "INSTN", "Code", "MSM", "MSWM", "MSGLO", "BSM", "BSWM", "BSGLO"


	d4 <- data.frame(
		plot_id = r4[["PLOT"]],
		rep = r4[["REP"]]
	)
##r4: "INSTN", "Code", "MSM", "MSWM", "MSGLO", "BSM", "BSWM", "BSGLO"


	d5 <- data.frame(
		plot_id = r5[["PLOT"]],
		rep = r5[["REP"]]
	)
##r5: "INSTN", "Code", "NTP", "NPH", "PPH", "NMTP", "NNoMTP", "NMTPL", "MTWP", "NoMTWP", "TNTP", "TNTPL", "TTWP", "TTWPL", "TTYNA", "TTYA", "MTWPL", "MTYNA", "MTYA", "ATW"


	d6 <- data.frame(
		plot_id = r6[["PLOT"]],
		rep = r6[["REP"]]
	)
##r6: "INSTN", "Code", "NTP", "NPH", "PPH", "NMTP", "NNoMTP", "NMTPL", "MTWP", "NoMTWP", "TNTP", "TNTPL", "TTWP", "TTWPL", "TTYNA", "TTYA", "MTWPL", "MTYNA", "MTYA", "ATW"


	d7 <- data.frame(
		plot_id = r7[["PLOT"]],
		rep = r7[["REP"]]
	)
##r7: "INSTN", "Code", "STYPE", "SCMEN", "SCWM", "SGLOB", "PMEN", "PWM", "PGLOB"


	d8 <- data.frame(
		soil_texture = tolower(r8[["texture"]])
	)
##r8: "INSTN", "PanelNo", "Sex", "appearance", "taste"


	d9 <- data.frame(
		soil_texture = tolower(r9[["texture"]])
	)
##r9: "INSTN", "PanelNo", "Sex", "appearance", "taste"


	d10 <- data.frame(
		variety = r10[["Clone or variety name"]]
	)
##r10: "Numeration", "Institutional number", "Female Institutional number", "Female code", "Male Institutional number", "References to previous trials"


	d11 <- data.frame(
		crop = tolower(r11[["Crop"]])
	)
##r11: "numeration", "Variable ID", "Variable name", "Variable label", "Variable synonyms", "Variable description", "Context of use", "Growth stage", "Variable status", "Variable Xref", "Institution", "Scientist", "Date", "Language", "Trait ID", "Trait name", "Trait class", "Trait description", "Trait synonyms", "Main trait abbreviation", "Alternative trait abbreviations", "Entity", "Attribute", "Trait status", "Trait Xref", "Method ID", "Method name", "Method class", "Method description", "Formula", "Method reference", "Scale ID", "Scale name", "Scale class", "Decimal places", "Lower limit", "Upper limit", "Scale Xref"


	d12a <- data.frame()
##r12a: "Factor", "Value"


	d12b <- data.frame()
##r12b: "Factor", "Mother", "Baby 1", "Baby 2", "Baby 3"


	d12c <- data.frame(
		variety = r12c[["Clone or variety name"]]
	)
##r12c: "Numeration", "Control", "Institutional number", "Code of clone", "Family Institutional number", "Female Institutional number", "Female code", "Male Institutional number", "Male code", "Seed source1", "References to silmultaneous trials", "References to previous trials", "Population Group"


	d12d <- data.frame()
##r12d: "Intervention category", "Intervention type", "Date", "Responsable", "Operator", "Observations", "Active Ingredient", "Product concentration", "Dose of application", "Quantity used", "Unit Cost", "Total cost"


	d12e <- data.frame()
##r12e: "Variables", "Abbreviture", "Unit", "Data1", "Data2"


	d12f <- data.frame(
		year = r12f[["Year"]]
	)
##r12f: "Month", "Day", "Time", "Intervale Time", "Temperature °C", "Relative humidity (%)"


	d12g <- data.frame(
		year = r12g[["Year"]]
	)
##r12g: "Day", "Month", "Time", "Intervale time", "Rainfall (mm)", "Average temperature (°C)", "Relative humidity (%)", "Dew point (°C)", "Solar Radiation (w/m2)", "Temperature Soil 1", "Temperature Soil 2", "Temperature Soil 3", "Temperature Soil 4", "Water Content 1", "Water Content 2", "Water Content 3", "Water Content 4"


	d12h <- data.frame()
##r12h: "Factor.Variables", "Abbreviations", "Fieldbook", "Summarize", "Analyze", "Selection direction", "Selection weight"


	d12i <- data.frame()
##r12i: "ORD", "PHASE", "Identified criteria", "SCMEN", "OR_M", "SCWM", "OR_WM", "SGLOB", "PMEN", "PWM", "PGLOB"


	d12j <- data.frame(
		plot_id = r12j[["PLOT"]],
		rep = r12j[["REP"]]
	)
##r12j: "INSTN", "Code", "MSM", "MSWM", "MSGLO", "BSM", "BSWM", "BSGLO", "Plant_Vigor", "PGH", "Flower", "SE"


	d12k <- data.frame(
		plot_id = r12k[["PLOT"]],
		rep = r12k[["REP"]]
	)
##r12k: "INSTN", "Code", "MSM", "MSWM", "MSGLO", "BSM", "BSWM", "BSGLO", "Num_Stolon", "Leng_Stolon", "Tuber_Apper", "Tub_Unif", "Tub_Size"


	d12l <- data.frame(
		plot_id = r12l[["PLOT"]],
		rep = r12l[["REP"]]
	)
##r12l: "INSTN", "Code", "NTP", "NPH", "PPH", "NMTP", "NNoMTP", "NMTPL", "MTWP", "NoMTWP", "TNTP", "TNTPL", "TTWP", "TTWPL", "TTYNA", "TTYA", "MTWPL", "MTYNA", "MTYA", "ATW"


	d12m <- data.frame(
		plot_id = r12m[["PLOT"]],
		rep = r12m[["REP"]]
	)
##r12m: "INSTN", "Code", "NTP", "NPH", "PPH", "NMTP", "NNoMTP", "NMTPL", "MTWP", "NoMTWP", "TNTP", "TNTPL", "TTWP", "TTWPL", "TTYNA", "TTYA", "MTWPL", "MTYNA", "MTYA", "ATW"


	d12n <- data.frame()
##r12n: "PanelNo:", "1", "...3", "...4", "...5", "...6", "...7"


	d12o <- data.frame()
##r12o: "Panel No :", "1", "...3", "...4", "...5", "...6", "...7"


	d12p <- data.frame(
		plot_id = r12p[["PLOT"]],
		rep = r12p[["REP"]]
	)
##r12p: "INSTN", "ITW", "STYPE", "NTE", "NTSP_EV1", "AVSNT_EV1", "PTYSP_EV1", "TAPH_EV1", "NTSP_EV2", "AVSNT_EV2", "PTYSP_EV2", "TAPH_EV2", "FTWSPT", "FTWRSPT", "PWL_SPT", "PWL_RSPT"


	d12q <- data.frame(
		plot_id = r12q[["PLOT"]],
		rep = r12q[["REP"]]
	)
##r12q: "INSTN", "Code", "STYPE", "SCMEN", "SCWM", "SGLOB", "PMEN", "PWM", "PGLOB"


	d12r <- data.frame(
		soil_texture = tolower(r12r[["texture"]])
	)
##r12r: "INSTN", "PanelNo", "Sex", "appearance", "taste"


	d12s <- data.frame(
		soil_texture = tolower(r12s[["texture"]])
	)
##r12s: "INSTN", "PanelNo", "Sex", "appearance", "taste"


	d12t <- data.frame(
		soil_texture = tolower(r12t[["texture_n_mother"]])
	)
##r12t: "INSTN", "PPH_n_mother", "PPH_mean_mother", "PPH_sd_mother", "PPH_n_baby", "PPH_mean_baby", "PPH_sd_baby", "TNTP_n_mother", "TNTP_n_baby", "TNTP_mean_baby", "TNTP_sd_baby", "TNTPL_n_mother", "TNTPL_n_baby", "TNTPL_mean_baby", "TNTPL_sd_baby", "TTWP_n_mother", "TTWP_mean_mother", "TTWP_sd_mother", "TTWP_n_baby", "TTWP_mean_baby", "TTWP_sd_baby", "TTYNA_n_mother", "TTYNA_mean_mother", "TTYNA_sd_mother", "TTYNA_n_baby", "TTYNA_mean_baby", "TTYNA_sd_baby", "TTYA_n_mother", "TTYA_mean_mother", "TTYA_sd_mother", "TTYA_n_baby", "TTYA_mean_baby", "TTYA_sd_baby", "MTWP_n_mother", "MTWP_mean_mother", "MTWP_sd_mother", "MTWP_n_baby", "MTWP_mean_baby", "MTWP_sd_baby", "MTYA_n_mother", "MTYA_mean_mother", "MTYA_sd_mother", "MTYA_n_baby", "MTYA_mean_baby", "MTYA_sd_baby", "MTYNA_n_mother", "MTYNA_mean_mother", "MTYNA_sd_mother", "MTYNA_n_baby", "MTYNA_mean_baby", "MTYNA_sd_baby", "appearance_n_mother", "appearance_mean_mother", "appearance_sd_mother", "appearance_n_baby", "appearance_mean_baby", "appearance_sd_baby", "taste_n_mother", "taste_mean_mother", "taste_sd_mother", "taste_n_baby", "taste_mean_baby", "taste_sd_baby", "texture_mean_mother", "texture_sd_mother", "texture_n_baby", "texture_mean_baby", "texture_sd_baby", "MSGLO_n_SF", "MSGLO_mean_SF", "MSGLO_sd_SF", "BSGLO_n_SF", "BSGLO_mean_SF", "BSGLO_sd_SF", "MSGLO_n_SH", "MSGLO_mean_SH", "MSGLO_sd_SH", "BSGLO_n_SH", "BSGLO_mean_SH", "BSGLO_sd_SH"


	d12u <- data.frame()
##r12u: "Standard deviations:"


	d12v <- data.frame()
##r12v: "Standard deviations:"


	d12w <- data.frame()
##r12w: "################################################################################"


	d12x <- data.frame()
##r12x: "################################################################################"


	d12y <- data.frame()
##r12y: "################################################################################"


	d12z <- data.frame()
##r12z: "################################################################################"


	d12NA <- data.frame()
##r12NA: "################################################################################"


	d12NA <- data.frame()
##r12NA: "################################################################################"


	d12NA <- data.frame()
##r12NA: "################################################################################"


	d12NA <- data.frame()
##r12NA: "################################################################################"


	d12NA <- data.frame()
##r12NA: "################################################################################"


	d12NA <- data.frame()
##r12NA: "################################################################################"


	d12NA <- data.frame()
##r12NA: "################################################################################"


	d12NA <- data.frame()
##r12NA: "################################################################################"


	d12NA <- data.frame()
##r12NA: "################################################################################"


	d12NA <- data.frame()
##r12NA: "################################################################################"


	d12NA <- data.frame()
##r12NA: "################################################################################"


	d12NA <- data.frame()
##r12NA: "################################################################################"


	d12NA <- data.frame(
		soil_texture = tolower(r12NA[["texture_mean_mother"]])
	)
##r12NA: "INSTN", "TTWP_mean_mother", "TTYNA_mean_mother", "TTYA_mean_mother", "MTWP_mean_mother", "MTYA_mean_mother", "MTYNA_mean_mother", "appearance_mean_mother", "taste_mean_mother", "TTWP_mean_baby", "TTYNA_mean_baby", "TTYA_mean_baby", "MTWP_mean_baby", "MTYA_mean_baby", "MTYNA_mean_baby", "appearance_mean_baby", "taste_mean_baby", "texture_mean_baby", "TTWP_Rank_mother", "TTYNA_Rank_mother", "TTYA_Rank_mother", "MTWP_Rank_mother", "MTYA_Rank_mother", "MTYNA_Rank_mother", "appearance_Rank_mother", "taste_Rank_mother", "texture_Rank_mother", "TTWP_Rank_baby", "TTYNA_Rank_baby", "TTYA_Rank_baby", "MTWP_Rank_baby", "MTYA_Rank_baby", "MTYNA_Rank_baby", "appearance_Rank_baby", "taste_Rank_baby", "texture_Rank_baby", "RSS", "SWD", "EIM"


	d12NA <- data.frame()
##r12NA: "Standard deviations:"


	d12NA <- data.frame()


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

