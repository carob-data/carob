# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Replication data for: Demand of high quality seed in Ecuador

The use of high quality seed is one of the current strategies to increase seed and food security in the highlands of Ecuador. Unfortunately, poor information about farmers' demand of high quality seed limits the implementation of this strategy. For this reason, this study aimed to investigate the characteristics of the demand of high quality seed in the country. To do this, 150 potato producers were surveyed using the rapid market assessment in five Ecuadorian provinces: Cañar, Carchi, Chimborazo, Cotopaxi, and Tungurahua (data in 'survey_answers1' and 'survey_answers2' sheet). Also, 13 interviews were carried out with experts that are involved in the seed value chain (data in the sheets referring to interviews and in the report associated to this study).
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/P3/ZP9LYT"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "Oficinas Para Estudio del Agro CIA. Ltda.; CIP",
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
	

	f1 <- ff[basename(ff) == "Dictionary.xlsx"]
	f2 <- ff[basename(ff) == "Interviews.xls"]
	f3 <- ff[basename(ff) == "Surveys.xls"]

	r1 <- carobiner::read.excel(f1)
	r2a <- carobiner::read.excel(f2, sheet="interview1")
	r2b <- carobiner::read.excel(f2, sheet="interview2")
	r2c <- carobiner::read.excel(f2, sheet="interview3")
	r2d <- carobiner::read.excel(f2, sheet="interview4")
	r2e <- carobiner::read.excel(f2, sheet="interview5")
	r2f <- carobiner::read.excel(f2, sheet="interview6")
	r2g <- carobiner::read.excel(f2, sheet="interview8")
	r2h <- carobiner::read.excel(f2, sheet="interview7")
	r2i <- carobiner::read.excel(f2, sheet="interview9")
	r2j <- carobiner::read.excel(f2, sheet="interview10")
	r2k <- carobiner::read.excel(f2, sheet="interview11")
	r2l <- carobiner::read.excel(f2, sheet="interview12")
	r2m <- carobiner::read.excel(f2, sheet="interview13")
	r3a <- carobiner::read.excel(f3, sheet="survey_answers1")
	r3b <- carobiner::read.excel(f3, sheet="survey_answers2")

## select the variables of interest and assign them to the correct name

	d1 <- data.frame()
##r1: "Abbreviation", "Description", "Unit"


	d2a <- data.frame()


	d2b <- data.frame()


	d2c <- data.frame()


	d2d <- data.frame()


	d2e <- data.frame()


	d2f <- data.frame()


	d2g <- data.frame()


	d2h <- data.frame()


	d2i <- data.frame()


	d2j <- data.frame()


	d2k <- data.frame()


	d2l <- data.frame()


	d2m <- data.frame()


	d3a <- data.frame(
		adm1 = carobiner::fix_name(r3a[["a5_estado"]], "title"),
		adm2 = carobiner::fix_name(r3a[["c1_provin"]], "title")
	)
##r3a: "X1", "X2", "X3", "a2_edad", "a3_sexo", "a4_relaci", "a6_edu", "a7_agro", "a7_artes", "a7_serv", "a7_comer", "a7_const", "a7_dom", "a7_otro", "a8_idiom", "a9_orga", "a10_orga2", "a11_contra", "a12_contra2", "b1_perso", "b2_niño5", "b3_niño1", "b4_ingreso", "c2_canton", "c3_parroq", "c4_comun", "c5_altura", "c6_inunda", "c6_sequia", "c6_graniz", "c6_helada", "c6_otro", "c8_papa", "tipologia", "c9_riego", "c10_riego2", "c11_parc1", "c11_parc2", "c11_parc3", "c11_parc4", "c11_parc5", "c12_agua", "c13_parc1", "c13_parc2", "c13_parc3", "c13_parc4", "c13_parc5", "d1v1_parc1", "d2v1_parc1", "d3v1_parc1", "d4v1_parc1", "d5v1_parc1", "d6v1_parc1", "d7v1_parc1", "d8v1_parc1", "d9ACv1_parc1", "d9Cv1_parc1", "d9SPv1_parc1", "d9SVv1_parc1", "d10v1_parc1", "d11v1_parc1", "d12v1_parc1", "d1v2_parc1", "d2v2_parc1", "d3v2_parc1", "d4v2_parc1", "d5v2_parc1", "d6v2_parc1", "d7v2_parc1", "d8v2_parc1", "d9ACv2_parc1", "d9Cv2_parc1", "d9SPv2_parc1", "d9SVv2_parc1", "d10v2_parc1", "d11v2_parc1", "d12v2_parc1", "d1v3_parc1", "d2v3_parc1", "d3v3_parc1", "d4v3_parc1", "d5v3_parc1", "d6v3_parc1", "d7v3_parc1", "d8v3_parc1", "d9ACv3_parc1", "d9Cv3_parc1", "d9SPv3_parc1", "d9SVv3_parc1", "d10v3_parc1", "d11v3_parc1", "d12v3_parc1", "d13_parc1", "d1v1_parc2", "d2v1_parc2", "d3v1_parc2", "d4v1_parc2", "d5v1_parc2", "d6v1_parc2", "d7v1_parc2", "d8v1_parc2", "d9ACv1_parc2", "d9Cv1_parc2", "d9SPv1_parc2", "d9SVv1_parc2", "d10v1_parc2", "d11v1_parc2", "d12v1_parc2", "d1v2_parc2", "d2v2_parc2", "d3v2_parc2", "d4v2_parc2", "d5v2_parc2", "d6v2_parc2", "d7v2_parc2", "d8v2_parc2", "d9ACv2_parc2", "d9Cv2_parc2", "d9SPv2_parc2", "d9SVv2_parc2", "d10v2_parc2", "d11v2_parc2", "d12v2_parc2", "d1v3_parc2", "d2v3_parc2", "d3v3_parc2", "d4v3_parc2", "d5v3_parc2", "d6v3_parc2", "d7v3_parc2", "d8v3_parc2", "d9ACv3_parc2", "d9Cv3_parc2", "d9SPv3_parc2", "d9SVv3_parc2", "d10v3_parc2", "d11v3_parc2", "d12v3_parc2", "d13_parc2", "d1v1_parc3", "d2v1_parc3", "d3v1_parc3", "d4v1_parc3", "d5v1_parc3", "d6v1_parc3", "d7v1_parc3", "d8v1_parc3", "d9ACv1_parc3", "d9Cv1_parc3", "d9SPv1_parc3", "d9SVv1_parc3", "d10v1_parc3", "d11v1_parc3", "d12v1_parc3", "d1v2_parc3", "d2v2_parc3", "d3v2_parc3", "d4v2_parc3", "d5v2_parc3", "d6v2_parc3", "d7v2_parc3", "d8v2_parc3", "d9ACv2_parc3", "d9Cv2_parc3", "d9SPv2_parc3", "d9SVv2_parc3", "d10v2_parc3", "d11v2_parc3", "d12v2_parc3", "d1v3_parc3", "d2v3_parc3", "d3v3_parc3", "d4v3_parc3", "d5v3_parc3", "d6v3_parc3", "d7v3_parc3", "d8v3_parc3", "d9ACv3_parc3", "d9Cv3_parc3", "d9SPv3_parc3", "d9SVv3_parc3", "d10v3_parc3", "d11v3_parc3", "d12v3_parc3", "d13_parc3", "d1v1_parc4", "d2v1_parc4", "d3v1_parc4", "d4v1_parc4", "d5v1_parc4", "d6v1_parc4", "d7v1_parc4", "d8v1_parc4", "d9ACv1_parc4", "d9Cv1_parc4", "d9SPv1_parc4", "d9SVv1_parc4", "d10v1_parc4", "d11v1_parc4", "d12v1_parc4", "d1v2_parc4", "d2v2_parc4", "d3v2_parc4", "d4v2_parc4", "d5v2_parc4", "d6v2_parc4", "d7v2_parc4", "d8v2_parc4", "d9ACv2_parc4", "d9Cv2_parc4", "d9SPv2_parc4", "d9SVv2_parc4", "d10v2_parc4", "d11v2_parc4", "d12v2_parc4", "d1v3_parc4", "d2v3_parc4", "d3v3_parc4", "d4v3_parc4", "d5v3_parc4", "d6v3_parc4", "d7v3_parc4", "d8v3_parc4", "d9ACv3_parc4", "d9Cv3_parc4", "d9SPv3_parc4", "d9SVv3_parc4", "d10v3_parc4", "d11v3_parc4", "d12v3_parc4", "d13_parc4"


	d3b <- data.frame(
		variety = r3b[["e5_variedad"]]
	)
##r3b: "X1", "X2", "X3", "d1v1_parc5", "d2v1_parc5", "d3v1_parc5", "d4v1_parc5", "d5v1_parc5", "d6v1_parc5", "d7v1_parc5", "d8v1_parc5", "d9ACv1_parc5", "d9Cv1_parc5", "d9SPv1_parc5", "d9SVv1_parc5", "d10v1_parc5", "d11v1_parc5", "d12v1_parc5", "d1v2_parc5", "d2v2_parc5", "d3v2_parc5", "d4v2_parc5", "d5v2_parc5", "d6v2_parc5", "d7v2_parc5", "d8v2_parc5", "d9ACv2_parc5", "d9Cv2_parc5", "d9SPv2_parc5", "d9SVv2_parc5", "d10v2_parc5", "d11v2_parc5", "d12v2_parc5", "d1v3_parc5", "d2v3_parc5", "d3v3_parc5", "d4v3_parc5", "d5v3_parc5", "d6v3_parc5", "d7v3_parc5", "d8v3_parc5", "d9ACv3_parc5", "d9Cv3_parc5", "d9SPv3_parc5", "d9SVv3_parc5", "d10v3_parc5", "d11v3_parc5", "d12v3_parc5", "d13_parc5", "e1_varmej", "e1_nativa", "e1_tolera", "e1_rendim", "e1_uso", "e1_precio", "e1_merca", "e1_trabajo", "e1_sabor", "e1_time", "e1_almac", "e2_textura", "e2_forma", "e2_size", "e2_color", "e2_regis", "e2_fisico", "e2_plaga", "e2_edad", "e2_ojo", "e2_cali", "e2_precio", "e2_lugar", "e2_quien", "e2_certif", "e2_otra", "e3", "e4", "e5_sembrar", "e5_venta", "e5_save", "e5_descon", "e5_dispon", "e5_otra", "e6", "e7", "e8_varied", "e8_reemp", "e8_dific", "e8_almac", "e8_mercado", "e8_otra", "e9", "e10_sitio1", "e10_sitio2", "e10_sitio3", "e10_sitio4", "f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11"


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

