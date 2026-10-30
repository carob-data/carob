# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Replication Data for: Morphological and metabolic profiling of a tropical‐adapted potato association panel subjected to water recovery treatment reveals new insights into plant vigor

All 258 genotypes of the potato association panel were grown in a field trial in 2017 in Ica, Peru. The field trial was performed with triplicates in an augmented block design, with (i) control conditions (NI) and (ii) water-deficit conditions with recovery (REC), as  described in the handbook for evaluation and data management of potato clones.  All potato genotypes were profiled for 45 morphological traits and 42 central metabolites via nuclear magnetic resonance.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/NT74IT"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "Pontifical Catholic University of Peru; CIP",
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
	

	f1 <- ff[basename(ff) == "Data S1. Morphological trait profiles_tpj14892-sup-0007-datas1.xlsx"]
	f2 <- ff[basename(ff) == "Data S2. Metabolite profiles_tpj14892-sup-0008-datas2.xlsx"]
	f3 <- ff[basename(ff) == "Data S3. Metabolite single nucleotide polymorphism marker set_tpj14892-sup-0009-datas3.xlsx"]
	f4 <- ff[basename(ff) == "Data S4. Fumarate single nucleotide polymorphism‐associated genes_tpj14892-sup-0010-datas4.xlsx"]
	f5 <- ff[basename(ff) == "Data S5. Potato association panel._tpj14892-sup-0011-datas5.xlsx"]
	f6 <- ff[basename(ff) == "Data S6. Morphological trait definitions_tpj14892-sup-0012-datas6.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2a <- carobiner::read.excel(f2, sheet="raw")
	r2b <- carobiner::read.excel(f2, sheet="mmol")
	r2c <- carobiner::read.excel(f2, sheet="mmol_stats")
	r3 <- carobiner::read.excel(f3)
	r4 <- carobiner::read.excel(f4)
	r5 <- carobiner::read.excel(f5)
	r6 <- carobiner::read.excel(f6)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		plot_id = r1[["PLOT"]],
		treatment = r1[["Treatment"]]
	)
##r1: "INSTN", "TAFW", "TADW", "APFW", "APDW", "APDMC", "Drone_NDVI_EV1", "Drone_NDVI_EV2", "Drone_NDVI_EV3", "Drone_NDVI_Slp", "Plant_vigor", "Plant_Unif", "SNPP", "PLAHE_EV1", "PLAHE_EV2", "PLAHE_EV3", "PLAHE_Slp", "SD_EV1", "SD_EV2", "SD_EV3", "SD_Slp", "ChISPAD_EV1", "ChISPAD_EV2", "ChISPAD_EV3", "ChISPAD_Slp", "CR_NDVI_EV1", "CR_NDVI_EV2", "CR_NDVI_EV3", "CR_NDVI_Slp", "CC_EV1", "CC_EV2", "CC_EV3", "CC_Slp", "NPH", "TNTP", "TNTPL", "TTWP", "TTWPL", "TTYA", "TTYNA", "ATW", "TFW", "TDW", "TDMC", "HI_FW", "HI_DW"


	d2a <- data.frame(
		treatment = r2a[["Treatment"]]
	)
##r2a: "Genotype", "α-Fructose", "α-Glucose", "β-Fructose", "β-Galactose", "β-Glucose", "Adenosine", "Alanine", "Allantoate", "Allantoin", "Asparagine", "Aspartate", "Choline", "Citrate", "Cytidine", "Dehydroascorbate", "Fumarate", "GABA", "Glutamate", "Glutamine", "Glycine", "Guanosine", "Histidine", "Isoleucine", "Lactate", "Leucine", "Lysine", "Malate", "Methionine", "myo-Inositol", "Phenylalanine", "Proline", "Pyroglutamate", "Quiniate", "Sucrose", "Threonine", "Trigonelline", "Tryptophan", "Tyrosine", "UNK_1", "UNK_2", "Uridine", "Valine"


	d2b <- data.frame(
		treatment = r2b[["Treatment"]]
	)
##r2b: "Genotype", "α-Fructose", "α-Glucose", "β-Fructose", "β-Galactose", "β-Glucose", "Adenosine", "Alanine", "Allantoate", "Allantoin", "Asparagine", "Aspartate", "Choline", "Citrate", "Cytidine", "Dehydroascorbate", "Fumarate", "GABA", "Glutamate", "Glutamine", "Glycine", "Guanosine", "Histidine", "Isoleucine", "Lactate", "Leucine", "Lysine", "Malate", "Methionine", "myo-Inositol", "Phenylalanine", "Proline", "Pyroglutamate", "Quiniate", "Sucrose", "Threonine", "Trigonelline", "Tryptophan", "Tyrosine", "UNK_1", "UNK_2", "Uridine", "Valine"


	d2c <- data.frame(
		treatment = r2c[["Treatment"]]
	)
##r2c: "Genotype", "α-Fructose_mean", "α-Fructose_stdErr", "α-Glucose_mean", "α-Glucose_stdErr", "β-Fructose_mean", "β-Fructose_stdErr", "β-Galactose_mean", "β-Galactose_stdErr", "β-Glucose_mean", "β-Glucose_stdErr", "Adenosine_mean", "Adenosine_stdErr", "Alanine_mean", "Alanine_stdErr", "Allantoate_mean", "Allantoate_stdErr", "Allantoin_mean", "Allantoin_stdErr", "Asparagine_mean", "Asparagine_stdErr", "Aspartate_mean", "Aspartate_stdErr", "Choline_mean", "Choline_stdErr", "Citrate_mean", "Citrate_stdErr", "Cytidine_mean", "Cytidine_stdErr", "Dehydroascorbate_mean", "Dehydroascorbate_stdErr", "Fumarate_mean", "Fumarate_stdErr", "GABA_mean", "GABA_stdErr", "Glutamate_mean", "Glutamate_stdErr", "Glutamine_mean", "Glutamine_stdErr", "Glycine_mean", "Glycine_stdErr", "Guanosine_mean", "Guanosine_stdErr", "Histidine_mean", "Histidine_stdErr", "Isoleucine_mean", "Isoleucine_stdErr", "Lactate_mean", "Lactate_stdErr", "Leucine_mean", "Leucine_stdErr", "Lysine_mean", "Lysine_stdErr", "Malate_mean", "Malate_stdErr", "Methionine_mean", "Methionine_stdErr", "myo-Inositol_mean", "myo-Inositol_stdErr", "Phenylalanine_mean", "Phenylalanine_stdErr", "Proline_mean", "Proline_stdErr", "Pyroglutamate_mean", "Pyroglutamate_stdErr", "Quiniate_mean", "Quiniate_stdErr", "Sucrose_mean", "Sucrose_stdErr", "Threonine_mean", "Threonine_stdErr", "Trigonelline_mean", "Trigonelline_stdErr", "Tryptophan_mean", "Tryptophan_stdErr", "Tyrosine_mean", "Tyrosine_stdErr", "UNK_1_mean", "UNK_1_stdErr", "UNK_2_mean", "UNK_2_stdErr", "Uridine_mean", "Uridine_stdErr", "Valine_mean", "Valine_stdErr"


	d3 <- data.frame()
##r3: "Trait", "Threshold", "Marker", "Chromosome", "Position", "Ref", "Alt", "Score"


	d4 <- data.frame()
##r4: "Chromosome", "Start position", "End position", "Protein Id", "Gene Id"


	d5 <- data.frame()
##r5: "population", "CIP.code", "female.parent", "male.parent"


	d6 <- data.frame()
##r6: "Morphological trait", "Abbreviation/Acronym", "Definition"


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

