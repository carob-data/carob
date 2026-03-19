# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Dataset for: Stability analysis for nutritional contents (minerals) on 40 native potato cultivars

Breeding efforts for enriching potato tubers with more Zn and Fe are in progress at the International Potato Center (CIP). Knowledge of genotype by environment interaction effects on the micronutrient concentrations of different genotypes is needed to identify cultivars that show high and stable concentrations and to inform breeding and selection schemes. Stability analysis for micronutrient content has been applied to biofortification and varietal dissemination strategies. Forty native potato cultivars were evaluated to: assess the magnitude and nature of Genotype (G), Environment (E), and GxE interaction effects for Zn and Fe concentrations in the tropical highlands of Peru and study the contribution of soil fertility to the micronutrient content of potato tubers. Tubers were taken from plots grown in randomized complete block designs with three replications of one hill per plot in each of 6 sites of the central Peruvian Andes in 2010: Ccasapata (3765 m.a.s.l); Sotopampa (3754 m.a.s.l); Ccollpaccasa (4067 m.a.s.l); Conayca (4178 m.a.s.l), la Victoria (3265 m.a.s.l) and Rangra (3323 m.a.s.l). Well-matured tubers were harvested at 150 and 180 days. Samples were prepared and analysed for Fe and Zn by inductively coupled plasma-optical emission spectrophotometry (ICP-OES) using an ARL 3580B ICP (ARL, Switzerland) (Burgos et al., 2007). Statistical analyses were performed using SAS software (SAS. 2003). ANOVA was performed using combined data for all environments. The Additive Main Effects and Multiplicative Interaction model (AMMI) was used for studying Genotype X Enviroment interaction, examining genotypic yield stability and adaptation (Crossa et al., 2002).
"

	uri <- "doi:10.21223/P3/SVHUCG"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=4,
		data_organization = "CIP; Yanapai",
		publication = NA,
		project = NA,
		carob_date = "2026-03-11",
		design = "RCB",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "tuber_Zn;tuber_Fe", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "DataDictionary_Minerals.xls"]
	f2 <- ff[basename(ff) == "TMIN200911_CCASAP_Stability_Field.xls"]
	f3 <- ff[basename(ff) == "TMIN200911_CCOLL_Stability_Field.xls"]
	f4 <- ff[basename(ff) == "TMIN200911_LAVICT_Stability_Field.xls"]
	f5 <- ff[basename(ff) == "TMIN200911_MRSCAC_Stability_Field.xls"]
	f6 <- ff[basename(ff) == "TMIN200911_RANGR_Stability_Field.xls"]
	f7 <- ff[basename(ff) == "TMIN200912_SOTOP_Stability_Field.xls"]

	r1 <- carobiner::read.excel(f1)
	ff1 = ff[grepl("Field",basename(ff))]
#### process
process  <- function(f) {
  
  r1 <- carobiner::read.excel(f, sheet="Minimal")
  if(grepl("LAVICT", f)){r2 <- carobiner::read.excel(f, sheet="ORIGINAL")
  }else if(grepl("RANGR|SOTOP", f)){r2 <- carobiner::read.excel(f, sheet="original")
  }else{r2 <- carobiner::read.excel(f, sheet="Original")}
         
  r3 <- carobiner::read.excel(f, sheet="Crop_management")
  if(grepl("RANGR", f)){
    r4 <- carobiner::read.excel(f, sheet="Soil-analysis")
  }else {r4 <- carobiner::read.excel(f, sheet="Soil_analysis")}
  
  
  
  names(r2) <-  r2[5,]
  r2 <- r2[-c(1:5),]
  names(r2) <-  gsub("Repetitions", "REP", names(r2))
  if( is.null(r2$INSTN)){ r2$INSTN <- NA }
  
  v <-  data.frame(t(r1$Value))
  names(v) <-  r1$Factor
  S <- data.frame(t(r4$Data1))
  names(S) <-  r4$Variables
  P <- data.frame(t(r3$Date))
  names(P) <-  r3$`Intervention category`
  d1 <- data.frame(
    variety_code = r2$INSTN,
    rep = as.integer(r2$REP),
    variety = r2$`Accession name`, 
    planting_date = P$Preparation,
    harvest_date = P$Harvest,
    country = v$Country,
    adm2 = v$Admin2,
    adm1 = v$Admin1,
    adm3 = v$Admin3,
    location = v$Locality,
    yield_moisture = as.numeric(r2$`Percentage Dry matter(%)`),
    tuber_Fe = r2$`Fe (mg/kg,DW)`,
    tuber_Mn = r2$`Mn (mg/kg,DW)`,
    tuber_B = r2$`B (mg/kg,DW)`,
    tuber_Cu = r2$`Cu (mg/kg,DW)`,
    tuber_Zn = r2$`Zn (mg/kg,DW)`,
    tuber_Ca = r2$`Ca (mg/kg,DW)`,
    tuber_Mg = r2$`Mg (mg/kg,DW)`,
    tuber_K = r2$`K (mg/kg,DW)`,
    tuber_P = r2$`P (mg/kg,DW)`,
    tuber_S = r2$`S (mg/kg,DW)`,
    tuber_Na = r2$`Na (mg/kg,DW)`,
    tuber_Al = r2$`Al (mg/kg,DW)`,
    tuber_Ti = r2$`Ti (mg/kg,DW)`,
    tuber_Cr = r2$`Cr (mg/kg,DW)`,
    tuber_Se = r2$`Se (mg/kg,DW)`,
    tuber_Pb = r2$`Pb (mg/kg,DW)`,
    tuber_Cd = r2$`Cd (mg/kg,DW)`,
    tuber_Co = r2$`Co (mg/kg,DW)`,
    tuber_Ni = r2$`Ni (mg/kg,DW`,
    latitude = as.numeric(v$Latitude),
    longitude = as.numeric(v$Longitude),
    crop = v$Crop,
    
    ## Adding soil
    soil_pH = as.numeric(S$`Soil PH`),
    soil_EC = as.numeric(S$`Electrical conductivity`),
    soil_SOM = as.numeric(S$`Organic matter`),
    soil_N = as.numeric(S$`Nitrogenum total`),
    soil_P = as.numeric(S$Phosphorus),
    soil_K = as.numeric(S$Potassium),
    soil_sand = as.numeric(S$Sand),
    soil_clay = as.numeric(S$Clay),
    soil_texture = S$`Soil texture`,
    soil_CEC = as.numeric(S$`Cation Exchange Capacity`),
    soil_Ca_exch = as.numeric(S$`Exchangeable Calcium`),
    soil_Mg_exch = as.numeric(S$`Exchangeable Magnesium`),
    soil_K_exch = as.numeric(S$`Exchangeable Potassium`),
    soil_Na_exch = as.numeric(S$`Exchangeable Sodium`),
    soil_ex_acidity = as.numeric(S$`Exchangeable Acidity`),
    soil_BS = as.numeric(S$`Base Saturation`),
    soil_WHC_sat	= as.numeric(S$`Water capacity retention`),
    trial_id = "1"  , 
    on_farm = TRUE, 
    is_survey = FALSE, 
    yield_part = "tubers", 
    irrigated = NA, 
    geo_from_source = TRUE
  )
  
return(d1)
}	

dd = lapply(ff1, process)
d = do.call(rbind, dd)

## drop rows with NA in variety
d <- d[!is.na(d$variety),]

### fixing soil texture 
P <- carobiner::fix_name(d$soil_texture)
P <- gsub("Franco arcillosos", "clay loam", P)
P <- gsub("Franco arcillosa", "clay loam", P)
P <- gsub("Franco arenoso", "sandy loam", P)
P <- gsub("Franco", "loam", P)
d$soil_texture <- P


d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

### fixing lon and lat error
i <- grepl("Ccollpaccasa", d$location)
d$latitude[i] <- -12.7327
d$longitude[i] <- -74.755

### remove duplicate rows
d <- unique(d)


carobiner::write_files(path, meta, d)

}

