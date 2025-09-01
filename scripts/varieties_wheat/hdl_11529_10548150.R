Carob_script <- function(path) {
  
  "TAMASA Tanzania. Agronomy Panel Survey (APS) 2016. Crop Cut & Soil data
  
  The 2016 Agronomy Panel Survey (APS) for Tanzania is the first wave of a yearly farm household panel dataset collected under the BMGF-funded project Taking Maize Agronomy to Scale in Africa (TAMASA), in collaboration with the Sustainable Intensification Innovation Lab (SIIL). The APS is a detailed farm household survey with agronomic, yield and soils components for a focal field. This is a yearly panel survey, i.e. data are collected on the same households and fields every year for the duration of the project. The purpose of the data collection is severalfold: 1. To better document spatio-temporal variability in agronomic management and yield outcomes 2. To facilitate research on a. drivers of yield outcomes (i.e. role of alternative agronomic practices), and b. the determinants of farmer decision-making (i.e. in adopting particular agronomic practices) that contribute to observed yield outcomes 3. To test the hypothesis that farm-level socioeconomic data adds value to our understanding of yield outcomes (and, thus, our ability to predict such outcomes in the future, using geospatially explicit approaches) 4. To serve as a baseline for the evaluation of site-specific nutrient management tools, using randomized control experimental approaches, combined with econometric analysis of panel survey data (2018-11-14)"
 
   uri <- "hdl:11529/10548150"
  group <- "soil_samples"
  ff <- carobiner::get_data(uri, path, group)
  meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
                                  data_organization = "CIMMYT",
                                  publication=NA,
                                  project="TAMASA",
                                  data_type= "survey",
                                  response_vars = "none",
                                  treatment_vars = "none",
                                  carob_contributor= "Blessing Dzuda",
                                  carob_date="2025-09-01",
                                  completion = 100,	
                                  design=NA,
                                  notes = NA
  )
  f <- ff[basename(ff) == "TAMASA_TZ_APS_Soil_2016.xlsx"]
  r <- carobiner::read.excel(f, sheet ="Data")
  
  d <- data.frame(
    country = r$Country,
    adm1 = r$Region,
    adm2 = r$District,
    adm3= r$Village,
    adm4= r$Ward,
    location=r$Hamlet,
    longitude = r$Longitude,
    latitude = r$Latitude,
    elevation=r$Altitude,
    soil_C = r$C,
    soil_pH = r$pH,
    soil_Al = r$Al,
    soil_B = r$B,
    soil_Ca = r$Ca,
    soil_Fe = r$Fe,
    soil_Mg = r$Mg,
    soil_Mn = r$Mn,
    soil_Na = r$Na,
    soil_S = r$S,
    soil_P=r$P,
    soil_Zn=r$Zn,
    soil_K=r$K,
    soil_N=r$N*10000,
    soil_EC=r$EC.S,
    geo_from_source = TRUE
  )
  
   #splitting depth column
   depth<- do.call(rbind,strsplit(as.character(r$Depth),"-"))
   
   #assigning the split column into 2 individual columns
   d$depth_top <- as.numeric(depth[,1])
   d$depth_bottom <- as.numeric(depth[,2])
   
   #dropping last 7 rows without useful data
   d <- d[!is.na(d$adm1),]
   
   soilmeta <- data.frame(
     soil_element = c("Al", "B", "Ca", "Fe", "K", "Mg", "Mn", "Na", "S"),
     soil_method = "spectroscopy (Mehlich3 extraction estimate)"
   )
   
  
  carobiner::write_files(path, meta, d)
}
