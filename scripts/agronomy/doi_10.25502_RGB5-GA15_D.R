# R script for "carob"


carob_script <- function(path) {
  
"Developing efficient and affordable fertilizer products for increased and sustained yields in the maize belt of Nigeria. Maize belt of Nigeria that covers 8 states: Bauchi, Kaduna, Kano, Katsina, Nasarawa, Niger, Plateau and Taraba"


  uri <- "doi:10.25502/RGB5-GA15/D"
  group <- "agronomy"
  ff <- carobiner::get_data(uri, path, group)
 
  meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
    publication=NA,
    data_organization = "IITA",
    carob_contributor="Cedric Ngakou",
    carob_date="2023-02-27",
    data_type="experiment",
	project=NA,
	response_vars = "yield",
	treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;Zn_fertilizer;S_fertilizer"
  )
  
  
  f1 <- ff[basename(ff) == "BUK_T1_VT_yieldatharvest_summ.csv"] # dataset from team1
  f2 <- ff[basename(ff) == "BUK-T2_VT_yieldatharvest_summ.csv"] # dataset from team2
  f3 <- ff[basename(ff) == "BUK_T3_VT_yieldatharvest_final.csv"]# dataset from team3
  # read the dataset
  d1 <- read.csv(f1)
  d2 <- read.csv(f2)
  d3 <- read.csv(f3)
  #d <- readxl::read_excel(f) |> as.data.frame()
  
  # process file(s)

##RH add plant density
  
	d1 <- d1[, c("Section_B.repeat.trt_name", "X_parent_index", "yld.plot.kg.ha")] 
	colnames(d1) <- c("treatment", "rep", "yield")
	d1$location <- "katsina-kaduna" # Areas serviced by Team3
	d2 <- d2[, c("trt_name", "parent_index", "plot.yld..kg.ha.")] 
	colnames(d2) <- c("treatment", "rep", "yield")
	d2$location <- "kaduna-kano" # Areas serviced by Team2
	d3 <- d3[, c("trt_name", "parent_index", "yld.plot")] 
	colnames(d3) <- c("treatment", "rep", "yield")
	d3$location <- "Bauchi" # Areas serviced by Team3
	d <- rbind(d1, d2, d3)
	d$plant_spacing <- 25 # get from VT protocol OCP Project Document 
	d$row_spacing <- 75   # get from VT protocol OCP Project Document
	 
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	
## RH: I see:  

#table(d$treatment)
#Control     NPK   OCPF1   OCPF2 
#    250     250     250     250 

# NPK Apply  15-15-15 means 15% N, 15% P2O5, 15% K2O
#OCPF1  (N) : 11% (P2O5): 21% (k2O): 22%  S: 5%  Zn: 1% 
#OCPF2 (N) : 14% (P205): 31% (k2O): 0%   S: 9%  Zn: 0.9%
	
	d$N_fertilizer <- ifelse(d$treatment == "Control", 0, 
                      ifelse(d$treatment == "OCPF1", 11,
                      ifelse(d$treatment=="OCPF2",14,15)))
  
  d$K_fertilizer <- ifelse(d$treatment == "Control", 0, 
                    ifelse(d$treatment == "OCPF1", 22/1.2051,
                    ifelse(d$treatment=="OCPF2",0,15/1.2051)))
  
  d$P_fertilizer <- ifelse(d$treatment == "Control", 0, 
                    ifelse(d$treatment == "OCPF1", 21/2.29,
                    ifelse(d$treatment=="OCPF2",31/2.29,15/2.29)))
  
  d$Zn_fertilizer <- ifelse(d$treatment == "Control", 0, 
                    ifelse(d$treatment == "OCPF1", 1,
                    ifelse(d$treatment=="OCPF2",0.9,0)))
  
  d$S_fertilizer <- ifelse(d$treatment == "Control", 0, 
                    ifelse(d$treatment == "OCPF1", 5,
                    ifelse(d$treatment=="OCPF2",9,0)))
  # add Columns 
  
  d$country <- "Nigeria"
  d$crop <- "maize"
	d$yield_part <- "grain"
  
  d$variety <- "Sammaz 15"    # get from VT protocol OCP Project Document
#Longitude and latitude fixed base on location
  d$latitude[d$location=="Bauchi"] <- 10.6228284
  d$longitude[d$location=="Bauchi"] <- 10.0287754
  d$latitude[d$location=="katsina-kaduna"] <- 12.5630825 # since the experience in both areas was under the same condition, the coordinate is for one area 
  d$longitude[d$location=="katsina-kaduna"] <- 7.6207063
  d$latitude[d$location=="kaduna-kano"] <- 10.5182899
  d$longitude[d$location=="kaduna-kano"] <- 7.4359863
  d$geo_from_source <- FALSE
# planting date is June 2017  get from VT protocol
  d$planting_date <- "2017-06-01"
  d$harvest_date <- "2017-11-01"
  d$season <- "2017"
  
  d$trial_id <- d$location
#data type
  d$yield <- as.numeric(d$yield)
  d <- d[!is.na(d$yield), ]

	d$yield_moisture <- as.numeric(NA) #needs to be checked

 carobiner::write_files(meta, d, path=path)
 
}

