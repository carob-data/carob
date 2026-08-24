# R script for "carob"

# these data are already included in doi_10.7910_DVN_UNLRGC


carob_script <- function(path) {

"The AFSIS project aimed to establish an  Africa Soil Information system. Data was collected in sentinel sites across sub-Saharan Africa using the Land Degradation Surveillance framework and included also multi-location diagnostic trials in selected sentinel sites to determine nutrient limitations and response to improved soil management practices (soil amendments) "

	uri <- "doi:10.25502/20180814/1355/HJ"
	group <- "agronomy"

	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		publication=NA,
		project="AfSIS",
		data_organization = "IITA;ICRISAT;ABC",
		carob_contributor="Cedric Ngakou",
		carob_date="2023-02-19",
		data_type="experiment"
	)

	meta$dataset_id <- paste0(meta$dataset_id, "_nodata")
	carobiner::write_files(path, meta)
 
}



### download and read data 

# f1 <- ff[basename(ff) == "Nkhata Bay_S2_Field.csv"] ## get Field dataset
# f2 <- ff[basename(ff) == "Nkhata Bay_S2_Plant.csv"] ## get Plant dataset
# f3 <- ff[basename(ff) == "Nkhata Bay_S2_plot.csv"]## get plot dataset

# read dataset
# d1 <- read.csv(f1)
# d2 <- read.csv(f2)
# d3 <- read.csv(f3)
#d <- readxl::read_excel(f) |> as.data.frame()

# process file(s)
#d <- carobiner::change_names(d, from, to)
# d3$dataset_id <- dataset_id
# d1$dataset_id <- dataset_id

# process field data
# d1$trial_id <- c(paste0(d1$dataset_id,"-",d1$ID))
# d1$latitude <- d1$Flat
# d1$longitude <- d1$Flong
# d1$location <- d1$Village
# d1$soil_type <- d1$Soil.texture.class
# d1$OM_type <- d1$MType1
# d1$previous_crop <- d1$PCrop1

# add column

# d1$country <- "Malawi"
# d1$crop <- "maize"
# d1$OM_used=ifelse(d1$OM_type== "None","FALSE",
                  # ifelse(d1$OM_type=="NA", "FALSE ", "TRUE" ))

# d1 <- d1[,c("trial_id","location","country",
           # "latitude","longitude","crop","soil_type","previous_crop",
           # "OM_type","OM_used")]


# process plot dataset
# d3$rep <- d3$Rep

# d3$treatment <- d3$TrtDesc

# d3$yield <- (d3$TGrainYld_adj)*1000

# d3$fwy_residue <- (d3$AdjTStoverYld)*1000
# d3$season <- d3$Season
# d3$site <- d3$Site

# d3$N_fertilizer <- ifelse(d3$TrtDesc=="Control",0,
                        # ifelse(d3$TrtDesc=="PK",0,100))

# d3$K_fertilizer <- ifelse(d3$TrtDesc=="Control",0,
                        # ifelse(d3$TrtDesc=="NP",0,60))

# d3$P_fertilizer <- ifelse(d3$TrtDesc=="Control",0,
                        # ifelse(d3$TrtDesc=="NK",0,30))

# d3$Zn_fertilizer <- ifelse(d3$TrtDesc=="NPK+MN",3,0)

# d3$S_fertilizer <- ifelse(d3$TrtDesc=="NPK+MN",5,0)

# d3=transform(d3,N_splits=ifelse(d3$N_fertilizer>0,3,0))

# d3 <- d3[,c("site","rep","treatment","season","yield","fwy_residue","N_fertilizer",
          # "K_fertilizer","P_fertilizer","Zn_fertilizer","S_fertilizer","N_splits")]

#merge all the data
# d <- merge(d1,d3,by=all.x = TRUE)
# data type
# d$season <- as.character(d$season)
# d$OM_type <- as.character(d$OM_type)
# d$OM_used <- as.logical(d$OM_used)
# crop terms normalization
# p <- carobiner::fix_name(gsub("/", "; ", d$previous_crop), "lower")
# p <- gsub("maize & s", "sweetpotato", p)
# p <- gsub("potatoes", "potato", p)
# p <- gsub("cowpeas", "cowpea", p)
# p <- gsub("maize & cassava", "maize; cassava", p)
# p <- gsub(" potato&cassava", "potato; cassava", p)
# p <- gsub("nuts&cassava", "cassava; nuts", p)
# p <- gsub(" maize&s", "maize" , p)
# p <- gsub(" nuts", "nuts", p)
# p <- gsub("s",NA , p)
# p <- gsub("fallow", "no crop", p)


# d$previous_crop <- p
# change date format

# fill whitespace in soil_type 
# d <- replace(d,d=='',NA)


# carobiner::write_files(meta, d, path=path)


# }
