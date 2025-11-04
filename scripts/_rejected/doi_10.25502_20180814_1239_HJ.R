# R script for "carob"

# not writing records as this datasets is
# already included in doi_10.7910_DVN_UNLRGC


carob_script <- function(path) {

# The AFSIS project aimed to establish an  Africa Soil Information system. Data was collected in sentinel sites across sub-Saharan Africa using the Land Degradation Surveillance framework and included also multi-location diagnostic trials in selected sentinel sites to determine nutrient limitations and response to improved soil management practices (soil amendments)

	uri <- "doi:10.25502/20180814/1239/HJ"
	group <- "agronomy"

	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		project="AFSIS",
		publication= NA,
		data_organization = "IITA;ICRISAT;ABC",
		carob_contributor="Cedric Ngakou",
		carob_date="2023-02-15",
		data_type="experiment"
	)
	meta$dataset_id <- paste0(meta$dataset_id, "_nodata")

	carobiner::write_files(path, meta)

}

# carob_script <- function(path) {
  
  # uri <- "doi:10.25502/20180814/1239/HJ"
  # group <- "agronomy"
  ### dataset level data 
  # meta <- # publication= NA,
    # data_organization = "IITA",
    # carob_contributor="Cedric Ngakou",
    # carob_date="2023-02-15",
    # data_type="experiment",

    # has_weather=FALSE
    #  
  # )
  
  
  
  
  # f1 <- ff[basename(ff) == "Mbinga_DT2010_field.csv"]## get field data 
  # f2 <- ff[basename(ff) == "Mbinga_DT2010_plant.csv"]##get plant data 
  # f3 <- ff[basename(ff) == "Mbinga_DT2010_plot.csv"]## get plot data 
  
  
  # d1 <- read.csv(f1)
  # d2 <- read.csv(f2)
  # d3 <- read.csv(f3)
  #d <- readxl::read_excel(f) |> as.data.frame()
  
  # process file(s)
  #d <- carobiner::change_names(d, from, to)

    # process plot data
  
  # d3$trial_id <- c(paste0(d3$dataset_id,"-",d3$ID))
  # d3$rep <- d3$Rep
  # d3$season <- d3$Season
  # d3$treatment <- d3$TrtDesc
  # d3$yield <- (d3$TGrainYld_adj)*1000
  # d3$fwy_residue <- (d3$AdjTStoverYld)*1000
  # d3$seed_weight <- d3$Wgt100grain
  # d3$N_fertilizer <- ifelse(d3$TrtDesc=="Control",0,
                          # ifelse(d3$TrtDesc=="PK",0,100))
  
  # d3$K_fertilizer <- ifelse(d3$TrtDesc=="Control",0,
                          # ifelse(d3$TrtDesc=="NP",0,60))
  
  # d3$P_fertilizer <- ifelse(d3$TrtDesc=="Control",0,
                          # ifelse(d3$TrtDesc=="NK",0,30))
  
  # d3$Zn_fertilizer <- ifelse(d3$TrtDesc=="NPK+MN",3,0)
  
  # d3$S_fertilizer <- ifelse(d3$TrtDesc=="NPK+MN",5,0)
  
  # d3=transform(d3,N_splits=ifelse(d3$N_fertilizer>0,3,0))
  
  # d3 <- d3[,c("rep","season","treatment","trial_id","yield","fwy_residue","seed_weight","N_fertilizer",
            # "K_fertilizer","P_fertilizer","Zn_fertilizer","S_fertilizer","N_splits")]
  # process field data
  
  # d1$latitude <- d1$Flat
  # d1$longitude <- d1$Flong
  # d1$planting_date <- d1$Basal.fertilizer.application
  # d1$harvest_date <- d1$HarvDa
  # d1$OM_type <- d1$MType1
  # d1$previous_crop <- d1$PCrop1
  # d1$site <- d1$Site
  # d1 <- d1[,c("site","longitude","latitude","planting_date","harvest_date","previous_crop","OM_type")]
 
  # merge dataset
  
  # d <- merge(d1,d3,by=all.x = TRUE)
  
  #add column
  # d$country <- "Tanzania"
  # d$crop <- "maize"
  
  #d <- transform(d,OM_used=ifelse(d$OM_type=="None", "FALSE","TRUE"))
  
  # p <- carobiner::fix_name(gsub("/", "; ", d$previous_crop), "lower")
  # p <- gsub("beans.", "common bean", p)
  # p <- gsub("beans", "common bean", p)
  # p <- gsub("beans and cassava", "common bean; cassava", p)
  # p <- gsub("fallowed", "no crop", p)
  # p <- gsub("-", NA, p)
  # p <- gsub("common beanand cassava", "common bean; cassava", p)
  # d$previous_crop <- p
  
  #data type
  # d$season <- as.character(d$season)
  # d$seed_weight <- as.numeric(d$seed_weight)
  
  #d$OM_used <- as.character(d$OM_used)
  # d <- d[,c("rep","season","country","site","treatment","longitude","latitude","planting_date",
          # "harvest_date","trial_id","crop","yield","fwy_residue","seed_weight","previous_crop","OM_type","N_fertilizer",
            # "K_fertilizer","P_fertilizer","Zn_fertilizer","S_fertilizer","N_splits")]
  # change date format
  # d$planting_date <- format(as.Date(d$planting_date, format = "%d/%m/%Y"), "%Y-%m-%d")
  
  # d$harvest_date <- format(as.Date(d$harvest_date, format = "%m/%d/%Y"), "%Y-%m-%d")
  
  
  # carobiner::write_files(meta, d, path=path)
  
# }

