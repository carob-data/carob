# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {
   
"This database compiles soybean phenology data to address significant variations in soybean adaptation and development caused by genetic improvements and regional climatic differences. The dataset includes growth staging information collected from field experiments conducted across 11 location-years in Arkansas, Minnesota, Ohio, Virginia, and Wisconsin (USA) during 2017 and 2018. It incorporates data from commercial soybean varieties spanning maturity groups 0 to 7.5. Growth stages were determined using Fehr and Caviness (1977) approach. This dataset is intended as a resource for the scientific community, students, and stakeholders, providing soybean phenological data to improve predictions and decision-making in areas such as input timing, yield estimation, irrigation management, cultivar selection, and phenotyping"
   
   uri <- "doi:10.5061/dryad.2bvq83c20"
   group <- "varieties"
   ff <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=3, minor=NA,
      data_organization = "UWM", #University of Wisconsin–Madison
      publication=NA, 
      project=NA, 
      data_type= "experiment", 
      treatment_vars= "variety", 
      response_vars = "growth_stage;node_count;yield", 
      carob_contributor= "Cedric Ngakou", 
      carob_date="2025-06-16",
      completion=90,
      notes="data on seed composition (protein, oil, lysine, etc.) not processed"
   )
   
   
   f <- ff[basename(ff) == "SeveroSilvaEtal_Phenology_Dataset_Dryad.xlsx"]
   
   ### General information 
   r1 <- carobiner::read.excel(f, sheet = "GeneralInformation", na= ("*"))

   lonlat <- do.call(rbind, strsplit(r1$`GPS coordinates`, ","))
   
   d1 <- data.frame(
      year= as.character(r1$Year...1),
      location= ifelse(grepl("Charleson", gsub("_", "-", r1$Location)), "South Charleston", gsub("_", "-", r1$Location)),
      adm1= r1$State...2,
      land_prep_method= ifelse(grepl("chisel", r1$`Tillage type`), "reduced tillage",
                        ifelse(grepl("no-till", r1$`Tillage type`), "minimum tillage", r1$`Tillage type`)),
	  longitude = as.numeric(lonlat[,2]),
      latitude = as.numeric(lonlat[,1]),
      ## see r3  ## planting_date= as.character(r1$`Planting Date`),
      harvest_date= gsub(" ", "", r1$`Harvest Date`),
      seed_density= as.numeric(r1$`Seeding rate (seeds/ha)`)
   )
   

  ### Fixing harvest data
   HD <- strsplit(as.character(gsub("and", ",", d1$harvest_date)), ",")
   d1$harvest_date <- lapply(HD, function(x) {
		hd <- ifelse(grepl("/", x), gsub("00", "20", format(as.Date(x, format = "%m/%d/%Y"), "%Y-%m-%d")), x)
		i <- grepl("^\\d{5}$", hd)
		hd[i] <- as.character(as.Date(as.numeric(hd[i]), origin = "1899-12-30"))
		paste(hd, collapse=";")
	}) |> unlist()


   ### Grain Harvest
   r2 <- carobiner::read.excel(f, sheet = "GrainHarvest",  na= ("*"))
   names(r2) <- r2[1,]
   r2 <- r2[-1, ]
   d2 <- data.frame(
      adm1= r2$State,
      location= r2$location,
      year= r2$year,
      plot_id= r2$plot,
      rep= as.integer(r2$rep),
      variety= r2$variety,
	  variety_traits = paste0("maturity=", round(as.numeric(r2$RM), 1)),
      #rm= r2$RM,
      yield_moisture= as.numeric(r2$`combine moisture`),
      yield= as.numeric(r2$`Total yield kg/a`)*2.471, # in kg/ha
      grain_protein= as.numeric(r2$`Protein 13%`),
      #grain_oil= r2$`Oil 13%`, ## oil content in %
      ### no!
	  ###trial_id=paste0(r2$location, "-", r2$plot)
	  trial_id=paste0(r2$location, "-", r2$year)
   )
   
   ### merge d1 and d2 
   ### d <- merge(d2, d1, by= c("adm1", "location", "year"))
    
   ### Phenology
   r3 <- carobiner::read.excel(f, sheet="PhenologyData", na="*", skip=2)
	grow_st <- grepl("Fehr|Be|End", names(r3))
    nms <- trimws(gsub("Fehr|Â ", "", names(r3)[grow_st]))
	i <- grepl("^Beg", nms)
	nms[i] <- trimws(paste0(gsub("^Beg", "", nms[i]), "_start"))
	i <- grepl("^End", nms)
	nms[i] <- trimws(paste0(gsub("^End", "", nms[i]), "_end"))
	names(r3)[grow_st] <- nms
	r3[,grow_st] <- as.character(as.Date(paste0(r3$year, "-01-01")) - 1 + as.matrix(r3[,grow_st]))
   d3 <- data.frame(
      adm1= r3$State,
      location= r3$location,
      plot_id=r3$plot,
      year= r3$year,
      rep= as.integer(r3$rep),
      variety= r3$variety,
##???   transplanting_days= as.numeric(r3$`Plant date`), ## in day of the year
	  planting_date = as.character(as.Date(paste0(r3$year, "-01-01")) + as.integer(r3$`Plant date`) - 1),
	  r3[, grow_st]
   )
   
   #### Node data 
   r4 <- carobiner::read.excel(f, sheet = "NodeData",  na= ("*"), skip=1)
   Node <- names(r4)[grepl("Date", names(r4))]
   d4 <- data.frame(
      adm1= r4$State,
      location= gsub("South Charleston, OH", "South Charleston", r4$location),
      year= r4$year,
      plot_id= r4$plot,
      rep= as.integer(r4$rep),
      variety= r4$variety,
	  r4[, Node]
   )
   nodes <- grepl("Date", names(d4))
	s <- which(is.na(d4$adm1))
	e <- c(s[-1]-1, nrow(d4))
	z <- lapply(1:length(s), \(i) {
				ds <- d4[s[i]:e[i], ]
				h <- as.integer(sapply(strsplit(unlist(ds[1,nodes]), " "), \(x) x[1]))
				ds <- ds[-1,]
				h <- as.character(as.Date(paste0(ds$year[1], "-01-01")) + h - 1)
				colnames(ds)[nodes] <- as.character(h)
				ds <- ds[, !is.na(colnames(ds))]   
				r <- reshape(ds, direction="long", varying=(which(nodes)[1]):ncol(ds), v.names="node_count", timevar="date")
				r$date <- h[r$date]
				r
			}
		)
	d4 <- na.omit(do.call(rbind, z))
	d4$node_count <- as.numeric(d4$node_count)

	# first merge d2 and d3
	dd <- merge(d2, d3, by=c("adm1", "location", "plot_id", "rep", "variety", "year"), all=TRUE)
	dd$record_id <- as.integer(1:nrow(dd))
	i <- which(names(dd) == "VE"):which(names(dd) == "R8")
	d3 <- dd[, c("record_id", names(dd)[i])]
	d <- dd[, -i]


	# then merge d and d1 to not have missing coordinates
	d <- merge(d, d1, by=c("adm1", "location", "year"))
	
	##d <- merge(d, d2, by=c("adm1", "location", "plot","rep","variety", "year"), all = TRUE)

	## transfer record_id to d4
	vars <- c("adm1", "location", "plot_id","rep","variety", "year")
	d4 <- merge(dd[, c("record_id", vars)], d4, by=vars)[, c("record_id", "node_count", "date")]

	nms <- names(d3)[-1]
	d3 <- reshape(d3, direction="long", varying=nms, v.names="date", timevar="growth_stage")
	d3$growth_stage <- nms[d3$growth_stage]
	d3$id <- NULL
	d3 <- d3[!is.na(d3$date), ]

	long <- carobiner::bindr(d3, d4)
	  

## fix
	d$adm1 <- carobiner::replace_values(dd$adm1, c("AR", "MN", "OH", "VA", "WI"), 
			c("Arkansas", "Minnesota", "Ohio", "Virginia", "Wisconsin"))
	d$year <- NULL


## enrich
    d$country= "United States"
    d$crop= "soybean"
    d$on_farm= TRUE
    d$is_survey= FALSE
    d$irrigated= NA
    d$geo_from_source= TRUE
    d$yield_part= "grain"

## missing variables    
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
   carobiner::write_files(path, meta, d, long = long)
   
}
