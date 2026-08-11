# R script for "carob"
# license: GPL (>=3)

## ISSUES

### 

carob_script <- function(path) {

"
Managing nitrogen and nutrient balances for long term sustainable rice production with a changing climate

The experiment was conducted in the Long-term experiment at IRRI. Experiment was carried out in a split plot design. This study focused on how we can achieve sustainable rice production under changing climatic conditions by managing N in soil.
"


	uri <- "doi:10.7910/DVN/TRURAO"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=4,
		data_organization = "IRRI",
		publication = NA,
		project = NA,
		design = "split plot design",
		data_type = "experiment",
		treatment_vars = "N_fertilizer",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-07-21",
		carob_completion = 80,	
		carob_effort = 6
	)
	

	f1 <- ff[basename(ff) == "LCC SPAD Biomass DS 2017.xlsx"]
	f2 <- ff[basename(ff) == "LTCCE _EP DS 2016 LCC SPAD biomass final 7mar17.xlsx"]
	f3 <- ff[basename(ff) == "LTCCE_EP_ EWS 2015 LCC SPAD biomass.xlsx"]
	f4 <- ff[basename(ff) == "LTCCE_EP_DS 2016 yield.xlsx"]
	f5 <- ff[basename(ff) == "LTCCE_EP_EWS 2015 yield.xlsx"]
	f6 <- ff[basename(ff) == "LTCCE_EP_EWS 2016 LCC SPAD.xlsx"]
	f7 <- ff[basename(ff) == "LTCCE_EP_EWS 2016 yield.xlsx"]
  
	
	### process 
	shaper <- function(d) {
	  nms <- names(d)
	  e <- reshape(d, varying = nms, times=nms, v.names="value", direction = "long", timevar="variable")
	  e$value <- as.numeric(e$value)
	  v <- do.call(rbind, strsplit(e$variable, "_"))
	  e$DAP <- as.integer(v[,2])
	  e$variable <- v[,1]
	  e <- reshape(e, timevar ="variable", idvar = c("id", "DAP"), direction = "wide")
	  names(e) <- gsub("^value.", "", names(e))
	  rownames(e) <- NULL
	  e
	}
	
	### process f1
	
	r1 <- carobiner::read.excel.hdr(f1, skip=4, hdr=2, sheet="SUMMARY")
	
	d1 <- data.frame(
	  season = r1$Year.Season,
	  trial_id = r1$ID,
	  plot_id = r1$Plot.no,
	  variety = r1$Variety.Name,
	  N_fertilizer = r1$Nrate.kg.N.ha,
	  rep = r1$Rep,
	  LAI_23 = r1$LEAF.AREA.INDEX_23.DAT,
	  LAI_43 = r1$X43.DAT,
	  LAI_57 = r1$X57.DAT,
	  LAI_68 = r1$X68.DAT,
	  spad_23 = r1$SPAD.MEASUREMENT_23,
	  spad_29 = r1$X29.DAT,
	  spad_36 = r1$X36.DAT,
	  spad_43 = r1$X43.DAT.1,
	  spad_50 = r1$X50.DAT,
	  spad_57 = r1$X57.DAT.1,
	  spad_64 = r1$X64DAT,
	  spad_71 = r1$X71.DAT,
	  spad_78 = r1$X78.DAT,
	  dmy_23 = r1$PLANT.DRY.WEIGHT.kg.ha._23.DAT,
	  dmy_43 = r1$X43.DAT.4,
	  dmy_57 = r1$X57.DAT.4,
	  dmy_68 = r1$X68.DAT.2
	)
	
 ### biomass from f1 
	  r2 <- carobiner::read.excel.hdr(f1, skip = 2, hdr =2 , sheet="Biomass ") 
	  
	  d2 <- data.frame(
	    season = r2$Year.Season,
	    trial_id = r2$ID,
	    plot_id = r2$Plot.no,
	    variety = r2$Variety.Name,
	    N_fertilizer = r2$Nrate.kg.N.ha,
	    rep = r2$Rep,
	    dmy.stems_23 = r2$Stem.dry.wt.kg.ha,
	    dmy.stems_43 = r2$Stem.dry.wt.kg.ha.1,
	    dmy.stems_57 = r2$Stem.dry.wt.kg.ha.2,
	    dmy.stems_68 = r2$Stem.dry.wt.kg.ha.3,
	    dmy.leaves_23 = r2$Green.leaf.dry.wt.kg.ha,
	    dmy.leaves_43 = r2$Green.leaf.dry.wt.kg.ha.1,
	    dmy.leaves_57 = r2$Green.leaf.dry.wt.kg.ha.2,
	    dmy.leaves_68 = r2$Green.leaf.dry.wt.kg.ha.3
	  )
	  
	  ### merge d11 and d12
	  d2 <- d2[!is.na(d2$season),]
	  d12 <- merge(d1, d2, by = intersect(names(d1), names(d2)), all = TRUE)
	  d12$id <- as.integer(1: nrow(d12))
	  
	  cols <- grep("LA|spad|dmy", names(d12))
	  g1 <- shaper(d12[, cols])
	  d12 <- d12[, -cols]
	  g1 <- merge(d12, g1, by= "id", all.y = TRUE)
	  ## dmy.leaves and dmy.stems
	  names(g1) <- gsub("dmy.", "dmy_", names(g1)) 
	  
	  ########### process f2
	  
	  r3 <- carobiner::read.excel.hdr(f2, skip = 4, hdr =3 , sheet="SUMMARY") 
	  d3 <- data.frame(
	    season = r3$Year.Season,
	    trial_id = r3$ID,
	    plot_id = r3$Plot.no,
	    variety = r3$Variety.Name,
	    N_fertilizer = r3$Nrate.kg.N.ha,
	    rep = r3$Rep,
	    LAI_23 = r3$LEAF.AREA.INDEX_23.DAT,
	    LAI_37 = r3$X37.DAT,
	    LAI_64 = r3$X64.DAT,
	    spad_23 = r3$SPAD.MEASUREMENT_23.DAT,
	    spad_30 = r3$X30.DAT,
	    spad_37 = r3$X37.DAT.1,
	    spad_43 = r3$X43.DAT,
	    spad_50 = r3$X50.DAT,
	    spad_57 = r3$X57.DAT,
	    spad_64 = r3$X64.DAT.1,
	    spad_71 = r3$X71.DAT,
	    spad_78 = r3$X78.DAT,
	    dmy_23 = r3$PLANT.DRY.WEIGHT.kg.ha._23.DAT,
	    dmy_43 = r3$X43.DAT.3,
	    dmy_56 = r3$X56.DAT.1,
	    dmy_65 = r3$X65.DAT.1,
	    id = as.integer(1:nrow(r3))
	    
	  )
	  
	  cols <- grep("LA|spad|dmy", names(d3))
	  g2 <- shaper(d3[, cols])
	  d3 <- d3[, -cols]
	  g2 <- merge(d3, g2, by= "id", all.y = TRUE)
	  #### process f3
	  
	  r4 <- carobiner::read.excel.hdr(f3, skip= 4, hdr = 3,  sheet="SUMMARY")
	  
	  d4 <- data.frame(
	    season = r4$Year.Season,
	    trial_id = r4$ID,
	    plot_id = r4$Plot.no,
	    variety = r4$Variety.Name,
	    N_fertilizer = r4$Nrate.kg.N.ha,
	    rep = r4$Rep,
	    LAI_23 = r4$LEAF.AREA.INDEX_30.DAT,
	    LAI_42 = r4$X42.DAT,
	    LAI_64 = r4$X64.DAT,
	    spad_23 = r4$SPAD.MEASUREMENT_23.DAT,
	    spad_30 = r4$X30.DAT,
	    spad_36 = r4$X36.DAT,
	    spad_42 = r4$X42.DAT.1,
	    spad_49 = r4$X49.DAT,
	    spad_56 = r4$X56.DAT,
	    spad_64 = r4$X64.DAT.2,
	    spad_71 = r4$X71.DAT,
	    spad_78 = r4$X78.DAT,
	    dmy_22 = r4$PLANT.DRY.WEIGHT.kg.ha._22.DAT,
	    dmy_42 = r4$X42.DAT.4,
	    dmy_56 = r4$X56.DAT.3,
	    dmy_65 = r4$X65.DAT.1,
	    id = as.integer(1: nrow(r4))
	  ) 
	  
	  cols <- grep("LA|spad|dmy", names(d4))
	  g3 <- shaper(d4[, cols])
	  d4 <- d4[, -cols]
	  g3 <- merge(d4,g3, by= "id", all.y = TRUE)
	  
	  #### process f6
	  r5 <- carobiner::read.excel.hdr(f6, skip = 4, hdr = 3,  sheet="SUMMARY")
    r5 <- r5[!is.na(r5$Experiment),]
	  d5 <- data.frame(
	    season = r5$Year.Season,
	    trial_id = r5$ID,
	    plot_id = r5$Plot.no,
	    variety = r5$Variety.Name,
	    N_fertilizer = r5$Nrate.kg.N.ha,
	    rep = r5$Rep,
	    LAI_23 = r5$LEAF.AREA.INDEX_22.DAT,
	    LAI_42 = r5$X42.DAT,
	    LAI_55 = r5$X55.DAT,
	    LAI_62 = r5$X62.DAT,
	    spad_23 = r5$SPAD.MEASUREMENT_22.DAT,
	    spad_29 = r5$X29.DAT,
	    spad_36 = r5$X36.DAT,
	    spad_42 = r5$X42.DAT.1,
	    spad_49 = r5$X49.DAT,
	    spad_56 = r5$X56.DAT,
	    spad_63 = r5$X63.DAT,
	    spad_71 = r5$X71.DAT,
	    spad_78 = r5$X78.DAT.1,
	    dmy_22 = r5$PLANT.DRY.WEIGHT.kg.ha._22.DAT,
	    dmy_42 = r5$X42.DAT.4,
	    dmy_55 = r5$X55.DAT.2,
	    dmy_62 = r5$X62.DAT.2,
	    id = as.integer(1: nrow(r5))
	  ) 
	  
	  cols <- grep("LA|spad|dmy", names(d5))
	  g4 <- shaper(d5[, cols])
	  d5 <- d5[, -cols]
	  g4 <- merge(d5, g4, by = 'id', all.y = TRUE)
	  
	  g <- carobiner::bindr(g1, g2, g3, g4)
	  names(g) <- gsub("dmy$", "dmy_total", names(g))
	  g$planting_date <- ifelse(grepl("2017", g$season), "2017", "2016") 
	  
	  g$id <- NULL
	
	  #------------ process grain yield files------------------------------
	  #### process f4
	 
	  r6 <- carobiner::read.excel.hdr(f4, , skip = 10, hdr = 9, sheet = "LTCCE_GY&YC RJB")

	  d6 <- data.frame(
	    trial_id = r6$LTE_Year.Season_Cropping.Number_Barcode.ID_ID,
	    plot_id = r6$Plot.Number_PlotNo,
	    N_fertilizer = r6$N.Rate_N.Rate,
	    variety = r6$Variety_Variety,
	    rep = r6$Rep_Rep,
	    yield_14 = as.numeric(r6$Grain.yield.14pct.t.ha)*1000,
	    yield_3 = as.numeric(r6$Grain.Yield.at.3pct)*1000,
	    dmy_total = as.numeric(r6$Total.DM.t.ha)*1000,
	    plant_height = r6$Plant.Ht.cm_PLTHTx,
	    harvest_index = r6$Harvest.index.2_HI2,
	    harvest_days = r6$Growth.duration,
	    DAP = as.integer(r6$Growth.duration),
	    dmy_residue = r6$Straw.yield.kg.ha._SYKHA,
	    grain_N = r6$Grain.N.kg.ha,
	    residue_N = r6$Straw.N.kg.ha,
	    planting_date = as.character(r6$Seedling.Date),
	    harvest_date = as.character(r6$Harvest.Date.dd.mmm.yyyy._HarvDate)
	  )
    
	  #### remove empty rows
	  d6 <- d6[!is.na(d6$plot_id),]
	  
	  #### process f5
	  
	  r7 <- carobiner::read.excel.hdr(f5, skip= 10, hdr= 3, sheet = "LTCCE_GY&YC RJB")
	  
	  d7 <- data.frame(
	    trial_id = r7$Barcode.ID_ID,
	    plot_id = r7$Plot.Number_PlotNo,
	    N_fertilizer = r7$N.Rate_N.Rate,
	    variety = r7$Variety_Variety,
	    rep = r7$Rep_Rep,
	    yield_14 = as.numeric(r7$Grain.yield.14pct.t.ha)*1000,
	    yield_3 = as.numeric(r7$Grain.Yield.at.3pct),
	    dmy_total = as.numeric(r7$Total.DM.t.ha)*1000,
	    plant_height = r7$Plant.Ht.cm_PLTHTx,
	    dmy_residue = r7$Straw.N.kg.ha,
	    harvest_index = r7$Harvest.index.2_HI2,
	    harvest_days = r7$Growth.duration,
	    DAP = as.integer(r7$Growth.duration),
	    grain_N = r7$Grain.N.kg.ha,
	    residue_N = r7$Straw.N.kg.ha,
	    planting_date = as.character(r7$Seedling.Date),
	    harvest_date = as.character(r7$Harvest.Date.dd.mmm.yyyy._HarvDate)
	  )
	  
	  ### process f7
	  r8 <- carobiner::read.excel.hdr(f7, skip= 10, hdr= 2, sheet = "LTCCE_GY&YC RJB")
	  
	  d8 <- data.frame(
	    trial_id = r8$Barcode.ID_ID,
	    plot_id = r8$Plot.Number_PlotNo,
	    N_fertilizer = r8$N.Rate_N.Rate,
	    variety = r8$Variety_Variety,
	    rep = r8$Rep_Rep,
	    yield_14 = as.numeric(r8$Grain.yield.14pct.t.ha)*1000,
	    yield_3 = as.numeric(r8$Grain.Yield.at.3pct),
	    plant_height = r8$Plant.Ht.cm_PLTHTx,
	    dmy_residue = r8$Straw.yield.kg.ha._SYKHA,
	    dmy_total = as.numeric(r8$Total.DM.t.ha)*1000,
	    harvest_index = r8$Harvest.index.2_HI2,
	    harvest_days = r8$Growth.duration,
	    DAP = as.integer(r8$Growth.duration),
	    grain_N = r8$Grain.N.kg.ha,
	    residue_N = r8$Straw.N.kg.ha,
	    planting_date = as.character(r8$Seedling.Date),
	    harvest_date = as.character(r8$Harvest.Date.dd.mmm.yyyy._HarvDate)
	  )
	  
	  #### remove empty rows
	  d8 <- d8[!is.na(d8$plot_id),]
	  
	  dy <- carobiner::bindr(d6, d7, d8)
	  
	  dy <- reshape(dy, varying = c("yield_14", "yield_3"), v.names = "yield", timevar = "yield_moisture", times = c(14, 3), direction = "long")
	  dy$id <- NULL
	  
	  ### merge data with yield 
	  
	  d <- merge(dy, g, by = intersect(names(dy), names(g)), all = TRUE)
	  
	  i <- grepl("EWS", d$trial_id)
	  d$season <- "dry"
	  d$season[i] <- "wet"
	  d$rep <- as.integer(d$rep)
	  cols <- c("dmy_residue","N_fertilizer", "dmy_total", "LAI", "dmy_stems", "dmy_leaves", "plant_height", "harvest_index", "grain_N", "residue_N")
	  d[cols] <- sapply(d[cols], as.numeric)
	  
	  ######
	  d$country <- "Philippines"
	  d$location <- "Laguna, Los Baños, IRRI"
	  d$latitude <- 14.167
	  d$longitude <- 121.254
	  
	  d$crop <- "rice"
	  d$is_survey <- FALSE
	  d$on_farm <- TRUE
	  d$yield_part <- "grain"
	  d$geo_from_source <- FALSE
	  d$irrigated <- NA  
	  
	  d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	  
	  ### keep DAP depending variables in the long format
	  d$record_id <- as.integer(1:nrow(d))
	  
	  cols <- grep("LA|spad|record_id|DAP|dmy", names(d)) 
	  d_lon <- d[, cols]
	  d_lon <- d_lon[!is.na(d_lon$DAP),]
	  
	  col <- grep("LA|spad|DAP|dmy", names(d)) 
	  d <- d[, -col]
	  
	carobiner::write_files(path, meta, d, long = d_lon)
}


