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
  
	
	### process f1
	r1 <- carobiner::read.excel(f1, sheet="SUMMARY")
	hdr = r1[3:4,]
	names(r1) <- apply(hdr, 2, function(x) {
	  x <- trimws(as.character(x))
	  x <- x[!is.na(x) & x != ""]
	  if (length(x)) x[1] else NA_character_
	})
	r1 <- r1[-(1:4),] ### header
	names(r1) <- make.names(names(r1), unique = TRUE)
	
	d11 <- data.frame(
	  season = r1$Year.Season,
	  trial_id = r1$ID,
	  plot_id = r1$Plot.no,
	  variety = r1$Variety.Name,
	  N_fertilizer = r1$Nrate..kg.N.ha.,
	  rep = r1$Rep,
	  LA_23 = r1$LEAF.AREA.INDEX,
	  LA_29 = NA,
	  LA_36 = NA,
	  LA_43 = r1$X43.DAT,
	  LA_50 = NA,
	  LA_57 = r1$X57.DAT,
	  LA_64 = NA,
	  LA_68 = r1$X68.DAT,
	  LA_71 = NA,
	  LA_78 = NA,
	  spad_23 = NA,
	  spad_29 = r1$X29.DAT,
	  spad_36 = r1$X36.DAT,
	  spad_43 = r1$X43.DAT.1,
	  spad_50 = r1$X50.DAT,
	  spad_57 = r1$X57.DAT.1,
	  spad_64 = r1$X64DAT,
	  spad_68 = NA,
	  spad_71 = r1$X71.DAT,
	  spad_78 = r1$X78.DAT,
	  dmy_total_23 = r1$PLANT.DRY.WEIGHT..kg.ha.,
	  dmy_total_43 = r1$X43.DAT.4,
	  dmy_total_57 = r1$X57.DAT.4,
	  dmy_total_68 = r1$X68.DAT.2,
	  dmy_total_29 = NA,
	  dmy_total_36 = NA,
	  dmy_total_50 = NA,
	  dmy_total_64 = NA,
	  dmy_total_71 = NA,
	  dmy_total_78 = NA
	)
	var1 <- paste0("LA_", c("23", "29", "36", "43", "50", "57", "64", "68", "71", "78"))
	var2 <- paste0("spad_", c("23", "29", "36", "43", "50", "57", "64", "68", "71", "78")) 
	var3 <-  paste0("dmy_total_", c("23", "29", "36", "43", "50", "57", "64", "68", "71", "78")) 
	d11 <- reshape(d11, varying = list(var1, var2, var3), v.names = c("LAI", "spad", "dmy_total"), 
	              times = c(23L, 29L, 36L, 43L, 50L, 57L, 64L, 68L, 71L, 78L),
	              timevar = "DAP",
	              direction = "long")
	
	d11$id <- NULL
	  
	  ### biomass from f1 
	  r4 <- carobiner::read.excel(f1, sheet="Biomass ")
	  hdr = r4[1:2,]
	  names(r4) <- apply(hdr, 2, function(x) {
	    x <- trimws(as.character(x))
	    x <- x[!is.na(x) & x != ""]
	    if (length(x)) x[1] else NA_character_
	  })
	  r4 <- r4[-(1:2),] ### header
	  names(r4) <- make.names(names(r4), unique = TRUE)
	  
	  d12 <- data.frame(
	    season = r4$Year.Season,
	    trial_id = r4$ID,
	    plot_id = r4$Plot.no,
	    variety = r4$Variety.Name,
	    N_fertilizer = r4$Nrate..kg.N.ha.,
	    rep = r4$Rep,
	    dmy_stems_23 = r4$Stem.dry.wt.kg.ha,
	    dmy_stems_43 = r4$Stem.dry.wt.kg.ha.1,
	    dmy_stems_57 = r4$Stem.dry.wt.kg.ha.2,
	    dmy_stems_68 = r4$Stem.dry.wt.kg.ha.3,
	    dmy_stems_78 = r4$Stem.dry.wt.kg.ha.4,
	    dmy_leaves_23 = r4$Green.leaf.dry.wt.kg.ha,
	    dmy_leaves_43 = r4$Green.leaf.dry.wt.kg.ha.1,
	    dmy_leaves_57 = r4$Green.leaf.dry.wt.kg.ha.2,
	    dmy_leaves_68 = r4$Green.leaf.dry.wt.kg.ha.3,
	    dmy_leaves_78 = r4$Green.leaf.dry.wt.kg.ha.4 
	  )
	  vars1 = paste0("dmy_stems_", c("23", "43", "57", "68", "78"))
	  vars2 = paste0("dmy_leaves_", c("23", "43", "57", "68", "78"))
	  d12 <- reshape(d12, varying = list(vars1, vars2), v.names = c("dmy_stems", "dmy_leaves"), times = c(23L, 43L, 57L, 68L, 78L), timevar = "DAP",direction = "long")
	  d12$id <- NULL
	  ### merge d11 and d12
	  d1 <- merge(d11, d12, by = intersect(names(d11), names(d12)), all = TRUE)
	  
	 
	  ########### process f2
	  
	  r7 <- carobiner::read.excel(f2, sheet="SUMMARY")
	  hdr = r7[3:4,]
	  names(r7) <- apply(hdr, 2, function(x) {
	    x <- trimws(as.character(x))
	    x <- x[!is.na(x) & x != ""]
	    if (length(x)) x[1] else NA_character_
	  })
	  r7 <- r7[-(1:4),] ### header
	  names(r7) <- make.names(names(r7), unique = TRUE)
	  
	  d2 <- data.frame(
	    season = r7$Year.Season,
	    trial_id = r7$ID,
	    plot_id = r7$Plot.no,
	    variety = r7$Variety.Name,
	    N_fertilizer = r7$Nrate..kg.N.ha.,
	    rep = r7$Rep,
	    LAI_23 = r7$LEAF.AREA.INDEX,
	    LAI_37 = r7$X37.DAT,
	    LAI_64 = r7$X64.DAT,
	    LAI_30 = NA,
	    LAI_43 = NA,
	    LAI_50 = NA,
	    LAI_57 = NA,
	    LAI_71 = NA,
	    LAI_78 = NA,
	    LAI_56 = NA,
	    LAI_65 = NA,
	    spad_23 = r7$SPAD.MEASUREMENT,
	    spad_30 = r7$X30.DAT,
	    spad_37 = r7$X37.DAT.1,
	    spad_43 = r7$X43.DAT,
	    spad_50 = r7$X50.DAT,
	    spad_57 = r7$X57.DAT,
	    spad_64 = r7$X64.DAT.1,
	    spad_71 = r7$X71.DAT,
	    spad_78 = r7$X78.DAT,
	    spad_56 = NA,
	    spad_65 = NA,
	    dmy_total_23 = r7$PLANT.DRY.WEIGHT..kg.ha.,
	    dmy_total_43 = r7$X43.DAT.3,
	    dmy_total_56 = r7$X56.DAT.1,
	    dmy_total_65 = r7$X65.DAT.1,
	    dmy_total_30 = NA,
	    dmy_total_37 = NA,
	    dmy_total_50 = NA,
	    dmy_total_57 = NA,
	    dmy_total_64 = NA,
	    dmy_total_71 = NA,
	    dmy_total_78 = NA
	  )
	  
	  var1 <- paste0("LAI_", c("23", "30", "37", "43", "50", "57", "64", "71", "78", "56", "65"))
	  var2 <- paste0("spad_", c("23", "30", "37", "43", "50", "57", "64", "71", "78", "56", "65")) 
	  var3 <- paste0("dmy_total_", c("23", "30", "37", "43", "50", "57", "64", "71", "78", "56", "65")) 
	  d2 <- reshape(d2, varying = list(var1, var2, var3), v.names = c("LAI", "spad", "dmy_total"), 
	                times = c(23L, 30L, 37L, 43L, 50L, 57L, 64L, 71L, 78L, 56L, 65L),
	                timevar = "DAP",
	                direction = "long")
	  
	  d2$id <- NULL
	  
	  
	  #### process f3
	  
	  r15 <- carobiner::read.excel(f3, sheet="SUMMARY")
	  hdr = r15[3:4,]
	  names(r15) <- apply(hdr, 2, function(x) {
	    x <- trimws(as.character(x))
	    x <- x[!is.na(x) & x != ""]
	    if (length(x)) x[1] else NA_character_
	  })
	  r15 <- r15[-(1:4),] ### header
	  names(r15) <- make.names(names(r15), unique = TRUE)
	  
	  d3 <- data.frame(
	    season = r15$Year.Season,
	    trial_id = r15$ID,
	    plot_id = r15$Plot.no,
	    variety = r15$Variety.Name,
	    N_fertilizer = r15$Nrate..kg.N.ha.,
	    rep = r15$Rep,
	    LAI_23 = r15$LEAF.AREA.INDEX,
	    LAI_42 = r15$X42.DAT,
	    LAI_64 = r15$X64.DAT,
	    LAI_30 = NA, 
	    LAI_65 = NA, 
	    LAI_36 = NA, 
	    LAI_49 = NA, 
	    LAI_56 = NA, 
	    LAI_71 = NA, 
	    LAI_78 = NA, 
	    spad_23 = r15$SPAD.MEASUREMENT,
	    spad_30 = r15$X30.DAT,
	    spad_36 = r15$X36.DAT,
	    spad_42 = r15$X42.DAT.1,
	    spad_49 = r15$X49.DAT,
	    spad_56 = r15$X56.DAT,
	    spad_64 = r15$X64.DAT.2,
	    spad_65 = NA,
	    spad_71 = r15$X71.DAT,
	    spad_78 = r15$X78.DAT,
	    dmy_total_23 = r15$PLANT.DRY.WEIGHT..kg.ha.,
	    dmy_total_42 = r15$X42.DAT.4,
	    dmy_total_56 = r15$X56.DAT.3,
	    dmy_total_65 = r15$X65.DAT.1,
	    dmy_total_64 = NA,
	    dmy_total_30 = NA, 
	    dmy_total_36 = NA, 
	    dmy_total_49 = NA, 
	    dmy_total_71 = NA, 
	    dmy_total_78 = NA
	  ) 
	  
	  var1 <- paste0("LAI_", c("23", "30", "36", "42", "49", "56", "64", "65", "71", "78"))
	  var2 <- paste0("spad_", c("23", "30", "36", "42", "49", "56", "64", "65", "71", "78")) 
	  var3 <- paste0("dmy_total_", c("23", "30", "36", "42", "49", "56", "64", "65", "71", "78")) 
	  d3 <- reshape(d3, varying = list(var1, var2, var3), v.names = c("LAI", "spad", "dmy_total"), 
	                times = c(23L, 30L, 36L, 42L, 49L, 56L, 64L, 65L, 71L, 78L),
	                timevar = "DAP",
	                direction = "long")
	  d3$id <- NULL
	 
	  #### f6
	  r42 <- carobiner::read.excel(f6, sheet="SUMMARY")
	  hdr = r42[3:4,]
	  names(r42) <- apply(hdr, 2, function(x) {
	    x <- trimws(as.character(x))
	    x <- x[!is.na(x) & x != ""]
	    if (length(x)) x[1] else NA_character_
	  })
	  r42 <- r42[-(1:4),] ### header
	  
	  names(r42) <- make.names(names(r42), unique = TRUE)
	  
	  d4 <- data.frame(
	    season = r42$Year.Season,
	    exp = r42$Experiment,
	    trial_id = r42$ID,
	    plot_id = r42$Plot.no,
	    variety = r42$Variety.Name,
	    N_fertilizer = r42$Nrate..kg.N.ha.,
	    rep = r42$Rep,
	    LAI_23 = r42$LEAF.AREA.INDEX,
	    LAI_42 = r42$X42.DAT,
	    LAI_55 = r42$X55.DAT,
	    LAI_62 = r42$X62.DAT,
	    LAI_29 = NA, 
	    LAI_36 = NA, 
	    LAI_63 = NA, 
	    LAI_56 = NA, 
	    LAI_49 = NA, 
	    LAI_71 = NA, 
	    LAI_78 = NA, 
	    spad_23 = r42$SPAD.MEASUREMENT,
	    spad_29 = r42$X29.DAT,
	    spad_36 = r42$X36.DAT,
	    spad_42 = r42$X42.DAT.1,
	    spad_49 = r42$X49.DAT,
	    spad_56 = r42$X56.DAT,
	    spad_63 = r42$X63.DAT,
	    spad_71 = r42$X71.DAT,
	    spad_78 = r42$X78.DAT.1,
	    spad_55 = NA,
	    spad_62 = NA,
	    dmy_total_23 = r42$PLANT.DRY.WEIGHT..kg.ha.,
	    dmy_total_42 = r42$X42.DAT.4,
	    dmy_total_55 = r42$X55.DAT.2,
	    dmy_total_62 = r42$X62.DAT.2,
	    dmy_total_29 = NA, 
	    dmy_total_36 = NA, 
	    dmy_total_49 = NA, 
	    dmy_total_56 = NA, 
	    dmy_total_63 = NA, 
	    dmy_total_71 = NA, 
	    dmy_total_78 = NA
	  ) 
	  
	  var1 <- paste0("LAI_", c("23", "42", "55", "62", "29", "36", "49", "56", "63", "71", "78"))
	  var2 <- paste0("spad_", c("23", "42", "55", "62", "29", "36", "49", "56", "63", "71", "78")) 
	  var3 <- paste0("dmy_total_", c("23", "42", "55", "62", "29", "36", "49", "56", "63", "71", "78")) 
	  d4 <- reshape(d4, varying = list(var1, var2, var3), v.names = c("LAI", "spad", "dmy_total"), 
	                times = c(23L, 42L, 55L, 62L, 29L, 36L, 49L, 56L, 63L, 71L, 78L),
	                timevar = "DAP",
	                direction = "long")
	  d4 <- d4[!is.na(d4$exp),]
	  d4$id <- d4$exp <- NULL
	  
	  dd <- carobiner::bindr(d1, d2, d3, d4)
	  dd$planting_date <- ifelse(grepl("2017", dd$season), "2017", "2016") 
	  
	  #------------ process grain yield files------------------------------
	  #### process f4
	  r30 <- carobiner::read.excel(f4, sheet = "LTCCE_GY&YC RJB")
	  hdr = r30[10:9,]
	  names(r30) <- apply(hdr, 2, function(x) {
	    x <- trimws(as.character(x))
	    x <- x[!is.na(x) & x != ""]
	    if (length(x)) x[1] else NA_character_
	  })
	  r30 <- r30[-(1:10),] ### header
	  names(r30) <- make.names(names(r30), unique = TRUE)
	  
	  d5 <- data.frame(
	    trial_id = r30$ID,
	    plot_id = r30$PlotNo,
	    N_fertilizer = r30$N.Rate,
	    variety = r30$Variety,
	    rep = r30$Rep,
	    yield_14 = as.numeric(r30$Grain.yield.14...t.ha.)*1000,
	    yield_3 = as.numeric(r30$Grain.Yield.at.3.)*1000,
	    dmy_total = as.numeric(r30$Total.DM..t.ha.)*1000,
	    plant_height = r30$PLTHTx,
	    harvest_index = r30$HI2,
	    dmy_residue = r30$SYKHA,
	    grain_N = r30$Grain.N..kg.ha.,
	    residue_N = r30$Straw.N..kg.ha.,
	    planting_date = as.character(as.Date(as.numeric(r30$Seedling.Date), "1899-12-30")),
	    harvest_date = as.character(as.Date(as.numeric(r30$HarvDate), "1899-12-30"))
	  )
    
	  #### remove empty rows
	  d5 <- d5[!is.na(d5$plot_id),]
	  
	  #### process f5
	  r41 <- carobiner::read.excel(f5, sheet = "LTCCE_GY&YC RJB")
	  hdr = r41[10:9,]
	  names(r41) <- apply(hdr, 2, function(x) {
	    x <- trimws(as.character(x))
	    x <- x[!is.na(x) & x != ""]
	    if (length(x)) x[1] else NA_character_
	  })
	  r41 <- r41[-(19:1),] ### header
	  names(r41) <- make.names(names(r41), unique = TRUE)
	  
	  d6 <- data.frame(
	    trial_id = r41$ID,
	    plot_id = r41$PlotNo,
	    N_fertilizer = r41$N.Rate,
	    variety = r41$Variety,
	    rep = r41$Rep,
	    yield_14 = as.numeric(r41$Grain.yield.14...t.ha.)*1000,
	    yield_3 = as.numeric(r41$Grain.Yield.at.3.),
	    dmy_total = as.numeric(r41$Total.DM..t.ha.)*1000,
	    plant_height = r41$PLTHTx,
	    dmy_residue = r41$Straw.N..kg.ha.,
	    harvest_index = r41$HI2,
	    grain_N = r41$Grain.N..kg.ha.,
	    residue_N = r41$Straw.N..kg.ha.,
	    planting_date = as.character(as.Date(as.numeric(r41$Seedling.Date), "1899-12-30")),
	    harvest_date = as.character(as.Date(as.numeric(r41$HarvDate), "1899-12-30"))
	  )
	  
	  #### remove empty rows
	  d6 <- d6[!is.na(d6$plot_id),]
	  
	  ### process f7
	  r57 <- carobiner::read.excel(f7,  sheet = "LTCCE_GY&YC RJB")
	  hdr = r57[10:9,]
	  names(r57) <- apply(hdr, 2, function(x) {
	    x <- trimws(as.character(x))
	    x <- x[!is.na(x) & x != ""]
	    if (length(x)) x[1] else NA_character_
	  })
	  r57 <- r57[-(1:10),] ### header
	  names(r57) <- make.names(names(r57), unique = TRUE)
	  
	  d7 <- data.frame(
	    trial_id = r57$ID,
	    plot_id = r57$PlotNo,
	    N_fertilizer = r57$N.Rate,
	    variety = r57$Variety,
	    rep = r57$Rep,
	    yield_14 = as.numeric(r57$Grain.yield.14...t.ha.)*1000,
	    yield_3 = as.numeric(r57$Grain.Yield.at.3.),
	    plant_height = r57$PLTHTx,
	    dmy_residue = r57$SYKHA,
	    dmy_total = as.numeric(r57$Total.DM..t.ha.)*1000,
	    harvest_index = r57$HI2,
	    grain_N = r57$Grain.N..kg.ha.,
	    residue_N = r57$Straw.N..kg.ha.,
	    planting_date = as.character(as.Date(as.numeric(r57$Seedling.Date), "1899-12-30")),
	    harvest_date = as.character(as.Date(as.numeric(r57$HarvDate), "1899-12-30"))
	  )
	  
	  #### remove empty rows
	  d7 <- d7[!is.na(d7$plot_id),]
	  
	  dd1 <- carobiner::bindr(d5, d6, d7)
	  
	  dd1 <- reshape(dd1, varying = c("yield_14", "yield_3"), v.names = "yield", timevar = "yield_moisture", times = c(14, 3), direction = "long")
	  dd1$id <- NULL
	  ### merge data with yield 
	  
	  d <- merge(dd, dd1, by = intersect(names(dd1), names(dd)), all = TRUE)
	  d <- d[!is.na(d$N_fertilizer),]
	  
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
	  
	  d_lon <- d[, c("LAI", "spad", "dmy_stems", "dmy_leaves", "DAP", "record_id", "dmy_total")]
	  d_lon <- d_lon[!is.na(d_lon$DAP),]
	  
	  d <- d[, !names(d)%in% c("LAI", "spad", "dmy_stems", "dmy_leaves", "DAP", "dmy_total")]
	  
	carobiner::write_files(path, meta, d, long = d_lon)
}


