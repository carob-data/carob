# R script for "carob"

carob_script <- function(path) {
  
"This trial is designed with 1 conventional farmers practice and 4 conservation agriculture (CA) treatments in 5 replications; Plots are subdivided into a continues maize area and a maize/legume (sunnhemp) rotation to investigate the effect of CA practices on soil quality and system productivity. The trial was set in the growing season of 2005 and is still running through to 2017 and beyond. The treatments are as follows: 
T1. Conventional mouldboard ploughing (CPM): maize with residue removal, manual seeding and fertilization in the tilled seedbed after ploughing. Plots are subdivided into split plots with continues maize and a maize/sunnhemp rotation 
T2. Sub-soiling with a Magoye ripper (RIM): maize with residue retention, manual seeding and fertilization in the ripping line. Plots are subdivided into split plots with continues maize and a maize/sunnhemp rotation 
T3. Direct seeding (DSM) with a Fitarelli Jabplanter: maize with residue retention, seeding and fertilization is carried out with the Jabplanter. Plots are subdivided into split plots with continues maize and a maize/sunnhemp rotation 
T4. Basin Planting (BAM): maize with residue retention, a manual system were basins (at 15cm x 15cm x 15cm spacing) are dug with hoes during the winter period and manually seeded and fertilized at the onset of rains. Plots are subdivided into split plots with continues maize and a maize/sunnhemp rotation 
T5. Magoye ripping (RI-ML): maize with residue retention, intercropped with cowpea (Vigna unguiculata) at seeding of maize. Plots are subdivided into split plots with continues maize/cowpea pea and a maize/cowpea//sunnh emp rotation."

  uri <- "hdl:11529/10843"
  group <- "agronomy"
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
    project=NA,
    publication= "doi:10.2135/cropsci2014.11.0796",
    data_organization = "CIMMYT",
    treatment_vars = "land_prep_method;residue_prevcrop_used;intercrops;crop_rotation",
    response_vars="yield",
    data_type="experiment",
    carob_contributor="Siyabusa Mkuhlani;Fredy Chimire", 
    carob_date="2024-07-20"
  )
  
  f <- ff[basename(ff) == "Henderson 2005.2016.xlsx"]
  
  # first sheet
  r1 <- carobiner::read.excel(f, sheet="All Maize yields Henderson")
  d1 <- data.frame(
    trial_id = (paste0(r1$Location, r1$'Harvest year')), 
    planting_date = as.character(r1$'Harvest year'-1), #Subtracting 1 because the harvest year is the following year after planting.
    country = r1$Country,
    location=r1$Location,
    rep=as.integer(r1$Rep),
    crop=tolower(r1$Crop), 
    treatment=r1$Label,
    dmy_residue=r1$'Non-cob Biomass',
    yield=r1$'Grainy ield',
    yield_part = "grain")
  
  # second sheet
  r2 <- carobiner::read.excel(f, sheet="Maize HRS Mz-SH rotation")
  d2 <- data.frame(
    trial_id = (paste0(r2$Location, r2$'Harvest year')), 
    planting_date = as.character(r2$'Harvest year'-1), #Subtracting 1 because the harvest year is the following year after planting.
    country = r2$Country,
    location=r2$Location,
    rep=as.integer(r2$Rep),
    crop=tolower(r2$Crop), 
    treatment=r2$Label,
    dmy_residue=r2$'Biomass yield (kg/ha)',
    yield=r2$'Grain yield (kg/ha)',
    yield_part = "grain")  
  
  # third sheet 
  r3 <- carobiner::read.excel(f, sheet="HRS sunnhemp yield")
  d3 <- data.frame(
    trial_id = (paste0(r3$Location, r3$Year)), 
    planting_date = as.character(r3$Year-1), #Subtracting 1 because the harvest year is the following year after planting.
    country = r3$Country,
    location=r3$Location,
    rep=as.integer(r3$Rep),
    crop=tolower(r3$Crop), 
    treatment=r3$Label,
	## assumed dmy based on paper
    dmy_residue=r3$'Biomass yield (kg/ha)',
    yield = r3$'Biomass yield (kg/ha)',
    yield_part = "aboveground biomass"
  )  
  
  #Specify information about the cropping systems
  d1$crop_rotation <- NA
  d1$intercrops <- NA
  d1$intercrops[d1$crop=="maize+mucuna"] <- "mucuna"
  d1$intercrops[d1$crop=="Maize+pigeonpea"] <- "pigeon pea"
  
  d2$crop_rotation <- NA
  d2$crop_rotation <- d2$crop
  d2$crop_rotation[d2$crop_rotation=="Maize +Pp"] <- "pigeon pea"
  d2$crop_rotation[d2$crop_rotation=="Maize +Cp"] <- "cowpea"
  d2$intercrops <- NA
  
  d3$crop_rotation <- NA
  d3$intercrops <- NA
  
  d <-rbind(d1,d2,d3)

    d$adm1 <- "Mashonaland Central"
    d$adm2 <- "Mazowe"
    d$longitude = 30.9869
    d$latitude = -17.5721

  
  d$crop[d$crop== 'sunnhemp'] <-"sunn hemp"
  d$crop[d$crop== 'maize +cp'] <-"maize"
  d$crop[d$crop== 'maize +pp'] <-"maize"
  d$crop[d$crop== 'maize+cowpea'] <-"maize"
  d$crop[d$crop== 'maize+mucuna'] <-"maize"
  d$crop[d$crop== 'maize+piegonpea'] <-"maize"
  d$crop[d$crop== 'maize+pigeonpea'] <-"maize"
  
  d$intercrops[d$intercrops== 'mucuna']<-"velvet bean"
  d$crop_rotation[d$crop_rotation== 'maize +cp']<- 'maize;cowpea'
  d$crop_rotation[d$crop_rotation== 'maize +pp']<- 'maize;pigeon pea'
  
  d$plot_area <- 18

  # fertilizer
  #Basal dressing 7:14:7 NPK for 150Kg/ha, meaning 10.5, 9.177 and 8.715 kg/ha,
  #for all treatments.Top dressing of 30kg N/ha, equally split applied twice. 
  d$fertilizer_type <- 'D-compound; AN'
  d$N_fertilizer <-NA
  # d$N_fertilizer[d$treatment=='CPM']<-10.5 + 15
  # d$N_fertilizer[d$treatment=='CPM']<-10.5 
  d$N_fertilizer <- ifelse(d$treatment=="CPM", 25.5,10.5)
  d$P_fertilizer <- 9.177
  d$K_fertilizer <- 8.715
  d$N_splits <- 2
  d$N_splits <- as.integer(d$N_splits)
  
  d$herbicide_used <- TRUE
  d$herbicide_product <- "glyphosate"
  
  d$on_farm <- FALSE
  d$is_survey <- FALSE
  d$irrigated <- FALSE
  
  d$land_prep_method <- NA
  d$planting_method <- NA
  
  d$planting_method[d$treatment %in% c("CP", "CPM", "CPR", "MCP", "BAM", "BAMR", "MBA", "RIM", "RIML", "MRMR",  "MRM", "MRMP", "MRMRPR", "MR")] <- "manual" 
  d$planting_method[d$treatment %in% c("DSM", "DSMR", "MDS", "MDSP")] <- "mechanized" 
  
  d$land_prep_method <- NA
  d$land_prep_method[d$treatment %in% c("CP", "CPM", "CPR", "MCP")] <- "conventional" 
  d$land_prep_method[d$treatment %in% c("BAM", "BAMR", "MBA")] <- "basins" 
  d$land_prep_method[d$treatment %in% c("RIM", "RIML", "MRMR", "MRM","MRMP","MRMRPR","MR")] <- "ripping" 
  d$land_prep_method[d$treatment %in% c("DSM", "DSMR", "MDS", "MDSP")] <- "reduced tillage" 
  
  d$residue_prevcrop_used <- TRUE
  d$residue_prevcrop_used[d$treatment %in% c("CP", "CPM", "CPR", "MCP")] <- FALSE
  
  #Replace treatment names in abbreviations with full names.
  treatcode = c("CPM","RIM","DSM","BAM","RIML","CPR","MRMR","DSMR","BAMR",
                "MRMRPR", "MCP","MR","MDS", "MBA","MDSP","CP","MRM","MRMP" )
  treatname = c("conventional mouldboard ploughing","Magoye ripper-maize resiude", "Direct seeding-animal drawn seeder", 
                "Basin","Magoye ripper-legume","Conventional mouldboard ploughing-residue",
                "Magoye ripper-maize rotation", "Direct seeding-animal drawn seeder rotation","Basin rotation",
                "Magoye ripper-legume rotation", "Conventional ploughing-residue","Magoye ripper-Maize rotation",
                "Direct seeding-animal drawn seeder","Basin","Direct seeding-animal drawn seeder",
                "Conventional mouldboard ploughing", "Magoye ripper","Magoye ripper-maize rotation")
  
  d$treatment <- treatname[match(d$treatment,treatcode)]
	d$geo_from_source <- FALSE
  
	d$yield_moisture <- as.numeric(NA) #needs to be checked

  carobiner::write_files(meta, d, path=path)
}


