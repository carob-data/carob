# R script for "carob"

## NOTES
# Plot-level measured wheat observations (Bacanora vs DH) at Buenos Aires, Ciudad Obregon, and Valdivia.
# Grain yield is kg DM/ha. Irrigation events are labeled "Model_estimate_to_mitigate_water_stress"; irrigated=TRUE but amounts not used.
# Fertilizer N is urea-ammonium nitrate (FE010) summed from Fertilizer_events. Simulations/model output folders were not used.

## ISSUES
# Irrigation mm are modeled, not measured; irrigation_amount not mapped.
# P and K not in Fertilizer_events (N only); omitted.
# Some seasons have a single plot (means?) rather than three replicates.
# Daily in-season crop observations (Obs_crop_daily_*) not processed as long records.
# Weather station files have "use limited, no secondary distribution" notes; included as source weather.
# write_files: P_fertilizer and K_fertilizer not in source; plant_density/seed_density exceed vocab max (source up to 370 plants/m2).


## NOT PROCESSED
# CIM_AgMIP.rar: not read. .rar file. Observation records are missing from d until this file is standardized.
# CIM_AgMIP.xlsx: not read. sheet ReadMe_format (of 30): Field Experiment Data Template, ...2. Observation records are missing from d until this file is standardized.
# CIM_AgMIP_measurements_key.txt: not read. columns: ICASA Dataset, ICASA Subset, ICASA Group, ICASA Subgroupe, Worksheet, Key, Code_Display, Variable_name, Description, Unit_or_type, Comments. Observation records are missing from d until this file is standardized.
# CIM_AgMIP_measurements_summary.txt: not read. columns: experiment_ID, treatment_ID, cultivar_identifier, planting_date, emergence_date, anthesis_date, physiologic_maturity_dat, harvest_date, harvest_no_at_maturity, grain_unit_dry_wt_matur, plant_density_emergence, plant_height_maturity, .... Observation records are missing from d until this file is standardized.
# CIM_AgMIP_Codes.txt: not read. columns: ICASA Dataset, ICASA Subset, ICASA Group, ICASA Subgroupe, Worksheet, Group/Topic, Code_Display, Code, Description, Comment. Observation records are missing from d until this file is standardized.
# CIM_AgMIP_Comments.txt: not read. columns: Worksheet, Identifier or variable, Query or comment, Sent to, Status, , , , , . Observation records are missing from d until this file is standardized.
# CIM_AgMIP_Crop_codes.txt: not read. columns: Code_display, Crop_code, Common_name, Latin_name, Crop_comments. Observation records are missing from d until this file is standardized.
# CIM_AgMIP_Dataset_description.txt: not read. columns: suite_id, suite_name, suite_objectives_of_study, suite_main_experiment_factor, suite_experimental_factor_comb, suite_experiment_type, suite_project_title, suite_project_acronym, suite_distrib_restrictions, suite_keywords, suite_email_address, suite_institute_name, .... Observation records are missing from d until this file is standardized.
# CIM_AgMIP_Definitions.txt: documentation or a codebook, not an observation table.
# CIM_AgMIP_Experiment_description.txt: not read. columns: experiment_ID, suite_id, people_level, institute_ID, name_of_experiment, ex_funding_source, exp_code_local, experiment_name_local, main_experiment_factor, experimental_factor_comb, experiment_type, site_name, .... Observation records are missing from d until this file is standardized.
# CIM_AgMIP_Genotypes.txt: not read. columns: cultivar_name, accession_id, accession_location, crop_ident_ICASA, cultivar_identifier, cultivar_class, breeding_program, cultivar_name_orig, cultivar_release_year, cultivar_notes. Observation records are missing from d until this file is standardized.
# CIM_AgMIP_Harvest_events.txt: not read. columns: experiment_ID, treatment_ID, harvest_operations_date, Harvest method, harvest_comments, , , . Observation records are missing from d until this file is standardized.
# CIM_AgMIP_Initial_condition_layers.txt: not read. columns: experiment_ID, treatment_ID, initial_conditions_date, soil_layer_top_depth, soil_layer_base_depth, initial_watr_conc_by_lyr, initial_Ntot_layer, initial_NH4_mass_layer, initial_NO3_mass_layer. Observation records are missing from d until this file is standardized.
# CIM_AgMIP_Institutions.txt: not read. columns: institute_ID,  institute_name, institute_type, institute_role. Observation records are missing from d until this file is standardized.
# CIM_AgMIP_Irrigation_events.txt: not read. columns: experiment_ID, treatment_ID, irrigation_date, irrigation_operation, irrigation_applic_depth, irrig_amount_depth, irrigation_notes, , , , , . Observation records are missing from d until this file is standardized.
# CIM_AgMIP_Obs_crop_daily_means.txt: not read. columns: experiment_ID, treatment_ID, cultivar_identifier, date_of_measurement, tops_dry_weight, leaf_area_index, cumulative_PAR_intercepted, PAR_intercepted_noon, stem_dry_weight, spike_dry_weight, , , .... Observation records are missing from d until this file is standardized.
# CIM_AgMIP_Obs_crop_daily_plots.txt: not read. columns: Plot ID, Experiment ID, Treatment ID, Cultivar name, Block, Plot number, Replicate, date_of_measurement, tops_dry_weight, leaf_area_index, cumulative_PAR_intercepted, PAR_intercepted_noon, .... Observation records are missing from d until this file is standardized.
# CIM_AgMIP_Obs_crop_summary_means.txt: not read. columns: experiment_ID, treatment_ID, cultivar_identifier, planting_date, emergence_date, anthesis_date, physiologic_maturity_dat, harvest_date, harvest_no_at_maturity, grain_unit_dry_wt_matur, plant_density_emergence, plant_height_maturity, .... Observation records are missing from d until this file is standardized.
# CIM_AgMIP_Persons.txt: not read. columns: people_level, institute_ID, researcher_first_name, researcher_last_name, researcher_role, e_mail_address. Observation records are missing from d until this file is standardized.
# CIM_AgMIP_Plots.txt: not read. columns: plot_id, experiment_ID, treatment_ID, cultivar_name, block_number, plot_number, replicate_number. Observation records are missing from d until this file is standardized.
# CIM_AgMIP_ReadMe_format.txt: documentation or a codebook, not an observation table.
# CIM_AgMIP_Residue.txt: not read. columns: experiment_ID, treatment_ID, init_residu_measure_date, residue_nature_prev_crop, residue_above_ground_wt, root_wt_previous_crop. Observation records are missing from d until this file is standardized.
# CIM_AgMIP_Soil_metadata.txt: documentation or a codebook, not an observation table.
# CIM_AgMIP_Soil_profile_layers.txt: not read. columns: soil_profile_ID, soil_layer_top_depth, soil_layer_base_depth, soil_water_saturated, soil_wat_drned_upper_lim, soil_water_lower_limit, soil_layer_available_water, soil_bulk_density_moist, soil_organic_N_conc, sat_hydraul_conductivity, soil_organic_C_perc_layr, soil_C_N_ratio, .... Observation records are missing from d until this file is standardized.
# CIM_AgMIP_Treatments.txt: not read. columns: treatment_ID, experiment_ID, soil_profile_ID, weather_station_code, weather_file, treatment_name, simulation_start_date, irrig_applied, fert_applied, irrigation_level, fertilizer_level, environmental_modif_lev, .... Observation records are missing from d until this file is standardized.
# Dynamics_1-Baca_Cal(formated).txt: not read. columns: Model, Trt, TRNO, Genotype, Env, Location, Year, TRNO.jdn, Planting.date, Planting.doy, Planting.jdn, Date, .... Observation records are missing from d until this file is standardized.
# merged dynamics_1-Baca_Cal.txt: not read. columns: Model, Year, Date, Treatment, Yield, Biom, LAI, WDrain, CumET, SoilAvW, Runoff, Transp, .... Observation records are missing from d until this file is standardized.
# merged summary_1-Baca_Cal.txt: not read. columns: Model, Planting.date, Treatment, Yield, Emergence, Ant, Mat, FLN, GNumber, Biom-an, Biom-ma, MaxLAI, .... Observation records are missing from d until this file is standardized.
# Step_1_EF.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_EF_BA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_EF_CO.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_EF_VA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_intercept.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_intercept_BA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_intercept_CO.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_intercept_VA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_LC.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_LC_BA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_LC_CO.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_LC_VA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_MSE.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_MSE_BA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_MSE_CO.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_MSE_VA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_NU.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_NU_BA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_NU_CO.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_NU_VA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_r2.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_r2_BA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_r2_CO.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_r2_VA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_RMSE.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_RMSE_BA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_RMSE_CO.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_RMSE_VA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_RMSRE.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_RMSRE_BA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_RMSRE_CO.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_RMSRE_VA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_RRMSE.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_RRMSE_BA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_RRMSE_CO.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_RRMSE_VA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_SB.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_SB_BA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_SB_CO.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_SB_VA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_slope.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_slope_BA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_slope_CO.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_1_slope_VA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Summary_1-Baca_Cal(formated).txt: not read. columns: Model, TRNO, Trt, Genotype, Env, Location, Year, Planting.date, Planting.doy, Planting.jdn, Emergence.date, Emergence.doy, .... Observation records are missing from d until this file is standardized.
# Dynamics_2-DH(formated).txt: not read. columns: Model, Trt, TRNO, Genotype, Env, Location, Year, TRNO.jdn, Planting.date, Planting.doy, Planting.jdn, Date, .... Observation records are missing from d until this file is standardized.
# merged dynamics_2-DH.txt: not read. columns: Model, Year, Date, Treatment, Yield, Biom, LAI, WDrain, CumET, SoilAvW, Runoff, Transp, .... Observation records are missing from d until this file is standardized.
# merged summary_2-DH.txt: not read. columns: Model, Planting.date, Treatment, Yield, Emergence, Ant, Mat, FLN, GNumber, Biom-an, Biom-ma, MaxLAI, .... Observation records are missing from d until this file is standardized.
# Step_2_EF.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_EF_BA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_EF_CO.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_EF_VA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_intercept.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_intercept_BA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_intercept_CO.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_intercept_VA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_LC.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_LC_BA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_LC_CO.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_LC_VA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_MSE.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_MSE_BA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_MSE_CO.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_MSE_VA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_NU.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_NU_BA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_NU_CO.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_NU_VA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_r2.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_r2_BA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_r2_CO.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_r2_VA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_RMSE.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_RMSE_BA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_RMSE_CO.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_RMSE_VA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_RMSRE.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_RMSRE_BA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_RMSRE_CO.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_RMSRE_VA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_RRMSE.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_RRMSE_BA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_RRMSE_CO.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_RRMSE_VA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_SB.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_SB_BA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_SB_CO.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_SB_VA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_slope.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_slope_BA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_slope_CO.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Step_2_slope_VA.txt: not read. columns: "sa", "aegf", "segf", "Biom.ma", "Yield", "GNumber", "GDW", "HI". Observation records are missing from d until this file is standardized.
# Summary_2-DH(formated).txt: not read. columns: Model, TRNO, Trt, Genotype, Env, Location, Year, Planting.date, Planting.doy, Planting.jdn, Emergence.date, Emergence.doy, .... Observation records are missing from d until this file is standardized.
# AECI.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AECI62.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AECI63.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AECI64.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AECI65.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AECI66.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AECI67.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AECI68.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AECI69.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AECI70.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AECI71.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AQCI.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AQCI62.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AQCI63.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AQCI64.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AQCI65.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AQCI66.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AQCI67.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AQCI68.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AQCI69.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AQCI70.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AQCI71.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AWCI.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AWCI62.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AWCI63.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AWCI64.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AWCI65.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AWCI66.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AWCI67.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AWCI68.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AWCI69.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AWCI70.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AWCI71.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# CSCI.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# CSCI62.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# CSCI63.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# CSCI64.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# CSCI65.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# CSCI66.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# CSCI67.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# CSCI68.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# CSCI69.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# CSCI70.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# CSCI71.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# D1CI.txt: not read. columns: AgMIP_Wheat_4_CI                                                    . Observation records are missing from d until this file is standardized.
# D1CI62.txt: not read. columns: AgMIP_Wheat_4_CI                                        . Observation records are missing from d until this file is standardized.
# D1CI63.txt: not read. columns: AgMIP_Wheat_4_CI                                        . Observation records are missing from d until this file is standardized.
# D1CI64.txt: not read. columns: AgMIP_Wheat_4_CI                                        . Observation records are missing from d until this file is standardized.
# D1CI65.txt: not read. columns: AgMIP_Wheat_4_CI                                        . Observation records are missing from d until this file is standardized.
# D1CI66.txt: not read. columns: AgMIP_Wheat_4_CI                                        . Observation records are missing from d until this file is standardized.
# D1CI67.txt: not read. columns: AgMIP_Wheat_4_CI                                        . Observation records are missing from d until this file is standardized.
# D1CI68.txt: not read. columns: AgMIP_Wheat_4_CI                                        . Observation records are missing from d until this file is standardized.
# D1CI69.txt: not read. columns: AgMIP_Wheat_4_CI                                        . Observation records are missing from d until this file is standardized.
# D1CI70.txt: not read. columns: AgMIP_Wheat_4_CI                                        . Observation records are missing from d until this file is standardized.
# D1CI71.txt: not read. columns: AgMIP_Wheat_4_CI                                        . Observation records are missing from d until this file is standardized.
# D3CI.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# D3CI62.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# D3CI63.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# D3CI64.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# D3CI65.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# D3CI66.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# D3CI67.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# D3CI68.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# D3CI69.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# D3CI70.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# D3CI71.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# D4CI.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# D4CI62.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# D4CI63.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# D4CI64.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# D4CI65.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# D4CI66.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# D4CI67.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# D4CI68.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# D4CI69.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# D4CI70.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# D4CI71.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DNCI.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DNCI62.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DNCI63.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DNCI64.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DNCI65.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DNCI66.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DNCI67.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DNCI68.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DNCI69.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DNCI70.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DNCI71.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DRCI.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DRCI62.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DRCI63.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DRCI64.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DRCI65.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DRCI66.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DRCI67.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DRCI68.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DRCI69.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DRCI70.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DRCI71.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DSCI.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DSCI62.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DSCI63.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DSCI64.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DSCI65.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DSCI66.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DSCI67.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DSCI68.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DSCI69.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DSCI70.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DSCI71.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# HECI.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# HECI62.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# HECI63.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# HECI64.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# HECI65.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# HECI66.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# HECI67.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# HECI68.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# HECI69.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# HECI70.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# HECI71.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# L5CI.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L5CI62.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L5CI63.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L5CI64.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L5CI65.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L5CI66.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L5CI67.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L5CI68.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L5CI69.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L5CI70.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L5CI71.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L6CI.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L6CI62.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L6CI63.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L6CI64.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L6CI65.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L6CI66.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L6CI67.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L6CI68.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L6CI69.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L6CI70.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L6CI71.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# LICI.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# LICI62.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# LICI63.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# LICI64.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# LICI65.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# LICI66.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# LICI67.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# LICI68.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# LICI69.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# LICI70.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# LICI71.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MCCI.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MCCI62.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MCCI63.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MCCI64.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MCCI65.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MCCI66.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MCCI67.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MCCI68.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MCCI69.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MCCI70.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MCCI71.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MOCI.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MOCI62.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MOCI63.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MOCI64.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MOCI65.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MOCI66.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MOCI67.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MOCI68.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MOCI69.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MOCI70.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MOCI71.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NCCI.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NCCI62.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NCCI63.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NCCI64.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NCCI65.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NCCI66.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NCCI67.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NCCI68.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NCCI69.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NCCI70.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NCCI71.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NGCI.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NGCI62.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NGCI63.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NGCI64.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NGCI65.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NGCI66.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NGCI67.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NGCI68.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NGCI69.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NGCI70.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NGCI71.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NPCI.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NPCI62.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NPCI63.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NPCI64.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NPCI65.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NPCI66.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NPCI67.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NPCI68.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NPCI69.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NPCI70.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NPCI71.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NSCI.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NSCI62.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NSCI63.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NSCI64.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NSCI65.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NSCI66.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NSCI67.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NSCI68.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NSCI69.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NSCI70.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# NSCI71.txt: not read. columns: AgMIP_Wheat_4_CIMMYT. Observation records are missing from d until this file is standardized.
# PGCI.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# PGCI62.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# PGCI63.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# PGCI64.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# PGCI65.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# PGCI66.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# PGCI67.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# PGCI68.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# PGCI69.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# PGCI70.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# PGCI71.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# S2CI.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# S2CI62.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# S2CI63.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# S2CI64.txt: not read. columns: HEB-YIELD_1, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# S2CI65.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# S2CI66.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# S2CI67.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# S2CI68.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# S2CI69.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# S2CI70.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# S2CI71.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SACI.txt: not read. columns: AgMIP_Wheat_4_ROTS. Observation records are missing from d until this file is standardized.
# SACI62.txt: not read. columns: AgMIP_Wheat_4_ROTS. Observation records are missing from d until this file is standardized.
# SACI63.txt: not read. columns: AgMIP_Wheat_4_ROTS. Observation records are missing from d until this file is standardized.
# SACI64.txt: not read. columns: AgMIP_Wheat_4_ROTS. Observation records are missing from d until this file is standardized.
# SACI65.txt: not read. columns: AgMIP_Wheat_4_ROTS. Observation records are missing from d until this file is standardized.
# SACI66.txt: not read. columns: AgMIP_Wheat_4_ROTS. Observation records are missing from d until this file is standardized.
# SACI67.txt: not read. columns: AgMIP_Wheat_4_ROTS. Observation records are missing from d until this file is standardized.
# SACI68.txt: not read. columns: AgMIP_Wheat_4_ROTS. Observation records are missing from d until this file is standardized.
# SACI69.txt: not read. columns: AgMIP_Wheat_4_ROTS. Observation records are missing from d until this file is standardized.
# SACI70.txt: not read. columns: AgMIP_Wheat_4_ROTS. Observation records are missing from d until this file is standardized.
# SACI71.txt: not read. columns: AgMIP_Wheat_4_ROTS. Observation records are missing from d until this file is standardized.
# SPCI.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SPCI62.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SPCI63.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SPCI64.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SPCI65.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SPCI66.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SPCI67.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SPCI68.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SPCI69.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SPCI70.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SPCI71.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SQCI.txt: not read. columns: AgMIP. Observation records are missing from d until this file is standardized.
# SQCI62.txt: not read. columns: AgMIP. Observation records are missing from d until this file is standardized.
# SQCI63.txt: not read. columns: AgMIP. Observation records are missing from d until this file is standardized.
# SQCI64.txt: not read. columns: AgMIP. Observation records are missing from d until this file is standardized.
# SQCI65.txt: not read. columns: AgMIP. Observation records are missing from d until this file is standardized.
# SQCI66.txt: not read. columns: AgMIP. Observation records are missing from d until this file is standardized.
# SQCI67.txt: not read. columns: AgMIP. Observation records are missing from d until this file is standardized.
# SQCI68.txt: not read. columns: AgMIP. Observation records are missing from d until this file is standardized.
# SQCI69.txt: not read. columns: AgMIP. Observation records are missing from d until this file is standardized.
# SQCI70.txt: not read. columns: AgMIP. Observation records are missing from d until this file is standardized.
# SQCI71.txt: not read. columns: AgMIP. Observation records are missing from d until this file is standardized.
# SSCI.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SSCI62.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SSCI63.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SSCI64.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SSCI65.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SSCI66.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SSCI67.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SSCI68.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SSCI69.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SSCI70.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SSCI71.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WGCI.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# WGCI62.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# WGCI63.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# WGCI64.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# WGCI65.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# WGCI66.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# WGCI67.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# WGCI68.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# WGCI69.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# WGCI70.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# WGCI71.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# WOCI.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WOCI62.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WOCI63.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WOCI64.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WOCI65.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WOCI66.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WOCI67.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WOCI68.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WOCI69.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WOCI70.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WOCI71.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WUCI.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WUCI62.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WUCI63.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WUCI64.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WUCI65.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WUCI66.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WUCI67.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WUCI68.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WUCI69.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WUCI70.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WUCI71.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AECI72.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AECI73.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AECI74.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AECI75.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AECI76.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AECI77.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AQCI72.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AQCI73.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AQCI74.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AQCI75.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AQCI76.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AQCI77.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AWCI72.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AWCI73.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AWCI74.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AWCI75.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AWCI76.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# AWCI77.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# CSCI72.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# CSCI73.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# CSCI74.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# CSCI75.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# CSCI76.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# CSCI77.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# D1CI72.txt: not read. columns: AgMIP_Wheat_4_CI                                        . Observation records are missing from d until this file is standardized.
# D1CI73.txt: not read. columns: AgMIP_Wheat_4_CI                                        . Observation records are missing from d until this file is standardized.
# D1CI74.txt: not read. columns: AgMIP_Wheat_4_CI                                        . Observation records are missing from d until this file is standardized.
# D1CI75.txt: not read. columns: AgMIP_Wheat_4_CI                                        . Observation records are missing from d until this file is standardized.
# D1CI76.txt: not read. columns: AgMIP_Wheat_4_CI                                        . Observation records are missing from d until this file is standardized.
# D1CI77.txt: not read. columns: AgMIP_Wheat_4_CI                                        . Observation records are missing from d until this file is standardized.
# D3CI72.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# D3CI73.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# D3CI74.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# D3CI75.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# D3CI76.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# D3CI77.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# D4CI72.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# D4CI73.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# D4CI74.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# D4CI75.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# D4CI76.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# D4CI77.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DNCI72.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DNCI73.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DNCI74.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DNCI75.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DNCI76.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DNCI77.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DRCI72.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DRCI73.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DRCI74.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DRCI75.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DRCI76.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DRCI77.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DSCI72.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DSCI73.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DSCI74.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DSCI75.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DSCI76.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# DSCI77.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# HECI62.TXT: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# HECI72.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# HECI73.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# HECI74.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# HECI75.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# HECI76.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# HECI77.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# L5CI72.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L5CI73.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L5CI74.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L5CI75.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L5CI76.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L5CI77.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L6CI72.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L6CI73.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L6CI74.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L6CI75.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L6CI76.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# L6CI77.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# LICI72.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# LICI73.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# LICI74.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# LICI75.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# LICI76.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# LICI77.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MCCI72.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MCCI73.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MCCI74.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MCCI75.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MCCI76.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MCCI77.TXT: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MOCI72.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MOCI73.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MOCI74.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MOCI75.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MOCI76.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# MOCI77.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NCCI.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NCCI62.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NCCI63.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NCCI67.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NCCI68.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NCCI72.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NCCI73.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NCCI74.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NCCI75.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NCCI76.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NCCI77.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NGCI.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NGCI62.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NGCI63.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NGCI67.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NGCI68.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NGCI72.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NGCI73.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NGCI74.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NGCI75.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NGCI76.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NGCI77.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NPCI.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NPCI62.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NPCI63.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NPCI67.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NPCI68.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NPCI72.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NPCI73.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NPCI74.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NPCI75.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NPCI76.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NPCI77.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NSCI.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NSCI62.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NSCI63.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NSCI67.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NSCI68.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NSCI72.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NSCI73.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NSCI74.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NSCI75.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NSCI76.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# NSCI77.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# PGCI72.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# PGCI73.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# PGCI74.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# PGCI75.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# PGCI76.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# PGCI77.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# S2CI72.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# S2CI73.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# S2CI74.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# S2CI75.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# S2CI76.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# S2CI77.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SACI72.txt: not read. columns: AgMIP_Wheat_4_ROTS. Observation records are missing from d until this file is standardized.
# SACI73.txt: not read. columns: AgMIP_Wheat_4_ROTS. Observation records are missing from d until this file is standardized.
# SACI74.txt: not read. columns: AgMIP_Wheat_4_ROTS. Observation records are missing from d until this file is standardized.
# SACI75.txt: not read. columns: AgMIP_Wheat_4_ROTS. Observation records are missing from d until this file is standardized.
# SACI76.txt: not read. columns: AgMIP_Wheat_4_ROTS. Observation records are missing from d until this file is standardized.
# SACI77.txt: not read. columns: AgMIP_Wheat_4_ROTS. Observation records are missing from d until this file is standardized.
# SPCI72.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SPCI73.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SPCI74.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SPCI75.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SPCI76.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SPCI77.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SQCI72.txt: not read. columns: AgMIP. Observation records are missing from d until this file is standardized.
# SQCI73.txt: not read. columns: AgMIP. Observation records are missing from d until this file is standardized.
# SQCI74.txt: not read. columns: AgMIP. Observation records are missing from d until this file is standardized.
# SQCI75.txt: not read. columns: AgMIP. Observation records are missing from d until this file is standardized.
# SQCI76.txt: not read. columns: AgMIP. Observation records are missing from d until this file is standardized.
# SQCI77.txt: not read. columns: AgMIP. Observation records are missing from d until this file is standardized.
# SSCI72.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SSCI73.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SSCI74.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SSCI75.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SSCI76.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# SSCI77.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WGCI72.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# WGCI73.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# WGCI74.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# WGCI75.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# WGCI76.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# WGCI77.txt: not read. columns: AgMIP_Wheat_4_CI. Observation records are missing from d until this file is standardized.
# WOCI72.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WOCI73.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WOCI74.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WOCI75.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WOCI76.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WOCI77.txt: not read. columns: AgMIP_Wheat_4_CI, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WUCI72.txt: not read. columns: AgMIP_Wheat_4_CIMMYT, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WUCI73.txt: not read. columns: AgMIP_Wheat_4_CIMMYT, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WUCI74.txt: not read. columns: AgMIP_Wheat_4_CIMMYT, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WUCI75.txt: not read. columns: AgMIP_Wheat_4_CIMMYT, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WUCI76.txt: not read. columns: AgMIP_Wheat_4_CIMMYT, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# WUCI77.txt: not read. columns: AgMIP_Wheat_4_CIMMYT, , , , , , , , , , , , .... Observation records are missing from d until this file is standardized.
# CIM_AgMIP_model_names.txt: not read. columns: #, Model_(version), Model_code, Documentation. Observation records are missing from d until this file is standardized.
# AgMIP_4_BxW_Observation_dynamics.xlsx: not read. sheet AgMIP_4_BxW_dynamics: TRNO, Trt, Location, Env, Genotype, Year, Bloc, Planting.date, Date, DAS, Biom, LAI, .... Observation records are missing from d until this file is standardized.
# AgMIP_4_BxW_Observation_dynamics_average.txt: not read. columns: TRNO, Date, Biom, cumPARi, Trt, Location, Env, Genotype, Year, Planting.date, DAS. Observation records are missing from d until this file is standardized.
# AgMIP_4_BxW_Observation_dynamics_cv.txt: not read. columns: TRNO, Date, Biom, cumPARi, Trt, Location, Env, Genotype, Year, Planting.date, DAS. Observation records are missing from d until this file is standardized.
# AgMIP_4_BxW_Observation_dynamics_median.txt: not read. columns: TRNO, Date, Biom, cumPARi, Trt, Location, Env, Genotype, Year, Planting.date, DAS. Observation records are missing from d until this file is standardized.
# AgMIP_4_BxW_Observation_dynamics_q.25.txt: not read. columns: TRNO, Date, Biom, cumPARi, Trt, Location, Env, Genotype, Year, Planting.date, DAS. Observation records are missing from d until this file is standardized.
# AgMIP_4_BxW_Observation_dynamics_q.75.txt: not read. columns: TRNO, Date, Biom, cumPARi, Trt, Location, Env, Genotype, Year, Planting.date, DAS. Observation records are missing from d until this file is standardized.
# AgMIP_4_BxW_Observation_dynamics_sd.txt: not read. columns: TRNO, Date, Biom, cumPARi, Trt, Location, Env, Genotype, Year, Planting.date, DAS. Observation records are missing from d until this file is standardized.
# AgMIP_4_BxW_Observation_summary_average.txt: not read. columns: TRNO, Trt, Location, Env, Genotype, Year, Planting.date, Emergence, Ant, Mat, ea, se, .... Observation records are missing from d until this file is standardized.
# AgMIP_4_BxW_Observation_summary_average.xlsx: not read. sheet AgMIP_4_BxW_Obs_summ_all: TRNO, Trt, Location, Env, Genotype, Year, Planting.date, Emergence, Ant, Mat, ea, se, .... Observation records are missing from d until this file is standardized.
# 00._Call do.run.all.r: not read. .r file. Observation records are missing from d until this file is standardized.
# AgMIP_BxW_Create_Observations.r: not read. .r file. Observation records are missing from d until this file is standardized.
# do.mme.analysis.r: not read. .r file. Observation records are missing from d until this file is standardized.
# do.model evaluation dynamics.r: not read. .r file. Observation records are missing from d until this file is standardized.
# do.model evaluation.r: not read. .r file. Observation records are missing from d until this file is standardized.
# do.plot model evaluation.r: not read. .r file. Observation records are missing from d until this file is standardized.
# do.plot percent change.r: not read. .r file. Observation records are missing from d until this file is standardized.
# do.plot percent change2.r: not read. .r file. Observation records are missing from d until this file is standardized.
# do.plot.dynamics.r: not read. .r file. Observation records are missing from d until this file is standardized.
# do.plot.scatter-Obs.r: not read. .r file. Observation records are missing from d until this file is standardized.
# do.plot.scatter-Obs_biomass_yield_g_m2_edit.r: not read. .r file. Observation records are missing from d until this file is standardized.
# do.plot.Sim-vs-Obs.r: not read. .r file. Observation records are missing from d until this file is standardized.
# do.rearrange.txt.r: not read. .r file. Observation records are missing from d until this file is standardized.
# do.run.all.r: not read. .r file. Observation records are missing from d until this file is standardized.
# merge_txt_files.r: not read. .r file. Observation records are missing from d until this file is standardized.
# automatically_install_package.r: not read. .r file. Observation records are missing from d until this file is standardized.
# do.add.NA.col.after.summaryBy.r: not read. .r file. Observation records are missing from d until this file is standardized.
# do.add.NA.col.after.summaryBy_4dynamics.r: not read. .r file. Observation records are missing from d until this file is standardized.
# do.add.NA.col.after.summaryBy_4summary.r: not read. .r file. Observation records are missing from d until this file is standardized.
# do.calculate.DATE.DOY.from.JDN.r: not read. .r file. Observation records are missing from d until this file is standardized.
# EllipseCorrelationMatrix.r: not read. .r file. Observation records are missing from d until this file is standardized.
# Put correlations on the upper panels of pairs().r: not read. .r file. Observation records are missing from d until this file is standardized.
# Trt_genotype_env.txt: not read. columns: TRNO, Trt, Genotype, Env, Location, Year. Observation records are missing from d until this file is standardized.
# readme.txt: documentation or a codebook, not an observation table.
# CIM_AgMIP_simulation_key.txt: not read. columns: Variable_Name, Unit, Description, File. Observation records are missing from d until this file is standardized.
# CIM_AgMIP_simulations_daily_step1.txt: not read. columns: Model, Trt, TRNO, Genotype, Location, Year, Planting.date, Date, DAS, LAI, Biom, Yield, .... Observation records are missing from d until this file is standardized.
# CIM_AgMIP_simulations_daily_step2.txt: not read. columns: Model, Trt, TRNO, Genotype, Location, Year, Planting.date, Date, DAS, LAI, Biom, Yield, .... Observation records are missing from d until this file is standardized.
# CIM_AgMIP_simulations_summary_step1.txt: not read. columns: Model, TRNO, Trt, Genotype, Location, Year, Planting.date, Emergence.date, Ant.date, Mat.date, MaxLAI, Biom.an, .... Observation records are missing from d until this file is standardized.
# CIM_AgMIP_simulations_summary_step2.txt: not read. columns: Model, TRNO, Trt, Genotype, Location, Year, Planting.date, Emergence.date, Ant.date, Mat.date, MaxLAI, Biom.an, .... Observation records are missing from d until this file is standardized.
# CIM_AgMIP_weather_key.txt: not read. columns: Weather keys. Observation records are missing from d until this file is standardized.
# CIMBUA20091822009365.wth: not read. .wth file. Observation records are missing from d until this file is standardized.
# CIMCIO19900011991365.wth: not read. .wth file. Observation records are missing from d until this file is standardized.
# CIMCIO20093352010122.wth: not read. .wth file. Observation records are missing from d until this file is standardized.
# CIMCIO20153052016212.wth: not read. .wth file. Observation records are missing from d until this file is standardized.
# CIMCIO20163062017134.wth: not read. .wth file. Observation records are missing from d until this file is standardized.
# CIMVAL20082142009028.wth: not read. .wth file. Observation records are missing from d until this file is standardized.
# CIMVAL20092442010077.wth: not read. .wth file. Observation records are missing from d until this file is standardized.
# CIMVAL20102132011081.wth: not read. .wth file. Observation records are missing from d until this file is standardized.
# CIMVAL20122542013059.wth: not read. .wth file. Observation records are missing from d until this file is standardized.
# CIMVAL20132552014059.wth: not read. .wth file. Observation records are missing from d until this file is standardized.
# CIMVAL20142392015059.wth: not read. .wth file. Observation records are missing from d until this file is standardized.

carob_script <- function(path) {

"
Data from the AgMIP-Wheat high-yielding traits experiment for modeling potential production of wheat: field experiments and multi-model simulations

The dataset reported here was created to analyze the value of physiological traits identified by the International Wheat Yield Partnership (IWYP) to improve wheat potential in high-yielding environments. This dataset consists of 11 growing seasons at three high-yielding locations in Buenos Aires (Argentina), Ciudad Obregon (Mexico), and Valdivia (Chile) with the spring wheat cultivar Bacanora and a high-yielding genotype selected from a doubled haploid (DH) population developed from the cross between the Bacanora and Weebil cultivars from the International Maize and Wheat Improvement Center (CIMMYT). This dataset was used in the Agricultural Model Intercomparison and Improvement Project (AgMIP) Wheat Phase 4 to evaluate crop model performance when simulating high-yielding physiological traits and to determine the potential production of wheat using an ensemble of 29 wheat crop models. The field trials were managed for non-stress conditions with full irrigation, fertilizer application, and without biotic stress. Data include local daily weather, soil characteristics and initial soil conditions, cultivar information, and crop measurements (anthesis and maturity dates, total above-ground biomass, final grain yield, yield components, and photosynthetically active radiation interception). Simulations include both daily in-season and end-of-season results for 25 crop variables simulated by 29 wheat crop models. The R code and formatted data used for the statistical analyses are included.
"

	uri <- "doi:10.7910/DVN/VKWKUP"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT",
		publication = "doi:10.18174/odjar.v9i0.18573",
		project = "AgMIP-Wheat",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety;N_fertilizer",
		response_vars = "yield;dmy_total",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 70,
		carob_effort = 1.5
	)

	f1 <- ff[basename(ff) == "CIM_AgMIP_Obs_crop_summary_plots.txt"]
	f2 <- ff[basename(ff) == "CIM_AgMIP_Fields.txt"]
	f3 <- ff[basename(ff) == "CIM_AgMIP_Planting_events.txt"]
	f4 <- ff[basename(ff) == "CIM_AgMIP_Fertilizer_events.txt"]
	f5 <- ff[basename(ff) == "CIM_AgMIP_Weather_daily.txt"]
	f6 <- ff[basename(ff) == "CIM_AgMIP_Weather_stations.txt"]

	r1 <- read.table(f1, header=TRUE, sep="\t", skip=2, check.names=FALSE)
	r2 <- read.table(f2, header=TRUE, sep="\t", skip=2, check.names=FALSE)
	r3 <- read.table(f3, header=TRUE, sep="\t", skip=2, check.names=FALSE)
	r4 <- read.table(f4, header=TRUE, sep="\t", skip=2, check.names=FALSE)

	# Seasonal N (kg/ha) from fertilizer events; same N for Bacanora and DH within a site-year
	nsum <- aggregate(list(N_fertilizer=as.numeric(r4$FEAMN)), list(EID=r4$EID), sum, na.rm=TRUE)

	g <- r2[match(r1$EID, r2$EID), ]
	p <- r3[match(r1$EID, r3$EID), ]
	n <- nsum$N_fertilizer[match(r1$EID, nsum$EID)]

	d <- data.frame(
		trial_id = r1$EID,
		rep = as.integer(r1$RP),
		variety = r1$CUL_NAME,
		planting_date = as.character(r1$PDATE),
		emergence_date = as.character(r1$PLDAE),
		flowering_date = as.character(r1$ADAT),
		maturity_date = as.character(r1$MDAT),
		harvest_date = as.character(r1$HDATE),
		# mg/seed equals g per 1000 seeds
		seed_weight = as.numeric(r1$GWGM),
		plant_density = as.numeric(r1$PLPAE) * 10000,
		plant_height = as.numeric(r1$PHTM) * 100,
		yield = as.numeric(r1$GWAM),
		dmy_total = as.numeric(r1$CWAM),
		country = g$FL_LOC_1,
		location = gsub("_", " ", g$FL_NAME),
		latitude = as.numeric(g$FL_LAT),
		longitude = as.numeric(g$FL_LONG),
		elevation = as.numeric(g$FLELE),
		row_spacing = as.numeric(p$PLRS),
		seed_density = as.numeric(p$PLPOP) * 10000,
		N_fertilizer = n,
		crop = "wheat",
		yield_part = "grain",
		yield_moisture = 0,
		irrigated = TRUE,
		geo_from_source = TRUE,
		on_farm = FALSE,
		is_survey = FALSE
	)

	d$treatment <- d$variety

	r5 <- read.table(f5, header=TRUE, sep="\t", skip=2, check.names=FALSE)
	# Weather file prefixes map to Fields coordinates
	loc <- ifelse(grepl("BUA", r5$WST_DATASET), "Buenos Aires",
		ifelse(grepl("CIO", r5$WST_DATASET), "Ciudad Obregon", "Valdivia"))
	wmeta <- data.frame(
		location = c("Buenos Aires", "Ciudad Obregon", "Valdivia"),
		country = c("Argentina", "Mexico", "Chile"),
		latitude = c(-34.60, 27.43, -39.80),
		longitude = c(-58.50, -109.92, -73.25),
		elevation = c(26, 38, 19)
	)
	wm <- wmeta[match(loc, wmeta$location), ]
	wth <- data.frame(
		country = wm$country,
		location = wm$location,
		longitude = wm$longitude,
		latitude = wm$latitude,
		elevation = wm$elevation,
		date = as.character(r5$W_DATE),
		srad = as.numeric(r5$SRAD),
		tmax = as.numeric(r5$TMAX),
		tmin = as.numeric(r5$TMIN),
		prec = as.numeric(r5$RAIN),
		vapr = as.numeric(r5$VPRSD),
		# km/d to m/s
		wspd = as.numeric(r5$WIND) / 86.4
	)
	wth <- unique(wth)
	wth <- wth[!is.na(wth$date), ]

	carobiner::write_files(path, meta, d, wth=wth)
}
