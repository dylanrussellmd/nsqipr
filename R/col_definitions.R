## ---- NA Strings ----

na_strings <- c("","NULL","NA","N/A","-99","Unknown","Unknown/Not Reported","Not entered","Not documented","Unknown or N/A")

## ---- GENERIC COLUMNS TO BE PROCESSED ----
yes_no_cols <- c(
  # ACS_NSQIP_PUF
  "electsurg","smoke","ventilat","hxcopd","ascites","hxchf","hypermed","renafail","dialysis","discancr","wndinf","steroid","wtloss","bleeddis","transfus","emergncy","sssipatos","dssipatos","ossipatos","pnapatos","ventpatos","utipatos","sepsispatos","sepshockpatos","returnor","stillinhosp","reoperation1","retorrelated","reoperation2","retor2related","reoperation3","readmission1","unplannedreadmission1","readmrelated1","readmission2","unplannedreadmission2","readmrelated2","readmission3","unplannedreadmission3","readmrelated3", "readmission4","unplannedreadmission4","readmrelated4","readmission5","unplannedreadmission5","readmrelated5", "etoh", "dnr", "cpneumon", "esovar","hxmi","prvpci","prvpcs","hxangina","hxpvd","restpain","impsens", "coma", "hemi", "hxtia","cva","cvano","tumorcns","para","quad","chemo","radio","pregnancy","proper30", "readmission","unplanreadmission","reoperation", "eol_wdcare","oxygen_support","hxfall","hxdementia",
  # PUF_TAR_AAA
  "aaa_paas", "aaa_cp_renrevasc","aaa_cp_viscrevasc","aaa_cp_ler","aaa_cp_are","aaa_colitis","aaa_lei","aaa_roa",
  # PUF_TAR_AIE
  "aie_hrf_phys","aie_premed_aspirin","aie_premed_statin","aie_premed_betab","aie_ulp","aie_bleeding","aie_mi_stroke","aie_wound","aie_amputation","aie_mrtas",
  # PUF_TAR_AIO
  "aio_hrf_phys","aio_premed_aspirin","aio_premed_statin","aio_premed_betab","aio_ulp","aio_bleeding","aio_mi_stroke","aio_wound","aio_mrtas","aio_amputation",
  # PUF_TAR_COL
  "col_steroid", "col_mech_bowel_prep","col_oral_antibiotic", "col_chemo", "col_margins","col_ileus",
  # PUF_TAR_PAN
  "pan_jaundice","pan_chemo","pan_radio","pan_drains","pan_drain_pod30","pan_percdrain_20140315","pan_woundprot","pan_drainsys_suctn",
  # PUF_TAR_HEP
  "hep_neoadj","hep_con_op_ablation","hep_pringle","hep_drains","hep_invasive","hep_drains_30d",
  # PUF_TAR_APP
  "app_appendix"
  )

numscale_cols <- c("wndclas","asaclas")

complication_cols <- c("supinfec","wndinfd","orgspcssi","dehis","oupneumo","reintub","pulembol","failwean","renainsf","oprenafl","urninfec","cnscva","cnscoma","neurodef","othgrafl","cdarrest","cdmi","othbleed","othdvt","othsysep","othseshock","othcdiff")

date_cols <- c("admyr","operyr","yrdeath","hdisdt", "admsyr","sdisdt")

integer_cols <- c(
  # ACS_NSQIP_PUF
  "caseid","height","weight","optime","tothlos", "admqtr","htooday", "dsupinfec","dwndinfd","dorgspcssi", "ddehis", "doupneumo","dreintub", "dpulembol","dfailwean","drenainsf","doprenafl","durninfec","dcnscva", "dcnscoma", "dneurodef", "dcdarrest","dcdmi","dothbleed", "dothgrafl", "dothdvt", "dothsysep","dothseshock","dopertod","doptodis","dothcdiff", "retorpodays", "retor2podays", "readmpodays1", "readmpodays2", "readmpodays3", "readmpodays4", "readmpodays5", "dprna", "dprbun", "dprcreat", "dpralbum", "dprbili", "dprsgot", "dpralkph", "dprwbc", "dprhct", "dprplate", "dprptt", "dprpt", "dprinr", "nsupinfec", "nwndinfd", "norgspcssi", "ndehis", "noupneumo", "nreintub", "npulembol", "nfailwean", "nrenainsf", "noprenafl", "nurninfec", "ncnscva", "ncnscoma", "nneurodef", "ncdarrest", "ncdmi", "nothbleed", "nothgrafl", "nothdvt", "nothsysep", "nothseshock", "nothcdiff", "packs", "pgy", "mallamp", "rbc", "anesurg","surgane","dpatrm","anetime", "stooday","totslos", "dsdtohd","bleed_units_tot",
  # PUF_TAR_AAA
  "aaa_dcolitis","aaa_dlei","aaa_droa",
  # PUF_TAR_AIE
  "aie_dulp","aie_dbleeding","aie_dmi_stroke","aie_dwound",
  # PUF_TAR_AIO
  "aio_dulp","aio_dbleeding","aio_dmi_stroke","aio_dwound",
  # PUF_TAR_COL
  "col_nodeseval",
  # PUF_TAR_PAN
  "damylase","ddrainremoval","ddrainsremoval"
  )

numeric_cols <- c(
  # ACS_NSQIP_PUF
  "prsodm","prbun","prcreat","pralbum","prbili","prsgot","pralkph","prwbc","prhct","prplate","prptt","prinr","prpt","hemo","mortprob","morbprob", "workrvu", "otherwrvu1", "otherwrvu2", "otherwrvu3", "otherwrvu4", "otherwrvu5", "otherwrvu6", "otherwrvu7", "otherwrvu8", "otherwrvu9", "otherwrvu10", "conwrvu1", "conwrvu2", "conwrvu3", "conwrvu4", "conwrvu5", "conwrvu6", "conwrvu7", "conwrvu8", "conwrvu9", "conwrvu10",
  # PUF_TAR_AAA
  "aaa_andiam",
  # PUF_TAR_PAN
  "pan_amylase_pod1","pan_amylase_pod230",
  # PUF_TAR_HEP
  "hep_drains_bili","hep_peakinr","hep_peakbili","hep_peakcreat"
  )

factor_cols <- c(
  # ACS_NSQIP_PUF
  "sex","fnstatus1","fnstatus2","typeintoc","airtra","opnote","attend","wound_closure","transt","readmsuspreason1","readmunrelsusp1","readmsuspreason2","readmunrelsusp2","readmsuspreason3","readmunrelsusp3","readmsuspreason4","readmunrelsusp4","readmsuspreason5","readmunrelsusp5","dischdest","anesthes", "surgspec","casetype","disfxnstat",
  # PUF_TAR_PAN
  "pan_drainsys_type","pan_oincis_type","pan_intra_antibiotics","pan_benign_tumorsize","pan_benign_histologic","pan_mstage","pan_tstage","pan_nstage","pan_malig_histologic","pan_resection","pan_drains_type","pan_gastduo","pan_reconstruction","pan_glandtext","pan_ductsize","pan_approach","pan_biliarystent","pan_lapthor","pan_percdrainage1", "pan_percdrainage2","pan_percdrainage3","pan_percdrainage4",
  # PUF_TAR_COL
  "col_malignancym","col_malignancyt","col_malignancyn","col_approach","col_emergent","col_indication",
  # PUF_TAR_AIE
  "aie_mostsevoutcome","aie_prehemo","aie_posthemo","aie_hrf_anat","aie_sympt","aie_proc",
  # PUF_TAR_AAA
  "aaa_surgind","aaa_surgap","aaa_pcl","aaa_pae","aaa_distext","aaa_mima","aaa_colitis_treat",
  # PUF_TAR_AIO
  "aio_sympt","aio_hrf_anat","aio_prehemo","aio_posthemo","aio_mostsevoutcome","aio_proc",
  # PUF_TAR_HEP
  "hep_lapthor","hep_biliarystent","hep_approach", "hep_livertext", "hep_liverfail_grade", "hep_neotherapy1", "hep_neotherapy2", "hep_neotherapy3", "hep_neotherapy4", "hep_neotherapy5", "hep_con_ablation1", "hep_con_ablation2", "hep_con_ablation3", "hep_con_ablation4", "hep_con_ablation5", "hep_invasive_type1", "hep_invasive_type2", "hep_invasive_type3", "hep_invasive_type4", "hep_invasive_type5", "hep_pathres", "hep_histologic", "hep_tstage", "hep_nstage", "hep_mstage", "hep_sec_histologic", "hep_sec_tumorsize", "hep_benign_histologic", "hep_benign_lesion",
  # PUF_TAR_APP
  "app_img_ultra","app_setting_ultra","app_img_ct","app_setting_ct","app_img_mri","app_setting_mri", "app_pathres","app_approach"
  )

redundant_cols <- c(
  # ACS_NSQIP_PUF
  "prncptx", "otherproc1", "otherproc2", "otherproc3", "otherproc4", "otherproc5", "otherproc6", "otherproc7", "otherproc8", "otherproc9", "otherproc10", "concurr1", "concurr2", "concurr3", "concurr4", "concurr5", "concurr6", "concurr7", "concurr8", "concurr9", "concurr10", "cpt", "othercpt1", "othercpt2", "othercpt3", "othercpt4", "othercpt5", "othercpt6", "othercpt7", "othercpt8", "othercpt9", "othercpt10", "concpt1", "concpt2", "concpt3", "concpt4", "concpt5", "concpt6", "concpt7", "concpt8", "concpt9", "concpt10", "workrvu", "otherwrvu1", "otherwrvu2", "otherwrvu3", "otherwrvu4", "otherwrvu5", "otherwrvu6", "otherwrvu7", "otherwrvu8", "otherwrvu9", "otherwrvu10", "conwrvu1", "conwrvu2", "conwrvu3", "conwrvu4", "conwrvu5", "conwrvu6", "conwrvu7", "conwrvu8", "conwrvu9", "conwrvu10", "race_new","cpneumon","anesthes_other","anesthes_other1","anesthes_other2","anesthes_other3","anesthes_other4","anesthes_other5","anesthes_other6","anesthes_other7","anesthes_other8","readmission","unplanreadmission","reoperation","reoperation1","retorpodays","reoporcpt1","retorrelated","reoporicd91","reoperation2","retor2podays","reopor2cpt1","retor2related","reopor2icd91","reoperation3","retor3podays","reopor3cpt1","retor3related","reopor3icd91","reopor3icd101","readmission","readmission1","readmpodays1","unplanreadmission","unplannedreadmission1","readmrelated1","readmsuspreason1","readmrelicd91","readmission2","readmpodays2","unplannedreadmission2","readmrelated2","readmsuspreason2","readmrelicd92","readmission3","readmpodays3","unplannedreadmission3","readmrelated3","readmsuspreason3","readmrelicd93","readmission4","readmpodays4","unplannedreadmission4","readmrelated4","readmsuspreason4","readmrelicd94","readmission5","readmpodays5","unplannedreadmission5","readmrelated5","readmsuspreason5","readmrelicd95","readmunrelsusp1","readmunrelicd91","readmunrelsusp2","readmunrelicd92","readmunrelsusp3","readmunrelicd93","readmunrelsusp4","readmunrelicd94","readmunrelsusp5","readmunrelicd95","reopor1icd101","reopor2icd101","readmrelicd101","readmunrelicd101","readmrelicd102","readmunrelicd102","readmrelicd103","readmunrelicd103","readmrelicd104","readmunrelicd104","readmrelicd105","readmunrelicd105",
  # PUF_TAR_AAA
  "aaa_andiam_unk",
  # PUF_TAR_COL
  "col_steroid_unk","col_oral_antibiotic_unk","col_chemo_unk","col_margins_unk","col_ileus_unk","col_mech_bowel_prep_unk","col_icd9_indication", "col_icd9_emergent",
  # PUF_TAR_PAN
  "pan_jaundice_unk", "pan_chemo_unk", "pan_radio_unk","pan_drains_unk","pan_amylase_pod1","pan_amylase_pod230","damylase","pan_amylase_pod1_unk","pan_amylase_pod230_unk","pan_amylase_unk","pan_drain_removal_unk","pan_percdrainage","pan_percdrainage1","pan_percdrainage2","pan_percdrainage3","pan_percdrainage4","pan_delgastric_20140315",
  # PUF_TAR_HEP
  "hep_neoadj_unk","hep_neotherapy_140101","hep_neotherapy1","hep_neotherapy2","hep_neotherapy3","hep_neotherapy4","hep_neotherapy5","hep_viral","hep_con_ablation_140101","hep_con_ablation1","hep_con_ablation2","hep_con_ablation3","hep_con_ablation4","hep_con_ablation5","hep_con_op_ablation_unk","hep_recon_unk","hep_drains_unk","hep_drains_bili_unk","hep_drains_removal_unk","hep_invasive_unk","hep_invasive_type","hep_invasive_type1","hep_invasive_type2","hep_invasive_type3","hep_invasive_type4","hep_invasive_type5","hep_peakinr_unk","hep_peakbili_unk","hep_peakcreat_unk",
  # PUF_TAR_APP
  "app_perfabscess")

coalesce_in_cols <- c("race","pnapatos","readmission1","unplannedreadmission1","reoperation1", "col_icd10_indication", "col_icd10_emergent")

coalesce_out_cols <- c("race_new","cpneumon","readmission","unplanreadmission","reoperation", "col_icd9_indication", "col_icd9_emergent")

## ----- INDIVIDUAL PUF/TARGETED COLUMN DEFINITIONS ----
# THESE SHOULD HAVE ALL COLUMNS INCLUDING REDUNDANT COLUMNS, RENAMED (OLD AND NEW), COLUMNS CONVERTED TO LONG, AND COLUMNS ADDED BY THE UNIQUE COLUMN FUNCTIONS

puf_cols <- c("pufyear","caseid","sex","race","race_new","ethnicity_hispanic","inout","transt","age","admyr","admsyr","operyr","dischdest","anesthes","attend","surgspec","casetype","electsurg","emergncy","urgent","height","weight","diabetes","insulin","smoke","packs","etoh","dyspnea","when_dyspnea","dnr","fnstatus1","fnstatus2","homesup","ventilat","hxcopd","cpneumon","ascites","esovar","hxchf","hxmi","prvpci","prvpcs","hxangina","hypermed","hxpvd","restpain","renafail","dialysis","impsens","coma","hemi","hxtia","cva","cvano","tumorcns","para","quad","discancr","wndinf","steroid","wtloss","bleeddis","transfus","chemo","radio","prsepis","type_prsepis","preop_covid","type_preop_covid","pregnancy","immuno_cat","oxygen_support","hxfall","hxdementia","proper30","dprna","dprbun","dprcreat","dpralbum","dprbili","dprsgot","dpralkph","dprwbc","dprhct","dprplate","dprptt","dprpt","dprinr","prsodm","prbun","prcreat","pralbum","prbili","prsgot","pralkph","prwbc","prhct","prplate","prptt","prinr","prpt","hemo","opnote","pgy","wndclas","asaclas","airtra","mallamp","mortprob","morbprob","rbc","anesurg","surgane","dpatrm","anetime","optime","typeintoc","wound_closure","sdisdt","hdisdt","yrdeath","tothlos","admqtr","htooday","stooday","totslos","nsupinfec","supinfec","sssipatos","dsupinfec","nwndinfd","wndinfd","dssipatos","dwndinfd","norgspcssi","orgspcssi","ossipatos","dorgspcssi","ndehis","dehis","ddehis","noupneumo","oupneumo","pnapatos","doupneumo","nreintub","reintub","dreintub","npulembol","pulembol","dpulembol","nfailwean","failwean","ventpatos","dfailwean","nrenainsf","renainsf","drenainsf","noprenafl","oprenafl","doprenafl","nurninfec","urninfec","utipatos","durninfec","ncnscva","cnscva","dcnscva","ncnscoma","cnscoma","dcnscoma","nneurodef","neurodef","dneurodef","ncdarrest","cdarrest","dcdarrest","ncdmi","cdmi","dcdmi","nothbleed","othbleed","dothbleed","nothgrafl","othgrafl","dothgrafl","nothdvt","othdvt","dothdvt","nothsysep","othsysep","sepsispatos","dothsysep","nothseshock","othseshock","sepshockpatos","dothseshock","othcdiff","nothcdiff","dothcdiff","delirium","bleed_units_tot","postop_covid","type_postop_covid","returnor","podiag","podiagtx","podiag10","podiagtx10","podiag_other","podiag_other10","dsdtohd","dopertod","doptodis","stillinhosp","dishomesvc","disfxnstat","eol_wdcare","prncptx","cpt","workrvu","otherproc1","othercpt1","otherwrvu1","otherproc2","othercpt2","otherwrvu2","otherproc3","othercpt3","otherwrvu3","otherproc4","othercpt4","otherwrvu4","otherproc5","othercpt5","otherwrvu5","otherproc6","othercpt6","otherwrvu6","otherproc7","othercpt7","otherwrvu7","otherproc8","othercpt8","otherwrvu8","otherproc9","othercpt9","otherwrvu9","otherproc10","othercpt10","otherwrvu10","concurr1","concpt1","conwrvu1","concurr2","concpt2","conwrvu2","concurr3","concpt3","conwrvu3","concurr4","concpt4","conwrvu4","concurr5","concpt5","conwrvu5","concurr6","concpt6","conwrvu6","concurr7","concpt7","conwrvu7","concurr8","concpt8","conwrvu8","concurr9","concpt9","conwrvu9","concurr10","concpt10","conwrvu10","reoperation","reoperation1","retorpodays","reoporcpt1","retorrelated","reoporicd91","reoperation2","retor2podays","reopor2cpt1","retor2related","reopor2icd91","reoperation3","readmission","unplanreadmission","readmission1","readmpodays1","unplannedreadmission1","readmrelated1","readmsuspreason1","readmrelicd91","readmission2","readmpodays2","unplannedreadmission2","readmrelated2","readmsuspreason2","readmrelicd92","readmission3","readmpodays3","unplannedreadmission3","readmrelated3","readmsuspreason3","readmrelicd93","readmission4","readmpodays4","unplannedreadmission4","readmrelated4","readmsuspreason4","readmrelicd94","readmission5","readmpodays5","unplannedreadmission5","readmrelated5","readmsuspreason5","readmrelicd95","readmunrelsusp1","readmunrelicd91","readmunrelsusp2","readmunrelicd92","readmunrelsusp3","readmunrelicd93","readmunrelsusp4","readmunrelicd94","readmunrelsusp5","readmunrelicd95","reopor1icd101","reopor2icd101","readmrelicd101","readmunrelicd101","readmrelicd102","readmunrelicd102","readmrelicd103","readmunrelicd103","readmrelicd104","readmunrelicd104","readmrelicd105","readmunrelicd105","anesthes_other")

aaa_cols <- c("aaa_surgind", "aaa_andiam", "aaa_andiam_unk", "aaa_paas", "aaa_surgap", "aaa_pcl", "aaa_pae", "aaa_distext", "aaa_mima", "aaa_cp_renrevasc", "aaa_cp_viscrevasc", "aaa_cp_ler", "aaa_cp_are", "aaa_colitis", "aaa_dcolitis", "aaa_colitiis_treat","aaa_colitis_treat", "aaa_lei", "aaa_dlei", "aaa_roa", "aaa_droa", "aaa_iculos")

aie_cols <- c("aie_proc", "aie_sympt", "aie_hrf_phys", "aie_hrf_anat", "aie_premed_aspirin", "aie_premed_statin", "aie_premed_betab", "aie_prehemo", "aie_ulp", "aie_dulp", "aie_bleeding", "aie_dbleeding", "aie_mi_stroke", "aie_dmi_stroke", "aie_wound", "aie_dwound", "aie_posthemo", "aie_mostsevoutcome", "aie_amputation", "aie_mrtas")

aio_cols <- c("aio_proc", "aio_sympt", "aio_hrf_phys", "aio_hrf_anat", "aio_premed_aspirin", "aio_premed_statin", "aio_premed_betab", "aio_prehemo", "aio_ulp", "aio_dulp", "aio_bleeding", "aio_dbleeding", "aio_mi_stroke", "aio_dmi_stroke", "aio_wound", "aio_dwound", "aio_posthemo", "aio_mostsevoutcome", "aio_mrtas", "aio_amputation")

col_cols <- c("col_steroid","col_steroid_unk","col_mech_bowel_prep","col_mech_bowel_prep_unk","col_oral_antibiotic","col_oral_antibiotic_unk","col_chemo","col_chemo_unk","col_indication","col_indication_icd","col_icd9_indication","col_icd10_indication","col_emergent","col_emergent_icd","col_icd9_emergent","col_icd10_emergent","col_approach","col_open_assist","col_unplanned_conversion","col_margins","col_margins_unk","col_malignancyt","col_malignancyn","col_malignancym","col_anastomotic","col_leak_treatment","col_ileus","col_ileus_unk","col_nodeseval")

pan_cols <- c("pan_lapthor", "pan_jaundice", "pan_jaundice_unk", "pan_biliarystent", "pan_chemo", "pan_chemo_unk", "pan_radio", "pan_radio_unk", "pan_intra_antibiotics", "pan_approach","pan_open_assist","pan_unplanned_conversion", "pan_oincis_type", "pan_woundprot", "pan_ductsize", "pan_glandtext", "pan_reconstruction", "pan_gastduo", "pan_drains", "pan_drains_unk", "pan_drains_type","pan_drainsys_type", "pan_drainsys_suctn", "pan_resection", "pan_amylase_pod1", "pan_amylase_pod1_unk", "pan_amylase_pod230", "pan_amylase_pod230_unk", "damylase", "pan_amylase_unk", "ddrainremoval", "pan_drain_removal_unk", "pan_drain_pod30", "pan_fistula","pan_fistula_type","pan_fistula_intervention", "pan_delgastric_20140315","pan_delgastric","pan_delgastric_tx","pan_percdrain_20140315", "pan_percdrain", "pan_percdrainage","pan_percdrainage1", "pan_percdrainage2","pan_percdrainage3","pan_percdrainage4", "pan_malig_histologic", "pan_tstage", "pan_nstage", "pan_mstage", "pan_benign_histologic", "pan_benign_tumorsize")

hep_cols <- c("hep_lapthor", "hep_biliarystent", "hep_neoadj", "hep_neoadj_unk", "hep_neotherapy_140101","hep_neotherapy1","hep_neotherapy2","hep_neotherapy3","hep_neotherapy4","hep_neotherapy5", "hep_viral", "hep_hepb", "hep_hepc", "hep_otherviral", "hep_approach","hep_open_assist","hep_unplanned_conversion", "hep_livertext", "hep_con_partres", "hep_con_ablation_140101","hep_con_ablation1","hep_con_ablation2","hep_con_ablation3","hep_con_ablation4","hep_con_ablation5", "hep_con_op_ablation", "hep_con_op_ablation_unk", "hep_pringle", "hep_recon", "hep_recon_unk", "hep_drains", "hep_drains_unk", "hep_drains_bili", "hep_drains_bili_unk", "hep_drains_30d", "ddrainsremoval", "damylase", "hep_drains_removal_unk", "hep_invasive", "hep_invasive_unk", "hep_invasive_type", "hep_invasive_type1", "hep_invasive_type2", "hep_invasive_type3", "hep_invasive_type4", "hep_invasive_type5", "hep_peakinr", "hep_peakinr_unk", "hep_peakbili", "hep_peakbili_unk", "hep_bileleakage", "hep_bileleakage_type", "hep_bileleakage_intervention", "hep_liverfail", "hep_liverfail_type", "hep_liverfail_grade", "hep_pathres", "hep_histologic", "hep_tstage", "hep_nstage", "hep_mstage", "hep_sec_histologic", "hep_sec_numtumors", "hep_sec_tumorsize", "hep_benign_histologic", "hep_benign_lesion", "hep_peakcreat", "hep_peakcreat_unk")

app_cols <- c("app_ultra","app_img_ultra","app_setting_ultra","app_ct","app_img_ct","app_setting_ct","app_mri","app_img_mri","app_setting_mri","app_pathres", "app_perfabscess", "app_perforation", "app_abscess","app_approach","app_open_assist","app_unplanned_conversion", "app_appendix", "app_intraabscess", "app_intraabscess_intervention")

add_cols <- c("dindo")

## ---- COLUMN ORDERS ----

col_order <- c(puf_cols, aaa_cols, aie_cols, aio_cols, col_cols, pan_cols, hep_cols, app_cols)
