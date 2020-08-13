yes_no_cols <- c("electsurg","smoke","ventilat","hxcopd","ascites","hxchf","hypermed",
                 "renafail","dialysis","discancr","wndinf","steroid","wtloss","bleeddis","transfus","emergncy",
                 "sssipatos","dssipatos","ossipatos","pnapatos","ventpatos","utipatos","sepsispatos",
                 "sepshockpatos","returnor","stillinhosp","reoperation1","retorrelated","reoperation2","retorrelated2","reoperation3",
                 "readmission1","unplannedreadmission1","readmrelated1","readmission2","unplannedreadmission2","readmrelated2",
                 "readmission3","unplannedreadmission3","readmrelated3", "readmission4","unplannedreadmission4","readmrelated4",
                 "readmission5","unplannedreadmission5","readmrelated5", "etoh", "dnr", "cpneumon", "esovar","hxmi","prvpci","prvpcs",
                 "hxangina","hxpvd","restpain","impsens", "hemi", "hxtia","cva", "cvano", "tumorcns","para","quad","chemo","radio","pregnancy",
                 "proper30", "readmission","unplanreadmission","reoperation", "col_steroid", "col_mech_bowel_prep","col_oral_antibiotic", "col_chemo",
                 "col_margins","col_ileus")

numscale_cols <- c("wndclas","asaclas")

complication_cols <- c("supinfec","wndinfd","orgspcssi","dehis","oupneumo","reintub","pulembol","failwean","renainsf","oprenafl","urninfec",
                       "cnscva","cdarrest","cdmi","othbleed","othdvt","othsysep","othseshock","othcdiff")

date_cols <- c("admyr","operyr","yrdeath","hdisdt", "admsyr","sdisdt")

integer_cols <- c("caseid","height","weight","optime","tothlos", "admqtr","htooday", "dsupinfec","dwndinfd","dorgspcssi", "doupneumo","dreintub",
                  "dpulembol","dfailwean","drenainsf","doprenafl","durninfec","dcnscva","dcdarrest","dcdmi","dothbleed","dothdvt",
                  "dothsysep","dothseshock","dopertod","doptodis","dothcdiff", "retorpodays", "retor2podays", "readmpodays1", "readmpodays2",
                  "readmpodays3", "readmpodays4", "readmpodays5", "dprna", "dprbun", "dprcreat", "dpralbum", "dprbili", "dprsgot", "dpralkph",
                  "dprwbc", "dprhct", "dprplate", "dprptt", "dprpt", "dprinr", "nsupinfec", "nwndinfd", "norgspcssi", "ndehis", "noupneumo",
                  "nreintub", "npulembol", "nfailwean", "nrenainsf", "noprenafl", "nurninfec", "ncnscva", "ncdarrest", "ncdmi", "nothbleed",
                  "nothdvt", "nothsysep", "nothseshock", "nothcdiff", "packs", "pgy", "mallamp", "rbc", "anesurg","surgane","dpatrm","anetime",
                  "stooday","totslos", "dsdtohd","col_nodeseval")

numeric_cols <- c("prsodm","prbun","prcreat","pralbum","prbili","prsgot","pralkph","prwbc","prhct","prplate","prptt","prinr","prpt",
                  "mortprob","morbprob", "workrvu", "otherwrvu1", "otherwrvu2", "otherwrvu3", "otherwrvu4", "otherwrvu5", "otherwrvu6",
                  "otherwrvu7", "otherwrvu8", "otherwrvu9", "otherwrvu10", "conwrvu1", "conwrvu2")

reason_cols <- c("readmsuspreason1", "readmunrelsusp1", "readmsuspreason2", "readmunrelsusp2", "readmsuspreason3", "readmunrelsusp3",
                 "readmsuspreason4", "readmunrelsusp4", "readmsuspreason5", "readmunrelsusp5")

redundant_cols <- c('race_new','readmission','unplanreadmission','reoperation')
