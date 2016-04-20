require(readr)

setwd("F:/Kaggle/Homesite")
set.seed(3)

load("./Data/alldata_coded_v7.RData")
ptr <- alldata[alldata$split1 == 0,]
pte <- alldata[alldata$split1 == 2,]

ptr <- ptr[, !names(ptr) %in% c("split1",
                                "PersonalField16", "PersonalField17", "PersonalField18", "PersonalField19",
                                "PCOMP_TSNE_1", "PCOMP_TSNE_2", "PF16_PF19_tw", "PF17_PF19_tw", "PF18_PF19_tw", "PF16_PF17_PF18_thw",
                                "PF16_PF18_PF19_thw", "PF17_PF18_PF19_thw", "PF16_PF17_tw", "PF16_PF18_tw", "PF17_PF18_tw",
                                "ex_TSNE_1", "ex_TSNE_2", "qt", "coverage_int_4", "coverage_int_5")]
pte <- pte[, !names(pte) %in% c("split1",
                                "PersonalField16", "PersonalField17", "PersonalField18", "PersonalField19",
                                "PCOMP_TSNE_1", "PCOMP_TSNE_2", "PF16_PF19_tw", "PF17_PF19_tw", "PF18_PF19_tw", "PF16_PF17_PF18_thw",
                                "PF16_PF18_PF19_thw", "PF17_PF18_PF19_thw", "PF16_PF17_tw", "PF16_PF18_tw", "PF17_PF18_tw",
                                "ex_TSNE_1", "ex_TSNE_2", "qt", "coverage_int_4", "coverage_int_5")]

write_csv(ptr, "./Data/train_rf.csv")
write_csv(pte, "./Data/test_rf.csv")
