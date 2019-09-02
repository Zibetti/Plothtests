library(Cairo)
CairoPNG("./images/rplot_F_test_01_ct3.png", width = 945, height = 550)
plot_F_test(alpha = 0.05, alternative = "two.sided",
            df1 = 10, df2 = 10,
            v1 = 1.5, v2 = 2.5,
            ratio = 1,
            annotations = TRUE,
            color = 3)
dev.off()

CairoPNG("./images/rplot_F_test_01_ct8.png", width = 945, height = 550)
plot_F_test(alpha = 0.05, alternative = "two.sided",
            df1 = 10, df2 = 10,
            v1 = 1.5, v2 = 2.5,
            ratio = 1,
            annotations = TRUE,
            color = 8)
dev.off()



CairoPNG("./images/rplot_T_test_01.png", width = 945, height = 550)
plot_T_test(alpha = 0.05, alternative = "two.sided",
            var.equal = "equal",
            n1 = 10, m1 = 2.92, v1 = 1.5,
            n2 = 20, m2 = 4.20, v2 = 1.7,             
            delta0 = 0,
            annotations = TRUE,
            color = 3)
dev.off()

CairoPNG("./images/rplot_chi_test_01.png", width = 945, height = 550)
plot_chi_test(alpha = 0.05,alternative = "two.sided",
              df = 20, va = 20,
              vp = 15,
              annotations = TRUE,
              color = 3)
dev.off()

CairoPNG("./images/rplot_chi_test_01.png", width = 945, height = 550)
plot_chi_test(alpha = 0.05,alternative = "two.sided",
              df = 20, va = 20,
              vp = 15,
              annotations = TRUE,
              color = 3)
dev.off()


Cairo(file="Cairo_PNG_72_dpi.png", 
      type="png", 
      width=945, 
      height=550,
      dpi=72)
plot_chi_test(alpha = 0.05,alternative = "two.sided",
              df = 20, va = 20,
              vp = 15,
              annotations = TRUE,
              color = 3)
dev.off()
