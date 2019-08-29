# Tests ============================================================================================================
plot_chi_test(alpha = 0.05, alternative = "two.sided", df = 10, va = 10, vp = 10, annotations = TRUE, color = 3)
plot_chi_test(alpha = 0.05, alternative = "two.sided", df = 10, va = 10.000002, vp = 10, annotations = TRUE, color = 3)
plot_chi_test(alpha = 0.05, alternative = "two.sided", df = 10, va = 9, vp = 10, annotations = TRUE, color = 3)
plot_chi_test(alpha = 0.05, alternative = "two.sided", df = 10, va = 9.6, vp = 10, annotations = TRUE, color = 3)
plot_chi_test(alpha = 0.05, alternative = "two.sided", df = 10, va = 9.7, vp = 10, annotations = TRUE, color = 3)
plot_chi_test(alpha = 0.05, alternative = "two.sided", df = 10, va = 9.8, vp = 10, annotations = TRUE, color = 3)
plot_chi_test(alpha = 0.05, alternative = "two.sided", df = 10, va = 9.9, vp = 10, annotations = TRUE, color = 3)
plot_chi_test(alpha = 0.05, alternative = "two.sided", df = 10, va = 9.99, vp = 10, annotations = TRUE, color = 3)

# Montgomery
plot_chi_test(alpha = 0.05, alternative = "two.sided", df = 19, va = 0.0153, vp = 0.01, annotations = TRUE, color = 3)
plot_chi_test(alpha = 0.05, alternative = "greater"  , df = 19, va = 0.0153, vp = 0.01, annotations = TRUE, color = 3)

#plot_chi_test(alpha = 0.05, alternative = "greater"  , df = 10, va = 10, vp = 10, annotations = TRUE, color = 3)
#plot_chi_test(alpha = 0.05, alternative = "less"     , df = 10, va = 10, vp = 10, annotations = TRUE, color = 3)
