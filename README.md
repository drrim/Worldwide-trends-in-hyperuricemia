# Worldwide-trends-in-hyperuricemia
Worldwide trends in hyperuricemia from 2000 to 2023

library(brms)
f <- bf(
  Cases | trials(Sample) ~ 
    Sex +
    s(Mean_age, by = Sex, bs = "cr", k = 4) +
    log(GDP) * Representativeness +
    HUA_Definition +
    Urban +
    (1 + Mean_age + Sex | Superregion)
)

priors <- c(
  set_prior("student_t(3, -1.6, 1)", class = "Intercept"),
  set_prior("normal( 0.50, 0.40)", class = "b", coef = "SexMen"),     
  set_prior("normal(-0.20, 0.40)", class = "b", coef = "SexWomen"),   
  set_prior("normal( 0.40, 0.30)", class = "b", coef = "logGDP"),    
  set_prior("normal( 0.30, 0.30)", class = "b", coef = "Urban"), 
  set_prior("normal( 0.00, 0.30)", class = "b", coef = "RepresentativenessNational"),
  set_prior("normal( 0.00, 0.20)", class = "b", coef = "logGDP:RepresentativenessNational"),
  set_prior("normal( 0.00, 0.50)", class = "b", coef = "HUA_DefinitionW6M7"),
  set_prior("student_t(3, 0, 0.6)", class = "sds", coef = "s(Meanage, by = Sex, bs = \"cr\", k = 4)"),
  set_prior("student_t(3, 0, 0.60)", class = "sd", group = "Superregion", coef = "Intercept"),
  set_prior("student_t(3, 0, 0.40)", class = "sd", group = "Superregion", coef = "Meanage"),
  set_prior("student_t(3, 0, 0.40)", class = "sd", group = "Superregion", coef = "SexMen"),
  set_prior("student_t(3, 0, 0.40)", class = "sd", group = "Superregion", coef = "SexWomen"),
   set_prior("lkj(2)", class = "cor", group = "Superregion")
)

m <- brm(f
  , data = mydata,
  family = binomial(link = "logit"),
  chains = 4,
  cores = 4,
  iter = 5000,
  threads = threading(2),
  prior = priors,
  backend = "cmdstanr",
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  )
