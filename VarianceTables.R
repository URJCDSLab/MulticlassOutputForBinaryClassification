source("RunVarianceTables.R")

res <- run_variance_tables(
  cases = "pima"
)
# CTG
run_variance_tables(cases = "ctg")

# Phishing
run_variance_tables(cases = "phishing")

# Credit Card Fraud: métodos de comparación
run_variance_tables(cases = "credit_sota")

# Credit Card Fraud: propuestas GPS
run_variance_tables(cases = "credit_after")

#update_variance_analysis_md()