# ==============================================================================
# estimate the causal effect of teacher turnover on student test scores
# note that this relies on the exclusion restriction, which is fundamentally
# unobservable.
# ==============================================================================
print("importing pre-processed saeb hierarchical")

run_module(
    "preprocess_saeb_hierarchical.R"
)

# add dummy for first year of the electoral term
saeb_hierarchical <- saeb_hierarchical %>%
    mutate(
        first_year = if_else(year == election_year + 1, 1, 0)
    )

print("begin estimation of instrumental variables model")

fe <- c(
    "as.factor(year)",
    "as.factor(state)"
)

predictor <- "turnover_index"
response <- "grade_exam"

controls <- c(
    teacher_covariates,
    principal_covariates,
    student_covariates,
    school_covariates,
    mun_covariates,
    fe
)

formula_iv <- reformulate(
    c(predictor, controls),
    response
)

formula_iv_coalition <- formula_iv %>%
    update_formula_iv(
        endogenous = "turnover_index",
        instrument = "coalition_share"
    )

formula_iv_first_term <- formula_iv %>%
    update_formula_iv(
        endogenous = "turnover_index",
        instrument = "mayor_reelected"
    )

fit_iv_coalition <- AER::ivreg(
    formula_iv_coalition,
    data = saeb_hierarchical
)

fit_iv_first_term <- AER::ivreg(
    formula_iv_first_term,
    data = saeb_hierarchical
)

write_model(
    fit_iv_coalition,
    "fit_ivreg_coalition.rds"
)

write_model(
    fit_iv_first_term,
    "fit_ivreg_first_term.rds"
)