library(foreign)
library(tidyverse)


snijders <- read.dta("https://data.princeton.edu/pop510/snijders.dta")

snijders <- snijders %>%
    mutate(
        iqvc = iq_verb - mean(iq_verb)
    )


fits <- group_by(snijders, schoolnr) %>% 
    do( lf = lm(langpost ~ iqvc, data = .))

ols <- data.frame(id = fits[[1]], t(sapply(fits[[2]],coef)))

names(ols) <- c("schoolnr", "sa", "sb")

snijders <- snijders %>%
    left_join(
        ols,
        by = "schoolnr"
    ) %>%
    mutate(
        fv = sa + sb * iqvc
    )

ggplot(snijders, aes(iqvc, fv, group = schoolnr)) + geom_line()