# Project:   mi-gspcr
# Objective: Analysis of pooled data
# Author:    Edoardo Costantini
# Created:   2023-08-17
# Modified:  2023-09-12
# Notes:

# Load Packages
source("0-prep-load-packages.R")

# Load variable type map
var_types <- readRDS("../input/var_types.rds")

# Load ipmuted data ------------------------------------------------------------

mids_migspcr <- readRDS("../output/20230817-155605-mids-mi-gspcr.rds")
mids_miexpert <- readRDS("../output/20230829-100551-mids-mi-expert.rds")

# Estiamte models --------------------------------------------------------------

# Define model variables
model_vars <- c(
    # Left / Right voting
    lr = "v174_LR",

    # Country
    country = "country",

    # Female
    sex = "v225",

    # Employment Status
    SES = "v246_egp",

    # Native attitudes (mean of items)
    nativ_1 = "v185", # jobs
    nativ_2 = "v186", # crime
    nativ_3 = "v187", # strain on welfare

    # Authoritarian Attitudes
    # Low and order attitudes
    strongL = "v145",
    order = "v110",

    # Political Interest
    pol_1 = "v97",

    # Political Action (mean of items)
    pa_1 = "v98",
    pa_2 = "v99",
    pa_3 = "v100",
    pa_4 = "v101",

    # Covariates
    age = "age_r3",
    edu = "v243_ISCED_1",
    mat = "v234",

    # urb = v276_r
    urb = "v276_r",

    # Religiousness
    rel = "v54",

    # Denomination
    denom = "v52_r"
)

# Make model formula
lm_model_3 <- paste0(model_vars[1], " ~ ", paste0(model_vars[-1], collapse = " + "))

# Put imputed datasets in long format to facilitate computation of derived variables
long_migspcr <- mice::complete(mids_migspcr, "long", include = TRUE)
long_miexpert <- mice::complete(mids_miexpert, "long", include = TRUE)

# Define complete cases based on analysis model
cc_data <- mids_migspcr$data[rowSums(is.na(mids_migspcr$data)) == 0, model_vars]

# Put datasets in the same list
long_list <- list(
    migspcr = long_migspcr,
    expert = long_miexpert,
    cc = cc_data
)

# Compute derived variables on every dataset
long_list_derived <- lapply(
    long_list,
    function(dat) {
        within(
            data = dat,
            expr = {
                Female <- fct_collapse(v225,
                    .no = levels(dat$v225)[1],
                    .yes = levels(dat$v225)[2]
                )
                Employment <- fct_collapse(v246_egp,
                    .empl = levels(dat$v246_egp)[c(2:5, 8:11)],
                    .self = levels(dat$v246_egp)[c(6, 7, 12)],
                    .unem = c("not applicable")
                )
                Occupation <- factor(
                    v246_egp,
                    levels = levels(v246_egp),
                    labels = paste0(".", gsub("\\s*:(.*)", "", levels(v246_egp)))
                )
                Nativist <- (as.numeric(v185) + as.numeric(v186) + as.numeric(v187)) / 3
                Leader <- factor(
                    v145,
                    levels = levels(v145),
                    labels = paste0(".", levels(v145)),
                    ordered = FALSE
                )
                Issue <- factor(
                    v110,
                    levels = levels(v110),
                    labels = paste0(".", c("order", "say", "prices", "speech"))
                )
                Political_interest <- as.numeric(v97)
                Political_action <- (as.numeric(v98) + as.numeric(v99) + as.numeric(v100) + as.numeric(v101)) / 4
                Age <- age_r3
                Education <- fct_collapse(v243_ISCED_1,
                    .primary = levels(dat$v243_ISCED_1)[1:2],
                    .secondary = levels(dat$v243_ISCED_1)[3:5],
                    .tertiary = levels(dat$v243_ISCED_1)[6:9]
                )
                Married <- fct_collapse(v234,
                    .yes = levels(dat$v234)[1:2],
                    .no = levels(dat$v234)[6],
                    .past = levels(dat$v234)[3:5]
                )
                Urb <- fct_collapse(factor(v276_r),
                    .less5000 = 1,
                    .less20000 = 2,
                    .less100000 = 3,
                    .more100000 = 4:5
                )
                Attendance <- fct_collapse(v54,
                    .sometimes = levels(dat$v54)[4:6],
                    .never = levels(dat$v54)[7],
                    .often = levels(dat$v54)[1:3]
                )
                Denom <- fct_collapse(v52_r,
                    .none = levels(dat$v52_r)[2],
                    .christian = levels(dat$v52_r)[1],
                    .other = levels(dat$v52_r)[3:5]
                )
            }
        )
    }
)

# Revert to mids objects
mids_derived <- lapply(long_list_derived[1:2], as.mids)

# Attach complete cases to mids
mids_derived$cc <- long_list_derived$cc

# Save datasets for future processing
saveRDS(mids_derived, "../output/estimation-data.rds")

# Read the data
data_est <- readRDS("../output/estimation-data.rds")

# Estimate model based on GSPCR
fits <- lapply(
    data_est,
    function(x) {
        with(
            x,
            lm(v174_LR ~
                Female +
                # Employment +
                Occupation +
                Nativist +
                Leader +
                Issue +
                Political_interest +
                Political_action +
                Nativist +
                Married +
                Urb +
                Attendance +
                Denom)
        )
    }
)

# Pool GSPCR
pools <- lapply(
    fits[1:2],
    pool
)

# Attach CC results
estiamtes <- list(
    migspcr = pools$migspcr$pooled,
    expert = pools$expert$pooled,
    cc = summary(fits$cc)$coefficients
)

# Define what to compare
measures <- c("estimate", "ubar", "b", "t", "riv", "lambda", "fmi")

# Compare one by one
measures_compared <- lapply(measures, function(x) {
    data.frame(
        estiamtes$expert[, 1:2],
        expert = round(estiamtes$expert[, x], 5),
        gscpr = round(estiamtes$migspcr[, x], 5)
    )
})

# Give meaningful names
names(measures_compared) <- measures

# Append CC analysis Estimates
cbind(
    measures_compared$estimate,
    CC = estiamtes$cc[, "Estimate"]
)

# Append CC analysis Standard errors
cbind(
    measures_compared$ubar,
    CC = estiamtes$cc[, "Std. Error"]
)

# Prepare data for plots -------------------------------------------------------

# Melt every MI method
estimates_mi <- lapply(
    c("migspcr", "expert"),
    function(x) {
        # Attach method name
        estiamtes[[x]] <- cbind(
            method = factor(x),
            estiamtes[[x]]
        )

        # Reshape for ggplot
        reshape2::melt(estiamtes[[x]], id.vars = c("term", "method"))
    }
)

# Append them
estimates_mi_gg_shape <- do.call(rbind, estimates_mi)

# Define the CC results
estimates_cc <- data.frame(estiamtes$cc)

# Harmonize names of CC results
colnames(estimates_cc) <- c("estimate", "t", "tvalue", "pvalue")

# Attach a term column
estimates_cc <- cbind(
    term = row.names(summary(fits$cc)$coefficients),
    estimates_cc
)

# Attach a term column
estimates_cc <- cbind(
    term = row.names(summary(fits$cc)$coefficients),
    method = factor("cc"),
    estimates_cc
)

# Shape CC estimates for plot
estimates_cc_gg_shape <- reshape2::melt(
    estimates_cc,
    id.vars = c("term", "method")
)

# Collect results
estimates_gg_shape <- rbind(
    estimates_mi_gg_shape,
    estimates_cc_gg_shape
)

# Save datasets for future processing
saveRDS(estimates_gg_shape, "../output/estimates-gg-shape.rds")

# Read the data
gg_shape <- readRDS("../output/estimates-gg-shape.rds")

# Plots ------------------------------------------------------------------------

# Decide what to plot
parameters <- c("estimate", "t", "ubar", "b", "fmi")
parameter <- "estimate"
terms <- levels(gg_shape$term)[-1]

# Make a plot for every measure
gg_plots <- lapply(parameters, function(parameter) {
    gg_shape %>%
        filter(
            term %in% terms,
            variable == parameter
        ) %>%
        mutate(value = abs(value)) %>% 
        ggplot(
            aes(
                x = term,
                y = value,
                fill = method
            )
        ) +
        scale_fill_manual(values = c("#aaaaaa", "#D4D4D4", "#ffffff")) +
        geom_bar(
            stat = "identity",
            position = position_dodge(),
            colour = "black",
            alpha = 0.75
        ) +
        labs(
            y = parameter
        ) +
        theme(
            # X axis layout
            axis.text.x = element_text(
                angle = 315,
                vjust = 0.5,
                hjust = 0
            ),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank(),
            # Grid
            panel.border = element_rect(color = "#D4D4D4", fill = NA, size = .5),
            # remove the vertical grid lines
            panel.grid.major.x = element_blank(),
            # explicitly set the horizontal lines (or they will disappear too)
            panel.grid.major.y = element_line(size = .1, color = "black"),
            # Legend
            legend.title = element_blank(),
            legend.key.size = unit(0.3, "cm"),
            # Background
            panel.background = element_rect(fill = NA, color = "gray")
        )
})

# Patchwork --------------------------------------------------------------------

# Estiamtes and variability
(
    gg_plots[[1]] +
        ylab("Estimate") +
        theme(
            axis.text.x = element_blank()
        )
) / (
    gg_plots[[2]] +
        ylab("Variability")
) +
    plot_layout(
        guides = "collect"
    ) &
    theme(legend.position = "top")

# MI variability# Patchwork
(
    gg_plots[[3]] +
        theme(
            axis.text.x = element_blank()
        )
) / (
    gg_plots[[4]] +
        theme(
            axis.text.x = element_blank()
        )
) / (
    gg_plots[[5]]
) +
    plot_layout(
        guides = "collect"
    ) &
    theme(legend.position = "top")
