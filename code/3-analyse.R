# Project:   mi-gspcr
# Objective: Analysis of pooled data
# Author:    Edoardo Costantini
# Created:   2023-08-17
# Modified:  2023-09-11
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

# Put datasets in the same list
long_list <- list(
    migspcr = long_migspcr,
    expert = long_miexpert
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
            Occupation <- v246_egp
            nat_Att <- (as.numeric(v185) + as.numeric(v186) + as.numeric(v187)) / 3
            Strict_Leader <- factor(
                v145,
                levels = levels(v145),
                ordered = FALSE
            )
            Strict_Order <- v110
            pol_Int <- as.numeric(v97)
            pol_Act <- (as.numeric(v98) + as.numeric(v99) + as.numeric(v100) + as.numeric(v101)) / 4
            Age <- age_r3
            Education <- fct_collapse(v243_ISCED_1,
                .primary = levels(dat$v243_ISCED_1)[1:2],
                .secondary = levels(dat$v243_ISCED_1)[3:5],
                .tertiary = levels(dat$v243_ISCED_1)[6:9]
            )
            Marital <- fct_collapse(v234,
                .has_P = levels(dat$v234)[1:2],
                .never = levels(dat$v234)[6],
                .had_P = levels(dat$v234)[3:5]
            )
            Urb <- fct_collapse(factor(v276_r),
                .less5000 = 1,
                .less20000 = 2,
                .less100000 = 3,
                .more100000 = 4:5
            )
            Rel_attendance <- fct_collapse(v54,
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
})

# Revert to mids objects
mids_derived <- lapply(long_list_derived, as.mids)

# Estimate model based on GSPCR
fits <- lapply(
    mids_derived,
    function(x){
        with(
            x,
            lm(v174_LR ~
                Female +
                # Employment +
                Occupation +
                nat_Att +
                Strict_Leader +
                Strict_Order +
                pol_Int +
                pol_Act +
                nat_Att +
                Marital +
                Urb +
                Rel_attendance +
                Denom)
        )
    }
)

# Pool GSPCR
pools <- lapply(
    fits,
    pool
)

# Define what to compare
measures <- c("estimate", "ubar", "b", "t", "riv", "lambda", "fmi")

# Compare one by one
measures_compared <- lapply(measures, function(x) {
    data.frame(
        pools$expert$pooled[, 1:2],
        expert = round(pools$expert$pooled[, x], 5),
        gscpr = round(pools$migspcr$pooled[, x], 5)
    )
})

# Give meaningful names
names(measures_compared) <- measures

# Plots ------------------------------------------------------------------------

# Decide what to plot
parameters <- c("estimate", "ubar", "b", "fmi")
parameter <- "df"

# Make a plot for every measure
gg_plots <- lapply(parameters[-1], function(parameter) {
    # Create a dataset for plot
    data_plot <- data.frame(
        pools$expert$pooled[-1, 1, drop = FALSE],
        expert = abs(round(pools$expert$pooled[-1, parameter], 10)),
        gscpr = abs(round(pools$migspcr$pooled[-1, parameter], 10))
    )

    # Melt the data
    gg_shape <- reshape2::melt(data_plot, id.vars = "term", value.name = parameter)

    # Give useful names
    colnames(gg_shape) <- c("coefficient", "imputation", parameter)

    # Make plot
    gg_plot <- ggplot(
        data = gg_shape,
        aes(
            x = coefficient,
            y = get(parameter),
            fill = imputation
        )
    ) +
        scale_fill_manual(values = c("#D4D4D4", "#ffffff")) +
        geom_bar(
            stat = "identity",
            position = position_dodge(),
            colour = "black",
            alpha = 0.75
        ) +
        coord_flip() +
        labs(
            y = parameter
        ) +
        theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            # Grid
            panel.border = element_rect(color = "#D4D4D4", fill = NA, size = .5),
            # remove the vertical grid lines
           panel.grid.major.x = element_blank() ,
           # explicitly set the horizontal lines (or they will disappear too)
           panel.grid.major.y = element_line( size=.1, color="black" ),
            # Legend
            legend.title = element_blank(),
            legend.position = "right",
            legend.key.size = unit(0.3, "cm"),
            # Background
            panel.background = element_rect(fill = NA, color = "gray")
        )

    # Return
    return(gg_plot)
})

# Make FMI plot ----------------------------------------------------------------

# Create a dataset for plot
data_plot <- data.frame(
    pools$expert$pooled[-1, 1, drop = FALSE],
    expert = abs(round(pools$expert$pooled[-1, "estimate"], 10)),
    gscpr = abs(round(pools$migspcr$pooled[-1, "estimate"], 10))
)

# Melt the data
gg_shape <- reshape2::melt(data_plot, id.vars = "term", value.name = "estimate")

# Give useful names
colnames(gg_shape) <- c("coefficient", "imputation", "estimate")

# Make plot
gg_plot_fmi <- ggplot(
    data = gg_shape,
    aes(
        x = coefficient,
        y = get("estimate"),
        fill = imputation
    )
) +
    geom_bar(
        stat = "identity",
        position = position_dodge(),
        colour = "black",
        alpha = 0.75
    ) +
    coord_flip() +
    # Choose colors for fill
    scale_fill_manual(values = c("#D4D4D4", "#ffffff")) +
    labs(
        y = "estimate"
    ) +
    theme(
        # axis.text.y = element_text(
        #     angle = 270, # 90
        #     vjust = 0.5,
        #     hjust = 0 # 1
        # ),
        # Grid
        panel.border = element_rect(color = "#D4D4D4", fill = NA, size = .5),
        # remove the vertical grid lines
           panel.grid.major.x = element_blank() ,
           # explicitly set the horizontal lines (or they will disappear too)
           panel.grid.major.y = element_line( size=.1, color="black" ),
        # Legend
        legend.title = element_blank(),
        legend.position = "right",
        legend.key.size = unit(0.3, "cm"),
        # Background
        panel.background = element_rect(fill = NA, color = "gray")
    )

# Patchwork
gg_plot_fmi + gg_plots[[1]] + gg_plots[[2]] + gg_plots[[3]] +
    plot_layout(
        widths = unit(rep(10, 4), c("cm", "cm", "cm")),
        guides = "collect"
    ) &
    theme(legend.position = "top")