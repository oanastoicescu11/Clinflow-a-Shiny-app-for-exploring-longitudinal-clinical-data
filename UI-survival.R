tabPanel(
    "Survival",
    
    # Application title.
    headerPanel(""),
    
    sidebarPanel(
        uiOutput("survivalparams"),
        uiOutput("surv_func_to_apply"),
        uiOutput("survlayer"),
        uiOutput("layer"),
        checkboxInput(
            "checklayer",
            "Add more covariates to the model and adjust them to see their effect"
        ),
        actionButton("plot_survival", "Plot survival curves")
    ),
    
    mainPanel(
        tabsetPanel(
            tabPanel("Plot",
                     plotOutput("plot_predicted")),
            tabPanel("Model Summary", verbatimTextOutput("model_summary")),
            tabPanel(
                "Model Forest plot",
                plotOutput("forest_plot"),
                helpText(
                    "For categorical variables, the reference group is set at a category, or at the lowest value if the groups have numerical
                     coding. The reference group is used for comparison and is set at a hazard ratio of 1.
                     With a continuous variable, the hazard ratio indicates the change
                     in the risk of event if the parameter in question rises by one unit. A hazard ratio of 1 means lack of association,
                     a hazard ratio greater than 1 suggests an increased risk,
                              and a hazard ratio below 1 suggests a smaller risk."
                )
            ),
            helpText(
                "If the model does not converge i.e shows <Inf> for a variable or group in the confidence interval, or the hazard plot cannot be displayed,
                     it means that there is not enough data in that category to calculate hazard ratio. Consider filtering your data or using different
                     covariates."
            ),
            id = "tabs"
        )
    )
)