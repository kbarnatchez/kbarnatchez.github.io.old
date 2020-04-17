---
layout: post
title: Final Project
modified: 3/29/2020, 9:20:12
excerpt: "Details behind the main analysis"
comments: true
category: blog
---

Final Project Implementation Details
------------------------------------

This file provides details on the implementation of my analysis of the
TICS data in my final project.

Data loading/cleaning
---------------------

The raw data is in wide format – there are separate variables for TICS
scores over different rounds of interviews. I find it to be more
convenient to work with the data in long format, where there are
multiple observations per person for the TICS and age variables, since
this allows me to more easily account for individuals with less than 5
follow-up interviews in my code.

    tics_data <- read.csv('tics.data.2019.csv')

    # Make a long format of the TICS data
    tics_data$start_age <- tics_data$Age01
    tics_long <- reshape(tics_data, 
                         varying = c("TICS01", "Age01", "TICS02", "Age02", "TICS03",
                                     "Age03", "TICS04", "Age04", "TICS05", "Age05"), 
                         v.names = c("Age","TICS"),
                         timevar = "time", 
                         times = c("1","2","3","4","5"), 
                         direction = "long")

    tics_long <- tics_long[order(tics_long$ID,tics_long$time),]

    # Varnames are flipped for some reason. Correct this
    colnames(tics_long)[length(colnames(tics_long))-1] <- "Age"
    colnames(tics_long)[length(colnames(tics_long))-2] <- "TICS"

    tics_long$delta_age <- tics_long$Age - tics_long$start_age
    head(tics_long)

    ##     ID fam.num sex ptype Age.at.Enrollment Age.Last.Contact Deceased      BMI
    ## 1.1  1       1   0     1                64               78       No 22.96386
    ## 1.2  1       1   0     1                64               78       No 22.96386
    ## 1.3  1       1   0     1                64               78       No 22.96386
    ## 1.4  1       1   0     1                64               78       No 22.96386
    ## 1.5  1       1   0     1                64               78       No 22.96386
    ## 2.1  2       1   0     1                70               84       No       NA
    ##     SH.Ever.Smoked. MC.Aspirin MC.Stroke MC.Diabetes.Mellitus MC.HTN
    ## 1.1              No        Yes        No                   No    Yes
    ## 1.2              No        Yes        No                   No    Yes
    ## 1.3              No        Yes        No                   No    Yes
    ## 1.4              No        Yes        No                   No    Yes
    ## 1.5              No        Yes        No                   No    Yes
    ## 2.1              No         No        No                   No    Yes
    ##     MC.Coronary.Artery.Disease MC.Cancer MC.Heart.Attack Years.of.Education
    ## 1.1                         No       Yes              No                 16
    ## 1.2                         No       Yes              No                 16
    ## 1.3                         No       Yes              No                 16
    ## 1.4                         No       Yes              No                 16
    ## 1.5                         No       Yes              No                 16
    ## 2.1                         No        No              No                 19
    ##     start_age time TICS Age id delta_age
    ## 1.1        70    1   12  70  1         0
    ## 1.2        70    2   25  72  1         2
    ## 1.3        70    3   23  74  1         4
    ## 1.4        70    4   19  76  1         6
    ## 1.5        70    5   22  78  1         8
    ## 2.1        76    1   17  76  2         0

Distinguising confounders
-------------------------

To choose a set of confounders, I examine which variables are
significantly different between centenarian and non-centenarian
offspring. To do this, I just loop through each variable and either run
a regular regression of that variable on an indicator for centenarian
status if the variable is continuous, or a logistic regression if the
variable is binary. I flag the variables where the coefficient on the
centenarian variable does not contain 0 (if regular regression) or 1 (if
logistic regression) in the 95% credible interval. The code below shows
this procedure:

    diff_indices <- c()
    for (i in 3:17) {
    if (class(tics_data[,i])!="factor") {
      temp_model <- "
    model {
      for (i in 1:N) {
        y[i] ~ dnorm(mu[i],tau)
        mu[i] = b0 + b1*centarian[i]
      }

      # Priors
      b0 ~ dnorm(0,0.00001)
      b1 ~ dnorm(0,0.00001)
      tau ~ dgamma(1,1)

    }
    "
      temp_data <- list(N=nrow(tics_data),
                        y=tics_data[,i],
                        centarian=tics_data$ptype)
    } else {
      temp_model <- "
      model {
      for (i in 1:N) {
        y[i] ~ dbinom(p[i],1)
        logit(p[i]) = b0 + b1*centarian[i]
      }

      # Priors
      b0 ~ dnorm(0,0.00001)
      b1 ~ dnorm(0,0.00001)
    }
    "    
      temp_data <- list(N=nrow(tics_data),
                        y=as.numeric(tics_data[,i]=="Yes"),
                        centarian=tics_data$ptype)  
    }



    temp_model <- jags.model(textConnection(temp_model),data=temp_data)
    temp <- update(temp_model,n.iter = 1000)
    temp_results <- coda.samples(temp_model,'b1',n.iter = 5000)

    # Flag the exceptions
    if (!(0 >= summary(temp_results)[[2]][1] & 0 <= summary(temp_results)[[2]][5])) {
      diff_indices <- c(diff_indices,i)
    }
    }

We can look at which variables are found to significantly differ between
the two groups:

    colnames(tics_data)[diff_indices]

    ##  [1] "sex"                        "ptype"                     
    ##  [3] "Age.Last.Contact"           "Deceased"                  
    ##  [5] "BMI"                        "SH.Ever.Smoked."           
    ##  [7] "MC.Aspirin"                 "MC.Stroke"                 
    ##  [9] "MC.Diabetes.Mellitus"       "MC.HTN"                    
    ## [11] "MC.Coronary.Artery.Disease"

Main model
----------

Since I have a long format of the TICS data, I am able to loop through
each row of the dataset. Before running the model, I remove rows with
NA’s for TICS scores, as these are rows corresponding to follow-ups that
never occurred.

    tics_long_na <- na.omit(tics_long,cols=Age)

Then, I write the model in RJAGS:

    main_model_text <- "
    model {
      
      for (i in 1:N) {
      
        # Impute missing values in BMI
        BMI[i] ~ dnorm(theta[i],nu)
        theta[i] <- a.0 + 
                    a[1]*sex[i]  +
                    a[2]*smoke[i] +
                    a[3]*aspirin[i] +
                    a[4]*stroke[i] +
                    a[5]*cor[i] +
                    a[6]*diab[i] +
                    a[7]*alive[i]
      
        # Model for TICS
        tics[i] ~ dnorm(mu[i],tau)
        mu[i] <- alpha[fam[i]] + b_cent*C[i] + 
                 b_age0*(a0[i]-mean(a0[])) +
                 b_age*(delta_age[i]-mean(delta_age[])) + 
                 b_cent_age*(C[i]*(delta_age[i]-mean(delta_age[]))) +
                 g[1]*sex[i] +
                 g[2]*(BMI[i]-mean(BMI[])) + 
                 g[3]*smoke[i] +
                 g[4]*aspirin[i] +
                 g[5]*stroke[i] +
                 g[6]*cor[i] +
                 g[7]*diab[i] +
                 g[8]*alive[i]
      }
      
      # Set flat priors for each beta
      b_cent     ~ dnorm(0,0.00001)
      b_age0     ~ dnorm(0,0.00001)
      b_age      ~ dnorm(0,0.00001)
      b_cent_age ~ dnorm(0,0.00001)
      # b0         ~ dnorm(0,0.00001)
      for (i in 1:8) {
        g[i] ~ dnorm(0,0.0001) 
      }
      for (i in 1:7) {
        a[i] ~ dnorm(0,0.0001)
      }
      a.0 ~ dnorm(0,0.0001)
        
      # Set flat priors for random effects
      for (i in 1:max(fam)) {
        alpha[i] ~ dnorm(mu_a,tau_a)
      }
      
      # Hyper-priors
      tau   ~ dgamma(1,1)
      tau_a ~ dgamma(1,1)
      mu_a  ~ dnorm(0,0.00001)
      nu    ~ dgamma(1,1)
    }
    "
    main_data <- list(N=nrow(tics_long_na),
                      C=tics_long_na$ptype,
                      a0=tics_long_na$start_age,
                      fam=tics_long_na$fam.num,
                      delta_age=tics_long_na$delta_age,
                      tics=tics_long_na$TICS,
                      sex=tics_long_na$sex,
                      BMI=tics_long_na$BMI,
                      smoke=tics_long_na$SH.Ever.Smoked.,
                      aspirin=tics_long_na$MC.Aspirin,
                      stroke=tics_long_na$MC.Stroke,
                      cor=tics_long_na$MC.Coronary.Artery.Disease,
                      diab=tics_long_na$MC.Diabetes.Mellitus,
                      alive=tics_long_na$Deceased,
                      m=as.numeric(is.na(tics_long_na$BMI)==T)
                      )

And finally I run 50,000 draws of the MCMC (with 1,000 for burn-in and a
thinning interval of 10):

    main_model <- jags.model(textConnection(main_model_text),data=main_data)

    ## Warning in jags.model(textConnection(main_model_text), data = main_data): Unused
    ## variable "m" in data

    ## Compiling model graph
    ##    Resolving undeclared variables
    ##    Allocating nodes
    ## Graph information:
    ##    Observed stochastic nodes: 5620
    ##    Unobserved stochastic nodes: 488
    ##    Total graph size: 41361
    ## 
    ## Initializing model

    temp <- update(main_model,n.iter = 1000) # for burn-in

    # Check OR output
    main_results  <- coda.samples(main_model, c('b_cent','b_age0','b_age','b_cent_age'), n.iter = 50000,thin = 10 )
    summary(main_results)

    ## 
    ## Iterations = 1010:51000
    ## Thinning interval = 10 
    ## Number of chains = 1 
    ## Sample size per chain = 5000 
    ## 
    ## 1. Empirical mean and standard deviation for each variable,
    ##    plus standard error of the mean:
    ## 
    ##                Mean      SD  Naive SE Time-series SE
    ## b_age      -0.18763 0.03897 0.0005511      0.0005647
    ## b_age0     -0.19280 0.01555 0.0002199      0.0002769
    ## b_cent      0.52004 0.18761 0.0026532      0.0041150
    ## b_cent_age  0.06391 0.04660 0.0006590      0.0006511
    ## 
    ## 2. Quantiles for each variable:
    ## 
    ##               2.5%      25%      50%      75%   97.5%
    ## b_age      -0.2645 -0.21356 -0.18791 -0.16168 -0.1096
    ## b_age0     -0.2235 -0.20331 -0.19288 -0.18229 -0.1620
    ## b_cent      0.1394  0.39462  0.52348  0.64733  0.8765
    ## b_cent_age -0.0287  0.03302  0.06304  0.09425  0.1555

### Diagnostic plots

I check the autocorrelation of my main model MCMC draws:

    autocorr.plot(main_results)

![](project_details_files/figure-markdown_strict/unnamed-chunk-7-1.png)

By setting the thinning interval to 10, the autocorrelation is low (this
is an issue when not setting a thinning interval). By running the chain
long enough (50,000 draws, given 5,000 are kept since the thinning
interval is set ti 10) the draws appear stable:

    plot(main_results)

![](project_details_files/figure-markdown_strict/unnamed-chunk-8-1.png)

And testing

    geweke.diag(main_results)

    ## [[1]]
    ## 
    ## Fraction in 1st window = 0.1
    ## Fraction in 2nd window = 0.5 
    ## 
    ##      b_age     b_age0     b_cent b_cent_age 
    ##     0.6960    -0.1829     0.1422    -1.3754

Other details
-------------

I created the LaTeX tables in a couple steps. First, I extracted the
2.5, 50 and 75th percentiles from the model’s summary output and
converted it into a dataframe, with presentable row and column names.
Then, I used the user-written function xtable to convert the table into
LaTeX code. I made a quick function to do this flexibly:

    jags_tex <- function(results,names) {
    # Creates LaTeX table with 50-th pctile and 95% CI,
    # given JAGS summary results and names list for coeffs
    temp_df <- as.data.frame(summary(results)[[2]])

    # 50-th percentile gets its own col
    p50_col <- format(temp_df[,3],digits=3,n.small=1)

    # Make the CI col
    for (i in 1:nrow(temp_df)) {
      ci_col[i] <- paste("(",
                         as.character(format(temp_df[i,1],digits = 3,n.small=1)),
                         ",",
                         as.character(format(temp_df[i,5],digits = 3,n.small=1)),
                         ")",
                         sep = ""
      )
    }

    # Combine and output with xtable
    out_data <- data.frame(p50_col,ci_col)
    colnames(out_data) <- c('Posterior Median','95 percent CI')
    rownames(out_data) <- names

    print(xtable(out_data),
          sanitize.rownames.function = identity,
          booktabs=TRUE)

    }

### Plots

Since our main questions of interest relate to baseline TICS and TICS
rate of change in centenarian offspring vs controls, it’s natural to try
to capture these factors in plots. One approach is to plot the
distributions of each TICS survey (e.g. TICS1, TICS2, …), but to *split*
the data over centenarian offspring and controls, so that there are two
plots.

The `ggplot` function gives us everything we need to do this, once we
get the data into the right format. I split the data into the two
groups, then use the `melt` function to convert the two datasets into a
format suitable for `ggplot`:

    tics_cent <- data.frame(TICS1=tics_data$TICS01,
                       TICS2=tics_data$TICS02,
                       TICS3=tics_data$TICS03,
                       TICS4=tics_data$TICS04,
                       TICS5=tics_data$TICS05)

    cent_idx <- which(tics_data$ptype==1)
    nah_idx  <- which(tics_data$ptype==0)

    tics_cent <- tics_cent[cent_idx,]
    tics_control <- tics_cent[nah_idx,]

    tics_cent <- melt(tics_cent)

    ## Using  as id variables

    tics_control <- melt(tics_control)

    ## Using  as id variables

After formatting the data, we can plot it:

    # Plot TICS dists over time, split between centenarians and controls
    pcent <- ggplot(tics_cent,aes(x=value, color=variable)) + 
             geom_density(alpha=0.15) +
             xlim(0,30) +
             xlab('TICS score') +
             ylab('Density') +
             ggtitle('Centenarian offspring') +
             theme(legend.position = c(0.9,0.6),legend.background = element_blank())


    pnah <- ggplot(tics_control,aes(x=value, color=variable)) + 
            geom_density(alpha=0.15) +
            xlim(0,30) +
            xlab('TICS score') +
            ylab('Density') +
            ggtitle('Non-centenarian offspring') +
            theme(legend.position = c(0.9,0.6),legend.background = element_blank())


    plot_grid(pcent, pnah)

    ## Warning: Removed 485 rows containing non-finite values (stat_density).

    ## Warning: Removed 800 rows containing non-finite values (stat_density).

![](project_details_files/figure-markdown_strict/unnamed-chunk-12-1.png)

And we see it’s a bit unclear whether there are level/rate effects for
the centenarian group. That’s what modeling is for!
