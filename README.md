# STATS_PERM
Introduction
This extension procedure provides permutation tests for simple two-group t tests, anova, and regression. These tests do not rely on a normality assumption, and they are appropriate even for small datasets where asymptotic properties might not be reliable.

This procedure does not support case weights or split files. They will be ignored with a warning. Cases with missing data will be excluded listwise. Variable measurement levels are used to provide appropriate handling for categorical and continuous (scale) variables, so be sure that these are set correctly. The dialog box generates syntax for the STATS PERM extension command.

Main Dialog
Dependent Variable:Select a scale level dependent variable.

Independent and Group Variables and Interaction Variables:

For a two group t test or anova model, specify the group variable (simple t test) or variables (anova and regression). Variables with a categorical measurement level will be converted to dummy variables. Variables in the Independent Variables box will be additive, and variables in the Interactions box will be multiplicative with all combinations included up to the default or specified maximum degree. For example, if there are three interaction variables, x, y, and z and allways is chosen for the degree, the model will include x, y, z, x*y, x*z, y*z, and x*y*z. Since main effects for interaction variables are automatically included, you do not need to also list those variables in the Independent Variables box.

Maximum Interaction Order specifies the highest interaction degree to include. By default, that is determined by the number of variables in the Interactions box, but you can limit it. In the previous example, specifying all two-way would include x, y, z, x*y, x*z, and y*z but not x*y*z.

In the dialog box, only one interaction group can be specified, but in syntax you can have up to three groups. Both categorical and scale variables can be used.

ID Variable ID variable, which is required if saving variables

Save Residuals As variable name not in use for residuals

Save Predicted Values As variable name not in use for predicted values

Options
Number of Permutations: Specify the number of permutations to be used.

Permutation Method:

When there are multiple independent variables involved, such as typical for anova and regression, for each test, one variable is selected at a time, and all the others are considered nuisance variables, Permutation tests assume exchangeability under the null hypothesis, but the nuisance variables violate this assumption, so the data are transformed in order to reduce the effect of the nuisance variables before performing the tests. The output shows all the estimated coefficients each computed with the effect of the other variables removed.


The first reference below explains the seven methods available. Here is an extract from that documentation. y is the dependent variable, D is the matrix of nuisance variables, and X is the variable of interest.

The freedman_lane method works as follows: we first fit the “small” model which only uses the nuisance variables D as predictors. Then, we permute its residuals and add them to the fitted values. Theses steps produce the permuted response variable which constitutes the “new sample”. It is fitted using the unchanged design D and X. In this procedure, only the residuals are permuted and they are supposed to share the same expectation (of zero) under the null hypothesis. For each permutation, the effect of nuisance variables is hence reduced.

The manly method simply permutes the response (this method is sometimes called raw permutations). Even if this method does not take into account the nuisance variables, it still has good asymptotic properties when using studentized statistics.

draper_stoneman permutes the design of interest (note that without nuisance variables permuting the design is equivalent to permuting the response variable). However, this method ignores the correlation between D and X that is typically present in regressions or unbalanced designs.

For the dekker method, we first orthogonalize X with respect to D, then we permute the design of interest. This transformation reduces the influence of the correlation between D and X and is more appropriate for unbalanced design.

The kennedy method orthogonalizes all of the elements (y, D and X) with respect to the nuisance variables, removing the nuisance variables in the equation, and then permutes the obtained response. Doing so, all the design matrices lie in the span of X, a sub-space of observed design X and D. However this projection modifies the distribution of the residuals that lose exchangeability (for original IID data).

The huh_jhun method is similar to kennedy but it applies a second transformation (V ⊤ D ) to the data to ensure exchangeability (up to the second moment, V ⊤ D RDy ∼ (0, In−(p−q)σ2)). It implies that the projctions matrices for the huh_jhun method have smaller dimensions.

The terBraak method is similar to freedman_lane but uses the residuals of the full model. This permutation method creates a new response variable y∗ which assumes that the observed value of the estimate ˆ β|y is the true value of β. Computing the statistic using y∗, X, D would not produce a permutation distribution under the null hypothesis. To circumvent this issue, the method changes the null hypothesis when computing the statistics at each permutation to H0 : β = ˆ β|y = (X⊤RDX)−1X⊤RDy|y. The right part of this new hypothesis corresponds to the observed estimate of the parameters of interest under the full model, and implicitly uses a pivotal assumption. Note that terBraak is the only method where the statistic computed with the identity permutation is different from the observed statistic. The notation RD,X means that the residuals matrix is based on the concatenation of the matrices D and X.

Plot Coefficients Check this box to plot the coefficient distributions. If there are many variables, it may be necessary to stretch out the plot in the SPSS Viewer window.

Acknowledgements
This procedure uses the R permuco package by Frossard J. Renaud from CRAN.
References
Frossard J, Renaud O (2021). “Permutation Tests for Regression, ANOVA, and Comparison of Signals: The permuco Package.” Journal of Statistical Software, 99(15), 1–32. doi:10.18637/jss.v099.i15. permuco

Wikipedia Permutation Tests

© Copyright Jon K Peck 2025
