<h1>STATS PERM Extension Command</h1>

<h2>Introduction</h2>
This extension procedure provides permutation tests for simple two-group t tests, anova, and regression.  These tests do not rely on a normality assumption, and they are appropriate even for small datasets where asymptotic properties might not be reliable.
<p>
This procedure does not support case weights or split files.   They will be ignored with a warning.
Cases with missing data will be excluded listwise.
Variable measurement levels are used to provide appropriate handling for categorical and continuous (scale) variables, so be sure that these are set correctly.  The dialog box
generates syntax for the STATS PERM extension command.
</p>
<H2>Syntax</h2>

<div class="syntax">
<p>STATS PERM</br>
 DEPVAR = Dependent Variable<sup>&#42;</sup> <br/>
 INDVARS list of independent variables<br/>
 INTERACTIONSA group of variables <br/>
 INTERACTIONSB group of variables <br/>
 INTERACTIONSC group of variables <br/>
 ORDERA highest degree of interactions for INTERACTIONSA <BR/>
 ORDERB highest degree of interactions for INTERACTIONSB <BR/>
 ORDERC highest degree of interactions for INTERACTIONSC <BR/>
 IDVAR = ID variable, required if saving residuals or predicted values<br/>
 RESVAR = name for new variable for residuals<br/>
 PREDVAR = name for new variable for predicted values</br>
 
 <p>/OPTIONS<br/>
 NPERM= number of permutations to create</br>
 METHOD = method for generating permutations</br>
 PLOTCOEF = NO<sup>&#42;&#42;</sup> or YES</br>
</p>

<p>/HELP</br>
STATS PERM /HELP displays this information and does nothing else.</p>

<p><sup>&#42;</sup> Required<br/>
<sup>&#42;&#42;</sup> Default</p>
</div>


<pre class="example"><code>
STATS PERM DEPVAR=salary INDVARS=gender jobtime 
INTERACTIONSA = minority jobcat RESVAR=resids PREDVAR=preds IDVAR=id
/OPTIONS NPERM=10000 METHOD=FREEDMAN_LANE 
/DISPLAY PLOTCOEF=no.
</code></pre>

<h2>Details</h2>
<p>
<p><strong>DEPVAR</strong> specifies a scale dependent variable.
<p><strong>INDVARS</strong> specifies independent variables to enter additively.
<p><strong>INTERACTIONSA</STRONG> Specifies independent variables to enter as interactions.
<p>For a two group t test or anova model, specify the group variable (simple t test) or variables (anova and regression).  Variables with a categorical measurement level will be converted to dummy variables.  Variables in the <b>INDVARS</b> list will be additive, and variables in the
<b>INTERACTIONSA</b> list will be multiplicative with all combinations included.  For example,
if there are three interaction variables, x, y, and z the model will include x, y, z, x*y, x*z, y*z, and x*y*z.
Both categorical and scale variables can be used.</p>
<p><strong>INTERACTIONSB</strong> and <strong>INTERACTIONSC</STRONG> allow for two additional
groups of interaction variables.</p>
<p><strong>ORDERA</STRONG>, <strong>ORDERB</STRONG>, and <strong>ORDERC</STRONG> specify the highest degree of the corresponding INTERACTIONS variable.
By default, that is determined by the number of variables in the INTERACTIONS list, but you
can limit it. In the previous example, specifying all two-way would include x, y, z, x*y, x*z, and y*z but
not x*y*z.  Since main effects for interaction variables
are automatically included, you do not need to also list those variables in the Independent Variables box.

<p><strong>IDVAR</strong>, <STRONG>RESVAR</STRONG>, and <strong>PREDVAR</STRONG> specify
an ID variable and names for new variables to hold the residuals and predicted values.
The ID variable is required if either RESVAR or PREDVAR is specified.
The ID values must be unique, and the input must be in ascending sort order.</p>

<p><strong>NPERM</strong> specifies the number of permutations to generate.  The default
value is 5000.</p>

<p><strong>METHOD</strong> specifies how to generate the permutations.
There are seven choices.  freedman_lane is the default.</p>
<p>When there are multiple independent variables involved, such as typical for anova and regression, for
each test, one variable is selected at a time, and all the others are considered nuisance variables, Permutation tests assume exchangeability under the null hypothesis, but the nuisance variables
violate this assumption, so the data are transformed in order to reduce the effect of the nuisance variables
before performing the tests.  The output shows all the estimated coefficients, each computed with
the effect of the other variables removed.<p>
<p>The first reference below explains the seven methods available. Here is an extract from that
documentation.  y is the dependent variable, D is the matrix of nuisance variables, and X is the variable
of interest.
<p>
The <b>freedman_lane</b> method  works as follows: we
first fit the “small” model which only uses the nuisance variables D as predictors. Then, we
permute its residuals and add them to the fitted values. Theses steps produce the permuted
response variable which constitutes the “new sample”. It is fitted using the unchanged
design D and X. In this procedure, only the residuals are permuted and they are supposed
to share the same expectation (of zero) under the null hypothesis. For each permutation,
the effect of nuisance variables is hence reduced.
<p>
The <b>manly</b> method simply permutes the
response (this method is sometimes called raw permutations). Even if this method does not
take into account the nuisance variables, it still has good asymptotic properties when using
studentized statistics. 
<p><b>draper_stoneman</b> permutes the design of interest (note that without
nuisance variables permuting the design is equivalent to permuting the response variable).
However, this method ignores the correlation between D and X that is typically present in
regressions or unbalanced designs.

<p>For the <b>dekker</b> method, we first orthogonalize X with
respect to D, then we permute the design of interest. This transformation reduces the influence
of the correlation between D and X and is more appropriate for unbalanced design. 
<p>
The
<b>kennedy</b> method orthogonalizes all of the elements (y, D and X) with respect to the nuisance
variables, removing the nuisance variables in the equation, and then permutes the obtained
response. Doing so, all the design matrices lie in the span of X, a sub-space of observed
design X and D. However this projection modifies the distribution of the residuals that lose
exchangeability for original IID data). 
<p>
The <b>huh_jhun</b> method is similar to
kennedy but it applies a second transformation to the data to ensure exchangeability (up
to the second moment.  It implies that the P’s matrices for the huh_jhun
method have smaller dimensions. 
<p>
The <b>terBraak</b> method is similar to freedman_lane but
uses the residuals of the full model. This permutation method creates a new response variable
y∗ which assumes that the observed value of the estimate ˆ β|y is the true value of β.
Computing the statistic using y*, X, D would not produce a permutation distribution under
the null hypothesis. To circumvent this issue, the method changes the null hypothesis when
computing the statistics at each permutation to H0 : β = ˆ β|y = (X⊤RDX)−1X⊤RDy|y. The
right part of this new hypothesis corresponds to the observed estimate of the parameters of
interest under the full model, and implicitly uses a pivotal assumption. Note that terBraak
is the only method where the statistic computed with the identity permutation is different
from the observed statistic. The notation RD,X means that the residuals matrix is based on
the concatenation of the matrices D and X. See Section 5.2 for advises on the choice of the
method.
<p>

<p><strong>PLOTCOEF</strong> specifies whether to plot the coefficient distributions.  If there are
many variables, it may be necessary to stretch out the plot in the SPSS Viewer window.

<h2>Acknowledgements</h2>
This procedure uses the R permuco package by Frossard J. Renaud from CRAN.
<h2>References</h2>
<p>Frossard J, Renaud O (2021). “Permutation Tests for Regression, ANOVA, and Comparison of Signals: The permuco Package.” Journal of Statistical Software, 99(15), 1–32. doi:10.18637/jss.v099.i15.
<a href="https://cran.r-project.org/web/packages/permuco/vignettes/permuco_tutorial.pdf">permuco</a>
<p><a href="https://en.wikipedia.org/wiki/Permutation_test">Wikipedia Permutation Tests</a>
</body>
<p style="font-size:80%;">
<p>&copy; Copyright Jon K Peck 2025</p>
