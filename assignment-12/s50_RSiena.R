###############################################################################
###
### - Rscript02SienaVariableFormat.R: a script for the introduction to RSiena -
###
###                               version June 19, 2015
###############################################################################
#
# The introductory script is divided into the following script files:
# Rscript01DataFormat.R, followed by
# RScriptSNADescriptives.R, code for descriptive analysis of the data, and
# Rscript02SienaVariableFormat.R, which formats data and specifies the model, and
# Rscript03SienaRunModel.R, which runs the model and estimates parameters
# Rscript04SienaBehaviour.R, which illustrates an example of analysing the
# coevolution of networks and behaviour
# Written by Tom Snijders, with earlier contributions from Robin Gauthier,
# Ruth Ripley, Johan Koskinen, and Paulina Preciado.
#
# This script, Rscript02SienaVariableFormat.R, sets up the variables for analysis.
# The manipulations in this script requires that you have gone through the
# first part, "CALLING THE DATA AND PRELIMINARY MANIPULATIONS",
# of the script "Rscript01DataFormat.R" beforehand.

#### FORMATTING DATA ACCORDING TO THEIR ROLES AS VARIABLES IN A SIENA MODEL ####

# For this script, you will need the data read and modified in the script
# Rscript01DataFormat.R. If you have already ran that script, you may
# load the required workspace:
# load("WorkspaceRscript01.R")
# If not, to make this script self-contained, you may run the following commands:

s501d<-read.table("s50-network1.dat", header=FALSE, sep=" ", quote="\"'")
s501<-as.matrix(s501d[,2:51])

s502d<-read.table("s50-network2.dat", header=FALSE, sep=" ", quote="\"'")
s502<-as.matrix(s502d[,2:51])

s503d<-read.table("s50-network3.dat", header=FALSE, sep=" ", quote="\"'")
s503<-as.matrix(s503d[,2:51])

s50a<-read.table("s50-alcohol.dat", header=FALSE, sep=" ", quote="\"'")
s50s<-read.table("s50-smoke.dat", header=FALSE, sep=" ", quote="\"'")

#install.packages("RSiena")
library(RSiena)
friend.data.w1 <- s501
friend.data.w2 <- s502
friend.data.w3 <- s503
drink <- as.matrix(s50a[,2:4])
smoke <- as.matrix(s50s[,2:4])

# A number of objects need to be created in R, as preparations to letting
# siena07 execute the estimation. This will be indicated by
# A: dependent variables;
# B: explanatory variables;
# C: combination of dependent and explanatory variables;
# D: model specification.

# ---- A. ----------------------------------------------------------------------
# First we have to create objects for the dependent variables.

# sienaDependent creates a sienaDependent object, here a network,
# from a matrix or array or list of sparse matrix of triples.
# This object will have the role of a dependent variable in the analysis.
# The name of this network object (here: friendship) will be used
# in the output file.

friendship <- sienaDependent(
  array( c( friend.data.w1, friend.data.w2, friend.data.w3 ),
         dim = c( 50, 50, 3 ) ) )

# The integers in the dim() here refer to the number of nodes (senders,
# receivers) and the number of waves.
# This object is an array of dimension 50 x 50 x 3, representing
# three adjacency matrices, with a number of attributes.
# Note that this is an object of class

class(friendship)

# with specific attributes and methods associated with it.
# You can get the detailed information by requesting

dim( friendship )
attributes( friendship )

# If you only are interested in the value of one particular attribute,
# you can request this by, e.g.,

attributes( friendship )$type

# A very concise description of the friendship data is obtained by typing

friendship

# The function sienaDependent can also be used to create a behavior variable object
# with the extra argument type = "behavior".
# (Non-mentioned attributes get the default value, and in this case
# oneMode is the default; see below.)

# The 'drink' data (created in RscriptDataFormat.R ) is made available as a
# dependent behavior variable by the function

drinkingbeh <- sienaDependent( drink, type = "behavior" )

# the class, class(drinkingbeh), is still sienaDependent.

# (Note: only use the variable in ONE role in a given model:
#  behavior variable or changing covariate!)

# The options available for defining a sienaDependent object
# are displayed by typing

# ?sienaDependent

# This shows that next to one-mode (unipartite) and behavior dependent variables,
# also two-mode (bipartite) dependent variables are possible.
# You can infer that oneMode is the default type from the fact
# that it is mentioned first.

# To create bipartite network objects you need two node sets and must create
# the node sets too. The following is an example
# (not really meaningful, just for the syntax):

bfriendship <- sienaDependent(array(c(friend.data.w1, friend.data.w2, friend.data.w3),
                                    dim=c(50, 50, 3)),
                              "bipartite", nodeSet=c("senders", "receivers"))
senders <- sienaNodeSet(50, nodeSetName="senders")
receivers <- sienaNodeSet(50, nodeSetName="receivers")


# ---- B. ----------------------------------------------------------------------
# Second we construct objects for the explanatory (independent) variables.
# From the help request
#       ?sienaDataCreate
# we see that these can be of five kinds:
# coCovar            Constant actor covariates
# varCovar           Time-varying actor covariates
# coDyadCovar        Constant dyadic covariates
# varDyadCovar       Time-varying dyadic covariates
# compositionChange  Composition change indicators

# You can get help about this by the following requests:
#       ?coCovar
#       ?varCovar
#       ?coDyadCovar
#       ?varDyadCovar
#       ?sienaCompositionChange

# The variables available for this data set all are changing actor covariates.
# For illustrative purposes, we use smoking as observed at the first wave
# as a constant covariate:

smoke1 <- coCovar( smoke[ , 1 ] )

# This selects the first column of smoke,
# which contains the first wave observations,
# and makes it available as a constant covariate.
# This is the pattern for for evey covariate file, e.g.
#       Attr1 <- coCovar( Covariate1 )
# where Covariate1 is a matrix with dim(Covariate1) equal to n x 1
# Note, if Covariates is a matrix with dim(Covariates) equal to n x p
# you can create constant covariates through
#       Attr1 <- coCovar(Covariates[,1])
#       ...
#       Attrk <- coCovar(Covariates[,p])

# We use the drinking data as a changing covariate.
# The function varCovar creates a changing covariate object from a matrix;
# the name comes from 'varying covariate'.

alcohol <- varCovar( drink )

# You need at least three waves in the data set to define a varying covariate
# by the function varCovar as the previous wave is used
# as a predictor of the next wave.

# The command

attributes( alcohol )

# will tell you the information that RSiena now has added to the drink data.

# ---- C. ----------------------------------------------------------------------
# We now combine the dependent and independent variables.
# The function sienaDataCreate creates a Siena data object from input networks,
# covariates and composition change objects;
# the objects that earlier were created by sienaDependent will have the role
# of dependent variables, and similarly the other roles are predetermined
# by creation by the functions coCovar, varCovar,
# coDyadCovar, varDyadCovar, and sienaCompositionChange.

mydata <- sienaDataCreate( friendship, smoke1, alcohol )

# You may check the result by requesting

mydata

# You should now understand how this differs from the result of
#       mybehdata <- sienaDataCreate( friendship, smoke1, drinkingbeh )

# If you would like to use different names, you could request this as follows:
#        mydata <- sienaDataCreate( nominations = friendship, smoke1,
#                                   drinking = alcohol )

# For bipartite networks you would have to specify the node sets, e.g.,

mybidata <- sienaDataCreate(bfriendship,
                            nodeSets=list(senders, receivers))

# If you wish to use a covariate, the relevant nodeSet must match, e.g.,
balcohol <- varCovar(drink, nodeSet="senders")
mybidata <- sienaDataCreate(bfriendship, balcohol,
                            nodeSets=list(senders, receivers))

# but we will not use this in these scripts.
# This finishes the data specification. Now we have to specify the model.
#

# ---- D. ----------------------------------------------------------------------
################################################################################
###
### ---- DEFINING EFFECTS ------------------------------------------------------
###
################################################################################
# The data set as combined in mydata implies a certain set of effects
# that can be included in the specification of the model.
# The function getEffects creates a dataframe of effects with a number of extra
# properties for use in RSiena:

myeff <- getEffects( mydata )

# mydata is needed as an argument as the effects depend on the number
# and types of covariates and dependent variables.
# Before we explain the object myeff and how we shall be going to use it,
# we first produce a data description which is available now:

print01Report( mydata, modelname = 's50_3_init' )

# This writes a basic report of the data to the file
# s50_3_init.out in the current working directory. Locate and open it!
# Inspecting this is important because it serves as a check and also contains
# a number of basic descriptives.
# In this description you can see that the third wave data for alcohol are not used.
# This is because changing covariates are assumed to be constant from one wave until
# immediately before the next wave, so that the values for the last wave are ignored.

# Let us now consider the myeff object, which is used to specify the model.
# It is of the class "sienaEffects", and contains the model specification.
# You can inspect the current model specification by simply requesting

myeff

# For starting, the model specification is just a very limited default
# (including rates of change, outdegree and reciprocity only)
# To make a meaningful analysis, you will need to add to it.

# The rows of myeff correspond to the effects.
# By requesting

names( myeff )

# you see the type of information that is stored about the effects,
# i.e., the columns (variables) defined for the effects.
# If desired, more information about these variables can be obtained
# from the help files:
#      ?getEffects

# Some often used variables are effectName, shortName, type, and parameter.
# The set of available effects and their most used columns
# can be inspected as follows:

#       effectsDocumentation(myeff)

# This gives a long list of effects: all defined in Section 12 of the manual,
# as far as they are meaningful for dataset mydata.
# The "include" column defines whether effects are included in the model.

myeff$include

# Here the TRUE values correspond to the default model specification which,
# however, is not meant as a serious model, being too limited.
# There are 3 main ways to operate on myeff.
# 1. Using RSiena functions "includeEffects", "setEffects", etc;
# 2. Changing myeff in spreadsheet form by the function fix();
# 3. Changing myeff directly by operating on its elements.
# Which one to use is a matter of personal preference.
# The first way is most in line with the design philosophy of R,
# and allows you to save scripts that also can be used when
# there will be new versions of RSiena.
# Therefore, we suggest that for starting you only study
# option 1, ' Adding/removing effects using includeEffects'.
# The other two options are treated only for special
# and more difficult occasions.

# For identifying your effects you need the "shortName"s,
# which can be read in the manual (section "Mathematical definition of effects"),
# or obtained from the "effectsDocumentation()" function mentioned above.


# ---- 1. Adding/removing effects using includeEffects -------------------------
# The best way of specifying the model is by the includeEffects function.
# This function uses short names instead of full names.
# The short names are given by the effectsDocumentation() function
# mentioned above, and also are listed in the descriptions given in
# Section 12 of the manual.
# A different table of effect information, including short names,
# that covers effects available for whatever data sets,
# is available as a pdf file in the R directory, and can be opened by

#        RShowDoc("effects", package="RSiena")

# For illustration, let us start from scratch with a new sienaEffects object,
# and add the transitive triples and 3-cycles effects

myeff <- getEffects( mydata )
myeff <- includeEffects( myeff, transTrip, cycle3 )

# To see the current model specification,

myeff

# Note that we can set several effects in one go!
# To remove an effect, e.g., the 3-cycle effects

myeff <- includeEffects( myeff, cycle3, include=FALSE )

# Check again which effects now are included in the model

myeff

# ---- Adding/removing covariate related effects -------------------------------
# The short names do not differentiate between the covariates:
# e.g., the effects 'alcohol ego' and 'smoke1 ego' both have shortName 'egoX',
# and the command

myeff <- includeEffects( myeff, egoX )

# results in a message that does not (like the earlier one)
# confirm the newly included effect.
# The covariates are indicated by the variable "interaction1"
# in the sienaEffects object, listed as "inter1" in the result of
# effectsDocumentation(), and this has to be mentioned to include these effects:

myeff <- includeEffects( myeff, egoX, altX, egoXaltX,
                         interaction1 = "alcohol" )
myeff <- includeEffects( myeff, simX, interaction1 = "smoke1" )

# We check the results again:

myeff

# By looking at the help offered by

# ?includeEffects

# you can see how to include endowment and creation effects.
# Effects that depend on other variables, such as egoX, altX, etc. above,
# need the specification of these variables to define them.
# This is done by the interaction1 parameter
# when only one variable name is needed,
# and by interaction2 if there is a second variable involved,
# such as AltsAvAlt (see the manual).
# Although the names of these parameters are interaction1 and interaction2,
# this does not refer to an interaction as commonly used
# in statistical modelling!

# ---- Creating interaction effects --------------------------------------------
# As a special topic, let us show how interaction effects are created.

# A convenient method to include an interaction is offered by the
# includeInteraction function.
# This can be used to interact two or three effects
# (if the interactions are allowed, which depends on their interactionType;
# see the manual for this).
# The interaction between smoke1 ego and reciprocity, for instance,
# can be defined by the command

myeff <- includeInteraction( myeff, egoX, recip,
                             interaction1 = c("smoke1","") )
myeff

# This shows the interaction as an "unspecified interaction effect";
# but when printing results of the estimation the names of the
# interacting effects will be mentioned.
# E.g., an interaction between smoke1 ego and alcohol ego is defined by

#        myeff <- includeInteraction( myeff, egoX, egoX,
#                                    interaction1 = c( "smoke1", "alcohol" ) )

# Note that the keyword 'interaction1' used by RSiena is used for identifying
# the covariate for which the ego effect is selected, and does not
# refer to the interaction effect itself.
# If at least one of the interacting effects requires the interaction1 parameter
# for it specification, then this parameter is also required for the
# includeInteraction function.
# Then the two or three interaction1 parameters must be combined using c();
# the same goes for interaction2, if that also is necessary for the definition.

# A second special topic is how to access other characteristics of effects.
# This can be done by the setEffect function.
# E.g., the dense triads effects
# counts the number of triplets with at least xx ties,
# where xx is the parameter of the effect, which can be 5 or 6
# (note that 6 is the maximum number of ties in a triplet).
# The default is 5. This is changed to 6 by the command

myeff <- setEffect(myeff, denseTriads, parameter = 6)
myeff

# The 'parameter' keyword refers to the effect parameter, described in
# Section 12 of the manual.

# ---- 2. Adding/removing effects using fix() ----------------------------------
# fix calls a data editor internal to R, so we can manually edit the effects.

#        fix( myeff )

# How to use fix() is presented merely for getting to know what myeff is.
# In practical analysis it is more convenient
# to use routine "includeEffects" instead, as explained above.
# fix() may not be usable if you do not have tcl/tk available!
# Note that the top of the dataframe shows the names of the columns:
# name, effectName, etc.
# You can edit the "include" column by changing the TRUE and FALSE values
# as required; when the editor is closed, the new values are stored.
# When you make an error, however, the effects object may be corrupted.
# Therefore, this way of adding and removing effects is more risky.

# ---- 3. Adding/removing effects by direct manipulation of myeff --------------
# Alternatively we can edit the dataframe directly by using R functions.
# You are advised to skip this part ("3.") at first and second reading,
# and read it only if you wish to get more understanding
# of the internal structure of the effects object.
# The commands below are used to set "include" to TRUE or FALSE,
# as an alternative to using the data editor.
# The "include" column with values TRUE or FALSE will always be
# located at the 9th column,
# but transitive triplets will not always be at the 13th row as this depends
# on the number of periods and variables; further, the list of available effects
# changes over different versions of RSiena.
# Some examples are the following
# (preceded by # because not proposed to be applied).

#myeff[13,9] <- TRUE   #transitive triples
#myeff[17,9] <- TRUE   #3 cycles
#myeff[19,9] <- TRUE   #transitive ties
#myeff[29,9] <- TRUE   #indegree popularity (sqrt)
#myeff[33,9] <- TRUE   #outdegree popularity (sqrt)
#myeff[35,9] <- TRUE   #indegree based activity (sqrt)
#myeff[37,9] <- TRUE   #outdegree based activity (sqrt)
#myeff[46,9] <- TRUE   #indegree-indegree assortativity
#myeff[69,9] <- TRUE   #alcohol alter
#myeff[73,9] <- TRUE   #alcohol ego
#myeff[75,9] <- TRUE   #alcohol similarity
#myeff[83,9] <- TRUE   #alcohol ego x alcohol alter

# But in other choices of data, the effect numbers will change.
# This is a reason why this is not a convenient method.
# The following methods require more typing the first time,
# but can be re-used much more robustly.
# Several variants are given, so that you can use what suits you best.
# We give a small and meaningful model.

# To understand the R commands, recall that myeff is a data.frame,
# i.e., a two-dimensional array with named columns,
# and myeff[i,j] refers to row/effect i and its characteristic j.
# A file with the effect shortNames, for easy access to their exact wordings,
# is obtained by effectsDocumentation() (see above).
# The following commands can be used to select the
# five mentioned effects.
# Note that == is the logical "equals", & is the logical "and",
# and what is between the square brackets [...] makes a selection
# of the specified row and column of the "myeff" data frame.

#        myeff[myeff$effectName=='transitive triplets' &
#              myeff$type=='eval', 'include'] <- TRUE
#        myeff[myeff$effectName=='3-cycles' &
#              myeff$type=='eval', 'include'] <- TRUE
#        myeff[myeff$effectName=='smoke1 similarity' &
#              myeff$type=='eval', 'include'] <- TRUE
#        myeff[myeff$effectName=='alcohol alter' &
#              myeff$type=='eval', 'include'] <- TRUE
#        myeff[myeff$effectName=='alcohol ego' &
#              myeff$type=='eval', 'include'] <- TRUE
#        myeff[myeff$effectName=='alcohol ego x alcohol alter' &
#              myeff$type=='eval', 'include'] <- TRUE

# You can similarly add other ones.
# If you make a typing error in the effect name, there will be no warning,
# but also no consequence of your command.
# Therefore it is good to check the results,
# by requesting the list of effects now included in the model:

#        myeff

# Parameters of the model are estimated by the function siena07.
# This requires the data specification; the effects specification;
# and a number of parameters, or settings, for the estimation algorithm.
# The latter are contained in an object created by the function
# sienaAlgorithmCreate. You can look at the help provided by
# ?sienaAlgorithmCreate
# to find out about options that you may use here;
# for beginning users, only the two options mentioned below are relevant.
#
# Output will be written to a file with name projname.out, where projname is
# whatever name is given; the default (used if no name is given) is Siena.
# This file will be written to your current directory.
# New estimation runs will append to it.
# A new call to print01Report will overwrite it!

myalgorithm <- sienaAlgorithmCreate(projname = 's50_3')

# Let us first redefine the model, to obtain a simpler specification
# that will serve as an illustration here.

myeff <- getEffects( mydata )
myeff <- includeEffects( myeff, transTrip, cycle3 )
myeff <- includeEffects( myeff, egoX, altX, egoXaltX,
                         interaction1 = "alcohol" )
myeff <- includeEffects( myeff, simX, interaction1 = "smoke1" )
myeff

# The function siena07 actually fits the specified model to the data
# If you wish the pretty picture of Siena on the screen as information
# about the progress of the algorithm, type

ans <- siena07( myalgorithm, data = mydata, effects = myeff)

# (ans for "answer").
# If however you do not want the pretty picture, or if this leads to
# difficulties (which may happen e.g. on a Mac), then type

#	ans <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE)

# and intermediate information will be written to the console.

# Function siena07 produces a so-called sienaFit object, here called ans;
# and it fills in a few things in the sienaEffects object myeff,
# if this is the first use of myeff in a siena07 call.
# By using various different effects objects, i.e., with different names,
# you can switch between specifications.

# The batch = FALSE parameters will give a graphical user interface being opened
# which reports on the progress of the estimation algorithm;

# verbose = TRUE leads to extensive diagnostic information being sent
# to the console during the estimation, and results after the estimation
# (these results are also copied to the output file projname.out, see above);
# while batch=TRUE gives only a limited amount of printout sent to the console
# during the estimation (which is seen when clicking in the console,
# or more immediately if the Buffered Output is deselected in the Misc menu)
# which monitors the progress of the estimation algorithm in a different way.

# The call of siena07 leads to output in the file s50_3.out
# (or more generally projname.out,
# where projname is the name given in sienaAlgorithmCreate)
# and to the creation of the object which here is called ans (for "answer").

# To use multiple processors, e.g., if you wish to use 2 processes, request

#	ans <- siena07( myalgorithm, data = mydata, effects = myeff,
#                     nbrNodes = 2, useCluster = TRUE)

# Adjust the nbrNodes to the number available.
# If you wish to work on with other programs while running siena07,
# it is advisable to use one node less than the number of available processors.
# If you wish to use other machines as well,
# see the more detailed instructions below.
# You will then need to use the clusterString argument as well.
#
# For more advanced use, it can be helpful to have access to the networks
# simulated in the so-called third phase of the estimation algorithm.
# These networks can be used, e.g., for checking goodness of fit.
# This can be achieved by using the parameter returnDeps=TRUE.
# The fitted object ans will then have a component named "sims"
# which contains a list (each iteration) of lists (each data object)
# of lists (each dependent network or behavior variable) of edgelists for
# networks or vectors for behavior variables.
# See the manual for further explanation.
#
# This option when used with multiple processors would require
# rather a lot of communication between multiple processes,
# slowing down the computations,
# so it might be better to avoid using the two options together.


################### LOOKING AT THE RESULTS ################################

# The file "s50_3.out" will contain the results of the estimation.
# It is contained in the current directory ("getwd()").
# This file can be read by any text editor.
# A summary of the results is obtained on the screen by

ans

# and a larger summary by

summary(ans)

# Depending on the random seed and the model specification,
# the results could be something like the following.

#Estimates, standard errors and convergence t-ratios
#
#                                       Estimate   Standard   Convergence
#                                                    Error      t-ratio
#
#Rate parameters:
#  0.1      Rate parameter period 1      6.6141  ( 1.1703   )
#  0.2      Rate parameter period 2      5.1578  ( 0.8523   )
#
#Other parameters:
#  1.  eval outdegree (density)         -2.7149  ( 0.1236   )   0.0618
#  2.  eval reciprocity                  2.4114  ( 0.2188   )   0.0412
#  3.  eval transitive triplets          0.6381  ( 0.1491   )   0.0673
#  4.  eval 3-cycles                    -0.0564  ( 0.3012   )   0.0842
#  5.  eval smoke1 similarity            0.2631  ( 0.2078   )   0.0894
#  6.  eval alcohol alter               -0.0217  ( 0.0688   )   0.0399
#  7.  eval alcohol ego                  0.0387  ( 0.0834   )   0.0319
#  8.  eval alcohol ego x alcohol alter  0.1270  ( 0.0512   )   0.0412

# Overall maximum convergence ratio:    0.2287
# Total of 2244 iteration steps.

# The results can also be viewed externally in the output file s50_3.out
# It is advisable that you have a look at all three reports and
# understand how information is organized in each of them.

# To understand the table above, note that the "convergence t-ratio"
# is the t-ratio for convergence checking,
# not the t statistic for testing the significance of this effect!
# See Section 6.1.2 of the manual to understand this better.
# In the external output file, these are called
# "t-ratios for deviations from targets".
# The rule of thumb is that all t-ratios for convergence
# should ideally be less than 0.1 in absolute value,
# and the "Overall maximum convergence ratio" should be less than 0.25;
# this signifies good convergence of the algorithm.
# In the example here, this is the case.
# If this would not be the case, the best thing to do would be
# to continue the estimation, using the estimates produced here,
# and contained in ans, as the new initial values.
# This is explained below.
# Because the estimation algorithm is based on random simulations of the
# network evolution, there always will be small differences between
# different runs of the algorithm.
# To obtain "publication grade" estimates, where this variability is minimized,
# choose the value of parameter n3 in sienaAlgorithmCreate()
# ("Number of iterations in phase 3") larger than the default value of 1000;
# e.g., n3=3000.

# With function siena07 we made ans as the object containing
# all the results of the estimation. For example,

ans$theta

# contains the vector of parameter estimates while

ans$covtheta

# contains the covariance matrix of the estimates.
# There are several "methods" available for viewing the object
# containing the results of the estimation.
# Above we already mentioned the commands
#	ans
# and
#	summary( ans )
# The command

siena.table( ans )

# will produce in your working directory a table formatted
# for inclusion in a LaTeX document.
# The command

siena.table( ans, type="html" )

# produces a table formatted in html,
# which can be included, e.g., in a Word document.
# See

# ?print.sienaFit

# for further information, e.g., about the use of the xtable package
# for RSiena; if you use xtable, see the set of vignettes for xtable at
# http://cran.r-project.org/web/packages/xtable ,
# which gives more options.


############## MORE ON INITIALIZING PARAMETERS FOR ESTIMATION ########

# If the estimation algorithm has not produced good estimates
# (it 'has not converged well'),
# as will be indicated by some of the t-ratios for convergence being larger
# than 0.1 or the overall maximum convergence ratio being more than 0.25,
# (the precise values of these thresholds may be taken with a gain of salt)
# the best thing to do is continuing the estimation,
# using the estimates produced here,
# and contained in ans, as the new initial values.
# This is done by the option prevAns ('previous ans') as in

ans <- siena07(myalgorithm, data=mydata, effects=myeff, prevAns=ans)

# the parameter estimates in ans then are extracted and
# used in the new estimation;
# moreover, Phase 1 will be omitted from the algorithm,
# as derivatives and covariance matrix are used from the previous run.
# This should be used only if the model specification in myeff
# has not changed, and if the provisional parameter estimates obtained
# in ans are reasonable; if they are not reasonable,
# make a fresh estimation withour the prevAns parameter.

# In Section 6.1.1 of the manual you can read more about the initial
# values used for the estimation algorithm; but this rarely is of any concern.

# If convergence is difficult to obtain, a better algorithm may be created
# by the options

betterAlgorithm <- sienaAlgorithmCreate( projname = 's50_3',
                                         diagonalize = 0.2, doubleAveraging = 0 )

# These options may become the default in future versions of RSiena;
# the impression is that they are preferable.

################################################################################
###
### ---- Testing effects -------------------------------------------------------
###
################################################################################
#
# Three types of tests are available in SIENA.

# 1. t-type tests of single parameters can be carried out by dividing
# the parameter estimate by its standard error.
# Under the null hypothesis that the parameter is 0, these tests have
# approximately a standard normal distribution.

# 2. Score-type tests of single and multiple parameters are described
# in the manual.
# Parameters can be restricted by putting TRUE in the
# include, fix and test columns of the effects object.
# For example, to request a score test for the indegree popularity effect,
# the commands can be as follows.

myeff <- setEffect(myeff, inPopSqrt, fix=TRUE, test=TRUE,
                   initialValue=0.0)
ans <- siena07(myalgorithm, data=mydata, effects=myeff)

# After such an operation, again request

summary(ans)

# to see the results, including those for the score test.

# 3. Wald tests of single and multiple parameters can be obtained by means
# of the functions Wald.RSiena and Multipar.RSiena;
# see the help pages for these functions and also see the manual.


################################################################################
###
### ---- Time test -------------------------------------------------------------
###
################################################################################
#
# An application of the score test is given for the special case of parameter
# heterogeneity by Lospinoso et al. (2010) and implemented in RSiena.
# To apply the test to the results obtained above, request, e.g.,
tt2 <- sienaTimeTest(ans)
tt2
# If you wish more information, also
#       summary(tt2)
#        plot(tt2, effects=3:4)
# If as a consequence of this analysis you wish to add time dummy terms,
# this may be done via
#       myeff <- includeTimeDummy(myeff, transTrip, cycle3)
#       myeff
#       ans3 <- siena07(myalgorithm, data=mydata, effects=myeff)
# and testing again,
#       (tt3 <- sienaTimeTest(ans3))
# and so on.

################################################################################
###
### ---- Summary of model fitted -----------------------------------------------
###
################################################################################

# define original data
friend.data.w1 <- s501
friend.data.w2 <- s502
friend.data.w3 <- s503
#drink <- s50a
#smoke <- s50s
drink <- as.matrix(s50a[,2:4])
smoke <- as.matrix(s50s[,2:4])
# define RSiena data structures
friendship <- sienaDependent( array( c( friend.data.w1,
                                        friend.data.w2, friend.data.w3 ),
                                     dim = c( 50, 50, 3 ) ) )
drinkingbeh <- sienaDependent( drink, type = "behavior" )
smoke1 <- coCovar( smoke[ , 1 ] )
alcohol <- varCovar( drink )
mydata <- sienaDataCreate( friendship, smoke1, alcohol )
# create effects structure
myeff <- getEffects( mydata )
# get initial description
print01Report( mydata, modelname = 's50_3_init' )
# specify model
myeff <- includeEffects( myeff, transTrip, cycle3 )
myeff <- includeEffects( myeff, egoX, altX,
                         egoXaltX, interaction1 = "alcohol" )
myeff <- includeEffects( myeff, simX, interaction1 = "smoke1" )
# estimate
myalgorithm <- sienaAlgorithmCreate( projname = 's50_3' )
(ans <- siena07( myalgorithm, data = mydata, effects = myeff))
# (the outer parentheses lead to printing the obttained result on the screen)S
# if necessary, estimate further
betterAlgorithm <- sienaAlgorithmCreate( projname = 's50_3',
                                         diagonalize = 0.2, doubleAveraging = 0 )
(ans <- siena07( betterAlgorithm,
                 data = mydata, effects = myeff, prevAns=ans))

################################################################################
###
### -- PROCEED TO Rscript04SienaBehaviour.R FOR
###                                 MODELING NETWORKS AND BEHAVIOUR BY RSIENA --
###
################################################################################