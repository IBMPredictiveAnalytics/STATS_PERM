#/***********************************************************************
# * (C) Copyright Jon K Peck, 2024
# ************************************************************************/

# version 1.0.0

# history
# Mar-2025    Initial version



# helpers
gtxt <- function(...) {
    return(gettext(...,domain="STATS_PERM"))
}

gtxtf <- function(...) {
    return(gettextf(...,domain="STATS_PERM"))
}

loadmsg = "The R %s package is required but could not be loaded."
tryCatch(suppressWarnings(suppressPackageStartupMessages(library(permuco, warn.conflicts=FALSE))), error=function(e){
    stop(gtxtf(loadmsg,"permuco"), call.=FALSE)
}
)

mylist2env = function(alist) {
    env = new.env()
    lnames = names(alist)
    for (i in 1:length(alist)) {
        assign(lnames[[i]],value = alist[[i]], envir=env)
    }
    return(env)
}

Warn = function(procname, omsid) {
    # constructor (sort of) for message management
    lcl = list(
        procname=procname,
        omsid=omsid,
        msglist = list(),  # accumulate messages
        msgnum = 0
    )
    # This line is the key to this approach
    lcl = mylist2env(lcl) # makes this list into an environment
    
    lcl$warn = function(msg=NULL, dostop=FALSE, inproc=FALSE) {
        # Accumulate messages and, if dostop or no message, display all
        # messages and end procedure state
        # If dostop, issue a stop.
        
        if (!is.null(msg)) { # accumulate message
            assign("msgnum", lcl$msgnum + 1, envir=lcl)
            # There seems to be no way to update an object, only replace it
            m = lcl$msglist
            m[[lcl$msgnum]] = msg
            assign("msglist", m, envir=lcl)
        } 
        
        if (is.null(msg) || dostop) {
            spssdata.CloseDataConnection()
            lcl$display(inproc)  # display messages and end procedure state

            if (dostop) {
                stop(gtxt("End of procedure"), call.=FALSE)  # may result in dangling error text
            }
        }
    }
    
    lcl$display = function(inproc=FALSE) {
        # display any accumulated messages as a warnings table or as prints
        # and end procedure state, if any
        

        if (lcl$msgnum == 0) {   # nothing to display
            if (inproc) {
                spsspkg.EndProcedure()
                procok = TRUE
            }
        } else {
            procok = inproc
            if (!inproc) {
                procok =tryCatch({
                    spsspkg.StartProcedure(lcl$procname, lcl$omsid)
                    procok = TRUE
                },
                error = function(e) {
                    prockok = FALSE
                }
                )
            }
            if (procok) {  # build and display a Warnings table if we can
                table = spss.BasePivotTable("Warnings and Messages","Warnings", isSplit=FALSE) # do not translate this
                rowdim = BasePivotTable.Append(table,Dimension.Place.row,
                                               gtxt("Message Number"), hideName = FALSE,hideLabels = FALSE)

                for (i in 1:lcl$msgnum) {
                    rowcategory = spss.CellText.String(as.character(i))
                    BasePivotTable.SetCategories(table,rowdim,rowcategory)
                    BasePivotTable.SetCellValue(table,rowcategory,
                        spss.CellText.String(lcl$msglist[[i]]))
                }
                spsspkg.EndProcedure()   # implies display
            } else { # can't produce a table
                for (i in 1:lcl$msgnum) {
                    print(lcl$msglist[[i]])
                }
            }
        }
    }
    return(lcl)
}


casecorrect = function(vlist, vardict, warns) {
    # correct the case of variable names
    # vlist is a list of names, possibly including TO and ALL
    # vardict is a variable dictionary
    # unrecognized names are returned as is as the GetDataFromSPSS api will handle them

    dictnames = vardict["varName",]
    names(dictnames) = tolower(dictnames)
    dictnames['all'] = "all"
    dictnames['to'] = "to"
    correctednames = list()
    for (item in vlist) {
        lcitem = tolower(item)
        itemc = dictnames[[lcitem]]
        if (is.null(itemc)) {
            warns$warn(gtxtf("Invalid variable name: %s", item), dostop=TRUE)
        }
        correctednames = append(correctednames, itemc)
    }
    return(correctednames)
}

procname=gtxt("Perm")
warningsprocname = gtxt("Perm Notes and Warnings")
omsid="STATSPERM"


methods = list(draper_stoneman="draper_stoneman", freedman_lane="freedman_lane", manly="manly",
               terbraak="terBraak", kennedy="kennedy", huh_jhun="huh_jhun", dekker="dekker")

# main worker
doperm<-function(depvar, indvars=NULL, interactionsa=NULL, interactionsb=NULL, interactionsc=NULL, 
    ordera=NULL, orderb=NULL, orderc=NULL, idvar=NULL, resvar=NULL, predvar=NULL,
    np=5000, method="freedman_lane", plotcoef=FALSE
    ) {
    #DEBUG
    #sink(file="c:/temp/permmout.log", type="output")
    #f = file("c:/temp/permmsgs.log", open="w")
    #sink(file=f, type="message")
    
    domain<-"STATS_PERM"
    setuplocalization(domain)
    warns = Warn(procname=warningsprocname,omsid=omsid)
    
    spsspkg.StartProcedure(gtxt("Perm"),"STATS PERM")
    if (!spsspkg.IsUTF8mode()) {
        warns$warn(gtxt("This procedure requires SPSS to be in Unicode mode"), dostop=TRUE)
    }
    if (!any(sapply(list(indvars, interactionsa, interactionsb, interactionsc), function(ff) !is.null(ff))))
        {warns$warn(gtxt("No independent variables were specified"), dostop=TRUE)
    }
    newvars = !is.null(resvar) || !is.null(predvar)
    if (newvars && is.null(idvar)) {
        warns$warn(gtxt("An id variable must be specified if saving new variables"), dostop=TRUE)
    }
    weightvar = spssdictionary.GetWeightVariable()
    if (!is.null(weightvar)) {
        warns$warn(gtxt("Case weights are ignored by this procedure"), dostop=FALSE)
    }
    method = methods[[method]]  # case correction

    vardict = spssdictionary.GetDictionaryFromSPSS()

    if (is.null(idvar) && newvars) {
        warns$warn(gtxt("An ID variable must be specified if predictions or residuals are requested"), dostop=TRUE)
    }

    depvar = unlist(casecorrect(list(depvar), vardict, warns))
    indvars = casecorrect(indvars, vardict, warns)
    if (!is.null(interactionsa)) {interactionsa = casecorrect(interactionsa, vardict, warns)}
    if (!is.null(interactionsb)) {interactionsb = casecorrect(interactionsb, vardict, warns)}
    if (!is.null(interactionsc)) {interactionsc = casecorrect(interactionsc, vardict, warns)}
    allind = unique(c(indvars, interactionsa, interactionsb, interactionsc))

    if ((!is.null(resvar) && resvar %in% vardict[1,]) || (!is.null(predvar) && predvar %in% vardict[1,])) {
        warns$warn(gtxt("Residual or prediction variable name already exists in active dataset"), dostop=TRUE)
    }
        datasetlist = spssdata.GetDataSetList()
        if ("*" %in% datasetlist ) {
            warns$warn(gtxt("The input dataset must have a name if creating new variables.  Please assign one and rerun the procedure."),
                       dostop=TRUE)
        }
        # todo: allow splits if not saving new variables
        nsplitvars = length(spssdata.GetSplitVariableNames())
        if (nsplitvars > 0 && newvars) {
            warns$warn(gtxt("Split files is not supported by this procedure"), dostop=TRUE)
        }

        if (length(intersect(depvar, allind)) > 0) {
            warns$warn(gtxt("The dependent variable appears in the independent variable list"),
                dostop=TRUE)
        }
        indvarsplus = paste(indvars, collapse="+")
        allvars = c(depvar, allind)
        # check for valid names in R
        tryCatch(
            {
            for (ch in allvars) {
                xxx = str2lang(ch)
            }
            },
            error = function(e) {warns$warn(gtxtf("%s is not a valid R variable name.  Please rename it", ch),
                dostop=TRUE)}
        )
        
        f = paste(depvar, indvarsplus, sep="~", collapse="")

        # get data api requires case match
        tryCatch(
            {
            if (!is.null(idvar)) {
                idvar = unlist(casecorrect(list(idvar), vardict, warns))
                dta = spssdata.GetDataFromSPSS(allvars, missingValueToNA=TRUE, factorMode="levels",
                keepUserMissing=FALSE, row.label=idvar)
            } else {
                dta = spssdata.GetDataFromSPSS(allvars, missingValueToNA=TRUE, factorMode="levels",
                                               keepUserMissing=FALSE)
            }
            },
            error=function(e) {warns$warn(paste(gtxt("error fetching data"), e, sep="\n"), dostop=TRUE)}
        )

        dta = dta[complete.cases(dta),]
        gc()
        ncases = nrow(dta)
        if (ncases == 0) {
            warns$warn(gtxt("There are no complete cases in the data"), dostop=TRUE)
        }
        f = makeformula(depvar, indvars, interactionsa, interactionsb, interactionsc,
            ordera, orderb, orderc, warns)
        if (is.null(f)) {
            warns$warn(gtxt("Interaction terms must specify at least two values"), dostop=TRUE)
        }
    tryCatch(
        {
        res = lmperm(f, data=dta, np=np, method=method)
        }, 
        error = function(e) {warns$warn(paste(gtxt("error estimating equation"), e, sep="\n"), dostop=TRUE)},
        warning = function(w) {warns$warn(paste(gtxt("error estimating equation"), w, sep="\n"), dostop=TRUE)}
    )

    displaytables(res, dta[[depvar]], depvar, indvars, idvar, ncases, np, method, vardict, warns)
    if (plotcoef) {
        tryCatch(
            {plot(res, lwd=2)
            },
        error = function(e) {warns$warn(e, dostop=FALSE)}
        )
    }
        
    # res, dta, vardict, idvar,     resvar, predvar, warns
    spsspkg.StartProcedure(gtxt("Perm"),"STATS PERM")
    if (newvars) {
            createvariables(res, dta, vardict, idvar, resvar, predvar, warns)
        }

    # DEBUG
    #sink(file=NULL, type="output")
    #sink(file=NULL, type="message")
    warns$display(inproc=FALSE)
}


makeformula = function(depvar, indvars, interactionsa, interactionsb, interactionsc,
        ordera, orderb, orderc, warns) {
    # make a formula with up to three sets of independent variabler
    f = paste(depvar, "~")
    
    if (!is.null(indvars)) {
        ivs = paste(indvars, collapse="+")
        f = paste(f, ivs)
    }
    if (!is.null(interactionsa)) {
        f = makehelper(interactionsa, ordera, f, warns)
    }
    if (!is.null(interactionsb)) {
        f = makehelper(interactionsb, orderb, f, warns)
    }    
    if (!is.null(interactionsc)) {
        f = makehelper(interactionsc, orderc, f, warns)
    }    
    
    ###print(f)
    return(as.formula(f))
}

makehelper = function(interactions, orderx, f, warns) {
    # make interaction term expression of order ordferx and add to formula
    if (length(interactions) < 2) {
        warns$warn(gtxt("An interaction list must have at least two variables"), dostop=TRUE)
    }
    interactions = paste(interactions, collapse="+")
    if (is.null(orderx)) {orderx=99}
    inter = sprintf("(%s)^%s", interactions, orderx)
    # if (substr(f, nchar(f), nchar(f)) != "~") {
    #     print(f)
    #     f = paste0(f, "+")
    # }
    f = paste(f, inter, sep="+")
    # the formula processor doesn't mind a superfluous + on the right hand side
    return(f)
}


# table column headings
theads = list(gtxt("Estimate"), gtxt("Std. Error"), gtxt("t Value"), gtxt("Parametric Pr(>|t|)"),
        gtxt("Resampled Pr(>|t|)"), gtxt("Resampled Pr(<t)"), gtxt("Resampled Pr(>t)")
)

displaytables = function(res, depvarvalues, depvar, indvars, idvar, ncases, np, method, vardict, warns) {
    # display all the tables

     if (length(res$xlevels) > 0) {
         # make a data frame with row labels the variable names and cells the concatenated levels
         z = res$xlevels
         
         zz = data.frame(sapply(z, function(ff) {paste(ff, collapse=", ")}))
         colnames(zz) = gtxt("Levels")
         
        spsspivottable.Display(
            zz,
            title=gtxt("Categorical Variable Levels"),
            outline=gtxt("Categorical Variable Levels"),
            templateName= "STATSPERMLEVELS",
            caption=gtxtf("Contrasts: %s", "Treatment")
         )
     }
    
    pack = gtxtf("Results computed by permuco version %s", packageVersion("permuco"))
    ssr = sum(res$residuals^2)
    sstot = sum((depvarvalues - mean(depvarvalues))^2)
    rsq = round(1 - ssr/sstot, 3)
    tbl = res$table[c(1,2,3,4,7,5,6)]
    colnames(tbl) = theads
    spsspivottable.Display(
        tbl, 
        title=gtxt("Marginal Coefficient t Tests"),
        outline=gtxt("Permutation Tests"),
        templateName="STATSPERMTESTS",
        caption=sprintf(gtxtf("Dependent variable: %s\nNumber of Permutations: %s\nPermutation Method: %s\nR-Squared: %s\n%s", 
            depvar, np, method,  rsq, pack))
    )
}

createvariables = function(res, dta, vardict, idvar, 
    resvar, predvar, warns) {
    # add output variables to active dataset
    
    if (length(c(resvar, predvar)) == 2 && tolower(resvar) == tolower(predvar)) {
        warns$warn(gtxt("The same name was given for residuals and predicted values"), dostop=TRUE)
    }
    activevars = tolower(vardict['varName',])
    if ((!is.null(resvar) && tolower(resvar) %in% activevars) || 
         (!is.null(predvar) && tolower(predvar) %in% activevars)) {
        warns$warn(gtxt("The residual or prediction variable already exists."), dostop=TRUE)
    }
    dictlist = list()
    # copy id variable properties
    dictlist[[1]] = vardict[, vardict['varName',] == idvar]
    preds = data.frame(row.names(dta))
    if (!is.null(resvar)) {
        dictlist[[2]] = c(resvar, "Residuals", 0, "F8.2", "scale")
        preds[[2]] = res$residuals
    }
    if (!is.null(predvar)) {
        dictlist[[length(dictlist)+1]] = c(predvar, "Predicted Values", 0, "F8.2", "scale")
        preds[predvar] = res$fitted.values
    }
    spsspkg.EndProcedure()
    newdict = spssdictionary.CreateSPSSDictionary(dictlist)
    csvtospss(paste("tospss", runif(1,0.05,1), sep=""), newdict, preds, idvar)
    spsspkg.StartProcedure(gtxt("Perm"),"STATS PERM")
    warns$display(inproc=TRUE)
}


csvtospss = function(preddataset, dict, preds, idvar) {
    # save a temporary csv file and read into SPSS
    # preddataset is the datgaset name for the prediction data
    # dict is the spss dictionary object for the prediction data
    # preds is the data
    
    # due to locale and encoding issues, we can't use a simple Submit
    # to do the Submit, so a temporary file with forced
    # encoding setting and INSERT is used
    
    csvfile = tempfile("csvpred", tmpdir=tempdir(), fileext=".csv")
    write.csv(preds, file=csvfile, row.names=FALSE)
    spsscmd = sprintf('
* Encoding: UTF-8.
        PRESERVE.
        SET DECIMAL DOT.
        GET DATA  /TYPE=TXT
        /FILE="%s"
        /ENCODING="UTF8"
        /DELCASE=LINE
        /DELIMITERS=","
        /QUALIFIER=""""
        /ARRANGEMENT=DELIMITED
        /FIRSTCASE=2
        /VARIABLES=', csvfile)
    
    varspecs = list()
    for (v in 1:ncol(dict)) {
        if (!strsplit(dict[["varFormat", v]], "\\d+") %in% c('A', 'F')) {
            dict[["varFormat", v]] = "F"
        }
    }
    for (v in 1:ncol(dict)) {
        varspecs = append(varspecs, paste(dict[["varName", v]], dict[["varFormat", v]], sep=" "))
    }
    varspecs = paste(varspecs, collapse="\n")
    activedataset = getactivedsname()
    cmd = paste(spsscmd, varspecs, ".\n", sprintf("dataset name %s.", preddataset), collapse="\n")
    syntemp = tempfile("csvsyn", tmpdir=tempdir(), fileext=".sps")
    writeLines(cmd, con=syntemp, useBytes=TRUE)
    spsspkg.Submit(sprintf("INSERT FILE='%s' ENCODING='UTF8'", syntemp))
    spsspkg.Submit("RESTORE.")
    spsspkg.Submit(sprintf("DATASET ACTIVATE %s.", activedataset))
    spsspkg.Submit("EXECUTE")
    spsspkg.Submit(sprintf("UPDATE /FILE=* /FILE=%s BY %s.", preddataset, idvar))
    spsspkg.Submit(sprintf("DATASET CLOSE %s", preddataset))
    unlink(csvfile)
    unlink(syntemp)
}
# subpunct approximates characters invalid in SPSS variable names
subpunct = "[-’‘%&'()*+,/:;<=>?\\^`{|}~’]"
fixnames = function(names) {
    # return list of legal, nonduplicative SPSS variable names for the input list
    
    # dta is a list/vector of names to correct
    # this function may not perfectly match SPSS name rules
    
    newnames = c()
    newnameslc = c()
    for (name in names) {
        newname = gsub(subpunct, "_", name)   # eliminate disallowed characters
        newname = gsub("(^[0-9])", "X_\\1", newname)  # fix names starting with digit
        newname = gsub("^\\.|\\.$", "_", newname)  # fix names starting or ending with "."
        # }
        # ensure that there are no duplicate names
        # preserve case but compare caselessly
        basename = newname
        for (i in 1:1000) {
            newnamelc = tolower(newname)
            if (!newnamelc %in% newnameslc) {
                break
            } else {
                newname = paste(basename, i, sep="_")
                newnamelc = tolower(newname)
            }
        }
        newnames = append(newnames, newname)
        newnameslc = append(newnameslc, newnamelc)
    }

    return(newnames)
}

getactivedsname = function() {
    # There is no api for this

    ds = spssdata.GetOpenedDataSetList()
    spsspkg.Submit("DATASET NAME X44074_60093_")  # renames active dataset
    ds2 = spssdata.GetOpenedDataSetList()
    diff = setdiff(ds, ds2)  # find out which one changed
    spsspkg.Submit("DATASET ACTIVATE X44074_60093_")  # reactivate the previously active one
    cmd = sprintf("DATASET NAME %s", diff)   # and give it back its name
    spsspkg.Submit(cmd)
    return(diff)
}

setuplocalization = function(domain) {
    # find and bind translation file names
    # domain is the root name of the extension command .R file, e.g., "SPSSINC_BREUSCH_PAGAN"
    # This would be bound to root location/SPSSINC_BREUSCH_PAGAN/lang

    fpath = Find(file.exists, file.path(.libPaths(), paste(domain, ".R", sep="")))
    if (!is.null(fpath)) {
        bindtextdomain(domain, file.path(dirname(fpath), domain, "lang"))
    }
} 


Run<-function(args){

    cmdname = args[[1]]
    args <- args[[2]]

    # variable keywords are typed as varname instead of existingvarlist in
    # order to allow for case correction of names later, since the data fetching apis are
    # case sensitive

    oobj <- spsspkg.Syntax(templ=list(
        spsspkg.Template("DEPVAR", subc="", ktype="varname", var="depvar", islist=FALSE),
        spsspkg.Template("INDVARS", subc="", ktype="varname", var="indvars", islist=TRUE),
        spsspkg.Template("INTERACTIONSA", subc="", ktype="varname", var="interactionsa", islist=TRUE),
        spsspkg.Template("INTERACTIONSB", subc="", ktype="varname", var="interactionsb", islist=TRUE),
        spsspkg.Template("INTERACTIONSC", subc="", ktype="varname", var="interactionsc", islist=TRUE),
        spsspkg.Template("MAXORDERA", subc="", ktype="int", var="ordera", vallist=list(2,99)),
        spsspkg.Template("MAXORDERB", subc="", ktype="int", var="orderb", vallist=list(2,99)),
        spsspkg.Template("MAXORDERC", subc="", ktype="int", var="orderc", vallist=list(2,99)),
        spsspkg.Template("IDVAR", subc="", ktype="varname", var="idvar", islist=FALSE),
        spsspkg.Template("RESVAR", subc="", ktype="varname", var="resvar", islist=FALSE),
        spsspkg.Template("PREDVAR", subc="", ktype="varname", var="predvar", islist=FALSE),
        
        spsspkg.Template("NPERM", subc="OPTIONS", ktype="int", var="np", islist=FALSE, vallist=list(1,500000)),
        spsspkg.Template("METHOD", subc="OPTIONS", ktype="str", var="method", islist=FALSE,
            vallist=list("draper_stoneman", "freedman_lane", "manly", "terbraak", "kennedy",
                "huh_jhun", "dekker")),
        spsspkg.Template("PLOTCOEF", subc="DISPLAY", ktype="bool", var="plotcoef", islist=FALSE)
        ))

    if ("HELP" %in% attr(args,"names"))
        #writeLines(helptext)
        helper(cmdname)
    else {
        res <- spsspkg.processcmd(oobj, args, "doperm")
    }
}


helper = function(cmdname) {
    # find the html help file and display in the default browser
    # cmdname may have blanks that need to be converted to _ to match the file
    
    fn = gsub(" ", "_", cmdname, fixed=TRUE)
    thefile = Find(file.exists, file.path(.libPaths(), fn, "markdown.html"))
    if (is.null(thefile)) {
        print("Help file not found")
    } else {
        browseURL(paste("file://", thefile, sep=""))
    }

    if (exists("spsspkg.helper")) {
        assign("helper", spsspkg.helper)
    }
}
