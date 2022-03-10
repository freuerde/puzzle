#' Table 1 for epidemiological purposes
#'
#' Creates an descriptive table stratified by groups with apropriate tests, if necessary.
#'
#' Creates the table 1 for non paired samples with 2 or more groups.
#' The function calculates the means, standard deviations, medians and IQRs for
#' numerical variables. Furthermore it performs dependent on the number of groups
#' the necessary parametric or non-parametric test. Furthermore, Tests for normality
#' and homogeneity will be performed.
#' In case of categorical variables frequencies and (rowwise or columnwise) proportions
#' will be calculated. In addition to this a chi^2-test or if desired Fisher's
#' exact-test will be performed.
#' @param d Data frame or data table with all variables of interest for
#' table 1 (including the stratification variable).
#' @param strat String which represents the name of the stratification variable
#'        that should be used in the table 1 (also for more than two levels).
#'        In case of overall descriptive statistics use the default (strat=NULL).
#' @param mrgn Sets the margin of the proportions for categorical variables (1=rowwise,
#'        2=columnwise (default)).
#' @param path Path, where the Excel file should be stored (requires ".xlxs" at the end).
#'        If NULL, no Excel-file will be generated.
#' @param sheetName String for a user defined sheet name in the Excel file, where the results should saved.
#' If NULL (the default), the sheet will be named automatically after the stratification variable.
#' @param mode Handle the tables of numerical variables. In case of "auto" the tests
#' and apropriate p-values will be choosen automatically.
#' @param cat_freq Handle the tables of categorical variables. "abs" reports only absolute,
#' "rel" only the proportions and "all" both statistics.
#' @param exact Controls, wether the Fisher's exact test should be used instead of the Chi^2 test.
#' @param p_values If TRUE, tests will be performed, otherwise not.
#' @param alpha The significance level.
#' @param rnd The number of digits of the decimal place.
#' @param coloured_excel If TRUE and path not NULL, the saved Excel-sheet will be
#' beautified for an better overview.
#' @param cont_cor Controls, wether the continuity correction within the Chi^2 tests shoud be applied.
#' @author Dennis Freuer
#' @return Returns a list: "table1":  the beautified table 1.
#' "tbl_num": all statistics for numerical variables.
#' "tbl_cat": all statistics for categorical variables.
#' "tests_num": all tests used for numerical variables.
#' "tests_cat": all tests used for categorical variables.
#' @import stats ggplot2 xlsx data.table
#' @importFrom car leveneTest
#' @importFrom lubridate is.Date
#' @export
#'
tbl1 <- function(d, strat=NULL, mrgn=2, path=NULL, sheetName=NULL, mode=c("auto","median","mean"),
                 cat_freq=c("all","rel","abs"), exact=c("ifNecessary","never","always"),
                 p_values=TRUE, alpha=0.05, rnd=3, coloured_excel=TRUE, cont_cor=FALSE){ 

  if(!is.null(path)){
    if(substr(path, nchar(path)-4, nchar(path)) != ".xlsx"){
      path <- paste0(path, ".xlsx")
    }
  }

  if(is.null(strat)){
    p_values <- FALSE
    d$st <- as.factor(1)
    strat <- "st"
    mrgn <- 2
    coloured_excel=FALSE

    if(mode[1] == "auto"){
      warning(" \"mode == auto\" isn't allowed for \"strat = NULL\" ")
      return(NULL)
    }
  }

  dt <- data.table(d)
  d <- data.frame(d)
  if((mode[1] == "auto") & (! p_values)){
    p_values <- TRUE
    warning("Set \"p_values = TRUE\" because of \"mode = auto\" ")
  }
  tests_num <- NULL; tests_cat <- NULL


  dt[,strat] <- dt[,strat, factor, with=FALSE]
  dt <- dt[, which(!sapply(dt, is.Date)), with=FALSE]
  if(ncol(d) != ncol(dt)){cat("Date variables will be ignored\n")}
  dt_num <- dt[, .SD, .SDcols = sapply(dt, is.numeric)]
  dt_cat <- dt[, -c(names(dt_num),strat), with=FALSE]
  dt_cat <- dt_cat[, lapply(.SD, as.factor)]
  dt_cat$strt <- d[,strat]
  dt_num$strt <- d[,strat]
  dt_cat <- dt_cat[!is.na(strt),]
  dt_num <- dt_num[!is.na(strt),]


  if(ncol(dt_num) > 0){
    lv <- levels(dt_num$strt)
    dtmp <- dt_num[,-"strt", with=FALSE]
    tbl_num <- data.table(variable = names(dtmp), n=colSums(!is.na(dtmp)))
    vnms <- names(tbl_num)

    if(mode[1] != "median"){
      means <- dt_num[, lapply(.SD, mean, na.rm=TRUE), by=strt]
      sds <- dt_num[, lapply(.SD, sd, na.rm=TRUE), by=strt]

      for(nm in 1:length(lv)){
        vnms <- c(vnms, paste0(c("mu","sd"),nm-1))
        tbl_num <- cbind(tbl_num,
                         as.numeric(means[strt==lv[nm],2:ncol(means)]),
                         as.numeric(sds[strt==lv[nm],2:ncol(means)]))
      }
    }

    if(mode[1] != "mean"){
      medians <- dt_num[, lapply(.SD, median, na.rm=TRUE), by=strt]
      iqrs <- dt_num[, lapply(.SD, quantile, probs=c(0.25,0.75), na.rm=TRUE), by=strt]

      for(nm in 1:length(lv)){
        vnms <- c(vnms, sprintf(paste0(c("med%d","qu%d_1","qu%d_3")),nm-1))
        tbl_num <- cbind(tbl_num,
                         as.numeric(medians[strt==lv[nm],2:ncol(medians)]),
                         as.numeric(iqrs[strt==lv[nm],2:ncol(medians)][1,]),
                         as.numeric(iqrs[strt==lv[nm],2:ncol(medians)][2,]))
      }
    }
    names(tbl_num) <- vnms


    if(p_values){
      if(length(lv)==2){
        tests_num <- apply(dt_num[,-"strt", with=FALSE], MARGIN=2, function(x){
          if( (length(x) < 3) | (length(x) > 5000) ){
            s <- tapply(x, dt_num$strt, ks.test, y="pnorm")
          } else{
            s <- tapply(x, dt_num$strt, shapiro.test)
          }
          l <- leveneTest(x, group=dt_num$strt, center=mean)
          t <- t.test(x~dt_num$strt, var.equal=FALSE)
          t2 <- t.test(x~dt_num$strt, var.equal=TRUE)
          w <- wilcox.test(x~dt_num$strt, conf.int=TRUE)
          list(s,t,t2,w,l)
        })

      } else{
        tests_num <- apply(dt_num[,-"strt", with=FALSE], MARGIN=2, function(x){
          if( (length(x) < 3) | (length(x) > 5000) ){
            s <- tapply(x, dt_num$strt, ks.test, y="pnorm")
          } else{
            s <- tapply(x, dt_num$strt, shapiro.test)
          }
          l <- leveneTest(x, group=dt_num$strt, center=mean)
          t <- oneway.test(x~dt_num$strt)
          t2 <- anova(lm(x~dt_num$strt))
          k <- kruskal.test(x, dt_num$strt)
          list(s,t,t2,k,l)
        })
      }

      for(i in 1:length(tests_num)){
        for(j in 1:(length(tests_num[[i]])-1)){
          if(j==1){
            for(k in 1:length(tests_num[[i]][[j]])){
              tests_num[[i]][[j]][[k]]$data.name <- paste(names(tests_num)[i], "for", strat,
                                                          "=", names(tests_num[[i]][[j]])[k])
            }
          } else{
            if((j==3) & (length(lv)>2)) { next }
            tests_num[[i]][[j]]$data.name <- paste(names(tests_num)[i], "by", strat)
          }
        }
      }

      pshapiro = as.numeric(unlist(lapply(tests_num, function(l){
        lapply(l[[1]], function(x){
          x$p.value
        })
      })))
      shapiro_df <- data.frame(matrix(pshapiro, ncol=length(lv), byrow=TRUE))
      names(shapiro_df) <- paste0("pshapiro_",lv)

      if(length(lv)==2){
        tbl_num <- cbind(tbl_num, pttest_diffVar = as.numeric(lapply(tests_num, function(l){ l[[2]]$p.value })),
                         pttest_eqVar = as.numeric(lapply(tests_num, function(l){ l[[3]]$p.value })),
                         pwilcox = as.numeric(lapply(tests_num, function(l){ l[[4]]$p.value })),
                         shapiro_df,
                         plevene = as.numeric(lapply(tests_num, function(l){ l[[5]]$`Pr(>F)`[1] })))

      } else {
        tbl_num <- cbind(tbl_num, panova_diffVar = as.numeric(lapply(tests_num, function(l){ l[[2]]$p.value })),
                         panova_eqVar = as.numeric(lapply(tests_num, function(l){ l[[3]]$`Pr(>F)`[1] })),
                         pkruskal = as.numeric(lapply(tests_num, function(l){ l[[4]]$p.value })),
                         shapiro_df,
                         plevene = as.numeric(lapply(tests_num, function(l){ l[[5]]$`Pr(>F)`[1] })))
      }
    }
  }


  if(ncol(dt_cat) > 0){

    tests_cat <- lapply(dt_cat[, -"strt", with=FALSE], function(x){
      rel_t <- NULL; tt <- NULL
      t <- table(x,dt_cat$strt, useNA="no")

      if(cat_freq[1] != "abs"){
        rel_t <- prop.table(t,margin=mrgn)
      }

      if(p_values){
        if(exact[1]=="always"){
          tt <- fisher.test(t, workspace=2e+07)
        }else if(exact[1]=="never"){
          tt <- chisq.test(t, correct=cont_cor)
        }else if(exact[1]=="ifNecessary"){
          if(any(t <= 5)){
            tt <- fisher.test(t, workspace=2e+07)
          }else{
            tt <- chisq.test(correct=cont_cor)
          }
        }
      }

      list(t, rel_t, tt)
    })


    if(! is.null(tests_cat[[1]][[3]])){
      for(i in 1:length(tests_cat)){
        tests_cat[[i]][[3]]$data.name <- paste(names(tests_cat)[i], "vs.", strat)
      }
    }

    if(cat_freq[1] != "rel"){
      tbls <- lapply(tests_cat, function(l){ as.data.frame.matrix(l[[1]]) })
      tbls_freq <- rbindlist(tbls)
      setnames(tbls_freq, paste0("freq_",names(tbls_freq)) )
    }

    if(cat_freq[1] != "abs"){
      tbls <- lapply(tests_cat, function(l){ as.data.frame.matrix(l[[2]]) })
      tbls_prop <- rbindlist(tbls)
      if(mrgn==1){
        setnames(tbls_prop, paste0("prop_rows_",names(tbls_prop)))
      } else{
        setnames(tbls_prop, paste0("prop_cols_",names(tbls_prop)))
      }
    }

    dtmp <- dt_cat[,-"strt", with=FALSE]
    lvls <- lapply(dtmp, levels)
    lvls_length <- lapply(lvls, length)
    lvls <- as.character(unlist(lvls))

    tbl_cat <- data.table(variable = paste0( rep(names(dtmp),lvls_length),"_",  lvls ),
                          n = rep( colSums(!is.na(dtmp)),lvls_length ))
    if(cat_freq[1] != "rel"){
      tbl_cat <- cbind(tbl_cat, tbls_freq)
    }
    if(cat_freq[1] != "abs"){
      tbl_cat <- cbind(tbl_cat, tbls_prop)
    }

    if(p_values){
      pvls <- as.numeric(lapply(tests_cat, function(l){ l[[3]]$p.value }))
      tbl_cat[,p_value := rep(pvls, lvls_length)][,
                                                  used_test := rep( as.character(unlist(lapply(tests_cat,
                                                                                               function(l){l[[3]]$method}))),lvls_length )]
    }
  }


  it <-  table(d[,strat], useNA="no")
  lvls <- names(it)
  numb <- as.numeric(it)

  if(p_values){
    clmns <- c("used_p", "used_test")

    if(ncol(dt_num) > 0){

      # Median:
      if(mode[1] == "median"){
        if(length(lv)==2){
          tbl_num[, (clmns) := list(pwilcox,"Wilcoxon Rank Sum Test")]
        } else {
          tbl_num[, (clmns) := list(pkruskal,"Kruskal-Wallis Rank Sum Test")]
        }

      } else if(mode[1] == "mean"){
        # Mean:
        tbl_num$pshapiro_min <- apply(shapiro_df, 1, min, na.rm=TRUE)

        if(length(lv)==2){
          tbl_num[plevene < alpha, (clmns) := list(pttest_diffVar,"Welch's t-test")][
            plevene >= alpha, (clmns) := list(pttest_eqVar, "t-test")]
        } else{
          tbl_num[plevene < alpha, (clmns) := list(panova_diffVar,"Welch's One-Way ANOVA")][
            plevene >= alpha, (clmns) := list(panova_eqVar, "One-Way ANOVA")]
        }

      } else{
        tbl_num$pshapiro_min <- apply(shapiro_df, 1, min, na.rm=TRUE)

        if(length(lv)==2){
          tbl_num[pshapiro_min < alpha, (clmns) := list(pwilcox,"Wilcoxon Rank Sum test")][
            (pshapiro_min >= alpha & plevene < alpha), (clmns) := list(pttest_diffVar,"Welch Test")][
              (pshapiro_min >= alpha & plevene >= alpha), (clmns) := list(pttest_eqVar,"t-Test")]

        } else{
          tbl_num[pshapiro_min < alpha, (clmns) := list(pkruskal,"Kruskal-Wallis Rank Sum Test")][
            (pshapiro_min >= alpha & plevene < alpha), (clmns) := list(panova_diffVar,"Welch's One-Way ANOVA")][
              (pshapiro_min >= alpha & plevene >= alpha), (clmns) := list(panova_eqVar,"One-Way ANOVA")]
        }
      }
    }
  }


  nms <- c("characteristics", "n", strat, rep(".",(length(lvls)-1)))
  df <- matrix(nrow=2,ncol=2+length(lvls),dimnames=list(rep(NULL,2),nms))
  df[1,2:(2+length(lvls))] <- c("total",lvls)
  df[2,2:(2+length(lvls))] <- c(sum(numb), paste0("n=",numb))


  cols <- names(which(sapply(tbl_num, is.numeric)))
  tbl_num[,(cols) := round(.SD,rnd), .SDcols=cols]
  dftmp <- cbind(tbl_num$variable, tbl_num$n)
  dftmp_med <- dftmp; dftmp_mean <- dftmp


  if(mode[1] != "mean"){
    for(nm in 1:length(lv)){
      tmp <- tbl_num[, sprintf(c("med%d","qu%d_1","qu%d_3"),nm-1), with=FALSE]
      a <- apply(tmp, 1, function(x){
        paste0(x[1]," (",x[2],"; ",x[3],")")
      })
      dftmp_med <- cbind(dftmp_med, a)
    }
  }

  if(mode[1] != "median"){
    for(nm in 1:length(lv)){
      tmp <- tbl_num[, sprintf(c("mu%d","sd%d"),nm-1), with=FALSE]
      a <- apply(tmp, 1, function(x){
        paste0(x[1]," (", x[2], ")")
      })
      dftmp_mean <- cbind(dftmp_mean, a)
    }
  }

  if(mode[1] == "median"){
    dftmp <- dftmp_med
  } else{
    dftmp <- dftmp_mean
  }

  if(! p_values){
    dimnames(dftmp)[[2]] <- dimnames(df)[[2]]
    df <- rbind(df, dftmp)
  } else{
    df <- cbind(df, matrix(nrow=nrow(df), ncol=2))
    dimnames(df)[[2]] <- c(nms,"p_value","test")

    dftmp <- cbind(dftmp, cbind(tbl_num$used_p, tbl_num$used_test))
    df <- rbind(df, dftmp)

    if(mode[1] == "auto"){
      ind <- which(tbl_num$used_test %in% c("Wilcoxon Rank Sum test",
                                            "Kruskal-Wallis Rank Sum Test"))
      df[ind+2,3:ncol(dftmp_med)] <- dftmp_med[ind,3:ncol(dftmp_med)]
    }
  }
  df[is.na(df)] <- ""


  cols <- names(which(sapply(tbl_cat, is.numeric)))
  tbl_cat[,(cols) := round(.SD,rnd), .SDcols=cols]
  v <- names(tests_cat)
  k <- 0

  for(i in 1:length(v)){
    dims <- dim(tests_cat[[i]][[1]])

    if(p_values){
      df <- rbind(df, c(v[i], tbl_cat$n[k+1], rep("",dims[2]),
                        tbl_cat$p_value[k+1], tbl_cat$used_test[k+1]))
    } else{
      df <- rbind(df, c(v[i], tbl_cat$n[k+1], rep("",dims[2])))
    }

    lvl <- tbl_cat$variable[(k+1):(k+dims[1])]

    if(cat_freq[1] == "abs"){
      fp <- as.matrix(round(tests_cat[[i]][[1]],rnd))
    } else if(cat_freq[1] == "rel"){
      fp <- as.matrix(round(tests_cat[[i]][[2]],rnd))
    } else{
      fp <- paste0(tests_cat[[i]][[1]], " (",
                   round(tests_cat[[i]][[2]],rnd), ")")
      fp <- matrix(fp,nrow=dims[1])
    }

    fp <- cbind(lvl,"",fp, matrix("",nrow=nrow(fp), ncol=ncol(df)-ncol(fp)-2))
    df <- rbind(df,fp)
    k <- k + dims[1]
  }


  if(! is.null(path)){
    row.names(df) <- NULL
    sn <- ifelse(is.null(sheetName), strat, sheetName)

    write.xlsx(df, file=path, sheetName=sn, append=TRUE, row.names=FALSE)

    if(coloured_excel){
      wb<-loadWorkbook(path)
      sheetObj <- getSheets(wb)[[sn]]
      rowObj <- getRows(sheetObj)
      rowObj_h <- getRows(sheetObj, rowIndex=1)
      rws <- nrow(df)+1
      cls <- ncol(df)

      st <- CellStyle(wb) + Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER")
      for(i in 2:(cls-1)){
        cellObj <- getCells(rowObj, colIndex=i)
        l <- lapply(cellObj, setCellStyle, st)
      }

      l <- lapply(getCells(rowObj_h), function(x){
        setCellStyle(x, st)
        a <- getCellValue(x)
        if((a == ".") | (substr(a,1,2) == "..")){
          setCellValue(x,"")
        }
      })

      if(p_values){
        cellObj <- getCells(rowObj, colIndex = which(colnames(df) == "p_value"))
        st <- st + Font(wb, color=10)

        l <- lapply(cellObj, function(x){
          a <- getCellValue(x)
          if(a <= 0.05){
            setCellStyle(x, st)
          }
        })
      }

      cb <- CellBlock(sheetObj, startRow=1, startColumn=1, noRows=3,
                      noColumns=cls, create=FALSE)
      border <-  Border(color="black", position="BOTTOM",
                        pen="BORDER_THIN")
      CB.setBorder(cb, border, rowIndex=3, colIndex=1:cls)

      font <- Font(wb, heightInPoints=12, isBold=TRUE)
      fill <- Fill(foregroundColor = "lightblue")

      for(i in 1:3){
        CB.setFont(cb, font, rowIndex=i, colIndex=1:cls)
        CB.setFill(cb, fill, rowIndex=i, colIndex=1:cls)
      }

      cb <- CellBlock(sheetObj, startRow=1, startColumn=1, noRows=rws,
                      noColumns=cls, create=FALSE)
      fill1 <- Fill(foregroundColor = "white")
      fill2 <- Fill(foregroundColor = "grey90")
      flag <- (-1)
      for(i in 4:rws){
        if(df[i-1,2] != ""){flag <- flag*(-1)}
        if(flag==1){
          CB.setFill(cb, fill1, rowIndex=i, colIndex=1:cls)
        } else{
          CB.setFill(cb, fill2, rowIndex=i, colIndex=1:cls)
        }
      }
      autoSizeColumn(sheetObj, colIndex=1:ncol(df))
      saveWorkbook(wb,path)
    }
  }

  if(p_values){
    if(tests_num[[1]][[1]][[1]]$method == "One-sample Kolmogorov-Smirnov test"){
      names(tbl_num) <- gsub("shapiro", "kolmogorov", names(tbl_num))
    }
  }

  tbls <- list(df, tbl_num, tbl_cat, tests_num, tests_cat)
  names(tbls) <- c("table1","tbl_num","tbl_cat","tests_num","tests_cat")

  return(tbls)
}


#' @describeIn tbl1 Applies tbl1() to multiple stratification variables
#' @param strats Vector of stratification variables (analogously to strat of tbl1() )
tbls1 <- function(d, strats, path=NULL, mode=c("auto","median","mean"),
                  exact=c("ifNecessary","never","always"), cat_freq=c("all","rel","abs"),
                  p_values=TRUE, coloured_excel=TRUE, mrgn=2, rnd=3, alpha=0.05, cont_cor=FALSE){
  d <- data.frame(d)
  tbls <- list()
  for(strt in strats){
    dft <- d[, !(names(d) %in% strats)]
    dft <- cbind(dft,d[,strt])
    names(dft) <- c(names(dft)[-length(dft)],strt)
    print(paste("Creating table 1 for",strt))
    tbls[[strt]] <- tbl1(dft, strat=strt, path=path, mode=mode, mrgn=mrgn,
                         exact=exact, cat_freq=cat_freq, p_values=p_values,
                         coloured_excel=coloured_excel,rnd=rnd, alpha=alpha)
  }
  return(tbls)
}
