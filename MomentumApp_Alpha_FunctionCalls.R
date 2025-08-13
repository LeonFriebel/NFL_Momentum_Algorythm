#1 Name-------------------------------------------------------------------------
#Leon Friebel
#Initiation: 07/01/2024
#Last edit: 07/19/2024

#libraries
#2 librarys---------------------------------------------------------------------
library(ggplot2)
library(forcats)
library(dplyr)
library(tidyr)
library(GGally)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)
library(DT)

setwd("~/Downloads/Sidehustle/Momentum/NFL Data/playbyplay")
#Historic NFL Functions---------------------------------------------------------
mAway.result <- function(df, i) {
  r <- 0
  
  # Check if posteam_type is NA
  if (is.na(df$posteam_type[i])) {
    return(NA)
  }
  
  if (df$posteam_type[i] == "away") {
    # Check if fixed_drive or fixed_drive_result is NA
    if (is.na(df$fixed_drive[i]) || is.na(df$fixed_drive[i+1]) || is.na(df$fixed_drive_result[i])) {
      return(NA)
    }
    
    if (df$fixed_drive[i] != df$fixed_drive[i+1]) {
      if (df$fixed_drive_result[i] == "Touchdown") {
        r <- 4.82
      } else if (df$fixed_drive_result[i] == "Field goal") {
        r <- 4.29
      } else if (df$fixed_drive_result[i] == "Missed field goal") {
        r <- -3.93
      } else if (df$fixed_drive_result[i] == "Turnover") {
        r <- -4.46
      } else if (df$fixed_drive_result[i] == "Punt") {
        r <- -3.39
      } else if (df$fixed_drive_result[i] == "Opp touchdown") {
        r <- -4.82
      } else if (df$fixed_drive_result[i] == "Turnover on downs") {
        r <- -3.57
      } else if (df$fixed_drive_result[i] == "End of half") {
        r <- 0
      } else if (df$fixed_drive_result[i] == "Safety") {
        r <- -4.1
      } else if (df$fixed_drive_result[i] == "") {
        r <- 0
      }
    }
    
  } else if (df$posteam_type[i] == "home") {
    # Check if fixed_drive or fixed_drive_result is NA
    if (is.na(df$fixed_drive[i]) || is.na(df$fixed_drive[i+1]) || is.na(df$fixed_drive_result[i])) {
      return(NA)
    }
    
    if (df$fixed_drive[i] != df$fixed_drive[i+1]) {
      if (df$fixed_drive_result[i] == "Touchdown") {
        r <- -4.82
      } else if (df$fixed_drive_result[i] == "Field goal") {
        r <- -4.29
      } else if (df$fixed_drive_result[i] == "Missed field goal") {
        r <- 3.93
      } else if (df$fixed_drive_result[i] == "Turnover") {
        r <- 4.46
      } else if (df$fixed_drive_result[i] == "Punt") {
        r <- 3.39
      } else if (df$fixed_drive_result[i] == "Opp touchdown") {
        r <- 4.82
      } else if (df$fixed_drive_result[i] == "Turnover on downs") {
        r <- 3.57
      } else if (df$fixed_drive_result[i] == "End of half") {
        r <- 0
      } else if (df$fixed_drive_result[i] == "Safety") {
        r <- 4.1
      } else if (df$fixed_drive_result[i] == "") {
        r <- 0
      }
    }
  } else {
    r <- 0
  }
  
  return(r)
}


mAway.yard <- function(df, i) {
  y <- 0
  if (is.na(df$posteam_type[i])) {
    return(NA)
  }
  if (df$posteam_type[i] == "away") {
    if (df$play_type[i] == "pass") {
      if (!is.na(df$yards_gained[i]) &&
          df$yards_gained[i] >= 17) {
        y <- 3.04
      } else if (!is.na(df$yards_gained[i]) &&
                 10 <= df$yards_gained[i] &&
                 df$yards_gained[i] < 17) {
        y <- 0.89
      } else if (!is.na(df$yards_gained[i]) &&
                 1 <= df$yards_gained[i] &&
                 df$yards_gained[i] < 10) {
        y <- 0.54
      } else if (!is.na(df$yards_gained[i]) &&
                 df$yards_gained[i] < 0) {
        y <- -0.71
      } else if (is.na(df$yards_gained[i]) ||
                 df$yards_gained[i] == 0){
        y <- -0.18
      }
    } else if (df$play_type[i] == "run") {
      if (!is.na(df$yards_gained[i]) &&
          df$yards_gained[i] >= 12) {
        y <- 3.04
      } else if (!is.na(df$yards_gained[i]) &&
                 4 <= df$yards_gained[i] && df$yards_gained[i] < 12) {
        y <- 0.89
      } else if (!is.na(df$yards_gained[i]) &&
                 1 <= df$yards_gained[i] &&
                 df$yards_gained[i] < 4) {
        y <- 0.54
      } else if (!is.na(df$yards_gained[i]) &&
                 df$yards_gained[i] < 0) {
        y <- -0.71
      } else if (is.na(df$yards_gained[i]) ||
                 df$yards_gained[i] == 0) {
        y <- -0.18
      }
    }
  } else if (df$posteam_type[i] == "home") {
    if (df$play_type[i] == "pass") {
      if (!is.na(df$yards_gained[i]) &&
          df$yards_gained[i] >= 17) {
        y <- -3.04
      } else if (!is.na(df$yards_gained[i]) &&
                 10 <= df$yards_gained[i] &&
                 df$yards_gained[i] < 17) {
        y <- -0.89
      } else if (!is.na(df$yards_gained[i]) &&
                 1 <= df$yards_gained[i] &&
                 df$yards_gained[i] < 10) {
        y <- -0.54
      } else if (!is.na(df$yards_gained[i]) &&
                 df$yards_gained[i] < 0) {
        y <- 0.71
      } else if (is.na(df$yards_gained[i]) ||
                 df$yards_gained[i] == 0){
        y <- 0.18
      }
    } else if (df$play_type[i] == "run") {
      if (!is.na(df$yards_gained[i]) &&
          df$yards_gained[i] >= 12) {
        y <- -3.04
      } else if (!is.na(df$yards_gained[i]) &&
                 4 <= df$yards_gained[i] && df$yards_gained[i] < 12) {
        y <- -0.89
      } else if (!is.na(df$yards_gained[i]) &&
                 1 <= df$yards_gained[i] &&
                 df$yards_gained[i] < 4) {
        y <- -0.54
      } else if (!is.na(df$yards_gained[i]) &&
                 df$yards_gained[i] < 0) {
        y <- 0.71
      } else if (is.na(df$yards_gained[i]) ||
                 df$yards_gained[i] == 0) {
        y <- 0.18
      }
    }
  }else {
    y = 0
  }
  
  return(y)
}

mAway.score <- function(df, i) {
  score_difference <- df$total_away_score[i] - df$total_home_score[i]
  
  # Check if score_difference is NA
  if (is.na(score_difference)) {
    return(NA)
  }
  
  s <- 0 
  
  if (-8 <= score_difference && score_difference < 0) {
    s <- -3.75
  } else if (-16 <= score_difference && score_difference < -8) {
    s <- -4.64
  } else if (0 < score_difference && score_difference <= 8) {
    s <- 3.75
  } else if (8 < score_difference && score_difference <= 16) {
    s <- 4.64
  } else if (score_difference > 16) {
    s <- 5
  } else if (score_difference == 0) {
    s <- 0
  } else if (score_difference < -16) {
    s <- -5
  }
  
  return(s)
}

mAway.DIST <- function(df, i) {
  d <- 0
  
  if (is.na(df$down[i])) {
    return(NA)
  }
  
  if (df$posteam_type[i] == "away") {
    
    if (df$down[i] == 1) {
      if (df$ydstogo[i] <= 5) {
        d = 1.07
      }else if (df$ydstogo[i] > 5 &&
                df$ydstogo[i] <= 10) {
        d = 0
      }else if (df$ydstogo[i] > 10) {
        d = -1.79
      }
      
    }else if (df$down[i] == 2) {
      if (df$ydstogo[i] <=2) {
        d = 1.79
      }else if (df$ydstogo[i] > 2 &&
                df$ydstogo[i] <= 6) {
        d = 0
      }else if (df$ydstogo[i] > 6 &&
                df$ydstogo[i] <= 10) {
        d = 0
      }else if (df$ydstogo[i] < 10) {
        d = -1.61
      }
      
    }else if (df$down[i] == 3) {
      if (df$ydstogo[i] <=2) {
        d = 1.43
      }else if (df$ydstogo[i] > 2 &&
                df$ydstogo[i] <= 6) {
        d = 1.25
      }else if (df$ydstogo[i] > 6 &&
                df$ydstogo[i] <= 10) {
        d = -1.61
      }else if (df$ydstogo[i] < 10) {
        d = -1.96
      }
    }else if (df$down[i] == 4) {
      if (df$ydstogo[i] <=2) {
        d = 1.43
      }else if (df$ydstogo[i] > 2 &&
                df$ydstogo[i] <= 6) {
        d = 1.25
      }else if (df$ydstogo[i] > 6 &&
                df$ydstogo[i] <= 10) {
        d = -1.61
      }else if (df$ydstogo[i] < 10) {
        d = -1.96
      } 
    }
    
  }else if (df$posteam_type[i] == "home") {
    
    if (df$down[i] == 1) {
      if (df$ydstogo[i] <= 5) {
        d = -1.07
      }else if (df$ydstogo[i] > 5 &&
                df$ydstogo[i] <= 10) {
        d = 0
      }else if (df$ydstogo[i] > 10) {
        d = 1.79
      }
      
    }else if (df$down[i] == 2) {
      if (df$ydstogo[i] <=2) {
        d = -1.79
      }else if (df$ydstogo[i] > 2 &&
                df$ydstogo[i] <= 6) {
        d = 0
      }else if (df$ydstogo[i] > 6 &&
                df$ydstogo[i] <= 10) {
        d = 0
      }else if (df$ydstogo[i] < 10) {
        d = 1.61
      }
      
    }else if (df$down[i] == 3) {
      if (df$ydstogo[i] <=2) {
        d = -1.43
      }else if (df$ydstogo[i] > 2 &&
                df$ydstogo[i] <= 6) {
        d = -1.25
      }else if (df$ydstogo[i] > 6 &&
                df$ydstogo[i] <= 10) {
        d = 1.61
      }else if (df$ydstogo[i] < 10) {
        d = 1.96
      }
      
    }else if (df$down[i] == 4) {
      if (df$ydstogo[i] <=2) {
        d = -1.43
      }else if (df$ydstogo[i] > 2 &&
                df$ydstogo[i] <= 6) {
        d = -1.25
      }else if (df$ydstogo[i] > 6 &&
                df$ydstogo[i] <= 10) {
        d = 1.61
      }else if (df$ydstogo[i] < 10) {
        d = 1.96
      }
    }
  }else {
    d = 0
  }
  return(d)
}

Away.momentum <- function(df, i) {
  x <- mAway.result(df, i)
  y <- mAway.yard(df, i)
  s <- mAway.score(df, i)
  d <- mAway.DIST(df, i)
  
  for (j in 1:nrow(df)) {
    if (df$qtr[j] == 1) {
      s <- s * 0.1
    } else if (df$qtr[j] == 2) {
      s <- s * 0.2
    } else if (df$qtr[j] == 3) {
      s <- s * 0.3
    } else if (df$qtr[j] == 4) {
      s <- s * 0.4
    } else if (df$qtr[j] == 5) {
      s <- s * 0.5
    }
  }
  
  m <- x + s + d + y
  return(m)
}

mainAway.momentum <- function(df) {
  df$MomentumAway <- 0  # Initialize Away Momentum Vector
  
  for (i in 2:nrow(df)) {
    df$MomentumAway[i] <- Away.momentum(df, i)
  }
  return(df)
}

mHome.result <- function(df, i) {
  r <- 0
  
  # Check if posteam_type is NA
  if (is.na(df$posteam_type[i])) {
    return(NA)
  }
  
  if (df$posteam_type[i] == "home") {
    # Check if fixed_drive or fixed_drive_result is NA
    if (is.na(df$fixed_drive[i]) || is.na(df$fixed_drive[i+1]) || is.na(df$fixed_drive_result[i])) {
      return(NA)
    }
    
    if (df$fixed_drive[i] != df$fixed_drive[i+1]) {
      if (df$fixed_drive_result[i] == "Touchdown") {
        r <- 4.82
      } else if (df$fixed_drive_result[i] == "Field goal") {
        r <- 4.29
      } else if (df$fixed_drive_result[i] == "Missed field goal") {
        r <- -3.93
      } else if (df$fixed_drive_result[i] == "Turnover") {
        r <- -4.46
      } else if (df$fixed_drive_result[i] == "Punt") {
        r <- -3.39
      } else if (df$fixed_drive_result[i] == "Opp touchdown") {
        r <- -4.82
      } else if (df$fixed_drive_result[i] == "Turnover on downs") {
        r <- -3.57
      } else if (df$fixed_drive_result[i] == "End of half") {
        r <- 0
      } else if (df$fixed_drive_result[i] == "Safety") {
        r <- -4.1
      } else if (df$fixed_drive_result[i] == "") {
        r <- 0
      }
    }
    
  } else if (df$posteam_type[i] == "away") {
    # Check if fixed_drive or fixed_drive_result is NA
    if (is.na(df$fixed_drive[i]) || is.na(df$fixed_drive[i+1]) || is.na(df$fixed_drive_result[i])) {
      return(NA)
    }
    
    if (df$fixed_drive[i] != df$fixed_drive[i+1]) {
      if (df$fixed_drive_result[i] == "Touchdown") {
        r <- -4.82
      } else if (df$fixed_drive_result[i] == "Field goal") {
        r <- -4.29
      } else if (df$fixed_drive_result[i] == "Missed field goal") {
        r <- 3.93
      } else if (df$fixed_drive_result[i] == "Turnover") {
        r <- 4.46
      } else if (df$fixed_drive_result[i] == "Punt") {
        r <- 3.39
      } else if (df$fixed_drive_result[i] == "Opp touchdown") {
        r <- 4.82
      } else if (df$fixed_drive_result[i] == "Turnover on downs") {
        r <- 3.57
      } else if (df$fixed_drive_result[i] == "End of half") {
        r <- 0
      } else if (df$fixed_drive_result[i] == "Safety") {
        r <- 4.1
      } else if (df$fixed_drive_result[i] == "") {
        r <- 0
      }
    }
  }
  
  return(r)
}

mHome.yard <- function(df, i) {
  y <- 0
  if (is.na(df$posteam_type[i])) {
    return(NA)
  }
  if (df$posteam_type[i] == "home") {
    if (df$play_type[i] == "pass") {
      if (!is.na(df$yards_gained[i]) &&
          df$yards_gained[i] >= 17) {
        y <- 3.04
      } else if (!is.na(df$yards_gained[i]) &&
                 10 <= df$yards_gained[i] &&
                 df$yards_gained[i] < 17) {
        y <- 0.89
      } else if (!is.na(df$yards_gained[i]) &&
                 1 <= df$yards_gained[i] &&
                 df$yards_gained[i] < 10) {
        y <- 0.54
      } else if (!is.na(df$yards_gained[i]) &&
                 df$yards_gained[i] < 0) {
        y <- -0.71
      } else if (is.na(df$yards_gained[i]) ||
                 df$yards_gained[i] == 0){
        y <- -0.18
      }
    } else if (df$play_type[i] == "run") {
      if (!is.na(df$yards_gained[i]) &&
          df$yards_gained[i] >= 12) {
        y <- 3.04
      } else if (!is.na(df$yards_gained[i]) &&
                 4 <= df$yards_gained[i] && df$yards_gained[i] < 12) {
        y <- 0.89
      } else if (!is.na(df$yards_gained[i]) &&
                 1 <= df$yards_gained[i] &&
                 df$yards_gained[i] < 4) {
        y <- 0.54
      } else if (!is.na(df$yards_gained[i]) &&
                 df$yards_gained[i] < 0) {
        y <- -0.71
      } else if (is.na(df$yards_gained[i]) ||
                 df$yards_gained[i] == 0) {
        y <- -0.18
      }
    }
  } else if (df$posteam_type[i] == "away") {
    if (df$play_type[i] == "pass") {
      if (!is.na(df$yards_gained[i]) &&
          df$yards_gained[i] >= 17) {
        y <- -3.04
      } else if (!is.na(df$yards_gained[i]) &&
                 10 <= df$yards_gained[i] &&
                 df$yards_gained[i] < 17) {
        y <- -0.89
      } else if (!is.na(df$yards_gained[i]) &&
                 1 <= df$yards_gained[i] &&
                 df$yards_gained[i] < 10) {
        y <- -0.54
      } else if (!is.na(df$yards_gained[i]) &&
                 df$yards_gained[i] < 0) {
        y <- 0.71
      } else if (is.na(df$yards_gained[i]) ||
                 df$yards_gained[i] == 0){
        y <- 0.18
      }
    } else if (df$play_type[i] == "run") {
      if (!is.na(df$yards_gained[i]) &&
          df$yards_gained[i] >= 12) {
        y <- -3.04
      } else if (!is.na(df$yards_gained[i]) &&
                 4 <= df$yards_gained[i] && df$yards_gained[i] < 12) {
        y <- -0.89
      } else if (!is.na(df$yards_gained[i]) &&
                 1 <= df$yards_gained[i] &&
                 df$yards_gained[i] < 4) {
        y <- -0.54
      } else if (!is.na(df$yards_gained[i]) &&
                 df$yards_gained[i] < 0) {
        y <- 0.71
      } else if (is.na(df$yards_gained[i]) ||
                 df$yards_gained[i] == 0) {
        y <- 0.18
      }
    }
  }else {
    y = 0
  }
  
  return(y)
}

mHome.score <- function(df, i) {
  score_difference <- df$total_home_score[i] - df$total_away_score[i]
  
  # Check if score_difference is NA
  if (is.na(score_difference)) {
    return(NA)
  }
  
  s <- 0 
  
  if (-8 <= score_difference && score_difference < 0) {
    s <- -3.75
  } else if (-16 <= score_difference && score_difference < -8) {
    s <- -4.64
  } else if (0 < score_difference && score_difference <= 8) {
    s <- 3.75
  } else if (8 < score_difference && score_difference <= 16) {
    s <- 4.64
  } else if (score_difference > 16) {
    s <- 5
  } else if (score_difference == 0) {
    s <- 0
  } else if (score_difference < -16) {
    s <- -5
  }
  
  return(s)
}

mHome.DIST <- function(df, i) {
  d <- 0
  
  if (is.na(df$down[i])) {
    return(NA)
  }
  
  if (df$posteam_type[i] == "home") {
    
    if (df$down[i] == 1) {
      if (df$ydstogo[i] <= 5) {
        d = 1.07
      }else if (df$ydstogo[i] > 5 &&
                df$ydstogo[i] <= 10) {
        d = 0
      }else if (df$ydstogo[i] > 10) {
        d = -1.79
      }
      
    }else if (df$down[i] == 2) {
      if (df$ydstogo[i] <=2) {
        d = 1.79
      }else if (df$ydstogo[i] > 2 &&
                df$ydstogo[i] <= 6) {
        d = 0
      }else if (df$ydstogo[i] > 6 &&
                df$ydstogo[i] <= 10) {
        d = 0
      }else if (df$ydstogo[i] < 10) {
        d = -1.61
      }
      
    }else if (df$down[i] == 3) {
      if (df$ydstogo[i] <=2) {
        d = 1.43
      }else if (df$ydstogo[i] > 2 &&
                df$ydstogo[i] <= 6) {
        d = 1.25
      }else if (df$ydstogo[i] > 6 &&
                df$ydstogo[i] <= 10) {
        d = -1.61
      }else if (df$ydstogo[i] < 10) {
        d = -1.96
      }
    }else if (df$down[i] == 4) {
      if (df$ydstogo[i] <=2) {
        d = 1.43
      }else if (df$ydstogo[i] > 2 &&
                df$ydstogo[i] <= 6) {
        d = 1.25
      }else if (df$ydstogo[i] > 6 &&
                df$ydstogo[i] <= 10) {
        d = -1.61
      }else if (df$ydstogo[i] < 10) {
        d = -1.96
      } 
    }
    
  }else if (df$posteam_type[i] == "away") {
    
    if (df$down[i] == 1) {
      if (df$ydstogo[i] <= 5) {
        d = -1.07
      }else if (df$ydstogo[i] > 5 &&
                df$ydstogo[i] <= 10) {
        d = 0
      }else if (df$ydstogo[i] > 10) {
        d = 1.79
      }
      
    }else if (df$down[i] == 2) {
      if (df$ydstogo[i] <=2) {
        d = -1.79
      }else if (df$ydstogo[i] > 2 &&
                df$ydstogo[i] <= 6) {
        d = 0
      }else if (df$ydstogo[i] > 6 &&
                df$ydstogo[i] <= 10) {
        d = 0
      }else if (df$ydstogo[i] < 10) {
        d = 1.61
      }
      
    }else if (df$down[i] == 3) {
      if (df$ydstogo[i] <=2) {
        d = -1.43
      }else if (df$ydstogo[i] > 2 &&
                df$ydstogo[i] <= 6) {
        d = -1.25
      }else if (df$ydstogo[i] > 6 &&
                df$ydstogo[i] <= 10) {
        d = 1.61
      }else if (df$ydstogo[i] < 10) {
        d = 1.96
      }
      
    }else if (df$down[i] == 4) {
      if (df$ydstogo[i] <=2) {
        d = -1.43
      }else if (df$ydstogo[i] > 2 &&
                df$ydstogo[i] <= 6) {
        d = -1.25
      }else if (df$ydstogo[i] > 6 &&
                df$ydstogo[i] <= 10) {
        d = 1.61
      }else if (df$ydstogo[i] < 10) {
        d = 1.96
      }
    }
  }else {
    d = 0
  }
  return(d)
}

Home.momentum <- function(df, i) {
  x <- mHome.result(df, i)
  y <- mHome.yard(df, i)
  s <- mHome.score(df, i)
  d <- mHome.DIST(df, i)
  
  for (j in 1:nrow(df)) {
    if (df$qtr[j] == 1) {
      s <- s * 0.1
    } else if (df$qtr[j] == 2) {
      s <- s * 0.2
    } else if (df$qtr[j] == 3) {
      s <- s * 0.3
    } else if (df$qtr[j] == 4) {
      s <- s * 0.4
    } else if (df$qtr[j] == 5) {
      s <- s * 0.5
    }
  }
  
  m <- x + s + d + y
  return(m)
}

mainHome.momentum <- function(df) {
  df$MomentumHome <- 0  # Initialize Home Momentum Vector
  
  for (i in 2:nrow(df)) {
    df$MomentumHome[i] <- Home.momentum(df, i)
  }
  return(df)
}

situational <- function(df) {
  df$MomentumHome <- 0  
  df$MomentumAway <- 0
  
  for (i in 2:nrow(df)) {
    df$MomentumHome[i] <- Home.momentum(df, i)
  }
  for (i in 2:nrow(df)) {
    df$MomentumAway[i] <- Away.momentum(df, i)
  }
  return(df)
}

AddedMomentum <- function(df) {
  
  # Initialize the new columns
  df$MomentumTotalHome <- df$MomentumHome[1]
  df$MomentumTotalAway <- df$MomentumAway[1]
  
  # Ensure the first values are not NA
  if (is.na(df$MomentumTotalHome[1])) df$MomentumTotalHome[1] <- 0
  if (is.na(df$MomentumTotalAway[1])) df$MomentumTotalAway[1] <- 0
  
  # Calculate MomentumTotalHome
  for (i in 2:nrow(df)) {
    x <- df$MomentumTotalHome[i-1] + df$MomentumHome[i]
    if (x <= 0) {
      df$MomentumTotalHome[i] <- 0
    } else {
      df$MomentumTotalHome[i] <- x
    }
  }
  
  # Calculate MomentumTotalAway
  for (i in 2:nrow(df)) {
    x <- df$MomentumTotalAway[i-1] + df$MomentumAway[i] 
    if (x <= 0) {
      df$MomentumTotalAway[i] <- 0
    } else {
      df$MomentumTotalAway[i] <- x
    }
  }
  
  return(df)
}

MomentumVisuals <- function(df) {
  ggplot(data = df, aes(x = PlayNumber)) +
    geom_line(aes(y = MomentumTotalHome, color = "Home Momentum"), size = 1.2) + 
    geom_line(aes(y = MomentumTotalAway, color = "Away Momentum"), size = 1.2) +
    geom_line(aes(y = total_home_score, color = "Total Home Score"), size = 1.2, alpha = 0.2) +
    geom_line(aes(y = total_away_score, color = "Total Away Score"), size = 1.2, alpha = 0.2) +
    geom_smooth(aes(y = MomentumTotalHome, color = "Home Momentum Trend"), method = "lm", se = FALSE, linetype = "dotted", size = 1.2) +
    geom_smooth(aes(y = MomentumTotalAway, color = "Away Momentum Trend"), method = "lm", se = FALSE, linetype = "dotted", size = 1.2) +
    geom_vline(data = df[df$quarter_end == 1, ], aes(xintercept = PlayNumber, color = "Quarter End"), linetype = "dashed", size = 0.4, alpha = 0.8) +
    labs(x = "Plays Played", y = "Momentum", title = "Momentum Comparison") +
    scale_x_continuous(breaks = seq(0, max(df$PlayNumber), by = 10)) +
    
    scale_color_manual(name = "Legend", 
                       values = c("Home Momentum" = "red", 
                                  "Away Momentum" = "blue", 
                                  "Total Home Score" = "red", 
                                  "Total Away Score" = "blue", 
                                  "Home Momentum Trend" = "red", 
                                  "Away Momentum Trend" = "blue", 
                                  "Quarter End" = "black"), 
                       guide = guide_legend(override.aes = list(
                         alpha = c(1, 1, 1, 1, 0.8, 0.2, 0.2), 
                         linetype = c("solid", "dotted", "solid", "dotted", "dotted", "solid", "solid"),
                         size = c(1.2, 1.2, 1.2, 1.2, 0.8, 1.2, 1.2)
                       ))) +
    theme_minimal()
}



adj_MomentumVisuals <- function(df) {
  
  ggplot(data = df, aes(x = PlayNumber)) +
    geom_line(aes(y = adj_MomentumTotalHome), color = "blue", size = 1.2) + 
    geom_line(aes(y = adj_MomentumTotalAway), color = "red", size = 1.2) +
    geom_line(aes(y = total_home_score), color = "blue", alpha = 0.2) +
    geom_line(aes(y = total_away_score), color = "red", alpha = 0.2) +
    labs(x = "Plays Played", y = "Momentum", title = "Adjusted Momentum Comparison") +
    scale_color_manual(values = c("blue", "red1"),
                       labels = c("ADJ Home Momentum", "ADJ Away Momentum",
                                  "Total Home Score", "Total Away Score")) +
    geom_text(aes(x = max(df$PlayNumber), y = df$adj_MomentumTotalHome[nrow(df)], label = "ADJ Home Momentum"),
              color = "blue", hjust = 1.1, vjust = -0.5, size = 4) +
    geom_text(aes(x = max(df$PlayNumber), y = df$adj_MomentumTotalAway[nrow(df)], label = "ADJ Away Momentum"),
              color = "red", hjust = 1.1, vjust = 1.5, size = 4) +
    geom_text(aes(x = max(df$PlayNumber), y = df$total_home_score[nrow(df)], label = "Total Home Score"),
              color = "blue", alpha = 0.2, hjust = 1.1, vjust = 0.5, size = 4, alpha = 0.2) +
    geom_text(aes(x = max(df$PlayNumber), y = df$total_away_score[nrow(df)], label = "Total Away Score"),
              color = "red", alpha = 0.2, hjust = 1.1, vjust = -1.5, size = 4, alpha = 0.2)
} 

s_analysis <- function(df) {
  drive <- c(df$fixed_drive[1])
  home_team <- c(df$home_team[1])
  away_team <- c(df$away_team[1])
  result <- c(df$fixed_drive_result[1])
  posteam_type <- c(df$posteam_type[2])
  
  for (i in 2:nrow(df)) {
    if (df$fixed_drive[i] != df$fixed_drive[i - 1]) {
      drive <- append(drive, df$fixed_drive[i])
      home_team <- append(home_team, df$home_team[i])
      result <- append(result, df$fixed_drive_result[i])
      posteam_type <- append(posteam_type, df$posteam_type[i])
      away_team <- append(away_team, df$away_team[i])
    }
  }
  
  ShortAnalysis <- data.frame(drive, result, posteam_type, home_team, away_team, stringsAsFactors = FALSE)
  
  return(ShortAnalysis)
}

effect_averages <- function(play_by_play) {
  
  Short_Analysis <- s_analysis(play_by_play)
  
  for (i in 1:nrow(Short_Analysis)) {
    if (Short_Analysis$result[i] == "Touchdown") {
      if (i < nrow(Short_Analysis) && Short_Analysis$result[i+1] == "Touchdown") {
        Short_Analysis$effect[i] <- 0.6
      } else if (i < nrow(Short_Analysis) && Short_Analysis$result[i+1] == "Field goal") {
        Short_Analysis$effect[i] <- 0.8
      } else {
        Short_Analysis$effect[i] <- 1
      }
    } else if (Short_Analysis$result[i] == "Field goal") {
      if (i < nrow(Short_Analysis) && Short_Analysis$result[i+1] == "Touchdown") {
        Short_Analysis$effect[i] <- 0.6 * 0.833333333
      } else if (i < nrow(Short_Analysis) && Short_Analysis$result[i+1] == "Field goal") {
        Short_Analysis$effect[i] <- 0.8 * 0.833333333
      } else {
        Short_Analysis$effect[i] <- 0.833333333
      }
    } else if (Short_Analysis$result[i] == "Turnover" || Short_Analysis$result[i] == "Turnover on downs") {
      Short_Analysis$effect[i] <- 0.5
    } else if (Short_Analysis$result[i] == "Punt") {
      Short_Analysis$effect[i] <- 0.666666667
    } else if (Short_Analysis$result[i] == "Missed field goal") {
      Short_Analysis$effect[i] <- 0.333333333
    } else {
      Short_Analysis$effect[i] <- 1
    }
  }
  
  df <- Short_Analysis
  
  # Initialize a list to store effects for each team
  effect_list <- list()
  
  # Loop through each row of the data frame `df`
  for (i in 1:nrow(df)) {
    home_team <- df$home_team[i]
    away_team <- df$away_team[i]
    effect <- df$effect[i]
    
    # Store effect for home_team
    if (!is.na(home_team)) {
      if (!home_team %in% names(effect_list)) {
        effect_list[[home_team]] <- numeric()
      }
      effect_list[[home_team]] <- c(effect_list[[home_team]], effect)
    }
    
    # Store effect for away_team
    if (!is.na(away_team)) {
      if (!away_team %in% names(effect_list)) {
        effect_list[[away_team]] <- numeric()
      }
      effect_list[[away_team]] <- c(effect_list[[away_team]], effect)
    }
  }
  
  # Initialize a list to store average effects for each team
  average_effects <- list()
  
  # Calculate average effect for each team
  for (team in names(effect_list)) {
    team_effects <- effect_list[[team]]
    avg_effect <- mean(team_effects)
    
    # Store average effect for the team
    average_effects[[team]] <- avg_effect
  }
  
  # Print or view the average effects for each team
  return(average_effects)
}  

adj_Momentum <- function(df, play_by_play) {
  # Initialize adjusted momentum columns with NA values
  df$adj_MomentumHome <- rep(NA, nrow(df))
  df$adj_MomentumAway <- rep(NA, nrow(df))
  
  # Calculate average effects for each team
  average_effects <- effect_averages(play_by_play)
  
  for (i in 1:nrow(df)) {
    a <- df$away_team[i]
    h <- df$home_team[i]
    
    # Retrieve average effects, defaulting to 1 if not found
    eH <- if (!is.null(average_effects[[h]])) average_effects[[h]] else 1
    eA <- if (!is.null(average_effects[[a]])) average_effects[[a]] else 1
    
    # Adjust MomentumHome and MomentumAway based on average effects
    if (eH > eA) {
      if (df$MomentumHome[i] < 0) {
        df$adj_MomentumHome[i] <- df$MomentumHome[i] * eH
      } else {
        df$adj_MomentumHome[i] <- df$MomentumHome[i]
      }
      df$adj_MomentumAway[i] <- df$MomentumAway[i]
    } else if (eA > eH) {
      if (df$MomentumAway[i] < 0) {
        df$adj_MomentumAway[i] <- df$MomentumAway[i] * eA
      } else {
        df$adj_MomentumAway[i] <- df$MomentumAway[i]
      }
      df$adj_MomentumHome[i] <- df$MomentumHome[i]
    } else {
      df$adj_MomentumHome[i] <- df$MomentumHome[i]
      df$adj_MomentumAway[i] <- df$MomentumAway[i]
    }
  }
  
  return(df)
}

Added_adj_Momentum <- function(df) {
  df$adj_MomentumTotalHome <- df$adj_MomentumHome[1]
  df$adj_MomentumTotalAway <- df$adj_MomentumAway[1]
  
  for (i in 2:nrow(df)) {
    x <- df$adj_MomentumTotalHome[i-1] + df$adj_MomentumHome[i]
    if (x <= 0) {
      df$adj_MomentumTotalHome[i] <- 0
    } else {
      df$adj_MomentumTotalHome[i] <- x
    }
  }
  
  for (i in 2:nrow(df)) {
    x <- df$adj_MomentumTotalAway[i-1] + df$adj_MomentumAway[i]
    if (x <= 0) {
      df$adj_MomentumTotalAway[i] <- 0
    } else {
      df$adj_MomentumTotalAway[i] <- x
    }
  }
  
  return(df)
}

Test <- function(play_by_play, ID) {
  p <- play_by_play
  t <- play_by_play[play_by_play$game_id %in% c(ID), ]
  Testdata <- t
  Testdata$posteam_type[Testdata$posteam_type == ""] <- NA
  Testdata$fixed_drive_result[Testdata$fixed_drive_result == ""] <- NA
  Testdata$PlayNumber <- seq(1, nrow(Testdata))
  
  TestResultAway <- mAway.result(Testdata, 2)
  TestYardAway <- mAway.yard(Testdata, 2)
  TestScoreAway <- mAway.score(Testdata, 2)
  TestDISTAway <- mAway.DIST(Testdata, 2)
  
  TestResultHome <- mHome.result(Testdata, 2)
  TestYardHome <- mHome.yard(Testdata, 2)
  TestScoreHome <- mHome.score(Testdata, 2)
  TestDISTHome <- mHome.DIST(Testdata, 2)
  
  
  Final <- situational(Testdata)
  Final$MomentumHome[is.na(Final$MomentumHome)] <- 0
  Final$MomentumAway[is.na(Final$MomentumAway)] <- 0
  
  AddedMomentumData <- AddedMomentum(Final)
  adj <- adj_Momentum(AddedMomentumData, p)
  adjFinal <- Added_adj_Momentum(adj)
  return(adjFinal)
}


#Live Data Functions------------------------------------------------------------
Live.update_dataset <- function(data) {
  data$GN.LS <- ifelse(data$ODK == "O", data$GN.LS * (-1), data$GN.LS)
  data$YARD.LN <- data$YARD.LN * (-1)
  data$ScoreAway <- 0
  data$ScoreAway2.0 <- 0
  data$ScoreHome <- 0
  data$ScoreHome2.0 <- 0
  data$RESULT <- ifelse(grepl("Def TD", data$RESULT), "DEFT", data$RESULT)
  data <- data[!is.na(data$PLAY.TYPE) & data$PLAY.TYPE != "", ]
  data <- subset(data, !grepl("Timeout", RESULT))
  data$quarter_end <- as.integer(c(0, diff(data$QTR) != 0))
  return(data)
}

Live.DataClean <- function(data) {
  C2 <- Live.update_dataset(data)
  C3 <- Live.BallColumn(C2)
  C4 <- Live.ScoreColumn(C3)
  C5 <- Live.ScoreColumn2.0(C4)
  C6 <- Live.drive(C5)
  C7 <- Live.PlayColumn(C6)
  
  return(C7)
}

Live.PlayColumn <- function(data) {
  data$PLAY.. <- seq_len(nrow(data))
  return(data)
}



Live.BallColumn <- function(data) {
  data$Ball <- ""
  for (i in 1:nrow(data)) {
    if (i > 1) {
      if (data$ODK[i] == "O") {
        data$Ball[i] <- "Home"
      } else if (data$ODK[i] == "D") {
        data$Ball[i] <- "Away"
      } else if (data$ODK[i] == "K" &&
                 data$Ball[i-1] == "Away" &&
                 data$ODK[i-1] != "K" &&
                 data$RESULT[i-1] != "DEFT") {
        data$Ball[i] <- "Away"
      } else if (data$ODK[i] == "K" &&
                 data$Ball[i-1] == "Home" &&
                 data$ODK[i-1] != "K" &&
                 data$RESULT[i-1] != "DEFT") {
        data$Ball[i] <- "Home"
      } else if (data$ODK[i] == "K" &&
                 data$ODK[i-1] == "K" &&
                 data$Ball[i-1] == "Away" &&
                 data$RESULT[i-1] != "TD") {
        data$Ball[i] <- "Home"
      } else if (data$ODK[i] == "K" &&
                 data$ODK[i-1] == "K" &&
                 data$Ball[i-1] == "Home" &&
                 data$RESULT[i-1] != "TD") {
        data$Ball[i] <- "Away"
      } else if (data$ODK[i] == "K" &&
                 data$RESULT[i-1] == "TD" &&
                 data$ODK[i-1] == "K" &&
                 data$Ball[i-1] == "Home") {
        data$Ball[i] <- "Home"
      } else if (data$ODK[i] == "K" &&
                 data$RESULT[i-1] == "TD" &&
                 data$ODK[i-1] == "K" &&
                 data$Ball[i-1] == "Away") {
        data$Ball[i] <- "Away"
      } else if (data$ODK[i-1] == "D" &&
                 data$RESULT[i-1] == "DEFT" &&
                 data$Ball[i-1] == "Away") {
        data$Ball[i] <- "Home"
      } else if (data$ODK[i-1] == "D" &&
                 data$RESULT[i-1] == "DEFT" &&
                 data$Ball[i-1] == "Home") {
        data$Ball[i] <- "Away"
      } 
    }
  }
  return(data)
}

Live.ScoreColumn <- function(data) {
  for (i in 1:nrow(data)) {
    if (i > 1) {
      if (data$Ball[i] == "Home" &&
          data$RESULT[i] == "DEFT") {
        data$ScoreAway[i] <- data$ScoreAway[i-1] + 6
      } else if (data$Ball[i] == "Away" &&
                 data$RESULT[i] == "DEFT") {
        data$ScoreAway[i] <- data$ScoreAway[i-1] - 6
      } else if (data$Ball[i] == "Home" &&
                 grepl("TD", data$RESULT[i])) {
        data$ScoreAway[i] <- data$ScoreAway[i-1] - 6
      } else if (data$Ball[i] == "Away" &&
                 grepl("TD", data$RESULT[i])) {
        data$ScoreAway[i] <- data$ScoreAway[i-1] + 6
      } else if (data$Ball[i] == "Home" &&
                 grepl("Extra Pt.", data$PLAY.TYPE[i]) &&
                 data$RESULT[i] == "Good") {
        data$ScoreAway[i] <- data$ScoreAway[i-1] - 1
      } else if (data$Ball[i] == "Away" &&
                 grepl("Extra Pt.", data$PLAY.TYPE[i]) &&
                 data$RESULT[i] == "Good") {
        data$ScoreAway[i] <- data$ScoreAway[i-1] + 1
      } else if (data$Ball[i] == "Home" &&
                 grepl("FG", data$PLAY.TYPE[i]) &&
                 data$RESULT[i] == "Good") {
        data$ScoreAway[i] <- data$ScoreAway[i-1] - 3
      } else if (data$Ball[i] == "Away" &&
                 grepl("FG", data$PLAY.TYPE[i]) &&
                 data$RESULT[i] == "Good") {
        data$ScoreAway[i] <- data$ScoreAway[i-1] + 3
      } else {
        data$ScoreAway[i] <- data$ScoreAway[i-1]
      }
      data$ScoreHome[i] <- data$ScoreAway[i] * -1
    }
  }
  
  if (nrow(data) > 0) {
    if (data$Ball[1] == "Home" &&
        grepl("TD", data$RESULT[1])) {
      data$ScoreAway[1] <- -6
    } else if (data$Ball[1] == "Away" &&
               grepl("TD", data$RESULT[1])) {
      data$ScoreAway[1] <- 6
    }
  }
  
  return(data)
}

Live.ScoreColumn2.0 <- function(data) {
  for (i in 1:nrow(data)) {
    if (i > 1) {
      if (data$Ball[i] == "Home" &&
          data$RESULT[i] == "DEFT") {
        data$ScoreAway2.0[i] <- data$ScoreAway2.0[i-1] + 6
      } else if (data$Ball[i] == "Away" &&
                 grepl("TD", data$RESULT[i])) {
        data$ScoreAway2.0[i] <- data$ScoreAway2.0[i-1] + 6
      } else if (data$Ball[i] == "Away" &&
                 grepl("Extra Pt.", data$PLAY.TYPE[i]) && data$RESULT[i] == "Good") {
        data$ScoreAway2.0[i] <- data$ScoreAway2.0[i-1] + 1
      } else if (data$Ball[i] == "Away" &&
                 grepl("FG", data$PLAY.TYPE[i]) &&
                 data$RESULT[i] == "Good") {
        data$ScoreAway2.0[i] <- data$ScoreAway2.0[i-1] + 3
      } else {
        data$ScoreAway2.0[i] <- data$ScoreAway2.0[i-1]
      }
    }
  }
  
  if (nrow(data) > 0) {
    if (data$Ball[1] == "Away" &&
        grepl("TD", data$RESULT[1])) {
      data$ScoreAway2.0[1] <- 6
    }
  }
  for (i in 1:nrow(data)) {
    if (i > 1) {
      if (data$Ball[i] == "Away" &&
          data$RESULT[i] == "DEFT") {
        data$ScoreHome2.0[i] <- data$ScoreHome2.0[i-1] + 6
      } else if (data$Ball[i] == "Home" &&
                 grepl("TD", data$RESULT[i])) {
        data$ScoreHome2.0[i] <- data$ScoreHome2.0[i-1] + 6
      } else if (data$Ball[i] == "Home" &&
                 grepl("Extra Pt.", data$PLAY.TYPE[i]) &&
                 data$RESULT[i] == "Good") {
        data$ScoreHome2.0[i] <- data$ScoreHome2.0[i-1] + 1
      } else if (data$Ball[i] == "Home" &&
                 grepl("FG", data$PLAY.TYPE[i]) &&
                 data$RESULT[i] == "Good") {
        data$ScoreHome2.0[i] <- data$ScoreHome2.0[i-1] + 3
      } else {
        data$ScoreHome2.0[i] <- data$ScoreHome2.0[i-1]
      }
    }
  }
  
  if (nrow(data) > 0) {
    if (data$Ball[1] == "Away" &&
        grepl("TD", data$RESULT[1])) {
      data$ScoreHome2.0[1] <- 6
    }
  }
  return(data)
}

Live.PlayType <- function(data) {
  for (i in 1:nrow(data)) {
    if (data$PLAY.TYPE[i] == "") {
      if (grepl("Rush", data$RESULT[i])) {
        data$PLAY.TYPE[i] <- "Run"
      } else if (grepl("Complete", data$RESULT[i]) || 
                 data$RESULT[i] == "Incomplete" || 
                 grepl("Sack", data$RESULT[i]) ||
                 data$RESULT[i] == "Scramble" ||
                 data$RESULT[i] == "Interception") {
        data$PLAY.TYPE[i] <- "Pass"
      }
    }
  }
  return(data)
}

Live.drive <- function(data) {
  
  data$drive <- 0
  
  for (i in 1:nrow(data)) {
    if (data$Ball[i] != data$Ball[i-1] && data$Ball[i] == data$Ball[i+1]) {
      data$drive[i] <- 1
    }
  }
  
  data$drive_length <- 0
  
  for (i in 1:nrow(data)) {
    if (data$drive[i] == 0) {
      data$drive_length[i] <- ifelse(i == 1, 1, data$drive_length[i-1] + 1)
    } else if (data$drive[i] == 1) {
      data$drive_length[i] <- 1  
    }
  }
  
  drive_count <- numeric(nrow(data))
  count <- 0
  
  for (i in 1:nrow(data)) {
    if (data$drive[i] == 1) {
      count <- count + 1
    }
    drive_count[i] <- count
  }
  
  data$drive_count <- drive_count
  
  return(data)
}

Live.IndicatorColumns <- function(data) {
  unique_formations <- unique(data$OFF.FORM)
  for (form in unique_formations) {
    column_name <- paste0("form_", gsub("\\s", "_", tolower(form)))
    data[[column_name]] <- ifelse(data$OFF.FORM == form, 1, 0)
  }
  
  unique_plays <- unique(data$OFF.PLAY)
  for (play in unique_plays) {
    column_name <- paste0("play_", gsub("\\s", "_", tolower(play)))
    data[[column_name]] <- ifelse(data$OFF.PLAY == play, 1, 0)
  }
  
  unique_fronts <- unique(data$DEF.FRONT)
  for (front in unique_fronts) {
    column_name <- paste0("front_", gsub("\\s", "_", tolower(front)))
    data[[column_name]] <- ifelse(data$DEF.FRONT == front, 1, 0)
  }
  
  unique_coverages <- unique(data$COVERAGE)
  for (coverage in unique_coverages) {
    column_name <- paste0("coverage_", gsub("\\s", "_", tolower(coverage)))
    data[[column_name]] <- ifelse(data$COVERAGE == coverage, 1, 0)
  }
  
  unique_blitzes <- unique(data$BLITZ)
  for (blitz in unique_blitzes) {
    column_name <- paste0("blitz_", gsub("\\s", "_", tolower(blitz)))
    data[[column_name]] <- ifelse(data$BLITZ == blitz, 1, 0)
  }
  
  return(data)
}

Live.mAway.result <- function(data, i) {
  r <- 0
  
  if (data$Ball[i] == "Away") {
    if (!is.na(data$RESULT[i]) && grepl("TD", data$RESULT[i])) {
      r <- 4.82
    } else if (!is.na(data$PLAY.TYPE[i]) &&
               data$PLAY.TYPE[i] == "FG" &&
               data$RESULT[i] == "Good") {
      r <- 4.29
    } else if (!is.na(data$PLAY.TYPE[i]) &&
               data$PLAY.TYPE[i] == "FG" &&
               data$RESULT[i] == "No Good") {
      r <- -3.93
    } else if (!is.na(data$PLAY.TYPE[i]) &&
               data$PLAY.TYPE[i] == "FG" &&
               data$RESULT[i] == "Block") {
      r <- -4.11
    } else if (!is.na(data$PLAY.TYPE[i]) &&
               data$PLAY.TYPE[i] == "Extra Pt." &&
               data$RESULT[i] == "Good") {
      r <- 2.68
    } else if (!is.na(data$PLAY.TYPE[i]) &&
               data$PLAY.TYPE[i] == "Extra Pt." &&
               data$RESULT[i] == "No Good") {
      r <- -2.32
    } else if (!is.na(data$PLAY.TYPE[i]) &&
               data$PLAY.TYPE[i] == "Extra Pt." &&
               data$RESULT[i] == "Block") {
      r <- -2.5
    } else if (!is.na(data$RESULT[i]) &&
               data$RESULT[i] == "Sack") {
      r <- -2.86
    } else if (!is.na(data$DN[i]) &&
               data$DN[i] == 1) {
      r <- 2.14
    } else if (!is.na(data$RESULT[i]) &&
               data$RESULT[i] == "Penalty" &&
               !is.na(data$GN.LS[i]) &&
               data$GN.LS[i] < 0) {
      r <- -3.21
    } else if (!is.na(data$RESULT[i]) &&
               data$RESULT[i] == "Penalty" &&
               !is.na(data$GN.LS[i]) &&
               data$GN.LS[i] > 0) {
      r <- 3.21
    } else if (!is.na(data$RESULT[i]) &&
               grepl("Punt", data$RESULT[i])) {
      r <- -3.39
    } else if (!is.na(data$RESULT[i]) &&
               grepl("Interception", data$RESULT[i])) {
      r <- -4.64
    } else if (!is.na(data$RESULT[i]) &&
               grepl("Fumble", data$RESULT[i])) {
      r <- -4.64
    } else {
      r <- 0
    } 
  } else if (data$Ball[i] == "Home") {
    if (!is.na(data$RESULT[i]) && grepl("TD", data$RESULT[i])) {
      r <- -4.82
    } else if (!is.na(data$PLAY.TYPE[i]) &&
               data$PLAY.TYPE[i] == "FG" &&
               data$RESULT[i] == "Good") {
      r <- -4.29
    } else if (!is.na(data$PLAY.TYPE[i]) &&
               data$PLAY.TYPE[i] == "FG" &&
               data$RESULT[i] == "No Good") {
      r <- 3.93
    } else if (!is.na(data$PLAY.TYPE[i]) &&
               data$PLAY.TYPE[i] == "FG" &&
               data$RESULT[i] == "Block") {
      r <- 4.11
    } else if (!is.na(data$PLAY.TYPE[i]) &&
               data$PLAY.TYPE[i] == "Extra Pt." &&
               data$RESULT[i] == "Good") {
      r <- -2.68
    } else if (!is.na(data$PLAY.TYPE[i]) &&
               data$PLAY.TYPE[i] == "Extra Pt." &&
               data$RESULT[i] == "No Good") {
      r <- 2.32
    } else if (!is.na(data$PLAY.TYPE[i]) &&
               data$PLAY.TYPE[i] == "Extra Pt." &&
               data$RESULT[i] == "Block") {
      r <- 2.5
    } else if (!is.na(data$RESULT[i]) &&
               data$RESULT[i] == "Sack") {
      r <- 2.86
    } else if (!is.na(data$DN[i]) &&
               data$DN[i] == 1) {
      r <- -2.14
    } else if (!is.na(data$RESULT[i]) &&
               data$RESULT[i] == "Penalty" &&
               !is.na(data$GN.LS[i]) &&
               data$GN.LS[i] < 0) {
      r <- 3.21
    } else if (!is.na(data$RESULT[i]) &&
               data$RESULT[i] == "Penalty" &&
               !is.na(data$GN.LS[i]) &&
               data$GN.LS[i] > 0) {
      r <- -3.21
    } else if (!is.na(data$RESULT[i]) &&
               grepl("Punt", data$RESULT[i])) {
      r <- 3.39
    } else if (!is.na(data$RESULT[i]) &&
               grepl("Interception", data$RESULT[i])) {
      r <- 4.64
    } else if (!is.na(data$RESULT[i]) &&
               grepl("Fumble", data$RESULT[i])) {
      r <- 4.64
    } else {
      r <- 0
    }
  }
  
  return(r)
}

Live.mAway.yard <- function(data, i) {
  y <- 0
  
  if (data$Ball[i] == "Away") {
    if (data$PLAY.TYPE[i] == "Pass") {
      if (!is.na(data$GN.LS[i]) &&
          data$GN.LS[i] >= 17) {
        y <- 3.04
      } else if (!is.na(data$GN.LS[i]) &&
                 10 <= data$GN.LS[i] &&
                 data$GN.LS[i] < 17) {
        y <- 0.89
      } else if (!is.na(data$GN.LS[i]) &&
                 1 <= data$GN.LS[i] &&
                 data$GN.LS[i] < 10) {
        y <- 0.54
      } else if (!is.na(data$GN.LS[i]) &&
                 data$GN.LS[i] < 0) {
        y <- -0.71
      } else if (is.na(data$GN.LS[i]) ||
                 data$GN.LS[i] == 0){
        y <- -0.18
      }
    } else if (data$PLAY.TYPE[i] == "Run") {
      if (!is.na(data$GN.LS[i]) &&
          data$GN.LS[i] >= 12) {
        y <- 3.04
      } else if (!is.na(data$GN.LS[i]) &&
                 4 <= data$GN.LS[i] && data$GN.LS[i] < 12) {
        y <- 0.89
      } else if (!is.na(data$GN.LS[i]) &&
                 1 <= data$GN.LS[i] &&
                 data$GN.LS[i] < 4) {
        y <- 0.54
      } else if (!is.na(data$GN.LS[i]) &&
                 data$GN.LS[i] < 0) {
        y <- -0.71
      } else if (is.na(data$GN.LS[i]) ||
                 data$GN.LS[i] == 0) {
        y <- -0.18
      }
    }
  } else if (data$Ball[i] == "Home") {
    if (data$PLAY.TYPE[i] == "Pass") {
      if (!is.na(data$GN.LS[i]) &&
          data$GN.LS[i] <= -17) {
        y <- -3.04
      } else if (!is.na(data$GN.LS[i]) &&
                 -10 >= data$GN.LS[i] &&
                 data$GN.LS[i] > -17) {
        y <- -0.89
      } else if (!is.na(data$GN.LS[i]) &&
                 -1 >= data$GN.LS[i] &&
                 data$GN.LS[i] > -10) {
        y <- -0.54
      } else if (!is.na(data$GN.LS[i]) &&
                 data$GN.LS[i] > 0) {
        y <- 0.71
      } else if (is.na(data$GN.LS[i]) ||
                 data$GN.LS[i] == 0){
        y <- 0.18
      }
    } else if (data$PLAY.TYPE[i] == "Run") {
      if (!is.na(data$GN.LS[i]) &&
          data$GN.LS[i] >= -12) {
        y <- -3.04
      } else if (!is.na(data$GN.LS[i]) &&
                 -4 >= data$GN.LS[i] && data$GN.LS[i] > -12) {
        y <- -0.89
      } else if (!is.na(data$GN.LS[i]) &&
                 -1 >= data$GN.LS[i] &&
                 data$GN.LS[i] > -4) {
        y <- -0.54
      } else if (!is.na(data$GN.LS[i]) &&
                 data$GN.LS[i] > 0) {
        y <- 0.71
      } else if (is.na(data$GN.LS[i]) ||
                 data$GN.LS[i] == 0) {
        y <- 0.18
      }
    }
  }
  
  return(y)
}

Live.mAway.score <- function(data, i) {
  s <- 0 
  
  if (-8 <= data$ScoreAway[i] &&
      data$ScoreAway[i] < 0) {
    s <- -3.75
  } else if (-16 <= data$ScoreAway[i] &&
             data$ScoreAway[i] < -8) {
    s <- -4.64
  } else if (0 < data$ScoreAway[i] &&
             data$ScoreAway[i] < 8) {
    s <- 3.75
  } else if (8 <= data$ScoreAway[i] &&
             data$ScoreAway[i] < 16) {
    s <- 4.64
  } else if (data$ScoreAway[i] >= 16) {
    s <- 5
  } else if (data$ScoreAway[i] == 0) {
    s <- 0
  } else if(data$ScoreAway[i] <= -16) {
    s <- -5
  }
  
  return(s)
}

Live.mAway.DIST <- function(data, i) {
  d <- 0
  
  # Check if the Ball value is "Away"
  if (data$Ball[i] == "Away") {
    
    # Ensure DN[i] is not NA before proceeding with the comparisons
    if (!is.na(data$DN[i]) && data$DN[i] == 1) {
      if (data$DIST[i] <= 5) {
        d <- 1.07
      } else if (data$DIST[i] > 5 && data$DIST[i] <= 10) {
        d <- 0
      } else if (data$DIST[i] > 10) {
        d <- -1.79
      }
      
    } else if (!is.na(data$DN[i]) && data$DN[i] == 2) {
      if (data$DIST[i] <= 2) {
        d <- 1.79
      } else if (data$DIST[i] > 2 && data$DIST[i] <= 6) {
        d <- 0
      } else if (data$DIST[i] > 6 && data$DIST[i] <= 10) {
        d <- 0
      } else if (data$DIST[i] > 10) {
        d <- -1.61
      }
      
    } else if (!is.na(data$DN[i]) && data$DN[i] == 3) {
      if (data$DIST[i] <= 2) {
        d <- 1.43
      } else if (data$DIST[i] > 2 && data$DIST[i] <= 6) {
        d <- 1.25
      } else if (data$DIST[i] > 6 && data$DIST[i] <= 10) {
        d <- -1.61
      } else if (data$DIST[i] > 10) {
        d <- -1.96
      }
    }
    
  } else if (data$Ball[i] == "Home") {
    
    if (!is.na(data$DN[i]) && data$DN[i] == 1) {
      if (data$DIST[i] <= 5) {
        d <- -1.07
      } else if (data$DIST[i] > 5 && data$DIST[i] <= 10) {
        d <- 0
      } else if (data$DIST[i] > 10) {
        d <- 1.79
      }
      
    } else if (!is.na(data$DN[i]) && data$DN[i] == 2) {
      if (data$DIST[i] <= 2) {
        d <- -1.79
      } else if (data$DIST[i] > 2 && data$DIST[i] <= 6) {
        d <- 0
      } else if (data$DIST[i] > 6 && data$DIST[i] <= 10) {
        d <- 0
      } else if (data$DIST[i] > 10) {
        d <- 1.61
      }
      
    } else if (!is.na(data$DN[i]) && data$DN[i] == 3) {
      if (data$DIST[i] <= 2) {
        d <- -1.43
      } else if (data$DIST[i] > 2 && data$DIST[i] <= 6) {
        d <- -1.25
      } else if (data$DIST[i] > 6 && data$DIST[i] <= 10) {
        d <- 1.61
      } else if (data$DIST[i] > 10) {
        d <- 1.96
      }
    }
  }
  
  return(d)
}


Live.Away.momentum <- function(data, i) {
  x <- Live.mAway.result(data, i)
  y <- Live.mAway.yard(data, i)
  s <- Live.mAway.score(data, i)
  d <- Live.mAway.DIST(data, i)
  
  for (j in 1:nrow(data)) {
    if (data$QTR[j] == 1) {
      s <- s * 0.1
    } else if (data$QTR[j] == 2) {
      s <- s * 0.2
    } else if (data$QTR[j] == 3) {
      s <- s * 0.3
    } else if (data$QTR[j] == 4) {
      s <- s * 0.4
    } else if (data$QTR[j] == 5) {
      s <- s * 0.5
    }
  }
  
  m <- x + y + d + s
  return(m)
}

Live.mainAway.momentum <- function(data) {
  data$MomentumAway <- 0  # Initialize Away Momentum Vector
  
  for (i in 1:nrow(data)) {
    data$MomentumAway[i] <- Live.Away.momentum(data, i)
  }
  
  return(data)
}

Live.mHome.result <- function(data, i) {
  r <- 0
  
  if (data$Ball[i] == "Home") {
    if (!is.na(data$RESULT[i]) &&
        grepl("TD", data$RESULT[i])) {
      r <- 4.82
    } else if (!is.na(data$PLAY.TYPE[i]) &&
               data$PLAY.TYPE[i] == "FG" &&
               data$RESULT[i] == "Good") {
      r <- 4.29
    } else if (!is.na(data$PLAY.TYPE[i]) &&
               data$PLAY.TYPE[i] == "FG" &&
               data$RESULT[i] == "No Good") {
      r <- -3.93
    } else if (!is.na(data$PLAY.TYPE[i]) &&
               data$PLAY.TYPE[i] == "FG" &&
               data$RESULT[i] == "Block") {
      r <- -4.11
    } else if (!is.na(data$PLAY.TYPE[i]) &&
               data$PLAY.TYPE[i] == "Extra Pt." &&
               data$RESULT[i] == "Good") {
      r <- 2.68
    } else if (!is.na(data$PLAY.TYPE[i]) &&
               data$PLAY.TYPE[i] == "Extra Pt." &&
               data$RESULT[i] == "No Good") {
      r <- -2.32
    } else if (!is.na(data$PLAY.TYPE[i]) &&
               data$PLAY.TYPE[i] == "Extra Pt." &&
               data$RESULT[i] == "Block") {
      r <- -2.5
    } else if (!is.na(data$RESULT[i]) &&
               data$RESULT[i] == "Sack") {
      r <- -2.86
    } else if (!is.na(data$DN[i]) &&
               data$DN[i] == 1) {
      r <- 2.14
    } else if (!is.na(data$RESULT[i]) &&
               data$RESULT[i] == "Penalty" &&
               !is.na(data$GN.LS[i]) &&
               data$GN.LS[i] < 0) {
      r <- -3.21
    } else if (!is.na(data$RESULT[i]) &&
               data$RESULT[i] == "Penalty" &&
               !is.na(data$GN.LS[i]) && data$GN.LS[i] > 0) {
      r <- 3.21
    } else if (!is.na(data$RESULT[i]) &&
               grepl("Punt", data$RESULT[i])) {
      r <- -3.39
    } else if (!is.na(data$RESULT[i]) &&
               grepl("Interception", data$RESULT[i])) {
      r <- -4.64
    } else if (!is.na(data$RESULT[i]) &&
               grepl("Fumble", data$RESULT[i])) {
      r <- -4.64
    } else {
      r <- 0
    } 
  } else if (data$Ball[i] == "Away") {
    if (!is.na(data$RESULT[i]) &&
        grepl("TD", data$RESULT[i])) {
      r <- -4.82
    } else if (!is.na(data$PLAY.TYPE[i]) &&
               data$PLAY.TYPE[i] == "FG" &&
               data$RESULT[i] == "Good") {
      r <- -4.29
    } else if (!is.na(data$PLAY.TYPE[i]) &&
               data$PLAY.TYPE[i] == "FG" &&
               data$RESULT[i] == "No Good") {
      r <- 3.93
    } else if (!is.na(data$PLAY.TYPE[i]) &&
               data$PLAY.TYPE[i] == "FG" &&
               data$RESULT[i] == "Block") {
      r <- 4.11
    } else if (!is.na(data$PLAY.TYPE[i]) &&
               data$PLAY.TYPE[i] == "Extra Pt." &&
               data$RESULT[i] == "Good") {
      r <- -2.68
    } else if (!is.na(data$PLAY.TYPE[i]) &&
               data$PLAY.TYPE[i] == "Extra Pt." &&
               data$RESULT[i] == "No Good") {
      r <- 2.32
    } else if (!is.na(data$PLAY.TYPE[i]) &&
               data$PLAY.TYPE[i] == "Extra Pt." &&
               data$RESULT[i] == "Block") {
      r <- 2.5
    } else if (!is.na(data$RESULT[i]) &&
               data$RESULT[i] == "Sack") {
      r <- 2.86
    } else if (!is.na(data$DN[i]) &&
               data$DN[i] == 1) {
      r <- -2.14
    } else if (!is.na(data$RESULT[i]) &&
               data$RESULT[i] == "Penalty" &&
               !is.na(data$GN.LS[i]) &&
               data$GN.LS[i] < 0) {
      r <- 3.21
    } else if (!is.na(data$RESULT[i]) &&
               data$RESULT[i] == "Penalty" &&
               !is.na(data$GN.LS[i]) &&
               data$GN.LS[i] > 0) {
      r <- -3.21
    } else if (!is.na(data$RESULT[i]) &&
               grepl("Punt", data$RESULT[i])) {
      r <- 3.39
    } else if (!is.na(data$RESULT[i]) &&
               grepl("Interception", data$RESULT[i])) {
      r <- 4.64
    } else if (!is.na(data$RESULT[i]) &&
               grepl("Fumble", data$RESULT[i])) {
      r <- 4.64
    } else {
      r <- 0
    }
  }
  
  return(r)
}

Live.mHome.yard <- function(data, i) {
  y <- 0
  
  if (data$Ball[i] == "Away") {
    if (data$PLAY.TYPE[i] == "Pass") {
      if (!is.na(data$GN.LS[i]) &&
          data$GN.LS[i] >= 17) {
        y <- -3.04
      } else if (!is.na(data$GN.LS[i]) &&
                 10 <= data$GN.LS[i] &&
                 data$GN.LS[i] < 17) {
        y <- -0.89
      } else if (!is.na(data$GN.LS[i]) &&
                 1 <= data$GN.LS[i] &&
                 data$GN.LS[i] < 10) {
        y <- -0.54
      } else if (!is.na(data$GN.LS[i]) &&
                 data$GN.LS[i] < 0) {
        y <- 0.71
      } else if (is.na(data$GN.LS[i]) ||
                 data$GN.LS[i] == 0){
        y <- 0.18
      }
    } else if (data$PLAY.TYPE[i] == "Run") {
      if (!is.na(data$GN.LS[i]) &&
          data$GN.LS[i] >= 12) {
        y <- -3.04
      } else if (!is.na(data$GN.LS[i]) &&
                 4 <= data$GN.LS[i] && data$GN.LS[i] < 12) {
        y <- -0.89
      } else if (!is.na(data$GN.LS[i]) &&
                 1 <= data$GN.LS[i] &&
                 data$GN.LS[i] < 4) {
        y <- -0.54
      } else if (!is.na(data$GN.LS[i]) &&
                 data$GN.LS[i] < 0) {
        y <- 0.71
      } else if (is.na(data$GN.LS[i]) ||
                 data$GN.LS[i] == 0) {
        y <- 0.18
      }
    }
  } else if (data$Ball[i] == "Home") {
    if (data$PLAY.TYPE[i] == "Pass") {
      if (!is.na(data$GN.LS[i]) &&
          data$GN.LS[i] <= -17) {
        y <- 3.04
      } else if (!is.na(data$GN.LS[i]) &&
                 -10 >= data$GN.LS[i] &&
                 data$GN.LS[i] > -17) {
        y <- 0.89
      } else if (!is.na(data$GN.LS[i]) &&
                 -1 >= data$GN.LS[i] &&
                 data$GN.LS[i] > -10) {
        y <- 0.54
      } else if (!is.na(data$GN.LS[i]) &&
                 data$GN.LS[i] > 0) {
        y <- -0.71
      } else if (is.na(data$GN.LS[i]) ||
                 data$GN.LS[i] == 0){
        y <- -0.18
      }
    } else if (data$PLAY.TYPE[i] == "Run") {
      if (!is.na(data$GN.LS[i]) &&
          data$GN.LS[i] >= -12) {
        y <- 3.04
      } else if (!is.na(data$GN.LS[i]) &&
                 -4 >= data$GN.LS[i] && data$GN.LS[i] > -12) {
        y <- 0.89
      } else if (!is.na(data$GN.LS[i]) &&
                 -1 >= data$GN.LS[i] &&
                 data$GN.LS[i] > -4) {
        y <- 0.54
      } else if (!is.na(data$GN.LS[i]) &&
                 data$GN.LS[i] > 0) {
        y <- -0.71
      } else if (is.na(data$GN.LS[i]) ||
                 data$GN.LS[i] == 0) {
        y <- -0.18
      }
    }
  }
  
  return(y)
}

Live.mHome.score <- function(data, i) {
  s <- 0 
  
  if (-8 <= data$ScoreHome[i] &&
      data$ScoreHome[i] < 0) {
    s <- -3.75
  } else if (-16 <= data$ScoreHome[i] &&
             data$ScoreHome[i] < -8) {
    s <- -4.64
  } else if (0 < data$ScoreHome[i] &&
             data$ScoreHome[i] < 8) {
    s <- 3.75
  } else if (8 <= data$ScoreHome[i] &&
             data$ScoreHome[i] < 16) {
    s <- 4.64
  } else if (data$ScoreHome[i] >= 16) {
    s <- 5
  } else if (data$ScoreHome[i] == 0) {
    s <- 0
  } else if(data$ScoreHome[i] <= -16) {
    s <- -5
  }
  
  return(s)
}

Live.mHome.DIST <- function(data, i) {
  d <- 0
  
  # Check if the DN value in the ith row is NA and handle it
  if (is.na(data$DN[i])) {
    d <- 0
  } else {
    # Only proceed with the comparison if DN[i] is not NA
    if (data$Ball[i] == "Home") {
      
      if (!is.na(data$DN[i]) && data$DN[i] == 1) {
        if (data$DIST[i] <= 5) {
          d <- 1.07
        } else if (data$DIST[i] > 5 && data$DIST[i] <= 10) {
          d <- 0
        } else if (data$DIST[i] > 10) {
          d <- -1.79
        }
      } else if (!is.na(data$DN[i]) && data$DN[i] == 2) {
        if (data$DIST[i] <= 2) {
          d <- 1.79
        } else if (data$DIST[i] > 2 && data$DIST[i] <= 6) {
          d <- 0
        } else if (data$DIST[i] > 6 && data$DIST[i] <= 10) {
          d <- 0
        } else if (data$DIST[i] > 10) {
          d <- -1.61
        }
      } else if (!is.na(data$DN[i]) && data$DN[i] == 3) {
        if (data$DIST[i] <= 2) {
          d <- 1.43
        } else if (data$DIST[i] > 2 && data$DIST[i] <= 6) {
          d <- 1.25
        } else if (data$DIST[i] > 6 && data$DIST[i] <= 10) {
          d <- -1.61
        } else if (data$DIST[i] > 10) {
          d <- -1.96
        }
      }
      
    } else if (data$Ball[i] == "Away") {
      
      if (!is.na(data$DN[i]) && data$DN[i] == 1) {
        if (data$DIST[i] <= 5) {
          d <- -1.07
        } else if (data$DIST[i] > 5 && data$DIST[i] <= 10) {
          d <- 0
        } else if (data$DIST[i] > 10) {
          d <- 1.79
        }
      } else if (!is.na(data$DN[i]) && data$DN[i] == 2) {
        if (data$DIST[i] <= 2) {
          d <- -1.79
        } else if (data$DIST[i] > 2 && data$DIST[i] <= 6) {
          d <- 0
        } else if (data$DIST[i] > 6 && data$DIST[i] <= 10) {
          d <- 0
        } else if (data$DIST[i] > 10) {
          d <- 1.61
        }
      } else if (!is.na(data$DN[i]) && data$DN[i] == 3) {
        if (data$DIST[i] <= 2) {
          d <- -1.43
        } else if (data$DIST[i] > 2 && data$DIST[i] <= 6) {
          d <- -1.25
        } else if (data$DIST[i] > 6 && data$DIST[i] <= 10) {
          d <- 1.61
        } else if (data$DIST[i] > 10) {
          d <- 1.96
        }
      }
    }
  }
  
  return(d)
}

Live.Home.momentum <- function(data, i) {
  x <- Live.mHome.result(data, i)
  y <- Live.mHome.yard(data, i)
  s <- Live.mHome.score(data, i)
  d <- Live.mHome.DIST(data, i)
  
  for (j in 1:nrow(data)) {
    if (data$QTR[j] == 1) {
      s <- s * 0.1
    } else if (data$QTR[j] == 2) {
      s <- s * 0.2
    } else if (data$QTR[j] == 3) {
      s <- s * 0.3
    } else if (data$QTR[j] == 4) {
      s <- s * 0.4
    } else if (data$QTR[j] == 5) {
      s <- s * 0.5
    }
  }
  
  m <- x + y + d + s
  return(m)
}

Live.mainHome.momentum <- function(data) {
  data$MomentumHome <- 0  # Initialize Home Momentum Vector
  
  for (i in 1:nrow(data)) {
    data$MomentumHome[i] <- Live.Home.momentum(data, i)
  }
  
  return(data)
}

Live.situational <- function(data) {
  data$MomentumHome <- 0  
  data$MomentumAway <- 0
  
  for (i in 1:nrow(data)) {
    data$MomentumHome[i] <- Live.Home.momentum(data, i)
  }
  for (i in 1:nrow(data)) {
    data$MomentumAway[i] <- Live.Away.momentum(data, i)
  }
  return(data)
}

Live.AddedMomentum <- function(data) {
  
  data$MomentumTotalHome <- data$MomentumHome[1]
  data$MomentumTotalAway <- data$MomentumAway[1]
  
  for (i in 2:nrow(data)) {
    x <- data$MomentumTotalHome[i-1] + data$MomentumHome[i]
    if (x <= 0) {
      data$MomentumTotalHome[i] <- 0
    } else {
      data$MomentumTotalHome[i] <- x
    }
  }
  
  for (i in 2:nrow(data)) {
    x <- data$MomentumTotalAway[i-1] + data$MomentumAway[i]  
    if (x <= 0) {
      data$MomentumTotalAway[i] <- 0
    } else {
      data$MomentumTotalAway[i] <- x
    }
  }
  
  return(data)
}

Live.MomentumVisuals <- function(data) {
  # Check if there are any quarter_end == 1 in the data
  has_quarter_end <- any(data$quarter_end == 1)
  
  # Define base ggplot object
  p <- ggplot(data = data, aes(x = PLAY..)) +
    geom_line(aes(y = MomentumTotalHome, color = "Home Momentum"), size = 1.2) + 
    geom_line(aes(y = MomentumTotalAway, color = "Away Momentum"), size = 1.2) +
    geom_line(aes(y = ScoreHome2.0, color = "Total Home Score"), alpha = 0.2) +
    geom_line(aes(y = ScoreAway2.0, color = "Total Away Score"), alpha = 0.2) +
    geom_smooth(aes(y = MomentumTotalHome, color = "Home Momentum Trend"), method = "lm", se = FALSE, linetype = "dotted") +
    geom_smooth(aes(y = MomentumTotalAway, color = "Away Momentum Trend"), method = "lm", se = FALSE, linetype = "dotted") +
    labs(x = "Plays Played", y = "Momentum", title = "Momentum Comparison") +
    scale_x_continuous(breaks = seq(0, max(data$PLAY..), by = 10))
  
  # Add geom_vline if there are quarter_end == 1
  if (has_quarter_end) {
    p <- p + geom_vline(data = subset(data, quarter_end == 1), aes(xintercept = PLAY.., color = "Quarter End"), linetype = "dashed", size = 0.4, alpha = 0.8)
  }
  
  # Define color scale and legend
  legend_colors <- c("Home Momentum" = "red", 
                     "Away Momentum" = "blue", 
                     "Total Home Score" = "red", 
                     "Total Away Score" = "blue",
                     "Home Momentum Trend" = "red", 
                     "Away Momentum Trend" = "blue")
  
  if (has_quarter_end) {
    legend_colors["Quarter End"] <- "black"
    legend_alpha <- c(1, 1, 1, 1, 0.8, 0.2, 0.2) # Match alpha values to the legend items
  } else {
    legend_alpha <- c(1, 1, 1, 1, 0.2, 0.2) # Exclude alpha for Quarter End
  }
  
  p <- p + scale_color_manual(name = "Legend", 
                              values = legend_colors, 
                              guide = guide_legend(override.aes = list(alpha = legend_alpha)))
  
  p + theme_minimal()
}



Live.effect_averages <- function(Short_Analysis, trial) {
  # Initialize the effect column
  Short_Analysis$effect <- NA
  
  for (i in 1:nrow(Short_Analysis)) {
    if (Short_Analysis$result[i] == "TD") {
      if (i < nrow(Short_Analysis) && Short_Analysis$result[i+1] == "TD") {
        Short_Analysis$effect[i] <- 0.6
      } else if (i < nrow(Short_Analysis) && Short_Analysis$result[i+1] == "FG") {
        Short_Analysis$effect[i] <- 0.8
      } else {
        Short_Analysis$effect[i] <- 1
      }
    } else if (Short_Analysis$result[i] == "FG Good") {
      if (i < nrow(Short_Analysis) && Short_Analysis$result[i+1] == "TD") {
        Short_Analysis$effect[i] <- 0.6 * 0.833333333
      } else if (i < nrow(Short_Analysis) && Short_Analysis$result[i+1] == "FG") {
        Short_Analysis$effect[i] <- 0.8 * 0.833333333
      } else {
        Short_Analysis$effect[i] <- 0.833333333
      }
    } else if (Short_Analysis$result[i] == "Interception" || Short_Analysis$result[i] == "Fumble") {
      Short_Analysis$effect[i] <- 0.5
    } else if (Short_Analysis$result[i] == "DEFT") {
      Short_Analysis$effect[i] <- 0.166666667
    } else if (Short_Analysis$result[i] == "Punt") {
      Short_Analysis$effect[i] <- 0.666666667
    } else if (Short_Analysis$result[i] == "Other") {
      Short_Analysis$effect[i] <- 1
    } else if (Short_Analysis$result[i] == "FG No Good") {
      Short_Analysis$effect[i] <- 0.333333333
    }
  }
  
  df <- Short_Analysis
  
  effect_list <- list()
  
  for (i in 1:nrow(df)) {
    team <- NA
    effect <- df$effect[i]
    
    if (df$ball[i] == "Home") {
      team <- df$away_team[i]
    } else if (df$ball[i] == "Away") {
      team <- df$home_team[i]
    }
    
    if (!is.na(team)) {
      if (!team %in% names(effect_list)) {
        effect_list[[team]] <- c()
      }
      
      effect_list[[team]] <- c(effect_list[[team]], effect)
    }
  }
  
  
  Live.average_effects <- list()
  
  for (team in names(effect_list)) {
    effects <- effect_list[[team]]
    
    avg_effect <- mean(effects)
    
    average_effects[[team]] <- avg_effect
  }
  
  return(average_effects)
}

Live.adj_Momentum <- function(data, play_by_play_2023) {
  data$adj_MomentumHome <- rep(NA, nrow(data))
  data$adj_MomentumAway <- rep(NA, nrow(data))
  
  helper <- Live.s_analysis(play_by_play_2023)
  average_effects <- Live.effect_averages(helper, play_by_play_2023)
  
  for (i in 1:nrow(data)) {
    a <- data$away_team[i]
    h <- data$home_team[i]
    
    eH <- if (!is.null(average_effects[[h]])) average_effects[[h]] else 1
    eA <- if (!is.null(average_effects[[a]])) average_effects[[a]] else 1
    
    if (data$MomentumHome[i] < 0) {
      data$adj_MomentumHome[i] <- data$MomentumHome[i] * eH
    } else {
      data$adj_MomentumHome[i] <- data$MomentumHome[i]
    }
    
    if (data$MomentumAway[i] < 0) {
      data$adj_MomentumAway[i] <- data$MomentumAway[i] * eA
    } else {
      data$adj_MomentumAway[i] <- data$MomentumAway[i]
    }
  }
  
  return(data)
}

Live.Added_adj_Momentum <- function(data) {
  data$adj_MomentumTotalHome <- data$adj_MomentumHome[1]
  data$adj_MomentumTotalAway <- data$adj_MomentumAway[1]
  
  for (i in 2:nrow(data)) {
    x <- data$adj_MomentumTotalHome[i-1] + data$adj_MomentumHome[i]
    if (x <= 0) {
      data$adj_MomentumTotalHome[i] <- 0
    } else {
      data$adj_MomentumTotalHome[i] <- x
    }
  }
  
  for (i in 2:nrow(data)) {
    x <- data$adj_MomentumTotalAway[i-1] + data$adj_MomentumAway[i]  
    if (x <= 0) {
      data$adj_MomentumTotalAway[i] <- 0
    } else {
      data$adj_MomentumTotalAway[i] <- x
    }
  }
  
  return(data)
}

Live.s_analysis <- function(data) {
  drive <- c(data$drive_count[1])
  home_team <- c(data$home_team[1])
  away_team <- c(data$away_team[1])
  
  for (i in 2:nrow(data)) {
    if (data$drive_count[i] != data$drive_count[i - 1]) {
      drive <- append(drive, data$drive_count[i])
      home_team <- append(home_team, data$home_team[i])
      away_team <- append(away_team, data$away_team[i])
    }
  }
  
  result <- character(length(drive))
  ball <- character(length(drive))
  ShortAnalysis <- data.frame(drive, result, ball, home_team, away_team, stringsAsFactors = FALSE)
  
  for (i in seq_along(drive)) {
    matching_rows <- which(data$drive_count == drive[i])
    
    ShortAnalysis$result[i] <- NA
    
    for (j in matching_rows) {
      play_type <- data$PLAY.TYPE[j]
      play_result <- data$RESULT[j]  # Renamed variable to avoid conflict with result column
      
      if (grepl("Punt", play_type)) {
        if (grepl("DEFT", play_result)) {
          ShortAnalysis$result[i] <- "DEFT"
        } else {
          ShortAnalysis$result[i] <- "Punt"
        }
      } else if (grepl("TD", play_result)) {
        ShortAnalysis$result[i] <- "TD"
      } else if (grepl("DEFT", play_result)) {
        ShortAnalysis$result[i] <- "DEFT"
      } else if (grepl("Fumble", play_result)) {
        if (grepl("DEFT", play_result)) {
          ShortAnalysis$result[i] <- "DEFT"
        } else {
          ShortAnalysis$result[i] <- "Fumble"
        }
      } else if (grepl("Interception", play_result)) {
        if (grepl("DEFT", play_result)) {
          ShortAnalysis$result[i] <- "DEFT"
        } else {
          ShortAnalysis$result[i] <- "Interception"
        }
      } else if (grepl("FG", play_type)) {
        if (play_result == "Good") {
          ShortAnalysis$result[i] <- "FG Good"
        } else if (play_result == "Blocked") {
          ShortAnalysis$result[i] <- "FG Blocked"
        } else if (play_result == "No Good") {
          ShortAnalysis$result[i] <- "FG No Good"
        }
      }
      
      ShortAnalysis$ball[i] <- data$Ball[j]
    }
    
    if (is.na(ShortAnalysis$result[i])) {
      ShortAnalysis$result[i] <- "Other"
    }
  }
  
  return(ShortAnalysis)
}

add <- function(add_Ball, add_DN, add_DIST, add_PLAY.TYPE, add_RESULT,
                add_GN.LS, add_H, add_ODK, add_YARD.LN, 
                add_PERSONNEL, add_BACKFIELD, add_OFF.FORM,
                add_MOTION, add_PROTECTION, add_OFF.PLAY, add_QTR) {
  
  # Calculate the PlayNumber for the new row
  new_PLAY.. <- ifelse(nrow(live_data) == 0, 1, max(live_data$PLAY.., na.rm = TRUE) + 1)
  
  # Create a new data frame with the new data
  add_data <- data.frame(
    PLAY.. = new_PLAY..,
    Ball = add_Ball,
    DN = add_DN,
    DIST = add_DIST,
    PLAY.TYPE = add_PLAY.TYPE,
    RESULT = add_RESULT,
    GN.LS = add_GN.LS,
    H = add_H,
    ODK = add_ODK,
    YARD.LN = add_YARD.LN,
    PERSONNEL = add_PERSONNEL,
    BACKFIELD = add_BACKFIELD,
    OFF.FORM = add_OFF.FORM,
    MOTION = add_MOTION,
    PROTECTION = add_PROTECTION,
    OFF.PLAY = add_OFF.PLAY,
    QTR = add_QTR,
    stringsAsFactors = FALSE
  )
  
  # Add the new row to the existing data
  live_data <<- rbind(live_data, add_data)
}

Live.Test <- function(df) {
  df <- Live.DataClean(df)
  TestResultAway <- Live.mAway.result(df, 2)
  TestYardAway <- Live.mAway.yard(df, 2)
  TestScoreAway <- Live.mAway.score(df, 2)
  TestDISTAway <- Live.mAway.DIST(df, 2)
  
  TestResultHome <- Live.mHome.result(df, 2)
  TestYardHome <- Live.mHome.yard(df, 2)
  TestScoreHome <- Live.mHome.score(df, 2)
  TestDISTHome <- Live.mHome.DIST(df, 2)
  
  Final <- Live.situational(df)
  Final$MomentumHome[is.na(Final$MomentumHome)] <- 0
  Final$MomentumAway[is.na(Final$MomentumAway)] <- 0
  
  AddedMomentumData <- AddedMomentum(Final)
  return(AddedMomentumData)
}

updateQuarterEnd <- function() {
  if (nrow(live_data$data) > 1) {
    live_data$data$quarter_end <- as.integer(c(0, diff(live_data$data$QTR) != 0))
  } else {
    live_data$data$quarter_end <- 0
  }
}

live_data <- data.frame(
  PLAY.. = integer(),
  Ball = integer(),
  DN = integer(),
  DIST = integer(),
  PLAY.TYPE = character(),
  RESULT = character(),
  GN.LS = integer(),
  H = character(),
  ODK = character(),
  YARD.LN = integer(),
  PERSONNEL = character(),
  BACKFIELD = character(),
  OFF.FORM = character(),
  MOTION = character(),
  PROTECTION = character(),
  OFF.PLAY = character(),
  QTR = integer(),
  ScoreAway = integer(),
  ScoreAway2.0 = integer(),
  ScoreHome = integer(),
  ScoreHome2.0 = integer(),
  drive = integer(),
  drive_length = integer(),
  drive_count = integer(),
  stringsAsFactors = FALSE
)

# Create Scout Data sets----
Scout.Away.momentum <- function(data, i) {
  x <- Live.mAway.result(data, i)
  y <- Live.mAway.yard(data, i)
  d <- Live.mAway.DIST(data, i)
  
  m <- x + y + d
  return(m)
}

Scout.Home.momentum <- function(data, i) {
  x <- Live.mHome.result(data, i)
  y <- Live.mHome.yard(data, i)
  d <- Live.mHome.DIST(data, i)
  
  m <- x + y + d
  return(m)
}


Scout.situational <- function(data) {
  data$MomentumHome <- 0  
  data$MomentumAway <- 0
  
  for (i in 1:nrow(data)) {
    data$MomentumHome[i] <- Scout.Home.momentum(data, i)
  }
  for (i in 1:nrow(data)) {
    data$MomentumAway[i] <- Scout.Away.momentum(data, i)
  }
  return(data)
}

addData <- function(data, HomeTeam, AwayTeam) {
  
  data$GN.LS <- ifelse(data$ODK == "O", data$GN.LS * (-1), data$GN.LS)
  data$YARD.LN <- data$YARD.LN * (-1)
  data$ScoreAway <- 0
  data$ScoreAway2.0 <- 0
  data$ScoreHome <- 0
  data$ScoreHome2.0 <- 0
  data$RESULT <- ifelse(grepl("Def TD", data$RESULT), "DEFT", data$RESULT)
  data <- data[!is.na(data$PLAY.TYPE) & data$PLAY.TYPE != "", ]
  data <- subset(data, !grepl("Timeout", RESULT))
  
  data$Ball <- ""
  for (i in 1:nrow(data)) {
    if (i > 1) {
      if (data$ODK[i] == "O") {
        data$Ball[i] <- "Home"
      } else if (data$ODK[i] == "D") {
        data$Ball[i] <- "Away"
      } else if (data$ODK[i] == "K" &&
                 data$Ball[i-1] == "Away" &&
                 data$ODK[i-1] != "K" &&
                 data$RESULT[i-1] != "DEFT") {
        data$Ball[i] <- "Away"
      } else if (data$ODK[i] == "K" &&
                 data$Ball[i-1] == "Home" &&
                 data$ODK[i-1] != "K" &&
                 data$RESULT[i-1] != "DEFT") {
        data$Ball[i] <- "Home"
      } else if (data$ODK[i] == "K" &&
                 data$ODK[i-1] == "K" &&
                 data$Ball[i-1] == "Away" &&
                 data$RESULT[i-1] != "TD") {
        data$Ball[i] <- "Home"
      } else if (data$ODK[i] == "K" &&
                 data$ODK[i-1] == "K" &&
                 data$Ball[i-1] == "Home" &&
                 data$RESULT[i-1] != "TD") {
        data$Ball[i] <- "Away"
      } else if (data$ODK[i] == "K" &&
                 data$RESULT[i-1] == "TD" &&
                 data$ODK[i-1] == "K" &&
                 data$Ball[i-1] == "Home") {
        data$Ball[i] <- "Home"
      } else if (data$ODK[i] == "K" &&
                 data$RESULT[i-1] == "TD" &&
                 data$ODK[i-1] == "K" &&
                 data$Ball[i-1] == "Away") {
        data$Ball[i] <- "Away"
      } else if (data$ODK[i-1] == "D" &&
                 data$RESULT[i-1] == "DEFT" &&
                 data$Ball[i-1] == "Away") {
        data$Ball[i] <- "Home"
      } else if (data$ODK[i-1] == "D" &&
                 data$RESULT[i-1] == "DEFT" &&
                 data$Ball[i-1] == "Home") {
        data$Ball[i] <- "Away"
      } 
    }
  }
  
  for (i in 1:nrow(data)) {
    if (i > 1) {
      if (data$Ball[i] == "Home" &&
          data$RESULT[i] == "DEFT") {
        data$ScoreAway[i] <- data$ScoreAway[i-1] + 6
      } else if (data$Ball[i] == "Away" &&
                 data$RESULT[i] == "DEFT") {
        data$ScoreAway[i] <- data$ScoreAway[i-1] - 6
      } else if (data$Ball[i] == "Home" &&
                 grepl("TD", data$RESULT[i])) {
        data$ScoreAway[i] <- data$ScoreAway[i-1] - 6
      } else if (data$Ball[i] == "Away" &&
                 grepl("TD", data$RESULT[i])) {
        data$ScoreAway[i] <- data$ScoreAway[i-1] + 6
      } else if (data$Ball[i] == "Home" &&
                 grepl("Extra Pt.", data$PLAY.TYPE[i]) &&
                 data$RESULT[i] == "Good") {
        data$ScoreAway[i] <- data$ScoreAway[i-1] - 1
      } else if (data$Ball[i] == "Away" &&
                 grepl("Extra Pt.", data$PLAY.TYPE[i]) &&
                 data$RESULT[i] == "Good") {
        data$ScoreAway[i] <- data$ScoreAway[i-1] + 1
      } else if (data$Ball[i] == "Home" &&
                 grepl("FG", data$PLAY.TYPE[i]) &&
                 data$RESULT[i] == "Good") {
        data$ScoreAway[i] <- data$ScoreAway[i-1] - 3
      } else if (data$Ball[i] == "Away" &&
                 grepl("FG", data$PLAY.TYPE[i]) &&
                 data$RESULT[i] == "Good") {
        data$ScoreAway[i] <- data$ScoreAway[i-1] + 3
      } else {
        data$ScoreAway[i] <- data$ScoreAway[i-1]
      }
      data$ScoreHome[i] <- data$ScoreAway[i] * -1
    }
  }
  
  if (nrow(data) > 0) {
    if (data$Ball[1] == "Home" &&
        grepl("TD", data$RESULT[1])) {
      data$ScoreAway[1] <- -6
    } else if (data$Ball[1] == "Away" &&
               grepl("TD", data$RESULT[1])) {
      data$ScoreAway[1] <- 6
    }
  }
  
  for (i in 1:nrow(data)) {
    if (i > 1) {
      if (data$Ball[i] == "Home" &&
          data$RESULT[i] == "DEFT") {
        data$ScoreAway2.0[i] <- data$ScoreAway2.0[i-1] + 6
      } else if (data$Ball[i] == "Away" &&
                 grepl("TD", data$RESULT[i])) {
        data$ScoreAway2.0[i] <- data$ScoreAway2.0[i-1] + 6
      } else if (data$Ball[i] == "Away" &&
                 grepl("Extra Pt.", data$PLAY.TYPE[i]) && data$RESULT[i] == "Good") {
        data$ScoreAway2.0[i] <- data$ScoreAway2.0[i-1] + 1
      } else if (data$Ball[i] == "Away" &&
                 grepl("FG", data$PLAY.TYPE[i]) &&
                 data$RESULT[i] == "Good") {
        data$ScoreAway2.0[i] <- data$ScoreAway2.0[i-1] + 3
      } else {
        data$ScoreAway2.0[i] <- data$ScoreAway2.0[i-1]
      }
    }
  }
  
  if (nrow(data) > 0) {
    if (data$Ball[1] == "Away" &&
        grepl("TD", data$RESULT[1])) {
      data$ScoreAway2.0[1] <- 6
    }
  }
  for (i in 1:nrow(data)) {
    if (i > 1) {
      if (data$Ball[i] == "Away" &&
          data$RESULT[i] == "DEFT") {
        data$ScoreHome2.0[i] <- data$ScoreHome2.0[i-1] + 6
      } else if (data$Ball[i] == "Home" &&
                 grepl("TD", data$RESULT[i])) {
        data$ScoreHome2.0[i] <- data$ScoreHome2.0[i-1] + 6
      } else if (data$Ball[i] == "Home" &&
                 grepl("Extra Pt.", data$PLAY.TYPE[i]) &&
                 data$RESULT[i] == "Good") {
        data$ScoreHome2.0[i] <- data$ScoreHome2.0[i-1] + 1
      } else if (data$Ball[i] == "Home" &&
                 grepl("FG", data$PLAY.TYPE[i]) &&
                 data$RESULT[i] == "Good") {
        data$ScoreHome2.0[i] <- data$ScoreHome2.0[i-1] + 3
      } else {
        data$ScoreHome2.0[i] <- data$ScoreHome2.0[i-1]
      }
    }
  }
  
  if (nrow(data) > 0) {
    if (data$Ball[1] == "Away" &&
        grepl("TD", data$RESULT[1])) {
      data$ScoreHome2.0[1] <- 6
    }
  }
  
  for (i in 1:nrow(data)) {
    if (data$PLAY.TYPE[i] == "") {
      if (grepl("Rush", data$RESULT[i])) {
        data$PLAY.TYPE[i] <- "Run"
      } else if (grepl("Complete", data$RESULT[i]) || 
                 data$RESULT[i] == "Incomplete" || 
                 grepl("Sack", data$RESULT[i]) ||
                 data$RESULT[i] == "Scramble" ||
                 data$RESULT[i] == "Interception") {
        data$PLAY.TYPE[i] <- "Pass"
      }
    }
  }
  
  data$drive <- 0
  
  for (i in 2:nrow(data)) {
    if (data$Ball[i] != data$Ball[i-1] && data$Ball[i] == data$Ball[i+1]) {
      data$drive[i] <- 1
    }
  }
  
  data$drive_length <- 0
  
  for (i in 1:nrow(data)) {
    if (data$drive[i] == 0) {
      data$drive_length[i] <- ifelse(i == 1, 1, data$drive_length[i-1] + 1)
    } else if (data$drive[i] == 1) {
      data$drive_length[i] <- 1  
    }
  }
  
  drive_count <- numeric(nrow(data))
  count <- 0
  
  for (i in 1:nrow(data)) {
    if (data$drive[i] == 1) {
      count <- count + 1
    }
    drive_count[i] <- count
  }
  
  data$drive_count <- drive_count
  
  data$home_team <- rep(HomeTeam, nrow(data))
  data$away_team <- rep(AwayTeam, nrow(data))
  
  data <-  Live.Test(data)
  return(data)
}

formationAverages <- function(df, team) {
  # Initialize lists to store results for each down
  down_dfs <- list()
  
  # Loop through each down from 1 to 4
  for (down in 1:4) {
    FormationMomentumAverages_list <- list()
    FormationYardGainLossAverages_list <- list()
    
    # Filter dataframe for current down
    down_df <- df[df$DN == down, ]
    
    for (i in 1:nrow(down_df)) {
      formation <- down_df$OFF.FORM[i]
      
      
      # Initialize momentum variable
      momentum <- NA
      
      if (down_df$home_team[i] == team) {
        if (down_df$ODK[i] == "D") {
          momentum <- down_df$MomentumAway[i]
          yard_gain_loss <- down_df$GN.LS[i]
        }
      } else if (down_df$away_team[i] == team) {
        if (down_df$ODK[i] == "O") {
          momentum <- down_df$MomentumHome[i]
          yard_gain_loss <- down_df$GN.LS[i] * -1
        }
      }
      
      # Store momentum and yard gain/loss for formation
      if (!is.na(formation) && !is.na(momentum) && !is.na(yard_gain_loss)) {
        if (!formation %in% names(FormationMomentumAverages_list)) {
          FormationMomentumAverages_list[[formation]] <- numeric()
          FormationYardGainLossAverages_list[[formation]] <- numeric()
        }
        FormationMomentumAverages_list[[formation]] <- c(FormationMomentumAverages_list[[formation]], momentum)
        FormationYardGainLossAverages_list[[formation]] <- c(FormationYardGainLossAverages_list[[formation]], yard_gain_loss)
      }
    }
    
    # Calculate statistics for current down
    FormationMomentumAverages <- sapply(FormationMomentumAverages_list, function(x) mean(x, na.rm = TRUE))
    FormationMomentumSD <- sapply(FormationMomentumAverages_list, function(x) sd(x, na.rm = TRUE))
    FormationYardGainLossAverages <- sapply(FormationYardGainLossAverages_list, function(x) mean(x, na.rm = TRUE))
    FormationOccurrences <- sapply(FormationMomentumAverages_list, length)
    
    # Convert to data frame and sort by Average Momentum in descending order
    df_averages <- data.frame(
      AverageMomentum = FormationMomentumAverages,
      StandardDeviation = FormationMomentumSD,
      AverageYardGainLoss = FormationYardGainLossAverages,
      Occurrences = FormationOccurrences
    )
    df_averages <- df_averages[order(-df_averages$AverageMomentum), ]
    
    # Store dataframe in the list, named by the down
    down_dfs[[as.character(down)]] <- df_averages
  }
  
  return(down_dfs)
}

playAverages <- function(df, team) {
  # Initialize lists to store results for each down
  down_dfs <- list()
  
  # Loop through each down from 1 to 4
  for (down in 1:4) {
    PlayMomentumAverages_list <- list()
    PlayYardGainLossAverages_list <- list()
    
    # Filter dataframe for current down
    down_df <- df[df$DN == down, ]
    
    for (i in 1:nrow(down_df)) {
      play <- down_df$OFF.PLAY[i]
      
      # Initialize momentum and yard_gain_loss variables
      momentum <- NA
      yard_gain_loss <- NA
      
      if (down_df$home_team[i] == team) {
        if (down_df$ODK[i] == "D") {
          momentum <- down_df$MomentumAway[i]
          yard_gain_loss <- down_df$GN.LS[i]
        }
      } else if (down_df$away_team[i] == team) {
        if (down_df$ODK[i] == "O") {
          momentum <- down_df$MomentumHome[i]
          yard_gain_loss <- down_df$GN.LS[i] * -1
        }
      }
      
      # Store momentum and yard gain/loss for play
      if (!is.na(play) && !is.na(momentum) && !is.na(yard_gain_loss)) {
        if (!play %in% names(PlayMomentumAverages_list)) {
          PlayMomentumAverages_list[[play]] <- numeric()
          PlayYardGainLossAverages_list[[play]] <- numeric()
        }
        PlayMomentumAverages_list[[play]] <- c(PlayMomentumAverages_list[[play]], momentum)
        PlayYardGainLossAverages_list[[play]] <- c(PlayYardGainLossAverages_list[[play]], yard_gain_loss)
      }
    }
    
    # Calculate statistics for current down
    PlayMomentumAverages <- sapply(PlayMomentumAverages_list, function(x) mean(x, na.rm = TRUE))
    PlayMomentumSD <- sapply(PlayMomentumAverages_list, function(x) sd(x, na.rm = TRUE))
    PlayYardGainLossAverages <- sapply(PlayYardGainLossAverages_list, function(x) mean(x, na.rm = TRUE))
    PlayOccurrences <- sapply(PlayMomentumAverages_list, length)
    
    # Convert to data frame and sort by AverageMomentum in descending order
    df_averages <- data.frame(
      Play = names(PlayMomentumAverages),
      AverageMomentum = PlayMomentumAverages,
      StandardDeviation = PlayMomentumSD,
      AverageYardGainLoss = PlayYardGainLossAverages,
      Occurrences = PlayOccurrences
    )
    df_averages <- df_averages[order(-df_averages$AverageMomentum), ]
    
    # Store dataframe in the list, named by the down
    down_dfs[[as.character(down)]] <- df_averages
  }
  
  return(down_dfs)
}



#Dickinson functions------------------------------------------------------------
Dickinson.Test <- function(data, ID) {
  play_by_play <- data
  df <- data[data$game_ID %in% c(ID), ]
  df <- Live.DataClean(df)
  
  TestResultAway <- Live.mAway.result(df, 2)
  TestYardAway <- Live.mAway.yard(df, 2)
  TestScoreAway <- Live.mAway.score(df, 2)
  TestDISTAway <- Live.mAway.DIST(df, 2)
  
  TestResultHome <- Live.mHome.result(df, 2)
  TestYardHome <- Live.mHome.yard(df, 2)
  TestScoreHome <- Live.mHome.score(df, 2)
  TestDISTHome <- Live.mHome.DIST(df, 2)
  
  Final <- Live.situational(df)
  Final$MomentumHome[is.na(Final$MomentumHome)] <- 0
  Final$MomentumAway[is.na(Final$MomentumAway)] <- 0
  
  AddedMomentumData <- AddedMomentum(Final)
  return(AddedMomentumData)
}

#Add Data functions-------------------------------------------------------------
select_columns <- function(data) {
  desired_columns <- c("PLAY..", "DN", "DIST", "PLAY.TYPE", "RESULT", "GN.LS", "H",
                       "ODK", "YARD.LN", "OFF.FORM", "OFF.PLAY", "DEF.FRONT",
                       "BLITZ", "COVERAGE", "QTR", "PERSONNEL", "MOTION",
                       "COMMENT", "TITLE", "CHECK", "SITUATION", "BACKFIELD", "PROTECTION")
  
  
  data <- data[, intersect(desired_columns, names(data)), drop = FALSE]
  
  missing_columns <- setdiff(desired_columns, names(data))
  if (length(missing_columns) > 0) {
    for (col in missing_columns) {
      data[[col]] <- NA
    }
  }
  
  return(data)
}

add.Data <- function(data, HomeTeam, AwayTeam, yearInput) {
  
  C1 <- select_columns(data)
  C2 <- Live.PlayType(C1)
  C3 <- Live.update_dataset(C2)
  C4 <- Live.BallColumn(C3)
  C5 <- Live.ScoreColumn(C4)
  C6 <- Live.ScoreColumn2.0(C5)
  C7 <- Live.drive(C6)
  C7 <- select(C7, -quarter_end)
  C7$year <- yearInput
  C7$home_team <- rep(HomeTeam, nrow(C7))
  C7$away_team <- rep(AwayTeam, nrow(C7))
  C7$game_ID <- paste0(C7$home_team, C7$away_team, C7$year)
  #df <- C7
  df <- rbind(dickinson_2023, C7)
  return(df)
}


#Final Calls--------------------------------------------------------------------
setwd("~/Downloads/Sidehustle/Momentum/App Directory/playbyplay")


