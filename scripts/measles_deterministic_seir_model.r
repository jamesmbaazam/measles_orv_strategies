# require(shiny)
require(deSolve)

######################################################
# SEIR model
######################################################
################
# Parameters
# B = transmission rate
# 1/r = latent period
# 1/g = infectious period
# q = vaccine efficacy
#
# P = target coverage
# Dt = length of vaccination campaign
# vax_day = Day of campaign start
# USAGE:
# times<-1:100
# xstrt<-c(S=.999,E=0,I=.001,R=0,K=0)
# par<-c(B=.5, r=1/7, g = 1/7, q = .8, P = 0, Dt = 10, vax_day = 80)
# out<-as.data.frame(lsoda(xstrt,times,simod,par))
# plot(out$time,out$I,type="l")
#
#
# par<-c(B=.5, r=1/7, g = 1/7, q = .8, P = .99, Dt = 10, vax_day = 50)
# out<-as.data.frame(lsoda(xstrt,times,simod,par))
# lines(out$time,out$I,col="red")
simod <- function(t, x, parms, browse = F) {
  if(browse) browser()
  S <- x[1]
  E <- x[2]
  I <- x[3]
  R <- x[4]
  K <- x[5]
  #
  with(as.list(parms), {
    Q <- ifelse(t < vax_day | t > vax_day + Dt, 0, (-log(1 - P) / Dt))
    dS <- -B * S * I - q * Q * S
    dE <- B * S * I - r * E
    dI <- r * E - g * I
    dR <- g * I + q * Q * S
    dK <- r * E
    res <- c(dS, dE, dI, dR, dK)
    list(res)
  })
}




#' run_orv_model
#'
#' @param strategy 
#' @param location_id 
#' @param mt_equip_type 
#' @param target_pop_size 
#' @param infectious_period 
#' @param vax_efficacy 
#' @param scenario_coverage 
#' @param vax_day 
#' @param scenario_campaign_duration 
#' @param R0 
#' @param browse 
#' @param latent_period 
#' @param I0 
#' @param max_time 
#'
#' @return
#' @export
#'
#' @examples
run_orv_model <- function(strategy, 
                          location_id, 
                          mt_equip_type, 
                          target_pop_size,
                          latent_period,
                          infectious_period,
                          I0,
                          R0,
                          vax_efficacy,
                          scenario_coverage,
                          vax_day,
                          scenario_campaign_duration,
                          max_time,
                          browse = F){
    
  if(browse) browser()
  
  #initial popualtion
  pop_init <- c(S = target_pop_size -I0, 
                E = 0, 
                I = I0, 
                R = 0, 
                K = 0
                )
  beta <- R0/infectious_period
  
  model_time <- 1:max_time
  
  
  orv_params <- c(B = beta, 
                  r = 1/latent_period, 
                  g = 1/infectious_period, 
                  q = vax_efficacy, 
                  P = scenario_coverage, 
                  Dt = scenario_campaign_duration, 
                  vax_day = vax_day
                  )
  
  results_df <- as.data.frame(lsoda(pop_init, times = model_time, simod, orv_params))
  
  scenario_table <- data.frame(strategy = rep(strategy, times = nrow(results_df)), 
                               location_id = rep(location_id, times = nrow(results_df)),
                               mt_equip_type = rep(mt_equip_type, times = nrow(results_df))
                               )
  
  sim_results <- cbind(scenario_table, results_df)

  return(sim_results)
  }

######################################################

#####################################################
# % Intervention
#####################################################
p_red <- function(R, vaccine_efficacy, target_vaccination, intervention_length, mtime = 120, LP = 7, IP = 7, N = 10000, step = 1) {
  steps <- (0:mtime)[seq(1, mtime, by = step)]
  p_red <- rep(NA, length(steps))
  xstrt <- c(S = 1 - 1 / N, E = 0, I = 1 / N, R = 0, K = 0) # starting values
  beta <- R / IP # transmission rate
  t <- 1
  for (i in 1:length(steps)) {
    par <- c(
      B = beta, r = 1 / LP, g = 1 / IP, q = vaccine_efficacy,
      P = target_vaccination, Dt = intervention_length, vax_day = steps[i]
    )
    out <- as.data.frame(lsoda(xstrt, steps, simod, par))
    p_red[t] <- out$K[dim(out)[1]]
    t <- t + 1
    cat("step ", i, "of ", floor(mtime / step), ".\r")
  }
  par <- c(
    B = beta, r = 1 / LP, g = 1 / IP, q = vaccine_efficacy,
    P = 0, Dt = 0, vax_day = Inf
  )
  outv <- as.data.frame(lsoda(xstrt, steps, simod, par))
  # fs should really be the prediction with steps=Inf?
  fs <- max(out$K)
  res <- list(
    out = cbind(steps, p_red / max(p_red)),
    R = R,
    vaccine_efficacy = vaccine_efficacy,
    target_vaccination = target_vaccination,
    intervention_length = intervention_length,
    mtime = mtime, LP = LP, IP = IP, N = N, step = step,
    virgin = outv$I, vfs = fs
  )
  class(res) <- "p_red"
  return(res)
}
######################################################

#####################################################
# plotting pred objects
#####################################################
plot.p_red <- function(object) {
  plot(object$out[, 1], object$out[, 2], type = "l", xlab = "First intervention day", ylab = "% final epidemic", ylim = c(0, 1))
  title(paste("target= ", round(100 * object$target_vaccination, 0), "% campaign = ", object$intervention_length, "d"))
  par(new = TRUE)
  plot(object$out[, 1], object$virgin, col = "red", axes = FALSE, xlab = "", ylab = "", type = "l")
  legend(x = "topleft", legend = c("natural epidemic", "%final size"), col = c("red", "black"), lty = c(1, 1))
}

# EX
# out<-p_red(R=4,vaccine_efficacy=.9,target_vaccination=.5,intervention_length=14, step=2)
# plot(out)

#####################################################
# % Sensitivity analysis on R
#####################################################
R_compare <- function(R = c(2, 4, 8), vaccine_efficacy = .9, target_vaccination, intervention_length, mtime = 120, LP = 7, IP = 7, N = 10000, step = 7) {
  out <- numeric(0)
  for (j in 1:length(R)) {
    tmp <- p_red(
      R = R[j], vaccine_efficacy = vaccine_efficacy,
      target_vaccination, intervention_length, mtime = mtime, LP = LP, IP = IP, N = N, step = step
    )
    out <- cbind(out, tmp$out[, 2])
  }
  res <- list(
    R = R, p_red = out, vax_day = tmp$out[, 1],
    vaccine_efficacy = vaccine_efficacy,
    target_vaccination = target_vaccination,
    intervention_length = intervention_length,
    mtime = mtime, LP = LP, IP = IP, N = N, step = step
  )
  class(res) <- "Rcomp"
  return(res)
}

#####################################################
# plotting pred objects
#####################################################
plot.Rcomp <- function(object) {
  plot(NA, xlim = range(object$vax_day), ylim = c(0, 1), xlab = "First intervention day", ylab = "%, final epidemic")
  title(paste("% final size: target= ", round(100 * object$target_vaccination, 0), "% campaign = ", object$intervention_length, "d"))
  for (j in 1:length(object$R)) {
    lines(object$vax_day, object$p_red[, j], lty = j)
  }
  legend(x = "right", legend = c(object$R), lty = 1:length(object$R), title = "R=")
}

# EX
# res<-R_compare(R=c(1.5, 2.5, 3.5), vaccine_efficacy=.9,target_vaccination=.5,intervention_length=7)
# plot(res)

#####################################################
# % Sensitivity analysis on length of intervention
#####################################################
Int_compare <- function(R, vaccine_efficacy, target_vaccination, intervention_length = c(7, 10, 14), mtime = 120, LP = 7, IP = 7, N = 10000, step = 7) {
  out <- numeric(0)
  for (j in 1:length(intervention_length)) {
    tmp <- p_red(
      R = R, vaccine_efficacy = vaccine_efficacy,
      target_vaccination, intervention_length[j], mtime = mtime, LP = LP, IP = IP, N = N, step = step
    )
    out <- cbind(out, tmp$out[, 2])
  }
  res <- list(
    R = R, p_red = out, vax_day = tmp$out[, 1],
    vaccine_efficacy = vaccine_efficacy,
    target_vaccination = target_vaccination,
    intervention_length = intervention_length,
    mtime = mtime, LP = LP, IP = IP, N = N, step = step
  )
  class(res) <- "Intcomp"
  return(res)
}

#####################################################
# plotting Intcomp objects
#####################################################
plot.Intcomp <- function(object) {
  plot(NA, xlim = range(object$vax_day), ylim = c(0, 1), xlab = "First intervention day", ylab = "% final epidemic")
  title(paste("% final size: target= ", round(100 * object$target_vaccination, 0), "%, R = ", object$R))
  for (j in 1:length(object$intervention_length)) {
    lines(object$vax_day, object$p_red[, j], lty = j)
  }
  legend(x = "right", legend = c(object$intervention_length), lty = 1:length(object$intervention_length), title = "Campaign D:")
}

# EX
# res<-Int_compare(intervention_length=c(7,10,14),R=3, vaccine_efficacy=.9, target_vaccination=.5)
# plot(res)

#####################################################
# % Sensitivity analysis on target Vaccination
#####################################################
Vacc_compare <- function(R, vaccine_efficacy, target_vaccination = c(.50, .70, .90), intervention_length = 7, mtime = 120, LP = 7, IP = 7, N = 10000, step = 7) {
  out <- numeric(0)
  for (j in 1:length(target_vaccination)) {
    tmp <- p_red(
      R = R, vaccine_efficacy = vaccine_efficacy,
      target_vaccination[j], intervention_length, mtime = mtime, LP = LP, IP = IP, N = N, step = step
    )
    out <- cbind(out, tmp$out[, 2])
  }
  res <- list(
    R = R, p_red = out, vax_day = tmp$out[, 1],
    vaccine_efficacy = vaccine_efficacy,
    target_vaccination = target_vaccination,
    intervention_length = intervention_length,
    mtime = mtime, LP = LP, IP = IP, N = N, step = step
  )
  class(res) <- "Vacccomp"
  return(res)
}

#####################################################
# plotting Vaccomp objects
#####################################################
plot.Vacccomp <- function(object) {
  plot(NA, xlim = range(object$vax_day), ylim = c(0, 1), xlab = "First intervention day", ylab = "% final epidemic")
  title(paste("% final size: R = ", object$R))
  for (j in 1:length(object$target_vaccination)) {
    lines(object$vax_day, object$p_red[, j], lty = j)
  }
  legend(x = "right", legend = c(round(100 * object$target_vaccination, 0)), lty = 1:length(object$target_vaccination), title = "Target %")
}

# EX
# res<-Vacc_compare(target_vaccination=c(.50,.70,.90), R=4, vaccine_efficacy=.9,intervention_length=7)
# plot(res)

#####################################################
# Mortality analysis on R
#####################################################
M_red <- function(object, case_fatality) {
  par(mfrow = c(2, 1))
  plot(object)
  out <- (1 - object$p_red) * object$N * case_fatality
  plot(NA, xlim = c(1, max(object$vax_day)), ylim = c(0, max(out)), xlab = "First intervention day", ylab = "Extra survivors")
  for (j in 1:dim(out)[2]) {
    lines(object$vax_day, out[, j], lty = j)
  }
  title(paste("Reduced burden of mortality; N = ", object$N))
  par(mfrow = c(1, 1))
}

# EX
# res<-Vacc_compare(target_vaccination=c(.50,.70,.90), R=4, vaccine_efficacy=.9,intervention_length=7)
# M_red(res, 0.5)


#####################################################
# Retrospective
#####################################################
retro <- function(R, day, vaccine_efficacy, target_vaccination, intervention_length, mtime = 120, LP = 7, IP = 7, N = 10000) {
  steps <- 1:mtime
  out <- matrix(NA, nrow = mtime, ncol = 3)
  xstrt <- c(S = 1 - 1 / N, E = 0, I = 1 / N, R = 0, K = 0) # starting values
  beta <- R / IP # transmission rate
  t <- 1
  par <- c(
    B = beta, r = 1 / LP, g = 1 / IP, q = vaccine_efficacy,
    P = 0, Dt = 0, vax_day = Inf
  )
  outv <- as.data.frame(lsoda(xstrt, steps, simod, par))
  # fsv and fsi should really be with steps=Inf?
  fsv <- max(outv$K)

  par <- c(
    B = beta, r = 1 / LP, g = 1 / IP, q = vaccine_efficacy,
    P = target_vaccination, Dt = intervention_length, vax_day = day
  )

  outi <- as.data.frame(lsoda(xstrt, steps, simod, par))

  fsi <- max(outi$K)
  out[, 1] <- steps
  out[, 2] <- outv$I
  out[, 3] <- outi$I
  res <- list(
    out = out,
    redn = fsi / fsv,
    R = R,
    vaccine_efficacy = vaccine_efficacy,
    target_vaccination = target_vaccination,
    intervention_length = intervention_length,
    mtime = mtime, LP = LP, IP = IP, N = N, day = day
  )
  class(res) <- "retro"
  return(res)
}
######################################################

#####################################################
# plotting pred objects
#####################################################
plot.retro <- function(object) {
  plot(object$out[, 1], object$out[, 2], type = "l", ylim = c(0, max(object$out[, 2])), xlab = "day", ylab = "prevalence")
  polygon(c(
    object$day, object$day, object$day + object$intervention_length,
    object$day + object$intervention_length
  ), c(-0.1, 1, 1, -.1), col = "gray")
  lines(object$out[, 1], object$out[, 2])
  lines(object$out[, 1], object$out[, 3], col = "red")
  title(paste("final size: ", round(100 * (object$redn), 1), "% (R=",
    object$R, ", target=", 100 * object$target_vaccination, "%)",
    sep = ""
  ))
  legend(
    x = "topright", legend = c("natural epidemic", "w intervention"),
    col = c("black", "red"), lty = c(1, 1)
  )
  text(
    x = object$day + object$intervention_length, y = 0, pos = 4,
    labels = paste(
      object$intervention_length,
      "d intervention from", object$day
    )
  )
}


# EX
# out<-retro(R=4, day=50, vaccine_efficacy=.9,target_vaccination=.5,intervention_length=14)
# plot(out)


# ui=navbarPage("ORV",
#               tabPanel("Intervention day",
#                        sidebarLayout(
#                          sidebarPanel(
#                            sliderInput("vaccine_target", "target:", 0.7,
#                                        min = 0, max = 1),
#                            sliderInput("vaccine_efficacy", "efficacy:", 0.9,
#                                        min = 0, max = 1),
#                            numericInput("intervention_length", "duration:", 14,
#                                         min = 1, max = 100),
#                            numericInput("N", "N:", 1E5,
#                                         min = 1E2, max = 1E6),
#                            numericInput("mtime", "endtime:", 120,
#                                         min = 10, max = 3*365),
#                            sliderInput("R", "R",
#                                        min = 1, max = 20, value=4),
#                            sliderInput("IP", "Infectious period (days)", 5,
#                                        min = 1, max = 100),
#                            sliderInput("LP", "Latent period (days):", 8,
#                                        min = 1, max = 100)),
#                          mainPanel(plotOutput("plot1"))
#                        )),
#               tabPanel("R sensitivty",
#                        sidebarLayout(
#                          sidebarPanel(
#                            sliderInput("R", "R",
#                                        min = 1, max = 20, value=4),
#                            numericInput("pm", "+/-:", 0.5,
#                                         min = 1, max = 10),
#                            sliderInput("vaccine_target", "target:", 0.7,
#                                        min = 0, max = 1),
#                            sliderInput("vaccine_efficacy", "efficacy:", 0.9,
#                                        min = 0, max = 1),
#                            numericInput("intervention_length", "duration:", 14,
#                                         min = 1, max = 100),
#                            numericInput("N", "N:", 1E5,
#                                         min = 1E2, max = 1E6),
#                            numericInput("mtime", "endtime:", 120,
#                                         min = 10, max = 3*365),
#                            sliderInput("IP", "Infectious period (days)", 5,
#                                        min = 1, max = 100),
#                            sliderInput("LP", "Latent period (days):", 8,
#                                        min = 1, max = 100)),
#                          mainPanel(plotOutput("plot2"))
#                        )),
#               tabPanel("Duration sensitivity",
#                        sidebarLayout(
#                          sidebarPanel(
#                            numericInput("intervention_length", "duration:", 14,
#                                         min = 1, max = 100),
#                            numericInput("pm2", "+/-:", 7,
#                                         min = 1, max = 21),
#                            sliderInput("vaccine_target", "target:", 0.7,
#                                        min = 0, max = 1),
#                            sliderInput("vaccine_efficacy", "efficacy:", 0.9,
#                                        min = 0, max = 1),
#                            sliderInput("R", "R",
#                                        min = 1, max = 20, value=4),
#                            numericInput("N", "N:", 1E5,
#                                         min = 1E2, max = 1E6),
#                            numericInput("mtime", "endtime:", 120,
#                                         min = 10, max = 3*365),
#                            sliderInput("IP", "Infectious period (days)", 5,
#                                        min = 1, max = 100),
#                            sliderInput("LP", "Latent period (days):", 8,
#                                        min = 1, max = 100)),
#                          mainPanel(plotOutput("plot3"))
#                        )),
#               tabPanel("Cover sensitivty",
#                        sidebarLayout(
#                          sidebarPanel(
#                            sliderInput("vaccine_target", "target:", 0.7,
#                                        min = 0, max = 1),
#                            numericInput("pm3", "+/-:", 0.1,
#                                         min = 0, max = .9),
#                            sliderInput("vaccine_efficacy", "efficacy:", 0.9,
#                                        min = 0, max = 1),
#                            numericInput("intervention_length", "duration:", 14,
#                                         min = 1, max = 100),
#                            sliderInput("R", "R",
#                                        min = 1, max = 20, value=4),
#                            numericInput("N", "N:", 1E5,
#                                         min = 1E2, max = 1E6),
#                            numericInput("mtime", "endtime:", 120,
#                                         min = 10, max = 3*365),
#                            sliderInput("IP", "Infectious period (days)", 5,
#                                        min = 1, max = 100),
#                            sliderInput("LP", "Latent period (days):", 8,
#                                        min = 1, max = 100)),
#                          mainPanel(plotOutput("plot4"))
#                        )),
#               tabPanel("Retrospective analysis",
#                        sidebarLayout(
#                          sidebarPanel(
#                            numericInput("day", "Start day:", 60,
#                                         min = 10, max = 3*365),
#                            sliderInput("vaccine_target", "target:", 0.7,
#                                        min = 0, max = 1),
#                            sliderInput("vaccine_efficacy", "efficacy:", 0.9,
#                                        min = 0, max = 1),
#                            numericInput("intervention_length", "duration:", 14,
#                                         min = 1, max = 100),
#                            sliderInput("R", "R",
#                                        min = 1, max = 20, value=4),
#                            numericInput("N", "N:", 1E5,
#                                         min = 1E2, max = 1E6),
#                            numericInput("mtime", "endtime:", 120,
#                                         min = 10, max = 3*365),
#                            sliderInput("IP", "Infectious period (days)", 5,
#                                        min = 1, max = 100),
#                            sliderInput("LP", "Latent period (days):", 8,
#                                        min = 1, max = 100)),
#                          mainPanel(plotOutput("plot5"))
#                        )),
#               tabPanel("Summary")
# )
#
# server=function(input, output){
#   output$plot1 <- renderPlot({
#     out<-p_red(R=input$R,input$vaccine_efficacy,input$vaccine_target,input$intervention_length, input$mtime, input$LP, input$IP, input$N, step=1)
#     plot(out)}
#   )
#
#   output$plot2 <- renderPlot({
#     R2=c(input$R-input$pm, input$R, input$R+input$pm)
#     R2[R2<0]=0
#     out2=R_compare(R=R2, input$vaccine_efficacy,input$vaccine_target,input$intervention_length, input$mtime, input$LP, input$IP, input$N, step=1)
#     plot(out2)}
#   )
#
#   output$plot3 <- renderPlot({
#     il=c(input$intervention_length-input$pm2,
#          input$intervention_length, input$intervention_length+input$pm2)
#     il[il<0]=0
#     out3=Int_compare(R=input$R, input$vaccine_efficacy,input$vaccine_target,il,  input$mtime, input$LP, input$IP, input$N, step=1)
#     plot(out3)}
#   )
#
#   output$plot4 <- renderPlot({
#     vt=c(input$vaccine_target-input$pm3,
#          input$vaccine_target, input$vaccine_target+input$pm3)
#     vt[vt<0]=0
#     vt[vt>1]=1
#     out4=Vacc_compare(R=input$R, input$vaccine_efficacy,vt,input$intervention_length,  input$mtime, input$LP, input$IP, input$N, step=1)
#     plot(out4)}
#   )
#
#   output$plot5 <- renderPlot({
#     out5<-retro(R=input$R, day=input$day, input$vaccine_efficacy,input$vaccine_target,input$intervention_length, input$mtime, input$LP, input$IP, input$N)
#     plot(out5)}
#   )
#
# }


# shinyApp(ui, server)