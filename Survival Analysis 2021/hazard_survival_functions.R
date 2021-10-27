######################################################
### Relation between Hazard and Survival Functions ###
######################################################


### Define hazard function h1, h2, h3 as you'd like
### All that's needed is they're positive for all times t

h1 <- function(t)  .10
h2 <- function(t)  pmax( (t-5)^2/200 + .2, 0)
h3 <- function(t)  .1*abs(cos(t)) + .05

### Define hazard function h1_bis, h2_bis, h3_bis proportional to h1, h2, h3
h1_bis <- function(t)  h1(t) * 3
h2_bis <- function(t)  h2(t) * 2
h3_bis <- function(t)  h3(t) * 3

### OR define hazard function h1_bis, h2_bis, h3_bis NON proportional to h1, h2, h3

h1_bis <- function(t)  t * .3
h2_bis <- function(t)  pmax( -1 * (t-5)^2/200 + 1, 0)
h3_bis <- function(t)  h3(t) + .05

### Run the rest down here and have fun! ####

prec <- 100
t_s <- seq(0, 10, length.out=prec)

surv_from_hazard <- function(f, t) {
  exp(-1*integrate(Vectorize(f), 0, t)$value)
}


par(xpd=FALSE)
jamacol <- c("#79AF97FF", "#B24745FF", "#00A1D5FF")

funs <- list(c(h1, h1_bis), c(h2, h2_bis), c(h3, h3_bis))

wl <- 5
#dev.new(width=length(funs)*wl/2, height=wl, noRStudioGD = TRUE)
par(mfcol=c( 2, length(funs) ) )

for (i in 1:length(funs)) {
  
if(i==1){ 
funlab1 <- "Hazard"
funlab2 <- "Survival"
} else {
funlab1 <- ""
funlab2 <- ""}

comp_h1 <- sapply(t_s, funs[[i]][1][[1]])
comp_h2 <- sapply(t_s, funs[[i]][2][[1]])

delta_h <- abs(min(c(comp_h1, comp_h2)) - max(c(comp_h1, comp_h2)))
ylim_min <- min(c(comp_h1, comp_h2)) - .2 *delta_h
ylim_max <- max(c(comp_h1, comp_h2)) + .2 *delta_h
    
plot(t_s, comp_h1, type='l', las=1, xlab="", ylab=funlab1,
     lwd=2,
     col=jamacol[1],
     ylim=c(ylim_min, ylim_max))

lines(t_s, comp_h2, 
      lwd=2,
      col=jamacol[2])

plot(t_s, sapply(t_s, surv_from_hazard, f=funs[[i]][1][[1]]), 
     type='l', las=1, xlab="Time (Months)", ylab=funlab2, 
     ylim=c(0,1),
     lwd=2,
     col=jamacol[1])

lines(t_s, sapply(t_s, surv_from_hazard, f=funs[[i]][2][[1]]),
      lwd=2,
      col=jamacol[2])

}

#dev.copy2pdf(file="hazard_non_prop.pdf")


