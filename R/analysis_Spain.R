#########################################
##   Correcting for Under-Reporting    ##
##           COVID-19 Model            ##
#########################################
# To further reduce memory usage, you may need to reduce the number of chains
# and increase the thinning level in the model.

library(ggplot2) # For reproducing plots seen in the paper.
library(nimble) # For MCMC computation using NIMBLE.
library(coda) # For manipulation of MCMC results.
library(mgcv)
library(dplyr)
library(ngspatial)
library(sp) # For plotting the micro-regions.
library(spdep) # For computing the neighbourhood and adjancency objects.
library(maps) # For adding a map to the plots of Spain.
library(viridis)
library(mapproj)
library(gdata)
library(rgdal)
library(WriteXLS)
library(tidyverse)
# Load in some functions.
source("Functions.R")

vp=viridis_pal()(20) # Colour pallette for plots.

seed <- 794637 # Seed for R and NIMBLE, 794637 is used for results in the article.

set.seed(seed)

# Part One: Setup
shapefile_ccaa <- readOGR(dsn = "Data", layer="Comunidades_Autonomas_ETRS89_30N")
shapefile_ccaa <- shapefile_ccaa[!shapefile_ccaa$Codigo %in% c("04", "05", "18", "19"),]
cases <- read.xls("Data/cases_2020-07-24.xls", fileEncoding="latin1")
cases$Date <- as.Date(cases$Date, format="%Y-%m-%d")

#### Conducted tests
tests <- read.table("Data/ccaa_covid19_test_realizados.txt", sep=",", header=T, dec=",")
tests <- tests[, c(1, 2, 3, 5, 7)]
tests <- tests[!tests$CCAA %in% c("Illes Balears", "Canarias", "Ceuta", "Melilla", "España"), ]
tests$Fecha <- as.Date(tests$Fecha, format="%Y-%m-%d")
tests_begin <- data.frame(Fecha=rep(seq.Date(cases$Date[1], tests$Fecha[1]-1,1), length(table(tests$CCAA))), 
                          CCAA=rep(names(table(tests$CCAA)), as.numeric(tests$Fecha[1]-cases$Date[1])))
#tests_end <- data.frame(Fecha=rep(seq.Date(tests$Fecha[length(tests$Fecha)]+1, cases$Date[length(cases$Date)],1), 
#                        length(table(tests$CCAA))), 
#                          CCAA=rep(names(table(tests$CCAA)), as.numeric(cases$Date[length(cases$Date)]-tests$Fecha[length(tests$Fecha)])))
tests_begin$PCR_x_1000hab. <- 0
tests_begin$TEST_ACC_x_1000hab. <- 0
#tests_end$PCR_x_1000hab. <- 0
#tests_end$TEST_Acc <- 0
tests <- tests[order(tests$Fecha, tests$cod_ine), ]
tests_begin$cod_ine <- NA
#tests_end$cod_ine <- NA
tests$CCAA <- factor(tests$CCAA)
tests_begin <- tests_begin[!tests_begin$CCAA %in% c("Illes Balears", "Canarias", "Ceuta", "Melilla", "España"),]
tests_begin$CCAA <- factor(tests_begin$CCAA)
#tests_end <- tests_end[!tests_end$CCAA %in% c("Illes Balears", "Canarias", "Ceuta", "Melilla", "España"),]
#tests_end$CCAA <- factor(tests_end$CCAA)
for (i in 1:dim(tests_begin)[1])
{
  CA <- tests_begin$CCAA[i]
  tests_begin$cod_ine[i] <- unique(tests$cod_ine[tests$CCAA==CA]) 
}
#for (i in 1:dim(tests_end)[1])
#{
#  CA <- tests_end$CCAA[i]
#  tests_end$cod_ine[i] <- unique(tests$cod_ine[tests$CCAA==CA]) 
#}
tests_begin <- tests_begin[order(tests_begin$Fecha, tests_begin$cod_ine), ]
tests_begin <- tests_begin[, c(1, 5, 2, 3, 4)]
#tests_end <- tests_end[order(tests_end$Fecha, tests_end$cod_ine), ]
#tests_end <- tests_end[, c(1, 5, 2, 3, 4)]
#tests2 <- rbind(tests_begin, tests, tests_end)
tests2 <- rbind(tests_begin, tests)
tests <- tests2[order(tests2$Fecha, tests2$cod_ine), ]
#rm(tests_begin, tests2)
tests2 <- complete(tests, Fecha = seq.Date(tests$Fecha[1], tests$Fecha[length(tests$Fecha)], by=1), CCAA=tests$CCAA)
tests2 <- tests2 %>% 
  group_by(CCAA, cod_ine) %>% 
  fill(cod_ine, PCR_x_1000hab., TEST_ACC_x_1000hab.)
tests <- tests2
rm(tests2)
tests2 <- tests %>%
  group_by(CCAA, cod_ine) %>%
  mutate(PCR = PCR_x_1000hab. - lag(PCR_x_1000hab.))
tests2 <- tests2 %>%
  group_by(CCAA, cod_ine) %>%
  mutate(TEST = TEST_ACC_x_1000hab. - lag(TEST_ACC_x_1000hab.))
tests <- tests2[, c(1, 2, 3, 6, 7)]
tests$PCR[is.na(tests$PCR)] <- 0
tests$TEST[is.na(tests$TEST)] <- 0
tests <- tests[tests$Fecha<=cases$Date[length(cases$Date)], ]
rm(tests2)
tests$CCAA2 <- NA
tests$CCAA2[tests$cod_ine==1] <- "Andalucía"
tests$CCAA2[tests$cod_ine==2] <- "Aragón"
tests$CCAA2[tests$cod_ine==6] <- "Cantabria"
tests$CCAA2[tests$cod_ine==8] <- "Castilla - La Mancha"
tests$CCAA2[tests$cod_ine==7] <- "Castilla y León"
tests$CCAA2[tests$cod_ine==9] <- "Cataluña"
tests$CCAA2[tests$cod_ine==15] <- "Comunidad Foral de Navarra"
tests$CCAA2[tests$cod_ine==10] <- "Comunidad Valenciana"
tests$CCAA2[tests$cod_ine==11] <- "Extremadura"
tests$CCAA2[tests$cod_ine==12] <- "Galicia"
tests$CCAA2[tests$cod_ine==17] <- "La Rioja"
tests$CCAA2[tests$cod_ine==13] <- "Madrid"
tests$CCAA2[tests$cod_ine==16] <- "País Vasco"
tests$CCAA2[tests$cod_ine==3] <- "Principado de Asturias"
tests$CCAA2[tests$cod_ine==14] <- "Región de Murcia"
tests$CCAA2[tests$CCAA=="Castilla-la Mancha"] <- "Castilla - La Mancha"
tests$CCAA2[tests$CCAA=="Asturias"] <- "Principado de Asturias"
tests$CCAA2[tests$CCAA=="Andalucía"] <- "Andalucía"
tests$CCAA2[tests$CCAA=="Aragón"] <- "Aragón"
tests$CCAA2[tests$CCAA=="Cantabria"] <- "Cantabria"
tests$CCAA2[tests$CCAA=="Castilla y León"] <- "Castilla y León"
tests$CCAA2[tests$CCAA=="Cataluña"] <- "Cataluña"
tests$CCAA2[tests$CCAA=="C. Foral de Navarra"] <- "Comunidad Foral de Navarra"
tests$CCAA2[tests$CCAA=="Comunidad Valenciana"] <- "Comunidad Valenciana"
tests$CCAA2[tests$CCAA=="Extremadura"] <- "Extremadura"
tests$CCAA2[tests$CCAA=="Galicia"] <- "Galicia"
tests$CCAA2[tests$CCAA=="La Rioja"] <- "La Rioja"
tests$CCAA2[tests$CCAA=="Madrid"] <- "Madrid"
tests$CCAA2[tests$CCAA=="País Vasco"] <- "País Vasco"
tests$CCAA2[tests$CCAA=="Región de Murcia"] <- "Región de Murcia"
tests$CCAA <- NULL
colnames(tests) <- c("Fecha", "cod_ine", "PCR", "TEST", "CCAA")
tests <- tests[order(tests$Fecha, tests$CCAA), ]

# Weather
weather <- read.csv("Data/TEMPS_final.csv")
weather$date <- as.Date (weather$date)
weather$period[weather$date<="2020-03-15"] <- 0                              # No intervention
weather$period[weather$date>="2020-03-16" & weather$date<="2020-03-30"] <- 1 # Emergency state
weather$period[weather$date>="2020-03-31" & weather$date<="2020-04-12"] <- 2 # Mandatory confinement
weather$period[weather$date>="2020-04-13" & weather$date<="2020-06-21"] <- 1
weather$period[weather$date>="2020-06-22"] <- 0
weather <- weather[!weather$CCAA_ISO %in% c("CE", "ML", "CN", "IB"), ]
weather <- weather[weather$date<=cases$Date[length(cases$Date)], ]
weather$CCAA <- NA
weather$CCAA[weather$CCAA_ISO=="AN"] <- "Andalucía"
weather$CCAA[weather$CCAA_ISO=="AR"] <- "Aragón"
weather$CCAA[weather$CCAA_ISO=="AS"] <- "Principado de Asturias"
weather$CCAA[weather$CCAA_ISO=="CB"] <- "Cantabria"
weather$CCAA[weather$CCAA_ISO=="CE"] <- "Ceuta"
weather$CCAA[weather$CCAA_ISO=="CL"] <- "Castilla y León"
weather$CCAA[weather$CCAA_ISO=="CM"] <- "Castilla - La Mancha"
weather$CCAA[weather$CCAA_ISO=="CN"] <- "Canarias"
weather$CCAA[weather$CCAA_ISO=="CT"] <- "Cataluña"
weather$CCAA[weather$CCAA_ISO=="EX"] <- "Extremadura"
weather$CCAA[weather$CCAA_ISO=="GA"] <- "Galicia"
weather$CCAA[weather$CCAA_ISO=="IB"] <- "Islas Baleares"
weather$CCAA[weather$CCAA_ISO=="MC"] <- "Región de Murcia"
weather$CCAA[weather$CCAA_ISO=="MD"] <- "Madrid"
weather$CCAA[weather$CCAA_ISO=="ML"] <- "Melilla"
weather$CCAA[weather$CCAA_ISO=="NC"] <- "Comunidad Foral de Navarra"
weather$CCAA[weather$CCAA_ISO=="PV"] <- "País Vasco"
weather$CCAA[weather$CCAA_ISO=="RI"] <- "La Rioja"
weather$CCAA[weather$CCAA_ISO=="VC"] <- "Comunidad Valenciana"
weather <- weather[order(weather$date, weather$CCAA), ]
weather <- weather[weather$date <= "2020-06-01" & weather$date >= "2020-01-31", ]
# Create polynomials.
poly_pcr=poly(tests$PCR, 2)
poly_anti=poly(tests$TEST, 2)
poly_mean=poly(weather$TEMP_PROM, 2)
poly_min=poly(weather$TEMP_MIN, 2)
poly_max=poly(weather$TEMP_MAX, 2)
poly_period=poly(weather$period, 2)

neighbourhood <- poly2nb(shapefile_ccaa)

# Set up necessary data for the ICAR prior.
adjacency=unlist(neighbourhood)
n_adj=card(neighbourhood)
l_adj=length(adjacency)
weights=rep(1,l_adj)

TB_N=length(cases$cases) # Number of observations.
n_regions=length(n_adj) # Number of regions.

# Set up index for spatial parameters rho and delta.
region_index=numeric(n_regions)
for(i in 1:n_regions){
  region_index[i]=which(as.integer(shapefile_ccaa$Codigo)==as.integer(cases$cod_ine[i]))
}

region_index<-rep(region_index, length(cases$cases[cases$CCAA=="Madrid"]))

# Model code.
TB_code=nimbleCode({ 
  for(i in 1:n){
    pi[i] <- ilogit(b[1]+b[2]*pcr[i,1]+b[3]*pcr[i,2]+
                      b[4]*anti[i,1]+b[5]*anti[i,2]+gamma[i])
    lambda[i] <- exp(log(pop[i])+a[1]+a[2]*temp_avg[i,1]+a[3]*temp_avg[i,2]+
                       a[4]*temp_min[i,1]+a[5]*temp_min[i,2]+a[6]*temp_max[i,1]+
                       a[7]*temp_max[i,2]+a[8]*ind_periode[i,1]+a[9]*ind_periode[i,2]+
                       phi[index[i]]+theta[index[i]])
    z[i] ~ dpois(pi[i]*lambda[i])
    gamma[i]~dnorm(0,sd=epsilon)
  }
  for(j in 1:R){
    theta[j] ~ dnorm(0,sd=sigma)
  }
  phi[1:R] ~ dcar_normal(adj=adj[1:l_adj], num=n_adj[1:R], tau=tau,zero_mean=1)
  a[1] ~ dnorm(-8,sd=1)
  for(i in 2:9){
    a[i] ~ dnorm(0,sd=10)
  }
  b[1] ~ dnorm(2,sd=0.6)
  for(i in 2:5){
    b[i] ~ dnorm(0,sd=10)
  }
  sigma ~ T(dnorm(0,1),0,)
  epsilon ~ T(dnorm(0,1),0,)
  nu ~ T(dnorm(0,1),0,)
  tau <- 1/nu^2
})

# Set up data for NIMBLE.
TB_constants=list(n=TB_N,pop=cases$Pob,n_adj=n_adj,pcr=poly_pcr, anti=poly_anti,
                  temp_avg=poly_mean, temp_min=poly_min, temp_max=poly_max, 
                  ind_periode=poly_period, 
                  adj=adjacency, R=n_regions,index=region_index,w=weights,l_adj=length(adjacency))
TB_data=list(z=cases$cases)

# Set initial values.
TB_inits1=list(sigma=0.25,nu=1,epsilon=0.25,a=c(-7,rep(-0.1,8)),b=c(1.8,rep(0.1,4)),
               gamma=rnorm(TB_N,0,0.25),phi=rnorm(n_regions,0,1),theta=rnorm(n_regions,0,0.25))
TB_inits2=list(sigma=0.5,nu=0.75,epsilon=0.5,a=c(-7,rep(0.1,8)),b=c(1.8,rep(-0.1,4)),
               gamma=rnorm(TB_N,0,0.5),phi=rnorm(n_regions,0,0.75),theta=rnorm(n_regions,0,0.5))
TB_inits3=list(sigma=0.75,nu=0.5,epsilon=0.25,a=c(-9,rep(-0.1,8)),b=c(2.2,rep(0.1,4)),
               gamma=rnorm(TB_N,0,0.25),phi=rnorm(n_regions,0,0.5),theta=rnorm(n_regions,0,0.75))
TB_inits4=list(sigma=1,nu=0.25,epsilon=0.5,a=c(-9,rep(0.1,8)),b=c(2.2,rep(-0.1,4)),
               gamma=rnorm(TB_N,0,0.5),phi=rnorm(n_regions,0,0.25),theta=rnorm(n_regions,0,1))
TB_inits=list(chain1=TB_inits1,chain2=TB_inits2,chain3=TB_inits3,chain4=TB_inits4)

# Build the model.
TB_model <- nimbleModel(TB_code, TB_constants,TB_data,TB_inits)
TB_compiled_model <- compileNimble(TB_model,resetFunctions = TRUE)

# Set up samplers.
TB_mcmc_conf <- configureMCMC(TB_model,monitors=c('a','b','sigma','tau','epsilon',
                                                  'pi','lambda','phi','theta'),useConjugacy = TRUE)
TB_mcmc_conf$removeSamplers(c('a[1]','b[1]','sigma','nu','epsilon'))
TB_mcmc_conf$addSampler(target=c('a[1]','b[1]','epsilon'),type='AF_slice')
TB_mcmc_conf$addSampler(target=c('sigma','nu'),type='AF_slice')

TB_mcmc<-buildMCMC(TB_mcmc_conf)
TB_compiled_mcmc<-compileNimble(TB_mcmc, project = TB_model,resetFunctions = TRUE)

# Run the model (a few hours).
TB_samples=runMCMC(TB_compiled_mcmc,inits=TB_inits,
                   nchains = 4, nburnin=400000,niter = 800000,samplesAsCodaMCMC = TRUE,thin=80,
                   summary = FALSE, WAIC = FALSE,setSeed=c(seed, 2*seed, 3*seed, 4*seed)) 

# Part Three: Model Checking

# Construct adjacency matrix:
TB_A=matrix(0,nrow=n_regions,ncol=n_regions)
TB_A[1,adjacency[1:n_adj[1]]]=1
for(i in 2:n_regions){
  TB_A[i,adjacency[(sum(n_adj[1:(i-1)])+1):(sum(n_adj[1:(i-1)])+n_adj[i])]]=1
}
TB_D <- diag(rowSums(TB_A))

n_sim=10000 # Number of prior samples to simulate.

# Simulate from parameter priors.
prior_alpha=cbind(rnorm(n_sim,-8,1),
                  rnorm(n_sim,0,10),
                  rnorm(n_sim,0,10),
                  rnorm(n_sim,0,10),
                  rnorm(n_sim,0,10),
                  rnorm(n_sim,0,10),
                  rnorm(n_sim,0,10),
                  rnorm(n_sim,0,10),
                  rnorm(n_sim,0,10))
prior_beta=cbind(rnorm(n_sim,2,0.6),
                 rnorm(n_sim,0,10),
                 rnorm(n_sim,0,10),
                 rnorm(n_sim,0,10),
                 rnorm(n_sim,0,10))
prior_sigma=qnorm(runif(n_sim,0.5,1),0,1)
prior_epsilon=qnorm(runif(n_sim,0.5,1),0,1)
prior_nu=qnorm(runif(n_sim,0.5,1),0,1)
prior_tau=1/prior_nu^2

# Simulate random effects.
prior_phi=prior_theta=matrix(nrow=n_sim,ncol=n_regions)
prior_gamma=matrix(nrow=n_sim,ncol=TB_N)

for(i in 1:n_sim){
  prior_theta[i,]=rnorm(n_regions,0,prior_sigma[i])
  prior_gamma[i,]=rnorm(TB_N,0,prior_epsilon[i])
  # Spatial effect phi can be slow to simulate.
  prior_phi[i,]=ricar_compiled(prior_tau[i]*(TB_D-TB_A),.Machine$double.eps)
} 

# Compute prior distributions for pi and lambda.
prior_pi=expit(prior_beta%*%t(cbind(1,poly_pcr,poly_anti))+prior_gamma)
prior_lambda=exp(log(cases$Pob)+prior_alpha%*%t(cbind(1,poly_mean,poly_min,poly_max,poly_period))+prior_theta[,region_index]+prior_phi[,region_index])

# Simulate z.
prior_z=t(apply(prior_lambda*prior_pi,1,function(x)rpois(TB_N,x)))
prior_rate=t(apply(prior_z,1,function(x) x/cases$Pob))
prior_lmse=apply(prior_z,1,function(x) log(mean((x-cases$cases)^2)))

# Combine MCMC chains.
TB_mcmc=do.call('rbind',TB_samples)

# Compute posterior quantities.
posterior_phi=TB_mcmc[,(16+TB_N):(15+TB_N+n_regions)]
posterior_theta=TB_mcmc[,(18+2*TB_N+n_regions):(17+2*TB_N+2*n_regions)]
posterior_lambda=TB_mcmc[,(16):(15+TB_N)]
posterior_pi=TB_mcmc[,(16+TB_N+n_regions):(15+2*TB_N+n_regions)]

# Simulate z.
posterior_z=t(apply(posterior_pi*posterior_lambda,1,function(x)rpois(TB_N,x)))
posterior_lmse=apply(posterior_z,1,function(x) log(mean((x-cases$cases)^2)))
posterior_rate=t(apply(posterior_z,1,function(x) x/cases$Pob))

# Simulate y values.
posterior_y=t(apply(posterior_lambda*(1-posterior_pi),1,function(x)rpois(TB_N,x)+cases$cases))
posterior_total_y=t(apply(posterior_y,1,function(x)c(sum(x[1:n_regions]),sum(x[(1:n_regions)+n_regions]),sum(x[(1:n_regions)+2*n_regions]))))

pi_means <- apply(posterior_pi,2,mean)
inverse_index=sort(region_index[1:n_regions],index.return=TRUE)$ix
pi_spatial <- cbind(pi_means[inverse_index],pi_means[inverse_index+n_regions],pi_means[inverse_index+2*n_regions]) %>%
  logit() %>% apply(.,1,mean) %>% expit()

# Ratio of mean log mean squared errors.
mean(exp(posterior_lmse))/mean(exp(prior_lmse))

# Fitted values plot.
ggplot(data.frame(x=100000*cases$cases/cases$Pob,m=100000*apply(posterior_rate,2,mean), CCAA=cases$CCAA)) +
  geom_abline(aes(slope=1,intercept=0))+geom_point(aes(x=x,y=m),col=vp[7],alpha=0.75) +
  labs(
    title = "Registered COVID-19 cases",
    subtitle ="per 100,000 people",
    y=expression('Mean predicted value'),
    x=expression('Observed value')
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 12, hjust=0.01, color = "#4e4d47", margin = margin(b = 0.1, t = 0.43, l = 2, unit = "cm"))
  )+facet_wrap(facets =  vars(CCAA), scales="free", ncol=5)
ggsave('fitted_covars.pdf',device='pdf',width=8.5,height=6.5)

# Coverage of posterior prediction intervals for recorded counts.

paste('Coverage of 95% posterior prediction intervals for z ',
      round(mean(cases$cases>=apply(posterior_z,2,quantile,0.025)&cases$cases<=apply(posterior_z,2,quantile,0.975)),3))

# Predictive quantile plot.
ggplot(data.frame(x=cases$cases,l=apply(posterior_z,2,quantile,0.025)-cases$cases,
                  u=apply(posterior_z,2,quantile,0.975)-cases$cases)) +
  geom_hline(yintercept=0)+
  geom_point(aes(x=x,y=l),col=vp[8]) +
  geom_point(aes(x=x,y=u),col=vp[10]) +
  labs(
    title = "Registered COVID-19 cases",
    y=expression('Predicted '*tilde(z)['t,s']*' - Observed '*z['t,s']),
    x=expression('Observed number of cases '*z['t,s'])
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = 0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 12, hjust=0.01, color = "#4e4d47", margin = margin(b = 0.1, t = 0.43, l = 2, unit = "cm"))
  )+scale_x_sqrt(limits=c(0,5000))
ggsave('z_plot_covars.pdf',device='pdf',width=4.5,height=3)

# Produce predictive checking plots.
m1=ggplot(data.frame(prior=log(apply(prior_z,1,mean)),post=log(apply(posterior_z,1,mean))))+
  stat_density(aes(x=prior),adjust=2,alpha=0.5,fill=vp[7])+
  geom_vline(xintercept=log(mean(cases$cases)),colour="#22211d")+
  labs(
    y='Prior density',
    x=expression(log('Sample mean'))
  )+
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA)
  )
m2=ggplot(data.frame(prior=log(apply(prior_z,1,mean)),post=log(apply(posterior_z,1,mean))))+
  stat_density(aes(x=post),adjust=2,alpha=0.5,fill=vp[7])+
  geom_vline(xintercept=log(mean(cases$cases)),colour="#22211d")+
  labs(
    y='Posterior density',
    x=expression(log('Sample mean'))
  )+
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA)
  )
v1=ggplot(data.frame(prior=log(apply(prior_z,1,var)),post=log(apply(posterior_z,1,var))))+
  stat_density(aes(x=prior),adjust=2,alpha=0.5,fill=vp[9])+
  geom_vline(xintercept=log(var(cases$cases)),colour="#22211d")+
  labs(
    y='Prior density',
    x=expression(log('Sample variance'))
  )+
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA)
  )
v2=ggplot(data.frame(prior=log(apply(prior_z,1,var)),post=log(apply(posterior_z,1,var))))+
  stat_density(aes(x=post),adjust=2,alpha=0.5,fill=vp[9])+
  geom_vline(xintercept=log(var(cases$cases)),colour="#22211d")+
  labs(
    y='Posterior density',
    x=expression(log('Sample variance'))
  )+
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA)
  )
e1=ggplot(data.frame(prior=prior_lmse,post=posterior_lmse))+
  stat_density(aes(x=prior),adjust=2,alpha=0.5,fill=vp[11])+
  labs(
    y='Prior density',
    x=expression(log('Mean Squared Error'))
  )+
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA)
  )+scale_x_continuous(limits=c(10,35))
e2=ggplot(data.frame(prior=prior_lmse,post=posterior_lmse))+
  stat_density(aes(x=post),adjust=2,alpha=0.5,fill=vp[11])+
  labs(
    y='Posterior density',
    x=expression(log('Mean Squared Error'))
  )+
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA)
  )

pdf(file='check_covars.pdf',width=9,height=4.5)
multiplot(m1,m2,v1,v2,e1,e2,cols=3)
dev.off()

# Part Four: Results
# Produce a map of the covid-19 cases.
sp1<-spTransform(shapefile_ccaa, CRS("+proj=longlat +datum=WGS84"))
spain_map=map_data(sp1)
spain_map$region=as.numeric(spain_map$region)+1
spain_map$region[spain_map$region>3] <- spain_map$region[spain_map$region>3]-2

# Produce spatial effect plots.
spain_map$phi=apply(posterior_phi,2,mean)[spain_map$region]
spain_map$theta=apply(posterior_theta,2,mean)[spain_map$region]
spain_map$combined=spain_map$phi+spain_map$theta[spain_map$region]
spain_map$pi=pi_spatial[spain_map$region]

phi=ggplot() + geom_polygon(data = spain_map, aes(x=long, y = lat, group = group,fill=phi)) +
  theme_void() +
  scale_fill_viridis( breaks=c(-0.5,-0.25,0,0.25,0.5), name=expression(phi[s]), 
                      guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(8, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
  labs(
    title = "Structured spatial effect"
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.75, 0.1)
  ) + coord_map()

theta=ggplot() + geom_polygon(data = spain_map, aes(x=long, y = lat, group = group,fill=theta)) +
  theme_void() +
  scale_fill_viridis( breaks=c(-0.05,-0.025,0,0.025,0.05), name=expression(theta[s]), guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(8, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
  labs(
    title = "Unstructured spatial effect"
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.75, 0.1)
  ) +
  coord_map()

pdf('spatial_effects_covars.pdf',width=12,height=6)
multiplot(phi,theta,cols=2)
dev.off()

# Map of combined spatial effects.
ggplot() + geom_polygon(data = spain_map, aes(x=long, y = lat, group = group,fill=combined))+
  theme_void() +
  scale_fill_viridis( breaks=c(-0.5,-0.25,0,0.25,0.5), name=expression(phi[s]+theta[s]), guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(8, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
  labs(
    title = "Combined spatial effect",
    subtitle = "on COVID-19 incidence"
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.75, 0.1)
  ) +
  coord_map()
ggsave('combined_spatial_covars.pdf',device='pdf',width=6,height=6)

# Map of reporting probabilities.
ggplot() + geom_polygon(data = spain_map, aes(x=long, y = lat, group = group,fill=pi))+
  theme_void() +
  scale_fill_viridis(trans = "logit", breaks=c(0.6,0.7,0.8,0.9), name=expression('Mean predicted '*pi[s]), guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(8, units = "mm"), label.position = "top", title.position = 'top', nrow=1) ) +
  labs(
    title = "Reporting probability",
    subtitle = "of COVID-19 cases"
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.75, 0.1)
  ) +
  coord_map()
ggsave('pi_map_covars.pdf',device='pdf',width=6,height=6)

# Density plot of proportion of spatial variance explained by phi.
ggplot(data.frame(x=apply(posterior_phi,1,var)/apply(posterior_phi+posterior_theta,1,var)))+
  stat_density(aes(x=x),adjust=2,fill=vp[9],alpha=0.5)+
  labs(
    y='Posterior density',
    x=expression(Var(phi[s])/Var(phi[s]+theta[s]))
  )+
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA)
  )
ggsave('phi_var_covars.pdf',device='pdf',width=3,height=2)

# Compute covariate effects.
tmean_seq=seq(min(weather$TEMP_PROM),max(weather$TEMP_PROM),length=1000)
tmin_seq=seq(min(weather$TEMP_MIN),max(weather$TEMP_MIN),length=1000)
tmax_seq=seq(min(weather$TEMP_MAX),max(weather$TEMP_MAX),length=1000)
period_seq=seq(min(weather$period),max(weather$period),length=1000)
pcr_seq=seq(min(tests$PCR),max(tests$PCR),length=1000)
anti_seq=seq(min(tests$TEST),max(tests$TEST),length=1000)
sorted_tmean=TB_mcmc[,2:3]%*%t(predict(poly_mean,tmean_seq))
sorted_tmin=TB_mcmc[,4:5]%*%t(predict(poly_min,tmin_seq))
sorted_tmax=TB_mcmc[,6:7]%*%t(predict(poly_max,tmax_seq))
sorted_period=TB_mcmc[,8:9]%*%t(predict(poly_period,period_seq))
sorted_pcr=expit(TB_mcmc[,10:12]%*%t(cbind(1,predict(poly_pcr,pcr_seq))))
sorted_anti=expit(TB_mcmc[,c(10,13:14)]%*%t(cbind(1,predict(poly_anti,anti_seq))))
posterior_pcr=expit(TB_mcmc[,10:12]%*%t(cbind(1,poly_pcr)))
posterior_anti=expit(TB_mcmc[,c(10,13:14)]%*%t(cbind(1,poly_anti)))
prova=apply(sorted_period,2,mean)
which(prova==max(prova));length(prova)
# Predict covariate effects on COVID-19 incidence.
f1=ggplot(data.frame(x=tmean_seq,m=apply(sorted_tmean,2,mean),
                     l=apply(sorted_tmean,2,quantile,0.025),u=apply(sorted_tmean,2,quantile,0.975))) +
  geom_ribbon(aes(x=x,ymin=l,ymax=u),fill=vp[8],alpha=0.5)+geom_line(aes(x=x,y=m),col=vp[8])+
  labs(
    y=expression(f[1](x['s,1'])),
    x=expression('Average temp. ('*x['s,1']*')')
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA)
  )
f2=ggplot(data.frame(x=tmin_seq,m=apply(sorted_tmin,2,mean),
                     l=apply(sorted_tmin,2,quantile,0.025),u=apply(sorted_tmin,2,quantile,0.975))) +
  geom_ribbon(aes(x=x,ymin=l,ymax=u),fill=vp[8],alpha=0.5)+geom_line(aes(x=x,y=m),col=vp[8])+
  labs(
    y=expression(f[2](x['s,2'])),
    x=expression('Minimum temp. ('*x['s,2']*')')
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA)
  )
f3=ggplot(data.frame(x=tmax_seq,m=apply(sorted_tmax,2,mean),
                     l=apply(sorted_tmax,2,quantile,0.025),u=apply(sorted_tmax,2,quantile,0.975))) +
  geom_ribbon(aes(x=x,ymin=l,ymax=u),fill=vp[8],alpha=0.5)+geom_line(aes(x=x,y=m),col=vp[8])+
  labs(
    y=expression(f[3](x['s,3'])),
    x=expression('Maximum temp. ('*x['s,3']*')')
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA)
  )
f4=ggplot(data.frame(x=period_seq,m=apply(sorted_period,2,mean),
                     l=apply(sorted_period,2,quantile,0.025),u=apply(sorted_period,2,quantile,0.975))) +
  geom_ribbon(aes(x=x,ymin=l,ymax=u),fill=vp[8],alpha=0.5)+geom_line(aes(x=x,y=m),col=vp[8])+
  labs(
    y=expression(f[4](x['s,4'])),
    x=expression('Non-pharmaceutical interventions ('*x['s,4']*')')
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA)
  )
pdf(file='f_plots_pr.pdf',width=9,height=4.5)
multiplot(f1,f2,f3,f4,cols=2)
dev.off()

# Predicted number of PCR tests effect.
ggplot(data.frame(x=pcr_seq,m=apply(sorted_pcr,2,mean),
                  l=apply(sorted_pcr,2,quantile,0.025),u=apply(sorted_pcr,2,quantile,0.975))) +
  geom_ribbon(aes(x=x,ymin=l,ymax=u),fill=vp[9],alpha=0.5)+geom_line(aes(x=x,y=m),col=vp[9])+
  labs(
    y=expression('Reporting Probability ('*pi[s]*')'),
    x=expression('Number of PCR tests ('*u[s]*')')
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA)
  )
ggsave('PCR.pdf',device='pdf',width=4.5,height=2.5)

# Predicted number of antibodies tests effect.
ggplot(data.frame(x=anti_seq,m=apply(sorted_anti,2,mean),
                  l=apply(sorted_anti,2,quantile,0.025),u=apply(sorted_anti,2,quantile,0.975))) +
  geom_ribbon(aes(x=x,ymin=l,ymax=u),fill=vp[9],alpha=0.5)+geom_line(aes(x=x,y=m),col=vp[9])+
  labs(
    y=expression('Reporting Probability ('*pi[s]*')'),
    x=expression('Number of antibodies tests ('*v[s]*')')
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA)
  )
ggsave('Antibodies.pdf',device='pdf',width=4.5,height=2.5)

# Proportion of spatial variance explained by phi.
round(mean(apply(posterior_phi,1,var)/apply(posterior_phi+posterior_theta,1,var)),2)
round(quantile(apply(posterior_phi,1,var)/apply(posterior_phi+posterior_theta,1,var),c(0.025,0.975)),2)

# Create plot of total COVID-19 cases.
bar_data=data.frame(date=rep(cases$Date, 4),
                    CCAA=rep(cases$CCAA, each=4),
                    type=as.factor(rep(c('Observed','Predicted 2.5%','Predicted 50%','Predicted 97.5%'), length(cases$cases))))
bar_data <- bar_data[order(bar_data$date, bar_data$CCAA, bar_data$type), ]
bar_data$value <- as.numeric(rbind(cases$cases, apply(posterior_y,2,quantile,c(0.025,0.5,0.975))))
write.table(bar_data, "time_series_results.csv", row.names=FALSE)
# Create bar plot of total COVID-19 cases.
sums1 <- vector()
sums2 <- vector()
sums3 <- vector()
sums4 <- vector()
for (i in 1:length(unique(bar_data$CCAA)))
{
  CA = unique(bar_data$CCAA)[i]
  sums1[i] <- sum(bar_data$value[bar_data$type=="Observed" & bar_data$CCAA==CA])
  sums2[i] <- sum(bar_data$value[bar_data$type=="Predicted 2.5%" & bar_data$CCAA==CA])
  sums3[i] <- sum(bar_data$value[bar_data$type=="Predicted 50%" & bar_data$CCAA==CA])
  sums4[i] <- sum(bar_data$value[bar_data$type=="Predicted 97.5%" & bar_data$CCAA==CA])
}
bar_data2=data.frame(CCAA=rep(unique(cases$CCAA), each=4), 
                     type=as.factor(rep(c('Observed','Predicted 2.5%','Predicted 50%','Predicted 97.5%'), n_regions)),
                     value=as.numeric(rbind(sums1, sums2, sums3, sums4)))

ggplot(data=bar_data2, aes(x=type, y=value))+
  geom_bar(width = 0.75, stat = "identity",position="dodge",alpha=1)+labs(
    x='',
    y=eval(expression(paste0('Total COVID-19 cases (period ', cases$Date[1], ' / ', cases$Date[length(cases$Date)], ')')))
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") )
    
  )+scale_fill_viridis(discrete = TRUE,begin=0.3,end=0.7)+theme(legend.title=element_blank())+facet_wrap(~CCAA, scales="free", ncol=5)
ggsave('bar_covars.pdf',device='pdf',width=18.5,height=6.5)

write.table(bar_data2, file="results_23072020_covars.csv", row.names = F)
