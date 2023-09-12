
rm(list=ls())

options(scipen=999,
        
        pillar.sigfig = 7)


if (!require(pacman)) install.packages("pacman") 

p_load( "dplyr", "tidyverse",
        "printr", "tinytex",
        "timetk", "tidyquant", 
        "readxl", "lubridate",
        'tseries', "openxlsx",
        "caret", "forecast",
        "zoo")


# Datos -------------------------------------------------------------------

#####################################################################
##### NOTA: Cargar datos a partir de codigo "Tratamiento_datos" #####
#####################################################################
#DF variables Y 
Datos_Y <- Datos_s[,colnames(Datos_s[,1:9])] 

#Fecha en en intervalo y formato correcto
fecha    <- (c(seq(from = as.Date("2015-01-01"), 
                   to = as.Date("2022-4-01"), 
                   by = 'month')))

Pred_HW  <- data.frame(fecha = seq(from = lubridate::as_date("2022-05-01"),
                                   by = "month",length.out = 8))

#Modelo XGBoost ----
#For loop XGboost por cada una de la series
for (i in c(colnames(Datos_Y))) {
#Data  Frame con una sola serie y fecha
data     <- cbind(fecha,Datos_Y) 
  
data     <- data[, c("fecha", i)]  

#DF extendido con NA
extended_data <-  tibble(fecha = seq(from = lubridate::as_date("2022-05-01"),
                                      by = "month",length.out = 8), 
                        Otros = rep(NA, 8))

names(extended_data)[2] <- i

extended_data <- rbind(data ,extended_data)

extended_data_mod <- extended_data %>%
                     dplyr::mutate(., 
                                 months = lubridate::month(fecha),
                                 years = lubridate::year(fecha))

train <- extended_data_mod[1:nrow(data), ] # Datos Inciales 

pred  <- extended_data_mod[(nrow(data) + 1):nrow(extended_data), ] #Extendemos el índice de tiempo 

#Matrices
x_train <- train %>%
           dplyr::select(months, years) %>%  
           as.matrix() 
          # xgb.DMatrix()

x_pred <-  pred %>%  
           dplyr::select(months , years) %>% 
           as.matrix() 
#%>%  
           #xgb.DMatrix()
  
 
#Train Y
y_train <- train[, i] %>%  
           replace_na(.,0)
           

#Control del modelo 
xgb_trcontrol <- caret::trainControl(
                        method = "cv", 
                        number = 5,
                        allowParallel = TRUE, 
                        verboseIter = FALSE, 
                        returnData = FALSE)

#Building parameters set------
xgb_grid <- base::expand.grid(
            list(nrounds = c(100, 200),
                 max_depth = c(10, 15, 20),   # maximum depth of a tree
                 colsample_bytree = seq(0.5), # subsample ratio of columns when construction each tree
                 eta = 0.1,                   # learning rate
                 gamma = 0,                   # minimum loss reduction
                 min_child_weight = 1,        # minimum sum of instance weight (hessian) needed ina child
                 subsample = 1                # subsample ratio of the training instances
                  ))
          
          

#Building the model -----------
xgb_model <- caret::train( x_train, 
                           y_train,
                           trControl = xgb_trcontrol,
                           tuneGrid  = xgb_grid,
                           method    = "xgbTree",
                           nthread    = 5 )                         

#Best Tune
 xgb_model$results
 
#Metrics
min_RMSE      <-  min(xgb_model$results$RMSE)
max_R_squared <-  max(xgb_model$results$Rsquared)

min_MAE       <-  min(xgb_model$results$MAE)
min_RMSESD     <-  min(xgb_model$results$RMSESD)


#Predicción 
xgb_pred <- xgb_model  %>% 
  stats::predict(x_pred)

#Forecast -------------------------

# prediction on a train set
fitted <- xgb_model %>%
  stats::predict(x_train) %>%
  stats::ts(start = zoo::as.yearmon(min(train$fecha)), 
            end = zoo::as.yearmon(max(train$fecha)),
            frequency = 12)

# prediction in a form of ts object
xgb_forecast <- xgb_pred %>%
  stats::ts(start = zoo::as.yearmon(min(pred$fecha)),
            end = zoo::as.yearmon(max(pred$fecha)),
            frequency = 12)

# original data as ts object
ts <- y_train %>% 
  stats::ts(start = zoo::as.yearmon(min(train$fecha)), 
            end = zoo::as.yearmon(max(train$fecha)), 
            frequency = 12)

df_results <- tibble(fitted ,ts)

#Suma de Residuos al Cuadrados
SRQ <- df_results %>%  
       mutate(residuales = ts - fitted) %>%  
       summarise(sum(residuales)^2)

#Forecast Object 

forecast_list <- list(
  model = xgb_model$modelInfo,
  method = xgb_model$method,
  mean = xgb_forecast,
  x = ts, 
  fitted = fitted,
  residuals = as.numeric(ts) - as.numeric(fitted)
)
#Class
class(forecast_list) <- "forecast"

#Plot 
forecast::autoplot(forecast_list)
 
name <- paste("HW_", i , sep = "")




Pred_HW[, c(i)] <-  cbind( c((assign(name, xgb_model)  %>% 
 
                        stats::predict(x_pred)) ))
 

}


#DF Observados
Datos_Y <- Datos_Y %>% 
            cbind(fecha=c(seq(from = as.Date("2015-01-01"), 
                              to = as.Date("2022-04-01"), 
                              by = 'month')))

#DF con predicción y fechas correctas 

Datos_Y_w <- rbind(Datos_Y , Pred_HW)



# Estimación para 2022  -------  
million <- 1000000

Datos_result_nivel_2022 <-  Datos_Y_w  %>%     
                            filter(fecha >="2022-01-01")    %>% 
                            mutate_at(vars(-one_of("fecha")), exp) %>% 
                            mutate_at(vars(-one_of("fecha")), ~./ million)

#Recaudación total estimada -----

n_recaudación_2022 <- Datos_result_nivel_2022 %>% 
                      dplyr::select( -(fecha)) %>%  
                      mutate(total_rec =  rowSums(.)) %>% 
                      
                      summarise(total = sum(total_rec))


#PLOTS 
df_ob_fc_long <- data_final_area_total %>%  
                 pivot_longer(cols = - fecha, names_to =  "Áreas Generadoras", values_to = "Valor") %>% 
                 mutate(mes   = format(as.Date(fecha), "%B")) %>% 
                 mutate( Tipo = if_else( fecha > '2021-07-01', "Pronóstico", "Observado")) 

pal   <- rev(gobmx_palette("Federal", n = 3))

#Plot 1 
df_ob_fc_long %>%  
  ggplot( aes(x = fecha, y =  Valor )) + 
  geom_point() +
  geom_line()+ 
  
  labs(title = "Pronóstico Modelo StepWise 5")+
  facet_wrap(~`Áreas Generadoras` , scales = "free")+
  theme_minimal() + 
  scale_color_manual(name = "", values = c("gray", "darkgreen")) +
  theme(legend.position = c("bottom"),
        plot.title = element_text(hjust = 0.5, lineheight = 1, size=14))
