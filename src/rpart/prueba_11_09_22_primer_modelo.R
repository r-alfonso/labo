#Feature Engineering
#creo nuevas variables dentro del mismo mes
#Condimentar a gusto con nuevas variables

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")

#Establezco el Working Directory
setwd("C:/Users/bbvis/Documents/RG/Master/laboratorio")


EnriquecerDataset <- function( dataset , arch_destino )
{
  columnas_originales <-  copy(colnames( dataset ))
  
  #INICIO de la seccion donde se deben hacer cambios con variables nuevas
  #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
  #varias formas de combinar Visa_status y Master_status
  dataset[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
  #dataset[ , mv_status02       := Master_status +  Visa_status ]
  #dataset[ , mv_status03       := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
  #dataset[ , mv_status04       := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
  #dataset[ , mv_status05       := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]
  
  #dataset[ , mv_status06       := ifelse( is.na(Visa_status), 
  #                                        ifelse( is.na(Master_status), 10, Master_status), 
  #                                        Visa_status)  ]
  
  #dataset[ , mv_status07       := ifelse( is.na(Master_status), 
  #                                        ifelse( is.na(Visa_status), 10, Visa_status), 
  #                                        Master_status)  ]
  
  
  
  #combino MasterCard y Visa
  dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]
  dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
  dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
  dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
  dataset[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
  dataset[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
  dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]
  
  #combino los margenes y los dividos por la antiguedad para obtener los margenes de ganancia promedio por mes
  dataset[ , fe_ganancia_margen_banco := rowSums( cbind( mactivos_margen,  mpasivos_margen) , na.rm=TRUE ) ]
  dataset[ , fe_avg_mes_ganancia_margen_banco := fe_ganancia_margen_banco /  cliente_antiguedad ]
  
  #combino los montos de todas las cuentas del cliente
  dataset[ , fe_montos_cuentas := rowSums( cbind(mcuenta_corriente_adicional, mcuenta_corriente, mcaja_ahorro, mcaja_ahorro_adicional, mcaja_ahorro_dolares) , na.rm=TRUE ) ]
  
  #combino la cantidad de prestamos
  dataset[ , fe_cantidad_prestamos := rowSums( cbind( cprestamos_personales, cprestamos_prendarios, cprestamos_hipotecarios) , na.rm=TRUE ) ]
  
  #combino el total de deuda
  dataset[ , fe_total_deuda := rowSums( cbind( mprestamos_personales, mprestamos_prendarios, mprestamos_hipotecarios) , na.rm=TRUE ) ]  
  
  #combino los montos de plazo fijo vigentes
  dataset[ , fe_montos_plazos_fijos := rowSums( cbind( mplazo_fijo_dolares, mplazo_fijo_pesos) , na.rm=TRUE ) ]
  
  #combino los montos de inversiones
  dataset[ , fe_montos_inversion := rowSums( cbind( minversion1_pesos, minversion1_dolares, minversion2) , na.rm=TRUE ) ]
  
  #combino la cantidad de seguros
  dataset[ , fe_cantidad_seguros := rowSums( cbind( cseguro_vida, cseguro_auto, cseguro_vivienda, cseguro_accidentes_personales) , na.rm=TRUE ) ]
  
  #combino las acreditaciones del cliente
  dataset[ , fe_acreditaciones := rowSums( cbind( mpayroll, mpayroll2) , na.rm=TRUE ) ]   
  
  
  columnas_extendidas <-  copy( setdiff(  colnames(dataset), columnas_originales ) )
  
  #grabo con nombre extendido
  fwrite( dataset,
          file=arch_destino,
          sep= "," )
}
#------------------------------------------------------------------------------

#kcarpeta_datasets    <- "../input/laboratorio-de-implementacion-i-2021/"   #KAGGLE
kcarpeta_datasets    <-  "./datasets/"    #"./buckets/b1/datasets/"                          #VM o Ubuntu

#Archivo con datos etiquetados para entrenamiento
karchivo_entrada      <-  paste0(kcarpeta_datasets, "competencia1_2022.csv")
karchivo_salida      <-  paste0(kcarpeta_datasets, "competencia1_2022_FE_test.csv")



#lectura rapida del dataset  usando fread  de la libreria  data.table
dataset1  <- fread(karchivo_entrada)

EnriquecerDataset( dataset1, karchivo_salida )

quit( save="no")













#Arbol elemental con libreria  rpart
#Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:/Users/bbvis/Documents/RG/Master/laboratorio")  #Establezco el Working Directory

#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022_FE_test.csv")

dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo

#genero el modelo,  aqui se construye el arbol
modelo  <- rpart(formula=   "clase_ternaria ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=      0,
                 cp=       -0.3,   #esto significa no limitar la complejidad de los splits
                 minsplit=  0,     #minima cantidad de registros para que se haga el split
                 minbucket= 1,     #tamaño minimo de una hoja
                 maxdepth=  4 )    #profundidad maxima del arbol


#grafico el arbol
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)


df_test<- as.data.frame(caret::varImp(modelo))

df<- as.data.frame(caret::varImp(modelo))




df2 <- cbind(df,df_test)


#aplico el modelo a los datos nuevos
prediccion  <- predict( object= modelo,
                        newdata= dapply,
                        type = "prob")

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/40
dapply[ , Predicted := as.numeric( prob_baja2 > 1/40 ) ]

#genero el archivo para Kaggle
#primero creo la carpeta donde va el experimento
dir.create( "./exp/" )
dir.create( "./exp/KA2001" )

fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA2001/K101_001.csv",
        sep=  "," )