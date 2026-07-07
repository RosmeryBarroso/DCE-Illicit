# DCE-Illicit - Orden para correr los archivos DCE Illicit

1. DCEi_01_CleanData.do: Importa y depura por separado las bases DCE de Colombia y Bolivia (construcción de variables: tiempo de respuesta,Fagerström, aversión al riesgo, preferencia intertemporal, ilicitud, valores prosociales),
estandariza nombres/variables entre países y las consolida en Pool.dta con un id único. 

2. DCEi_02_OrganizeData.do: Prepara la base Pool.dta del DCE (Colombia/Bolivia): recodifica atributos y precios por alternativa,
filtra por tiempo mínimo de respuesta y exporta la base depurada (price_continuos.csv) lista para estimación. 

3. DCEi_03a_batch.R: controla la ejecución automática y secuencial de los scripts de estimación, definiendo las rutas de trabajo y los grupos a correr. Dentro de este codigo se corren los siguientes codigos:
   -  DCEi_03b_estimacionMMNL_sinInt: ejecuta las estimaciones del modelo por país y por grupo
   -  DCEe_04b_estimacionMMNL_groups_pooled:Estima el modelo MMNL de forma conjunta (Colombia y Bolivia) y por subgrupo de la muestra.
   
Para realizar las figuras de heterogeneidades se tiene en cuenta el siguiente script: 
4. DCEi_05_Figures.R: Genera forest plots comparando Colombia y Bolivia por subgrupos de heterogeneidad (historial de compra, reconocimiento de ilícitos, precio, aversión al riesgo, preferencias intertemporales, dependencia, ciudad y valores prosociales) a partir de los coeficientes MMNL exportados.

5. DCE_05_simulaciones_precio_Licito.R : Estima el modelo MMNL principal y luego simula el efecto de subir el precio de los productos licitos (manteniendo fijo el precio del ilícito) sobre las participaciones de mercado y elasticidades, tanto para la muestra completa como por subgrupos de compradores.
