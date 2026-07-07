# DCE-Illicit - Orden para correr los archivos DCE Illicit

1. DCEi_01_CleanData.do: Importa y depura por separado las bases DCE de Colombia y Bolivia (construcción de variables: tiempo de respuesta,Fagerström, aversión al riesgo, preferencia intertemporal, ilicitud, valores prosociales),
estandariza nombres/variables entre países y las consolida en Pool.dta con un id único. 

4. DCEi_02_OrganizeData: Prepara la base Pool.dta del DCE (Colombia/Bolivia): recodifica atributos y precios por alternativa,
filtra por tiempo mínimo de respuesta y exporta la base depurada (price_continuos.csv) lista para estimación. 

5. DCEi_03a_batch: controla la ejecución automática y secuencial de los scripts de estimación, definiendo las rutas de trabajo y los grupos a correr.

6. DCEi_03b_estimacionMMNL_sinInt: ejecuta las estimaciones del modelo por país y por grupo
7. DCEe_04b_estimacionMMNL_groups_pooled
   
Para realizar las figuras de heterogeneidades se tiene en cuenta el script: 
6. DCEi_05_Figures: 
