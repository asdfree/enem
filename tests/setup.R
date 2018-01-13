if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)

library(lodown)
lodown( "enem" , output_dir = file.path( getwd() ) )
library(lodown)
# examine all available ENEM microdata files
enem_cat <-
	get_catalog( "enem" ,
		output_dir = file.path( getwd() ) )

# 2015 only
enem_cat <- subset( enem_cat , year == 2015 )
# download the microdata to your local computer

library(DBI)
dbdir <- file.path( getwd() , "SQLite.db" )
db <- dbConnect( RSQLite::SQLite() , dbdir )

dbSendQuery( db , "ALTER TABLE microdados_enem_2015 ADD COLUMN female INTEGER" )

dbSendQuery( db , 
	"UPDATE microdados_enem_2015 
	SET female = 
		CASE WHEN tp_sexo = 2 THEN 1 ELSE 0 END" 
)

dbSendQuery( db , "ALTER TABLE microdados_enem_2015 ADD COLUMN fathers_education INTEGER" )

dbSendQuery( db , 
	"UPDATE microdados_enem_2015 
	SET fathers_education = 
		CASE WHEN q001 = 1 THEN '01 - nao estudou'
			WHEN q001 = 2 THEN '02 - 1 a 4 serie'
			WHEN q001 = 3 THEN '03 - 5 a 8 serie'
			WHEN q001 = 4 THEN '04 - ensino medio incompleto'
			WHEN q001 = 5 THEN '05 - ensino medio'
			WHEN q001 = 6 THEN '06 - ensino superior incompleto'
			WHEN q001 = 7 THEN '07 - ensino superior'
			WHEN q001 = 8 THEN '08 - pos-graduacao'
			WHEN q001 = 9 THEN '09 - nao estudou' ELSE NULL END" 
)
dbGetQuery( db , "SELECT COUNT(*) FROM microdados_enem_2015" )

dbGetQuery( db ,
	"SELECT
		fathers_education ,
		COUNT(*) 
	FROM microdados_enem_2015
	GROUP BY fathers_education"
)
dbGetQuery( db , "SELECT AVG( nota_mt ) FROM microdados_enem_2015" )

dbGetQuery( db , 
	"SELECT 
		fathers_education , 
		AVG( nota_mt ) AS mean_nota_mt
	FROM microdados_enem_2015 
	GROUP BY fathers_education" 
)
dbGetQuery( db , 
	"SELECT 
		uf_residencia , 
		COUNT(*) / ( SELECT COUNT(*) FROM microdados_enem_2015 ) 
			AS share_uf_residencia
	FROM microdados_enem_2015 
	GROUP BY uf_residencia" 
)
dbGetQuery( db , "SELECT SUM( nota_mt ) FROM microdados_enem_2015" )

dbGetQuery( db , 
	"SELECT 
		fathers_education , 
		SUM( nota_mt ) AS sum_nota_mt 
	FROM microdados_enem_2015 
	GROUP BY fathers_education" 
)
RSQLite::initExtension( db )

dbGetQuery( db , 
	"SELECT 
		LOWER_QUARTILE( nota_mt ) , 
		MEDIAN( nota_mt ) , 
		UPPER_QUARTILE( nota_mt ) 
	FROM microdados_enem_2015" 
)

dbGetQuery( db , 
	"SELECT 
		fathers_education , 
		LOWER_QUARTILE( nota_mt ) AS lower_quartile_nota_mt , 
		MEDIAN( nota_mt ) AS median_nota_mt , 
		UPPER_QUARTILE( nota_mt ) AS upper_quartile_nota_mt
	FROM microdados_enem_2015 
	GROUP BY fathers_education" 
)
dbGetQuery( db ,
	"SELECT
		AVG( nota_mt )
	FROM microdados_enem_2015
	WHERE in_presenca_mt = 1"
)
RSQLite::initExtension( db )

dbGetQuery( db , 
	"SELECT 
		VARIANCE( nota_mt ) , 
		STDEV( nota_mt ) 
	FROM microdados_enem_2015" 
)

dbGetQuery( db , 
	"SELECT 
		fathers_education , 
		VARIANCE( nota_mt ) AS var_nota_mt ,
		STDEV( nota_mt ) AS stddev_nota_mt
	FROM microdados_enem_2015 
	GROUP BY fathers_education" 
)
enem_three_columns_df <- 
	dbGetQuery( db , 
		"SELECT 
			nota_mt , 
			female ,
			uf_residencia
		FROM microdados_enem_2015" 
	)

t.test( nota_mt ~ female , enem_three_columns_df )
this_table <- table( enem_three_columns_df[ , c( "female" , "uf_residencia" ) ] )

chisq.test( this_table )
glm_result <- 
	glm( 
		nota_mt ~ female + uf_residencia , 
		data = enem_three_columns_df
	)

summary( glm_result )
library(dplyr)
library(dbplyr)
dplyr_db <- dplyr::src_sqlite( dbdir )
enem_tbl <- tbl( dplyr_db , 'microdados_enem_2015' )
enem_tbl %>%
	summarize( mean = mean( nota_mt ) )

enem_tbl %>%
	group_by( fathers_education ) %>%
	summarize( mean = mean( nota_mt ) )
dbGetQuery( db , "SELECT COUNT(*) FROM microdados_enem_2015" )
