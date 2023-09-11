#
#
#
library(httr)
library(archive)

tf <- tempfile()

this_url <-	"https://download.inep.gov.br/microdados/microdados_enem_2022.zip"

GET( this_url , write_disk( tf ) , progress() )

archive_extract( tf , dir = tempdir() )
library(readr)

enem_fns <- list.files( tempdir() , recursive = TRUE , full.names = TRUE )

enem_fn <- grep( "MICRODADOS_ENEM_([0-9][0-9][0-9][0-9])\\.csv$" , enem_fns , value = TRUE )

enem_tbl <- read_csv2( enem_fn )

enem_df <- data.frame( enem_tbl )

names( enem_df ) <- tolower( names( enem_df ) )
# enem_fn <- file.path( path.expand( "~" ) , "ENEM" , "this_file.rds" )
# saveRDS( enem_df , file = enem_fn , compress = FALSE )
# enem_df <- readRDS( enem_fn )
enem_df <- 
	transform( 
		enem_df , 
		
		# qual foi o tempo gasto por voce para concluir a prova?
		less_than_two_hours = as.numeric( co_rs_i9 %in% c( 'A' , 'B' ) ) ,
		
		administrative_category =
			factor(
				co_categad ,
				levels = c( 1:5 , 7 ) ,
				labels = c( '1. Pública Federal' , '2. Pública Estadual' , 
				'3. Pública Municipal' , '4. Privada com fins lucrativos' , 
				'5. Privada sem fins lucrativos' , '7. Especial' )
			) ,

		state_name = 
			factor( 
				co_uf_curso , 
				levels = c( 11:17 , 21:29 , 31:33 , 35 , 41:43 , 50:53 ) ,
				labels = c( "Rondonia" , "Acre" , "Amazonas" , 
				"Roraima" , "Para" , "Amapa" , "Tocantins" , 
				"Maranhao" , "Piaui" , "Ceara" , "Rio Grande do Norte" , 
				"Paraiba" , "Pernambuco" , "Alagoas" , "Sergipe" , 
				"Bahia" , "Minas Gerais" , "Espirito Santo" , 
				"Rio de Janeiro" , "Sao Paulo" , "Parana" , 
				"Santa Catarina" , "Rio Grande do Sul" , 
				"Mato Grosso do Sul" , "Mato Grosso" , "Goias" , 
				"Distrito Federal" )
			)

	)
	
nrow( enem_df )

table( enem_df[ , "fathers_education" ] , useNA = "always" )
mean( enem_df[ , "nota_mt" ] )

tapply(
	enem_df[ , "nota_mt" ] ,
	enem_df[ , "fathers_education" ] ,
	mean 
)
prop.table( table( enem_df[ , "uf_residencia" ] ) )

prop.table(
	table( enem_df[ , c( "uf_residencia" , "fathers_education" ) ] ) ,
	margin = 2
)
sum( enem_df[ , "nota_mt" ] )

tapply(
	enem_df[ , "nota_mt" ] ,
	enem_df[ , "fathers_education" ] ,
	sum 
)
quantile( enem_df[ , "nota_mt" ] , 0.5 )

tapply(
	enem_df[ , "nota_mt" ] ,
	enem_df[ , "fathers_education" ] ,
	quantile ,
	0.5 
)
sub_enem_df <- subset( enem_df , in_presenca_mt = 1 )
mean( sub_enem_df[ , "nota_mt" ] )
var( enem_df[ , "nota_mt" ] )

tapply(
	enem_df[ , "nota_mt" ] ,
	enem_df[ , "fathers_education" ] ,
	var 
)
t.test( nota_mt ~ female , enem_df )
this_table <- table( enem_df[ , c( "female" , "uf_residencia" ) ] )

chisq.test( this_table )
glm_result <- 
	glm( 
		nota_mt ~ female + uf_residencia , 
		data = enem_df
	)

summary( glm_result )

library(dplyr)
enem_tbl <- as_tibble( enem_df )
enem_tbl %>%
	summarize( mean = mean( nota_mt ) )

enem_tbl %>%
	group_by( fathers_education ) %>%
	summarize( mean = mean( nota_mt ) )
library(data.table)
enem_dt <- data.table( enem_df )
enem_dt[ , mean( nota_mt ) ]

enem_dt[ , mean( nota_mt ) , by = fathers_education ]
library(duckdb)
con <- dbConnect( duckdb::duckdb() , dbdir = 'my-db.duckdb' )
dbWriteTable( con , 'enem' , enem_df )
dbGetQuery( con , 'SELECT AVG( nota_mt ) FROM enem' )

dbGetQuery(
	con ,
	'SELECT
		fathers_education ,
		AVG( nota_mt )
	FROM
		enem
	GROUP BY
		fathers_education'
)
