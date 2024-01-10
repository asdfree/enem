# graduation stage
# shake hands, toss cap, unroll scroll,
# mais um exame?
library(httr)
library(archive)

tf <- tempfile()

this_url <-	"https://download.inep.gov.br/microdados/microdados_enem_2022.zip"

GET( this_url , write_disk( tf ) , progress() )

archive_extract( tf , dir = tempdir() )
library(readr)

enem_fns <- list.files( tempdir() , recursive = TRUE , full.names = TRUE )

enem_fn <- grep( "MICRODADOS_ENEM_([0-9][0-9][0-9][0-9])\\.csv$" , enem_fns , value = TRUE )

enem_tbl <- read_csv2( enem_fn , locale = locale( encoding = 'latin1' ) )

enem_df <- data.frame( enem_tbl )

names( enem_df ) <- tolower( names( enem_df ) )
# enem_fn <- file.path( path.expand( "~" ) , "ENEM" , "this_file.rds" )
# saveRDS( enem_df , file = enem_fn , compress = FALSE )
# enem_df <- readRDS( enem_fn )
enem_df <- 
	transform( 
		enem_df , 
		
		domestic_worker = as.numeric( q007 %in% c( 'B' , 'C' , 'D' ) ) ,
		
		administrative_category =
			factor(
				tp_dependencia_adm_esc ,
				levels = 1:4 ,
				labels = c( 'Federal' , 'Estadual' , 'Municipal' , 'Privada' )
			) ,

		state_name = 
			factor( 
				co_uf_esc , 
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

table( enem_df[ , "administrative_category" ] , useNA = "always" )
mean( enem_df[ , "nu_nota_mt" ] , na.rm = TRUE )

tapply(
	enem_df[ , "nu_nota_mt" ] ,
	enem_df[ , "administrative_category" ] ,
	mean ,
	na.rm = TRUE 
)
prop.table( table( enem_df[ , "state_name" ] ) )

prop.table(
	table( enem_df[ , c( "state_name" , "administrative_category" ) ] ) ,
	margin = 2
)
sum( enem_df[ , "nu_nota_mt" ] , na.rm = TRUE )

tapply(
	enem_df[ , "nu_nota_mt" ] ,
	enem_df[ , "administrative_category" ] ,
	sum ,
	na.rm = TRUE 
)
quantile( enem_df[ , "nu_nota_mt" ] , 0.5 , na.rm = TRUE )

tapply(
	enem_df[ , "nu_nota_mt" ] ,
	enem_df[ , "administrative_category" ] ,
	quantile ,
	0.5 ,
	na.rm = TRUE 
)
sub_enem_df <- subset( enem_df , q002 %in% c( 'E' , 'F' , 'G' ) )
mean( sub_enem_df[ , "nu_nota_mt" ] , na.rm = TRUE )
var( enem_df[ , "nu_nota_mt" ] , na.rm = TRUE )

tapply(
	enem_df[ , "nu_nota_mt" ] ,
	enem_df[ , "administrative_category" ] ,
	var ,
	na.rm = TRUE 
)
t.test( nu_nota_mt ~ domestic_worker , enem_df )
this_table <- table( enem_df[ , c( "domestic_worker" , "state_name" ) ] )

chisq.test( this_table )
glm_result <- 
	glm( 
		nu_nota_mt ~ domestic_worker + state_name , 
		data = enem_df
	)

summary( glm_result )
stopifnot( nrow( enem_df ) == 3476105 )
library(dplyr)
enem_tbl <- as_tibble( enem_df )
enem_tbl %>%
	summarize( mean = mean( nu_nota_mt , na.rm = TRUE ) )

enem_tbl %>%
	group_by( administrative_category ) %>%
	summarize( mean = mean( nu_nota_mt , na.rm = TRUE ) )
library(data.table)
enem_dt <- data.table( enem_df )
enem_dt[ , mean( nu_nota_mt , na.rm = TRUE ) ]

enem_dt[ , mean( nu_nota_mt , na.rm = TRUE ) , by = administrative_category ]
library(duckdb)
con <- dbConnect( duckdb::duckdb() , dbdir = 'my-db.duckdb' )
dbWriteTable( con , 'enem' , enem_df )
dbGetQuery( con , 'SELECT AVG( nu_nota_mt ) FROM enem' )

dbGetQuery(
	con ,
	'SELECT
		administrative_category ,
		AVG( nu_nota_mt )
	FROM
		enem
	GROUP BY
		administrative_category'
)
