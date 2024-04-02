library(RPostgreSQL)
library(DBI)
library(dotenv)
library(here)

load_dot_env(file = here("SQL/.env"))         # put all of your credentials in this .env file

dsn_database = Sys.getenv("dsn_database")     # database name
dsn_hostname = Sys.getenv("dsn_hostname")     # hostname of db server (or ip)
dsn_port = Sys.getenv("dsn_port")             # port on server
dsn_uid = Sys.getenv("dsn_uid")               # username to access db
dsn_pwd = Sys.getenv("dsn_pwd")               # password


### Fancy version with tryCatch and nice print messages
# tryCatch({drv <- dbDriver("PostgreSQL")
#          print("Connecting to Database")
#          connec <- dbConnect(drv,
#                              dbname = dsn_database,
#                              host = dsn_hostname,
#                              port = dsn_port,
#                              user = dsn_uid,
#                              password = dsn_pwd)
#          print("Database Connected!")
#          },
#          error=function(cond) {
#           print("Unable to connect to Database")
#          })

drv <- dbDriver("PostgreSQL")
connec <- dbConnect(drv,
                   dbname = dsn_database,
                   host = dsn_hostname,
                   port = dsn_port,
                   user = dsn_uid,
                   password = dsn_pwd)
connec



con <- dbCanConnect(RPostgreSQL::PostgreSQL(),
                    host = dsn_hostname,
                    dbname = dsn_database,
                    port = dsn_port,
                    user = dsn_uid,
                    password = dsn_pwd)

con
