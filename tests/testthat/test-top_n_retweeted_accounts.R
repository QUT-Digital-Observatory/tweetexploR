# Database connection ####

# Connect to sqlite .db file
sqlite_con <- connect_to_sqlite_db(test_path("fixtures", "auspol-test.db"))

# TODO: Write tests

# Disconnect from database ####
DBI::dbDisconnect(sqlite_con)
