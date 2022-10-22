## Load tweet data
cd /tmp/data/tweets/raw || exit

 ls -1 data_*.json | while read col; do
   mongoimport -d thesis-data -c tweets --jsonArray -u "$MONGO_INITDB_ROOT_USERNAME" -p "$MONGO_INITDB_ROOT_PASSWORD" --authenticationDatabase admin < "$col";
 done

## Load user data
 ls -1 users_*.json | while read col; do
   mongoimport -d thesis-data -c temp -u "$MONGO_INITDB_ROOT_USERNAME" -p "$MONGO_INITDB_ROOT_PASSWORD" --authenticationDatabase admin < "$col";
 done
