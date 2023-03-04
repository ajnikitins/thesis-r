conn = Mongo(`mongodb://${process.env['MONGO_INITDB_ROOT_USERNAME']}:${process.env['MONGO_INITDB_ROOT_PASSWORD']}@localhost:27017/admin`);


db = conn.getDB('thesis-data');

// Split places from users
db.temp_users.aggregate([
  {$project: {'_id': 0, 'places': 1}},
  {$unwind: '$places'},
  {$sort: {'places.id': 1}},
  {$group: {
      '_id': '$places.id',
      'places': {$first: '$places'}
  }},
  {$replaceRoot: {newRoot: '$places'}},
  {$addFields: {'_id': '$id', 'id': 0}},
  {$merge: {
    into: 'places',
      on: '_id',
      whenMatched: 'replace'
  }}
  // {$out: 'places'}
])

// Split users from users
db.temp_users.aggregate([
  {$project: {'_id': 0, 'users': 1}},
  {$unwind: '$users'},
  {$sort: {'users.id': 1}},
  {$group: {
      '_id': '$users.id',
      'users': {$first: '$users'}
    }},
  {$replaceRoot: {newRoot: '$users'}},
  {$addFields: {'_id': '$id'}},
  {$unset: "id"},
  {$merge: {
      into: 'users',
      on: '_id',
      whenMatched: 'replace'
  }}
  // {$out: 'users'}
])

db.temp_users.drop()

db.temp_tweets.aggregate([
  {$addFields: {'_id': '$id'}},
  {$unset: "id"},
  {$merge: {
      into: 'tweets',
      on: '_id',
      whenMatched: 'replace'
  }}
  // {$out: 'tweets'}
])

// db.temp_tweets.drop()
