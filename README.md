# couch_authkey

simple authentication handler for rcouch. This handler validate a key
passed in headers or in the query string and create a user context from
it.

## How it works?

A key should be under the form `<dbname>,<keyid>` . The `<dbname>` will
be the database containing the key.

Key are retrieved using a special views. This views emit keys from
documents containing the `auth_keys` property.

ex:

    {
        "auth_keys": [
            {
                "id": "some",
                "roles": [
                    "_admin"
                ]
            }
        ]
    }

A key is an object containing at least 2 properties:

- `id`: the key used to authenticate
- `roles`: thde list of roles available for this key.

An optionnal `expires parameter` can be used to set when the key expires
(unix timestamp).

## Example of usage


Initialize a database named `testdb`:

    $ curl -XPUT localhost:5984/testdb
    {"ok":true}
    $ curl -XPUT localhost:5984/testdb/test -d'{}'
    {"ok":true,"id":"test","rev":"1-967a00dff5e02add41819138abb3284d"}

Create a document containing some keys

    $ curl -XPUT localhost:5984/testdb/testkey -d'{
    > "auth_keys": [
    > {
    >   "id": "some",
    >   "roles": ["_admin"]
    > }]}'
    {"ok":true,"id":"testkey","rev":"1-3aef7395bef669f548d0c5c7cf850515"}

Test it:

     $ curl localhost:5984/testdb/_all_docs?auth_key=testdb,some
    {"total_rows":3,"offset":0,"rows":[
    {"id":"_design\/_auth_keys","key":"_design\/_auth_keys","value":{"rev":"1-a2956efbf41d393bd82e3796e7ecac57"}},
    {"id":"test","key":"test","value":{"rev":"1-967a00dff5e02add41819138abb3284d"}},
    {"id":"testkey","key":"testkey","value":{"rev":"2-3ce4765efaba9a3d2f2fb2b2e31ad5a6"}}
    ]}


    $ curl localhost:5984/testdb/_all_docs
    {"total_rows":3,"offset":0,"rows":[
    {"id":"_design\/_auth_keys","key":"_design\/_auth_keys","value":{"rev":"1-a2956efbf41d393bd82e3796e7ecac57"}},
    {"id":"test","key":"test","value":{"rev":"1-967a00dff5e02add41819138abb3284d"}},
    {"id":"testkey","key":"testkey","value":{"rev":"1-3aef7395bef669f548d0c5c7cf850515"}}
    ]}

    $ curl localhost:5984/testdb/_all_docs?auth_key=testdb,som
    {"error":"unauthorized","reason":"invalid key"}


With `require_valid_user = true`:

    $ curl localhost:5984/testdb/_all_docs
    {"error":"unauthorized","reason":"Authentication required."}

    $ curl localhost:5984/testdb/_all_docs?auth_key=testdb,some
    {"total_rows":3,"offset":0,"rows":[
    {"id":"_design\/_auth_keys","key":"_design\/_auth_keys","value":{"rev":"1-a2956efbf41d393bd82e3796e7ecac57"}},
    {"id":"test","key":"test","value":{"rev":"1-967a00dff5e02add41819138abb3284d"}},
    {"id":"testkey","key":"testkey","value":{"rev":"2-3ce4765efaba9a3d2f2fb2b2e31ad5a6"}}
    ]}

    curl admin:test@localhost:5984/testdb/_all_docs
    {"total_rows":3,"offset":0,"rows":[
    {"id":"_design\/_auth_keys","key":"_design\/_auth_keys","value":{"rev":"1-a2956efbf41d393bd82e3796e7ecac57"}},
    {"id":"test","key":"test","value":{"rev":"1-967a00dff5e02add41819138abb3284d"}},
    {"id":"testkey","key":"testkey","value":{"rev":"2-3ce4765efaba9a3d2f2fb2b2e31ad5a6"}}
    ]}
