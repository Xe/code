Logsmith
========

Logsmith is a login/authentication server for sites to use accounts in an 
Atheme database as an external identity provider.

Using
-----

Point `ATHEME_URL` to the URL of your Atheme install's XMLRPC frontend. Then 
set `ATHEME_USERNAME` and `ATHEME_PASSWORD` to the username and password of 
a bot account that has permissions in Atheme to see user account ID's.

In order to check logins, point a client to `/login` and post their `username` 
and `password`. This will return a json object with the following schema:

```json
{
    "account": "account name of user",
    "authkey": "authentication key for future atheme use",
    "uid":     "unique identifier of user"
}
```
