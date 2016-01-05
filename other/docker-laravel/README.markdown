Laravel Docker Container
========================

This is a dead simple `ONBUILD` container for Laravel PHP applications. This is 
designed to make the resulting dockerfile down to:

```Dockerfile
FROM xena/laravel
```

And everything will automatically build from there. This does use Apache and as 
such using this in a high traffic site should be done with care.

---

This draws inspiration from:

- https://github.com/niaquinto/docker-laravel
- https://github.com/tutumcloud/tutum-docker-php
