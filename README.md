# skemmtun

## What is it?

skemmtun (*entertainment* in Icelandic) is a [MAL](http://myanimelist.net) client written in Haskell.

## How to build/run?

- Building:
```
stack build
```

- Running:
```
stack exec mal command
```
where `command` is one of the commands described below.

- A configuration file named `$HOME/.mal.conf` is needed. The format must be the following:
```
username = foo
password = bar
```

## Commands included

- list (mangas / animes):
```
mal -m list [status] [username]
mal -a list [status] [username]
```

- inc (increment chapter / episode):
```
mal -m inc name
mal -a inc name
```

- incv (increment volume):
```
mal -m incv name
```

- set:
```
mal -m set --status onhold name
mal -a set --score 7 name
mal -m set --read 100 name
mal -m set --readv 20 name
mal -a set --watched 13 name
```

- search/add:
```
mal -a search name
```

- delete:
```
mal -m delete id
```

## References

- [Unofficial MAL API](https://github.com/chuyeow/myanimelist-api)
- [Official API](http://myanimelist.net/modules.php?go=api)
- [Comparison of Anime Library APIs](http://taiga.erengy.com/api.html)

