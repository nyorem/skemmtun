# skemmtun

## What is it?

skemmtun (entertainment in Icelandic) is a [MAL](http://myanimelist.net) client written in Haskell.

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
mal manga list [username]
mal anime list [username]
```

- inc (increment chapter / episode):
```
mal manga inc name
mal anime inv name
```

- incv (increment volume):
```
mal manga incv name
```

## References

- [Unofficial MAL API](https://github.com/chuyeow/myanimelist-api)
- [Official API](http://myanimelist.net/modules.php?go=api)
- [Comparison of Anime Library APIs](http://taiga.erengy.com/api.html)

