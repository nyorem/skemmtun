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

- A configuration file named `mal.txt` is needed. The format must be the following:
```
username = foo
password = bar
```

## Commands included

- list:
```
skemmtun manga list username
skemmtun anime list username
```

## References

- [Unofficial MAL API](https://github.com/chuyeow/myanimelist-api)
- [Official API](http://myanimelist.net/modules.php?go=api)

