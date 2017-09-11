# lock-manager

## Development

```bash
$ lein repl 
```
```clojure
;; for starting 
user> (go)

;; reload any code change and restart everything
dev> (reset)
```

## Production

```bash
$ lein uberjar

$ java -jar ./target/uberjar/lock-manager-0.1.0-standalone.jar
```

## Swagger UI

http://localhost:1234/api-docs/index.html
