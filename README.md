An attempt to apply [package principles] (https://docs.google.com/file/d/0BwhCYaYDn8EgZjI3OTU4ZTAtYmM4Mi00MWMyLTgxN2YtMzk5YTY1NTViNTBh/edit?hl=en)
to Clojure namespaces. Outputs a table showing dependency count, dependent count, and instability factor for each source namespace.

```bash
$ lein install
```

```clojure
:plugins [[lein-instability "0.0.1"]]
```

```bash
$ lein instability :table
```

```bash
$ lein instability :tree
```

```bash
$ lein instability :reuse
```

