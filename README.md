### cl-parse-bookmarks

Parse Google Chrome Bookmarks

#### Useage

1. Export your Chrome Bookmarks to html.

2. Read bookmarks html file.

```lisp
(parse-bookmarks-from-html "bookmarks.html")
```

3. Get the bookmarks instance.

```lisp
(list-bookmark-items)
```

4. Save the bookmark items to file.

```lisp
(save-bookmark-items)
```
