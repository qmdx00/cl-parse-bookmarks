### cl-parse-bookmarks

Parse Google Chrome Bookmarks

#### Useage

1. Export your Chrome Bookmarks to html file.

2. Install asdf package.

```lisp
(asdf:load-system :cl-parse-bookmarks)
```

3. Read bookmarks html file.

```lisp
(cl-parse-bookmarks:parse-bookmarks-from-html "bookmarks.html")
```

4. Get the bookmarks instance.

```lisp
(cl-parse-bookmarks:list-bookmark-items)
```

5. Save the bookmark items to file.

```lisp
(cl-parse-bookmarks:save-bookmark-items "bookmarks.db")
```
