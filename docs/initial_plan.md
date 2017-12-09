# General idea and requirements

- Tags.
- Tag inheritance. Multiple inheritance must be supported, i.e., for example, "J.S.Bach" tag is a descendant of "music" tag as well as of "people" tag; "php" tag is a descendant of "programming language" tag as well as "зашквар" tag.
- Three will be no folders because we will use tags instead.
- Filtering by multiple tags.
- Saving the snapshot of the page when a bookmark is added.
- Searching by title, description, and at the same time it must be possible to filer by tags.
- Sorting by every possible filed. Moreover, it must support sorting by multiple fields.
- Displaying tags as directories in a file system; possibility to see all parents of an opened tag somewhere in a status bar or somehow else; possibility to see how many bookmarks a tag have; possibility to just see all bookmarks (flat mode.)
- Synchronizing bookmarks between multiple different browsers.
- Importing bookmarks from firefox.

# Model

![model](./model.png)

# API

## Create bookmark

```
POST /api/bookmarks
{
  "url": "https://example.com/",
  "title": "Example",
  "description": "example",
  "tags": ["tag1", "tag2"]
}
```
How it will work:
- check that `url` is present and it is a valid url
- check that `title` and at least one tag are present
- check that there is no bookmark with the same name yet
- insert the bookmark into `bookmark` table
- insert nonexistent tags into `tag` table
- for this bookmark, insert all tags into `bookmark_tag` table
- start a background async task that for each tag finds all its parents, grandparents and so on and inserts them into `bookmark_tag` table.  
It is assumed and expected that "tags" array already contains all of them, but we should check it anyway.
- give `id` of the bookmark to a service that will asynchronously take a snapshot of the page and update `snapshot_url` of the bookmark
- give `id` of the bookmark to a service that will asynchronously get (or find already exising) favicon and update `favicon_id` of the bookmark

## Relations between tags

Root tags are tags that have no parents in `tag_arrow` table.  
Get all root tags:
```
GET /api/tags/roots
```
Get all children of the tag:
```
GET /api/tags/{id}/children
```
Get all parent tags of the tag:
```
GET /api/tags/{id}/parents
```
Get all bookmarks of the tag:
```
GET /api/tags/{id}/bookmarks
```
Get all root bookmarks (that have no tags):
```
GET /api/bookmarks/roots
```
This is all that is needed to navigate like in a  file system. The implementation of the requests is obvious.

Get tag's parents, grandparents and so on:
```
GET /api/tags/{id}/ascendants
```

To create a new parent-child relation between tags:
```
POST /api/tags/{id}/parents/{parent_tag_id}
```
How it will work:  
- we have to avoid creating a cycle in the graph https://en.wikipedia.org/wiki/Cycle_graph#Directed_cycle_graph i.e. we need to check that we do not add a cycle. As a result it will be https://en.wikipedia.org/wiki/Directed_acyclic_graph
- add the record into `tag_arrow` table
- add the records into `bookmark_tag` table, i.e. for each bookmark if a bookmark has `{id}` tag, then add also `{parent_tag_id}` tag (and also its parenets, grandparents and so on)

To remove parent-child relation between tags:
```
DELETE /api/tags/{id}/parents/{parent_tag_id}
```
- delete the record from `tag_arrow` table
- удалить связи в таблице `bookmark_tag`: если букмарк имеет и тег `{id}`, и тег `{parent_tag_id}`, то отвязать второй. И надо отвязать еще парентов этого парента и т.д. Но нельзя отвязывать других парентов, грандпарентов и т.д. тега `{id}`. Т.е. отвязать всех, кроме этих.

## Delete bookmark

```
DELETE /api/bookmarks/{id}
```
- delete records from `bookmark_tag` table where `bookmark_id = {id}`
- delete the record from `bookmark` table

## Delete tag

A tag can be deleted only when it has no bookmarks and no children.
```
DELETE /api/tags/{id}
```
- check that there are no records in `bookmark_tag` table with `tag_id = {id}`
- check that there are no records in `tag_arrow` table with `parent_tag_id = {id}`
- delete the record from `tag` table

## Edit bookmark
```
PATCH /api/bookmarks/{id}
{
  "url": "https://example.com/",
  "title": "Example",
  "description": "example",
  "tags": ["tag1", "tag2"]
}
```
- if `url` is present, check if it is valid
- if `tags` is present, delete from `bookmark_tag` records where `bookmark_id = {id}` and add all tags from `tags` field the same way we do it when a bookmark is being created
- update the fields of the record in `bookmark` table

## Update bookmark's visit info

I hope it is possible for web extensions to  specify some function that will be called every time a url is visited.
```
POST /api/visits
{
  "url": "https://example.com/",
  "visit_date": 123123123
}
```
- check that `url` is present
- check that `visit_date` is present and it is a valid timestamp, and it is not older that one minute from now
- find the bookmark with such `url`
- update bookmark's `last_visit_date` to `visit_date` and increment `visit_count`

## Simple get requests

Get the bookmark by `id`:
```
GET /api/bookmarks/{id}
```
Get the tag by `id`:
```
GET /api/tags/{id}
```

## Sort

Sort by multiple fields ("-" means DESCENDING):
```
GET /api/bookmarks?sort=-date_added,visit_count
```
It should also work for all other requests that return lists.

## Offset and limit for lists

For list requests, it must be possible to pass "skip" and "take" parameters (as numbers of course).

## Search

todo: describe how searching/filtering will work

поиск:  
- по нескольким тегам  
- по тексту в тайтле  
- 

# UI
## Flat mode

This view looks pretty much the same as in firefox.

We show a grid of all bookmarks with columns "Name", "Tags", "Url", "Descrption", "Added", "Most Recent Visit", "Last Modified", "Visit Count", "Snapshot". "Name" contains titles. Before each title the favicon is shown.

The grid is virtualized, i.e. it shows the limited amount of bookmarks and loads more when a user has scrolled to the bottom.

"Tags" contains comma-separated list of all tags of the bookmark. "Snapshot" contains the link to the snapshot of the page.

It must be possible to hide/show columns the same way as in firefox:  
![show/hide columns](show_hide_columns.PNG)

Except column names are different. By default columns "Most Recent Visit", "Last Modified", "Visit Count" are hidden.

When a bookmark is dbl-clicked, we open it in the current tab. When it is right-clicked, we show a context menu with the same options as in firefox.

It must be possible to rearrange columns.

Grid can be ordered by each column in desc/asc order. By default it is ordered by "Added" desc. It must be possible to order by multiple columns. In this case columns for ordering are taken from left to right and if the columns are rearranged, it affects ordering.

The state of the grid must be persistent. It includes columns visibility, arrangement and ordering.

When a bookmark is selected, it shows a form at the bottom. The form has the following fields (single line): "Name", "Url" and "Tags". But there must be button "More" below that adds "Descrption" field (multi-line).

"Tags" field is a multiselect combobox. There must be a button on the right side which opens the list of all tags sorted by name (it must be virtualized, i.e. only a limited amount of items is loaded and it loads more on scrolling). When a user begins to type a tag name (at least one symbol is entered), the (virtualized) list of all tags that starts with this text is shows.

After a tag has been added, its parents (and grandparents and so on) are loaded and they must be shown below the combobox as "_derived:_ ptag1, ptag2, ptag3". If later the user adds one of these parent tags manually, it will disappear from "_derived_" list.

Tags that are already added must be excluded from the suggestion list.

## Hierarchy mode

It is a grid that lists tags and bookmarks. Columns are the same as in the flat mode, except one more is added - "B.
Tags here are like directories in a file system. For tags, values in columns "Url", "Descrption", "Most Recent Visit", "Visit Count", "Snapshot" are empty; "Name" is the name of a tag and there should be the same icon before each tag name; "Tags" contains all parent tags of the tag.

Tags are always grouped, i.e. the grid is always sorted by the type of entity first. Sorting functionality is the same as in the flat mode.
Also there is the same show/hide feature. Click/dbl-click/rigt-click on a bookmark also works the same.

This grid also loads more items on srolling.

At the root level, the grid contains all root tags and all not tagged bookmarks. 
If a tag is dbl-clicked, it shows all its children tags and all bookmarks that have this tag.

If a tag is right-clicked, it show a context menu with the following options: "**Open**", "Open all in tabs", "New Tag", "New Bookmark", "Cut", "Copy", "Paste", "Delete".

If a tag is copied and pasted somewhere, it will  create a new parent tag for it and all its bookmarks will have this parent tag.

If a tag is cut and pasted somewhere, it will no longer have its old parent and so its bookmarks.

A tag will be completely deleted only if it has 0 or 1 parent and no bookmarks. Otherwise, only its relation with one of its parents will be deleted (which will affect its bookmarks accordingly.)

