## General idea and requirements

- tags
- tag inheritance. Обязательно множественная наследуемость, т.е., например, тег J.S.Bach относится и к тегу music и к тегу people; тег php относится и к тегу programming language и к тегу "зашквар".
- folders are not needed, tags will be used instead
- filtering by multiple tags
- saving the shapshot of the page when a bookmark is added
- поиск по тайтлам, description, с возможностью одновременной фильтрацией по тегам, если нужно
- сортировка по всему чему только можно. Обязательно с возможностью сортировать по нескольким полям одновременно.
- отображение тегов в виде директорий как в файловой системе; возможность видеть всех парентов открытого тега где-нибудь в статус баре или еще как-нибудь; возможность видеть сколько всего букмарков принадлежит тегу; возможность видеть просто все букмарки (flat mode)
- синхронизация букмарков между разными браузерами
- importing bookmarks from firefox

## Model

![model](./model.png)

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
- проверяем, что букмарка с таким же урлом еще нет  
- добавляем букмарк в базу  
- досоздаем теги, которых нет  
- добавляем в `bookmark_tag` таблицу все теги этого букмарка.  
Важный момент: теги букмарка это не только переданные в массиве теги, но и еще все паренты этих тегов, паренты этих парентов и т.д.  
Поэтому если юзер создает букмарк с тегом `J.S.Bach`, то тег `music` должен подхватиться автоматически.  
- отдаем `id` букмарка сервису, который асинхронно сделает снапшот и обновит `shapshot_url` букмарка  
- отдаем `id` букмарка сервису, который асинхронно создаст (или найдет уже существующий) favicon и обновит `favicon_id` букмарка

## Relations between tags

Root tags - это теги у которых нету парента в таблице `tag_arrow`.  
Получить все root tags:
```
GET /api/tags/roots
```
Получить все чайлд теги данного тега:
```
GET /api/tags/{id}/children
```
Получить все родительские теги данного тега:
```
GET /api/tags/{id}/parents
```
Получить все букмарки тега:
```
GET /api/tags/{id}/bookmarks
```
Get all root bookmarks (that have no tags):
```
GET /api/bookmarks/roots
```
Это все для навигации как по дереву каталогов в файловой системе. Реализация запросов очевидна.

Чтобы создать связь парент-чайлд между тегами:
```
POST /api/tags/{id}/parents/{parent_tag_id}
```
Как будет работать:  
- нужно не допустить создание циклов в графе https://en.wikipedia.org/wiki/Cycle_graph#Directed_cycle_graph т.е. надо проверить, что не образуется цикл. Т.е. должен получиться https://en.wikipedia.org/wiki/Directed_acyclic_graph  
- добавить связь в таблицу `tag_arrow`  
- добавить связи в таблицу `bookmark_tag`: если букмарк имеет тег `{id}`, то добавить ему еще и тег `{parent_tag_id}` (и еще парентов этого парента и т.д.)

Чтобы разорвать связь парент-чайлд между тегами:
```
DELETE /api/tags/{id}/parents/{parent_tag_id}
```
- удалить связь в таблице `tag_arrow`  
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
POST /api/visit
{
  "url": "https://example.com/",
  "visit_date": 123123123
}
```
- check that `url` is present
- check that `visit_date` is present and it is a valid timestamp, and it is not older that one minute from now
- find the bookmark with such `url`
- update bookmark's `last_visit_date` to `visit_date` and increment `visit_count`

## Само собой разумеющееся  
Взять букмарк по `id`:
```
GET /api/bookmarks/{id}
```
Взять тег по `id`:
```
GET /api/tag/{id}
```
Сортировка по нескольким филдам (- для DESCENDING):
```
GET /api/bookmarks?sort=-date_added,visit_count
```
Должно работать и для всех остальных запросов, возвращающих списки.

## Search

todo: describe how searching/filtering will work

поиск:  
- по нескольким тегам  
- по тексту в тайтле  
- 
