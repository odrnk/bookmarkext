# bookmarkext
Browser extension of bookmark system

## SQL migrations
[Flyway](https://flywaydb.org/) is used for migrations.
First, create the database manually:
```sql
DROP DATABASE IF EXISTS bm;
CREATE DATABASE bm;
```
Edit `backend/sql/flyway.conf`. Apply migrations. That's it.
