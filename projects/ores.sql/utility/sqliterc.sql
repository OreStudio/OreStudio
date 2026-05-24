-- =====================================================================
-- SQLite command-line shell configuration (.sqliterc)
-- ---------------------------------------------------------------------
-- Quality-of-life defaults for inspecting ORE Studio's SQLite databases
-- from the `sqlite3` shell. These are shell *dot-commands* and PRAGMAs,
-- not SQL DDL, despite the `.sql` extension (project convention).
--
-- Use it one of two ways:
--   * Per session:  sqlite3 -init projects/ores.sql/utility/sqliterc.sql my.db
--   * Always on:     source it from your global ~/.sqliterc with .read,
--                    which preserves any settings you already have there:
--                    echo ".read $PWD/projects/ores.sql/utility/sqliterc.sql" >> ~/.sqliterc
--
-- Requires SQLite 3.37.0+ (Nov 2021) for the columnar `--wrap` option below.
-- =====================================================================

-- ---------------------------------------------------------------------
-- Output formatting
-- ---------------------------------------------------------------------
-- Show column names as a header row above each result set.
.headers on

-- Render results as a Unicode box-drawing grid. `--wrap 0` disables line
-- wrapping: columns grow to fit their widest value instead of being
-- folded onto multiple lines. (Plain `.mode box` wraps long values; this
-- is the "larger columns, no wrapping" behaviour we want.)
-- Needs SQLite 3.37.0+; older shells reject the `--wrap` option.
.mode box --wrap 0

-- Print a visible marker for NULL so it is not confused with an empty
-- string or whitespace.
.nullvalue '∅'

-- ---------------------------------------------------------------------
-- Interactive ergonomics
-- ---------------------------------------------------------------------
-- Report wall-clock / CPU time taken by each statement.
.timer on

-- After INSERT/UPDATE/DELETE, print how many rows were affected, so a
-- mistyped WHERE clause is obvious immediately.
.changes on

-- Do not re-echo each command back to the terminal (this is the default;
-- stated explicitly so it survives an inherited `.echo on`).
.echo off

-- Branded primary prompt plus an indented continuation prompt, so it is
-- clear when the shell is waiting for the rest of a multi-line statement.
.prompt 'ore> ' '  ...> '

-- ---------------------------------------------------------------------
-- Session settings (per connection)
-- ---------------------------------------------------------------------
-- Enforce foreign-key constraints. SQLite leaves these OFF by default;
-- turning them on catches referential-integrity mistakes during manual
-- DML. Scoped to this connection only.
PRAGMA foreign_keys = ON;

-- Wait up to 5s for a lock to clear instead of failing instantly with
-- "database is locked" — useful when the running app holds the DB open.
PRAGMA busy_timeout = 5000;

-- PRAGMA journal_mode = WAL; -- Better read/write concurrency, BUT this
--   is persisted in the database file and affects every connection, so
--   set it deliberately against the file, not from an interactive rc.

-- ---------------------------------------------------------------------
-- Handy levers (left off by default)
-- ---------------------------------------------------------------------
-- .width N1 N2 ...   Set *minimum* column widths (0 = auto). Pads short
--                    columns; combine with `--wrap 0` above to never wrap.
-- .once FILE         Send only the next query's output to FILE.
-- .output FILE       Redirect all subsequent output to FILE (.output to reset).
-- .excel             Open the next result set in your spreadsheet app.
-- .schema ?TABLE?    Show the CREATE statements for the schema (or one table).
