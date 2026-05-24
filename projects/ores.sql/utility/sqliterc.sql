-- -----------------------------------------------------
-- Output Formatting
-- -----------------------------------------------------
-- Displays column names in your SELECT output
.headers on
-- Uses a clean, bordered grid format for tables
.mode box
-- Prints '∅' instead of blank space for NULL values
.nullvalue '∅'
-- Measures execution time of SQL queries
.timer on

-- -----------------------------------------------------
-- Session Settings
-- -----------------------------------------------------
-- PRAGMA foreign_keys = ON; -- Ensures relational integrity (disabled by default)
-- PRAGMA journal_mode = WAL; -- Sets Write-Ahead Logging for better concurrency & speed

-- -----------------------------------------------------
-- Command Aliases (Custom Shortcuts)
-- -----------------------------------------------------
-- .alias .tables_info 'SELECT tbl_name, sql FROM sqlite_master WHERE type="table"'
