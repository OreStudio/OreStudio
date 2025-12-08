-- Drop the notification trigger
DROP TRIGGER IF EXISTS currency_change_notify_trigger ON oresdb.currencies;

-- Drop the notification function
DROP FUNCTION IF EXISTS oresdb.notify_currency_changes();