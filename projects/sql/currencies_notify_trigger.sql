-- Trigger function to send notifications on currency changes
CREATE OR REPLACE FUNCTION oresdb.notify_currency_changes()
RETURNS TRIGGER AS $$
DECLARE
    notification_payload jsonb;
    entity_name text := 'ores.risk.currency';
    change_timestamp timestamptz := NOW();
BEGIN
    -- Construct the JSON payload
    notification_payload := jsonb_build_object(
        'entity', entity_name,
        'timestamp', to_char(change_timestamp, 'YYYY-MM-DD"T"HH24:MI:SS.MS"Z"')
    );

    -- Notify on the 'ores_currencies' channel
    PERFORM pg_notify('ores_currencies', notification_payload::text);

    RETURN NULL; -- AFTER triggers can return NULL
END;
$$ LANGUAGE plpgsql;

-- Trigger to fire after INSERT, UPDATE, or DELETE on the currencies table
CREATE OR REPLACE TRIGGER currency_change_notify_trigger
AFTER INSERT OR UPDATE OR DELETE ON oresdb.currencies
FOR EACH ROW EXECUTE FUNCTION oresdb.notify_currency_changes();