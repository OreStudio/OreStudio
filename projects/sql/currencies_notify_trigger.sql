-- Trigger function to send notifications on currency changes
create or replace function oresdb.notify_currency_changes()
returns trigger as $$
declare
    notification_payload jsonb;
    entity_name text := 'ores.risk.currency';
    change_timestamp timestamptz := NOW();
begin
    -- Construct the JSON payload
    notification_payload := jsonb_build_object(
        'entity', entity_name,
        'timestamp', to_char(change_timestamp, 'YYYY-MM-DD"T"HH24:MI:SS.MS"Z"')
    );

    -- Notify on the 'ores_currencies' channel
    perform pg_notify('ores_currencies', notification_payload::text);

    return null; -- AFTER triggers can return NULL
end;
$$ language plpgsql;

-- trigger to fire after insert, update, or delete on the currencies table
create or replace trigger currency_change_notify_trigger
after insert or update or delete on oresdb.currencies
for each row execute function oresdb.notify_currency_changes();
