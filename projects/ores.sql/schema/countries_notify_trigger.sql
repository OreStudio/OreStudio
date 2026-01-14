-- Trigger function to send notifications on country changes
create or replace function ores.notify_country_changes()
returns trigger as $$
declare
    notification_payload jsonb;
    entity_name text := 'ores.refdata.country';
    change_timestamp timestamptz := NOW();
    changed_alpha2_code text;
begin
    -- Get the alpha-2 code of the changed country
    if TG_OP = 'DELETE' then
        changed_alpha2_code := OLD.alpha2_code;
    else
        changed_alpha2_code := NEW.alpha2_code;
    end if;

    -- Construct the JSON payload with entity_ids
    notification_payload := jsonb_build_object(
        'entity', entity_name,
        'timestamp', to_char(change_timestamp, 'YYYY-MM-DD HH24:MI:SS'),
        'entity_ids', jsonb_build_array(changed_alpha2_code)
    );

    -- Notify on the 'ores_countries' channel
    perform pg_notify('ores_countries', notification_payload::text);

    return null; -- AFTER triggers can return NULL
end;
$$ language plpgsql;

-- trigger to fire after insert, update, or delete on the countries table
create or replace trigger country_change_notify_trigger
after insert or update or delete on ores.countries
for each row execute function ores.notify_country_changes();
