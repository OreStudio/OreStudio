-- Trigger function to send notifications on currency_images table changes
--
-- This ensures that when a currency's flag is updated (or a new mapping is
-- created/deleted), a notification is sent out. The ImageCache on the client
-- listens for this event to invalidate its cache and reload the currency icons.

create or replace function assets.notify_currency_image_changes()
returns trigger as $$
declare
    notification_payload jsonb;
    entity_name text := 'ores.assets.currency_image';
    change_timestamp timestamptz := NOW();
    changed_iso_code text;
begin
    -- Get the ISO code of the changed currency image mapping
    if TG_OP = 'DELETE' then
        changed_iso_code := OLD.iso_code;
    else
        changed_iso_code := NEW.iso_code;
    end if;

    -- Construct the JSON payload matching entity_change_event structure
    notification_payload := jsonb_build_object(
        'entity', entity_name,
        'timestamp', to_char(change_timestamp, 'YYYY-MM-DD"T"HH24:MI:SS.MS"Z"'),
        'entity_ids', jsonb_build_array(changed_iso_code)
    );

    -- Notify on the 'ores_currency_images' channel
    perform pg_notify('ores_currency_images', notification_payload::text);

    return null; -- result is ignored since this is an AFTER trigger
end;
$$ language plpgsql;

-- Drop the trigger if it exists to ensure a clean setup
drop trigger if exists currency_image_change_notify_trigger on assets.currency_images;

-- Create the trigger to execute the function after any row change
create trigger currency_image_change_notify_trigger
after insert or update or delete on assets.currency_images
for each row execute function assets.notify_currency_image_changes();
