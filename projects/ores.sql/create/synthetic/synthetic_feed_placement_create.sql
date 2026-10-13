/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

/**
 * Feed placement
 *
 * A feed names its collection and may name no folder: a feed added by hand names none. A
 * screen lists feeds by their folders, so a feed with no folder would sit outside the tree.
 * The trigger below runs after the generated insert trigger has resolved the tenant. It puts a
 * feed that names no folder in the instrument type folder the publish function uses for its kind,
 * under the collection's asset class folder, and makes the folders that are missing, the
 * root of the party and the folders of the collection among them.
 */

create or replace function ores_synthetic_place_feed_in_folder_fn()
returns trigger as $$
declare
    v_asset_class_name text;
    v_instrument_name text;
    v_config_name text;
    v_root uuid;
    v_collection uuid;
    v_asset_class uuid;
begin
    if NEW.folder_id is not null then
        return NEW;
    end if;

    if tg_table_name = 'ores_synthetic_fx_spot_generation_configs_tbl' then
        v_asset_class_name := 'FX';
        v_instrument_name := 'FX Rates';
    else
        v_asset_class_name := 'Rates';
        v_instrument_name := 'IR Curves';
    end if;

    select name into v_config_name
    from ores_synthetic_market_data_generation_configs_tbl
    where tenant_id = NEW.tenant_id and id = NEW.config_id
      and valid_to = ores_utility_infinity_timestamp_fn();
    if v_config_name is null then
        return NEW;
    end if;

    select id into v_root
    from ores_synthetic_folders_tbl
    where tenant_id = NEW.tenant_id and party_id = NEW.party_id
      and parent_id is null and kind = 'root'
      and valid_to = ores_utility_infinity_timestamp_fn();
    if v_root is null then
        v_root := gen_random_uuid();
        insert into ores_synthetic_folders_tbl (
            tenant_id, id, version, party_id, parent_id, name, kind, collection_id,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            NEW.tenant_id, v_root, 0, NEW.party_id, null, 'Synthetic', 'root', null,
            NEW.modified_by, NEW.performed_by, NEW.change_reason_code, 'Root folder for synthetic feeds'
        );
    end if;

    select id into v_collection
    from ores_synthetic_folders_tbl
    where tenant_id = NEW.tenant_id and party_id = NEW.party_id
      and kind = 'collection' and collection_id = NEW.config_id
      and valid_to = ores_utility_infinity_timestamp_fn();
    if v_collection is null then
        v_collection := gen_random_uuid();
        insert into ores_synthetic_folders_tbl (
            tenant_id, id, version, party_id, parent_id, name, kind, collection_id,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            NEW.tenant_id, v_collection, 0, NEW.party_id, v_root, v_config_name, 'collection', NEW.config_id,
            NEW.modified_by, NEW.performed_by, NEW.change_reason_code, 'Collection folder for ' || v_config_name
        );
    end if;

    select id into v_asset_class
    from ores_synthetic_folders_tbl
    where tenant_id = NEW.tenant_id and party_id = NEW.party_id
      and kind = 'asset_class' and parent_id = v_collection and name = v_asset_class_name
      and valid_to = ores_utility_infinity_timestamp_fn();
    if v_asset_class is null then
        v_asset_class := gen_random_uuid();
        insert into ores_synthetic_folders_tbl (
            tenant_id, id, version, party_id, parent_id, name, kind, collection_id,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            NEW.tenant_id, v_asset_class, 0, NEW.party_id, v_collection, v_asset_class_name, 'asset_class', null,
            NEW.modified_by, NEW.performed_by, NEW.change_reason_code, 'Asset class folder'
        );
    end if;

    select id into NEW.folder_id
    from ores_synthetic_folders_tbl
    where tenant_id = NEW.tenant_id and party_id = NEW.party_id
      and kind = 'instrument_type' and parent_id = v_asset_class and name = v_instrument_name
      and valid_to = ores_utility_infinity_timestamp_fn();
    if NEW.folder_id is null then
        NEW.folder_id := gen_random_uuid();
        insert into ores_synthetic_folders_tbl (
            tenant_id, id, version, party_id, parent_id, name, kind, collection_id,
            modified_by, performed_by, change_reason_code, change_commentary
        ) values (
            NEW.tenant_id, NEW.folder_id, 0, NEW.party_id, v_asset_class, v_instrument_name, 'instrument_type', null,
            NEW.modified_by, NEW.performed_by, NEW.change_reason_code, 'Instrument type folder'
        );
    end if;

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- The name sorts after the generated insert trigger, which fires first and resolves the tenant.
create or replace trigger ores_synthetic_fx_spot_generation_configs_place_trg
before insert on "ores_synthetic_fx_spot_generation_configs_tbl"
for each row execute function ores_synthetic_place_feed_in_folder_fn();

create or replace trigger ores_synthetic_ir_curve_generation_configs_place_trg
before insert on "ores_synthetic_ir_curve_generation_configs_tbl"
for each row execute function ores_synthetic_place_feed_in_folder_fn();
