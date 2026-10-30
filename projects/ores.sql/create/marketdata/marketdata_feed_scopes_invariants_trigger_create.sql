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
 * Feed Scope Invariants
 *
 * A scope row names a feed of the system tenant. No item belongs to two feeds of
 * one publisher, and a vendor instrument it names belongs to the feed's
 * publisher. The checks are here rather than in codegen because they read other
 * rows, not only the row being written.
 */

create or replace function ores_marketdata_validate_feed_scope_fn()
returns trigger as $$
declare
    v_publisher text;
begin
    select publisher_code into v_publisher
    from ores_marketdata_feeds_tbl
    where tenant_id = ores_utility_system_tenant_id_fn()
      and id = NEW.feed_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_publisher is null then
        raise exception 'Invalid feed scope: feed % is not in the catalogue.', NEW.feed_id
            using errcode = '23503';
    end if;

    if NEW.vendor_instrument_id is not null then
        if not exists (
            select 1 from ores_marketdata_market_data_vendor_instruments_tbl
            where tenant_id = ores_utility_system_tenant_id_fn()
              and id = NEW.vendor_instrument_id
              and provider_code = v_publisher
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid feed scope: vendor instrument % is not a vendor instrument of publisher %.',
                NEW.vendor_instrument_id, v_publisher
                using errcode = '23503';
        end if;
    end if;

    if exists (
        select 1
        from ores_marketdata_feed_scopes_tbl s
        join ores_marketdata_feeds_tbl f on f.id = s.feed_id
        where s.tenant_id = ores_utility_system_tenant_id_fn()
          and s.valid_to = ores_utility_infinity_timestamp_fn()
          and s.item = NEW.item
          and s.id <> NEW.id
          and f.valid_to = ores_utility_infinity_timestamp_fn()
          and f.publisher_code = v_publisher
    ) then
        raise exception 'Invalid feed scope: item % already belongs to another feed of publisher %.',
            NEW.item, v_publisher
            using errcode = '23514';
    end if;

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_marketdata_feed_scopes_invariants_trg
before insert or update of feed_id, item, vendor_instrument_id
on ores_marketdata_feed_scopes_tbl
for each row execute function ores_marketdata_validate_feed_scope_fn();
