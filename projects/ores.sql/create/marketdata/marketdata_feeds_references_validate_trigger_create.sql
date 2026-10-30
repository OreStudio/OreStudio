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
 * Feed Session Validation
 *
 * A feed names a session, and the session is a system-tenant row. The generated
 * insert trigger validates the publisher against the catalogue; this trigger
 * validates the session, which the codegen carries no per-column function for.
 */

create or replace function ores_marketdata_validate_feed_session_fn()
returns trigger as $$
begin
    if not exists (
        select 1 from ores_marketdata_sessions_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and code = NEW.session_code
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid feed: session % is not in the catalogue.', NEW.session_code
            using errcode = '23503';
    end if;

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_marketdata_feeds_session_validate_trg
before insert or update of session_code on ores_marketdata_feeds_tbl
for each row execute function ores_marketdata_validate_feed_session_fn();
