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
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: sql_schema_domain_entity_view_create.mustache
 * To modify, update the template and regenerate.
 *
 * Trade Summary View
 *
 * The read model under the trade list. A row holds a trade with its book, its
 * current state, its booking, its counterparty, its instrument type and its
 * authorisation status, and every identifier of the trade as one search text.
 * The service pages, sorts and filters, so the client does no joining.
 *
 * The row reads the current state of every part. It offers no as_of: a view
 * cannot take the instant, and the trade list does not need it yet.
 */

create or replace view "ores_trading_trade_summaries_vw"
with (security_invoker = true) as
select
    t.tenant_id,
    t.id,
    'trade'::text as row_kind,
    t.party_id,
    t.counterparty_id,
    b.book_id,
    t.trade_type as instrument_type,
    st.name as status,
    case
        when ar.state_code is not null then ar.state_code
        when t.booking_nature = 'actual' then 'not_requested'
        else 'not_required'
    end as authorisation_status,
    b.trade_date,
    1 as trade_count,
    t.id::text || ' ' || coalesce(ids.id_values, '') as search_text
from ores_trading_trades_tbl t
left join ores_trading_trade_bookings_tbl b
  on b.tenant_id = t.tenant_id
 and b.trade_id = t.id
 and b.valid_to = ores_utility_infinity_timestamp_fn()
left join ores_trading_trade_states_tbl ts
  on ts.tenant_id = t.tenant_id
 and ts.trade_id = t.id
 and ts.valid_to = ores_utility_infinity_timestamp_fn()
left join ores_dq_fsm_states_tbl st
  on st.id = ts.status_id
 and st.valid_to = ores_utility_infinity_timestamp_fn()
left join lateral (
    select r.state_code
    from ores_trading_trade_authorisation_requests_tbl a
    join ores_inbox_approval_requests_tbl r
      on r.tenant_id = a.tenant_id
     and r.id = a.request_id
     and r.valid_to = ores_utility_infinity_timestamp_fn()
    where a.tenant_id = t.tenant_id
      and a.trade_id = t.id
      and a.valid_to = ores_utility_infinity_timestamp_fn()
    order by a.trade_version desc
    limit 1
) ar on true
left join lateral (
    select string_agg(i.id_value, ' ' order by i.id_type) as id_values
    from ores_trading_trade_identifiers_tbl i
    where i.tenant_id = t.tenant_id
      and i.trade_id = t.id
      and i.valid_to = ores_utility_infinity_timestamp_fn()
) ids on true
where t.valid_to = ores_utility_infinity_timestamp_fn();
