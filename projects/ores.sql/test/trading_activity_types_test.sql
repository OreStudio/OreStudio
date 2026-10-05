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
 * pgTAP tests for the activity type axes and priority.
 *
 * Tests cover:
 * - Sample seeded types state the axes and priority the trade activity note gives
 * - Only the null amend is not real
 * - A type that is not real cannot be economic or confirmable
 * - The priority stays within the versioning order's ranks
 *
 * Run with: pg_prove -d <database> test/trading_activity_types_test.sql
 */

begin;

select plan(8);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temp view t_types as
select code, requires_confirmation, is_economic, is_real, priority
from ores_trading_activity_types_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn();

select results_eq(
    $$select requires_confirmation, is_economic, is_real, priority
      from t_types where code = 'transfer'$$,
    $$values (false, false, true, 1)$$,
    'a book move is internal, not economic, and the highest priority');

select results_eq(
    $$select requires_confirmation, is_economic, is_real, priority
      from t_types where code = 'rebook'$$,
    $$values (true, true, true, 8)$$,
    'a misbooking rebook is confirmable and economic, and ranks as a misbooking');

select results_eq(
    $$select requires_confirmation, is_economic, is_real, priority
      from t_types where code = 'rate_reset'$$,
    $$values (false, true, true, 7)$$,
    'a rate reset is economic, needs no confirmation and ranks as a fixing');

select results_eq(
    $$select requires_confirmation, is_economic, is_real, priority
      from t_types where code = 'null_amend'$$,
    $$values (false, false, false, 9)$$,
    'a null amend is real on no axis');

select is(
    (select string_agg(code, ',' order by code) from t_types where not is_real),
    'null_amend',
    'only the null amend is not real');

select is(
    (select count(*) from t_types where priority not between 1 and 9),
    0::bigint,
    'every seeded priority is a rank of the versioning order');

select throws_ok(
    $$insert into ores_trading_activity_types_tbl (code, tenant_id, version, category,
          requires_confirmation, is_economic, is_real, priority,
          modified_by, change_reason_code, change_commentary)
      values ('test_unreal_economic', ores_utility_system_tenant_id_fn(), 0, 'lifecycle_event',
          false, true, false, 9,
          (select username from ores_iam_accounts_tbl where account_type = 'service'
             and valid_to = ores_utility_infinity_timestamp_fn() order by username limit 1),
          'system.initial_load', 'test')$$,
    '23514',
    null,
    'a type that is not real cannot be economic');

select throws_ok(
    $$insert into ores_trading_activity_types_tbl (code, tenant_id, version, category,
          requires_confirmation, is_economic, is_real, priority,
          modified_by, change_reason_code, change_commentary)
      values ('test_rank_ten', ores_utility_system_tenant_id_fn(), 0, 'lifecycle_event',
          false, false, true, 10,
          (select username from ores_iam_accounts_tbl where account_type = 'service'
             and valid_to = ores_utility_infinity_timestamp_fn() order by username limit 1),
          'system.initial_load', 'test')$$,
    '23514',
    null,
    'a priority outside the versioning order is refused');

select * from finish();

rollback;
