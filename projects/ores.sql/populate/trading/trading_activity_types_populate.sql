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
 * Activity Types Population Script
 *
 * Seeds all internal trade activity types with their FpML event type mappings
 * and FSM transition links where applicable.
 *
 * fsm_transition_id is populated by looking up the transition name in the
 * trade_status machine. Types that don't change status have null transition.
 *
 * This script is idempotent.
 */

\echo '--- Activity Types ---'

do $$
declare
    v_sys_tenant uuid := ores_iam_system_tenant_id_fn();
    v_machine_id uuid;
    v_tr_initial_booking uuid;
    v_tr_confirm         uuid;
    v_tr_cancel_new      uuid;
    v_tr_expire          uuid;
    v_tr_cancel_live     uuid;
begin
    -- Look up the trade_status machine
    select id into v_machine_id
    from ores_dq_fsm_machines_tbl
    where tenant_id = v_sys_tenant
      and name = 'trade_status'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if not found then
        raise exception 'trade_status FSM machine not found. Run dq_fsm_trade_status_populate.sql first.';
    end if;

    -- Look up transitions by name
    select id into v_tr_initial_booking
    from ores_dq_fsm_transitions_tbl
    where tenant_id = v_sys_tenant
      and machine_id = v_machine_id
      and name = 'initial_booking'
      and valid_to = ores_utility_infinity_timestamp_fn();

    select id into v_tr_confirm
    from ores_dq_fsm_transitions_tbl
    where tenant_id = v_sys_tenant
      and machine_id = v_machine_id
      and name = 'confirm'
      and valid_to = ores_utility_infinity_timestamp_fn();

    select id into v_tr_cancel_new
    from ores_dq_fsm_transitions_tbl
    where tenant_id = v_sys_tenant
      and machine_id = v_machine_id
      and name = 'cancel_new'
      and valid_to = ores_utility_infinity_timestamp_fn();

    select id into v_tr_expire
    from ores_dq_fsm_transitions_tbl
    where tenant_id = v_sys_tenant
      and machine_id = v_machine_id
      and name = 'expire'
      and valid_to = ores_utility_infinity_timestamp_fn();

    select id into v_tr_cancel_live
    from ores_dq_fsm_transitions_tbl
    where tenant_id = v_sys_tenant
      and machine_id = v_machine_id
      and name = 'cancel_live'
      and valid_to = ores_utility_infinity_timestamp_fn();

    -- -------------------------------------------------------------------------
    -- New Activity category
    -- -------------------------------------------------------------------------
    insert into ores_trading_activity_types_tbl (
        code, tenant_id, version, category, requires_confirmation,
        description, fpml_event_type_code, fsm_transition_id,
        modified_by, change_reason_code, change_commentary
    ) values
        ('new_booking',
         v_sys_tenant, 0, 'new_activity', true,
         'Initial booking of a new trade.',
         'New', v_tr_initial_booking,
         current_user, 'system.initial_load', 'Seed activity types'),
        ('new_internal_transfer',
         v_sys_tenant, 0, 'new_activity', false,
         'Internal transfer creating a new trade record.',
         'New', v_tr_initial_booking,
         current_user, 'system.initial_load', 'Seed activity types'),

        -- -------------------------------------------------------------------------
        -- Lifecycle Event category
        -- -------------------------------------------------------------------------
        ('amendment',
         v_sys_tenant, 0, 'lifecycle_event', false,
         'Change to trade economics or contractual terms.',
         'Amendment', null,
         current_user, 'system.initial_load', 'Seed activity types'),
        ('confirmation',
         v_sys_tenant, 0, 'lifecycle_event', false,
         'Trade confirmed by both counterparties; status transitions to live.',
         null, v_tr_confirm,
         current_user, 'system.initial_load', 'Seed activity types'),
        ('novation',
         v_sys_tenant, 0, 'lifecycle_event', true,
         'Transfer of a trade to a new counterparty.',
         'Novation', null,
         current_user, 'system.initial_load', 'Seed activity types'),
        ('partial_termination',
         v_sys_tenant, 0, 'lifecycle_event', false,
         'Reduction of notional on a live trade.',
         'PartialTermination', null,
         current_user, 'system.initial_load', 'Seed activity types'),
        ('full_termination',
         v_sys_tenant, 0, 'lifecycle_event', false,
         'Full early termination of a live trade.',
         'FullTermination', v_tr_cancel_live,
         current_user, 'system.initial_load', 'Seed activity types'),
        ('maturity',
         v_sys_tenant, 0, 'lifecycle_event', false,
         'Trade has reached its contractual maturity date.',
         null, v_tr_expire,
         current_user, 'system.initial_load', 'Seed activity types'),
        ('exercise',
         v_sys_tenant, 0, 'lifecycle_event', false,
         'Option exercise event.',
         null, null,
         current_user, 'system.initial_load', 'Seed activity types'),
        ('assignment',
         v_sys_tenant, 0, 'lifecycle_event', false,
         'Assignment of trade rights to another party.',
         'Novation', null,
         current_user, 'system.initial_load', 'Seed activity types'),
        ('increase',
         v_sys_tenant, 0, 'lifecycle_event', false,
         'Increase of notional on a live trade.',
         'Amendment', null,
         current_user, 'system.initial_load', 'Seed activity types'),
        ('decrease',
         v_sys_tenant, 0, 'lifecycle_event', false,
         'Decrease of notional on a live trade (partial unwind).',
         'PartialTermination', null,
         current_user, 'system.initial_load', 'Seed activity types'),
        ('roll',
         v_sys_tenant, 0, 'lifecycle_event', false,
         'Extension of maturity date (roll or extension).',
         'Amendment', null,
         current_user, 'system.initial_load', 'Seed activity types'),
        ('compression',
         v_sys_tenant, 0, 'lifecycle_event', false,
         'Portfolio compression: partial termination of offsetting trades.',
         'PartialTermination', null,
         current_user, 'system.initial_load', 'Seed activity types'),
        ('clearing',
         v_sys_tenant, 0, 'lifecycle_event', false,
         'Trade submitted for central clearing.',
         null, null,
         current_user, 'system.initial_load', 'Seed activity types'),
        ('netting',
         v_sys_tenant, 0, 'lifecycle_event', false,
         'Netting of offsetting positions.',
         'FullTermination', null,
         current_user, 'system.initial_load', 'Seed activity types'),

        -- -------------------------------------------------------------------------
        -- Valuation Change category
        -- -------------------------------------------------------------------------
        ('rate_reset',
         v_sys_tenant, 0, 'valuation_change', false,
         'Floating rate fixing or reset event.',
         null, null,
         current_user, 'system.initial_load', 'Seed activity types'),
        ('fx_reset',
         v_sys_tenant, 0, 'valuation_change', false,
         'FX rate reset for cross-currency trades.',
         null, null,
         current_user, 'system.initial_load', 'Seed activity types'),
        ('accrual',
         v_sys_tenant, 0, 'valuation_change', false,
         'Accrual period end-of-day valuation update.',
         null, null,
         current_user, 'system.initial_load', 'Seed activity types'),
        ('cashflow',
         v_sys_tenant, 0, 'valuation_change', false,
         'Scheduled cashflow payment event.',
         null, null,
         current_user, 'system.initial_load', 'Seed activity types'),

        -- -------------------------------------------------------------------------
        -- Misbooking category
        -- -------------------------------------------------------------------------
        ('correction',
         v_sys_tenant, 0, 'misbooking', false,
         'Correction of a data entry error; no economic change.',
         'Amendment', null,
         current_user, 'system.initial_load', 'Seed activity types'),
        ('rebook',
         v_sys_tenant, 0, 'misbooking', false,
         'Cancellation and re-booking of a misbooking.',
         null, null,
         current_user, 'system.initial_load', 'Seed activity types'),
        ('transfer',
         v_sys_tenant, 0, 'misbooking', false,
         'Transfer of a trade between books within the firm.',
         null, null,
         current_user, 'system.initial_load', 'Seed activity types'),
        ('split',
         v_sys_tenant, 0, 'misbooking', false,
         'Split of a trade into two or more trades.',
         null, null,
         current_user, 'system.initial_load', 'Seed activity types'),
        ('merge',
         v_sys_tenant, 0, 'misbooking', false,
         'Merge of two or more trades into one.',
         null, null,
         current_user, 'system.initial_load', 'Seed activity types'),

        -- -------------------------------------------------------------------------
        -- Cancellation category
        -- -------------------------------------------------------------------------
        ('cancel',
         v_sys_tenant, 0, 'cancellation', false,
         'Cancellation of a new trade before confirmation.',
         null, v_tr_cancel_new,
         current_user, 'system.initial_load', 'Seed activity types'),
        ('cancel_confirmed',
         v_sys_tenant, 0, 'cancellation', false,
         'Cancellation of a live (confirmed) trade.',
         'FullTermination', v_tr_cancel_live,
         current_user, 'system.initial_load', 'Seed activity types'),
        ('void',
         v_sys_tenant, 0, 'cancellation', false,
         'Void of a trade that should never have been booked.',
         null, v_tr_cancel_new,
         current_user, 'system.initial_load', 'Seed activity types')

    on conflict (tenant_id, code)
    where valid_to = ores_utility_infinity_timestamp_fn()
    do nothing;

    raise notice 'Activity types seeded.';
end;
$$;

select 'Activity Types' as entity, count(*) as count
from ores_trading_activity_types_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
