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
 * Trade Sample Data
 *
 * Demonstrates temporal versioning by booking an Interest Rate Swap (IRS)
 * as a 'New' lifecycle event, then applying a 'Novation' event that
 * replaces the original counterparty.
 *
 * Prerequisites:
 *   - ores_refdata_books_tbl must contain at least one book for the system tenant.
 *   - ores_refdata_portfolios_tbl must contain at least one portfolio for the
 *     system tenant.
 *   - ores_refdata_counterparties_tbl must contain at least two counterparties
 *     for the system tenant (original + transferee).
 *   - Trade reference data (types, lifecycle events, party role types) must be
 *     populated via trade_trade_types_populate.sql etc.
 *
 * Usage:
 *   psql -U ores_cli_user -d your_database -f populate/trade/trade_sample_data_populate.sql
 */

do $$
declare
    v_sys_tenant  uuid := ores_iam_system_tenant_id_fn();
    v_trade_id    uuid := gen_random_uuid();
    v_book_id     uuid;
    v_portfolio_id uuid;
    v_cp_a_id     uuid;
    v_cp_b_id     uuid;
    v_uti_id      uuid := gen_random_uuid();
    v_int_id      uuid := gen_random_uuid();
    v_role_a_id   uuid := gen_random_uuid();
begin
    -- Look up prerequisite data
    select id into v_book_id
    from ores_refdata_books_tbl
    where tenant_id = v_sys_tenant
      and valid_to = ores_utility_infinity_timestamp_fn()
    limit 1;

    if v_book_id is null then
        raise notice 'SKIP: No book found for system tenant. Run refdata sample data first.';
        return;
    end if;

    select id into v_portfolio_id
    from ores_refdata_portfolios_tbl
    where tenant_id = v_sys_tenant
      and valid_to = ores_utility_infinity_timestamp_fn()
    limit 1;

    if v_portfolio_id is null then
        raise notice 'SKIP: No portfolio found for system tenant. Run refdata sample data first.';
        return;
    end if;

    -- Pick two counterparties for the novation demo
    select id into v_cp_a_id
    from ores_refdata_counterparties_tbl
    where tenant_id = v_sys_tenant
      and valid_to = ores_utility_infinity_timestamp_fn()
    order by full_name
    limit 1;

    select id into v_cp_b_id
    from ores_refdata_counterparties_tbl
    where tenant_id = v_sys_tenant
      and valid_to = ores_utility_infinity_timestamp_fn()
      and id <> v_cp_a_id
    order by full_name
    limit 1;

    if v_cp_a_id is null or v_cp_b_id is null then
        raise notice 'SKIP: Need at least 2 counterparties for system tenant.';
        return;
    end if;

    -- Skip if sample trade already exists (idempotency via existence check)
    if exists (
        select 1 from ores_trading_trades_tbl
        where tenant_id = v_sys_tenant
          and netting_set_id = 'NS-SAMPLE-001'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise notice 'SKIP: Sample trade NS-SAMPLE-001 already exists.';
        return;
    end if;

    -- =========================================================================
    -- Step 1: Book the IRS as a 'New' lifecycle event
    -- =========================================================================

    insert into ores_trading_trades_tbl (
        id, tenant_id, version,
        external_id, book_id, portfolio_id,
        trade_type, netting_set_id, lifecycle_event,
        trade_date, execution_timestamp, effective_date, termination_date,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        v_trade_id, v_sys_tenant, 0,
        'SAMPLE-IRS-001', v_book_id, v_portfolio_id,
        'Swap', 'NS-SAMPLE-001', 'New',
        current_date,
        current_timestamp,
        current_date,
        current_date + interval '5 years',
        current_user, current_user,
        'system.new_record', 'Sample IRS trade booking'
    );

    raise notice 'Booked IRS trade id=%', v_trade_id;

    -- UTI identifier
    insert into ores_trading_identifiers_tbl (
        id, tenant_id, version,
        trade_id, issuing_party_id, id_value, id_type, id_scheme,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        v_uti_id, v_sys_tenant, 0,
        v_trade_id, null,
        'UTI-SAMPLE-' || to_char(current_date, 'YYYYMMDD') || '-001',
        'UTI', null,
        current_user, current_user,
        'system.new_record', 'UTI for sample IRS'
    );

    -- Internal ID
    insert into ores_trading_identifiers_tbl (
        id, tenant_id, version,
        trade_id, issuing_party_id, id_value, id_type, id_scheme,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        v_int_id, v_sys_tenant, 0,
        v_trade_id, null,
        'INT-SAMPLE-001',
        'Internal', null,
        current_user, current_user,
        'system.new_record', 'Internal ID for sample IRS'
    );

    -- Counterparty A as Counterparty role
    insert into ores_trading_party_roles_tbl (
        id, tenant_id, version,
        trade_id, counterparty_id, role,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        v_role_a_id, v_sys_tenant, 0,
        v_trade_id, v_cp_a_id, 'Counterparty',
        current_user, current_user,
        'system.new_record', 'Original counterparty for sample IRS'
    );

    raise notice 'Step 1 complete: trade booked with counterparty A';

    -- =========================================================================
    -- Step 2: Novation — transfer to Counterparty B
    -- =========================================================================
    -- Allow a brief moment for distinct timestamps (in tests this is fine since
    -- insert trigger uses current_timestamp which is per-statement in a txn,
    -- but in a real flow they would be distinct transactions).

    -- Re-insert trade with 'Novation' lifecycle event (upsert-by-insert)
    insert into ores_trading_trades_tbl (
        id, tenant_id, version,
        external_id, book_id, portfolio_id,
        trade_type, netting_set_id, lifecycle_event,
        trade_date, execution_timestamp, effective_date, termination_date,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        v_trade_id, v_sys_tenant, 0,
        'SAMPLE-IRS-001', v_book_id, v_portfolio_id,
        'Swap', 'NS-SAMPLE-001', 'Novation',
        current_date,
        current_timestamp,
        current_date,
        current_date + interval '5 years',
        current_user, current_user,
        'system.amendment', 'Sample IRS novation to counterparty B'
    );

    -- Soft-delete old Counterparty role for counterparty A
    delete from ores_trading_party_roles_tbl
    where id = v_role_a_id
      and tenant_id = v_sys_tenant;

    -- New Counterparty role for counterparty B
    insert into ores_trading_party_roles_tbl (
        id, tenant_id, version,
        trade_id, counterparty_id, role,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        gen_random_uuid(), v_sys_tenant, 0,
        v_trade_id, v_cp_b_id, 'Counterparty',
        current_user, current_user,
        'system.amendment', 'New counterparty after novation'
    );

    -- NovationTransferee role for counterparty B
    insert into ores_trading_party_roles_tbl (
        id, tenant_id, version,
        trade_id, counterparty_id, role,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        gen_random_uuid(), v_sys_tenant, 0,
        v_trade_id, v_cp_b_id, 'NovationTransferee',
        current_user, current_user,
        'system.amendment', 'Novation transferee'
    );

    raise notice 'Step 2 complete: novation applied, counterparty now B';
    raise notice 'Trade id=% now shows 2 temporal rows (New + Novation)', v_trade_id;

end;
$$;

-- Summary
select
    trade_id,
    trade_type,
    netting_set_id,
    counterparty,
    lifecycle_event,
    valid_from
from ores_trade_ore_envelope_vw
where tenant_id = ores_iam_system_tenant_id_fn()
  and netting_set_id = 'NS-SAMPLE-001';
