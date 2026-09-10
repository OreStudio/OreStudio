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
 * One-shot migration: wide bond instrument columns -> relational bond rows
 *
 * The ores_trading_bond_instruments table once carried every product's
 * economics as flat scalar columns on the trade row: the bond terms,
 * the settlement days, and the extension columns of the option, trs,
 * future and ascot products. The relational model (task D7943D7E)
 * splits those economics across an issue row (the terms, one row per
 * security), the slim instrument row (which pins to its issue through
 * issue_id), the per-product fact rows keyed by instrument_id, and the
 * issue-keyed child rows. This script reshapes a legacy wide table in
 * place and moves its data to that shape.
 *
 * Per the deliverable (trading_bond_relational_model.org), deduplication
 * over the security identifier is the only join: the wide table never
 * carried a security identifier, so this script derives one per row as
 * 'MIG-' || md5 over the eight NOT NULL term columns. Rows whose terms
 * are identical merge into one issue row; terms disagreements within a
 * merged group are reported as a notice. Closed rows (no live sibling)
 * mint closed issue rows from their own terms; a closed row whose terms
 * match a live issue joins it.
 *
 * PREREQUISITES (run against a database created before the reshape):
 *   1. The eight new-shape create scripts have been applied:
 *      trading_bond_issues_create.sql, trading_bond_options_create.sql,
 *      trading_bond_futures_create.sql, trading_bond_trs_create.sql,
 *      trading_bond_repos_create.sql, trading_ascots_create.sql,
 *      trading_bond_issue_call_dates_create.sql and
 *      trading_bond_issue_conversion_targets_create.sql. They are
 *      idempotent. Their tables must hold no rows for the tenants that
 *      hold rows in the legacy instrument table; this script refuses
 *      otherwise. Development databases carry a loader-era population
 *      in these tables; clear it first.
 *   2. Run this script.
 *   3. Apply trading_bond_instruments_create.sql and
 *      trading_bond_instruments_notify_trigger_create.sql. They install
 *      the reshaped insert trigger, the delete rule and the notify
 *      trigger. Until they run, any write to the instrument table fails
 *      loudly: the legacy trigger and notify functions reference the
 *      pre-reshape id column.
 *
 * On a freshly recreated database the instrument table carries
 * instrument_id and no id column; this script is a guarded no-op.
 * This script is idempotent.
 *
 * Not migrated, with the count raised as a notice:
 *   - future_expiry_date has no destination column in the futures fact
 *     table. Its column set comes from the XSD, which carries no such
 *     scalar; the deliverable's mapping row is superseded by the
 *     wave 1.1 XSD survey.
 *   - option_expiry_date feeds exercise-schedule child rows; those
 *     tables are not part of this wave.
 *   - conversion_ratio feeds conversion-target rows, which require an
 *     underlying identifier the wide table never carried.
 *   - BondRepo rows: the wide table never carried repo economics, so
 *     no repo fact rows exist to migrate.
 *   - Instrument rows whose option or trs extension values fail the
 *     destination checks are left unmigrated and counted.
 *
 * Destination checks on the moved values: the option facts take only
 * option_type in (Call, Put) with a non-negative strike; the trs facts
 * take only a TotalReturn or PriceReturn return type and a funding leg
 * code ('Fixed' decodes to a fixed leg with no index, any other code to
 * a floating leg carrying the code as the funding index; funding_rate
 * has no legacy source and stays null); the ascot facts take any
 * non-null ascot_option_type. The call-date child rows are deduplicated
 * per (issue, call_date). settlement_days is nullable in the legacy
 * shape and NOT NULL on the issue; nulls are written as 0 and counted.
 *
 * The new rows are raw-inserted with the target triggers disabled, so
 * their validity windows survive the move: every fact and child row
 * carries its source instrument row's valid_from. The change reason is
 * 'system.new_record' with a migration commentary. The reference
 * migration used 'system.data_migration', but that code is not in the
 * canonical change-reason catalogue (it holds 'system.new_record' and
 * 'system.test' only). Audit on the new rows follows the reference:
 * modified_by = coalesce(ores_iam_current_service_fn(), current_user)
 * and performed_by = current_user. The instrument rows keep their own
 * audit and validity unchanged.
 */

\echo '--- Migrating wide bond instrument columns to relational rows ---'

do $$
declare
    v_has_legacy_id boolean;
    v_bad_codes integer;
    v_target_tables text[] := array[
        'ores_trading_bond_issues_tbl',
        'ores_trading_bond_options_tbl',
        'ores_trading_bond_futures_tbl',
        'ores_trading_bond_trs_tbl',
        'ores_trading_bond_repos_tbl',
        'ores_trading_ascots_tbl',
        'ores_trading_bond_issue_call_dates_tbl',
        'ores_trading_bond_issue_conversion_targets_tbl'
    ];
    v_target_table text;
    v_has_target_rows boolean;
    v_orphans integer;
    v_settlement_null integer;
    v_variance_groups integer;
    v_option_gate integer;
    v_trs_gate integer;
    v_ascot_null integer;
    v_future_expiry integer;
    v_option_expiry integer;
    v_conversion integer;
begin
    -- 1. Guard: the legacy shape must be present. On a reshaped or
    --    fresh table the id column is gone and the migration is a no-op.
    select exists (
        select 1 from information_schema.columns
        where table_name = 'ores_trading_bond_instruments_tbl'
          and column_name = 'id'
    ) into v_has_legacy_id;

    if not v_has_legacy_id then
        raise notice 'ores_trading_bond_instruments_tbl has no legacy id column; migration not needed.';
        return;
    end if;

    -- 2. Abort: every instrument row must fall inside the ten-code
    --    trade_type_code check of the reshaped table.
    select count(*) into v_bad_codes
    from ores_trading_bond_instruments_tbl
    where trade_type_code not in (
        'Bond', 'ForwardBond', 'BondFuture', 'BondOption', 'BondRepo',
        'BondTRS', 'BondPosition', 'CallableBond', 'ConvertibleBond',
        'Ascot'
    );

    if v_bad_codes > 0 then
        raise exception 'Aborting: % instrument rows carry a trade_type_code outside the ten codes.', v_bad_codes;
    end if;

    -- 3. Abort: the target family tables must hold no rows for the
    --    tenants that hold rows in the instrument table.
    foreach v_target_table in array v_target_tables loop
        execute format(
            'select exists (
                select 1 from %I
                where tenant_id in (
                    select distinct tenant_id
                    from ores_trading_bond_instruments_tbl
                )
            )', v_target_table
        ) into v_has_target_rows;

        if v_has_target_rows then
            raise exception 'Aborting: % holds rows for the migrating tenants. Clear the loader-era population first.', v_target_table;
        end if;
    end loop;

    -- 4. Rename the legacy key in place. The primary key, the exclusion
    --    constraint, the checks and the indexes follow the column.
    alter table ores_trading_bond_instruments_tbl
        rename column id to instrument_id;

    -- 5. Snapshot every legacy row with its derived security fingerprint.
    --    The fingerprint covers the eight NOT NULL term columns, so a
    --    merged issue's terms are identical by construction. All later
    --    statements read this snapshot; the wide columns of the live
    --    table are only dropped at the end.
    create temp table bond_mig_src on commit drop as
    select
        instrument_id, tenant_id, version, trade_type_code,
        issuer, currency, face_value, coupon_rate, coupon_frequency_code,
        day_count_code, issue_date, maturity_date, settlement_days,
        description, workspace_id, call_date, conversion_ratio,
        option_type, option_strike, option_expiry_date, future_expiry_date,
        trs_return_type, trs_funding_leg_code, ascot_option_type,
        valid_from, valid_to,
        md5(concat_ws(
            '|', issuer, currency, face_value::text, coupon_rate::text,
            coupon_frequency_code, day_count_code, issue_date::text,
            maturity_date::text
        )) as fp,
        (valid_to = ores_utility_infinity_timestamp_fn()) as is_current
    from ores_trading_bond_instruments_tbl;

    -- 6. Mint one open issue per distinct fingerprint among the live
    --    rows. The issue row takes its values from the earliest row of
    --    the group.
    create temp table bond_mig_issue (
        tenant_id uuid,
        fp text,
        is_open boolean,
        issue_id uuid
    ) on commit drop;
    -- The issues insert trigger validates the account username, which
    -- raises for the session user, and rewrites valid_from and valid_to.
    -- The minted issue rows bypass it; the enable after the fact and
    -- child inserts re-arms it.
    alter table ores_trading_bond_issues_tbl disable trigger all;

    insert into bond_mig_issue (tenant_id, fp, is_open, issue_id)
    select s.tenant_id, s.fp, true, gen_random_uuid()
    from bond_mig_src s
    where s.is_current
    group by s.tenant_id, s.fp;

    insert into ores_trading_bond_issues_tbl (
        issue_id, tenant_id, version, security_id,
        issuer, currency, face_value, coupon_rate, coupon_frequency_code,
        day_count_code, issue_date, maturity_date, settlement_days,
        description, workspace_id,
        modified_by, performed_by, change_reason_code, change_commentary,
        valid_from, valid_to
    )
    select distinct on (i.tenant_id, i.fp)
        i.issue_id, i.tenant_id, 1, 'MIG-' || i.fp,
        s.issuer, s.currency, s.face_value, s.coupon_rate,
        s.coupon_frequency_code, s.day_count_code, s.issue_date,
        s.maturity_date, coalesce(s.settlement_days, 0), s.description,
        s.workspace_id,
        coalesce(ores_iam_current_service_fn(), current_user), current_user,
        'system.new_record', 'Migrated from the wide legacy bond instrument columns',
        s.valid_from, ores_utility_infinity_timestamp_fn()
    from bond_mig_issue i
    join bond_mig_src s
        on s.tenant_id = i.tenant_id
       and s.fp = i.fp
       and s.is_current
    order by i.tenant_id, i.fp, s.valid_from;

    -- 7. Mint closed issues for the remaining closed rows: a fingerprint
    --    with no live issue gets one closed issue row, carrying the
    --    validity window of the latest row that bears it.
    insert into bond_mig_issue (tenant_id, fp, is_open, issue_id)
    select s.tenant_id, s.fp, false, gen_random_uuid()
    from bond_mig_src s
    where not s.is_current
      and not exists (
          select 1 from bond_mig_issue i
          where i.tenant_id = s.tenant_id and i.fp = s.fp
      )
    group by s.tenant_id, s.fp;

    insert into ores_trading_bond_issues_tbl (
        issue_id, tenant_id, version, security_id,
        issuer, currency, face_value, coupon_rate, coupon_frequency_code,
        day_count_code, issue_date, maturity_date, settlement_days,
        description, workspace_id,
        modified_by, performed_by, change_reason_code, change_commentary,
        valid_from, valid_to
    )
    select distinct on (i.tenant_id, i.fp)
        i.issue_id, i.tenant_id, 1, 'MIG-' || i.fp,
        s.issuer, s.currency, s.face_value, s.coupon_rate,
        s.coupon_frequency_code, s.day_count_code, s.issue_date,
        s.maturity_date, coalesce(s.settlement_days, 0), s.description,
        s.workspace_id,
        coalesce(ores_iam_current_service_fn(), current_user), current_user,
        'system.new_record', 'Migrated from the wide legacy bond instrument columns',
        s.valid_from, s.valid_to
    from bond_mig_issue i
    join bond_mig_src s
        on s.tenant_id = i.tenant_id
       and s.fp = i.fp
       and not s.is_current
    where not i.is_open
    order by i.tenant_id, i.fp, s.valid_from desc;

    -- 8. Map every instrument row to its issue.
    create temp table bond_mig_map on commit drop as
    select s.tenant_id, s.instrument_id, i.issue_id
    from bond_mig_src s
    join bond_mig_issue i
        on i.tenant_id = s.tenant_id
       and i.fp = s.fp;

    -- 9. Pin the instrument rows to their issues. The notify trigger
    --    fires on update and its installed body references the
    --    pre-reshape id column, so it is disabled around the update.
    alter table ores_trading_bond_instruments_tbl
        add column issue_id uuid;

    alter table ores_trading_bond_instruments_tbl disable trigger all;

    update ores_trading_bond_instruments_tbl t
    set issue_id = m.issue_id
    from bond_mig_map m
    where m.tenant_id = t.tenant_id
      and m.instrument_id = t.instrument_id;

    select count(*) into v_orphans
    from ores_trading_bond_instruments_tbl
    where issue_id is null;

    if v_orphans > 0 then
        raise exception 'Aborting: % instrument rows could not be mapped to an issue.', v_orphans;
    end if;

    alter table ores_trading_bond_instruments_tbl
        alter column issue_id set not null;

    alter table ores_trading_bond_instruments_tbl enable trigger all;

    -- 10. Move the product economics into the fact and child rows. The
    --     inserts are raw: the target triggers are disabled so the
    --     validity windows survive and the account-username validation
    --     (which rejects a DBA session user) does not run.
    alter table ores_trading_bond_options_tbl disable trigger all;
    alter table ores_trading_bond_trs_tbl disable trigger all;
    alter table ores_trading_ascots_tbl disable trigger all;
    alter table ores_trading_bond_issue_call_dates_tbl disable trigger all;

    insert into ores_trading_bond_options_tbl (
        instrument_id, tenant_id, version, option_type, option_strike,
        modified_by, performed_by, change_reason_code, change_commentary,
        valid_from, valid_to
    )
    select
        s.instrument_id, s.tenant_id, 1, s.option_type, s.option_strike,
        coalesce(ores_iam_current_service_fn(), current_user), current_user,
        'system.new_record', 'Migrated from the wide legacy bond instrument columns',
        s.valid_from, ores_utility_infinity_timestamp_fn()
    from bond_mig_src s
    where s.is_current
      and s.trade_type_code = 'BondOption'
      and s.option_type in ('Call', 'Put')
      and s.option_strike >= 0;

    insert into ores_trading_bond_trs_tbl (
        instrument_id, tenant_id, version, return_type, funding_leg_type,
        funding_rate, funding_index,
        modified_by, performed_by, change_reason_code, change_commentary,
        valid_from, valid_to
    )
    select
        s.instrument_id, s.tenant_id, 1, s.trs_return_type,
        case when s.trs_funding_leg_code = 'Fixed'
            then 'Fixed' else 'Floating' end,
        null,
        case when s.trs_funding_leg_code = 'Fixed'
            then null else s.trs_funding_leg_code end,
        coalesce(ores_iam_current_service_fn(), current_user), current_user,
        'system.new_record', 'Migrated from the wide legacy bond instrument columns',
        s.valid_from, ores_utility_infinity_timestamp_fn()
    from bond_mig_src s
    where s.is_current
      and s.trade_type_code = 'BondTRS'
      and s.trs_return_type in ('TotalReturn', 'PriceReturn')
      and s.trs_funding_leg_code is not null;

    insert into ores_trading_ascots_tbl (
        instrument_id, tenant_id, version, ascot_option_type,
        modified_by, performed_by, change_reason_code, change_commentary,
        valid_from, valid_to
    )
    select
        s.instrument_id, s.tenant_id, 1, s.ascot_option_type,
        coalesce(ores_iam_current_service_fn(), current_user), current_user,
        'system.new_record', 'Migrated from the wide legacy bond instrument columns',
        s.valid_from, ores_utility_infinity_timestamp_fn()
    from bond_mig_src s
    where s.is_current
      and s.trade_type_code = 'Ascot'
      and s.ascot_option_type is not null;

    insert into ores_trading_bond_issue_call_dates_tbl (
        issue_id, sequence_number, tenant_id, version, call_date,
        modified_by, performed_by, change_reason_code, change_commentary,
        valid_from, valid_to
    )
    with cand as (
        select m.issue_id, s.tenant_id, s.call_date,
               min(s.valid_from) as valid_from
        from bond_mig_src s
        join bond_mig_map m
            on m.tenant_id = s.tenant_id
           and m.instrument_id = s.instrument_id
        where s.is_current
          and s.trade_type_code = 'CallableBond'
          and s.call_date is not null
        group by m.issue_id, s.tenant_id, s.call_date
    )
    select
        cand.issue_id,
        row_number() over (
            partition by cand.tenant_id, cand.issue_id
            order by cand.call_date
        ),
        cand.tenant_id, 1, cand.call_date,
        coalesce(ores_iam_current_service_fn(), current_user), current_user,
        'system.new_record', 'Migrated from the wide legacy bond instrument columns',
        cand.valid_from, ores_utility_infinity_timestamp_fn()
    from cand;

    -- The repos and futures fact tables and the conversion-target table
    -- receive no rows: the wide table carried no repo or conversion
    -- economics with a destination (see the header).
    alter table ores_trading_bond_issues_tbl enable trigger all;
    alter table ores_trading_bond_options_tbl enable trigger all;
    alter table ores_trading_bond_trs_tbl enable trigger all;
    alter table ores_trading_ascots_tbl enable trigger all;
    alter table ores_trading_bond_issue_call_dates_tbl enable trigger all;

    -- 11. Count the rows the destination gates refused or the wide
    --     columns carried without a destination. The snapshot still
    --     holds every legacy value.
    select count(*) into v_settlement_null
    from bond_mig_src where settlement_days is null;

    select count(*) into v_variance_groups
    from (
        select tenant_id, fp
        from bond_mig_src
        group by tenant_id, fp
        having count(distinct settlement_days) > 1
            or count(distinct coalesce(description, '')) > 1
            or count(distinct workspace_id) > 1
    ) v;

    select count(*) into v_option_gate
    from bond_mig_src
    where is_current and trade_type_code = 'BondOption'
      and (option_type not in ('Call', 'Put') or option_strike is null or option_strike < 0);

    select count(*) into v_trs_gate
    from bond_mig_src
    where is_current and trade_type_code = 'BondTRS'
      and (trs_return_type not in ('TotalReturn', 'PriceReturn') or trs_funding_leg_code is null);

    select count(*) into v_ascot_null
    from bond_mig_src
    where is_current and trade_type_code = 'Ascot' and ascot_option_type is null;

    select count(*) into v_future_expiry
    from bond_mig_src
    where is_current and trade_type_code = 'BondFuture' and future_expiry_date is not null;

    select count(*) into v_option_expiry
    from bond_mig_src
    where is_current and trade_type_code = 'BondOption' and option_expiry_date is not null;

    select count(*) into v_conversion
    from bond_mig_src
    where is_current and trade_type_code = 'ConvertibleBond' and conversion_ratio is not null;

    -- 12. Drop the migrated columns. Their inline checks disappear with
    --     them; the remaining checks and indexes keep working against
    --     the reshaped columns.
    alter table ores_trading_bond_instruments_tbl
        drop column if exists issuer,
        drop column if exists currency,
        drop column if exists face_value,
        drop column if exists coupon_rate,
        drop column if exists coupon_frequency_code,
        drop column if exists day_count_code,
        drop column if exists issue_date,
        drop column if exists maturity_date,
        drop column if exists settlement_days,
        drop column if exists description,
        drop column if exists call_date,
        drop column if exists conversion_ratio,
        drop column if exists option_type,
        drop column if exists option_strike,
        drop column if exists option_expiry_date,
        drop column if exists future_expiry_date,
        drop column if exists trs_return_type,
        drop column if exists trs_funding_leg_code,
        drop column if exists ascot_option_type;

    -- 13. Install the reshaped trade_type_code check.
    alter table ores_trading_bond_instruments_tbl
        add constraint ores_trading_bond_instruments_tbl_trade_type_code_check
        check (trade_type_code in (
            'Bond', 'ForwardBond', 'BondFuture', 'BondOption', 'BondRepo',
            'BondTRS', 'BondPosition', 'CallableBond', 'ConvertibleBond',
            'Ascot'
        ));

    raise notice 'Issues merged with intra-group variance (settlement days, description or workspace): %', v_variance_groups;
    raise notice 'Rows with a null settlement_days written as 0: %', v_settlement_null;
    raise notice 'BondOption rows refused by the option gates: %', v_option_gate;
    raise notice 'BondTRS rows refused by the trs gates: %', v_trs_gate;
    raise notice 'Ascot rows with a null ascot_option_type left unmigrated: %', v_ascot_null;
    raise notice 'BondFuture rows dropped future_expiry_date (no destination column): %', v_future_expiry;
    raise notice 'BondOption rows dropped option_expiry_date (no exercise-schedule tables this wave): %', v_option_expiry;
    raise notice 'ConvertibleBond rows dropped conversion_ratio (no underlying identifier in the wide table): %', v_conversion;
end $$;

-- Summary
select 'ores_trading_bond_issues_tbl' as entity, count(*) as rows
from ores_trading_bond_issues_tbl
union all
select 'ores_trading_bond_instruments_tbl', count(*) from ores_trading_bond_instruments_tbl
union all
select 'ores_trading_bond_options_tbl', count(*) from ores_trading_bond_options_tbl
union all
select 'ores_trading_bond_futures_tbl', count(*) from ores_trading_bond_futures_tbl
union all
select 'ores_trading_bond_trs_tbl', count(*) from ores_trading_bond_trs_tbl
union all
select 'ores_trading_bond_repos_tbl', count(*) from ores_trading_bond_repos_tbl
union all
select 'ores_trading_ascots_tbl', count(*) from ores_trading_ascots_tbl
union all
select 'ores_trading_bond_issue_call_dates_tbl', count(*) from ores_trading_bond_issue_call_dates_tbl
union all
select 'ores_trading_bond_issue_conversion_targets_tbl', count(*) from ores_trading_bond_issue_conversion_targets_tbl;

-- Integrity
select count(*) as instrument_rows_without_issue
from ores_trading_bond_instruments_tbl t
where not exists (
    select 1 from ores_trading_bond_issues_tbl i
    where i.tenant_id = t.tenant_id and i.issue_id = t.issue_id
);
