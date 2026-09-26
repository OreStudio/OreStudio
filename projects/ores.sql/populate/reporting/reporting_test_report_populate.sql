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
 * Report Instance FSM Test Fixture
 *
 * Seeds a small family of report definitions so that the report instance
 * FSM can be walked end to end in any environment, from ores.shell, without
 * hand-writing rows first.
 *
 * The FSM has seven states. Each fixture exercises a specific path:
 *
 *   FSM Happy Path     pending -> running -> completed
 *   FSM Fail Policy    a second trigger while one runs -> failed
 *   FSM Queue Policy   a second trigger while one runs -> queued, then promoted
 *   FSM Skip Policy    a second trigger while one runs -> skipped
 *   FSM No Scope       a config with no book scope -> failed
 *
 * Why "no scope" fails rather than reporting everything:
 * ores_reporting_resolve_book_ids_for_config_fn returns an empty set when
 * neither the book nor the portfolio junction has a row, and gather_trades
 * treats that as a configuration error. Reporting every book the tenant can
 * see was the earlier intent; the function does not do that.
 *
 * Every definition carries a dormant cron, "0 0 1 1 *" — once a year on the
 * first of January. The scheduler reconciles a job for each definition that
 * has no scheduler_job_id, so these become scheduled without firing during a
 * test session. A test that needs the scheduler path should create its own
 * job with a live cron and remove it afterwards.
 *
 * The fixture is deterministic and idempotent. It needs no trade data: the
 * trading export writes an empty list and answers success when a book holds
 * no trades, so the happy path walks the whole workflow on a fresh database.
 *
 * It runs after the reference data publication, because a definition needs a
 * party and a book, and neither exists before it.
 */

\echo '--- Report Instance FSM Test Fixture ---'

do $$
declare
    v_tenant        uuid := ores_utility_system_tenant_id_fn();
    v_workspace     uuid := ores_utility_live_workspace_id_fn();
    v_actor         text;
    v_party         uuid;
    v_book          uuid;
    v_active_state  uuid;
    v_def_id        uuid;
    v_config_id     uuid;
    v_inserted      integer := 0;
    rec             record;
begin
    -- The actor a seeded row names. Prefer a human account, fall back to
    -- whatever exists, and only then to the database user: the insert
    -- triggers reject a name that is not an account, once bootstrap is over.
    -- The author the seeded rows name. Populate runs before any human account
    -- exists, so a plain ordering picks whichever service account sorts first
    -- and the audit trail then credits an unrelated service. The DDL user is
    -- who actually performs population, and it is what the other seeds name.
    select username into v_actor
    from ores_iam_accounts_tbl
    where valid_to = ores_utility_infinity_timestamp_fn()
    order by (username like '%\_ddl\_user') desc,
             (account_type <> 'service') desc,
             username
    limit 1;
    v_actor := coalesce(v_actor, current_user);

    -- The fixture is owned by the tenant's system party, which every session
    -- can see. Owning it with an arbitrary business party hid it from a session
    -- scoped to another party, so the shell could not find the definition it
    -- was meant to trigger.
    select id into v_party from ores_refdata_read_system_party_fn(v_tenant) limit 1;

    -- One book to put in scope. Its own party does not matter: the scope
    -- junction is read through the definition's party.
    select b.id into v_book
    from ores_refdata_books_tbl b
    where b.tenant_id = v_tenant
      and b.valid_to = ores_utility_infinity_timestamp_fn()
    order by b.name
    limit 1;

    if v_party is null or v_book is null then
        raise warning 'report FSM fixture: tenant % has no system party or no book; skipped.',
            v_tenant;
        return;
    end if;

    -- The state a schedulable definition sits in.
    select s.id into v_active_state
    from ores_dq_fsm_states_tbl s
    join ores_dq_fsm_machines_tbl m
      on m.tenant_id = s.tenant_id and m.id = s.machine_id
    where m.name = 'report_definition_lifecycle'
      and s.name = 'active'
      and s.valid_to = ores_utility_infinity_timestamp_fn()
      and m.valid_to = ores_utility_infinity_timestamp_fn();

    for rec in
        select * from (values
            ('FSM Happy Path', 'fail', true,
             'Walks pending, running and completed with a book in scope.'),
            ('FSM Fail Policy', 'fail', true,
             'A second trigger while one instance runs exercises the fail policy.'),
            ('FSM Queue Policy', 'queue', true,
             'A second trigger while one instance runs exercises queued and its promotion.'),
            ('FSM Skip Policy', 'skip', true,
             'A second trigger while one instance runs exercises skipped.'),
            ('FSM No Scope', 'fail', false,
             'Its config has no book scope, which gather_trades rejects: exercises failed.')
        ) as t(name, policy, scoped, commentary)
    loop
        select id into v_def_id
        from ores_reporting_report_definitions_tbl
        where tenant_id = v_tenant
          and name = rec.name
          and valid_to = ores_utility_infinity_timestamp_fn();

        if v_def_id is null then
            v_def_id := gen_random_uuid();
            insert into ores_reporting_report_definitions_tbl (
                id, tenant_id, version, name, party_id, description, report_type,
                fsm_state_id, schedule_expression, concurrency_policy, scheduler_job_id,
                workspace_id, modified_by, performed_by, change_reason_code, change_commentary,
                valid_from, valid_to)
            values (v_def_id, v_tenant, 0, rec.name, v_party, rec.commentary, 'risk',
                    v_active_state, '0 0 1 1 *', rec.policy, null,
                    v_workspace, v_actor, v_actor, 'system.initial_load', rec.commentary,
                    clock_timestamp(), ores_utility_infinity_timestamp_fn());
            v_inserted := v_inserted + 1;
            raise debug 'report FSM fixture: created definition %', rec.name;
        end if;

        select id into v_config_id
        from ores_reporting_risk_report_configs_tbl
        where tenant_id = v_tenant
          and report_definition_id = v_def_id
          and valid_to = ores_utility_infinity_timestamp_fn();

        if v_config_id is null then
            v_config_id := gen_random_uuid();
            insert into ores_reporting_risk_report_configs_tbl (
                id, tenant_id, version, report_definition_id, base_currency,
                modified_by, performed_by, change_reason_code, change_commentary,
                valid_from, valid_to)
            values (v_config_id, v_tenant, 0, v_def_id, 'USD',
                    v_actor, v_actor, 'system.initial_load', rec.commentary,
                    clock_timestamp(), ores_utility_infinity_timestamp_fn());
            raise debug 'report FSM fixture: created config for %', rec.name;
        end if;

        -- The scope. Only the definitions that mean to run get a book;
        -- FSM No Scope is deliberately left empty so that gather_trades
        -- rejects it.
        if rec.scoped then
            if not exists (
                select 1 from ores_reporting_risk_report_config_books_tbl
                where tenant_id = v_tenant
                  and risk_report_config_id = v_config_id
                  and book_id = v_book
                  and valid_to = ores_utility_infinity_timestamp_fn()
            ) then
                insert into ores_reporting_risk_report_config_books_tbl (
                    tenant_id, risk_report_config_id, book_id, valid_from, valid_to)
                values (v_tenant, v_config_id, v_book,
                        clock_timestamp(), ores_utility_infinity_timestamp_fn());
                raise debug 'report FSM fixture: scoped % to book %', rec.name, v_book;
            end if;
        end if;
    end loop;

    raise notice 'report FSM fixture: % new definition(s), party %, book %.',
        v_inserted, v_party, v_book;
end;
$$;

\echo '--- Report Instance FSM Test Fixture Complete ---'
