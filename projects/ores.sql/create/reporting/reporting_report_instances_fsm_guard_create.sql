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
 * Report Instance FSM Guard
 *
 * The report_instance_lifecycle machine is seeded, with its states and its
 * transitions, but until now nothing read it. A caller assigned fsm_state_id
 * directly, so an illegal transition was unrepresentable in the tables and
 * unconstrained in the code. This trigger makes the seeded machine
 * load-bearing: a state change the table does not allow is refused, in the
 * database, for every writer including the generated CRUD path.
 *
 * A report instance is a temporal table, so a state change arrives as a new
 * version: the insert function closes the current row and inserts its
 * successor. The previous state is therefore read as the most recent version
 * of the same instance, ordered by valid_to descending, which is the row being
 * replaced whether or not the close has happened yet. That keeps the guard
 * independent of trigger firing order.
 *
 * A null fsm_state_id passes. The column is nullable and the generated
 * integration tests insert instances without a state, so requiring one here
 * would reject them. Instances without a state are a separate gap, recorded on
 * the task.
 *
 * The machine is not named. A state row carries its machine_id, so the guard
 * reads the machine from the state and would serve any seeded machine.
 */

create or replace function ores_reporting_validate_instance_transition_fn()
returns trigger as $$
declare
    v_old_state   uuid;
    v_to_machine  uuid;
    v_to_name     text;
    v_to_initial  boolean;
    v_old_name    text;
begin
    if NEW.fsm_state_id is null then
        return NEW;
    end if;

    -- The state the instance is moving to, and what the machine says about it.
    -- The machine is seeded once, under the system tenant, and is shared by
    -- every tenant's instances, so the state is read there and not from the
    -- instance's own tenant.
    select s.machine_id, s.name, s.is_initial
      into v_to_machine, v_to_name, v_to_initial
      from ores_dq_fsm_states_tbl s
     where s.tenant_id = ores_utility_system_tenant_id_fn()
       and s.id = NEW.fsm_state_id
       and s.valid_to = ores_utility_infinity_timestamp_fn();

    if not found then
        raise exception 'report_instance % names an unknown FSM state %',
            NEW.id, NEW.fsm_state_id
            using errcode = '23503';
    end if;

    -- The state it is moving from. On insert this is the version being
    -- replaced, if there is one; on update it is the row as it stands.
    if TG_OP = 'UPDATE' then
        v_old_state := OLD.fsm_state_id;
    else
        select t.fsm_state_id
          into v_old_state
          from ores_reporting_report_instances_tbl t
         where t.tenant_id = NEW.tenant_id
           and t.id = NEW.id
         order by t.valid_to desc
         limit 1;
    end if;

    if v_old_state is not distinct from NEW.fsm_state_id then
        return NEW;
    end if;

    -- The first state an instance takes must be an initial state. A state
    -- that is initial and terminal is the concurrency policy's record of a
    -- trigger that never ran, which is why skipped and failed are both.
    if v_old_state is null then
        if not v_to_initial then
            raise exception 'report_instance % cannot start in state %: it is not an initial state',
                NEW.id, v_to_name
                using errcode = '23514';
        end if;
        return NEW;
    end if;

    select s.name into v_old_name
      from ores_dq_fsm_states_tbl s
     where s.tenant_id = ores_utility_system_tenant_id_fn()
       and s.id = v_old_state
       and s.valid_to = ores_utility_infinity_timestamp_fn();

    if not exists (
        select 1
        from ores_dq_fsm_transitions_tbl tr
        where tr.tenant_id = ores_utility_system_tenant_id_fn()
          and tr.machine_id = v_to_machine
          and tr.from_state_id = v_old_state
          and tr.to_state_id = NEW.fsm_state_id
          and tr.valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'report_instance % cannot move from % to %: no such transition',
            NEW.id, coalesce(v_old_name, v_old_state::text), v_to_name
            using errcode = '23514';
    end if;

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

comment on function ores_reporting_validate_instance_transition_fn() is
'Refuses a report instance state change the report_instance_lifecycle FSM does
not allow. The first state must be an initial state; every later one must be
reachable from the state it replaces.';

-- Fires before ores_reporting_report_instances_insert_trg, so the version it
-- reads is the row about to be closed. The read above tolerates either order.
create or replace trigger ores_reporting_report_instances_fsm_guard_trg
before insert or update on "ores_reporting_report_instances_tbl"
for each row execute function ores_reporting_validate_instance_transition_fn();

revoke execute on function ores_reporting_validate_instance_transition_fn() from public;

-- A trigger function is not callable by a client, so the grant is belt and
-- braces. It is stated because the revocation above would otherwise be the
-- only word on the subject, and a later change that made the function callable
-- would then fail for a reason nobody had written down.
grant execute on function ores_reporting_validate_instance_transition_fn()
    to :reporting_service_user;
