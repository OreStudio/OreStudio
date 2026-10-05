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
 * The rules every approval decision keeps, whatever the kind.
 *
 * Four-eyes: the person who asked a request does not decide it. A withdrawal
 * is the one decision the person who asked takes, and nobody else takes it.
 * A decision names a current request; without one there is nobody to compare
 * the decider with, so it is refused rather than let through.
 *
 * A decision is never edited. A new version of the row only closes the old
 * one, so an update that changes the request, the decision or the decider is
 * refused, and the rules above cannot be stepped around by an update.
 *
 * The one-approval-per-person index is modelled, in the decision's own table.
 */

create or replace function ores_inbox_approval_decisions_rules_fn()
returns trigger as $$
declare
    v_requested_by uuid;
begin
    select requested_by into v_requested_by
    from ores_inbox_approval_requests_tbl
    where tenant_id = NEW.tenant_id
      and id = NEW.request_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_requested_by is null then
        raise exception 'No current request % to decide.', NEW.request_id
            using errcode = '23503';
    end if;

    if NEW.decision_code = 'withdraw' then
        if NEW.decided_by <> v_requested_by then
            raise exception 'Only the person who asked can withdraw request %.', NEW.request_id
                using errcode = '23514';
        end if;
    elsif NEW.decided_by = v_requested_by then
        raise exception 'The person who asked cannot decide request %.', NEW.request_id
            using errcode = '23514';
    end if;

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_inbox_approval_decisions_rules_trg
before insert on "ores_inbox_approval_decisions_tbl"
for each row execute function ores_inbox_approval_decisions_rules_fn();

create or replace function ores_inbox_approval_decisions_unchanged_fn()
returns trigger as $$
begin
    if NEW.request_id is distinct from OLD.request_id
       or NEW.decision_code is distinct from OLD.decision_code
       or NEW.decided_by is distinct from OLD.decided_by then
        raise exception 'A decision on request % cannot be changed; take a new decision instead.', OLD.request_id
            using errcode = '23514';
    end if;

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_inbox_approval_decisions_unchanged_trg
before update on "ores_inbox_approval_decisions_tbl"
for each row execute function ores_inbox_approval_decisions_unchanged_fn();
