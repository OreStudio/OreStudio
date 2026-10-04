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
