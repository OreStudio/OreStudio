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
 * Every person account holds Member.
 *
 * Member holds the reads every person's screens need: the reference data
 * vocabulary and the furniture every screen uses. A person account is created
 * by sign-up, by an administrator, by the staff dataset at provisioning and by
 * bootstrap, so the rule is one trigger on the accounts table rather than a
 * step in each of those paths.
 *
 * The trigger gives Member when a person account is first written, version 1,
 * and never again, so an administrator who takes Member away is not overruled
 * by the account's next version. A service account gets nothing. A tenant
 * whose roles hold no Member gets nothing either: the account then reads only
 * what its other roles grant, which fails closed.
 */
create or replace function ores_iam_accounts_give_member_fn()
returns trigger as $$
begin
    if NEW.account_type <> 'user' or NEW.version <> 1 then
        return null;
    end if;

    insert into ores_iam_account_roles_tbl (
        tenant_id, account_id, role_id, assigned_by, assigned_at,
        change_reason_code, change_commentary, valid_from, valid_to
    )
    select NEW.tenant_id, NEW.id, r.id, '', current_timestamp,
           'system.new_record', 'Every person holds Member',
           current_timestamp, ores_utility_infinity_timestamp_fn()
    from ores_iam_roles_tbl r
    where r.tenant_id = NEW.tenant_id
      and r.name = 'Member'
      and r.valid_to = ores_utility_infinity_timestamp_fn()
      and not exists (
          select 1 from ores_iam_account_roles_tbl ar
          where ar.tenant_id = NEW.tenant_id
            and ar.account_id = NEW.id
            and ar.role_id = r.id
            and ar.valid_to = ores_utility_infinity_timestamp_fn());

    return null;
end;
$$ language plpgsql;

create or replace trigger ores_iam_accounts_give_member_trg
after insert on ores_iam_accounts_tbl
for each row
execute function ores_iam_accounts_give_member_fn();
