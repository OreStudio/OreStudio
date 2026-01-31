/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
create or replace function ores_dq_validate_change_reason_fn(
    p_change_reason_code text
) returns text as $$
begin
    if p_change_reason_code is null or p_change_reason_code = '' then
        return 'system.new_record';
    end if;

    if not exists (select 1 from ores_dq_change_reasons_tbl limit 1) then
        return p_change_reason_code;
    end if;

    if not exists (
        select 1 from ores_dq_change_reasons_tbl
        where code = p_change_reason_code
        and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid change_reason_code: %. Reason must exist in ores_dq_change_reasons_tbl.',
            p_change_reason_code
            using errcode = '23503';
    end if;

    return p_change_reason_code;
end;
$$ language plpgsql;
