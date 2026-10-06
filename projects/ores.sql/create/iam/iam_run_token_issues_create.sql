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

-- =============================================================================
-- Run token issues: one row per run grant exchange that reached the log.
--
-- A row says who asked for a run token, for which grant and which run, and
-- whether the token was issued or refused. The exchange reads it to count the
-- distinct runs a grant has already served, so a grant with a max_runs admits
-- exactly that many runs. Designed for a TimescaleDB hypertable on issued_at.
-- No RLS, matching the auth events log: it is a system-level audit log, read
-- and written only by IAM's own exchange.
--
-- Outcomes:
--   issued    - a run token was minted and answered
--   refused   - the exchange refused; reason names the check that refused it,
--               or is "unavailable" when a limit refused before the checks
--
-- No retention policy is set. A grant that names a max_runs is counted from
-- this table, so dropping rows would let a grant serve more runs than its
-- grantor allowed.
-- =============================================================================

create table if not exists ores_iam_run_token_issues_tbl (
    "id"                  text not null,
    "issued_at"           timestamp with time zone not null,
    "tenant_id"           text not null default '',
    "party_id"            text not null default '',
    "grant_id"            text not null default '',
    "run_id"              text not null default '',
    "service"             text not null default '',
    "grantor_account_id"  text not null default '',
    "outcome"             text not null,
    "reason"              text not null default '',
    primary key (id, issued_at)
);

create index if not exists run_token_issues_grant_run_idx
on ores_iam_run_token_issues_tbl (grant_id, run_id);

create index if not exists run_token_issues_tenant_time_idx
on ores_iam_run_token_issues_tbl (tenant_id, issued_at desc);

do $$
declare
    tsdb_installed boolean;
begin
    select exists (
        select 1 from pg_extension where extname = 'timescaledb'
    ) into tsdb_installed;

    if tsdb_installed then
        raise notice 'TimescaleDB detected - creating hypertable (1-day chunks)';

        perform public.create_hypertable(
            'ores_iam_run_token_issues_tbl',
            'issued_at',
            chunk_time_interval => interval '1 day',
            if_not_exists => true
        );
    else
        raise notice 'TimescaleDB not available - using regular table';
    end if;
end $$;
