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

drop function if exists ores_reporting_validate_concurrency_policy_fn(uuid, text);
drop rule if exists ores_reporting_concurrency_policies_delete_rule on "ores_reporting_concurrency_policies_tbl";
drop trigger if exists ores_reporting_concurrency_policies_insert_trg on "ores_reporting_concurrency_policies_tbl";
drop function if exists ores_reporting_concurrency_policies_insert_fn;
drop index if exists ores_reporting_concurrency_policies_code_uniq_idx;
drop index if exists ores_reporting_concurrency_policies_version_uniq_idx;
drop index if exists ores_reporting_concurrency_policies_tenant_idx;
drop table if exists "ores_reporting_concurrency_policies_tbl";
