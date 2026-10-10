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
-- Drop Row-Level Security Policies for Inbox Junction Tables
-- =============================================================================
-- Must be dropped before the corresponding tables are dropped.

drop policy if exists notification_recipients_tbl_tenant_isolation_policy on "ores_inbox_notification_recipients_tbl";
drop policy if exists notification_arguments_tbl_tenant_isolation_policy on "ores_inbox_notification_arguments_tbl";
drop policy if exists approval_parts_tbl_tenant_isolation_policy on "ores_inbox_approval_parts_tbl";
drop policy if exists approval_policies_tbl_tenant_isolation_policy on "ores_inbox_approval_policies_tbl";
