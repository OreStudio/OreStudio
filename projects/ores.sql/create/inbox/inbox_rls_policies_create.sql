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
-- Row-Level Security Policies for Inbox Junction Tables
-- =============================================================================
-- The generated entity tables carry their own tenant isolation policy. The
-- junctions do not, so their policies are stated here, as every component
-- states its junctions' policies.

-- -----------------------------------------------------------------------------
-- Notification Arguments (one row per value a message names)
-- -----------------------------------------------------------------------------
alter table ores_inbox_notification_arguments_tbl enable row level security;

drop policy if exists notification_arguments_tbl_tenant_isolation_policy on ores_inbox_notification_arguments_tbl;

create policy notification_arguments_tbl_tenant_isolation_policy on ores_inbox_notification_arguments_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Notification Recipients (a person reads only their own copy through the service)
-- -----------------------------------------------------------------------------
alter table ores_inbox_notification_recipients_tbl enable row level security;

drop policy if exists notification_recipients_tbl_tenant_isolation_policy on ores_inbox_notification_recipients_tbl;

create policy notification_recipients_tbl_tenant_isolation_policy on ores_inbox_notification_recipients_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
