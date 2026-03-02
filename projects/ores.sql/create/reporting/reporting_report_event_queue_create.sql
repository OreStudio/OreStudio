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
 * Report Event Queue Table
 *
 * A simple event queue populated by pg_cron when a scheduled report definition
 * fires. Each row represents one trigger of a report, indicating that the
 * report runner should execute the named report for the given tenant.
 *
 * The payload JSON always contains:
 *   - report_name   : name of the report_definition that fired
 *   - event         : 'scheduled' (may be extended to 'cancelled', etc.)
 *   - triggered_at  : wall-clock time the pg_cron job ran (now()::text)
 *
 * The pg_cron command for a scheduled report definition is:
 *   INSERT INTO ores_reporting_report_event_queue_tbl
 *       (id, tenant_id, payload, queued_at)
 *   VALUES (gen_random_uuid(), '<tenant_uuid>'::uuid,
 *       jsonb_build_object('report_name', '<name>',
 *                          'event', 'scheduled',
 *                          'triggered_at', now()::text),
 *       now());
 *
 * Note: RLS is intentionally disabled on this table. pg_cron background
 * workers need to INSERT here without a session-level IAM context, and the
 * table is not user-facing. The report runner process reads and clears rows
 * using its own service account.
 */

create table if not exists "ores_reporting_report_event_queue_tbl" (
    "id"               uuid                     not null default gen_random_uuid(),
    "tenant_id"        uuid                     not null,
    "payload"          jsonb                    not null,
    "queued_at"        timestamp with time zone not null default now(),
    "processed_at"     timestamp with time zone          null,
    "processing_note"  text                              null,
    primary key (id)
);

-- Index for efficient polling of unprocessed events per tenant
create index if not exists ores_reporting_report_event_queue_pending_idx
on "ores_reporting_report_event_queue_tbl" (tenant_id, queued_at)
where processed_at is null;
