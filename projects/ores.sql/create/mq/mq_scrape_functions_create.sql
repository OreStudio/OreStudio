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
-- MQ metrics scrape function.
-- Called by the pg_cron job registered via mq_job_initializer at service startup.
-- Reads a point-in-time snapshot from pgmq.metrics_all() and inserts it into
-- ores_mq_metrics_samples_tbl. ON CONFLICT DO NOTHING makes it safe to call
-- multiple times within the same clock second.
-- =============================================================================

create or replace function ores_mq_scrape_metrics_fn()
returns void
language plpgsql
security definer
as $$
begin
    insert into ores_mq_metrics_samples_tbl (
        queue_name,
        sample_time,
        queue_length,
        total_messages
    )
    select
        queue_name,
        now(),
        queue_length,
        total_messages
    from pgmq.metrics_all()
    on conflict (queue_name, sample_time) do nothing;
end;
$$;
