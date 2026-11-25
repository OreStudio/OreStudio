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
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

-- The run output tables are chunked by the instant they were written, because
-- that is the only time column a run's rows share: as_of is the date the engine
-- stated, which for a projected measure is decades ahead.
--
-- valid_from is that instant, and the bitemporal insert trigger sets it. A
-- hypertable checks its partitioning column for null before any BEFORE INSERT
-- trigger runs, so the column needs a default as well, or no row reaches the
-- trigger. The two values agree: current_timestamp is frozen for the
-- transaction, so the trigger writes back the instant the default already gave
-- the row.

alter table "ores_reporting_report_cashflows_tbl"
    alter column "valid_from" set default current_timestamp;

alter table "ores_reporting_report_metrics_tbl"
    alter column "valid_from" set default current_timestamp;

alter table "ores_reporting_report_text_values_tbl"
    alter column "valid_from" set default current_timestamp;
