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
 * Delivery Outcome Types Population Script
 *
 * Seeds the closed set of notification delivery outcomes. The codes match the
 * C++ enum domain::delivery_outcome.
 *
 * The table is immutable, so a row is written once and a re-run inserts
 * nothing. This script is idempotent.
 */

\echo '--- Delivery Outcome Types ---'

insert into ores_inbox_delivery_outcome_types_tbl (code, description) values
    ('pending',   'The attempt is queued and has not finished'),
    ('delivered', 'The channel took the notification'),
    ('failed',    'The channel refused the notification')
on conflict (code) do nothing;
