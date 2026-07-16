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
 * Badge Severities Dataset Population Script
 *
 * Registers the ore.badge_severities dataset so it can be published, via
 * the DQ publish-from-dq pipeline, into any tenant's own copy of
 * ores_dq_badge_severities_tbl.
 */

DO $$
BEGIN
    -- --- Badge Severities Dataset ---

    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'ore.badge_severities',
        'ORE',
        'General',
        'Reference Data',
        'NONE',
        'Primary',
        'Actual',
        'Raw',
        'ORE Internal',
        'Badge Severities',
        'Severity levels for badge visual classification (secondary/info/success/warning/danger/primary).',
        'ORE_STUDIO',
        'Badge system metadata',
        current_date,
        'Internal',
        'badge_severities'
    );
END $$;
