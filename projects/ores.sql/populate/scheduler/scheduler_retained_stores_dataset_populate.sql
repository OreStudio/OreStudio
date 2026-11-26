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
 * Retained Stores Dataset Population Script
 *
 * Registers the scheduler.retained_stores dataset so the catalogue of stores the
 * installation keeps is delivered as platform data: staged once, published into
 * the system tenant, and re-publishable without a rebuild.
 */

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'scheduler.retained_stores',
        'ORE',
        'General',
        'Reference Data',
        'NONE',
        'Primary',
        'Actual',
        'Raw',
        'ORE Internal',
        'Retained Stores',
        'Every store the installation keeps: the name a person reads, what it holds, the shape of its target, and the bound it ships with.',
        'ORE_STUDIO',
        'Housekeeping',
        current_date,
        'Internal',
        'retained_stores'
    );
END $$;
