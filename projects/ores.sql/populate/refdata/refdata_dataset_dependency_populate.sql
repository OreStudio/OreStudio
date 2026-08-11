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
 * Refdata Dataset Dependencies (complementary edges)
 *
 * The core refdata dependency edges are declared inline in each seed
 * populate script (PR #1935): currency_pairs -> iso.currencies,
 * currency_pair_conventions -> currency_pairs,
 * currency_calendars -> refdata.calendars + iso.currencies,
 * currency_pair_convention_calendars -> currency_pair_conventions +
 * refdata.calendars, and currency_countries -> iso.countries +
 * iso.currencies. This file declares only the remaining edges that PR
 * #1935 does not cover, mirroring the documented execution order of the
 * seed scripts in this directory:
 *
 * - refdata.calendars has no incoming edge there, yet its production
 *   insert validates country_code (ISO 3166-1 alpha-2) and
 *   coding_scheme_code against published iso tables, so on a fresh
 *   database the topological sort can emit it before iso.countries and
 *   abort the whole bundle workflow (the same failure class as the
 *   original currency_pairs bug).
 * - currency_pair_conventions reads iso.currencies for its convention
 *   definitions (seed comment: "depends on refdata.currency_pairs and
 *   iso.currencies").
 * - currency_pair_convention_calendars reads the union of both legs'
 *   own calendars from refdata.currency_calendars (seed comment:
 *   "depends on refdata.currency_pair_conventions and
 *   refdata.currency_calendars").
 *
 * This script is idempotent.
 */

DO $$
BEGIN
    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.calendars', 'refdata.calendar_types', 'calendar_type_reference');
    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.calendars', 'iso.countries', 'country_reference');
    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.calendars', 'iso.coding_schemes', 'coding_scheme_reference');

    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.currency_pair_conventions', 'iso.currencies', 'base_quote_currency');

    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.currency_pair_convention_calendars', 'refdata.currency_calendars',
        'calendar_reference');
END $$;
