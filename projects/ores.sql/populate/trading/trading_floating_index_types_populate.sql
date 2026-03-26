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
 * Floating Index Types Population Script
 *
 * Seeds the database with common interest rate floating index codes.
 * These are used in floating swap legs and other instruments.
 * This script is idempotent.
 */

\echo '--- Floating Index Types ---'

insert into ores_trading_floating_index_types_tbl (
    code, tenant_id, version, description,
    modified_by, change_reason_code, change_commentary
) values
    -- Euro
    ('EUR-EURIBOR-6M',  ores_iam_system_tenant_id_fn(), 0, 'EURIBOR 6 Month',
     'ores_trading_service', 'system.initial_load', 'Seed floating index types'),
    ('EUR-EURIBOR-3M',  ores_iam_system_tenant_id_fn(), 0, 'EURIBOR 3 Month',
     'ores_trading_service', 'system.initial_load', 'Seed floating index types'),
    ('EUR-EURIBOR-1M',  ores_iam_system_tenant_id_fn(), 0, 'EURIBOR 1 Month',
     'ores_trading_service', 'system.initial_load', 'Seed floating index types'),
    ('EUR-ESTR',        ores_iam_system_tenant_id_fn(), 0, 'Euro Short-Term Rate (ESTR)',
     'ores_trading_service', 'system.initial_load', 'Seed floating index types'),
    -- USD
    ('USD-SOFR',        ores_iam_system_tenant_id_fn(), 0, 'Secured Overnight Financing Rate (SOFR)',
     'ores_trading_service', 'system.initial_load', 'Seed floating index types'),
    ('USD-SOFR-3M',     ores_iam_system_tenant_id_fn(), 0, 'SOFR 3 Month Term Rate',
     'ores_trading_service', 'system.initial_load', 'Seed floating index types'),
    ('USD-LIBOR-3M',    ores_iam_system_tenant_id_fn(), 0, 'USD LIBOR 3 Month (legacy)',
     'ores_trading_service', 'system.initial_load', 'Seed floating index types'),
    ('USD-LIBOR-6M',    ores_iam_system_tenant_id_fn(), 0, 'USD LIBOR 6 Month (legacy)',
     'ores_trading_service', 'system.initial_load', 'Seed floating index types'),
    -- GBP
    ('GBP-SONIA',       ores_iam_system_tenant_id_fn(), 0, 'Sterling Overnight Index Average (SONIA)',
     'ores_trading_service', 'system.initial_load', 'Seed floating index types'),
    ('GBP-LIBOR-6M',    ores_iam_system_tenant_id_fn(), 0, 'GBP LIBOR 6 Month (legacy)',
     'ores_trading_service', 'system.initial_load', 'Seed floating index types'),
    -- CHF
    ('CHF-SARON',       ores_iam_system_tenant_id_fn(), 0, 'Swiss Average Rate Overnight (SARON)',
     'ores_trading_service', 'system.initial_load', 'Seed floating index types'),
    -- JPY
    ('JPY-TONAR',       ores_iam_system_tenant_id_fn(), 0, 'Tokyo Overnight Average Rate (TONAR)',
     'ores_trading_service', 'system.initial_load', 'Seed floating index types'),
    ('JPY-LIBOR-6M',    ores_iam_system_tenant_id_fn(), 0, 'JPY LIBOR 6 Month (legacy)',
     'ores_trading_service', 'system.initial_load', 'Seed floating index types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Floating Index Types' as entity, count(*) as count
from ores_trading_floating_index_types_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
