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

insert into ores_refdata_floating_index_types_tbl (
    code, tenant_id, version, description,
    modified_by, performed_by, change_reason_code, change_commentary
) values
    -- Euro
    ('EUR-EURIBOR-6M',  ores_utility_system_tenant_id_fn(), 0, 'EURIBOR 6 Month',
     current_user, current_user, 'system.initial_load', 'Seed floating index types'),
    ('EUR-EURIBOR-3M',  ores_utility_system_tenant_id_fn(), 0, 'EURIBOR 3 Month',
     current_user, current_user, 'system.initial_load', 'Seed floating index types'),
    ('EUR-EURIBOR-1M',  ores_utility_system_tenant_id_fn(), 0, 'EURIBOR 1 Month',
     current_user, current_user, 'system.initial_load', 'Seed floating index types'),
    ('EUR-ESTR',        ores_utility_system_tenant_id_fn(), 0, 'Euro Short-Term Rate (ESTR)',
     current_user, current_user, 'system.initial_load', 'Seed floating index types'),
    -- USD
    ('USD-SOFR',        ores_utility_system_tenant_id_fn(), 0, 'Secured Overnight Financing Rate (SOFR)',
     current_user, current_user, 'system.initial_load', 'Seed floating index types'),
    ('USD-SOFR-3M',     ores_utility_system_tenant_id_fn(), 0, 'SOFR 3 Month Term Rate',
     current_user, current_user, 'system.initial_load', 'Seed floating index types'),
    ('USD-LIBOR-3M',    ores_utility_system_tenant_id_fn(), 0, 'USD LIBOR 3 Month (legacy)',
     current_user, current_user, 'system.initial_load', 'Seed floating index types'),
    ('USD-LIBOR-6M',    ores_utility_system_tenant_id_fn(), 0, 'USD LIBOR 6 Month (legacy)',
     current_user, current_user, 'system.initial_load', 'Seed floating index types'),
    -- GBP
    ('GBP-SONIA',       ores_utility_system_tenant_id_fn(), 0, 'Sterling Overnight Index Average (SONIA)',
     current_user, current_user, 'system.initial_load', 'Seed floating index types'),
    ('GBP-LIBOR-6M',    ores_utility_system_tenant_id_fn(), 0, 'GBP LIBOR 6 Month (legacy)',
     current_user, current_user, 'system.initial_load', 'Seed floating index types'),
    -- CHF
    ('CHF-SARON',       ores_utility_system_tenant_id_fn(), 0, 'Swiss Average Rate Overnight (SARON)',
     current_user, current_user, 'system.initial_load', 'Seed floating index types'),
    -- JPY
    ('JPY-TONAR',       ores_utility_system_tenant_id_fn(), 0, 'Tokyo Overnight Average Rate (TONAR)',
     current_user, current_user, 'system.initial_load', 'Seed floating index types'),
    ('JPY-LIBOR-6M',    ores_utility_system_tenant_id_fn(), 0, 'JPY LIBOR 6 Month (legacy)',
     current_user, current_user, 'system.initial_load', 'Seed floating index types'),
    -- Overnight RFRs for the remaining top-20-by-turnover currencies (see
    -- synthetic_ir_curve_configs_realistic_populate.sql, which seeds a curve per one of these).
    ('AUD-AONIA',       ores_utility_system_tenant_id_fn(), 0, 'AONIA (RBA Cash Rate overnight index)',
     current_user, current_user, 'system.initial_load', 'Seed floating index types'),
    ('CAD-CORRA',       ores_utility_system_tenant_id_fn(), 0, 'Canadian Overnight Repo Rate Average (CORRA)',
     current_user, current_user, 'system.initial_load', 'Seed floating index types'),
    ('CNY-SHIBOR-ON',   ores_utility_system_tenant_id_fn(), 0, 'Shanghai Interbank Offered Rate, Overnight',
     current_user, current_user, 'system.initial_load', 'Seed floating index types'),
    ('HKD-HONIA',       ores_utility_system_tenant_id_fn(), 0, 'Hong Kong Overnight Index Average (HONIA)',
     current_user, current_user, 'system.initial_load', 'Seed floating index types'),
    ('SGD-SORA',        ores_utility_system_tenant_id_fn(), 0, 'Singapore Overnight Rate Average (SORA)',
     current_user, current_user, 'system.initial_load', 'Seed floating index types'),
    ('SEK-SWESTR',      ores_utility_system_tenant_id_fn(), 0, 'Swedish Krona Short-Term Rate (SWESTR)',
     current_user, current_user, 'system.initial_load', 'Seed floating index types'),
    ('NOK-NOWA',        ores_utility_system_tenant_id_fn(), 0, 'Norwegian Overnight Weighted Average (NOWA)',
     current_user, current_user, 'system.initial_load', 'Seed floating index types'),
    ('NZD-NZIONA',      ores_utility_system_tenant_id_fn(), 0, 'New Zealand Interbank Overnight Cash Rate Average (NZIONA)',
     current_user, current_user, 'system.initial_load', 'Seed floating index types'),
    ('KRW-KOFR',        ores_utility_system_tenant_id_fn(), 0, 'Korea Overnight Financing Repo Rate (KOFR)',
     current_user, current_user, 'system.initial_load', 'Seed floating index types'),
    ('INR-MIBOR',       ores_utility_system_tenant_id_fn(), 0, 'Mumbai Interbank Overnight Rate (MIBOR)',
     current_user, current_user, 'system.initial_load', 'Seed floating index types'),
    ('MXN-TIIE-ON',     ores_utility_system_tenant_id_fn(), 0, 'TIIE de Fondeo a 1 Dia (Mexican overnight rate)',
     current_user, current_user, 'system.initial_load', 'Seed floating index types'),
    ('ZAR-ZARONIA',     ores_utility_system_tenant_id_fn(), 0, 'South African Rand Overnight Index Average (ZARONIA)',
     current_user, current_user, 'system.initial_load', 'Seed floating index types'),
    ('DKK-DESTR',       ores_utility_system_tenant_id_fn(), 0, 'Denmark Short-Term Rate (DESTR)',
     current_user, current_user, 'system.initial_load', 'Seed floating index types'),
    ('PLN-POLONIA',     ores_utility_system_tenant_id_fn(), 0, 'Polish Overnight Index Average (POLONIA)',
     current_user, current_user, 'system.initial_load', 'Seed floating index types'),
    ('TWD-TAIBOR-ON',   ores_utility_system_tenant_id_fn(), 0, 'Taipei Interbank Offered Rate, Overnight',
     current_user, current_user, 'system.initial_load', 'Seed floating index types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select 'Floating Index Types' as entity, count(*) as count
from ores_refdata_floating_index_types_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
