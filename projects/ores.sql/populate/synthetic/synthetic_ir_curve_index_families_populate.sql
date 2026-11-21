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
 * IR Curve Index Families Population Script
 *
 * Populates the rate index families an ir_curve_generation_config can name,
 * and the tenors each admits: an overnight curve names no tenor, a term curve
 * names one and projects off its currency's overnight curve, and a dated
 * tenor, such as SOFR's FOMC, names a meeting-dated curve. hibor is both an
 * overnight index (CNH) and a term index (HKD).
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- IR Curve Index Families ---'

insert into ores_synthetic_ir_curve_index_families_tbl (
    tenant_id, code, version, name, description,
    admits_overnight, admits_term, dated_tenor, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'libor', 0, 'LIBOR',
     'London Interbank Offered Rate. Retired; kept as learning material.',
     false, true, '',
     1, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'euribor', 0, 'EURIBOR',
     'Euro Interbank Offered Rate, published by the European Money Markets Institute.',
     false, true, '',
     2, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'sofr', 0, 'SOFR',
     'Secured Overnight Financing Rate, published by the Federal Reserve Bank of New York. FOMC names its meeting-dated curve.',
     true, false, 'FOMC',
     3, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'estr', 0, 'ESTR',
     'Euro short-term rate, published by the European Central Bank.',
     true, false, '',
     4, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'sonia', 0, 'SONIA',
     'Sterling Overnight Index Average, published by the Bank of England.',
     true, false, '',
     5, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'tona', 0, 'TONA',
     'Tokyo Overnight Average Rate, published by the Bank of Japan.',
     true, false, '',
     6, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'saron', 0, 'SARON',
     'Swiss Average Rate Overnight.',
     true, false, '',
     7, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'aonia', 0, 'AONIA',
     'Australian dollar overnight index average.',
     true, false, '',
     8, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'corra', 0, 'CORRA',
     'Canadian Overnight Repo Rate Average, published by the Bank of Canada.',
     true, false, '',
     9, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'honia', 0, 'HONIA',
     'Hong Kong dollar overnight index average.',
     true, false, '',
     10, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'sora', 0, 'SORA',
     'Singapore Overnight Rate Average.',
     true, false, '',
     11, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'swestr', 0, 'SWESTR',
     'Swedish krona short-term rate, published by the Riksbank.',
     true, false, '',
     12, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'nowa', 0, 'NOWA',
     'Norwegian Overnight Weighted Average.',
     true, false, '',
     13, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'kofr', 0, 'KOFR',
     'Korean won overnight repo rate.',
     true, false, '',
     14, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'mibor', 0, 'MIBOR',
     'Mumbai interbank overnight rate.',
     true, false, '',
     15, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'zaronia', 0, 'ZARONIA',
     'South African rand overnight index average.',
     true, false, '',
     16, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'destr', 0, 'DESTR',
     'Danish krone short-term rate.',
     true, false, '',
     17, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'polonia', 0, 'POLONIA',
     'Polish zloty overnight index average.',
     true, false, '',
     18, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'nzonia', 0, 'NZONIA',
     'New Zealand dollar overnight index average.',
     true, false, '',
     19, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'shibor', 0, 'SHIBOR',
     'Shanghai Interbank Offered Rate.',
     true, false, '',
     20, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'tiie', 0, 'TIIE',
     'Mexican peso interbank equilibrium rate.',
     true, false, '',
     21, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'taibor', 0, 'TAIBOR',
     'Taipei Interbank Offered Rate.',
     true, false, '',
     22, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'cdi', 0, 'CDI',
     'Brazilian real interbank deposit rate.',
     true, false, '',
     23, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'czeonia', 0, 'CZEONIA',
     'Czech koruna overnight index average, published by the Czech National Bank.',
     true, false, '',
     24, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'buboron', 0, 'BUBOR O/N',
     'Budapest interbank overnight rate.',
     true, false, '',
     25, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'telboron', 0, 'TELBOR O/N',
     'Tel Aviv interbank overnight rate.',
     true, false, '',
     26, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'tlref', 0, 'TLREF',
     'Turkish lira overnight reference rate.',
     true, false, '',
     27, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'camara', 0, 'CAMARA',
     'Chilean peso interbank overnight rate.',
     true, false, '',
     28, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'ibr', 0, 'IBR',
     'Colombian peso interbank reference rate.',
     true, false, '',
     29, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'hibor', 0, 'HIBOR',
     'Hong Kong Interbank Offered Rate: an overnight index for CNH, and a term index at 2W, 1M and 3M for HKD.',
     true, true, '',
     30, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'shir', 0, 'SHIR',
     'Shekel overnight interest rate, published by the Bank of Israel.',
     true, false, '',
     31, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families'),
    (ores_utility_system_tenant_id_fn(), 'hufonia', 0, 'HUFONIA',
     'Hungarian forint overnight index average.',
     true, false, '',
     32, current_user, current_user, 'system.initial_load', 'Initial population of IR curve index families')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'synthetic_ir_curve_index_families' as entity, count(*) as count
from ores_synthetic_ir_curve_index_families_tbl;
