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
 * Overnight Index Conventions Population Script
 *
 * Seeds one ores_refdata_overnight_index_conventions_tbl row per (currency,
 * RFR family) pair used by the synthetic.ir_curve_configs.* seed themes
 * (ore_samples_2016/realistic_2026/uniform_demo) -- id is
 * "<currency_code>-<upper(index_family)>", matching
 * ir_curve_generation_config_service::validate_index_family()'s own lookup
 * shape. fixing_calendar values are refdata.calendars codes (see
 * refdata_calendars_populate.sql); day_count_fraction values are FpML
 * canonical day-count-fraction codes (see
 * external/fpml/codelist/day-count-fraction-2-3.xml).
 *
 * Sourced from ORE's own vendored QuantExt/QuantLib index class
 * definitions (real conventions, not fabricated) -- see each row's own
 * comment for its source class. ZAR-ZARONIA and NZD-NZONIA have no
 * equivalent class in ORE's index library (neither RFR is modelled there
 * yet); their conventions were corroborated instead from public SARB/RBNZ
 * documentation (Act/365, national calendar, 0-day lag -- consistent with
 * every other RFR's own convention shape).
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO UPDATE.
 */

\echo '--- Overnight Index Conventions ---'

insert into ores_refdata_overnight_index_conventions_tbl (
    tenant_id, id, version, fixing_calendar, day_count_fraction, settlement_days,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    -- Source: QuantLib::Sofr (ql/indexes/ibor/sofr.hpp/.cpp).
    (ores_utility_system_tenant_id_fn(), 'USD-SOFR', 0, 'UnitedStates.SOFR', 'ACT/360', 0,
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data'),
    -- Source: QuantLib::Estr (ql/indexes/ibor/estr.hpp/.cpp).
    (ores_utility_system_tenant_id_fn(), 'EUR-ESTR', 0, 'TARGET', 'ACT/360', 0,
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data'),
    -- Source: QuantLib::Tonar (ql/indexes/ibor/tonar.hpp), aka Tona.
    (ores_utility_system_tenant_id_fn(), 'JPY-TONA', 0, 'Japan', 'ACT/365.FIXED', 0,
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data'),
    -- Source: QuantLib::Sonia (ql/indexes/ibor/sonia.hpp/.cpp).
    (ores_utility_system_tenant_id_fn(), 'GBP-SONIA', 0, 'UnitedKingdom.Exchange', 'ACT/365.FIXED', 0,
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data'),
    -- Source: QuantExt::CHFSaron (qle/indexes/ibor/chfsaron.hpp).
    (ores_utility_system_tenant_id_fn(), 'CHF-SARON', 0, 'Switzerland', 'ACT/360', 0,
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data'),
    -- Source: QuantLib::Aonia (ql/indexes/ibor/aonia.hpp).
    (ores_utility_system_tenant_id_fn(), 'AUD-AONIA', 0, 'Australia', 'ACT/365.FIXED', 0,
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data'),
    -- Source: QuantExt::CORRA (qle/indexes/ibor/corra.hpp).
    (ores_utility_system_tenant_id_fn(), 'CAD-CORRA', 0, 'Canada.Settlement', 'ACT/365.FIXED', 0,
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data'),
    -- Source: QuantExt::HKDHonia (qle/indexes/ibor/hkdhonia.hpp).
    (ores_utility_system_tenant_id_fn(), 'HKD-HONIA', 0, 'HongKong', 'ACT/365.FIXED', 0,
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data'),
    -- Source: QuantExt::Sora (qle/indexes/ibor/sora.hpp).
    (ores_utility_system_tenant_id_fn(), 'SGD-SORA', 0, 'Singapore', 'ACT/365.FIXED', 0,
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data'),
    -- Source: QuantLib::Swestr (ql/indexes/ibor/swestr.hpp).
    (ores_utility_system_tenant_id_fn(), 'SEK-SWESTR', 0, 'Sweden', 'ACT/360', 0,
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data'),
    -- Source: QuantExt::Nowa (qle/indexes/ibor/nowa.hpp).
    (ores_utility_system_tenant_id_fn(), 'NOK-NOWA', 0, 'Norway', 'ACT/ACT.ISMA', 0,
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data'),
    -- Source: QuantLib::Kofr (ql/indexes/ibor/kofr.hpp).
    (ores_utility_system_tenant_id_fn(), 'KRW-KOFR', 0, 'SouthKorea.Settlement', 'ACT/365.FIXED', 0,
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data'),
    -- Source: QuantExt::INRMiborOis (qle/indexes/ibor/inrmiborois.hpp).
    (ores_utility_system_tenant_id_fn(), 'INR-MIBOR', 0, 'India', 'ACT/365.FIXED', 0,
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data'),
    -- Source: QuantLib::Destr (ql/indexes/ibor/destr.hpp).
    (ores_utility_system_tenant_id_fn(), 'DKK-DESTR', 0, 'Denmark', 'ACT/360', 0,
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data'),
    -- Source: QuantExt::PLNPolonia (qle/indexes/ibor/plnpolonia.hpp).
    (ores_utility_system_tenant_id_fn(), 'PLN-POLONIA', 0, 'Poland', 'ACT/365.FIXED', 1,
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data'),
    -- No ORE index class -- corroborated from SARB market-convention publications
    -- (Act/365, South Africa calendar; see task C80D94BB's Notes for sources).
    (ores_utility_system_tenant_id_fn(), 'ZAR-ZARONIA', 0, 'SouthAfrica', 'ACT/365.FIXED', 0,
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data'),
    -- No ORE index class -- corroborated from RBNZ/domestic NZ money-market convention
    -- (Act/365 Fixed, New Zealand calendar; see task C80D94BB's Notes for sources). Real
    -- benchmark name is NZONIA, not the "NZIONA" the seed data originally (incorrectly) used.
    (ores_utility_system_tenant_id_fn(), 'NZD-NZONIA', 0, 'NewZealand', 'ACT/365.FIXED', 0,
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data')
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    fixing_calendar = excluded.fixing_calendar,
    day_count_fraction = excluded.day_count_fraction,
    settlement_days = excluded.settlement_days,
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

-- Summary
select 'refdata_overnight_index_conventions' as entity, count(*) as count
from ores_refdata_overnight_index_conventions_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
