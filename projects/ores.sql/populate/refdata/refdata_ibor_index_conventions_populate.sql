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
 * Ibor Index Conventions Population Script
 *
 * Seeds one ores_refdata_ibor_index_conventions_tbl row per (currency, term
 * index family) pair used by the synthetic.ir_curve_configs.* seed themes --
 * id is "<currency_code>-<upper(index_family)>", matching
 * ir_curve_generation_config_service::validate_index_family()'s own lookup
 * shape. Covers both the 3M/6M-tenor legacy LIBOR/EURIBOR curves
 * (ore_samples_2016) and the overnight-tenor CNY/MXN/TWD curves
 * (realistic_2026/uniform_demo) whose real-world benchmark is technically
 * an IborIndex, not an OvernightIndex, even though this codebase's
 * synthetic themes use them at an overnight tenor.
 *
 * fixing_calendar values are refdata.calendars codes (see
 * refdata_calendars_populate.sql); day_count_fraction values are FpML
 * canonical day-count-fraction codes; business_day_convention values are
 * refdata.business_day_convention_types codes (see
 * refdata_business_day_convention_types_populate.sql).
 *
 * Sourced from ORE's own vendored QuantLib/QuantExt index class
 * definitions (real conventions, not fabricated) -- see each row's own
 * comment for its source class.
 *
 * This script is idempotent - uses INSERT ON CONFLICT DO UPDATE.
 */

\echo '--- Ibor Index Conventions ---'

insert into ores_refdata_ibor_index_conventions_tbl (
    tenant_id, id, version, fixing_calendar, day_count_fraction, settlement_days,
    business_day_convention, end_of_month,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    -- Source: QuantLib::USDLibor (ql/indexes/ibor/usdlibor.hpp), 3M/6M tenor conventions
    -- (ql/indexes/ibor/libor.cpp's liborConvention()/liborEOM()).
    (ores_utility_system_tenant_id_fn(), 'USD-LIBOR', 0, 'UnitedStates.LiborImpact', 'ACT/360', 2,
     'ModifiedFollowing', true,
     current_user, current_user, 'system.initial_load', 'Ibor index convention reference data'),
    -- Source: QuantLib::GBPLibor (ql/indexes/ibor/gbplibor.hpp).
    (ores_utility_system_tenant_id_fn(), 'GBP-LIBOR', 0, 'UnitedKingdom.Exchange', 'ACT/365.FIXED', 0,
     'ModifiedFollowing', true,
     current_user, current_user, 'system.initial_load', 'Ibor index convention reference data'),
    -- Source: QuantLib::JPYLibor (ql/indexes/ibor/jpylibor.hpp).
    (ores_utility_system_tenant_id_fn(), 'JPY-LIBOR', 0, 'Japan', 'ACT/360', 2,
     'ModifiedFollowing', true,
     current_user, current_user, 'system.initial_load', 'Ibor index convention reference data'),
    -- Source: QuantLib::Euribor (ql/indexes/ibor/euribor.hpp/.cpp), 3M/6M tenor conventions
    -- (euriborConvention()/euriborEOM()).
    (ores_utility_system_tenant_id_fn(), 'EUR-EURIBOR', 0, 'TARGET', 'ACT/360', 2,
     'ModifiedFollowing', true,
     current_user, current_user, 'system.initial_load', 'Ibor index convention reference data'),
    -- Source: QuantLib::Shibor (ql/indexes/ibor/shibor.hpp/.cpp), overnight-tenor conventions
    -- (shiborConvention() at Days granularity: Following, settlement 0, EOM false).
    (ores_utility_system_tenant_id_fn(), 'CNY-SHIBOR', 0, 'China.IB', 'ACT/360', 0,
     'Following', false,
     current_user, current_user, 'system.initial_load', 'Ibor index convention reference data'),
    -- Source: QuantExt::MXNTiie (qle/indexes/ibor/mxntiie.hpp).
    (ores_utility_system_tenant_id_fn(), 'MXN-TIIE', 0, 'Mexico', 'ACT/360', 1,
     'Following', false,
     current_user, current_user, 'system.initial_load', 'Ibor index convention reference data'),
    -- Source: QuantExt::TWDTaibor (qle/indexes/ibor/twdtaibor.hpp).
    (ores_utility_system_tenant_id_fn(), 'TWD-TAIBOR', 0, 'Taiwan', 'ACT/365.FIXED', 2,
     'ModifiedFollowing', false,
     current_user, current_user, 'system.initial_load', 'Ibor index convention reference data')
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    fixing_calendar = excluded.fixing_calendar,
    day_count_fraction = excluded.day_count_fraction,
    settlement_days = excluded.settlement_days,
    business_day_convention = excluded.business_day_convention,
    end_of_month = excluded.end_of_month,
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

-- Summary
select 'refdata_ibor_index_conventions' as entity, count(*) as count
from ores_refdata_ibor_index_conventions_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
