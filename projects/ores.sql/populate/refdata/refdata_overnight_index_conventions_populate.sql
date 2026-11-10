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
 * Seeds the rows the synthetic.feeds.* seed datasets
 * (ore_samples_2016/bloomberg/uniform_demo) name that ORE does not define
 * itself: a (currency, RFR family) pair whose id is
 * "<currency_code>-<upper(index_family)>", the shape a curve's
 * (currency_code, index_family) pair names. The families ORE builds in code
 * are seeded by refdata_builtin_index_conventions_populate.sql instead, from
 * the engine, so this script holds only what the engine has no index for, and
 * nothing validates a curve against these rows.
 *
 * fixing_calendar values are still refdata.calendars codes (see
 * refdata_calendars_populate.sql), which is a different vocabulary from the
 * ORE calendar names the built-in script states and ORE's parser accepts; see
 * the capture on the two calendar vocabularies. day_count_fraction values are
 * FpML canonical day-count-fraction codes (see
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
    tenant_id, id, version, fixing_calendar, day_count_fraction, settlement_days, oresmd_uri,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    -- Source: QuantLib::Estr (ql/indexes/ibor/estr.hpp/.cpp).
    (ores_utility_system_tenant_id_fn(), 'EUR-ESTR', 0, 'TARGET', 'ACT/360', 0, 'oresmd://ir/EUR?type=fixing&index=ibor&name=ESTER',
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data'),
    -- Source: QuantLib::Tonar (ql/indexes/ibor/tonar.hpp), aka Tona.
    (ores_utility_system_tenant_id_fn(), 'JPY-TONA', 0, 'Japan', 'ACT/365.FIXED', 0, 'oresmd://ir/JPY?type=fixing&index=ibor&name=TONAR',
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data'),
    -- Source: QuantLib::Swestr (ql/indexes/ibor/swestr.hpp).
    (ores_utility_system_tenant_id_fn(), 'SEK-SWESTR', 0, 'Sweden', 'ACT/360', 0, 'oresmd://ir/SEK?type=fixing&index=ibor&name=SWESTR',
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data'),
    -- Source: QuantLib::Kofr (ql/indexes/ibor/kofr.hpp).
    (ores_utility_system_tenant_id_fn(), 'KRW-KOFR', 0, 'SouthKorea.Settlement', 'ACT/365.FIXED', 0, null,
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data'),
    -- Source: QuantExt::INRMiborOis (qle/indexes/ibor/inrmiborois.hpp).
    (ores_utility_system_tenant_id_fn(), 'INR-MIBOR', 0, 'India', 'ACT/365.FIXED', 0, null,
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data'),
    -- Source: QuantLib::Destr (ql/indexes/ibor/destr.hpp).
    (ores_utility_system_tenant_id_fn(), 'DKK-DESTR', 0, 'Denmark', 'ACT/360', 0, null,
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data'),
    -- No ORE index class -- corroborated from SARB market-convention publications
    -- (Act/365, South Africa calendar; see task C80D94BB's Notes for sources).
    (ores_utility_system_tenant_id_fn(), 'ZAR-ZARONIA', 0, 'SouthAfrica', 'ACT/365.FIXED', 0, null,
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data'),
    -- No ORE index class -- corroborated from RBNZ/domestic NZ money-market convention
    -- (Act/365 Fixed, New Zealand calendar; see task C80D94BB's Notes for sources). Real
    -- benchmark name is NZONIA, not the "NZIONA" the seed data originally (incorrectly) used.
    (ores_utility_system_tenant_id_fn(), 'NZD-NZONIA', 0, 'NewZealand', 'ACT/365.FIXED', 0, null,
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data'),
    -- The eight benchmarks below carry the curves the ACME books need beyond
    -- the ones above. Each id and day count is the one ORE's own conventions
    -- use for the benchmark (refdata_conventions_seed_populate.sql), so a
    -- curve the synthetic feeds tick names an index an ORE run can resolve.
    -- CZK CZEONIA: the Czech National Bank's overnight rate.
    (ores_utility_system_tenant_id_fn(), 'CZK-CZEONIA', 0, 'CzechRepublic', 'ACT/365.FIXED', 0, null,
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data'),
    -- HUF BUBORON: the Budapest interbank overnight rate.
    (ores_utility_system_tenant_id_fn(), 'HUF-BUBORON', 0, 'Hungary', 'ACT/365.FIXED', 0, null,
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data'),
    -- ILS TELBORON: the Tel Aviv interbank overnight rate.
    (ores_utility_system_tenant_id_fn(), 'ILS-TELBORON', 0, 'Israel.Settlement', 'ACT/365.FIXED', 0, null,
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data'),
    -- TRY TLREF: Borsa Istanbul's Turkish lira overnight reference rate.
    (ores_utility_system_tenant_id_fn(), 'TRY-TLREF', 0, 'Turkey', 'ACT/365.FIXED', 0, null,
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data'),
    -- COP IBR: the Colombian overnight interbank reference rate. The calendar
    -- CNH HIBOR: the offshore renminbi overnight fixing in Hong Kong.
    (ores_utility_system_tenant_id_fn(), 'CNH-HIBOR', 0, 'HongKong', 'ACT/365.FIXED', 0, null,
     current_user, current_user, 'system.initial_load', 'Overnight index convention reference data')
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do update set
    fixing_calendar = excluded.fixing_calendar,
    day_count_fraction = excluded.day_count_fraction,
    settlement_days = excluded.settlement_days,
    oresmd_uri = coalesce(excluded.oresmd_uri,
                          ores_refdata_overnight_index_conventions_tbl.oresmd_uri),
    modified_by = current_user,
    performed_by = current_user,
    change_reason_code = 'system.initial_load',
    change_commentary = 'Update during initial population';

-- Summary
select 'refdata_overnight_index_conventions' as entity, count(*) as count
from ores_refdata_overnight_index_conventions_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
