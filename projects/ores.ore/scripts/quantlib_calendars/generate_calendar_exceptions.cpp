/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
// compass:except-file(entity_writes/cpp_writer) expires=2027-05-18 approver=marco reason=a build-time generator that prints the SQL loading the QuantLib calendar holidays into the system tenant; it never runs against a database; recorded in entity_table_writes_baseline.json
#include <ql/time/calendars/all.hpp>
#include <ql/version.hpp>
#include <cstdio>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <map>
#include <string>
#include <utility>
#include <vector>

using namespace QuantLib;

namespace {

constexpr Year first_year = 2020;
constexpr Year last_year = 2060;

std::vector<std::pair<std::string, Calendar>> published_calendars() {
    return {
        {"TARGET", TARGET()},
        {"WeekendsOnly", WeekendsOnly()},
        {"Argentina", Argentina()},
        {"Australia", Australia()},
        {"Austria", Austria()},
        {"Botswana", Botswana()},
        {"Brazil", Brazil()},
        {"Chile", Chile()},
        {"CzechRepublic", CzechRepublic()},
        {"Denmark", Denmark()},
        {"Finland", Finland()},
        {"France", France()},
        {"HongKong", HongKong()},
        {"Hungary", Hungary()},
        {"Iceland", Iceland()},
        {"India", India()},
        {"Italy", Italy()},
        {"Japan", Japan()},
        {"Mexico", Mexico()},
        {"NewZealand", NewZealand()},
        {"Norway", Norway()},
        {"Poland", Poland()},
        {"Romania", Romania()},
        {"Russia", Russia()},
        {"SaudiArabia", SaudiArabia()},
        {"Singapore", Singapore()},
        {"Slovakia", Slovakia()},
        {"SouthAfrica", SouthAfrica()},
        {"Sweden", Sweden()},
        {"Switzerland", Switzerland()},
        {"Taiwan", Taiwan()},
        {"Thailand", Thailand()},
        {"Turkey", Turkey()},
        {"Ukraine", Ukraine()},
        {"UnitedStates.Settlement", UnitedStates(UnitedStates::Settlement)},
        {"UnitedStates.NYSE", UnitedStates(UnitedStates::NYSE)},
        {"UnitedStates.GovernmentBond", UnitedStates(UnitedStates::GovernmentBond)},
        {"UnitedStates.NERC", UnitedStates(UnitedStates::NERC)},
        {"UnitedStates.LiborImpact", UnitedStates(UnitedStates::LiborImpact)},
        {"UnitedStates.FederalReserve", UnitedStates(UnitedStates::FederalReserve)},
        {"UnitedStates.SOFR", UnitedStates(UnitedStates::SOFR)},
        {"UnitedKingdom.Settlement", UnitedKingdom(UnitedKingdom::Settlement)},
        {"UnitedKingdom.Exchange", UnitedKingdom(UnitedKingdom::Exchange)},
        {"UnitedKingdom.Metals", UnitedKingdom(UnitedKingdom::Metals)},
        {"China.SSE", China(China::SSE)},
        {"China.IB", China(China::IB)},
        {"Germany.Settlement", Germany(Germany::Settlement)},
        {"Germany.FrankfurtStockExchange", Germany(Germany::FrankfurtStockExchange)},
        {"Germany.Xetra", Germany(Germany::Xetra)},
        {"Germany.Eurex", Germany(Germany::Eurex)},
        {"Germany.Euwax", Germany(Germany::Euwax)},
        {"Canada.Settlement", Canada(Canada::Settlement)},
        {"Canada.TSX", Canada(Canada::TSX)},
        {"SouthKorea.Settlement", SouthKorea(SouthKorea::Settlement)},
        {"SouthKorea.KRX", SouthKorea(SouthKorea::KRX)},
        {"Indonesia.BEJ", Indonesia(Indonesia::BEJ)},
        {"Indonesia.JSX", Indonesia(Indonesia::JSX)},
        {"Indonesia.IDX", Indonesia(Indonesia::IDX)},
        {"Israel.Settlement", Israel(Israel::Settlement)},
        {"Israel.TASE", Israel(Israel::TASE)},
    };
}

bool is_plain_weekday(const Date& d) {
    return d.weekday() != Saturday && d.weekday() != Sunday;
}

std::string iso(const Date& d) {
    char text[11];
    std::snprintf(text,
                  sizeof text,
                  "%04d-%02d-%02d",
                  static_cast<int>(d.year()),
                  static_cast<int>(d.month()),
                  d.dayOfMonth());
    return text;
}

}

int main(int argc, char** argv) {
    if (argc != 2) {
        std::cerr << "usage: generate_calendar_exceptions <output.sql>\n";
        return 2;
    }
    std::ofstream out(argv[1]);
    out << R"PREFIX(/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
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

/*
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Generated by projects/ores.ore/scripts/quantlib_calendars/generate.sh
 *
 * Registers the refdata.calendar_exceptions dataset and stages its rows: each
 * is a day on which a QuantLib calendar differs from a Monday to Friday week,
 * so a Friday weekend needs no rule of its own.
 *
 * The dataset is the one writer of ores_refdata_calendar_exceptions_tbl. The
 * bring-up publishes it into the system tenant, and no populate script inserts
 * the live rows.
 */

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.calendar_exceptions',
        'Calendar Exception Reference Data',
        'Calendars',
        'Reference Data',
        'NONE',
        'Primary',
        'Actual',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Calendar Exceptions',
        'Days on which a calendar differs from a Monday to Friday week, generated from QuantLib.',
        'QUANTLIB',
        'Seed data for the calendar exceptions Librarian bundle',
        current_date,
        'Internal Use Only',
        'calendar_exceptions'
    );

    PERFORM ores_dq_dataset_dependencies_upsert_fn(ores_utility_system_tenant_id_fn(),
        'refdata.calendar_exceptions',
        'refdata.calendars',
        'calendar_reference'
    );
END $$;

\echo ''
\echo '--- DQ Calendar Exceptions Artefact Seed ---'

DO $$
declare
    v_dataset_id uuid;
    v_tenant_id uuid := ores_utility_system_tenant_id_fn();
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where tenant_id = v_tenant_id
      and code = 'refdata.calendar_exceptions'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset not found: refdata.calendar_exceptions';
    end if;

    delete from ores_dq_calendar_exceptions_artefact_tbl
    where dataset_id = v_dataset_id;

    insert into ores_dq_calendar_exceptions_artefact_tbl (
        dataset_id, tenant_id, id, version, calendar_code, exception_date,
        is_business_day, description
    )
    select
        v_dataset_id, v_tenant_id, md5(v.code || v.day)::uuid, 0, v.code, v.day::date,
        v.open, 'QuantLib )PREFIX"
        << QL_VERSION << R"PREFIX('
    from (values
)PREFIX";

    std::cout << "QuantLib " << QL_VERSION << ", " << first_year << " to " << last_year << "\n";
    std::cout << "calendar,rows,closed_2026,closed_2035,closed_2050\n";
    std::size_t total = 0;
    bool first = true;
    for (const auto& [code, calendar] : published_calendars()) {
        std::size_t rows = 0;
        std::map<Year, int> closed;
        const Date end(31, December, last_year);
        for (Date d(1, January, first_year); d <= end; ++d) {
            const bool business = calendar.isBusinessDay(d);
            if (!business && is_plain_weekday(d))
                ++closed[d.year()];
            if (business == is_plain_weekday(d))
                continue;
            out << (first ? " " : ",") << "('" << code << "','" << iso(d) << "',"
                << (business ? "true" : "false") << ")\n";
            first = false;
            ++rows;
        }
        total += rows;
        std::cout << code << ',' << rows << ',' << closed[2026] << ',' << closed[2035] << ','
                  << closed[2050] << '\n';
    }
    out << R"SUFFIX() as v(code, day, open);
end $$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- DQ Calendar Exceptions Summary ---'

select 'Total DQ Calendar Exceptions' as metric, count(*) as count
from ores_dq_calendar_exceptions_artefact_tbl;
)SUFFIX";
    std::cout << "total," << total << "\n";
    return 0;
}
