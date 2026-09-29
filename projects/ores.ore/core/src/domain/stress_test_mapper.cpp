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
#include "ores.ore.core/domain/stress_test_mapper.hpp"
#include <array>
#include <string_view>
#include <utility>

namespace ores::ore::domain {

namespace {

bool is_true(domain::bool_ v) {
    using b = domain::bool_;
    switch (v) {
        case b::Y:
        case b::YES:
        case b::TRUE_:
        case b::True:
        case b::true_:
        case b::_1:
            return true;
        default:
            return false;
    }
}

using family_probe = std::pair<std::string_view, bool>;

/**
 * The shift families a scenario can carry, with whether it does.
 *
 * The document's binding is typed per family, so the probe is written out: one
 * line per family, in the order the schema lists them.
 */
std::array<family_probe, 16> families_of(const stresstest& v) {
    return {{{"ParShifts", static_cast<bool>(v.ParShifts)},
             {"DiscountCurves", static_cast<bool>(v.DiscountCurves)},
             {"IndexCurves", static_cast<bool>(v.IndexCurves)},
             {"YieldCurves", static_cast<bool>(v.YieldCurves)},
             {"FxSpots", static_cast<bool>(v.FxSpots)},
             {"FxVolatilities", static_cast<bool>(v.FxVolatilities)},
             {"SwaptionVolatilities", static_cast<bool>(v.SwaptionVolatilities)},
             {"CapFloorVolatilities", static_cast<bool>(v.CapFloorVolatilities)},
             {"EquitySpots", static_cast<bool>(v.EquitySpots)},
             {"EquityVolatilities", static_cast<bool>(v.EquityVolatilities)},
             {"CommodityCurves", static_cast<bool>(v.CommodityCurves)},
             {"IntradayPowerCurves", static_cast<bool>(v.IntradayPowerCurves)},
             {"CommodityVolatilities", static_cast<bool>(v.CommodityVolatilities)},
             {"SecuritySpreads", static_cast<bool>(v.SecuritySpreads)},
             {"RecoveryRates", static_cast<bool>(v.RecoveryRates)},
             {"SurvivalProbabilities", static_cast<bool>(v.SurvivalProbabilities)}}};
}


}

mapped_stress_test stress_test_mapper::map(const stresstesting& v) {
    mapped_stress_test r;

    if (v.UseSpreadedTermStructures)
        r.library.use_spreaded_term_structures = is_true(*v.UseSpreadedTermStructures);

    int position = 0;
    for (const auto& scenario : v.StressTest) {
        ++position;

        analytics::domain::stress_test_scenario row;
        row.name = scenario.id;
        row.position = position;
        if (scenario.Date)
            row.date = std::string(*scenario.Date);
        r.scenarios.push_back(std::move(row));

        for (const auto& [family, present] : families_of(scenario)) {
            if (present)
                ++r.unmodelled[std::string(family)];
        }
    }

    return r;
}

stresstesting stress_test_mapper::reverse(const mapped_stress_test& v) {
    stresstesting r;

    if (v.library.use_spreaded_term_structures)
        r.UseSpreadedTermStructures =
            *v.library.use_spreaded_term_structures ? domain::bool_::True : domain::bool_::False;

    for (const auto& row : v.scenarios) {
        stresstest scenario;
        scenario.id = row.name;
        if (row.date)
            scenario.Date = stresstest_Date_t(*row.date);
        r.StressTest.push_back(std::move(scenario));
    }

    return r;
}

}
