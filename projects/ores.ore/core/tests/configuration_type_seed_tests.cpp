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
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.reporting.core/repository/configuration_type_repository.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <catch2/catch_test_macros.hpp>
#include <functional>
#include <map>
#include <set>
#include <string>
#include <string_view>

/**
 * @file configuration_type_seed_tests.cpp
 * @brief The seeded configuration types against the ORE binding.
 *
 * Each configuration type records the root element of the ORE document it
 * serialises to. For every kind the binding can save, an empty document is
 * saved and must open with that element, so the seed cannot name a root the
 * binding does not write. The kinds the binding cannot save yet are listed, so
 * a kind that gains a binding without joining this check fails here.
 */

namespace {

const std::string_view test_suite("ores.ore.tests");
const std::string tags("[ore][configuration_types][database]");

using namespace ores::logging;

template <typename Document>
std::string saved_empty() {
    return ores::ore::domain::save_data(Document{});
}

const std::map<std::string, std::function<std::string()>>& savers() {
    using namespace ores::ore::domain;
    static const std::map<std::string, std::function<std::string()>> table = {
        {"pricing_engines", saved_empty<pricingengines>},
        {"todays_market", saved_empty<todaysmarket>},
        {"simulation", saved_empty<simulation>},
        {"sensitivity", saved_empty<sensitivityanalysis>},
        {"stress_test", saved_empty<stresstesting>},
        {"credit_simulation", saved_empty<creditsimulation>},
        {"curve_configuration", saved_empty<curveconfiguration>},
        {"conventions", saved_empty<conventions>},
        {"calendar_adjustment", saved_empty<calendaradjustment>},
        {"currency_configuration", saved_empty<currencyConfig>},
        {"counterparty_information", saved_empty<counterpartyInformation>},
        {"netting_set_definitions", saved_empty<nettingsetdefinitions>},
    };
    return table;
}

// The first element of the saved document must be exactly the seeded root, so
// a seeded name that is only a prefix of the real one does not pass.
bool opens_with_element(const std::string& xml, const std::string& name) {
    const auto start = xml.find('<' + name);
    if (start == std::string::npos)
        return false;
    const auto after = start + 1 + name.size();
    if (after >= xml.size())
        return false;
    const char next = xml[after];
    return next == '>' || next == '/' || next == ' ' || next == '\n' || next == '\t';
}

const std::set<std::string> unbound = {"simm_calibration",
                                       "historical_return",
                                       "basel_traffic_light",
                                       "reference_data",
                                       "ibor_fallback"};

}

TEST_CASE("every seeded configuration type the binding can save names its root element", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;

    // The kinds are seeded once for the system tenant and shared by every tenant.
    const auto sys_ctx = h.context().with_tenant(ores::utility::uuid::tenant_id::system(), "");
    ores::reporting::repository::configuration_type_repository repo;
    const auto types = repo.read_latest(sys_ctx);
    REQUIRE(types.size() == savers().size() + unbound.size());

    std::set<std::string> roots;
    for (const auto& type : types) {
        INFO(type.code);
        CHECK(roots.insert(type.ore_root_element).second);
        CHECK(!type.owning_component.empty());

        const auto saver = savers().find(type.code);
        if (saver == savers().end()) {
            CHECK(unbound.contains(type.code));
            continue;
        }
        const auto xml = saver->second();
        INFO(xml.substr(0, 200));
        CHECK(opens_with_element(xml, type.ore_root_element));
    }
    BOOST_LOG_SEV(lg, info) << "Checked " << types.size() << " configuration types";
}
