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
#include "ores.ore.core/xml/binding.hpp"
#include "ores.ore.core/xml/generated/calendar_adjustments.hpp"
#include "ores.ore.core/xml/generated/conventions.hpp"
#include "ores.ore.core/xml/generated/counterparty_information.hpp"
#include "ores.ore.core/xml/generated/credit_simulation.hpp"
#include "ores.ore.core/xml/generated/currency_config.hpp"
#include "ores.ore.core/xml/generated/curve_configuration.hpp"
#include "ores.ore.core/xml/generated/netting_set_definitions.hpp"
#include "ores.ore.core/xml/generated/ore.hpp"
#include "ores.ore.core/xml/generated/pricing_engines.hpp"
#include "ores.ore.core/xml/generated/stress_testing.hpp"
#include "ores.ore.core/xml/generated/todays_market.hpp"
#include "ores.reporting.core/repository/configuration_type_repository.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <map>
#include <regex>
#include <set>
#include <string>
#include <string_view>

/**
 * @file configuration_type_seed_tests.cpp
 * @brief The seeded configuration types against the ORE codec.
 *
 * Each configuration type records the root element of the ORE document it
 * serialises to. For every kind the codec carries, the root element the codec
 * writes must be that element, so the seed cannot name a root the codec does
 * not write. The kinds the codec does not carry are listed, so a kind that
 * gains a table without joining this check fails here. Every seeded
 * run-document parameter must also appear in a shipped run document.
 */

namespace {

using namespace ores::logging;

namespace xg = ores::ore::xml::generated;

// The root element the codec writes for each kind it carries.
const std::map<std::string, std::string>& roots_written() {
    static const std::map<std::string, std::string> table = {
        {"pricing_engines", std::string(xg::pricing_engines::pricing_engines_document().root_name)},
        {"risk_report_config", std::string(xg::ore::ore_document().root_name)},
        {"engine_settings", std::string(xg::ore::ore_document().root_name)},
        {"todays_market", std::string(xg::todays_market::todays_market_document().root_name)},
        {"stress_test", std::string(xg::stress_testing::stress_testing_document().root_name)},
        {"credit_simulation",
         std::string(xg::credit_simulation::credit_simulation_document().root_name)},
        {"curve_configuration",
         std::string(xg::curve_configuration::curve_configuration_document().root_name)},
        {"conventions", std::string(xg::conventions::conventions_document().root_name)},
        {"calendar_adjustment",
         std::string(xg::calendar_adjustments::calendar_adjustments_document().root_name)},
        {"currency_configuration",
         std::string(xg::currency_config::currency_config_document().root_name)},
        {"counterparty_information",
         std::string(xg::counterparty_information::counterparty_information_document().root_name)},
        {"netting_set_definitions",
         std::string(xg::netting_set_definitions::netting_set_definitions_document().root_name)},
    };
    return table;
}

// The first element of the saved document, after any declaration or comment,
// must be exactly the seeded root, so neither a nested element of that name
// nor a seeded name that is only a prefix of the real one passes.
bool opens_with_element(const std::string& xml, const std::string& name) {
    auto start = xml.find('<');
    while (start != std::string::npos && start + 1 < xml.size() &&
           (xml[start + 1] == '?' || xml[start + 1] == '!'))
        start = xml.find('<', start + 1);
    if (start == std::string::npos || xml.compare(start + 1, name.size(), name) != 0)
        return false;
    const auto after = start + 1 + name.size();
    if (after >= xml.size())
        return false;
    const char next = xml[after];
    return next == '>' || next == '/' || next == ' ' || next == '\n' || next == '\t';
}

// Every parameter name the shipped run documents use, so a seeded run_parameter
// that no ORE run document writes is caught as a typo.
std::set<std::string> run_document_parameters() {
    std::set<std::string> names;
    const std::regex parameter(R"re(<Parameter name="([A-Za-z]+)")re");
    const auto root = ores::testing::project_root::resolve("external/ore/examples");
    for (const auto& entry : std::filesystem::recursive_directory_iterator(root)) {
        const auto file = entry.path().filename().string();
        if (!entry.is_regular_file() || !file.starts_with("ore") || !file.ends_with(".xml"))
            continue;
        std::ifstream in(entry.path());
        const std::string content((std::istreambuf_iterator<char>(in)),
                                  std::istreambuf_iterator<char>());
        for (std::sregex_iterator it(content.begin(), content.end(), parameter), end; it != end;
             ++it)
            names.insert((*it)[1].str());
    }
    return names;
}

// The kinds that are parts of the run document, so they share its root element. Every
// other kind has a root of its own.
const std::set<std::string> run_document_kinds = {"risk_report_config", "engine_settings"};

const std::set<std::string> unbound = {"simulation",
                                       "sensitivity",
                                       "simm_calibration",
                                       "historical_return",
                                       "basel_traffic_light",
                                       "reference_data",
                                       "ibor_fallback"};

}

TEST_CASE("every seeded configuration type the codec carries names its root element", "[ore][configuration_types][database]") {
    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::scoped_database_helper h;

    // The kinds are seeded once for the system tenant and shared by every tenant.
    const auto sys_ctx = h.context().with_tenant(ores::utility::uuid::tenant_id::system(), "");
    ores::reporting::repository::configuration_type_repository repo;
    const auto types = repo.read_latest(sys_ctx);
    REQUIRE(types.size() == roots_written().size() + unbound.size());

    const auto parameters = run_document_parameters();
    REQUIRE(!parameters.empty());

    std::set<std::string> roots;
    for (const auto& type : types) {
        INFO(type.code);
        if (!run_document_kinds.contains(type.code))
            CHECK(roots.insert(type.ore_root_element).second);
        CHECK(!type.owning_component.empty());
        if (type.run_parameter)
            CHECK(parameters.contains(*type.run_parameter));

        const auto written = roots_written().find(type.code);
        if (written == roots_written().end()) {
            CHECK(unbound.contains(type.code));
            continue;
        }
        CHECK(opens_with_element("<" + written->second + "/>", type.ore_root_element));
    }
    BOOST_LOG_SEV(lg, info) << "Checked " << types.size() << " configuration types";
}
