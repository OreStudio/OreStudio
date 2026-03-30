/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.ore/domain/domain.hpp"

#include <filesystem>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.testing/project_root.hpp"

namespace {

const std::string_view test_suite("ores.ore.tests");
const std::string tags("[ore][xml][file_io]");

std::filesystem::path ore_path(const std::string& relative) {
    return ores::testing::project_root::resolve("external/ore/" + relative);
}

using namespace ores::logging;

}

// =============================================================================
// load_file / save_data / load_data roundtrip tests
// =============================================================================

TEST_CASE("currency_config_load_file_save_file_roundtrip", tags) {
    auto lg(make_logger(test_suite));

    using ores::ore::domain::currencyConfig;

    const auto input = ore_path("examples/Input/currencies.xml");
    BOOST_LOG_SEV(lg, debug) << "Loading from file: " << input;

    currencyConfig original;
    ores::ore::domain::load_file(input.string(), original);
    REQUIRE(original.Currency.size() == 179);

    const std::string serialised = ores::ore::domain::save_data(original);

    currencyConfig reloaded;
    ores::ore::domain::load_data(serialised, reloaded);
    CHECK(reloaded.Currency.size() == original.Currency.size());

    BOOST_LOG_SEV(lg, info) << "File I/O roundtrip passed for currencyConfig";
}

TEST_CASE("simulation_load_file_save_file_roundtrip", tags) {
    auto lg(make_logger(test_suite));

    using ores::ore::domain::simulation;

    const auto input = ore_path("examples/ORE-API/Input/simulation.xml");
    BOOST_LOG_SEV(lg, debug) << "Loading from file: " << input;

    simulation original;
    ores::ore::domain::load_file(input.string(), original);

    const std::string serialised = ores::ore::domain::save_data(original);

    simulation reloaded;
    ores::ore::domain::load_data(serialised, reloaded);

    CHECK(static_cast<bool>(reloaded.Parameters) ==
          static_cast<bool>(original.Parameters));
    CHECK(static_cast<bool>(reloaded.CrossAssetModel) ==
          static_cast<bool>(original.CrossAssetModel));

    BOOST_LOG_SEV(lg, info) << "File I/O roundtrip passed for simulation";
}

TEST_CASE("todaysmarket_load_file_save_file_roundtrip", tags) {
    auto lg(make_logger(test_suite));

    using ores::ore::domain::todaysmarket;

    const auto input = ore_path("examples/ORE-API/Input/todaysmarket.xml");
    BOOST_LOG_SEV(lg, debug) << "Loading from file: " << input;

    todaysmarket original;
    ores::ore::domain::load_file(input.string(), original);
    REQUIRE(!original.Configuration.empty());

    const std::string serialised = ores::ore::domain::save_data(original);

    todaysmarket reloaded;
    ores::ore::domain::load_data(serialised, reloaded);
    CHECK(reloaded.Configuration.size() == original.Configuration.size());

    BOOST_LOG_SEV(lg, info) << "File I/O roundtrip passed for todaysmarket";
}
