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

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"

namespace {

const std::string_view test_suite("ores.ore.ore.roundtrip.tests");
const std::string tags("[ore][xml][roundtrip][ore_config]");

std::filesystem::path ore_path(const std::string& relative) {
    return ores::testing::project_root::resolve("external/ore/" + relative);
}

using ores::ore::domain::ore;
using namespace ores::logging;

void require_ore_equal(const ore& original, const ore& roundtripped) {
    REQUIRE(roundtripped.Setup.Parameter.size() ==
            original.Setup.Parameter.size());
    for (size_t i = 0; i < original.Setup.Parameter.size(); ++i) {
        CHECK(roundtripped.Setup.Parameter.at(i).name ==
              original.Setup.Parameter.at(i).name);
    }
    CHECK(static_cast<bool>(roundtripped.Logging) ==
          static_cast<bool>(original.Logging));
    CHECK(static_cast<bool>(roundtripped.Markets) ==
          static_cast<bool>(original.Markets));
    REQUIRE(roundtripped.Analytics.Analytic.size() ==
            original.Analytics.Analytic.size());
    for (size_t i = 0; i < original.Analytics.Analytic.size(); ++i) {
        CHECK(roundtripped.Analytics.Analytic.at(i).type ==
              original.Analytics.Analytic.at(i).type);
        CHECK(roundtripped.Analytics.Analytic.at(i).Parameter.size() ==
              original.Analytics.Analytic.at(i).Parameter.size());
    }
}

void test_roundtrip_from_file(const std::string& relative_path) {
    auto lg(make_logger(test_suite));

    const auto f = ore_path(relative_path);
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(f);

    BOOST_LOG_SEV(lg, debug) << "Parsing original XML from: " << f.string();
    ore original;
    ores::ore::domain::load_data(content, original);

    BOOST_LOG_SEV(lg, debug) << "Serializing to XML";
    const std::string serialized = ores::ore::domain::save_data(original);

    BOOST_LOG_SEV(lg, debug) << "Parsing serialized XML";
    ore roundtripped;
    ores::ore::domain::load_data(serialized, roundtripped);

    require_ore_equal(original, roundtripped);
    BOOST_LOG_SEV(lg, info) << "Roundtrip test passed for: " << f.string();
}

}

TEST_CASE("ore_roundtrip_minimal_setup", tags) {
    test_roundtrip_from_file("examples/MinimalSetup/Input/ore.xml");
}

TEST_CASE("ore_roundtrip_ore_api", tags) {
    test_roundtrip_from_file("examples/ORE-API/Input/ore.xml");
}
