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

const std::string_view test_suite("ores.ore.creditsimulation.roundtrip.tests");
const std::string tags("[ore][xml][roundtrip][creditsimulation]");

std::filesystem::path ore_path(const std::string& relative) {
    return ores::testing::project_root::resolve("external/ore/" + relative);
}

using ores::ore::domain::creditsimulation;
using namespace ores::logging;

void require_creditsimulation_equal(const creditsimulation& original,
                                     const creditsimulation& roundtripped) {
    CHECK(roundtripped.Entities.Entity.size() ==
          original.Entities.Entity.size());
}

void test_roundtrip_from_file(const std::string& relative_path) {
    auto lg(make_logger(test_suite));

    const auto f = ore_path(relative_path);
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(f);

    BOOST_LOG_SEV(lg, debug) << "Parsing original XML from: " << f.string();
    creditsimulation original;
    ores::ore::domain::load_data(content, original);

    BOOST_LOG_SEV(lg, debug) << "Serializing to XML";
    const std::string serialized = ores::ore::domain::save_data(original);

    BOOST_LOG_SEV(lg, debug) << "Parsing serialized XML";
    creditsimulation roundtripped;
    ores::ore::domain::load_data(serialized, roundtripped);

    require_creditsimulation_equal(original, roundtripped);
    BOOST_LOG_SEV(lg, info) << "Roundtrip test passed for: " << f.string();
}

}

TEST_CASE("creditsimulation_roundtrip_credit_portfolio_model", tags) {
    test_roundtrip_from_file(
        "examples/CreditRisk/Input/CreditPortfolioModel/creditsimulation.xml");
}
