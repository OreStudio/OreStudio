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
#include "ores.ore.core/xml/importer.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.trading.api/domain/bond_instrument_rows.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <set>
#include <string>
#include <variant>
#include <vector>

namespace {

namespace fs = std::filesystem;

using ores::ore::xml::importer;
using ores::trading::domain::bond_instrument_data;

/*
 * The container a booking starts from: the members that are not rows of the
 * trade or the security. Assembling the rows onto it must give back the whole.
 */
bond_instrument_data header_of(const bond_instrument_data& data) {
    bond_instrument_data out;
    out.instrument = data.instrument;
    out.issue = data.issue;
    out.call_dates = data.call_dates;
    out.conversion_targets = data.conversion_targets;
    return out;
}

}

TEST_CASE("every bond in the corpus comes back whole from its rows",
          "[ore][xml][bond][rows][corpus][external]") {
    const auto dir = ores::testing::project_root::resolve("external/ore/examples");
    REQUIRE(fs::exists(dir));

    std::size_t bonds = 0;
    std::set<std::string> differing;
    for (const auto& entry : fs::recursive_directory_iterator(dir)) {
        if (!entry.is_regular_file() || entry.path().extension() != ".xml")
            continue;

        std::vector<ores::ore::xml::trade_import_item> items;
        try {
            items = importer::import_portfolio_with_context(entry.path());
        } catch (const std::exception&) {
            continue;
        }

        for (const auto& item : items) {
            const auto* bond = std::get_if<bond_instrument_data>(&item.instrument);
            if (bond == nullptr)
                continue;
            ++bonds;

            const auto rows = ores::trading::domain::disassemble_bond(*bond);
            auto rebuilt = header_of(*bond);
            ores::trading::domain::assemble_bond(rebuilt, rows.trade, rows.issue);
            if (!(rebuilt == *bond))
                differing.insert(entry.path().filename().string() + " (" + item.ore_id + ")");
        }
    }

    INFO("bonds=" << bonds);
    CHECK(bonds > 0);
    std::string listed;
    for (const auto& name : differing)
        listed += "\n  " + name;
    if (!differing.empty())
        FAIL_CHECK("these bonds do not come back whole from their rows:" + listed);
}
