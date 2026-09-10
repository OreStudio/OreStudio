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
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <string>
#include <vector>

/**
 * @file xml_bond_source_fidelity_tests.cpp
 * @brief Fidelity of our writing against the ORE documents we read.
 *
 * The golden suite compares our output to a stored copy of our own
 * output. That catches drift, but it cannot catch a write the reader
 * does not read back the same way. This suite closes the gap over the
 * source documents in external/ore: a document canonicalises to a form
 * that re-reads to itself, so the read and the write lose nothing.
 */

namespace {

const std::string_view test_suite("ores.ore.bond.source.fidelity.tests");
const std::string tags("[ore][xml][roundtrip][bond][fidelity]");

using ores::ore::domain::portfolio;
using namespace ores::logging;

const std::vector<std::string> bond_trade_types = {"Bond",
                                                   "ForwardBond",
                                                   "BondFuture",
                                                   "BondOption",
                                                   "BondTRS",
                                                   "BondRepo",
                                                   "BondPosition",
                                                   "CallableBond",
                                                   "ConvertibleBond",
                                                   "Ascot"};

std::filesystem::path example_dir() {
    return ores::testing::project_root::resolve("external/ore/examples/Products/Example_Trades");
}

bool states_a_bond_trade(const portfolio& p) {
    for (const auto& t : p.Trade) {
        const std::string type = ores::ore::domain::to_string(t.TradeType);
        if (std::find(bond_trade_types.begin(), bond_trade_types.end(), type) !=
            bond_trade_types.end())
            return true;
    }
    return false;
}

std::string canonical_form(const std::string& xml) {
    portfolio p;
    ores::ore::domain::load_data(xml, p);
    return ores::ore::domain::save_data(p);
}

void require_stable_canonical_form(const std::string& filename) {
    using ores::platform::filesystem::file;

    const std::string source = file::read_content(example_dir() / filename);
    const std::string once = canonical_form(source);
    const std::string twice = canonical_form(once);

    INFO("Document: " << filename);
    CHECK(!once.empty());
    CHECK(twice == once);
}

} // namespace

TEST_CASE("bond_source_documents_reload_to_their_own_canonical_form", tags) {
    auto lg(make_logger(test_suite));
    using ores::platform::filesystem::file;

    std::vector<std::filesystem::path> documents;
    for (const auto& entry : std::filesystem::directory_iterator(example_dir())) {
        if (entry.path().extension() != ".xml")
            continue;
        portfolio p;
        ores::ore::domain::load_data(file::read_content(entry.path()), p);
        if (states_a_bond_trade(p))
            documents.push_back(entry.path());
    }

    std::sort(documents.begin(), documents.end());
    REQUIRE(!documents.empty());
    BOOST_LOG_SEV(lg, info) << "Bond source documents: " << documents.size();

    for (const auto& document : documents)
        require_stable_canonical_form(document.filename().string());

    BOOST_LOG_SEV(lg, info) << "Every bond source document canonicalises to a stable form.";
}
