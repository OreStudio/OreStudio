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
#include <cstdlib>
#include <filesystem>
#include <vector>

/**
 * @file xml_example_coverage_tests.cpp
 * @brief Round-trips every ORE example trade, so that no product sits
 * outside the golden round-trip suites.
 *
 * The per-asset-class golden suites name their examples one by one. A
 * product whose example nobody remembered to add to one of those lists
 * therefore has no serialization coverage at all, and nothing notices.
 * This test walks the example directory instead of a hand-kept list, so
 * an example added upstream fails here until its golden is committed.
 *
 * Set ORES_BOOTSTRAP_GOLDENS=1 to write goldens that are missing. A
 * golden that exists is still compared, so bootstrapping cannot hide a
 * serializer change. Review the resulting diff before committing.
 */

namespace {

const std::string_view test_suite("ores.ore.example.coverage.tests");
const std::string tags("[ore][xml][roundtrip][golden][coverage]");

using ores::ore::domain::portfolio;
using ores::platform::filesystem::file;
using namespace ores::logging;

bool bootstrapping() {
    return std::getenv("ORES_BOOTSTRAP_GOLDENS") != nullptr;
}

std::vector<std::filesystem::path> example_files() {
    std::vector<std::filesystem::path> files;
    const auto dir = ores::testing::project_root::resolve(
        "external/ore/examples/Products/Example_Trades");
    for (const auto& entry : std::filesystem::directory_iterator(dir)) {
        if (entry.is_regular_file() && entry.path().extension() == ".xml")
            files.push_back(entry.path());
    }
    std::sort(files.begin(), files.end());
    return files;
}

}

TEST_CASE("every_example_trade_round_trips_through_a_golden", tags) {
    auto lg(make_logger(test_suite));

    const auto files = example_files();
    REQUIRE_FALSE(files.empty());

    const bool bootstrap = bootstrapping();
    const auto goldens_dir = ores::testing::project_root::resolve(
        "assets/test_data/golden_dataset/Products/Example_Trades");
    std::size_t written = 0;
    std::size_t missing = 0;
    std::size_t mismatched = 0;

    // Each golden is compared rather than required, so that one missing
    // golden does not hide the state of every example after it.
    for (const auto& src : files) {
        const auto name = src.filename().string();
        INFO("Example trade: " + name);

        portfolio p;
        ores::ore::domain::load_data(file::read_content(src), p);
        const std::string canonical = ores::ore::domain::save_data(p);

        const auto gpath = goldens_dir / name;
        const bool exists = std::filesystem::exists(gpath);

        // Bootstrap fills gaps only. An existing golden is always
        // compared, so this mode cannot mask a drifted serializer.
        if (bootstrap && !exists) {
            file::write_content(gpath, canonical);
            ++written;
            continue;
        }

        if (!exists) {
            ++missing;
            INFO("No golden for this example"
                 " (re-run with ORES_BOOTSTRAP_GOLDENS=1 to write it)");
            CHECK(false);
            continue;
        }

        if (canonical != file::read_content(gpath)) {
            ++mismatched;
            INFO("Golden drifted: the serializer output no longer matches the"
                 " committed golden");
            CHECK(false);
        }
    }

    BOOST_LOG_SEV(lg, info) << "Checked " << files.size() << " example trades: " << missing
                            << " had no golden, " << mismatched << " drifted, " << written
                            << " written.";
}
