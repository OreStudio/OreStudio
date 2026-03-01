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
#include "ores.ore/scanner/ore_directory_scanner.hpp"

#include <atomic>
#include <fstream>
#include <string>
#include <filesystem>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.ore.tests");
const std::string tags("[ore][scanner][directory_scanner]");

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

struct temp_dir {
    std::filesystem::path path;

    temp_dir() {
        static std::atomic<int> counter{0};
        path = std::filesystem::temp_directory_path() /
               ("ores_scanner_test_" + std::to_string(++counter));
        std::filesystem::create_directories(path);
    }

    ~temp_dir() {
        std::error_code ec;
        std::filesystem::remove_all(path, ec);
    }

    void touch(const std::filesystem::path& rel) const {
        const auto full = path / rel;
        std::filesystem::create_directories(full.parent_path());
        std::ofstream{full};  // create empty file
    }
};

bool contains(const std::vector<std::filesystem::path>& v,
              const std::filesystem::path& p) {
    return std::find(v.begin(), v.end(), p) != v.end();
}

}

using ores::ore::scanner::ore_directory_scanner;
using namespace ores::logging;

// =============================================================================
// Classification tests
// =============================================================================

TEST_CASE("scan_empty_directory_produces_empty_results", tags) {
    auto lg(make_logger(test_suite));

    temp_dir d;
    ore_directory_scanner scanner(d.path);
    const auto result = scanner.scan();

    BOOST_LOG_SEV(lg, debug) << "Empty scan: "
                             << result.currency_files.size() << " currency, "
                             << result.portfolio_files.size() << " portfolio";

    CHECK(result.currency_files.empty());
    CHECK(result.portfolio_files.empty());
    CHECK(result.ignored_files.empty());
    CHECK(result.root == d.path);
}

TEST_CASE("scan_classifies_currencyconfig_as_currency", tags) {
    auto lg(make_logger(test_suite));

    temp_dir d;
    d.touch("currencyconfig.xml");

    ore_directory_scanner scanner(d.path);
    const auto result = scanner.scan();

    BOOST_LOG_SEV(lg, debug) << "Currency files: " << result.currency_files.size();

    REQUIRE(result.currency_files.size() == 1);
    CHECK(result.portfolio_files.empty());
    CHECK(contains(result.currency_files, d.path / "currencyconfig.xml"));
}

TEST_CASE("scan_classifies_portfolio_xml_as_portfolio", tags) {
    auto lg(make_logger(test_suite));

    temp_dir d;
    d.touch("portfolio.xml");
    d.touch("portfolio_swaps.xml");
    d.touch("portfolioFX.xml");

    ore_directory_scanner scanner(d.path);
    const auto result = scanner.scan();

    BOOST_LOG_SEV(lg, debug) << "Portfolio files: " << result.portfolio_files.size();

    REQUIRE(result.portfolio_files.size() == 3);
    CHECK(result.currency_files.empty());
    CHECK(contains(result.portfolio_files, d.path / "portfolio.xml"));
    CHECK(contains(result.portfolio_files, d.path / "portfolio_swaps.xml"));
    CHECK(contains(result.portfolio_files, d.path / "portfolioFX.xml"));
}

TEST_CASE("scan_ignores_unrecognised_xml_files", tags) {
    auto lg(make_logger(test_suite));

    temp_dir d;
    d.touch("ore.xml");
    d.touch("config.xml");
    d.touch("readme.txt");

    ore_directory_scanner scanner(d.path);
    const auto result = scanner.scan();

    CHECK(result.currency_files.empty());
    CHECK(result.portfolio_files.empty());
    CHECK(result.ignored_files.size() == 3);
}

TEST_CASE("scan_recurses_into_subdirectories", tags) {
    auto lg(make_logger(test_suite));

    temp_dir d;
    d.touch("Rates/portfolio_irs.xml");
    d.touch("Credit/portfolio_cds.xml");
    d.touch("Input/currencyconfig.xml");

    ore_directory_scanner scanner(d.path);
    const auto result = scanner.scan();

    BOOST_LOG_SEV(lg, debug) << "Portfolio files: " << result.portfolio_files.size()
                             << ", Currency files: " << result.currency_files.size();

    REQUIRE(result.portfolio_files.size() == 2);
    REQUIRE(result.currency_files.size() == 1);
}

// =============================================================================
// Exclusion tests
// =============================================================================

TEST_CASE("scan_excludes_files_in_excluded_directory", tags) {
    auto lg(make_logger(test_suite));

    temp_dir d;
    d.touch("Input/portfolio.xml");        // should be excluded
    d.touch("Rates/portfolio_irs.xml");    // should be found

    ore_directory_scanner scanner(d.path, {"Input"});
    const auto result = scanner.scan();

    BOOST_LOG_SEV(lg, debug) << "Portfolio files: " << result.portfolio_files.size()
                             << ", Ignored: " << result.ignored_files.size();

    REQUIRE(result.portfolio_files.size() == 1);
    CHECK(contains(result.portfolio_files, d.path / "Rates" / "portfolio_irs.xml"));
    CHECK(contains(result.ignored_files, d.path / "Input" / "portfolio.xml"));
}

TEST_CASE("scan_excludes_multiple_exclusion_directories", tags) {
    auto lg(make_logger(test_suite));

    temp_dir d;
    d.touch("Input/portfolio.xml");
    d.touch("Output/portfolio.xml");
    d.touch("Rates/portfolio_irs.xml");

    ore_directory_scanner scanner(d.path, {"Input", "Output"});
    const auto result = scanner.scan();

    REQUIRE(result.portfolio_files.size() == 1);
    CHECK(contains(result.portfolio_files, d.path / "Rates" / "portfolio_irs.xml"));
}

TEST_CASE("scan_excluded_currency_file_is_ignored", tags) {
    auto lg(make_logger(test_suite));

    temp_dir d;
    d.touch("Input/currencyconfig.xml");   // excluded dir
    d.touch("currencyconfig.xml");         // root — not excluded

    ore_directory_scanner scanner(d.path, {"Input"});
    const auto result = scanner.scan();

    REQUIRE(result.currency_files.size() == 1);
    CHECK(contains(result.currency_files, d.path / "currencyconfig.xml"));
}

TEST_CASE("scan_nonexistent_directory_returns_empty_result", tags) {
    auto lg(make_logger(test_suite));

    const auto nonexistent =
        std::filesystem::temp_directory_path() / "ores_no_such_dir_xyz";

    ore_directory_scanner scanner(nonexistent);
    const auto result = scanner.scan();

    CHECK(result.currency_files.empty());
    CHECK(result.portfolio_files.empty());
}
