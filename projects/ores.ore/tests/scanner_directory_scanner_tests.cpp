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

// Minimal XML snippets that the scanner recognises by their root element.
const std::string_view portfolio_xml  = "<Portfolio>\n</Portfolio>\n";
const std::string_view currency_xml   = "<CurrencyConfig>\n</CurrencyConfig>\n";
const std::string_view unrelated_xml  = "<Simulation>\n</Simulation>\n";

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

    // Create an empty file (for non-XML or excluded files under test).
    void touch(const std::filesystem::path& rel) const {
        const auto full = path / rel;
        std::filesystem::create_directories(full.parent_path());
        std::ofstream{full};
    }

    // Create a file with explicit content.
    void write(const std::filesystem::path& rel,
               std::string_view content) const {
        const auto full = path / rel;
        std::filesystem::create_directories(full.parent_path());
        std::ofstream{full} << content;
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

TEST_CASE("scan_classifies_currency_config_by_root_element", tags) {
    auto lg(make_logger(test_suite));

    temp_dir d;
    // Any filename works — classification is by root element content.
    d.write("currencies.xml",    currency_xml);
    d.write("my_currency.xml",   currency_xml);

    ore_directory_scanner scanner(d.path);
    const auto result = scanner.scan();

    BOOST_LOG_SEV(lg, debug) << "Currency files: " << result.currency_files.size();

    REQUIRE(result.currency_files.size() == 2);
    CHECK(result.portfolio_files.empty());
    CHECK(contains(result.currency_files, d.path / "currencies.xml"));
    CHECK(contains(result.currency_files, d.path / "my_currency.xml"));
}

TEST_CASE("scan_classifies_portfolio_xml_by_root_element", tags) {
    auto lg(make_logger(test_suite));

    temp_dir d;
    // Arbitrary filenames — all recognised by <Portfolio> root element.
    d.write("portfolio.xml",    portfolio_xml);
    d.write("fxoption.xml",     portfolio_xml);
    d.write("swap_usd.xml",     portfolio_xml);

    ore_directory_scanner scanner(d.path);
    const auto result = scanner.scan();

    BOOST_LOG_SEV(lg, debug) << "Portfolio files: " << result.portfolio_files.size();

    REQUIRE(result.portfolio_files.size() == 3);
    CHECK(result.currency_files.empty());
    CHECK(contains(result.portfolio_files, d.path / "portfolio.xml"));
    CHECK(contains(result.portfolio_files, d.path / "fxoption.xml"));
    CHECK(contains(result.portfolio_files, d.path / "swap_usd.xml"));
}

TEST_CASE("scan_ignores_unrecognised_xml_files", tags) {
    auto lg(make_logger(test_suite));

    temp_dir d;
    d.write("ore.xml",    unrelated_xml);
    d.write("config.xml", unrelated_xml);
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
    d.write("Rates/irs.xml",       portfolio_xml);
    d.write("Credit/cds.xml",      portfolio_xml);
    d.write("Input/currencies.xml", currency_xml);

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
    d.touch("Input/trades.xml");           // excluded — content never read
    d.write("Rates/irs.xml", portfolio_xml); // should be found

    ore_directory_scanner scanner(d.path, {"Input"});
    const auto result = scanner.scan();

    BOOST_LOG_SEV(lg, debug) << "Portfolio files: " << result.portfolio_files.size()
                             << ", Ignored: " << result.ignored_files.size();

    REQUIRE(result.portfolio_files.size() == 1);
    CHECK(contains(result.portfolio_files, d.path / "Rates" / "irs.xml"));
    CHECK(contains(result.ignored_files, d.path / "Input" / "trades.xml"));
}

TEST_CASE("scan_excludes_multiple_exclusion_directories", tags) {
    auto lg(make_logger(test_suite));

    temp_dir d;
    d.touch("Input/trades.xml");
    d.touch("Output/results.xml");
    d.write("Rates/irs.xml", portfolio_xml);

    ore_directory_scanner scanner(d.path, {"Input", "Output"});
    const auto result = scanner.scan();

    REQUIRE(result.portfolio_files.size() == 1);
    CHECK(contains(result.portfolio_files, d.path / "Rates" / "irs.xml"));
}

TEST_CASE("scan_excluded_currency_file_is_ignored", tags) {
    auto lg(make_logger(test_suite));

    temp_dir d;
    d.touch("Input/currencies.xml");         // excluded dir — content never read
    d.write("currencies.xml", currency_xml); // root level — not excluded

    ore_directory_scanner scanner(d.path, {"Input"});
    const auto result = scanner.scan();

    REQUIRE(result.currency_files.size() == 1);
    CHECK(contains(result.currency_files, d.path / "currencies.xml"));
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
