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
#include "ores.ore/planner/ore_import_planner.hpp"

#include <set>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.testing/project_root.hpp"

namespace {

const std::string_view test_suite("ores.ore.tests");
const std::string tags("[ore][planner][import_planner]");

std::filesystem::path ore_path(const std::string& relative) {
    return ores::testing::project_root::resolve("external/ore/" + relative);
}

// Build a minimal scan_result pointing at a legacy ORE example directory
ores::ore::scanner::scan_result make_scan_result(
    const std::filesystem::path& root) {
    ores::ore::scanner::scan_result r;
    r.root = root;

    // Look for currencyconfig.xml / currencies.xml
    const auto currency_xml = root / "currencies.xml";
    const auto currency_cfg = root / "currencyconfig.xml";
    if (std::filesystem::exists(currency_cfg))
        r.currency_files.push_back(currency_cfg);
    else if (std::filesystem::exists(currency_xml))
        r.currency_files.push_back(currency_xml);

    // Collect portfolio XML files
    for (const auto& entry :
             std::filesystem::recursive_directory_iterator(root)) {
        if (!entry.is_regular_file()) continue;
        if (entry.path().extension() != ".xml") continue;
        const auto fname = entry.path().filename().string();
        if (fname.starts_with("portfolio"))
            r.portfolio_files.push_back(entry.path());
    }
    return r;
}

ores::ore::planner::import_choices default_choices(
    const std::filesystem::path& /*root*/) {
    ores::ore::planner::import_choices c;
    c.parent_portfolio_name = "Example1";
    c.create_parent_portfolio = true;
    c.exclusions = {"Input"};
    return c;
}

}

using ores::ore::planner::ore_import_planner;
using ores::ore::planner::currency_import_mode;
using namespace ores::logging;

// =============================================================================
// Currency tests
// =============================================================================

TEST_CASE("plan_includes_all_currencies_when_mode_is_all", tags) {
    auto lg(make_logger(test_suite));

    const auto root = ore_path("examples/Legacy/Example_1/Input");
    const auto sr = make_scan_result(root);

    if (sr.currency_files.empty()) {
        SUCCEED("No currency file in Example_1/Input — skipping");
        return;
    }

    auto choices = default_choices(root);
    choices.currency_mode = currency_import_mode::all;

    ore_import_planner planner(sr, {}, choices);
    const auto plan = planner.plan();

    BOOST_LOG_SEV(lg, info) << "Currencies in plan: " << plan.currencies.size();
    CHECK(!plan.currencies.empty());
}

TEST_CASE("plan_skips_existing_iso_codes_when_mode_is_missing_only", tags) {
    auto lg(make_logger(test_suite));

    const auto root = ore_path("examples/Legacy/Example_1/Input");
    const auto sr = make_scan_result(root);

    if (sr.currency_files.empty()) {
        SUCCEED("No currency file in Example_1/Input — skipping");
        return;
    }

    auto choices = default_choices(root);
    choices.currency_mode = currency_import_mode::missing_only;

    // First run — no existing codes
    ore_import_planner planner_all(sr, {}, choices);
    const auto plan_all = planner_all.plan();
    const std::size_t total = plan_all.currencies.size();
    BOOST_LOG_SEV(lg, info) << "All currencies: " << total;

    // Collect all ISO codes from the plan
    std::set<std::string> all_codes;
    for (const auto& c : plan_all.currencies)
        all_codes.insert(c.iso_code);

    // Second run — mark all as existing → plan should have zero currencies
    ore_import_planner planner_none(sr, all_codes, choices);
    const auto plan_none = planner_none.plan();

    BOOST_LOG_SEV(lg, info) << "Currencies after filtering: "
                            << plan_none.currencies.size();
    CHECK(plan_none.currencies.empty());

    // Third run — mark only half as existing
    if (total >= 2) {
        std::set<std::string> half_codes;
        std::size_t idx = 0;
        for (const auto& c : plan_all.currencies) {
            if (idx++ % 2 == 0)
                half_codes.insert(c.iso_code);
        }
        ore_import_planner planner_half(sr, half_codes, choices);
        const auto plan_half = planner_half.plan();
        BOOST_LOG_SEV(lg, info) << "Currencies with half filtered: "
                                << plan_half.currencies.size();
        CHECK(plan_half.currencies.size() < total);
        CHECK(plan_half.currencies.size() > 0);
    }
}

// =============================================================================
// Portfolio / book tests
// =============================================================================

TEST_CASE("plan_creates_portfolios_and_books_from_portfolio_files", tags) {
    auto lg(make_logger(test_suite));

    const auto root = ore_path("examples/Legacy/Example_1");
    auto sr = make_scan_result(root / "Input");
    sr.root = root;

    // Add Input/ portfolio files relative to root
    sr.portfolio_files.clear();
    for (const auto& entry :
             std::filesystem::recursive_directory_iterator(root)) {
        if (!entry.is_regular_file()) continue;
        if (entry.path().extension() != ".xml") continue;
        if (entry.path().filename().string().starts_with("portfolio"))
            sr.portfolio_files.push_back(entry.path());
    }

    if (sr.portfolio_files.empty()) {
        SUCCEED("No portfolio files in Example_1 — skipping");
        return;
    }

    auto choices = default_choices(root);
    choices.create_parent_portfolio = true;
    choices.parent_portfolio_name = "Example1";

    ore_import_planner planner(sr, {}, choices);
    const auto plan = planner.plan();

    BOOST_LOG_SEV(lg, info) << "Portfolios: " << plan.portfolios.size()
                            << ", Books: " << plan.books.size()
                            << ", Trades: " << plan.trades.size();

    // With create_parent_portfolio, we always have at least one portfolio
    CHECK(!plan.portfolios.empty());
    // We must have at least one book per portfolio file
    CHECK(plan.books.size() >= sr.portfolio_files.size());
}

TEST_CASE("plan_with_parent_portfolio_has_one_extra_portfolio", tags) {
    auto lg(make_logger(test_suite));

    const auto root = ore_path("examples/Legacy/Example_1");
    auto sr = make_scan_result(root);
    sr.root = root;

    if (sr.portfolio_files.empty()) {
        SUCCEED("No portfolio files — skipping");
        return;
    }

    // Run once without parent portfolio
    auto choices_no_parent = default_choices(root);
    choices_no_parent.create_parent_portfolio = false;
    ore_import_planner p1(sr, {}, choices_no_parent);
    const auto plan_no_parent = p1.plan();

    // Run with parent portfolio
    auto choices_with_parent = default_choices(root);
    choices_with_parent.create_parent_portfolio = true;
    choices_with_parent.parent_portfolio_name = "Root";
    ore_import_planner p2(sr, {}, choices_with_parent);
    const auto plan_with_parent = p2.plan();

    BOOST_LOG_SEV(lg, info) << "Without parent: " << plan_no_parent.portfolios.size()
                            << " portfolios. With parent: "
                            << plan_with_parent.portfolios.size() << " portfolios.";

    CHECK(plan_with_parent.portfolios.size() ==
          plan_no_parent.portfolios.size() + 1);
}

// =============================================================================
// Trade tests
// =============================================================================

TEST_CASE("plan_trades_have_book_id_and_portfolio_id_stamped", tags) {
    auto lg(make_logger(test_suite));

    const auto root = ore_path("examples/Legacy/Example_1");
    auto sr = make_scan_result(root);
    sr.root = root;

    if (sr.portfolio_files.empty()) {
        SUCCEED("No portfolio files — skipping");
        return;
    }

    auto choices = default_choices(root);
    ore_import_planner planner(sr, {}, choices);
    const auto plan = planner.plan();

    if (plan.trades.empty()) {
        SUCCEED("No trades in plan — skipping");
        return;
    }

    // All trades must have non-nil book_id and portfolio_id
    boost::uuids::uuid nil;
    for (const auto& item : plan.trades) {
        INFO("Trade: " << item.trade.external_id);
        CHECK(item.trade.book_id != nil);
        CHECK(item.trade.portfolio_id != nil);
    }

    BOOST_LOG_SEV(lg, info) << "All " << plan.trades.size()
                            << " trades have valid book/portfolio IDs";
}

TEST_CASE("plan_trade_defaults_override_parsed_values", tags) {
    auto lg(make_logger(test_suite));

    const auto root = ore_path("examples/Legacy/Example_1");
    auto sr = make_scan_result(root);
    sr.root = root;

    if (sr.portfolio_files.empty()) {
        SUCCEED("No portfolio files — skipping");
        return;
    }

    auto choices = default_choices(root);
    choices.defaults.trade_date = "2026-01-01";
    choices.defaults.lifecycle_event = "Amendment";

    ore_import_planner planner(sr, {}, choices);
    const auto plan = planner.plan();

    if (plan.trades.empty()) {
        SUCCEED("No trades — skipping");
        return;
    }

    for (const auto& item : plan.trades) {
        INFO("Trade: " << item.trade.external_id);
        CHECK(item.trade.trade_date == "2026-01-01");
        CHECK(item.trade.lifecycle_event == "Amendment");
    }
}
