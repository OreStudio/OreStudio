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
#include "ores.http/routes/reporting/result_rollup.hpp"
#include "ores.logging/make_logger.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

using namespace ores::logging;
using namespace ores::http::routes::reporting::rollup;

namespace {

priced_trade trade(std::string id, double npv, std::string book) {
    priced_trade row;
    row.trade_id = std::move(id);
    row.npv = npv;
    row.npv_currency = "USD";
    row.notional = npv * 10;
    row.notional_currency = "USD";
    row.book_id = std::move(book);
    return row;
}

/// A book row with a stated total, so a roll-up can be checked against it.
aggregate_row book_row(std::string key, double total, std::size_t trades) {
    aggregate_row row;
    row.key = std::move(key);
    row.name = row.key;
    row.trades = trades;
    row.totals.push_back(money{"USD", total});
    row.notional.push_back(money{"USD", total * 10});
    return row;
}

double total_of(const aggregate_row& row) {
    return row.totals.empty() ? 0 : row.totals.front().total;
}

}

TEST_CASE("a_run_totals_each_currency_apart", "[result_rollup]") {
    auto lg(make_logger(test_suite));

    std::vector<priced_trade> rows{trade("t1", 100.0, "b1"), trade("t2", 250.0, "b1")};
    rows[1].npv_currency = "EUR";

    const auto totals = npv_totals(rows);
    REQUIRE(totals.size() == 2);
    CHECK(totals[0].currency == "EUR");
    CHECK(totals[0].total == 250.0);
    CHECK(totals[1].currency == "USD");
    CHECK(totals[1].total == 100.0);
}

TEST_CASE("a_book_holds_the_trades_of_that_book", "[result_rollup]") {
    auto lg(make_logger(test_suite));

    const std::vector<priced_trade> rows{trade("t1", 100, "b1"),
                                         trade("t2", 250, "b1"),
                                         trade("t3", 40, "b2")};
    const auto rolled = aggregate_rows(rows,
                                       [](const priced_trade& row) { return row.book_id; },
                                       {{"b1", "Desk one"}, {"b2", "Desk two"}});

    REQUIRE(rolled.size() == 2);
    // The largest book reads first.
    CHECK(rolled[0].key == "b1");
    CHECK(rolled[0].name == "Desk one");
    CHECK(rolled[0].trades == 2);
    CHECK(total_of(rolled[0]) == 350.0);
    CHECK(rolled[1].key == "b2");
    CHECK(rolled[1].trades == 1);
}

TEST_CASE("a_portfolio_sums_the_books_under_it", "[result_rollup]") {
    auto lg(make_logger(test_suite));

    const std::vector<aggregate_row> books{book_row("b1", 100, 1), book_row("b2", 250, 2)};
    const auto rows = with_portfolio_tree(books,
                                          {{"b1", "p1"}, {"b2", "p1"}},
                                          {{"p1", "Rates"}},
                                          {});

    REQUIRE(rows.size() == 3);
    CHECK(rows[0].is_group);
    CHECK(rows[0].name == "Rates");
    CHECK(rows[0].trades == 3);
    CHECK(total_of(rows[0]) == 350.0);
    // The books sit under the portfolio, largest first, each one indented.
    CHECK(rows[1].key == "b2");
    CHECK(rows[1].depth == 1);
    CHECK(rows[1].group == "Rates");
    CHECK(rows[2].key == "b1");
}

TEST_CASE("a_nested_portfolio_holds_the_portfolio_under_it", "[result_rollup]") {
    auto lg(make_logger(test_suite));

    const std::vector<aggregate_row> books{book_row("b1", 100, 1), book_row("b2", 250, 2)};
    const auto rows = with_portfolio_tree(books,
                                          {{"b1", "child"}, {"b2", "parent"}},
                                          {{"child", "Child"}, {"parent", "Parent"}},
                                          {{"child", "parent"}});

    REQUIRE(rows.size() == 4);
    CHECK(rows[0].name == "Parent");
    CHECK(rows[0].trades == 3);
    CHECK(rows[1].key == "b2");
    CHECK(rows[2].name == "Child");
    CHECK(rows[2].depth == 1);
    // The child portfolio's own book sits under the child, not the parent.
    CHECK(rows[3].key == "b1");
    CHECK(rows[3].depth == 2);
}

TEST_CASE("a_book_in_no_portfolio_stands_at_the_top", "[result_rollup]") {
    auto lg(make_logger(test_suite));

    const std::vector<aggregate_row> books{book_row("b1", 100, 1), book_row("b2", 250, 2)};
    const auto rows = with_portfolio_tree(books,
                                          {{"b1", "p1"}},
                                          {{"p1", "Rates"}},
                                          {});

    REQUIRE(rows.size() == 3);
    CHECK(rows[0].name == "Rates");
    CHECK(rows[2].key == "b2");
    CHECK(rows[2].depth == 0);
    CHECK(rows[2].group.empty());
}

TEST_CASE("a_counterparty_group_sums_its_members", "[result_rollup]") {
    auto lg(make_logger(test_suite));

    const std::vector<aggregate_row> counterparties{book_row("c1", 100, 1),
                                                    book_row("c2", 250, 2),
                                                    book_row("c3", 40, 1)};
    const auto rows = with_groups(counterparties,
                                  {{"c1", "group"}, {"c2", "group"}},
                                  {{"c1", "Member one"},
                                   {"c2", "Member two"},
                                   {"group", "The group"},
                                   {"c3", "Alone"}});

    REQUIRE(rows.size() == 4);
    CHECK(rows[0].is_group);
    CHECK(rows[0].name == "The group");
    CHECK(rows[0].trades == 3);
    CHECK(total_of(rows[0]) == 350.0);
    CHECK(rows[1].key == "c2");
    CHECK(rows[1].group == "The group");
    CHECK(rows[1].depth == 1);
    // The counterparty in no group stands on its own.
    CHECK(rows[3].key == "c3");
    CHECK(rows[3].depth == 0);
}

TEST_CASE("a_comparison_states_the_change_and_keeps_a_vanished_row", "[result_rollup]") {
    auto lg(make_logger(test_suite));

    const std::vector<aggregate_row> current{book_row("b1", 150, 2)};
    const std::vector<aggregate_row> earlier{book_row("b1", 100, 1), book_row("b2", 40, 1)};

    const auto rows = compared_with(current, earlier);
    REQUIRE(rows.size() == 2);

    CHECK(rows[0].key == "b1");
    CHECK(total_of(rows[0]) == 150.0);
    CHECK(rows[0].previous.size() == 1);
    CHECK(rows[0].previous[0].total == 100.0);
    CHECK(rows[0].change[0].total == 50.0);
    CHECK(rows[0].change_pct[0].total == 50.0);
    CHECK_FALSE(rows[0].is_removed);

    // A book only the earlier run had is kept, and states what it was.
    CHECK(rows[1].key == "b2");
    CHECK(rows[1].is_removed);
    CHECK(rows[1].trades == 0);
    CHECK(rows[1].totals.empty());
    CHECK(rows[1].previous[0].total == 40.0);
    CHECK(rows[1].change[0].total == -40.0);
}

TEST_CASE("a_share_of_no_figure_is_not_a_figure", "[result_rollup]") {
    auto lg(make_logger(test_suite));

    const std::vector<aggregate_row> current{book_row("b1", 150, 2)};
    const std::vector<aggregate_row> earlier{};

    const auto rows = compared_with(current, earlier);
    REQUIRE(rows.size() == 1);
    CHECK(rows[0].change[0].total == 150.0);
    // The earlier run stated nothing, so the change has no share.
    CHECK_FALSE(rows[0].change_pct[0].stated);
}
