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
#include "ores.trading.core/service/trade_batcher.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <initializer_list>
#include <map>
#include <memory>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

namespace {

const std::string_view test_suite("ores.trading.service.trade_batcher.tests");
const std::string tags("[trading][service][batcher]");

using namespace ores::trading::service;
using namespace ores::logging;

std::chrono::year_month_day the_eighth() {
    using namespace std::chrono;
    return year_month_day{year{2026}, month{10}, day{8}};
}

trade_population population_of(std::initializer_list<std::pair<std::string, std::string>> trades) {
    trade_population population;
    population.report_name = "Headline Position";
    population.as_of = the_eighth();
    for (const auto& [trade_id, book_id] : trades)
        population.trades.push_back({.trade_id = trade_id, .book_id = book_id});
    return population;
}

/** The trades a partition covers, so a case can assert on the whole of it. */
std::vector<std::string> trades_covered(const std::vector<trade_set>& sets) {
    std::vector<std::string> covered;
    for (const auto& set : sets)
        covered.insert(covered.end(), set.trade_ids.begin(), set.trade_ids.end());
    std::ranges::sort(covered);
    return covered;
}

/** A strategy that groups by the first three characters of the book, standing
 * in for the desk, counterparty or grid-behaviour strategies that follow. */
class by_desk final : public trade_batching_strategy {
public:
    std::vector<trade_set> split(const trade_population& population,
                                 const std::vector<trade_set>& sets) const override {
        std::unordered_map<std::string, std::string> book_of;
        for (const auto& trade : population.trades)
            book_of.emplace(trade.trade_id, trade.book_id);

        std::map<std::string, std::vector<std::string>> by_desk;
        for (const auto& set : sets)
            for (const auto& trade_id : set.trade_ids)
                by_desk[book_of.at(trade_id).substr(0, 4)].push_back(trade_id);

        std::vector<trade_set> out;
        for (auto& [desk, trade_ids] : by_desk) {
            trade_set child;
            child.name = "Headline Position:" + desk + ":2026-10-08";
            child.as_of = population.as_of;
            child.trade_ids = std::move(trade_ids);
            out.push_back(std::move(child));
        }
        return out;
    }

    std::string_view name() const override {
        return "by-desk";
    }
};

/** A strategy that returns its input unchanged, for a case that needs a
 * strategy on the seam but no splitting. */
class splits_nothing final : public trade_batching_strategy {
public:
    std::vector<trade_set> split(const trade_population&,
                                 const std::vector<trade_set>& sets) const override {
        return sets;
    }

    std::string_view name() const override {
        return "splits-nothing";
    }
};

/** A strategy that loses trades: it keeps the first and forgets the rest. */
class drops_a_trade final : public trade_batching_strategy {
public:
    std::vector<trade_set> split(const trade_population&,
                                 const std::vector<trade_set>& sets) const override {
        if (sets.empty() || sets.front().trade_ids.empty())
            return sets;
        auto kept = sets.front();
        kept.trade_ids.resize(1);
        return {std::move(kept)};
    }

    std::string_view name() const override {
        return "drops-a-trade";
    }
};

/** A strategy that puts one trade in two sets. */
class duplicates_a_trade final : public trade_batching_strategy {
public:
    std::vector<trade_set> split(const trade_population&,
                                 const std::vector<trade_set>& sets) const override {
        if (sets.empty())
            return sets;
        auto first = sets.front();
        auto second = sets.front();
        first.name = "first";
        second.name = "second";
        first.trade_ids = {sets.front().trade_ids.front()};
        second.trade_ids = first.trade_ids;
        return {std::move(first), std::move(second)};
    }

    std::string_view name() const override {
        return "duplicates-a-trade";
    }
};

/** A strategy that gives two sets the same batch key. */
class reuses_a_key final : public trade_batching_strategy {
public:
    std::vector<trade_set> split(const trade_population&,
                                 const std::vector<trade_set>& sets) const override {
        if (sets.empty())
            return sets;
        auto whole = sets.front();
        auto also = whole;
        return {std::move(whole), std::move(also)};
    }

    std::string_view name() const override {
        return "reuses-a-key";
    }
};

}

TEST_CASE("batch_by_book gives one set per book named for the report and the date", tags) {
    auto lg(make_logger(test_suite));

    const auto population = population_of({
        {"trade-1", "BOOK-A"},
        {"trade-2", "BOOK-B"},
        {"trade-3", "BOOK-A"},
    });

    const auto sets = trade_batcher(std::make_shared<batch_by_book>()).partition(population);

    REQUIRE(sets.size() == 2);

    CHECK(sets[0].name == "Headline Position:BOOK-A:2026-10-08");
    CHECK(sets[0].trade_ids == std::vector<std::string>{"trade-1", "trade-3"});
    CHECK(sets[1].name == "Headline Position:BOOK-B:2026-10-08");
    CHECK(sets[1].trade_ids == std::vector<std::string>{"trade-2"});

    CHECK(sets[0].as_of == the_eighth());
    CHECK(sets[1].as_of == the_eighth());
}

TEST_CASE("batch_by_book covers the population exactly once", tags) {
    auto lg(make_logger(test_suite));

    const auto population = population_of({
        {"trade-1", "BOOK-A"},
        {"trade-2", "BOOK-B"},
        {"trade-3", "BOOK-A"},
        {"trade-4", "BOOK-C"},
    });

    const auto sets = trade_batcher(std::make_shared<batch_by_book>()).partition(population);

    CHECK(trades_covered(sets) ==
          std::vector<std::string>{"trade-1", "trade-2", "trade-3", "trade-4"});
}

TEST_CASE("a book whose trades exceed the limit splits into bounded sets", tags) {
    auto lg(make_logger(test_suite));

    const auto population = population_of({
        {"trade-1", "BOOK-A"},
        {"trade-2", "BOOK-A"},
        {"trade-3", "BOOK-A"},
        {"trade-4", "BOOK-B"},
    });

    auto strategy = std::make_shared<composite_strategy>(
        std::vector<std::shared_ptr<const trade_batching_strategy>>{
            std::make_shared<batch_by_book>(), std::make_shared<at_most_trades>(2)});

    const auto sets = trade_batcher(std::move(strategy)).partition(population);

    REQUIRE(sets.size() == 3);

    // The strategies run in order, so BOOK-A is split where it stands and
    // BOOK-B follows it untouched, rather than the bounded sets gathering at
    // one end.
    CHECK(sets[0].name == "Headline Position:BOOK-A:2026-10-08#1");
    CHECK(sets[0].trade_ids == std::vector<std::string>{"trade-1", "trade-2"});
    CHECK(sets[1].name == "Headline Position:BOOK-A:2026-10-08#2");
    CHECK(sets[1].trade_ids == std::vector<std::string>{"trade-3"});

    // The book under the limit keeps its own key.
    CHECK(sets[2].name == "Headline Position:BOOK-B:2026-10-08");
    CHECK(sets[2].trade_ids == std::vector<std::string>{"trade-4"});

    CHECK(trades_covered(sets) ==
          std::vector<std::string>{"trade-1", "trade-2", "trade-3", "trade-4"});
}

TEST_CASE("a composed strategy says which strategies it ran", tags) {
    auto lg(make_logger(test_suite));

    const auto strategy =
        composite_strategy(std::vector<std::shared_ptr<const trade_batching_strategy>>{
            std::make_shared<batch_by_book>(), std::make_shared<at_most_trades>(1000)});

    CHECK(strategy.name() == "batch-by-book,at-most-trades");
}

TEST_CASE("a strategy that partitions on another attribute needs no other change", tags) {
    auto lg(make_logger(test_suite));

    const auto population = population_of({
        {"trade-1", "RATE-1"},
        {"trade-2", "CRED-1"},
        {"trade-3", "RATE-2"},
    });

    const auto sets = trade_batcher(std::make_shared<by_desk>()).partition(population);

    REQUIRE(sets.size() == 2);
    CHECK(sets[0].name == "Headline Position:CRED:2026-10-08");
    CHECK(sets[0].trade_ids == std::vector<std::string>{"trade-2"});
    CHECK(sets[1].name == "Headline Position:RATE:2026-10-08");
    CHECK(sets[1].trade_ids == std::vector<std::string>{"trade-1", "trade-3"});
}

TEST_CASE("a set within the limit keeps its own batch key", tags) {
    auto lg(make_logger(test_suite));

    const auto population = population_of({{"trade-1", "BOOK-A"}});

    auto strategy = std::make_shared<composite_strategy>(
        std::vector<std::shared_ptr<const trade_batching_strategy>>{
            std::make_shared<splits_nothing>(), std::make_shared<at_most_trades>(2)});

    const auto sets = trade_batcher(std::move(strategy)).partition(population);

    REQUIRE(sets.size() == 1);
    CHECK(sets[0].name == "Headline Position");
}

TEST_CASE("a population that leaves a trade out of every set is refused", tags) {
    auto lg(make_logger(test_suite));

    const auto population = population_of({
        {"trade-1", "BOOK-A"},
        {"trade-2", "BOOK-B"},
    });

    CHECK_THROWS(trade_batcher(std::make_shared<drops_a_trade>()).partition(population));
}

TEST_CASE("a population that puts a trade in two sets is refused", tags) {
    auto lg(make_logger(test_suite));

    const auto population = population_of({
        {"trade-1", "BOOK-A"},
        {"trade-2", "BOOK-B"},
    });

    CHECK_THROWS(trade_batcher(std::make_shared<duplicates_a_trade>()).partition(population));
}

TEST_CASE("two sets under one batch key are refused", tags) {
    auto lg(make_logger(test_suite));

    const auto population = population_of({
        {"trade-1", "BOOK-A"},
        {"trade-2", "BOOK-B"},
    });

    CHECK_THROWS(trade_batcher(std::make_shared<reuses_a_key>()).partition(population));
}

TEST_CASE("a batch holding more than zero trades is required", tags) {
    auto lg(make_logger(test_suite));

    CHECK_THROWS(at_most_trades(0));
    CHECK_NOTHROW(at_most_trades(1));
}

TEST_CASE("a batcher without a strategy is refused", tags) {
    auto lg(make_logger(test_suite));

    CHECK_THROWS(trade_batcher(nullptr));
}
