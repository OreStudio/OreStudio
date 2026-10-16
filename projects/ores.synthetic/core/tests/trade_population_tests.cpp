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
#include "ores.synthetic.core/service/trade_population_planner.hpp"
#include "ores.synthetic.core/service/trade_population_service.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <map>
#include <set>
#include <string>

// The generator's whole point is that a second provision states the same
// population: no trade duplicated and no version advanced. These cases drive
// the run against a writer that records what it was asked to do, so the
// convergence is asserted on the generator's own answer rather than on a
// database's, and a real provisioning is the same assertion against real rows.

namespace {

const std::string tags("[trade_population]");

using namespace ores::synthetic;
using namespace ores::synthetic::service;

const boost::uuids::uuid config_id =
    boost::uuids::string_generator()("0cd68b09-6ebc-4180-a298-0cc386590f31");
const boost::uuids::uuid party_id =
    boost::uuids::string_generator()("11111111-1111-1111-1111-111111111111");
const boost::uuids::uuid bond_book =
    boost::uuids::string_generator()("22222222-2222-2222-2222-222222222222");
const boost::uuids::uuid second_book =
    boost::uuids::string_generator()("33333333-3333-3333-3333-333333333333");
const boost::uuids::uuid bank =
    boost::uuids::string_generator()("44444444-4444-4444-4444-444444444444");
const boost::uuids::uuid second_bank =
    boost::uuids::string_generator()("55555555-5555-5555-5555-555555555555");

domain::trade_generation_scope bond_scope() {
    domain::trade_generation_scope scope;
    scope.config_id = config_id;
    scope.party_id = party_id;
    scope.asset_class_code = "bond";
    scope.min_tenor_code = "1Y";
    scope.max_tenor_code = "10Y";
    scope.min_notional = 100000.0;
    scope.max_notional = 10000000.0;
    scope.as_of_date = std::chrono::year{2026} / 1 / 2;
    scope.seed = 7;
    scope.book_ids = {bond_book, second_book};
    scope.counterparty_ids = {bank, second_bank};
    scope.trader_usernames = {"trader.one"};
    scope.currency_code = "EUR";
    return scope;
}

/**
 * @brief A writer that holds the trades a run booked, in a map.
 *
 * Writing a trade twice is a failure the test can see: the real store would
 * either refuse the second booking or bump the version, and a map that
 * overwrote would hide both. So the stub records the versions a run touched
 * and every write after the first is a version.
 */
struct recording_writer final : trade_population_writer {
    domain::trade_generation_scope scope;
    std::map<boost::uuids::uuid, std::uint32_t> versions;
    std::size_t bookings = 0;
    std::size_t instruments = 0;

    explicit recording_writer(domain::trade_generation_scope s)
        : scope(std::move(s)) {}

    domain::trade_generation_scope resolve_scope(const boost::uuids::uuid&) override {
        return scope;
    }

    std::optional<trade_population_stored> read_trade(const boost::uuids::uuid& id) override {
        const auto found = versions.find(id);
        if (found == versions.end())
            return std::nullopt;
        return trade_population_stored{.version = found->second, .external_version = 0};
    }

    void write_trade(const trade_population_write& write,
                     const domain::trade_generation_scope&) override {
        ++bookings;
        versions[write.slot.trade_id] = 1;
    }

    void write_instrument(const trade_population_write&,
                          const domain::trade_generation_scope&) override {
        ++instruments;
    }
};

/**
 * @brief The version a slot stands at, or nothing when the run never wrote it.
 */
std::optional<std::uint32_t> version_of(const recording_writer& writer,
                                        const boost::uuids::uuid& trade_id) {
    const auto found = writer.versions.find(trade_id);
    if (found == writer.versions.end())
        return std::nullopt;
    return found->second;
}

}

TEST_CASE("a_bond_scope_states_one_trade_per_slot_across_its_books", tags) {
    const auto plan = plan_population(bond_scope());

    REQUIRE(plan.slots.size() == trades_per_book * 2);
    REQUIRE(plan.slots.front().trade_type_code == "Bond");
    REQUIRE(plan.slots.front().book_id == bond_book);
    REQUIRE(plan.slots[1].book_id == second_book);
    REQUIRE(plan.slots.front().currency == "EUR");
}

TEST_CASE("the_same_scope_states_the_same_population_twice", tags) {
    const auto once = plan_population(bond_scope());
    const auto twice = plan_population(bond_scope());

    REQUIRE(once.slots.size() == twice.slots.size());
    for (std::size_t i = 0; i < once.slots.size(); ++i) {
        REQUIRE(once.slots[i].trade_id == twice.slots[i].trade_id);
        REQUIRE(once.slots[i].internal_identifier == twice.slots[i].internal_identifier);
        REQUIRE(once.slots[i].security_id == twice.slots[i].security_id);
        REQUIRE(once.slots[i].notional == twice.slots[i].notional);
        REQUIRE(once.slots[i].trade_date == twice.slots[i].trade_date);
        REQUIRE(once.slots[i].maturity_date == twice.slots[i].maturity_date);
    }
}

TEST_CASE("a_different_seed_states_a_different_population", tags) {
    auto other = bond_scope();
    other.seed = 8;

    const auto once = plan_population(bond_scope());
    const auto twice = plan_population(other);

    REQUIRE(once.slots.front().trade_id != twice.slots.front().trade_id);
}

TEST_CASE("every_trade_date_derives_from_the_as_of_anchor", tags) {
    auto earlier = bond_scope();
    earlier.as_of_date = std::chrono::year{2026} / 1 / 2;
    auto later = bond_scope();
    later.as_of_date = std::chrono::year{2027} / 1 / 2;

    const auto first = plan_population(earlier);
    const auto second = plan_population(later);

    REQUIRE(first.slots.front().trade_date < second.slots.front().trade_date);

    const auto expected_maturity = [](const domain::trade_population_slot& slot) {
        return slot.trade_date + std::chrono::years{static_cast<int>(slot.tenor_years)};
    };
    REQUIRE(first.slots.front().maturity_date == expected_maturity(first.slots.front()));
    REQUIRE(second.slots.front().maturity_date == expected_maturity(second.slots.front()));
}

TEST_CASE("a_tenor_or_notional_band_that_does_not_order_is_refused", tags) {
    auto inverted_tenor = bond_scope();
    inverted_tenor.min_tenor_code = "10Y";
    inverted_tenor.max_tenor_code = "1Y";
    CHECK_THROWS_AS(plan_population(inverted_tenor), trade_population_error);

    auto inverted_notional = bond_scope();
    inverted_notional.min_notional = 10000000.0;
    inverted_notional.max_notional = 100000.0;
    CHECK_THROWS_AS(plan_population(inverted_notional), trade_population_error);
}

TEST_CASE("a_scope_naming_no_book_counterparty_or_trader_is_refused", tags) {
    auto no_book = bond_scope();
    no_book.book_ids.clear();
    CHECK_THROWS_AS(plan_population(no_book), trade_population_error);

    auto no_counterparty = bond_scope();
    no_counterparty.counterparty_ids.clear();
    CHECK_THROWS_AS(plan_population(no_counterparty), trade_population_error);

    auto no_trader = bond_scope();
    no_trader.trader_usernames.clear();
    CHECK_THROWS_AS(plan_population(no_trader), trade_population_error);
}

TEST_CASE("a_family_the_generator_does_not_cover_is_refused_by_name", tags) {
    auto rates = bond_scope();
    rates.asset_class_code = "interest_rates";

    CHECK_THROWS_WITH(plan_population(rates), Catch::Matchers::ContainsSubstring("interest_rates"));
}

TEST_CASE("the_notional_of_every_slot_sits_in_the_band", tags) {
    const auto scope = bond_scope();
    const auto plan = plan_population(scope);

    for (const auto& slot : plan.slots) {
        REQUIRE(slot.notional >= scope.min_notional);
        REQUIRE(slot.notional <= scope.max_notional);
        REQUIRE(slot.tenor_years >= 1);
        REQUIRE(slot.tenor_years <= 10);
    }
}

TEST_CASE("one_security_is_shared_by_every_trade_of_a_book", tags) {
    const auto plan = plan_population(bond_scope());

    std::set<std::string> securities;
    for (const auto& slot : plan.slots)
        securities.insert(slot.security_id);

    REQUIRE(securities.size() == 2);
}

TEST_CASE("two_runs_over_one_scope_write_nothing_the_second_time", tags) {
    recording_writer writer(bond_scope());
    trade_population_service service(writer);

    const auto first = service.run(config_id);
    REQUIRE(first.planned == trades_per_book * 2);
    REQUIRE(first.written == trades_per_book * 2);
    REQUIRE(first.already_present == 0);
    REQUIRE(writer.bookings == trades_per_book * 2);
    REQUIRE(writer.instruments == trades_per_book * 2);

    const auto versions_after_first = writer.versions;

    const auto second = service.run(config_id);
    REQUIRE(second.planned == trades_per_book * 2);
    REQUIRE(second.written == 0);
    REQUIRE(second.already_present == trades_per_book * 2);
    REQUIRE(writer.bookings == trades_per_book * 2);

    /*
     * The convergence acceptance, stated as the two properties it names: no
     * trade duplicated, and no trade's version advanced.
     */
    REQUIRE(second.outcomes.size() == first.outcomes.size());
    for (std::size_t i = 0; i < second.outcomes.size(); ++i) {
        REQUIRE(second.outcomes[i].trade_id == first.outcomes[i].trade_id);
        REQUIRE(second.outcomes[i].internal_identifier == first.outcomes[i].internal_identifier);
        REQUIRE(version_of(writer, second.outcomes[i].trade_id) ==
                version_of(writer, first.outcomes[i].trade_id));
        REQUIRE(version_of(writer, second.outcomes[i].trade_id) ==
                versions_after_first.at(first.outcomes[i].trade_id));
    }
    REQUIRE(writer.versions == versions_after_first);
}
