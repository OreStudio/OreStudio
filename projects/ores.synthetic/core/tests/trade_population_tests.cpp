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
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <chrono>
#include <format>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <variant>
#include <vector>

// The generator's whole point is that a second provision states the same
// population: no trade duplicated and no version advanced. These cases drive
// the run against a writer that records what it was asked to do, so the
// convergence is asserted on the generator's own answer rather than on a
// database's, and a real provisioning is the same assertion against real rows.

namespace {

using namespace ores::synthetic;
using namespace ores::synthetic::service;

/*
 * Plans a scope through the registry, which is the one way a population is
 * stated. The cases below are about what a planner decides, so they go through
 * the same seam the service does rather than calling one planner directly.
 */
domain::trade_population_plan plan_of(const domain::trade_generation_scope& scope) {
    return planner_for(scope.asset_class_code).plan(scope);
}

/*
 * The bond facts a slot carries. The slot states the keys every family shares
 * and its own family's terms, so a case that reads a bond fact says so.
 */

/*
 * What a bond slot states, read from its rows. A bond states a security before
 * it states a trade, so the case reads the issue and its schedule rather than a
 * bag: the term a security has is the distance its schedule spans.
 */
struct bond_view final {
    std::string security_id;
    boost::uuids::uuid issue_id;
    boost::uuids::uuid issuer_id;
    std::chrono::year_month_day issue_date;
    std::chrono::year_month_day maturity_date;
    std::uint32_t tenor_years = 0;
};

bond_view bond_of(const domain::trade_population_slot& slot) {
    const auto& issue = *slot.bond_issue;
    const auto& schedule = slot.bond_issue_leg_schedules.front();
    bond_view view;
    view.security_id = issue.security_id;
    view.issue_id = issue.issue_id;
    view.issuer_id = boost::uuids::string_generator()(issue.issuer);
    view.issue_date = *issue.issue_date;
    view.maturity_date = *schedule.end_date;
    const auto days =
        std::chrono::sys_days{view.maturity_date} - std::chrono::sys_days{view.issue_date};
    view.tenor_years = static_cast<std::uint32_t>(
        std::chrono::duration_cast<std::chrono::days>(days).count() / 365);
    return view;
}

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
const boost::uuids::uuid trader_account =
    boost::uuids::string_generator()("66666666-6666-6666-6666-666666666666");

/*
 * How many trades each book of the scene receives. The case states it rather
 * than leaning on the struct's default, so a change to that default cannot
 * quietly change what these cases assert.
 */
constexpr std::uint32_t scene_trades_per_book = 8;

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
    scope.trades_per_book = scene_trades_per_book;
    /*
     * A convertible bond states the share it may convert into, so every bond
     * scope needs the equity catalogue the writer loads for it.
     */
    scope.equity_underlyings = {domain::equity_underlying_reference{
        .code = "GB0005405286", .kind = "share", .currency = "GBP"}};
    scope.book_ids = {bond_book, second_book};
    scope.counterparty_ids = {bank, second_bank};
    scope.issuer_ids = {bank, second_bank};
    scope.traders = {domain::trade_population_trader{trader_account, "trader.one"}};
    scope.currency_code = "EUR";
    return scope;
}

/*
 * A scope of the rate family. The seven trade types the router sends to
 * rate_instrument are nine here, and the catalogue is the tenant's own, so the
 * scope states three indices of one currency and one of another.
 */
domain::trade_generation_scope rate_scope() {
    auto scope = bond_scope();
    scope.asset_class_code = "interest_rates";
    scope.floating_index_codes = {"EUR-EURIBOR-6M", "EUR-EONIA", "EUR-ESTR", "USD-SOFR"};
    return scope;
}

/*
 * A scope of the FX family. The ten routed types are sixteen here, and the
 * catalogue carries one pair of the book's currency and one that names neither
 * side of it, so the preference is testable.
 */
domain::trade_generation_scope fx_scope() {
    auto scope = bond_scope();
    scope.asset_class_code = "fx";
    scope.currency_code = "EUR";
    scope.currency_pairs = {{.base = "EUR", .quote = "USD"}, {.base = "GBP", .quote = "USD"}};
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

    void write_trade(const domain::trade_population_slot& slot,
                     const domain::trade_generation_scope&) override {
        ++bookings;
        versions[slot.trade_id] = 1;
    }

    bool structure_exists(const boost::uuids::uuid& structure_id) override {
        return structures.contains(structure_id);
    }

    void write_structure(const domain::trade_population_structure& structure,
                         const domain::trade_generation_scope&) override {
        structures[structure.structure_id];
    }

    void write_structure_member(const boost::uuids::uuid& structure_id,
                                const domain::trade_population_slot& slot) override {
        structures[structure_id].push_back(slot.trade_id);
    }

    /// The structures a run grouped, with the trades it added to each.
    std::map<boost::uuids::uuid, std::vector<boost::uuids::uuid>> structures;
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

TEST_CASE("the_population_size_is_the_configs_and_not_the_generators", "[trade_population]") {
    auto narrower = bond_scope();
    narrower.trades_per_book = 2;

    const auto plan = plan_of(narrower);

    REQUIRE(plan.slots.size() == 2u * narrower.book_ids.size());
    REQUIRE(plan.slots.size() < plan_of(bond_scope()).slots.size());
}

/*
 * The scene's commodity catalogue. It holds two commodities because a spread
 * option names a second one, and both carry the unit their catalogue row
 * states.
 */
domain::trade_generation_scope commodity_scope() {
    auto scope = bond_scope();
    scope.asset_class_code = "commodity";
    scope.commodities = {
        domain::commodity_reference{.code = "NYMEX:CL", .unit = "bbl", .quotation_currency = "USD"},
        domain::commodity_reference{
            .code = "NYMEX:NG", .unit = "MMBtu", .quotation_currency = "USD"}};
    return scope;
}

/*
 * The scene's equity catalogue. It holds four underlyings because a worst-of
 * basket names two, and the kinds differ so a basket drawn across them states
 * more than one vocabulary.
 */
domain::trade_generation_scope equity_scope() {
    auto scope = bond_scope();
    scope.asset_class_code = "equity";
    scope.equity_underlyings = {
        domain::equity_underlying_reference{.code = ".SPX", .kind = "index", .currency = "USD"},
        domain::equity_underlying_reference{.code = "MSFT.O", .kind = "share", .currency = "USD"},
        domain::equity_underlying_reference{
            .code = ".STOXX50E", .kind = "index", .currency = "EUR"},
        domain::equity_underlying_reference{
            .code = "GB0005405286", .kind = "share", .currency = "GBP"}};
    return scope;
}

domain::trade_generation_scope credit_scope() {
    auto scope = bond_scope();
    scope.asset_class_code = "credit";
    scope.trades_per_book = 4;
    scope.credit_reference_entities = {"529900T8BM49AURSDO55", "5493001KJTIIGC8Y1R12"};
    return scope;
}

TEST_CASE("a_credit_slot_states_its_row_and_names_a_legal_entity", "[trade_population]") {
    const auto plan = plan_of(credit_scope());

    REQUIRE(!plan.slots.empty());
    for (const auto& slot : plan.slots) {
        REQUIRE(slot.instruments.credit_instruments.size() == 1u);
        const auto& row = slot.instruments.credit_instruments.front();
        // The row states the party boundary and no trade type: the trade states
        // that one.
        CHECK_FALSE(row.reference_entity_id_type.empty());
        /*
         * A credit trade names a party by one identifier, and the reference is
         * the LEI the dataset holds.
         */
        CHECK(row.reference_entity_id_type == "LEI");
        CHECK(row.reference_entity_id.size() == 20u);
    }
}

/*
 * The codes a plan states. A planner cycles its routed list by the slot's
 * position, so a plan shorter than the list states a prefix of it and the codes
 * past that position are never drawn. A config that wants every code states one
 * slot per code, which is what the seeded profiles do.
 */
std::set<std::string> codes_of(const domain::trade_population_plan& plan) {
    std::set<std::string> codes;
    for (const auto& slot : plan.slots)
        codes.insert(slot.trade_type_code);
    return codes;
}

TEST_CASE("every_commodity_trade_type_the_catalogue_routes_is_planned", "[trade_population]") {
    auto wide = commodity_scope();
    wide.trades_per_book = 25;

    const auto plan = plan_of(wide);

    std::set<std::string> types;
    for (const auto& slot : plan.slots)
        types.insert(slot.trade_type_code);

    /*
     * Twenty-five codes share the one table the catalogue routes them all to.
     * The planner cycles through them, so a population of one book of
     * twenty-five reaches every one.
     */
    REQUIRE(types.size() == 25u);
}

TEST_CASE("every_routed_equity_type_appears_in_a_population_of_that_length", "[trade_population]") {
    auto wide = equity_scope();
    wide.trades_per_book = 16;

    const auto plan = plan_of(wide);

    std::set<std::string> types;
    for (const auto& slot : plan.slots)
        types.insert(slot.trade_type_code);

    /*
     * Sixteen codes share nine tables -- the outperformance option is routed
     * to a table whose check cannot admit it -- and the planner cycles through
     * them, so one book of sixteen reaches every one.
     */
    REQUIRE(types.size() == 16u);
}

namespace {

/*
 * The header row a slot states, whichever table its type routes to. A slot
 * states one, so a reader that wants the drawn underlying does not have to know
 * the routing table.
 */
std::string underlying_of(const domain::trade_population_slot& slot) {
    const auto& batch = slot.instruments;
    if (!batch.equity_forward_instruments.empty())
        return batch.equity_forward_instruments.front().underlying_name;
    if (!batch.equity_option_instruments.empty())
        return batch.equity_option_instruments.front().underlying_name;
    if (!batch.equity_asian_option_instruments.empty())
        return batch.equity_asian_option_instruments.front().underlying_name;
    if (!batch.equity_barrier_option_instruments.empty())
        return batch.equity_barrier_option_instruments.front().underlying_name;
    if (!batch.equity_digital_option_instruments.empty())
        return batch.equity_digital_option_instruments.front().underlying_name;
    if (!batch.equity_swap_instruments.empty())
        return batch.equity_swap_instruments.front().underlying_name;
    if (!batch.equity_variance_swap_instruments.empty())
        return batch.equity_variance_swap_instruments.front().underlying_name;
    if (!batch.equity_accumulator_instruments.empty())
        return batch.equity_accumulator_instruments.front().underlying_name;
    if (!batch.equity_position_instruments.empty())
        return batch.equity_position_instruments.front().underlying_name;
    return {};
}

std::size_t header_rows_of(const domain::trade_population_slot& slot) {
    const auto& batch = slot.instruments;
    return batch.equity_forward_instruments.size() + batch.equity_option_instruments.size() +
           batch.equity_asian_option_instruments.size() +
           batch.equity_barrier_option_instruments.size() +
           batch.equity_digital_option_instruments.size() + batch.equity_swap_instruments.size() +
           batch.equity_variance_swap_instruments.size() +
           batch.equity_accumulator_instruments.size() + batch.equity_position_instruments.size();
}

}

TEST_CASE("an_equity_slot_names_an_underlying_the_catalogue_holds", "[trade_population]") {
    const auto plan = plan_of(equity_scope());

    std::set<std::string> named;
    for (const auto& slot : plan.slots) {
        /*
         * The family states its rows and no bag: one header row per slot, in
         * the table its type routes to.
         */
        REQUIRE(header_rows_of(slot) == 1u);
        const auto underlying = underlying_of(slot);
        if (underlying.empty()) {
            /*
             * A worst-of basket names no single underlying on its header: the
             * names are its constituents, and it states none of its own.
             */
            REQUIRE(slot.trade_type_code == "EquityWorstOfBasketSwap");
            REQUIRE(slot.instruments.equity_basket_constituents.size() == 2u);
            for (const auto& member : slot.instruments.equity_basket_constituents)
                named.insert(member.underlying_name);
        } else {
            named.insert(underlying);
        }
    }

    // Every underlying the scope states is drawn, and no other is invented.
    REQUIRE(named.size() == 4u);
    REQUIRE((named.contains(".SPX") && named.contains("MSFT.O")));
}

TEST_CASE("a_worst_of_basket_states_its_constituents_and_its_periods", "[trade_population]") {
    auto scope = equity_scope();
    scope.trades_per_book = 16;

    const auto plan = plan_of(scope);

    bool found = false;
    for (const auto& slot : plan.slots) {
        if (slot.trade_type_code != "EquityWorstOfBasketSwap")
            continue;
        found = true;
        REQUIRE(slot.instruments.equity_swap_instruments.size() == 1u);
        // Two names over four dates, which is the shape the corpus states.
        REQUIRE(slot.instruments.equity_basket_constituents.size() == 2u);
        REQUIRE(slot.instruments.equity_swap_periods.size() == 4u);
        for (const auto& member : slot.instruments.equity_basket_constituents) {
            REQUIRE_FALSE(member.underlying_name.empty());
            REQUIRE_FALSE(member.underlying_kind.empty());
            REQUIRE(member.initial_price.has_value());
        }
        for (const auto& period : slot.instruments.equity_swap_periods) {
            REQUIRE(period.period_date.has_value());
            REQUIRE(period.knock_out_level.has_value());
            REQUIRE(period.fixed_trigger_level.has_value());
        }
    }
    REQUIRE(found);
}

TEST_CASE("an_equity_population_with_no_catalogue_is_refused", "[trade_population]") {
    auto empty = equity_scope();
    empty.equity_underlyings.clear();

    REQUIRE_THROWS_AS(plan_of(empty), trade_population_error);
}

TEST_CASE("a_commodity_slot_states_its_own_rows_and_names_a_catalogue_commodity",
          "[trade_population]") {
    const auto plan = plan_of(commodity_scope());

    std::set<std::string> commodities;
    for (const auto& slot : plan.slots) {
        /*
         * The family states its rows and no bag: the plan carries what the
         * writer persists. A slot with no bag must not read as any family,
         * and the bond writer takes the first alternative it finds.
         */
        REQUIRE(slot.instruments.commodity_instruments.size() == 1u);
        const auto& row = slot.instruments.commodity_instruments.front();
        REQUIRE_FALSE(row.commodity_code.empty());
        REQUIRE_FALSE(row.unit.empty());
        commodities.insert(row.commodity_code);
    }

    // Every commodity the scope states is drawn, and no other is invented.
    REQUIRE(commodities.size() == 2u);
    for (const auto& code : commodities)
        REQUIRE((code == "NYMEX:CL" || code == "NYMEX:NG"));
}

TEST_CASE("a_commodity_population_with_no_catalogue_is_refused", "[trade_population]") {
    auto empty = commodity_scope();
    empty.commodities.clear();

    CHECK_THROWS_WITH(plan_of(empty), Catch::Matchers::ContainsSubstring("catalogue"));
}

TEST_CASE("a_bond_slot_states_its_instrument_and_its_security", "[trade_population]") {
    const auto plan = plan_of(bond_scope());

    for (const auto& slot : plan.slots) {
        /*
         * A bond states a security before it states a trade, so the plan carries
         * the issue with the rows the composite write takes, beside the batch.
         */
        REQUIRE(slot.instruments.bond_instruments.size() == 1u);
        REQUIRE(slot.bond_issue.has_value());
        const auto& issue = *slot.bond_issue;
        REQUIRE_FALSE(issue.security_id.empty());
        CHECK(issue.issue_id == slot.instruments.bond_instruments.front().instrument.issue_id);
        REQUIRE(slot.bond_issue_legs.size() == 1u);
        REQUIRE(slot.bond_issue_leg_rates.size() == 1u);
        REQUIRE(slot.bond_issue_leg_schedules.size() == 1u);
        // A notional and a coupon, which is the pair the engine prices.
        REQUIRE(slot.bond_issue_leg_amounts.size() == 2u);
        REQUIRE(slot.bond_issue_leg_amounts.front().amount_role == "notional");
        REQUIRE(slot.bond_issue_leg_amounts.back().amount_role == "rate");
    }
}

TEST_CASE("a_bond_scope_states_one_trade_per_slot_across_its_books", "[trade_population]") {
    const auto plan = plan_of(bond_scope());

    REQUIRE(plan.slots.size() == scene_trades_per_book * 2);
    REQUIRE(plan.slots.front().trade_type_code == "Bond");
    REQUIRE(plan.slots.front().book_id == bond_book);
    REQUIRE(plan.slots[1].book_id == second_book);
    REQUIRE(plan.slots.front().currency == "EUR");
}

TEST_CASE("the_same_scope_states_the_same_population_twice", "[trade_population]") {
    const auto once = plan_of(bond_scope());
    const auto twice = plan_of(bond_scope());

    REQUIRE(once.slots.size() == twice.slots.size());
    for (std::size_t i = 0; i < once.slots.size(); ++i) {
        REQUIRE(once.slots[i].trade_id == twice.slots[i].trade_id);
        REQUIRE(once.slots[i].internal_identifier == twice.slots[i].internal_identifier);
        REQUIRE(once.slots[i].bond_issue->security_id == twice.slots[i].bond_issue->security_id);
        REQUIRE(once.slots[i].notional == twice.slots[i].notional);
        REQUIRE(once.slots[i].trade_date == twice.slots[i].trade_date);
        REQUIRE(once.slots[i].bond_issue_leg_schedules.front().end_date ==
                twice.slots[i].bond_issue_leg_schedules.front().end_date);
    }
}

TEST_CASE("a_different_seed_states_a_different_population", "[trade_population]") {
    auto other = bond_scope();
    other.seed = 8;

    const auto once = plan_of(bond_scope());
    const auto twice = plan_of(other);

    REQUIRE(once.slots.front().trade_id != twice.slots.front().trade_id);
}

TEST_CASE("every_trade_date_derives_from_the_as_of_anchor", "[trade_population]") {
    auto earlier = bond_scope();
    earlier.as_of_date = std::chrono::year{2026} / 1 / 2;
    auto later = bond_scope();
    later.as_of_date = std::chrono::year{2027} / 1 / 2;

    const auto first = plan_of(earlier);
    const auto second = plan_of(later);

    REQUIRE(first.slots.front().trade_date < second.slots.front().trade_date);

    REQUIRE(first.slots.front().bond_issue_leg_schedules.front().end_date <
            second.slots.front().bond_issue_leg_schedules.front().end_date);
}

TEST_CASE("a_tenor_or_notional_band_that_does_not_order_is_refused", "[trade_population]") {
    auto inverted_tenor = bond_scope();
    inverted_tenor.min_tenor_code = "10Y";
    inverted_tenor.max_tenor_code = "1Y";
    CHECK_THROWS_AS(plan_of(inverted_tenor), trade_population_error);

    auto inverted_notional = bond_scope();
    inverted_notional.min_notional = 10000000.0;
    inverted_notional.max_notional = 100000.0;
    CHECK_THROWS_AS(plan_of(inverted_notional), trade_population_error);
}

TEST_CASE("a_scope_naming_no_book_counterparty_or_trader_is_refused", "[trade_population]") {
    auto no_book = bond_scope();
    no_book.book_ids.clear();
    CHECK_THROWS_AS(plan_of(no_book), trade_population_error);

    auto no_counterparty = bond_scope();
    no_counterparty.counterparty_ids.clear();
    CHECK_THROWS_AS(plan_of(no_counterparty), trade_population_error);

    auto no_issuer = bond_scope();
    no_issuer.issuer_ids.clear();
    CHECK_THROWS_AS(plan_of(no_issuer), trade_population_error);

    auto no_trader = bond_scope();
    no_trader.traders.clear();
    CHECK_THROWS_AS(plan_of(no_trader), trade_population_error);
}

TEST_CASE("a_family_the_generator_does_not_cover_is_refused_by_name", "[trade_population]") {
    // Inflation has no planner yet, so it is the class the registry refuses by
    // name. Equity used to serve this case until its planner was written.
    auto inflation = bond_scope();
    inflation.asset_class_code = "inflation";

    CHECK_THROWS_WITH(plan_of(inflation), Catch::Matchers::ContainsSubstring("inflation"));
}

/*
 * The fields every planner states through the shared envelope, rendered in a
 * fixed order. Two plans render the same exactly when they agree on the
 * envelope, which is what shows that extracting it changed nothing.
 */
std::string render_envelope(const domain::trade_population_plan& plan) {
    std::ostringstream s;
    for (const auto& slot : plan.slots) {
        s << slot.slot << ' ' << boost::uuids::to_string(slot.trade_id) << ' '
          << boost::uuids::to_string(slot.book_id) << ' '
          << boost::uuids::to_string(slot.counterparty_id) << ' '
          << (slot.netting_set_id ? boost::uuids::to_string(*slot.netting_set_id) : "-") << ' '
          << slot.trader.username << ' ' << slot.trade_type_code << ' ' << slot.internal_identifier
          << ' ' << slot.trade_date << ' ' << slot.notional << ' ' << slot.currency << '\n';
    }
    return s.str();
}

/*
 * A 64-bit FNV-1a over the envelope, so the recorded value is one number rather
 * than a paragraph. A collision would have to be found on purpose to pass a
 * plan the extraction changed.
 */
std::uint64_t envelope_hash(const std::string& rendered) {
    std::uint64_t hash = 1469598103934665603ULL;
    for (const auto character : rendered) {
        hash ^= static_cast<std::uint64_t>(static_cast<unsigned char>(character));
        hash *= 1099511628211ULL;
    }
    return hash;
}

TEST_CASE("the_envelope_of_a_bond_plan_is_the_one_that_was_agreed", "[trade_population]") {
    /*
     * Recorded from the planner as it stood before the envelope was extracted,
     * so this case fails if the extraction changed anything a slot states. The
     * value moved once since, deliberately: the bond planner now draws every
     * code the catalogue routes rather than a plain Bond alone, and the trade
     * type is part of the envelope.
     */
    CHECK(envelope_hash(render_envelope(plan_of(bond_scope()))) == 15911127303722676035ULL);
}

TEST_CASE("a_rate_population_states_two_legs_of_one_currency", "[trade_population]") {
    const auto scope = rate_scope();
    const auto plan = plan_of(scope);

    REQUIRE(!plan.slots.empty());
    for (const auto& slot : plan.slots) {
        /*
         * The family states its rows and no bag: the legs, the amounts, the
         * rates and the strikes are what the writer persists.
         */
        REQUIRE(slot.instruments.rate_instruments.size() == 1u);
        REQUIRE(slot.instruments.swap_legs.size() == 2u);
        REQUIRE(slot.instruments.swap_leg_amounts.size() == 2u);
        REQUIRE(slot.instruments.swap_leg_rates.size() == 2u);

        const auto& row = slot.instruments.rate_instruments.front();
        REQUIRE(row.start_date.has_value());
        REQUIRE(row.maturity_date.has_value());
        CHECK(*row.start_date < *row.maturity_date);

        const auto& floating = slot.instruments.swap_legs.back();
        CHECK(floating.leg_type_code == "Floating");
        CHECK(floating.payer.has_value());
        CHECK_FALSE(*floating.payer);
        if (slot.trade_type_code == "CrossCurrencySwap") {
            // Two legs of one currency are not a cross currency swap, so the
            // floating side states a currency the book does not.
            CHECK_FALSE(floating.currency.empty());
            CHECK(floating.currency != scope.currency_code);
        } else {
            CHECK(floating.currency == scope.currency_code);
        }

        // Only the cap floor states a strike; the swaption constructor it is
        // drawn for states its own terms and no strike.
        const bool struck = slot.trade_type_code == "CapFloor";
        CHECK(slot.instruments.instrument_strikes.size() == (struck ? 1u : 0u));

        CHECK(slot.notional >= scope.min_notional);
        CHECK(slot.notional <= scope.max_notional);
    }
}

TEST_CASE("every_routed_rate_type_appears_in_a_population_of_that_length", "[trade_population]") {
    const auto scope = rate_scope();
    const auto plan = plan_of(scope);

    // The nine the router sends to rate_instrument, less the inflation swap.
    CHECK(codes_of(plan).size() == 9);
}

TEST_CASE("a_rate_config_that_states_one_slot_per_routed_code_draws_every_code",
          "[trade_population]") {
    /*
     * The seeded rate configs state one trade per routed code, because a plan
     * cycles the list by position: a config of eight slots into nine codes
     * never reaches the ninth, and the knock-out swap was the code it missed.
     */
    auto wide = rate_scope();
    wide.trades_per_book = 64;
    const auto routed = codes_of(plan_of(wide));
    REQUIRE(routed.size() == 9);

    auto seeded = rate_scope();
    seeded.book_ids = {bond_book};
    seeded.trades_per_book = static_cast<std::uint32_t>(routed.size());

    const auto plan = plan_of(seeded);
    REQUIRE(plan.slots.size() == routed.size());
    CHECK(codes_of(plan) == routed);
    CHECK(codes_of(plan).contains("KnockOutSwap"));
}

TEST_CASE("a_rate_population_without_an_index_catalogue_is_refused", "[trade_population]") {
    auto scope = rate_scope();
    scope.floating_index_codes.clear();

    CHECK_THROWS_AS(plan_of(scope), trade_population_error);
}

TEST_CASE("an_fx_population_states_the_row_its_type_routes_to", "[trade_population]") {
    const auto plan = plan_of(fx_scope());

    const auto barrier = [](const std::string& type) {
        return type.starts_with("FxBarrier") || type == "FxDoubleBarrierOption" ||
               type == "FxEuropeanBarrierOption" || type == "FxGenericBarrierOption" ||
               type == "FxKIKOBarrierOption";
    };
    const auto digital = [](const std::string& type) {
        return type == "FxDigitalOption" || type == "FxTouchOption" ||
               type == "FxDoubleTouchOption" || type == "FxDigitalBarrierOption";
    };

    REQUIRE(!plan.slots.empty());
    for (const auto& slot : plan.slots) {
        /*
         * The family states its rows and no bag: exactly one routed row per
         * slot, and it is the row the type belongs to.
         */
        const auto& type = slot.trade_type_code;
        const auto& batch = slot.instruments;

        if (type == "FxForward" || type == "FxSwap") {
            REQUIRE(batch.fx_forward_instruments.size() == 1u);
            const auto& row = batch.fx_forward_instruments.front();
            CHECK_FALSE(row.bought_currency.empty());
            CHECK_FALSE(row.sold_currency.empty());
        } else if (type == "FxOption") {
            REQUIRE(batch.fx_vanilla_option_instruments.size() == 1u);
            const auto& row = batch.fx_vanilla_option_instruments.front();
            CHECK_FALSE(row.bought_currency.empty());
            CHECK_FALSE(row.option_type.empty());
        } else if (barrier(type)) {
            REQUIRE(batch.fx_barrier_option_instruments.size() == 1u);
            const auto& row = batch.fx_barrier_option_instruments.front();
            CHECK_FALSE(row.bought_currency.empty());
            CHECK_FALSE(row.barrier_type.empty());
            CHECK(row.lower_barrier.has_value());
        } else if (digital(type)) {
            REQUIRE(batch.fx_digital_option_instruments.size() == 1u);
            const auto& row = batch.fx_digital_option_instruments.front();
            CHECK_FALSE(row.foreign_currency.empty());
            CHECK_FALSE(row.payoff_amount.to_string() == "0");
        } else if (type == "FxAverageForward" || type == "FxTaRF") {
            // The average forward and the target redemption forward are
            // written on an index, not on a named underlying.
            REQUIRE(batch.fx_asian_forward_instruments.size() == 1u);
            const auto& row = batch.fx_asian_forward_instruments.front();
            CHECK_FALSE(row.fx_index.empty());
            CHECK(row.reference_notional.has_value());
        } else if (type == "FxVarianceSwap") {
            REQUIRE(batch.fx_variance_swap_instruments.size() == 1u);
            const auto& row = batch.fx_variance_swap_instruments.front();
            CHECK_FALSE(row.underlying_code.empty());
            // The store requires an end after the start, and a short tenor band
            // can draw a window of no length.
            INFO("start " << row.start_date << " end " << row.end_date);
            CHECK(row.end_date > row.start_date);
            CHECK_FALSE(row.moment_type.empty());
        } else {
            REQUIRE(batch.fx_accumulator_instruments.size() == 1u);
            const auto& row = batch.fx_accumulator_instruments.front();
            CHECK_FALSE(row.underlying_code.empty());
            CHECK(row.knock_out_barrier.has_value());
        }
    }
}

TEST_CASE("every_routed_fx_type_appears_in_a_population_of_that_length", "[trade_population]") {
    const auto plan = plan_of(fx_scope());

    std::set<std::string> types;
    for (const auto& slot : plan.slots)
        types.insert(slot.trade_type_code);

    CHECK(types.size() == 16);
}

TEST_CASE("an_fx_config_that_states_one_slot_per_routed_code_draws_every_code",
          "[trade_population]") {
    /*
     * The fx planner cycles sixteen routed codes. A config of eight slots never
     * reaches the variance swap or the accumulator, which is what a tenant held
     * before the seeded configs stated one trade per code.
     */
    auto wide = fx_scope();
    wide.trades_per_book = 64;
    const auto routed = codes_of(plan_of(wide));
    REQUIRE(routed.size() == 16);

    auto seeded = fx_scope();
    seeded.book_ids = {bond_book};
    seeded.trades_per_book = static_cast<std::uint32_t>(routed.size());

    const auto plan = plan_of(seeded);
    REQUIRE(plan.slots.size() == routed.size());
    CHECK(codes_of(plan) == routed);
    CHECK(codes_of(plan).contains("FxVarianceSwap"));
    CHECK(codes_of(plan).contains("FxAccumulator"));
}

TEST_CASE("an_fx_variance_swap_states_a_window_with_a_length", "[trade_population]") {
    /*
     * The seeded fx configs state a tenor band from one month, so a draw can
     * land under a year and the window would have no length. The store refuses
     * an end that is not after the start, so the planner must state one.
     */
    auto scope = fx_scope();
    scope.book_ids = {bond_book};
    scope.trades_per_book = 16;
    scope.min_tenor_code = "1M";
    scope.max_tenor_code = "2Y";

    bool found = false;
    for (const auto& slot : plan_of(scope).slots) {
        if (slot.instruments.fx_variance_swap_instruments.empty())
            continue;
        found = true;
        const auto& row = slot.instruments.fx_variance_swap_instruments.front();
        INFO("start " << row.start_date << " end " << row.end_date);
        CHECK(row.end_date > row.start_date);
        CHECK(row.start_date.year() != std::chrono::year{0});
        CHECK(row.end_date.year() != std::chrono::year{0});
    }
    REQUIRE(found);
}

TEST_CASE("an_fx_population_prefers_the_pairs_that_name_the_book_currency", "[trade_population]") {
    const auto plan = plan_of(fx_scope());

    /*
     * EUR/USD names the book's currency and GBP/USD does not, so every slot
     * takes the first: a bought-and-sold row states it as the bought currency,
     * and an index row as the currency it references.
     */
    for (const auto& slot : plan.slots) {
        const auto& batch = slot.instruments;
        if (!batch.fx_forward_instruments.empty()) {
            CHECK(batch.fx_forward_instruments.front().bought_currency == "EUR");
        } else if (!batch.fx_vanilla_option_instruments.empty()) {
            CHECK(batch.fx_vanilla_option_instruments.front().bought_currency == "EUR");
        } else if (!batch.fx_barrier_option_instruments.empty()) {
            CHECK(batch.fx_barrier_option_instruments.front().bought_currency == "EUR");
        } else if (!batch.fx_digital_option_instruments.empty()) {
            CHECK(batch.fx_digital_option_instruments.front().foreign_currency == "EUR");
        } else if (!batch.fx_asian_forward_instruments.empty()) {
            CHECK(batch.fx_asian_forward_instruments.front().reference_currency == "EUR");
        }
    }
}

TEST_CASE("an_fx_population_anchored_on_a_months_last_day_states_only_dates_that_exist",
          "[trade_population]") {
    /*
     * The population is anchored on the day it runs, and a slot starts a few
     * months before it. On the 31st of October two of those months hold no
     * 31st, so a month that keeps the day it was given invents a date. The run
     * that drew them stated both ends of the variance window as the absent
     * date and the store refused the swap.
     */
    auto scope = fx_scope();
    scope.as_of_date = std::chrono::year{2026} / 10 / 31;

    const auto plan = plan_of(scope);
    REQUIRE(plan.slots.size() == 16u);

    bool saw_variance_swap = false;
    for (const auto& slot : plan.slots) {
        CHECK(slot.trade_date.ok());
        const auto& batch = slot.instruments;
        for (const auto& row : batch.fx_forward_instruments)
            CHECK(row.value_date.ok());
        for (const auto& row : batch.fx_vanilla_option_instruments)
            CHECK(row.expiry_date.ok());
        for (const auto& row : batch.fx_barrier_option_instruments)
            CHECK(row.expiry_date.ok());
        for (const auto& row : batch.fx_digital_option_instruments)
            CHECK(row.expiry_date.ok());
        for (const auto& row : batch.fx_asian_forward_instruments)
            if (row.payment_date.has_value())
                CHECK(row.payment_date->ok());
        for (const auto& row : batch.fx_accumulator_instruments)
            CHECK(row.start_date.ok());
        for (const auto& row : batch.fx_variance_swap_instruments) {
            saw_variance_swap = true;
            CHECK(row.start_date.ok());
            CHECK(row.end_date.ok());
            CHECK(row.end_date > row.start_date);
        }
    }
    CHECK(saw_variance_swap);
}

TEST_CASE("an_fx_population_without_a_pair_catalogue_is_refused", "[trade_population]") {
    auto scope = fx_scope();
    scope.currency_pairs.clear();

    CHECK_THROWS_AS(plan_of(scope), trade_population_error);
}

TEST_CASE("the_notional_of_every_slot_sits_in_the_band", "[trade_population]") {
    const auto scope = bond_scope();
    const auto plan = plan_of(scope);

    for (const auto& slot : plan.slots) {
        REQUIRE(slot.notional >= scope.min_notional);
        REQUIRE(slot.notional <= scope.max_notional);
        // The security's term is the distance its schedule spans.
        const auto& schedule = slot.bond_issue_leg_schedules.front();
        REQUIRE(schedule.start_date.has_value());
        REQUIRE(schedule.end_date.has_value());
        const auto days =
            std::chrono::sys_days{*schedule.end_date} - std::chrono::sys_days{*schedule.start_date};
        CHECK(days.count() >= 300);
        CHECK(days.count() <= 3700);
    }
}

TEST_CASE("every_trade_of_a_security_states_the_terms_of_that_security", "[trade_population]") {
    const auto plan = plan_of(bond_scope());

    std::map<std::string, domain::trade_population_slot> first_of;
    for (const auto& slot : plan.slots) {
        const auto [found, inserted] = first_of.emplace(bond_of(slot).security_id, slot);
        if (inserted)
            continue;
        REQUIRE(bond_of(slot).issue_id == bond_of(found->second).issue_id);
        REQUIRE(bond_of(slot).issuer_id == bond_of(found->second).issuer_id);
        REQUIRE(bond_of(slot).issue_date == bond_of(found->second).issue_date);
        REQUIRE(bond_of(slot).maturity_date == bond_of(found->second).maturity_date);
        REQUIRE(bond_of(slot).tenor_years == bond_of(found->second).tenor_years);
    }
}

TEST_CASE("a_securitys_maturity_is_its_issue_date_plus_its_term", "[trade_population]") {
    const auto plan = plan_of(bond_scope());

    for (const auto& slot : plan.slots) {
        REQUIRE(bond_of(slot).maturity_date ==
                bond_of(slot).issue_date +
                    std::chrono::years{static_cast<int>(bond_of(slot).tenor_years)});
        REQUIRE(slot.trade_date >= bond_of(slot).issue_date);
        REQUIRE(slot.trade_date < bond_of(slot).maturity_date);
    }
}

TEST_CASE("the_tenor_band_reaches_the_maturities_of_the_securities", "[trade_population]") {
    const auto plan = plan_of(bond_scope());

    /*
     * A security is named by its term and its product, so a term carries several
     * securities and no security carries two terms.
     */
    std::map<std::string, std::uint32_t> term_of;
    std::set<std::uint32_t> terms;
    for (const auto& slot : plan.slots) {
        const auto bond = bond_of(slot);
        terms.insert(bond.tenor_years);
        const auto [found, inserted] = term_of.emplace(bond.security_id, bond.tenor_years);
        if (!inserted)
            REQUIRE(found->second == bond.tenor_years);
    }

    REQUIRE(terms.size() > 1);
    REQUIRE(term_of.size() >= terms.size());
}

TEST_CASE("a_security_is_named_by_its_product", "[trade_population]") {
    const auto plan = plan_of(bond_scope());

    /*
     * Ten bond codes route to the instrument table and each states its own rows,
     * so two products of one term are two securities: a callable bond states the
     * dates its issuer may call it and a plain bond of the same term states none.
     */
    std::map<std::string, std::set<std::string>> products_of;
    for (const auto& slot : plan.slots) {
        const auto bond = bond_of(slot);
        products_of[bond.security_id].insert(slot.trade_type_code);
    }
    const auto two_products = std::ranges::any_of(
        products_of, [](const auto& entry) { return entry.second.size() > 1u; });
    REQUIRE_FALSE(two_products);

    const auto term = plan.slots.front();
    const auto& bond = bond_of(term);
    REQUIRE(security_id_for(bond_scope(), bond.tenor_years, "Bond", bond.issuer_id) !=
            security_id_for(bond_scope(), bond.tenor_years, "CallableBond", bond.issuer_id));
}

TEST_CASE("a_security_states_the_currency_of_the_books_that_hold_it", "[trade_population]") {
    auto euro = bond_scope();
    euro.currency_code = "EUR";
    auto dollar = bond_scope();
    dollar.currency_code = "USD";

    const auto issuer = issuer_id_for(euro, 5);
    REQUIRE(security_id_for(euro, 5, "Bond", issuer) !=
            security_id_for(dollar, 5, "Bond", issuer));
    REQUIRE(security_id_for(euro, 5, "Bond", issuer) ==
            security_id_for(euro, 5, "Bond", issuer));
}

TEST_CASE("a_security_is_not_tied_to_a_book", "[trade_population]") {
    /*
     * The name comes from the security's own facts, so the same security named
     * from a scope that keeps one book and from a scope that keeps another is one
     * name. The draw may still put a given security in one book only, because
     * the product and the book are both taken from the slot.
     */
    auto first = bond_scope();
    first.book_ids = {bond_book};
    auto second = bond_scope();
    second.book_ids = {second_book};

    const auto issuer = issuer_id_for(first, 5);
    REQUIRE(security_id_for(first, 5, "Bond", issuer) ==
            security_id_for(second, 5, "Bond", issuer));
}

TEST_CASE("a_security_is_named_by_its_issuer", "[trade_population]") {
    /*
     * Each legal entity of a tenant holds its own issuer catalogue, so the same
     * currency, anchor, term and product under two issuers is two securities.
     * One name for both made the second entity's config write against the first
     * one's issue, and the store refused it as stored with other terms.
     */
    auto first = bond_scope();
    first.issuer_ids = {bank};
    auto second = bond_scope();
    second.issuer_ids = {second_bank};

    const auto first_issuer = issuer_id_for(first, 5);
    const auto second_issuer = issuer_id_for(second, 5);
    REQUIRE(first_issuer != second_issuer);
    REQUIRE(security_id_for(first, 5, "Bond", first_issuer) !=
            security_id_for(second, 5, "Bond", second_issuer));
}

TEST_CASE("two_runs_over_one_scope_write_nothing_the_second_time", "[trade_population]") {
    recording_writer writer(bond_scope());
    trade_population_service service(writer);

    const auto first = service.run(config_id);
    REQUIRE(first.planned == scene_trades_per_book * 2);
    REQUIRE(first.written == scene_trades_per_book * 2);
    REQUIRE(first.already_present == 0);
    REQUIRE(writer.bookings == scene_trades_per_book * 2);

    const auto versions_after_first = writer.versions;

    const auto second = service.run(config_id);
    REQUIRE(second.planned == scene_trades_per_book * 2);
    REQUIRE(second.written == 0);
    REQUIRE(second.already_present == scene_trades_per_book * 2);
    REQUIRE(writer.bookings == scene_trades_per_book * 2);

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

TEST_CASE("a_security_has_the_same_issuer_whatever_counterparties_a_config_scopes",
          "[trade_population]") {
    const auto wide = bond_scope();
    auto narrow = bond_scope();
    narrow.counterparty_ids = {second_bank};
    std::ranges::reverse(narrow.issuer_ids);

    std::map<std::string, boost::uuids::uuid> issuer_of;
    for (const auto& slot : plan_of(wide).slots)
        issuer_of.emplace(bond_of(slot).security_id, bond_of(slot).issuer_id);

    for (const auto& slot : plan_of(narrow).slots) {
        const auto found = issuer_of.find(bond_of(slot).security_id);
        if (found != issuer_of.end())
            REQUIRE(bond_of(slot).issuer_id == found->second);
    }
}

/*
 * The FX family states sixteen types and the profile of the ACME FX config
 * draws a band from one month to two years, so the variance swap is reached.
 * Its own table refuses a row whose end is not after its start, so the plan
 * has to state both in order before anything is booked.
 */
TEST_CASE("a_planned_fx_variance_swap_ends_after_it_starts", "[trade_population]") {
    auto scope = fx_scope();
    scope.min_tenor_code = "1M";
    scope.max_tenor_code = "2Y";
    scope.trades_per_book = 16;
    scope.seed = 44;
    /*
     * The day a run anchors on is the day the population draws its months back
     * from, so the thirty-first is the anchor that has no such day in every
     * month. It is the anchor a run actually takes.
     */
    scope.as_of_date = std::chrono::year{2026} / std::chrono::October / 31;

    const auto plan = plan_of(scope);

    std::size_t swaps = 0;
    for (const auto& slot : plan.slots) {
        for (const auto& row : slot.instruments.fx_variance_swap_instruments) {
            ++swaps;
            INFO("start " << std::format("{:%Y-%m-%d}", row.start_date) << " end "
                          << std::format("{:%Y-%m-%d}", row.end_date));
            CHECK(row.end_date > row.start_date);
        }
    }
    /* One per book: sixteen types in the cycle, two books in the scope. */
    CHECK(swaps == 2);
}
