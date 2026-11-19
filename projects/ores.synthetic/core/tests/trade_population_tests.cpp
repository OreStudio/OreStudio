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
#include "ore_example_conventions.hpp"
#include "ores.synthetic.core/service/trade_population_planner.hpp"
#include "ores.synthetic.core/service/trade_population_service.hpp"
#include "ores.trading.api/domain/instrument_batch.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <chrono>
#include <format>
#include <map>
#include <optional>
#include <set>
#include <sstream>
#include <string>
#include <type_traits>
#include <utility>
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
    std::string issuer;
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
    view.issuer = issue.issuer;
    view.issue_date = *issue.issue_date;
    view.maturity_date = *schedule.end_date;
    const auto days =
        std::chrono::sys_days{view.maturity_date} - std::chrono::sys_days{view.issue_date};
    view.tenor_years = static_cast<std::uint32_t>(
        std::chrono::duration_cast<std::chrono::days>(days).count() / 365);
    return view;
}

namespace {

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

} // namespace

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
    scope.conventions = ores::synthetic::tests::ore_example_conventions();
    /*
     * A convertible bond states the share it may convert into, so every bond
     * scope needs the equity catalogue the writer loads for it.
     */
    scope.equity_underlyings = {domain::equity_underlying_reference{
        .code = "GB0005405286", .kind = "share", .currency = "GBP"}};
    // A callable bond names the credit curve of a legal entity the tenant holds.
    scope.credit_reference_entities = {"529900T8BM49AURSDO55", "5493001KJTIIGC8Y1R12"};
    scope.book_ids = {bond_book, second_book};
    scope.counterparty_ids = {bank, second_bank};
    scope.issuers = {{.id = bank, .name = "Bank One Plc"},
                     {.id = second_bank, .name = "Bank Two Plc"}};
    scope.traders = {domain::trade_population_trader{trader_account, "trader.one"}};
    scope.currency_code = "EUR";
    return scope;
}

/*
 * A scope of the rate family. The trade types the router sends to
 * rate_instrument are eleven here, and the catalogue is the tenant's own, so the
 * scope states three indices of one currency and one of another.
 */
domain::trade_generation_scope rate_scope() {
    auto scope = bond_scope();
    scope.asset_class_code = "interest_rates";
    scope.floating_index_codes = {"EUR-EURIBOR-6M", "EUR-EONIA", "EUR-ESTR", "USD-SOFR"};
    return scope;
}

/*
 * A scope of the scripted family. The class is the refdata taxonomy's name for a
 * payoff stated as a script, and the underlyings are the tenant's equity
 * catalogue, which is what the population's scripts observe.
 */
domain::trade_generation_scope scripted_scope() {
    auto scope = bond_scope();
    scope.asset_class_code = "scripted";
    scope.equity_underlyings = {domain::equity_underlying_reference{
                                    .code = "GB0005405286", .kind = "share", .currency = "GBP"},
                                domain::equity_underlying_reference{
                                    .code = "US0378331005", .kind = "share", .currency = "USD"},
                                domain::equity_underlying_reference{
                                    .code = "DE0007164600", .kind = "share", .currency = "EUR"}};
    scope.script_inputs = ores::synthetic::tests::asian_basket_option_inputs();
    return scope;
}

/*
 * A scope of the inflation family. The tenant holds three curves, one per
 * currency its catalogue carries an index for, so a population of more than
 * three slots states an index more than once.
 */
domain::trade_generation_scope inflation_scope() {
    auto scope = bond_scope();
    scope.asset_class_code = "inflation";
    scope.inflation_index_codes = {"UKRPIX", "USCPIA", "BECPIHL"};
    return scope;
}

/*
 * A scope of the FX family. The routed types are twenty-six here, and the
 * catalogue carries one pair of the book's currency and one that names neither
 * side of it, so the preference is testable.
 */
domain::trade_generation_scope fx_scope() {
    auto scope = bond_scope();
    scope.asset_class_code = "fx";
    scope.currency_code = "EUR";
    scope.currency_pairs = {{.base = "EUR", .quote = "USD"}, {.base = "GBP", .quote = "USD"}};
    scope.floating_index_codes = {"EUR-ESTR", "USD-SOFR"};
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
    scope.conventions = ores::synthetic::tests::ore_example_conventions();
    return scope;
}

TEST_CASE("a_credit_slot_states_its_row_and_names_a_legal_entity", "[trade_population]") {
    const auto plan = plan_of(credit_scope());

    REQUIRE(!plan.slots.empty());
    for (const auto& slot : plan.slots) {
        // A CBO states its structure inline: its scalars, its two tranches with
        // a leg each, and the bonds of its basket.
        if (slot.trade_type_code == "CBO") {
            CHECK(slot.instruments.credit_instruments.size() == 1u);
            CHECK(slot.instruments.cbo_instruments.size() == 1u);
            CHECK(slot.instruments.cbo_structures.size() == 1u);
            CHECK(slot.instruments.cbo_tranches.size() == 2u);
            CHECK(slot.instruments.swap_legs.size() == 2u);
            CHECK(slot.instruments.cbo_basket_members.size() == 3u);
            continue;
        }
        REQUIRE(slot.instruments.credit_instruments.size() == 1u);
        const auto& row = slot.instruments.credit_instruments.front();
        CHECK_FALSE(slot.instruments.swap_legs.empty());
        // A tranche is cut from an index, which is no legal entity.
        if (slot.trade_type_code == "SyntheticCDO") {
            CHECK(row.reference_entity_id_type == "Internal");
            CHECK(row.reference_entity_id.starts_with("oresmd://credit/"));
            continue;
        }
        // Any other credit trade names its party by the LEI the dataset holds.
        CHECK(row.reference_entity_id_type == "LEI");
        CHECK(row.reference_entity_id.size() == 20u);
    }
}

TEST_CASE("a_credit_trade_names_the_credit_curve_of_its_reference_entity", "[trade_population]") {
    auto scope = credit_scope();
    scope.trades_per_book = 1;
    const auto plan = plan_of(scope);

    REQUIRE_FALSE(plan.slots.empty());
    const auto& batch = plan.slots.front().instruments;
    REQUIRE(batch.credit_default_swap_instruments.size() == 1u);
    const auto& curve = batch.credit_default_swap_instruments.front().credit_curve_id;
    REQUIRE(curve.has_value());
    CHECK(curve->starts_with("oresmd://credit/" +
                             batch.credit_instruments.front().reference_entity_id + "?"));
}

namespace {

/// Gives each swap convention a rival that names its index with other terms.
void name_each_swap_index_twice(domain::trade_generation_scope& scope) {
    const auto swaps = scope.conventions.swaps;
    for (auto rival : swaps) {
        rival.id += "-RIVAL";
        rival.fixed_frequency = rival.fixed_frequency == "Annual" ? "Semiannual" : "Annual";
        scope.conventions.swaps.push_back(rival);
    }
}

}

TEST_CASE("an_rpa_builds_its_underlying_swap_when_two_conventions_name_the_index",
          "[trade_population]") {
    auto scope = credit_scope();
    scope.trade_type_codes = {"RiskParticipationAgreement"};
    name_each_swap_index_twice(scope);

    const auto plan = plan_of(scope);

    REQUIRE_FALSE(plan.slots.empty());
    CHECK_FALSE(plan.slots.front().instruments.swap_legs.empty());
}

TEST_CASE("an_rpa_in_a_currency_no_swap_convention_floats_in_is_refused", "[trade_population]") {
    auto scope = credit_scope();
    scope.trade_type_codes = {"RiskParticipationAgreement"};
    scope.currency_code = "JPY";
    CHECK_THROWS_AS(plan_of(scope), trade_population_error);
}

TEST_CASE("a_credit_scope_whose_conventions_lack_the_cds_convention_is_refused",
          "[trade_population]") {
    auto scope = credit_scope();
    scope.conventions.cds.clear();
    CHECK_THROWS_AS(plan_of(scope), trade_population_error);
}

/*
 * The codes a plan states. A planner cycles its routed list by the slot's
 * position, so a plan shorter than the list states a prefix of it and the codes
 * past that position are never drawn. A config that wants every code states one
 * slot per code, which is what the seeded profiles do.
 */
/// Whether the slot is the scripted twin of a native slot.
bool is_scripted_twin(const domain::trade_population_slot& slot) {
    return slot.internal_identifier.ends_with("-scripted");
}

/// The codes the planner draws: the native slots' codes, without their twins.
std::set<std::string> codes_of(const domain::trade_population_plan& plan) {
    std::set<std::string> codes;
    for (const auto& slot : plan.slots)
        if (!is_scripted_twin(slot))
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
     * Twenty-four codes share the one table the catalogue routes them all to.
     * The planner cycles through them, so a population of one book of
     * twenty-four reaches every one. The pairwise variance swap is not among
     * them: ORE has no builder for it, so the generator leaves it out.
     */
    REQUIRE(types.size() == 24u);
    CHECK(types.contains("CommodityWindowBarrierOption"));
}

TEST_CASE("every_routed_equity_type_appears_in_a_population_of_that_length", "[trade_population]") {
    auto wide = equity_scope();
    wide.trades_per_book = 33;

    const auto plan = plan_of(wide);

    std::set<std::string> types;
    std::size_t held = 0;
    std::size_t twins = 0;
    for (const auto& slot : plan.slots) {
        if (is_scripted_twin(slot)) {
            ++twins;
            continue;
        }
        types.insert(slot.trade_type_code);
        if (slot.structure_role == "sub_trade")
            ++held;
    }

    /*
     * Thirty-three codes, thirty products and three containers, and the
     * planner cycles through them, so one book of thirty-three reaches every one. The
     * composite holds two options and each return swap one position, which
     * are trades of their own.
     */
    REQUIRE(types.size() == 33u);
    const auto books = wide.book_ids.size();
    CHECK(held == 4u * books);
    CHECK(plan.slots.size() - twins == 37u * books);
    CHECK(plan.structures.size() == 3u * books);
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

    // A name is an equity address, or a basket member's catalogue code; both name one code.
    const auto code_of = [](const std::string& name) {
        const auto query = name.find('?');
        if (!name.starts_with("oresmd://") || query == std::string::npos)
            return name;
        const auto slash = name.rfind('/', query);
        return name.substr(slash + 1, query - slash - 1);
    };
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
                named.insert(code_of(member.underlying_name));
        } else {
            named.insert(code_of(underlying));
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
        REQUIRE(slot.instruments.equity_basket_constituents.size() == 2u);
        // The periods run to maturity, and each schedule holds the start and every period end.
        const auto& periods = slot.instruments.basket_swap_periods;
        REQUIRE_FALSE(periods.empty());
        REQUIRE(periods.back().period_date ==
                slot.instruments.equity_swap_instruments.front().maturity_date);
        for (const auto* role : {"floating_period_schedule", "floating_pay_schedule"}) {
            const auto dates =
                std::ranges::count_if(slot.instruments.instrument_schedule_dates,
                                      [&](const auto& date) { return date.schedule_role == role; });
            REQUIRE(static_cast<std::size_t>(dates) == periods.size() + 1);
        }
        for (const auto& member : slot.instruments.equity_basket_constituents) {
            REQUIRE_FALSE(member.underlying_name.empty());
            REQUIRE_FALSE(member.underlying_kind.empty());
            REQUIRE(member.initial_price.has_value());
        }
        for (const auto& period : slot.instruments.basket_swap_periods) {
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
        /*
         * A basket's members may count different units, so a basket row states
         * none. A digital's quantity is the cash it pays, which counts none.
         */
        const auto& type = row.identity.trade_type_code;
        if (type != "CommodityBasketOption" && type != "CommodityDigitalOption" &&
            type != "CommodityDigitalAveragePriceOption")
            REQUIRE_FALSE(row.unit.empty());
        // Every commodity row is built, so it names its commodity by address.
        REQUIRE(row.commodity_code.starts_with("oresmd://"));
        const auto query = row.commodity_code.find('?');
        const auto slash = row.commodity_code.rfind('/', query);
        commodities.insert(row.commodity_code.starts_with("oresmd://") ?
                               row.commodity_code.substr(slash + 1, query - slash - 1) :
                               row.commodity_code);
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
        // An option on a bond future names a contract rather than the slot's
        // security, and it is held on the bond family's own future and option
        // rows.
        if (slot.trade_type_code == "BondFutureOption") {
            CHECK(slot.instruments.bond_instruments.size() == 1u);
            CHECK_FALSE(slot.instruments.bond_futures.empty());
            CHECK_FALSE(slot.instruments.bond_options.empty());
            continue;
        }
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
    no_issuer.issuers.clear();
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
     * value moved twice since, deliberately: the bond planner draws every
     * code the catalogue routes rather than a plain Bond alone, and the trade
     * type is part of the envelope; then the option on a bond future joined
     * the cycle.
     */
    CHECK(envelope_hash(render_envelope(plan_of(bond_scope()))) == 5144697870441103994ULL);
}

TEST_CASE("a_forward_rate_agreement_draws_an_ibor_index_not_an_overnight_one",
          "[trade_population]") {
    /*
     * The tenant's swap conventions also state overnight indices, and an
     * agreement fixes on an Ibor one. At this length the agreement is drawn
     * often enough that a pool of every swap convention lands it on the
     * overnight index, which its builder refuses.
     */
    auto scope = rate_scope();
    auto overnight_swap = scope.conventions.swaps.front();
    overnight_swap.id = "EUR-EONIA-1Y-SWAP";
    overnight_swap.index = "EUR-EONIA";
    overnight_swap.oresmd_uri = "oresmd://ir/EUR?type=fixing&index=overnight&name=EONIA";
    scope.conventions.swaps.push_back(overnight_swap);
    ores::refdata::domain::overnight_index_convention eonia;
    eonia.id = "EUR-EONIA";
    scope.conventions.overnight_indices.push_back(eonia);
    scope.trades_per_book = 22;

    const auto plan = plan_of(scope);
    bool saw_agreement = false;
    for (const auto& slot : plan.slots) {
        if (slot.trade_type_code != "ForwardRateAgreement")
            continue;
        saw_agreement = true;
        const auto& floating = slot.instruments.swap_legs.back();
        CHECK(floating.floating_index_code.find("index=ibor") != std::string::npos);
    }
    CHECK(saw_agreement);
}

TEST_CASE("a_rate_population_states_its_legs_in_the_book_currency", "[trade_population]") {
    const auto scope = rate_scope();
    const auto plan = plan_of(scope);

    REQUIRE(!plan.slots.empty());
    for (const auto& slot : plan.slots) {
        // A rate digital option holds no legs: its terms are a row of the rates
        // family's own table, under the family header.
        if (slot.trade_type_code == "RateDigitalOption") {
            CHECK(slot.instruments.rate_instruments.size() == 1u);
            CHECK(slot.instruments.rate_digital_option_instruments.size() == 1u);
            CHECK(slot.instruments.swap_legs.empty());
            continue;
        }
        /*
         * The family states its rows and no bag: the legs, the amounts, the
         * rates and the strikes are what the writer persists.
         */
        // A cap or a floor pays on the floating leg alone; every other type
        // states a fixed leg against it.
        const auto legs = slot.trade_type_code == "CapFloor" ? 1u : 2u;
        REQUIRE(slot.instruments.rate_instruments.size() == 1u);
        REQUIRE(slot.instruments.swap_legs.size() == legs);
        REQUIRE(slot.instruments.swap_leg_amounts.size() == legs);
        REQUIRE(slot.instruments.swap_leg_rates.size() == legs);

        const auto& row = slot.instruments.rate_instruments.front();
        REQUIRE(row.start_date.has_value());
        REQUIRE(row.maturity_date.has_value());
        CHECK(*row.start_date < *row.maturity_date);

        const auto& floating = slot.instruments.swap_legs.back();
        CHECK(floating.leg_type_code == "Floating");
        CHECK(floating.payer.has_value());
        CHECK(floating.floating_index_code.starts_with("oresmd://ir/"));
        if (slot.trade_type_code == "CrossCurrencySwap") {
            // Two legs of one currency are not a cross currency swap, so the
            // floating side states a currency the book does not.
            CHECK_FALSE(floating.currency.empty());
            CHECK(floating.currency != scope.currency_code);
        } else {
            CHECK(floating.currency == scope.currency_code);
        }

        // Only the cap floor states a strike; a swaption's strike is its fixed rate.
        const bool struck = slot.trade_type_code == "CapFloor";
        CHECK(slot.instruments.instrument_strikes.size() == (struck ? 1u : 0u));

        CHECK(slot.notional >= scope.min_notional);
        CHECK(slot.notional <= scope.max_notional);
    }
}

TEST_CASE("every_routed_rate_type_appears_in_a_population_of_that_length", "[trade_population]") {
    auto scope = rate_scope();
    scope.trades_per_book = 11;
    const auto plan = plan_of(scope);

    /*
     * The eleven the planner books, which are the ones the router sends to
     * rate_instrument less the inflation swap, and the rate digital option the
     * scripted table holds.
     */
    CHECK(codes_of(plan).size() == 11);
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
    REQUIRE(routed.size() == 11);

    auto seeded = rate_scope();
    seeded.book_ids = {bond_book};
    seeded.trades_per_book = static_cast<std::uint32_t>(routed.size());

    const auto plan = plan_of(seeded);
    REQUIRE(static_cast<std::size_t>(std::ranges::count_if(
                plan.slots, [](const auto& s) { return !is_scripted_twin(s); })) == routed.size());
    CHECK(codes_of(plan) == routed);
    CHECK(codes_of(plan).contains("KnockOutSwap"));
}

TEST_CASE("a_rate_population_draws_an_index_its_alias_conventions_name_and_skips_a_conflict",
          "[trade_population]") {
    auto scope = rate_scope();
    const auto six_months = scope.conventions.swaps.front();
    auto alias = six_months;
    alias.id = "EUR-SWAP";
    auto conflict = six_months;
    conflict.id = "EUR-EURIBOR-3M-SWAP";
    conflict.index = "EUR-EURIBOR-3M";
    conflict.oresmd_uri = "oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=3M";
    auto other_terms = conflict;
    other_terms.id = "EUR-3M-SWAP-CONVENTIONS";
    other_terms.fixed_frequency = "Semiannual";
    scope.conventions.swaps.push_back(alias);
    scope.conventions.swaps.push_back(conflict);
    scope.conventions.swaps.push_back(other_terms);

    const auto plan = plan_of(scope);

    REQUIRE_FALSE(plan.slots.empty());
}

TEST_CASE("a_rate_population_without_a_swap_convention_in_its_currency_is_refused",
          "[trade_population]") {
    auto scope = rate_scope();
    scope.conventions.swaps.clear();

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
            CHECK((row.lower_barrier.has_value() || row.upper_barrier.has_value()));
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
            CHECK(row.fx_index.starts_with("oresmd://fx/"));
            // A TaRF states its fixing amount; only an average forward states
            // a reference notional.
            if (type == "FxAverageForward")
                CHECK(row.reference_notional.has_value());
            else
                CHECK(row.fixing_amount.has_value());
        } else if (type == "FxVarianceSwap") {
            REQUIRE(batch.fx_variance_swap_instruments.size() == 1u);
            const auto& row = batch.fx_variance_swap_instruments.front();
            CHECK_FALSE(row.underlying_code.empty());
            // The store requires an end after the start, and a short tenor band
            // can draw a window of no length.
            INFO("start " << row.start_date << " end " << row.end_date);
            CHECK(row.end_date > row.start_date);
            CHECK_FALSE(row.moment_type.empty());
            CHECK(row.end_date > row.start_date);
        } else {
            REQUIRE(batch.fx_accumulator_instruments.size() == 1u);
            const auto& row = batch.fx_accumulator_instruments.front();
            CHECK_FALSE(row.underlying_code.empty());
            CHECK(row.knock_out_barrier.has_value());
        }
    }
}

TEST_CASE("a_scope_with_no_anchor_is_refused", "[trade_population]") {
    auto scope = fx_scope();
    scope.as_of_date = std::chrono::year_month_day{};
    CHECK_THROWS_AS(plan_of(scope), trade_population_error);
}

TEST_CASE("a month_end_anchor_still_dates_every_fx_trade", "[trade_population]") {
    /*
     * 31 October shifted back a month states 31 September, which is not a date.
     * The planner clamps it, so every row it states is a date the store
     * accepts.
     */
    auto scope = fx_scope();
    scope.as_of_date = std::chrono::year{2026} / std::chrono::October / 31;
    const auto plan = plan_of(scope);
    for (const auto& slot : plan.slots) {
        const auto& rows = slot.instruments.fx_variance_swap_instruments;
        for (const auto& row : rows) {
            CHECK(row.start_date.ok());
            CHECK(row.end_date.ok());
            CHECK(row.end_date > row.start_date);
        }
    }
}

TEST_CASE("every_routed_fx_type_appears_in_a_population_of_that_length", "[trade_population]") {
    auto scope = fx_scope();
    scope.trades_per_book = 26;
    const auto plan = plan_of(scope);

    CHECK(codes_of(plan).size() == 26);
}

TEST_CASE("an_fx_config_that_states_one_slot_per_routed_code_draws_every_code",
          "[trade_population]") {
    /*
     * The fx planner cycles twenty-six routed codes. A config of eight slots never
     * reaches the variance swap or the accumulator, which is what a tenant held
     * before the seeded configs stated one trade per code.
     */
    auto wide = fx_scope();
    wide.trades_per_book = 64;
    const auto routed = codes_of(plan_of(wide));
    REQUIRE(routed.size() == 26);

    auto seeded = fx_scope();
    seeded.book_ids = {bond_book};
    seeded.trades_per_book = static_cast<std::uint32_t>(routed.size());

    const auto plan = plan_of(seeded);
    REQUIRE(static_cast<std::size_t>(std::ranges::count_if(
                plan.slots, [](const auto& s) { return !is_scripted_twin(s); })) == routed.size());
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

TEST_CASE("a_basket_in_a_book_whose_currency_no_second_pair_quotes_is_still_planned",
          "[trade_population]") {
    /*
     * The UK book's currency is GBP, and EUR/GBP is the only pair quoted in it,
     * so a slot of a product on two pairs that drew EUR/GBP had no partner and
     * refused the whole population. Such a slot now draws from the pairs that
     * have one.
     */
    auto scope = fx_scope();
    scope.currency_code = "GBP";
    scope.currency_pairs = {{.base = "EUR", .quote = "GBP"},
                            {.base = "EUR", .quote = "USD"},
                            {.base = "GBP", .quote = "USD"}};
    scope.trades_per_book = 64;

    domain::trade_population_plan plan;
    REQUIRE_NOTHROW(plan = plan_of(scope));
    const auto codes = codes_of(plan);
    CHECK(codes.contains("FxBasketOption"));
    CHECK(codes.contains("FxRainbowOption"));
    CHECK(codes.contains("FxBasketVarianceSwap"));
    CHECK(codes.contains("FxPairwiseVarianceSwap"));
    CHECK(codes.contains("FxWorstOfBasketSwap"));
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
            CHECK(batch.fx_asian_forward_instruments.front().fx_index.starts_with(
                "oresmd://fx/EUR?"));
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

namespace {

/// The code of the multi-asset product the batch holds, or nothing when it holds none.
std::optional<std::string>
multi_asset_type_of(const ores::trading::domain::instrument_batch& batch) {
    if (!batch.autocallable_instruments.empty())
        return batch.autocallable_instruments.front().identity.trade_type_code;
    if (!batch.double_digital_option_instruments.empty())
        return batch.double_digital_option_instruments.front().identity.trade_type_code;
    if (!batch.european_option_barrier_instruments.empty())
        return batch.european_option_barrier_instruments.front().identity.trade_type_code;
    if (!batch.performance_option_instruments.empty())
        return batch.performance_option_instruments.front().identity.trade_type_code;
    return std::nullopt;
}

}

TEST_CASE("a_scripted_population_states_a_script_and_the_variables_it_reads",
          "[trade_population]") {
    const auto scope = scripted_scope();
    const auto plan = plan_of(scope);

    std::set<std::string> types;
    std::set<std::string> observed;
    for (const auto& slot : plan.slots) {
        // A product held in the multi-asset family states its own row and its
        // underlyings, and no script variables.
        if (const auto code = multi_asset_type_of(slot.instruments)) {
            CHECK(*code == slot.trade_type_code);
            REQUIRE_FALSE(slot.instruments.instrument_underlyings.empty());
            for (const auto& underlying : slot.instruments.instrument_underlyings)
                observed.insert(underlying.name);
            types.insert(*code);
            continue;
        }
        REQUIRE(slot.instruments.scripted_instruments.size() == 1u);
        const auto& row = slot.instruments.scripted_instruments.front();
        CHECK(row.identity.trade_type_code == slot.trade_type_code);
        CHECK_FALSE(row.script_name.empty());

        const auto& parameters = slot.instruments.scripted_parameters;
        REQUIRE_FALSE(parameters.empty());
        for (std::size_t i = 0; i < parameters.size(); ++i) {
            CHECK(parameters[i].sequence_number == static_cast<int>(i) + 1);
            CHECK_FALSE(parameters[i].name.empty());
            CHECK_FALSE(parameters[i].declared_type.empty());
        }

        /*
         * A script that reads an index names it by address, so the projection
         * writes the ORE name the address resolves to. The address names an
         * equity of the tenant's catalogue rather than one the planner invents.
         */
        // A list states its addresses as value rows rather than one value.
        for (const auto& parameter : parameters) {
            if (parameter.declared_type != "Index")
                continue;
            std::vector<std::string> addresses;
            if (parameter.value)
                addresses.push_back(*parameter.value);
            for (const auto& value : slot.instruments.scripted_parameter_values)
                if (value.parameter_sequence_number == parameter.sequence_number)
                    addresses.push_back(value.value);
            INFO("parameter " << parameter.name);
            REQUIRE_FALSE(addresses.empty());
            for (const auto& address : addresses) {
                CHECK(address.starts_with("oresmd://equity/"));
                CHECK(std::ranges::any_of(scope.equity_underlyings, [&](const auto& underlying) {
                    return address.find(underlying.code) != std::string::npos;
                }));
                observed.insert(address);
            }
        }
        // A product that states Underlying blocks names them there instead.
        for (const auto& underlying : slot.instruments.instrument_underlyings)
            observed.insert(underlying.name);
        types.insert(row.identity.trade_type_code);
    }

    // The five codes the scripted family books, each through its builder.
    CHECK(types.size() == 5u);
    CHECK(types.contains("Autocallable_01"));
    CHECK(observed.size() > 1u);
}

TEST_CASE("a_scripted_product_that_states_two_underlyings_is_given_two", "[trade_population]") {
    /*
     * The double digital and the performance option state an Underlying block
     * for each name, and the engine reads one underlying for each. A trade given
     * one name exports with no second underlying, and the engine refuses it with
     * a null node, so the count decides whether the trade is priced at all.
     */
    const auto scope = scripted_scope();
    const auto plan = plan_of(scope);

    std::map<std::string, std::size_t> names_by_type;
    for (const auto& slot : plan.slots) {
        names_by_type[slot.trade_type_code] = slot.instruments.instrument_underlyings.size();
        for (const auto& underlying : slot.instruments.instrument_underlyings) {
            INFO("underlying " << underlying.name);
            CHECK(underlying.underlying_type == "Equity");
            CHECK(underlying.name.starts_with("oresmd://equity/"));
        }
    }

    CHECK(names_by_type["DoubleDigitalOption"] == 2u);
    CHECK(names_by_type["PerformanceOption_01"] == 2u);
    CHECK(names_by_type["EuropeanOptionBarrier"] == 2u);
    CHECK(names_by_type["ScriptedTrade"] == 0u);
}

TEST_CASE("a_scripted_population_that_names_no_underlying_is_refused", "[trade_population]") {
    auto scope = scripted_scope();
    scope.equity_underlyings.clear();

    CHECK_THROWS_AS(plan_of(scope), trade_population_error);
}

TEST_CASE("the_scripted_family_is_planned_by_name", "[trade_population]") {
    CHECK(planner_for("scripted").asset_class_code() == "scripted");

    const auto classes = planned_asset_classes();
    CHECK(std::ranges::find(classes, std::string_view("scripted")) != classes.end());
}

TEST_CASE("an_inflation_population_states_the_index_the_tenant_holds", "[trade_population]") {
    const auto scope = inflation_scope();
    const auto plan = plan_of(scope);

    std::set<std::string> indices;
    for (const auto& slot : plan.slots) {
        CHECK(slot.trade_type_code == "InflationSwap");
        /*
         * The code routes to the rates family's table, so the population states
         * that family's header and the product's own fact row beside it.
         */
        REQUIRE(slot.instruments.rate_instruments.size() == 1u);
        const auto& header = slot.instruments.rate_instruments.front();
        REQUIRE(header.start_date.has_value());
        REQUIRE(header.maturity_date.has_value());
        CHECK(header.maturity_date > header.start_date);

        REQUIRE(slot.instruments.inflation_swap_instruments.size() == 1u);
        const auto& fact = slot.instruments.inflation_swap_instruments.front();
        // The index is the address of one the tenant's own curves name, and
        // its lag is the one the index's convention states.
        CHECK(fact.inflation_index_code.starts_with("oresmd://inflation/"));
        CHECK_FALSE(fact.lag_convention.empty());
        REQUIRE(fact.base_cpi.has_value());
        CHECK(*fact.base_cpi > 0.0);
        CHECK(std::ranges::any_of(scope.inflation_index_codes, [&](const auto& code) {
            return fact.inflation_index_code.find("/" + code + "?") != std::string::npos;
        }));
        indices.insert(fact.inflation_index_code);

        /*
         * The document reads the index, its base level and the lag off a CPI
         * leg, so the population states one and its fixed counterpart.
         */
        REQUIRE(slot.instruments.swap_legs.size() == 2u);
        const auto cpi = std::ranges::find_if(
            slot.instruments.swap_legs, [](const auto& leg) { return leg.leg_type_code == "CPI"; });
        REQUIRE(cpi != slot.instruments.swap_legs.end());
        CHECK(cpi->floating_index_code == fact.inflation_index_code);
        CHECK_FALSE(cpi->currency.empty());
    }

    // The index cycles the tenant's curves, so every one it holds appears.
    CHECK(indices.size() == scope.inflation_index_codes.size());
}

TEST_CASE("an_inflation_population_that_names_no_curve_is_refused", "[trade_population]") {
    auto scope = inflation_scope();
    scope.inflation_index_codes.clear();

    CHECK_THROWS_AS(plan_of(scope), trade_population_error);
}

TEST_CASE("the_inflation_family_is_planned_by_name", "[trade_population]") {
    CHECK(planner_for("inflation").asset_class_code() == "inflation");

    const auto classes = planned_asset_classes();
    CHECK(std::ranges::find(classes, std::string_view("inflation")) != classes.end());
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
        REQUIRE(bond_of(slot).issuer == bond_of(found->second).issuer);
        REQUIRE(bond_of(slot).issue_date == bond_of(found->second).issue_date);
        REQUIRE(bond_of(slot).maturity_date == bond_of(found->second).maturity_date);
        REQUIRE(bond_of(slot).tenor_years == bond_of(found->second).tenor_years);
    }
}

TEST_CASE("a_forward_bond_matures_before_its_bond_does", "[trade_population]") {
    auto scope = bond_scope();
    scope.trades_per_book = 64;
    std::size_t forwards = 0;
    for (const auto& slot : plan_of(scope).slots) {
        for (const auto& forward : slot.instruments.bond_forwards) {
            REQUIRE(forward.forward_maturity_date.has_value());
            const auto bond_maturity = std::format("{:%Y-%m-%d}", bond_of(slot).maturity_date);
            INFO("forward " << *forward.forward_maturity_date << " bond " << bond_maturity);
            CHECK(*forward.forward_maturity_date < bond_maturity);
            ++forwards;
        }
    }
    CHECK(forwards > 0u);
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

TEST_CASE("a_securitys_own_rows_do_not_depend_on_the_slot_that_wrote_them", "[trade_population]") {
    /*
     * Two trades of one security state one issue, so the security's own rows are
     * a function of the security and not of the slot. A band of one term over
     * two books puts two convertibles on one security: their targets must agree,
     * or the second write reaches an issue that already states other terms and
     * the store refuses it.
     */
    auto scope = bond_scope();
    scope.min_tenor_code = "1Y";
    scope.max_tenor_code = "1Y";
    scope.trades_per_book = 20;

    const auto plan = plan_of(scope);
    std::map<std::string, std::set<std::string>> targets_of;
    for (const auto& slot : plan.slots) {
        if (!slot.bond_issue.has_value())
            continue;
        for (const auto& target : slot.bond_issue_conversion_targets)
            targets_of[slot.bond_issue->security_id].insert(
                std::format("{}:{:.2f}", target.underlying_id, target.conversion_ratio));
    }

    REQUIRE_FALSE(targets_of.empty());
    for (const auto& [security, targets] : targets_of)
        CHECK(targets.size() == 1u);
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
    const auto issuer = issuer_for(bond_scope(), bond.tenor_years).id;
    REQUIRE(security_id_for(bond_scope(), bond.tenor_years, "Bond", issuer) !=
            security_id_for(bond_scope(), bond.tenor_years, "CallableBond", issuer));
}

TEST_CASE("a_security_states_the_currency_of_the_books_that_hold_it", "[trade_population]") {
    auto euro = bond_scope();
    euro.currency_code = "EUR";
    auto dollar = bond_scope();
    dollar.currency_code = "USD";

    const auto issuer = issuer_for(euro, 5);
    REQUIRE(security_id_for(euro, 5, "Bond", issuer.id) !=
            security_id_for(dollar, 5, "Bond", issuer.id));
    REQUIRE(security_id_for(euro, 5, "Bond", issuer.id) ==
            security_id_for(euro, 5, "Bond", issuer.id));
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

    const auto issuer = issuer_for(first, 5);
    REQUIRE(security_id_for(first, 5, "Bond", issuer.id) ==
            security_id_for(second, 5, "Bond", issuer.id));
}

TEST_CASE("a_security_is_named_by_its_issuer", "[trade_population]") {
    /*
     * Each legal entity of a tenant holds its own issuer catalogue, so the same
     * currency, anchor, term and product under two issuers is two securities.
     * One name for both made the second entity's config write against the first
     * one's issue, and the store refused it as stored with other terms.
     */
    auto first = bond_scope();
    first.issuers = {{.id = bank, .name = "Bank One Plc"}};
    auto second = bond_scope();
    second.issuers = {{.id = second_bank, .name = "Bank Two Plc"}};

    const auto first_issuer = issuer_for(first, 5);
    const auto second_issuer = issuer_for(second, 5);
    REQUIRE(first_issuer.id != second_issuer.id);
    REQUIRE(security_id_for(first, 5, "Bond", first_issuer.id) !=
            security_id_for(second, 5, "Bond", second_issuer.id));
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
    std::ranges::reverse(narrow.issuers);

    std::map<std::string, std::string> issuer_of;
    for (const auto& slot : plan_of(wide).slots)
        issuer_of.emplace(bond_of(slot).security_id, bond_of(slot).issuer);

    for (const auto& slot : plan_of(narrow).slots) {
        const auto found = issuer_of.find(bond_of(slot).security_id);
        if (found != issuer_of.end())
            REQUIRE(bond_of(slot).issuer == found->second);
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
    scope.trades_per_book = 26;
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
    /* One per book: twenty-six types in the cycle, two books in the scope. */
    CHECK(swaps == 2);
}

TEST_CASE("a_config_that_states_trade_types_books_only_those", "[trade_population]") {
    auto scope = fx_scope();
    scope.trade_type_codes = {"FxForward"};

    const auto plan = plan_of(scope);

    REQUIRE_FALSE(plan.slots.empty());
    for (const auto& slot : plan.slots)
        CHECK(slot.trade_type_code == "FxForward");
}

TEST_CASE("the_stated_trade_types_keep_the_order_the_class_cycles", "[trade_population]") {
    auto scope = bond_scope();
    scope.trade_type_codes = {"CallableBond", "Bond"};

    const auto plan = plan_of(scope);

    REQUIRE(plan.slots.size() >= 2u);
    CHECK(plan.slots[0].trade_type_code == "Bond");
    CHECK(plan.slots[1].trade_type_code == "CallableBond");
}

TEST_CASE("a_trade_type_the_class_does_not_book_is_refused", "[trade_population]") {
    auto scope = fx_scope();
    scope.trade_type_codes = {"Bond"};

    REQUIRE_THROWS_AS(plan_of(scope), trade_population_error);
}

TEST_CASE("an_equity_swap_builds_its_funding_leg_when_two_conventions_name_the_index",
          "[trade_population]") {
    auto scope = equity_scope();
    scope.trade_type_codes = {"EquitySwap"};
    name_each_swap_index_twice(scope);

    const auto plan = plan_of(scope);

    REQUIRE_FALSE(plan.slots.empty());
    CHECK(plan.slots.front().instruments.equity_swap_instruments.size() == 1u);
}

namespace {

/// A closed either-or a row states, as the column the rows name it by.
const std::set<std::string>& either_or_columns() {
    static const std::set<std::string> columns{"long_short",
                                               "option_type",
                                               "payer",
                                               "fixed_payer",
                                               "fixed_side",
                                               "protection_side",
                                               "long_in_forward",
                                               "cap_floor_kind",
                                               "buy_sell"};
    return columns;
}

/**
 * @brief The either-ors a product fixes by its structure rather than draws.
 *
 * A bond's legs are the security's coupons, which the holder always receives;
 * a cap, a floor or an option strip states its direction on the option, not on
 * its leg; and the average forward's long_short is a label ORE does not read,
 * whose direction is fixed_payer. The list is closed in both directions.
 */
const std::set<std::string>& structural_either_ors() {
    static const std::set<std::string> columns{
        "Ascot bond_legs.payer",
        "BondRepo bond_legs.payer",
        "BondTRS bond_legs.payer",
        "CapFloor swap_legs.payer",
        "CommodityOptionStrip swap_legs.payer",
        "FxAverageForward fx_asian_forward_instruments.long_short"};
    return columns;
}

template <typename T>
std::optional<std::string> spelled(const T& value) {
    if constexpr (std::is_same_v<T, std::string>)
        return value.empty() ? std::nullopt : std::optional<std::string>(value);
    else if constexpr (std::is_same_v<T, bool>)
        return std::string(value ? "true" : "false");
    else if constexpr (requires {
                           value.has_value();
                           *value;
                       })
        return value ? spelled(*value) : std::nullopt;
    else
        return std::nullopt;
}

/// Records the either-or columns one row states, under its type and array.
template <typename Row>
void collect_either_ors(const Row& row,
                        const std::string& prefix,
                        std::map<std::string, std::set<std::string>>& seen) {
    if constexpr (requires { wire_fields(std::type_identity<Row>{}); }) {
        const auto one = [&](const auto& field) {
            const std::string name(field.name);
            if (!either_or_columns().contains(name))
                return;
            if (const auto v = spelled(row.*(field.member)))
                seen[prefix + name].insert(*v);
        };
        ores::utility::codec::apply_fields([&](const auto&... fields) { (one(fields), ...); },
                                           wire_fields(std::type_identity<Row>{}));
    }
}

/**
 * @brief The values each (trade type, either-or column) takes over a plan.
 */
std::map<std::string, std::set<std::string>>
either_or_values(const domain::trade_population_plan& plan) {
    std::map<std::string, std::set<std::string>> seen;
    for (const auto& slot : plan.slots) {
        const auto one = [&](const auto& array) {
            const auto prefix = slot.trade_type_code + " " + std::string(array.name) + ".";
            for (const auto& row : slot.instruments.*(array.member))
                collect_either_ors(row, prefix, seen);
        };
        ores::utility::codec::apply_fields(
            [&](const auto&... arrays) { (one(arrays), ...); },
            wire_fields(std::type_identity<ores::trading::domain::instrument_batch>{}));
    }
    return seen;
}

std::vector<domain::trade_generation_scope> every_class_scope() {
    std::vector<domain::trade_generation_scope> scopes{bond_scope(),
                                                       rate_scope(),
                                                       scripted_scope(),
                                                       inflation_scope(),
                                                       fx_scope(),
                                                       commodity_scope(),
                                                       equity_scope(),
                                                       credit_scope()};
    // About eight trades of each type a book, so a drawn either-or reaches both of its ways.
    for (auto& scope : scopes)
        scope.trades_per_book = 256;
    return scopes;
}

}

TEST_CASE("every_trade_type_reaches_every_book", "[trade_population]") {
    std::set<std::string> missing;
    for (const auto& scope : every_class_scope()) {
        const auto plan = plan_of(scope);
        std::map<std::string, std::set<std::string>> books;
        for (const auto& slot : plan.slots)
            books[slot.trade_type_code].insert(boost::uuids::to_string(slot.book_id));
        for (const auto& [type, reached] : books)
            if (reached.size() != scope.book_ids.size())
                missing.insert(type);
    }
    std::string listed;
    for (const auto& type : missing)
        listed += "\n  " + type;
    if (!missing.empty())
        FAIL_CHECK("these trade types do not reach every book:" + listed);
}

TEST_CASE("every_closed_either_or_is_drawn_both_ways", "[trade_population]") {
    std::set<std::string> one_way;
    std::set<std::string> structural_seen;
    for (const auto& scope : every_class_scope())
        for (const auto& [column, values] : either_or_values(plan_of(scope))) {
            if (structural_either_ors().contains(column)) {
                structural_seen.insert(column);
                continue;
            }
            if (values.size() < 2)
                one_way.insert(column + " = " + *values.begin());
        }
    std::string listed;
    for (const auto& column : one_way)
        listed += "\n  " + column;
    if (!one_way.empty())
        FAIL_CHECK("these either-ors are drawn one way only:" + listed);
    CHECK(structural_seen == structural_either_ors());
}

TEST_CASE("a_bond_issue_names_its_issuer_by_its_legal_name", "[trade_population]") {
    const auto plan = plan_of(bond_scope());
    REQUIRE_FALSE(plan.slots.empty());
    for (const auto& slot : plan.slots) {
        REQUIRE(slot.bond_issue.has_value());
        INFO(slot.trade_type_code);
        CHECK((slot.bond_issue->issuer == "Bank One Plc" ||
               slot.bond_issue->issuer == "Bank Two Plc"));
    }
}

TEST_CASE("a_cap_floor_population_states_every_kind_with_every_position", "[trade_population]") {
    /*
     * The kind and the position are two either-ors of one trade. Picked from
     * one stream they move together, and every cap would be long and every
     * floor short; each reads its own, so all four pairs occur.
     */
    auto scope = rate_scope();
    scope.trade_type_codes = {"CapFloor"};
    scope.trades_per_book = 16;

    std::set<std::pair<std::string, std::string>> pairs;
    for (const auto& slot : plan_of(scope).slots) {
        const auto& instruments = slot.instruments;
        REQUIRE(instruments.cap_floor_instruments.size() == 1u);
        REQUIRE(instruments.instrument_strikes.size() == 1u);
        pairs.emplace(instruments.instrument_strikes.front().strike_role,
                      instruments.cap_floor_instruments.front().long_short.value_or(""));
    }
    CHECK(pairs.size() == 4u);
}

TEST_CASE("a_product_with_a_scripted_form_is_booked_again_as_its_scripted_twin",
          "[trade_population]") {
    auto scope = fx_scope();
    scope.trades_per_book = 64;
    const auto plan = plan_of(scope);

    const auto native = std::ranges::find_if(
        plan.slots, [](const auto& s) { return s.trade_type_code == "FxBasketOption"; });
    REQUIRE(native != plan.slots.end());
    const auto twin = std::ranges::find_if(plan.slots, [&](const auto& s) {
        return s.internal_identifier == native->internal_identifier + "-scripted";
    });
    REQUIRE(twin != plan.slots.end());
    CHECK(twin->trade_type_code == "ScriptedTrade");
    CHECK(twin->trade_id != native->trade_id);
    CHECK(twin->book_id == native->book_id);

    REQUIRE(twin->instruments.scripted_instruments.size() == 1u);
    CHECK(twin->instruments.scripted_instruments[0].script_name == "VanillaBasketOption");
    const auto& parameters = twin->instruments.scripted_parameters;
    const auto value_of = [&](const char* name) -> std::string {
        for (const auto& p : parameters)
            if (p.name == name)
                return p.value.value_or("");
        return "<absent>";
    };
    // The twin states the native trade's own terms, read from its rows.
    REQUIRE(native->instruments.fx_basket_option_instruments.size() == 1u);
    const auto& basket = native->instruments.fx_basket_option_instruments[0];
    REQUIRE(basket.strike.has_value());
    CHECK(value_of("Strike") == basket.strike->to_string());
    CHECK(value_of("Notional") == basket.notional.to_string());
    CHECK(value_of("PayCcy") == basket.currency);
    REQUIRE(native->instruments.instrument_options.size() == 1u);
    CHECK(value_of("LongShort") ==
          (native->instruments.instrument_options[0].long_short == "Long" ? "1" : "-1"));
    // The basket's members are the twin's underlyings, in the native order.
    std::vector<std::string> members;
    for (const auto& u : native->instruments.instrument_underlyings)
        members.push_back(u.name);
    const auto list =
        std::ranges::find_if(parameters, [](const auto& p) { return p.name == "Underlyings"; });
    REQUIRE(list != parameters.end());
    std::vector<std::string> stated;
    for (const auto& value : twin->instruments.scripted_parameter_values)
        if (value.parameter_sequence_number == list->sequence_number)
            stated.push_back(value.value);
    CHECK(stated == members);
}
