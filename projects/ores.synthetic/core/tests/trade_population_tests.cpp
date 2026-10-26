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
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <variant>

// The generator's whole point is that a second provision states the same
// population: no trade duplicated and no version advanced. These cases drive
// the run against a writer that records what it was asked to do, so the
// convergence is asserted on the generator's own answer rather than on a
// database's, and a real provisioning is the same assertion against real rows.

namespace {

const std::string tags("[trade_population]");

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
const domain::bond_terms& bond_of(const domain::trade_population_slot& slot) {
    return std::get<domain::bond_terms>(slot.terms);
}

const domain::rate_terms& rate_of(const domain::trade_population_slot& slot) {
    return std::get<domain::rate_terms>(slot.terms);
}

const domain::fx_terms& fx_of(const domain::trade_population_slot& slot) {
    return std::get<domain::fx_terms>(slot.terms);
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

TEST_CASE("the_population_size_is_the_configs_and_not_the_generators", tags) {
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

TEST_CASE("every_commodity_trade_type_the_catalogue_routes_is_planned", tags) {
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

TEST_CASE("a_commodity_slot_states_its_own_terms_and_names_a_catalogue_commodity", tags) {
    const auto plan = plan_of(commodity_scope());

    std::set<std::string> commodities;
    for (const auto& slot : plan.slots) {
        REQUIRE(std::holds_alternative<domain::commodity_terms>(slot.terms));
        const auto& terms = std::get<domain::commodity_terms>(slot.terms);
        REQUIRE_FALSE(terms.commodity_code.empty());
        REQUIRE_FALSE(terms.unit.empty());
        commodities.insert(terms.commodity_code);
    }

    // Every commodity the scope states is drawn, and no other is invented.
    REQUIRE(commodities.size() == 2u);
    for (const auto& code : commodities)
        REQUIRE((code == "NYMEX:CL" || code == "NYMEX:NG"));
}

TEST_CASE("a_commodity_population_with_no_catalogue_is_refused", tags) {
    auto empty = commodity_scope();
    empty.commodities.clear();

    CHECK_THROWS_WITH(plan_of(empty), Catch::Matchers::ContainsSubstring("catalogue"));
}

TEST_CASE("a_bond_slot_still_states_bond_terms", tags) {
    const auto plan = plan_of(bond_scope());

    for (const auto& slot : plan.slots)
        REQUIRE(std::holds_alternative<domain::bond_terms>(slot.terms));
}

TEST_CASE("a_bond_scope_states_one_trade_per_slot_across_its_books", tags) {
    const auto plan = plan_of(bond_scope());

    REQUIRE(plan.slots.size() == scene_trades_per_book * 2);
    REQUIRE(plan.slots.front().trade_type_code == "Bond");
    REQUIRE(plan.slots.front().book_id == bond_book);
    REQUIRE(plan.slots[1].book_id == second_book);
    REQUIRE(plan.slots.front().currency == "EUR");
}

TEST_CASE("the_same_scope_states_the_same_population_twice", tags) {
    const auto once = plan_of(bond_scope());
    const auto twice = plan_of(bond_scope());

    REQUIRE(once.slots.size() == twice.slots.size());
    for (std::size_t i = 0; i < once.slots.size(); ++i) {
        REQUIRE(once.slots[i].trade_id == twice.slots[i].trade_id);
        REQUIRE(once.slots[i].internal_identifier == twice.slots[i].internal_identifier);
        REQUIRE(bond_of(once.slots[i]).security_id == bond_of(twice.slots[i]).security_id);
        REQUIRE(once.slots[i].notional == twice.slots[i].notional);
        REQUIRE(once.slots[i].trade_date == twice.slots[i].trade_date);
        REQUIRE(bond_of(once.slots[i]).maturity_date == bond_of(twice.slots[i]).maturity_date);
    }
}

TEST_CASE("a_different_seed_states_a_different_population", tags) {
    auto other = bond_scope();
    other.seed = 8;

    const auto once = plan_of(bond_scope());
    const auto twice = plan_of(other);

    REQUIRE(once.slots.front().trade_id != twice.slots.front().trade_id);
}

TEST_CASE("every_trade_date_derives_from_the_as_of_anchor", tags) {
    auto earlier = bond_scope();
    earlier.as_of_date = std::chrono::year{2026} / 1 / 2;
    auto later = bond_scope();
    later.as_of_date = std::chrono::year{2027} / 1 / 2;

    const auto first = plan_of(earlier);
    const auto second = plan_of(later);

    REQUIRE(first.slots.front().trade_date < second.slots.front().trade_date);

    REQUIRE(bond_of(first.slots.front()).maturity_date <
            bond_of(second.slots.front()).maturity_date);
}

TEST_CASE("a_tenor_or_notional_band_that_does_not_order_is_refused", tags) {
    auto inverted_tenor = bond_scope();
    inverted_tenor.min_tenor_code = "10Y";
    inverted_tenor.max_tenor_code = "1Y";
    CHECK_THROWS_AS(plan_of(inverted_tenor), trade_population_error);

    auto inverted_notional = bond_scope();
    inverted_notional.min_notional = 10000000.0;
    inverted_notional.max_notional = 100000.0;
    CHECK_THROWS_AS(plan_of(inverted_notional), trade_population_error);
}

TEST_CASE("a_scope_naming_no_book_counterparty_or_trader_is_refused", tags) {
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

TEST_CASE("a_family_the_generator_does_not_cover_is_refused_by_name", tags) {
    // Equity has no planner, so it is the class the registry refuses by name.
    auto equity = bond_scope();
    equity.asset_class_code = "equity";

    CHECK_THROWS_WITH(plan_of(equity), Catch::Matchers::ContainsSubstring("equity"));
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

TEST_CASE("the_envelope_of_a_bond_plan_is_the_one_that_was_agreed", tags) {
    // Recorded from the planner as it stood before the envelope was extracted,
    // so this case fails if the extraction changed anything a slot states.
    CHECK(envelope_hash(render_envelope(plan_of(bond_scope()))) == 9368398884618711992ULL);
}

TEST_CASE("a_rate_population_states_two_legs_of_one_currency", tags) {
    const auto scope = rate_scope();
    const auto plan = plan_of(scope);

    REQUIRE(!plan.slots.empty());
    for (const auto& slot : plan.slots) {
        const auto& terms = rate_of(slot);
        REQUIRE(terms.start_date < terms.maturity_date);
        CHECK(terms.fixed_rate > 0.0);
        CHECK(terms.fixed_rate < 0.1);
        CHECK_FALSE(terms.floating_index_code.empty());
        CHECK(terms.notional >= scope.min_notional);
        CHECK(terms.notional <= scope.max_notional);
        if (slot.trade_type_code == "CrossCurrencySwap") {
            // Two legs of one currency are not a cross currency swap, so the
            // floating side states a currency the book does not.
            CHECK_FALSE(terms.floating_currency.empty());
            CHECK(terms.floating_currency != scope.currency_code);
        } else {
            CHECK(terms.floating_currency.empty());
        }
        if (slot.trade_type_code == "Swaption") {
            REQUIRE(terms.expiry_date.has_value());
            CHECK(*terms.expiry_date > scope.as_of_date);
        }
    }
}

TEST_CASE("every_routed_rate_type_appears_in_a_population_of_that_length", tags) {
    const auto scope = rate_scope();
    const auto plan = plan_of(scope);

    std::set<std::string> types;
    for (const auto& slot : plan.slots)
        types.insert(slot.trade_type_code);

    // The nine the router sends to rate_instrument, less the inflation swap.
    CHECK(types.size() == 9);
}

TEST_CASE("a_rate_population_without_an_index_catalogue_is_refused", tags) {
    auto scope = rate_scope();
    scope.floating_index_codes.clear();

    CHECK_THROWS_AS(plan_of(scope), trade_population_error);
}

TEST_CASE("an_fx_population_states_the_idiom_its_type_needs", tags) {
    const auto scope = fx_scope();
    const auto plan = plan_of(scope);

    const auto bought_and_sold = [](const std::string& type) {
        return type == "FxForward" || type == "FxSwap" || type == "FxOption" ||
               type.starts_with("FxBarrier") || type == "FxDoubleBarrierOption" ||
               type == "FxEuropeanBarrierOption" || type == "FxGenericBarrierOption" ||
               type == "FxKIKOBarrierOption";
    };
    const auto digital = [](const std::string& type) {
        return type == "FxDigitalOption" || type == "FxTouchOption" ||
               type == "FxDoubleTouchOption" || type == "FxDigitalBarrierOption";
    };

    REQUIRE(!plan.slots.empty());
    for (const auto& slot : plan.slots) {
        const auto& terms = fx_of(slot);
        const auto& type = slot.trade_type_code;
        if (bought_and_sold(type)) {
            CHECK_FALSE(terms.bought_currency.empty());
            CHECK_FALSE(terms.sold_currency.empty());
            CHECK(terms.bought_amount > 0.0);
        } else if (digital(type)) {
            CHECK_FALSE(terms.foreign_currency.empty());
            CHECK_FALSE(terms.payoff_currency.empty());
            REQUIRE(terms.payoff_amount.has_value());
        } else if (type == "FxAverageForward" || type == "FxTaRF") {
            // The average forward and the target redemption forward are
            // written on an index, not on a named underlying.
            CHECK_FALSE(terms.fx_index.empty());
            CHECK(terms.fixing_amount > 0.0);
        } else {
            CHECK_FALSE(terms.underlying_code.empty());
            CHECK(terms.fixing_amount > 0.0);
        }
    }
}

TEST_CASE("every_routed_fx_type_appears_in_a_population_of_that_length", tags) {
    const auto plan = plan_of(fx_scope());

    std::set<std::string> types;
    for (const auto& slot : plan.slots)
        types.insert(slot.trade_type_code);

    CHECK(types.size() == 16);
}

TEST_CASE("an_fx_population_prefers_the_pairs_that_name_the_book_currency", tags) {
    const auto scope = fx_scope();
    const auto plan = plan_of(scope);

    // EUR/USD names the book's currency and GBP/USD does not, so every slot
    // takes the first.
    for (const auto& slot : plan.slots) {
        const auto& terms = fx_of(slot);
        if (!terms.bought_currency.empty()) {
            CHECK(terms.bought_currency == "EUR");
        } else if (!terms.reference_currency.empty()) {
            CHECK(terms.reference_currency == "EUR");
        }
    }
}

TEST_CASE("an_fx_population_without_a_pair_catalogue_is_refused", tags) {
    auto scope = fx_scope();
    scope.currency_pairs.clear();

    CHECK_THROWS_AS(plan_of(scope), trade_population_error);
}

TEST_CASE("the_notional_of_every_slot_sits_in_the_band", tags) {
    const auto scope = bond_scope();
    const auto plan = plan_of(scope);

    for (const auto& slot : plan.slots) {
        REQUIRE(slot.notional >= scope.min_notional);
        REQUIRE(slot.notional <= scope.max_notional);
        REQUIRE(bond_of(slot).tenor_years >= 1);
        REQUIRE(bond_of(slot).tenor_years <= 10);
    }
}

TEST_CASE("every_trade_of_a_security_states_the_terms_of_that_security", tags) {
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

TEST_CASE("a_securitys_maturity_is_its_issue_date_plus_its_term", tags) {
    const auto plan = plan_of(bond_scope());

    for (const auto& slot : plan.slots) {
        REQUIRE(bond_of(slot).maturity_date ==
                bond_of(slot).issue_date +
                    std::chrono::years{static_cast<int>(bond_of(slot).tenor_years)});
        REQUIRE(slot.trade_date >= bond_of(slot).issue_date);
        REQUIRE(slot.trade_date < bond_of(slot).maturity_date);
    }
}

TEST_CASE("the_tenor_band_reaches_the_maturities_of_the_securities", tags) {
    const auto plan = plan_of(bond_scope());

    std::set<std::string> securities;
    std::set<std::uint32_t> terms;
    for (const auto& slot : plan.slots) {
        securities.insert(bond_of(slot).security_id);
        terms.insert(bond_of(slot).tenor_years);
    }

    REQUIRE(securities.size() == terms.size());
    REQUIRE(terms.size() > 1);
}

TEST_CASE("a_security_states_the_currency_of_the_books_that_hold_it", tags) {
    auto euro = bond_scope();
    euro.currency_code = "EUR";
    auto dollar = bond_scope();
    dollar.currency_code = "USD";

    REQUIRE(security_id_for(euro, 5) != security_id_for(dollar, 5));
    REQUIRE(security_id_for(euro, 5) == security_id_for(euro, 5));
}

TEST_CASE("a_security_is_not_tied_to_a_book", tags) {
    const auto plan = plan_of(bond_scope());

    std::map<std::string, std::set<boost::uuids::uuid>> books_of;
    for (const auto& slot : plan.slots)
        books_of[bond_of(slot).security_id].insert(slot.book_id);

    const auto shared =
        std::ranges::any_of(books_of, [](const auto& entry) { return entry.second.size() > 1; });
    REQUIRE(shared);
}

TEST_CASE("two_runs_over_one_scope_write_nothing_the_second_time", tags) {
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

TEST_CASE("a_security_has_the_same_issuer_whatever_counterparties_a_config_scopes", tags) {
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
