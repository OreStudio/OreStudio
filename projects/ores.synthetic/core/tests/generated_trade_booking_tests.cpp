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
#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/generators/book_generator.hpp"
#include "ores.refdata.api/generators/counterparty_generator.hpp"
#include "ores.refdata.api/generators/currency_generator.hpp"
#include "ores.refdata.api/generators/equity_underlying_generator.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.api/generators/portfolio_generator.hpp"
#include "ores.refdata.core/repository/book_repository.hpp"
#include "ores.refdata.core/repository/counterparty_repository.hpp"
#include "ores.refdata.core/repository/currency_repository.hpp"
#include "ores.refdata.core/repository/equity_underlying_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.refdata.core/repository/portfolio_repository.hpp"
#include "ores.synthetic.core/service/trade_population_planner.hpp"
#include "ores.synthetic.core/trading/trading_trade_population_writer.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.trading.api/domain/bond_instrument_rows.hpp"
#include "ores.trading.core/service/trade_export_service.hpp"
#include "ores.trading.core/service/trade_operations_service.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "test_suite.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <set>
#include <string>
#include <type_traits>
#include <vector>

namespace {

using ores::synthetic::domain::trade_generation_scope;
using ores::synthetic::domain::trade_population_slot;
using ores::synthetic::tests::trade_generation_scope_recipe;
using ores::synthetic::service::config_id_for;
using ores::synthetic::service::planned_asset_classes;
using ores::synthetic::service::planner_for;
using ores::synthetic::service::trading_trade_population_writer;
using ores::trading::messaging::book_trade_request;
using ores::trading::messaging::put_bond_issue_composite_request;
using ores::trading::service::book_rules_port;
using ores::trading::service::trade_operations_service;
using ores::utility::domain::outcome;

const std::string tags("[service][integration]");

/// The name of the config one asset class is drawn from.
std::string booking_config_name(const std::string& asset_class_code) {
    return "generated_trade_booking:" + asset_class_code;
}

/// A port that lets every booking through, so the case tests the store and not
/// the book's own rules.
class open_books final : public book_rules_port {
public:
    [[nodiscard]] answer
    of(const boost::uuids::uuid&, std::string_view, std::string_view) const override {
        return {book_rules_port::status::open, book_rules_port::product_type::allows};
    }
};

/**
 * @brief A tenant with a party, a book and one account to write as.
 *
 * The trade's audit columns name the person who entered it, and the store
 * refuses a name that is not an account of the tenant, so the population is
 * drawn as one real account.
 */
struct fixture final {
    ores::testing::scoped_database_helper h;
    ores::utility::generation::generation_context gen = ores::testing::make_generation_context(h);
    ores::database::context ctx = h.context();
    boost::uuids::uuid account_id{};
    std::string username;
    boost::uuids::uuid party_id{};
    /// The trade's counterparty: a refdata row of its own, not a party.
    boost::uuids::uuid counterparty_id{};
    boost::uuids::uuid book_id{};
    std::string currency_code = "EUR";

    fixture() {
        namespace gen_rd = ores::refdata::generators;
        namespace repo = ores::refdata::repository;

        auto maker = ores::iam::generators::generate_synthetic_account(gen);
        maker.account_type = "service";
        maker.change_reason_code = "system.test";
        ores::iam::repository::account_repository{}.write(h.context(), maker);
        account_id = maker.id;
        username = maker.username;

        auto party = gen_rd::generate_synthetic_party(gen);
        party.change_reason_code = "system.test";
        for (const auto& existing : repo::party_repository().read_latest(h.context())) {
            if (existing.tenant_id == party.tenant_id) {
                party.parent_party_id = existing.id;
                break;
            }
        }
        repo::party_repository().write(h.context(), party);
        party_id = party.id;
        ctx = h.context().with_party(h.tenant_id(), party.id, {party.id}, maker.username);

        // The store holds a trade's counterparty to the counterparties table,
        // so the population trades with one that exists there.
        auto counterparty = gen_rd::generate_synthetic_counterparty(gen);
        repo::counterparty_repository().write(ctx, counterparty);
        counterparty_id = counterparty.id;

        // The conventions the population draws on name indices in EUR, so the
        // book the trades land in is kept in EUR as well.
        auto currency = gen_rd::generate_synthetic_currency(gen);
        currency.iso_code = currency_code;
        repo::currency_repository().write(ctx, {currency});

        auto portfolio = gen_rd::generate_synthetic_portfolio(gen);
        portfolio.party_id = party.id;
        portfolio.aggregation_ccy = currency_code;
        portfolio.change_reason_code = "system.test";
        repo::portfolio_repository().write(ctx, portfolio);

        auto book = gen_rd::generate_synthetic_book(gen);
        book.party_id = party.id;
        book.parent_portfolio_id = portfolio.id;
        book.functional_currency = currency_code;
        book.change_reason_code = "system.test";
        repo::book_repository().write(ctx, book);
        book_id = book.id;

        // The scope names the equity underlyings its trades draw on, and the
        // store refuses a basket constituent whose code the catalogue lacks, so
        // the fixture writes the vocabulary its own scope states.
        std::vector<ores::refdata::domain::equity_underlying> underlyings;
        for (const auto& reference : trade_generation_scope_recipe().equity_underlyings) {
            auto underlying = gen_rd::generate_synthetic_equity_underlying(gen);
            underlying.code = reference.code;
            underlying.name = reference.code;
            underlying.description = "The equity underlying the booking scope names.";
            underlying.instrument_type = reference.kind;
            underlying.currency_code = reference.currency;
            underlying.change_reason_code = "system.test";
            underlyings.push_back(underlying);
        }
        repo::equity_underlying_repository{}.write(ctx, underlyings);
    }
};

/**
 * @brief The scope of one asset class, drawn so every code it cycles appears once.
 *
 * A planner draws its trade type by the slot's position, so a population of the
 * class's own code count states each code exactly once.
 */
trade_generation_scope booking_scope_of(const fixture& f, const std::string& asset_class_code) {
    auto scope = trade_generation_scope_recipe();
    // A config states one asset class, so each class is drawn from its own:
    // the trades are named by their config, and two classes on one config
    // would book the same trade id twice.
    scope.config_id = config_id_for(f.party_id, booking_config_name(asset_class_code));
    scope.party_id = f.party_id;
    scope.asset_class_code = asset_class_code;
    scope.trades_per_book = static_cast<std::uint32_t>(planner_for(asset_class_code).code_count());
    scope.book_ids = {f.book_id};
    scope.counterparty_ids = {f.counterparty_id};
    scope.issuers = {{.id = f.counterparty_id, .name = "Booking Bank Plc"}};
    scope.traders = {ores::synthetic::domain::trade_population_trader{f.account_id, f.username}};
    return scope;
}

/// Writes the security a bond trade is booked against, as the writer does.
///
/// A bond states its issue before its trade, and the issue's own rows belong to
/// the security rather than to the trade, so they are written here and not in
/// the booking request.
void write_issue(const trade_population_slot& slot,
                 trade_operations_service& books,
                 const std::string& asset_class) {
    if (!slot.bond_issue)
        return;
    put_bond_issue_composite_request request;
    request.issue = *slot.bond_issue;
    request.legs = slot.bond_issue_legs;
    request.amounts = slot.bond_issue_leg_amounts;
    request.rates = slot.bond_issue_leg_rates;
    request.schedules = slot.bond_issue_leg_schedules;
    request.call_dates = slot.bond_issue_call_dates;
    request.conversion_targets = slot.bond_issue_conversion_targets;
    const auto response = books.put_bond_issue_composite(request);
    INFO("asset class " << asset_class << ", trade type " << slot.trade_type_code
                        << ": the issue was refused: " << response.result.message);
    CHECK(response.result.outcome == outcome::ok);
}

/// Resets a member to its default. A member with no default fails to compile.
template <typename T>
void reset(T& member) {
    if constexpr (std::is_same_v<T, ores::utility::uuid::tenant_id>)
        member = ores::utility::uuid::tenant_id::system();
    else
        member = T{};
}

/**
 * @brief Clears what the store, not the trade, states on a row: its keys, its
 * version and its audit. What is left is what the trade's terms say.
 */
template <typename Row>
void normalise(Row& row) {
    if constexpr (requires { row.instrument; }) {
        normalise(row.instrument);
        // A bond's issue is the security's row, written before the trade and
        // read back whole, so the trade's own terms are the header.
        if constexpr (requires { row.issue; })
            reset(row.issue);
    }
    if constexpr (requires { row.identity; })
        normalise(row.identity);
    if constexpr (requires { row.audit; })
        reset(row.audit);
    if constexpr (requires { row.version; })
        reset(row.version);
    if constexpr (requires { row.tenant_id; })
        reset(row.tenant_id);
    if constexpr (requires { row.party_id; })
        reset(row.party_id);
    if constexpr (requires { row.trade_id; })
        reset(row.trade_id);
    if constexpr (requires { row.trade_activity_id; })
        reset(row.trade_activity_id);
    if constexpr (requires { row.modified_by; })
        reset(row.modified_by);
    if constexpr (requires { row.performed_by; })
        reset(row.performed_by);
    if constexpr (requires { row.change_reason_code; })
        reset(row.change_reason_code);
    if constexpr (requires { row.change_commentary; })
        reset(row.change_commentary);
    if constexpr (requires { row.recorded_at; })
        reset(row.recorded_at);
}

/// The trade a row belongs to, or nothing for a row keyed by something else.
template <typename Row>
std::optional<boost::uuids::uuid> trade_of(const Row& row) {
    if constexpr (requires { row.instrument.identity.trade_id; })
        return row.instrument.identity.trade_id;
    else if constexpr (requires { row.identity.trade_id; })
        return row.identity.trade_id;
    else if constexpr (requires { row.trade_id; })
        return row.trade_id;
    else
        return std::nullopt;
}

/// The names of the fields that differ between two rows.
template <typename Row>
std::vector<std::string> differing_fields(const Row& a, const Row& b) {
    std::vector<std::string> out;
    const auto one = [&](const auto& field) {
        const auto& x = a.*(field.member);
        const auto& y = b.*(field.member);
        if (x == y)
            return;
        using Member = std::remove_cvref_t<decltype(x)>;
        const std::string name(field.name);
        if constexpr (requires { wire_fields(std::type_identity<Member>{}); }) {
            for (const auto& inner : differing_fields(x, y))
                out.push_back(name + "." + inner);
        } else if constexpr (std::is_same_v<Member, std::string>) {
            out.push_back(name + "=" + x + "|" + y);
        } else if constexpr (std::is_arithmetic_v<Member>) {
            out.push_back(name + "=" + std::to_string(x) + "|" + std::to_string(y));
        } else {
            out.push_back(name);
        }
    };
    ores::utility::codec::apply_fields([&](const auto&... fields) { (one(fields), ...); },
                                       wire_fields(std::type_identity<Row>{}));
    return out;
}

/**
 * @brief For each booked row with no equal row read back, the fields that
 * differ from the closest row read back.
 */
template <typename Row>
std::string describe_mismatch(const std::vector<Row>& expected, const std::vector<Row>& actual) {
    std::string out;
    for (const auto& row : expected) {
        if (actual.empty() || std::ranges::find(actual, row) != actual.end())
            continue;
        std::vector<std::string> closest;
        for (const auto& candidate : actual) {
            auto fields = differing_fields(row, candidate);
            if (closest.empty() || fields.size() < closest.size())
                closest = std::move(fields);
        }
        out += " [";
        for (std::size_t i = 0; i < closest.size(); ++i)
            out += (i == 0 ? "" : ",") + closest[i];
        out += "]";
    }
    return out;
}

/**
 * @brief Names each array of the batch whose rows for the trade differ
 * between what was booked and what the export reads back.
 */
std::vector<std::string> differing_arrays(ores::trading::domain::instrument_batch booked,
                                          ores::trading::domain::instrument_batch exported,
                                          const boost::uuids::uuid& trade_id) {
    ores::trading::domain::split_bond_rows(booked);
    ores::trading::domain::split_bond_rows(exported);
    std::vector<std::string> out;
    ores::utility::codec::apply_fields(
        [&](const auto&... fields) {
            (
                [&] {
                    auto expected = booked.*(fields.member);
                    if (expected.empty() || !trade_of(expected.front()))
                        return;
                    auto actual = exported.*(fields.member);
                    std::erase_if(actual,
                                  [&](const auto& row) { return trade_of(row) != trade_id; });
                    for (auto& row : expected)
                        normalise(row);
                    for (auto& row : actual)
                        normalise(row);
                    if (expected.size() != actual.size() ||
                        !std::is_permutation(expected.begin(), expected.end(), actual.begin()))
                        out.push_back(std::string(fields.name) + " (booked " +
                                      std::to_string(expected.size()) + ", read " +
                                      std::to_string(actual.size()) + ")" +
                                      describe_mismatch(expected, actual));
                }(),
                ...);
        },
        wire_fields(std::type_identity<ores::trading::domain::instrument_batch>{}));
    return out;
}

/**
 * @brief Trade types whose export does not yet give back every row they were
 * booked with. The list is closed in both directions: a type that stops
 * differing fails the test until it leaves the list.
 */
const std::set<std::string>& known_rebook_differences() {
    /*
     * A bond's rows live in its carrier, and the flat arrays beside it are the
     * write-time shape split_bond_rows makes. The export hands the carrier back,
     * so a bond type whose facts are bond option and bond future rows reads
     * them in the carrier and states none in the arrays. That is a shape
     * difference and not a lost value; the round trip through the projection is
     * what proves the values.
     */
    static const std::set<std::string> types{"BondFutureOption"};
    return types;
}

}

TEST_CASE("every_generated_trade_type_books_through_the_database", tags) {
    auto lg(make_logger(test_suite));
    fixture f;
    // The service holds a reference to the port, so the port outlives it.
    open_books open;
    trade_operations_service books(f.ctx, open);

    const auto asset_classes = planned_asset_classes();
    REQUIRE(!asset_classes.empty());
    BOOST_LOG_SEV(lg, info) << "Booking one trade of every type, over " << asset_classes.size()
                            << " asset classes.";

    std::vector<std::pair<std::string, book_trade_request>> booked;
    for (const auto code : asset_classes) {
        const std::string asset_class(code);
        const auto offered = planner_for(asset_class).code_count();
        // A class that states no code would book nothing and pass below, so
        // the case states that each class has types to book.
        INFO("asset class " << asset_class);
        REQUIRE(offered > 0);

        const auto scope = booking_scope_of(f, asset_class);
        const auto plan = planner_for(asset_class).plan(scope);
        // A container adds the trades it holds, and a product with a scripted
        // form adds its twin; neither takes a slot of its own.
        const auto offered_slots = std::ranges::count_if(plan.slots, [](const auto& s) {
            return s.structure_role != "sub_trade" && !s.internal_identifier.ends_with("-scripted");
        });
        REQUIRE(static_cast<std::size_t>(offered_slots) == offered);

        std::set<std::string> booked_types;
        for (const auto& slot : plan.slots) {
            INFO("asset class " << asset_class << ", trade type " << slot.trade_type_code
                                << ", slot " << slot.slot);
            write_issue(slot, books, asset_class);

            const book_trade_request request =
                trading_trade_population_writer::book_request_for(slot, scope);
            const auto response = books.book_trade(request, "");
            INFO("the database refused it: " << response.result.message);
            CHECK(response.result.outcome == outcome::ok);
            if (response.result.outcome == outcome::ok) {
                if (!slot.internal_identifier.ends_with("-scripted"))
                    booked_types.insert(slot.trade_type_code);
                booked.emplace_back(slot.trade_type_code, request);
            }
        }
        // The scope states each code the class cycles, so a type the store
        // refused is a type the population cannot book.
        INFO("asset class " << asset_class);
        CHECK(booked_types.size() == offered);
        BOOST_LOG_SEV(lg, info) << "Booked " << booked_types.size() << " type(s) of " << asset_class
                                << ".";
    }

    // Every booked trade is read back through the export and compared row by
    // row with what was booked, apart from keys, version and audit, so a
    // family whose writer or reader drops a column is named here.
    ores::trading::domain::instrument_batch exported;
    ores::trading::service::trade_export_service(f.ctx).export_books(
        {boost::uuids::to_string(f.book_id)}, 0, 100000, exported);
    std::set<std::string> differing;
    std::string detail;
    for (const auto& [type, request] : booked) {
        const auto arrays = differing_arrays(request.instruments, exported, request.anchor.id);
        if (arrays.empty())
            continue;
        if (differing.insert(type).second) {
            detail += "\n  " + type + ":";
            for (const auto& name : arrays)
                detail += " " + name;
        }
    }
    if (differing != known_rebook_differences())
        FAIL_CHECK("the types whose export differs from what was booked are not the known ones:" +
                   detail);
}

TEST_CASE("a_stress_run_of_bond_populations_writes_every_issue", tags) {
    /*
     * A bond issue once failed its composite write with std::bad_alloc, and
     * the retry drew different terms, so it could not say whether a drawn value
     * caused it. This run writes the issues of many bond populations, each with
     * its own seed, anchor and config, so a value-dependent failure has many
     * draws to show itself in.
     */
    auto lg(make_logger(test_suite));
    fixture f;
    open_books open;
    trade_operations_service books(f.ctx, open);

    std::size_t written = 0;
    std::size_t already = 0;
    std::set<std::string> refused;
    for (std::uint32_t run = 0; run < 24; ++run) {
        auto scope = booking_scope_of(f, "bond");
        scope.seed = 1000 + run;
        scope.config_id = config_id_for(f.party_id, "bond stress " + std::to_string(run));
        scope.as_of_date = std::chrono::year_month_day{std::chrono::sys_days{scope.as_of_date} -
                                                       std::chrono::days{31 * run}};
        scope.trades_per_book = 96;
        for (const auto& slot : planner_for("bond").plan(scope).slots) {
            if (!slot.bond_issue)
                continue;
            put_bond_issue_composite_request request;
            request.issue = *slot.bond_issue;
            request.legs = slot.bond_issue_legs;
            request.amounts = slot.bond_issue_leg_amounts;
            request.rates = slot.bond_issue_leg_rates;
            request.schedules = slot.bond_issue_leg_schedules;
            request.call_dates = slot.bond_issue_call_dates;
            request.conversion_targets = slot.bond_issue_conversion_targets;
            const auto response = books.put_bond_issue_composite(request);
            if (response.result.outcome == outcome::ok)
                ++written;
            else if (response.result.code == "already_exists")
                ++already;
            else
                refused.insert(slot.trade_type_code + ": " + response.result.code + " " +
                               response.result.message);
        }
    }
    BOOST_LOG_SEV(lg, info) << "Stress run wrote " << written << " bond issues; " << already
                            << " were already stored.";
    INFO("written=" << written << " already=" << already);
    CHECK(written > 0);
    std::string listed;
    for (const auto& reason : refused)
        listed += "\n  " + reason;
    if (!refused.empty())
        FAIL_CHECK("these bond issues were refused:" + listed);
}
