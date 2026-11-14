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
#include "ores.trading.core/service/trade_operations_service.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <set>
#include <string>
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
    scope.issuer_ids = {f.counterparty_id};
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

    for (const auto code : asset_classes) {
        const std::string asset_class(code);
        const auto offered = planner_for(asset_class).code_count();
        // A class that states no code would book nothing and pass below, so
        // the case states that each class has types to book.
        INFO("asset class " << asset_class);
        REQUIRE(offered > 0);

        const auto scope = booking_scope_of(f, asset_class);
        const auto plan = planner_for(asset_class).plan(scope);
        REQUIRE(plan.slots.size() == offered);

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
            if (response.result.outcome == outcome::ok)
                booked_types.insert(slot.trade_type_code);
        }
        // The scope states each code the class cycles, so a type the store
        // refused is a type the population cannot book.
        INFO("asset class " << asset_class);
        CHECK(booked_types.size() == offered);
        BOOST_LOG_SEV(lg, info) << "Booked " << booked_types.size() << " type(s) of " << asset_class
                                << ".";
    }
}
