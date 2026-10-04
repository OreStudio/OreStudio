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
#include "ores.refdata.api/generators/book_generator.hpp"
#include "ores.refdata.api/generators/currency_generator.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.api/generators/portfolio_generator.hpp"
#include "ores.refdata.core/repository/book_repository.hpp"
#include "ores.refdata.core/repository/currency_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.refdata.core/repository/portfolio_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.trading.api/generators/trade_anchor_generator.hpp"
#include "ores.trading.api/generators/trade_booking_generator.hpp"
#include "ores.trading.core/repository/trade_anchor_repository.hpp"
#include "ores.trading.core/repository/trade_booking_repository.hpp"
#include "ores.trading.core/repository/trade_state_repository.hpp"
#include "ores.trading.core/service/trade_operations_service.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

namespace {

const std::string tags("[service]");

using ores::trading::messaging::book_trade_request;
using ores::trading::service::trade_operations_service;
using ores::utility::domain::outcome;

/**
 * @brief A tenant with a party in session and a real book of that party.
 */
struct fixture final {
    ores::testing::scoped_database_helper h;
    ores::utility::generation::generation_context gen = ores::testing::make_generation_context(h);
    ores::database::context ctx = h.context();
    boost::uuids::uuid book_id{};

    fixture() {
        namespace gen_rd = ores::refdata::generators;
        namespace repo = ores::refdata::repository;

        auto party = gen_rd::generate_synthetic_party(gen);
        party.change_reason_code = "system.test";
        for (const auto& e : repo::party_repository().read_latest(h.context())) {
            if (e.tenant_id == party.tenant_id) {
                party.parent_party_id = e.id;
                break;
            }
        }
        repo::party_repository().write(h.context(), party);
        ctx = h.context().with_party(h.tenant_id(), party.id, {party.id}, h.db_user());

        auto currency = gen_rd::generate_synthetic_currency(gen);
        repo::currency_repository().write(ctx, {currency});

        auto portfolio = gen_rd::generate_synthetic_portfolio(gen);
        portfolio.party_id = party.id;
        portfolio.change_reason_code = "system.test";
        repo::portfolio_repository().write(ctx, portfolio);

        auto book = gen_rd::generate_synthetic_book(gen);
        book.party_id = party.id;
        book.parent_portfolio_id = portfolio.id;
        book.functional_currency = currency.iso_code;
        book.change_reason_code = "system.test";
        repo::book_repository().write(ctx, book);
        book_id = book.id;
    }

    book_trade_request request() {
        book_trade_request r;
        r.anchor = ores::trading::generators::generate_synthetic_trade_anchor(gen);
        r.booking = ores::trading::generators::generate_synthetic_trade_booking(gen);
        r.booking.book_id = book_id;
        r.booking.change_reason_code = "system.test";
        r.activity_type_code = "new_booking";
        return r;
    }
};

}

TEST_CASE("book_trade_writes_the_anchor_the_booking_and_the_state", tags) {
    fixture f;
    auto req = f.request();
    const auto executed_at =
        std::chrono::floor<std::chrono::seconds>(std::chrono::system_clock::now());
    req.booking.execution_timestamp = executed_at;
    const auto id = boost::uuids::to_string(req.anchor.id);

    const auto response = trade_operations_service(f.ctx).book_trade(req);

    REQUIRE(response.result.outcome == outcome::ok);
    REQUIRE(ores::trading::repository::trade_anchor_repository().read_latest(f.ctx, id).size() ==
            1);
    const auto bookings =
        ores::trading::repository::trade_booking_repository().read_latest(f.ctx, id);
    REQUIRE(bookings.size() == 1);
    CHECK(bookings.front().book_id == f.book_id);
    CHECK(bookings.front().party_id == *f.ctx.party_id());
    REQUIRE(bookings.front().execution_timestamp.has_value());
    CHECK(*bookings.front().execution_timestamp == executed_at);
    REQUIRE(ores::trading::repository::trade_state_repository().read_latest(f.ctx, id).size() == 1);
}

TEST_CASE("a_second_booking_of_the_same_trade_is_a_conflict", tags) {
    fixture f;
    const auto req = f.request();
    REQUIRE(trade_operations_service(f.ctx).book_trade(req).result.outcome == outcome::ok);

    const auto response = trade_operations_service(f.ctx).book_trade(req);

    CHECK(response.result.outcome == outcome::conflict);
    CHECK(response.result.code == "already_exists");
    const auto bookings = ores::trading::repository::trade_booking_repository().read_latest(
        f.ctx, boost::uuids::to_string(req.anchor.id));
    REQUIRE(bookings.size() == 1);
    CHECK(bookings.front().version == 1);
}

TEST_CASE("a_booking_that_fails_writes_no_anchor", tags) {
    fixture f;
    auto req = f.request();
    req.booking.book_id = f.gen.generate_uuid();

    CHECK_THROWS(trade_operations_service(f.ctx).book_trade(req));

    CHECK(ores::trading::repository::trade_anchor_repository()
              .read_latest(f.ctx, boost::uuids::to_string(req.anchor.id))
              .empty());
}
