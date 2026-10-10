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
#include "ores.trading.api/generators/trade_booking_generator.hpp"
#include "ores.trading.api/generators/trade_generator.hpp"
#include "ores.trading.api/generators/trade_identifier_generator.hpp"
#include "ores.trading.api/messaging/trade_operations_protocol.hpp"
#include "ores.trading.core/repository/trade_identifier_repository.hpp"
#include "ores.trading.core/repository/trade_repository.hpp"
#include "ores.trading.api/generators/vanilla_swap_instrument_generator.hpp"
#include "ores.trading.core/repository/vanilla_swap_instrument_repository.hpp"
#include "ores.trading.core/service/trade_operations_service.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>

/*
 * These tests exercise the MECHANISM, not the final economic definition: a
 * component write refreshes the stored digest, the external version does not
 * move because this seam cannot ask the customer-visible boundary, and a null
 * amend changes neither.
 *
 * They drive the mechanism through trade_identifier only because it is one of
 * the seven components the fold observes today. trade_identifier is not
 * economic and will leave the fold when the component set is corrected to the
 * instruments and their legs and amounts, at which point cases 1 and 3 must
 * move to an instrument write. A red suite here after that correction is
 * expected and is not a defect: see the provisional-set comment above the fold
 * in trade_economic_digest_writer.cpp.
 *
 * The third case is the one that catches a future re-enablement of the bump
 * that forgets the boundary.
 */

namespace {

const std::string tags("[digest]");

using ores::trading::messaging::book_trade_request;
using ores::trading::repository::trade_identifier_repository;
using ores::trading::repository::trade_repository;
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
        r.anchor = ores::trading::generators::generate_synthetic_trade(gen);
        r.booking = ores::trading::generators::generate_synthetic_trade_booking(gen);
        r.booking.book_id = book_id;
        r.booking.change_reason_code = "system.test";
        r.activity_type_code = "new_booking";
        return r;
    }
};

/**
 * @brief Books a trade and builds the identifier a case will write.
 */
struct prepared_trade final {
    std::string trade_id;
    ores::trading::domain::trade_identifier identifier;
};

prepared_trade prepare(fixture& f) {
    auto request = f.request();
    const auto trade_id = boost::uuids::to_string(request.anchor.id);
    const auto booking = trade_operations_service(f.ctx).book_trade(request);
    REQUIRE(booking.result.outcome == outcome::ok);
    REQUIRE(booking.activity_id.has_value());

    /*
     * The economic write: a Swap routes to vanilla_swap_instrument, and it is
     * the instrument that states the trade's economics. Booking alone no
     * longer leaves a digest, because a book and a lifecycle state are terms
     * nobody agreed to.
     */
    auto instrument =
        ores::trading::generators::generate_synthetic_vanilla_swap_instrument(f.gen);
    instrument.identity.trade_id = request.anchor.id;
    instrument.identity.trade_type_code = "Swap";
    instrument.identity.trade_activity_id = *booking.activity_id;
    ores::trading::repository::vanilla_swap_instrument_repository().write(f.ctx, instrument);

    auto identifier = ores::trading::generators::generate_synthetic_trade_identifier(f.gen);
    identifier.trade_id = request.anchor.id;
    identifier.trade_activity_id = *booking.activity_id;
    identifier.id_type = "Internal";
    identifier.id_value = "UTI-ALPHA";
    return {trade_id, identifier};
}

}

/*
 * A first component write states a digest where the trade held none.
 */
TEST_CASE("a_component_write_refreshes_the_digest_and_leaves_the_external_version", tags) {
    fixture f;
    const auto prepared = prepare(f);

    const auto before = trade_repository().read_latest(f.ctx, prepared.trade_id);
    REQUIRE(before.size() == 1);
    REQUIRE_FALSE(before.front().economic_digest.empty());
    const auto digest_before = before.front().economic_digest;
    const auto version_before = before.front().external_version;

    trade_identifier_repository().write(f.ctx, prepared.identifier);

    const auto after = trade_repository().read_latest(f.ctx, prepared.trade_id);
    REQUIRE(after.size() == 1);
    CHECK(after.front().economic_digest != digest_before);
    CHECK(after.front().external_version == version_before);
}

/*
 * The null amend: the same value written again changes neither the digest nor
 * the external version.
 */
TEST_CASE("a_null_amend_changes_neither_the_digest_nor_the_external_version", tags) {
    fixture f;
    const auto prepared = prepare(f);
    trade_identifier_repository().write(f.ctx, prepared.identifier);

    const auto first = trade_repository().read_latest(f.ctx, prepared.trade_id);
    REQUIRE(first.size() == 1);

    trade_identifier_repository().write(f.ctx, prepared.identifier);

    const auto second = trade_repository().read_latest(f.ctx, prepared.trade_id);
    REQUIRE(second.size() == 1);
    CHECK(second.front().economic_digest == first.front().economic_digest);
    CHECK(second.front().external_version == first.front().external_version);
}

/*
 * An economic change refreshes the digest and still leaves the external version
 * alone, because the write is not an agreement. This is the assertion that a
 * future re-enablement of the bump must not break by accident.
 */
TEST_CASE("an_economic_change_refreshes_the_digest_and_still_leaves_the_external_version", tags) {
    fixture f;
    const auto prepared = prepare(f);
    trade_identifier_repository().write(f.ctx, prepared.identifier);

    const auto first = trade_repository().read_latest(f.ctx, prepared.trade_id);
    REQUIRE(first.size() == 1);

    auto amended = prepared.identifier;
    amended.id_value = "UTI-BETA";
    trade_identifier_repository().write(f.ctx, amended);

    const auto second = trade_repository().read_latest(f.ctx, prepared.trade_id);
    REQUIRE(second.size() == 1);
    CHECK(second.front().economic_digest != first.front().economic_digest);
    CHECK(second.front().external_version == first.front().external_version);
}

/*
 * The amendment: a component written with a *user* reason is a change of the
 * terms, so the digest moves and the external version moves with it. A system
 * reason is an import or a migration and moves neither, which is what the
 * three cases above pin.
 */
TEST_CASE("an_amendment_moves_the_digest_and_the_external_version", tags) {
    fixture f;
    const auto prepared = prepare(f);

    const auto before = trade_repository().read_latest(f.ctx, prepared.trade_id);
    REQUIRE(before.size() == 1);
    const auto version_before = before.front().external_version;

    auto amended = ores::trading::generators::generate_synthetic_vanilla_swap_instrument(f.gen);
    amended.identity.trade_id = boost::uuids::string_generator()(prepared.trade_id);
    amended.identity.trade_type_code = "Swap";
    amended.identity.trade_activity_id = prepared.identifier.trade_activity_id;
    amended.audit.change_reason_code = "common.rectification";
    ores::trading::repository::vanilla_swap_instrument_repository().write(f.ctx, amended);

    const auto after = trade_repository().read_latest(f.ctx, prepared.trade_id);
    REQUIRE(after.size() == 1);
    CHECK(after.front().economic_digest != before.front().economic_digest);
    CHECK(after.front().external_version == version_before + 1);
}
