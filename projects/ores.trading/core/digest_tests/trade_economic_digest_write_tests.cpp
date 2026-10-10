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
#include "ores.platform/time/datetime.hpp"
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
#include "ores.trading.api/domain/trade_type_routing.hpp"
#include "ores.trading.api/generators/bond_instrument_generator.hpp"
#include "ores.trading.api/generators/bond_issue_generator.hpp"
#include "ores.trading.api/generators/commodity_instrument_generator.hpp"
#include "ores.trading.api/generators/composite_instrument_generator.hpp"
#include "ores.trading.api/generators/credit_instrument_generator.hpp"
#include "ores.trading.api/generators/equity_accumulator_instrument_generator.hpp"
#include "ores.trading.api/generators/equity_asian_option_instrument_generator.hpp"
#include "ores.trading.api/generators/equity_barrier_option_instrument_generator.hpp"
#include "ores.trading.api/generators/equity_digital_option_instrument_generator.hpp"
#include "ores.trading.api/generators/equity_forward_instrument_generator.hpp"
#include "ores.trading.api/generators/equity_option_instrument_generator.hpp"
#include "ores.trading.api/generators/equity_position_instrument_generator.hpp"
#include "ores.trading.api/generators/equity_swap_instrument_generator.hpp"
#include "ores.trading.api/generators/equity_variance_swap_instrument_generator.hpp"
#include "ores.trading.api/generators/fx_accumulator_instrument_generator.hpp"
#include "ores.trading.api/generators/fx_asian_forward_instrument_generator.hpp"
#include "ores.trading.api/generators/fx_barrier_option_instrument_generator.hpp"
#include "ores.trading.api/generators/fx_digital_option_instrument_generator.hpp"
#include "ores.trading.api/generators/fx_forward_instrument_generator.hpp"
#include "ores.trading.api/generators/fx_vanilla_option_instrument_generator.hpp"
#include "ores.trading.api/generators/fx_variance_swap_instrument_generator.hpp"
#include "ores.trading.api/generators/rate_instrument_generator.hpp"
#include "ores.trading.api/generators/scripted_instrument_generator.hpp"
#include "ores.trading.api/generators/trade_booking_generator.hpp"
#include "ores.trading.api/generators/trade_generator.hpp"
#include "ores.trading.api/generators/trade_identifier_generator.hpp"
#include "ores.trading.api/generators/vanilla_swap_instrument_generator.hpp"
#include "ores.trading.api/messaging/trade_operations_protocol.hpp"
#include "ores.trading.core/repository/bond_instrument_repository.hpp"
#include "ores.trading.core/repository/bond_issue_repository.hpp"
#include "ores.trading.core/repository/commodity_instrument_repository.hpp"
#include "ores.trading.core/repository/composite_instrument_repository.hpp"
#include "ores.trading.core/repository/credit_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_accumulator_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_asian_option_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_barrier_option_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_digital_option_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_forward_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_option_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_position_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_swap_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_variance_swap_instrument_repository.hpp"
#include "ores.trading.core/repository/fx_accumulator_instrument_repository.hpp"
#include "ores.trading.core/repository/fx_asian_forward_instrument_repository.hpp"
#include "ores.trading.core/repository/fx_barrier_option_instrument_repository.hpp"
#include "ores.trading.core/repository/fx_digital_option_instrument_repository.hpp"
#include "ores.trading.core/repository/fx_forward_instrument_repository.hpp"
#include "ores.trading.core/repository/fx_vanilla_option_instrument_repository.hpp"
#include "ores.trading.core/repository/fx_variance_swap_instrument_repository.hpp"
#include "ores.trading.core/repository/rate_instrument_repository.hpp"
#include "ores.trading.core/repository/scripted_instrument_repository.hpp"
#include "ores.trading.core/repository/trade_identifier_repository.hpp"
#include "ores.trading.core/repository/trade_repository.hpp"
#include "ores.trading.core/repository/vanilla_swap_instrument_repository.hpp"
#include "ores.trading.core/service/trade_operations_service.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <exception>
#include <functional>
#include <string>
#include <type_traits>
#include <vector>

/*
 * These tests exercise the MECHANISM, not the final economic definition: a
 * component write refreshes the stored digest, the external version does not
 * move because this seam cannot ask the customer-visible boundary, and a null
 * amend changes neither.
 *
 * Most cases drive the mechanism through trade_identifier, which is one of the
 * components the fold observes today. trade_identifier is not economic and
 * will leave the fold when the component set is corrected to the instruments
 * and their legs and amounts, at which point those cases must move to an
 * instrument write. A red suite here after that correction is expected and is
 * not a defect: see the provisional-set comment above the fold in
 * trade_economic_digest_writer.cpp.
 *
 * The per-family case writes the instrument of every family and checks that
 * the trade's digest moves.
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
    boost::uuids::uuid party_id{};

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
        party_id = party.id;
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
 * @brief Writes a Swap: the rates family header, then the vanilla swap fact
 * row that joins it by trade id.
 */
void write_swap(fixture& f,
                const boost::uuids::uuid& trade_id,
                const boost::uuids::uuid& activity_id,
                const std::string& reason_code) {
    auto header = ores::trading::generators::generate_synthetic_rate_instrument(f.gen);
    header.identity.trade_id = trade_id;
    header.identity.trade_activity_id = activity_id;
    header.identity.party_id = f.party_id;
    header.identity.trade_type_code = "Swap";
    header.start_date = ores::platform::time::datetime::from_iso8601_date("2025-01-15");
    header.maturity_date = ores::platform::time::datetime::from_iso8601_date("2030-01-15");
    header.audit.change_reason_code = reason_code;
    ores::trading::repository::rate_instrument_repository().write(f.ctx, header);

    auto fact = ores::trading::generators::generate_synthetic_vanilla_swap_instrument(f.gen);
    fact.trade_id = trade_id;
    fact.trade_activity_id = activity_id;
    fact.change_reason_code = reason_code;
    ores::trading::repository::vanilla_swap_instrument_repository().write(f.ctx, fact);
}

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
    write_swap(f, request.anchor.id, *booking.activity_id, "system.test");

    auto identifier = ores::trading::generators::generate_synthetic_trade_identifier(f.gen);
    identifier.trade_id = request.anchor.id;
    identifier.trade_activity_id = *booking.activity_id;
    identifier.id_type = "Internal";
    identifier.id_value = "UTI-ALPHA";
    return {trade_id, identifier};
}


/**
 * @brief Writes one instrument of a family for a booked trade, through the
 * family's own repository.
 *
 * The rates header also needs dates its check accepts: maturity after start.
 */
template <typename Repository, typename Generate, typename Adjust>
void write_header(fixture& f,
                  Generate generate,
                  Adjust adjust,
                  const boost::uuids::uuid& trade_id,
                  const boost::uuids::uuid& activity_id,
                  const std::string& trade_type) {
    auto instrument = generate(f.gen);
    instrument.identity.trade_id = trade_id;
    instrument.identity.trade_activity_id = activity_id;
    instrument.identity.party_id = f.party_id;
    instrument.identity.trade_type_code = trade_type;
    if constexpr (std::is_same_v<decltype(instrument), ores::trading::domain::rate_instrument>) {
        instrument.start_date = ores::platform::time::datetime::from_iso8601_date("2025-01-15");
        instrument.maturity_date = ores::platform::time::datetime::from_iso8601_date("2030-01-15");
    }
    adjust(f, instrument);
    Repository().write(f.ctx, instrument);
}

/**
 * @brief One instrument family: the table the catalogue routes a trade type
 * to, that trade type, and how to write an instrument into the table.
 */
struct family final {
    ores::trading::domain::instrument_table table;
    std::string trade_type;
    std::function<void(fixture&, const boost::uuids::uuid&, const boost::uuids::uuid&)> write;
};

/**
 * @brief Leaves a generated instrument as it is.
 */
struct no_adjustment final {
    template <typename Instrument>
    void operator()(fixture&, Instrument&) const {}
};

/**
 * @brief A bond instrument references a bond issue, which must exist first.
 */
struct with_a_bond_issue final {
    void operator()(fixture& f, ores::trading::domain::bond_instrument& instrument) const {
        auto issue = ores::trading::generators::generate_synthetic_bond_issue(f.gen);
        ores::trading::repository::bond_issue_repository().write(f.ctx, issue);
        instrument.issue_id = issue.issue_id;
    }
};

/**
 * @brief A digital option states either an option type and a strike, or a
 * barrier level and a barrier type, never both. The generator states neither
 * consistently.
 */
struct as_a_digital_option final {
    void operator()(fixture&,
                    ores::trading::domain::equity_digital_option_instrument& instrument) const {
        instrument.option_type = "Call";
        instrument.strike = ores::utility::decimal::decimal::from_string("100").value();
        instrument.barrier_level.reset();
        instrument.barrier_type.clear();
    }
};

template <typename Repository, typename Generate, typename Adjust = no_adjustment>
family make_family(ores::trading::domain::instrument_table table,
                   std::string trade_type,
                   Generate generate,
                   Adjust adjust = {}) {
    return {table, trade_type,
            [trade_type, generate, adjust](fixture& f,
                                           const boost::uuids::uuid& trade_id,
                                           const boost::uuids::uuid& activity_id) {
                write_header<Repository>(f, generate, adjust, trade_id, activity_id, trade_type);
            }};
}

/**
 * @brief Every value of instrument_table, once. A new family fails the count
 * in the test below until it is listed here.
 */
std::vector<family> families() {
    using ores::trading::domain::instrument_table;
    namespace gen = ores::trading::generators;
    namespace repo = ores::trading::repository;
    return {
        make_family<repo::bond_instrument_repository>(
            instrument_table::bond_instrument, "Bond", &gen::generate_synthetic_bond_instrument,
            with_a_bond_issue{}),
        make_family<repo::commodity_instrument_repository>(
            instrument_table::commodity_instrument, "CommodityForwardVolatilityAgreement", &gen::generate_synthetic_commodity_instrument),
        make_family<repo::composite_instrument_repository>(
            instrument_table::composite_instrument, "CompositeTrade", &gen::generate_synthetic_composite_instrument),
        make_family<repo::credit_instrument_repository>(
            instrument_table::credit_instrument, "RiskParticipationAgreement", &gen::generate_synthetic_credit_instrument),
        make_family<repo::equity_accumulator_instrument_repository>(
            instrument_table::equity_accumulator_instrument, "EquityAccumulator", &gen::generate_synthetic_equity_accumulator_instrument),
        make_family<repo::equity_asian_option_instrument_repository>(
            instrument_table::equity_asian_option_instrument, "EquityAsianOption", &gen::generate_synthetic_equity_asian_option_instrument),
        make_family<repo::equity_barrier_option_instrument_repository>(
            instrument_table::equity_barrier_option_instrument, "EquityBarrierOption", &gen::generate_synthetic_equity_barrier_option_instrument),
        make_family<repo::equity_digital_option_instrument_repository>(
            instrument_table::equity_digital_option_instrument, "EquityDigitalOption",
            &gen::generate_synthetic_equity_digital_option_instrument, as_a_digital_option{}),
        make_family<repo::equity_forward_instrument_repository>(
            instrument_table::equity_forward_instrument, "EquityForward", &gen::generate_synthetic_equity_forward_instrument),
        make_family<repo::equity_option_instrument_repository>(
            instrument_table::equity_option_instrument, "EquityOption", &gen::generate_synthetic_equity_option_instrument),
        make_family<repo::equity_position_instrument_repository>(
            instrument_table::equity_position_instrument, "EquityPosition", &gen::generate_synthetic_equity_position_instrument),
        make_family<repo::equity_swap_instrument_repository>(
            instrument_table::equity_swap_instrument, "EquitySwap", &gen::generate_synthetic_equity_swap_instrument),
        make_family<repo::equity_variance_swap_instrument_repository>(
            instrument_table::equity_variance_swap_instrument, "EquityVarianceSwap", &gen::generate_synthetic_equity_variance_swap_instrument),
        make_family<repo::fx_accumulator_instrument_repository>(
            instrument_table::fx_accumulator_instrument, "FxAccumulator", &gen::generate_synthetic_fx_accumulator_instrument),
        make_family<repo::fx_asian_forward_instrument_repository>(
            instrument_table::fx_asian_forward_instrument, "FxAverageForward", &gen::generate_synthetic_fx_asian_forward_instrument),
        make_family<repo::fx_barrier_option_instrument_repository>(
            instrument_table::fx_barrier_option_instrument, "FxBarrierOption", &gen::generate_synthetic_fx_barrier_option_instrument),
        make_family<repo::fx_digital_option_instrument_repository>(
            instrument_table::fx_digital_option_instrument, "FxDigitalOption", &gen::generate_synthetic_fx_digital_option_instrument),
        make_family<repo::fx_forward_instrument_repository>(
            instrument_table::fx_forward_instrument, "FxForward", &gen::generate_synthetic_fx_forward_instrument),
        make_family<repo::fx_vanilla_option_instrument_repository>(
            instrument_table::fx_vanilla_option_instrument, "FxOption", &gen::generate_synthetic_fx_vanilla_option_instrument),
        make_family<repo::fx_variance_swap_instrument_repository>(
            instrument_table::fx_variance_swap_instrument, "FxVarianceSwap", &gen::generate_synthetic_fx_variance_swap_instrument),
        make_family<repo::rate_instrument_repository>(
            instrument_table::rate_instrument, "Swap", &gen::generate_synthetic_rate_instrument),
        make_family<repo::scripted_instrument_repository>(
            instrument_table::scripted_instrument, "ScriptedTrade", &gen::generate_synthetic_scripted_instrument)
    };
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
    amended.trade_id = boost::uuids::string_generator()(prepared.trade_id);
    amended.trade_activity_id = prepared.identifier.trade_activity_id;
    amended.settlement_lag = 2;
    amended.change_reason_code = "common.rectification";
    ores::trading::repository::vanilla_swap_instrument_repository().write(f.ctx, amended);

    const auto after = trade_repository().read_latest(f.ctx, prepared.trade_id);
    REQUIRE(after.size() == 1);
    CHECK(after.front().economic_digest != before.front().economic_digest);
    CHECK(after.front().external_version == version_before + 1);
}

/*
 * The fold reads the instrument the trade's type routes to. Each family is
 * given a booked trade of its routed type; writing the family's instrument
 * must move the digest the booking left, or the fold does not read that
 * family. A missing or wrong arm in the fold fails exactly one family here.
 */
TEST_CASE("the_digest_observes_an_instrument_of_every_family", tags) {
    using ores::trading::domain::instrument_table;
    const auto all = families();

    /*
     * The count of instrument_table values. A new family must be added to
     * families() and to the fold together, and this number moves with them.
     */
    REQUIRE(all.size() == 22);

    fixture f;
    for (const auto& fam : all) {
        INFO("family trade type: " << fam.trade_type);
        CHECK(ores::trading::domain::instrument_table_for(fam.trade_type) == fam.table);

        auto request = f.request();
        request.anchor.trade_type = fam.trade_type;
        const auto trade_id = boost::uuids::to_string(request.anchor.id);
        const auto booking = trade_operations_service(f.ctx).book_trade(request);
        REQUIRE(booking.result.outcome == outcome::ok);
        REQUIRE(booking.activity_id.has_value());

        const auto before = trade_repository().read_latest(f.ctx, trade_id);
        REQUIRE(before.size() == 1);

        try {
            fam.write(f, request.anchor.id, *booking.activity_id);
        } catch (const std::exception& e) {
            FAIL_CHECK("writing the " << fam.trade_type << " instrument failed: " << e.what());
            continue;
        }

        const auto after = trade_repository().read_latest(f.ctx, trade_id);
        REQUIRE(after.size() == 1);
        CHECK_FALSE(after.front().economic_digest.empty());
        CHECK(after.front().economic_digest != before.front().economic_digest);
    }
}
