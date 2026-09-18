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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_nats_integration_test.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.database/domain/context.hpp"
#include "ores.eventing.api/domain/entity_change_event.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.eventing.api/service/event_bus.hpp"
#include "ores.eventing.core/service/entity_event_publisher.hpp"
#include "ores.eventing.core/service/postgres_event_source.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.trading.api/domain/trade.hpp"
#include "ores.trading.api/domain/trade_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/eventing/trade_changed_event.hpp"
#include "ores.trading.api/generators/trade_generator.hpp"
#include "ores.trading.core/repository/trade_repository.hpp"
#include "ores.trading.core/service/trade_service.hpp"
// Party seeds (mandatory party_id soft FKs, direct or via a parent's own
// mandatory party_id FK): the party generator and repository are used
// regardless of the child's generator facet, hence the fully-qualified
// refdata paths.
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
// FK-parent aggregation-currency seed: a seeded portfolio parent's insert
// trigger validates aggregation_ccy against the currencies table for the
// write tenant, and the synthetic portfolio generator always emits the
// X-0 sentinel -- the test seeds it before the parent write or the
// parent insert is rejected. Like the entity-level currency seed, the
// currency generator and repository are used regardless of the child's
// generator facet, hence the fully-qualified refdata paths.
#include "ores.refdata.api/generators/currency_generator.hpp"
#include "ores.refdata.core/repository/currency_repository.hpp"
// Soft-FK parent seeding (ores_refdata_books_tbl): the parent may live in another
// component, so its own component names the headers.
#include "ores.refdata.api/generators/book_generator.hpp"
#include "ores.refdata.core/repository/book_repository.hpp"
// Grand-parent seeding (ores_refdata_currencies_tbl): the parent's own mandatory soft FKs
// reference rows the test seeds before the parent, so their generator
// and repository headers are needed too.
#include "ores.refdata.api/generators/currency_generator.hpp"
#include "ores.refdata.core/repository/currency_repository.hpp"
// Grand-parent seeding (ores_refdata_portfolios_tbl): the parent's own mandatory soft FKs
// reference rows the test seeds before the parent, so their generator
// and repository headers are needed too.
#include "ores.refdata.api/generators/portfolio_generator.hpp"
#include "ores.refdata.core/repository/portfolio_repository.hpp"
// Soft-FK parent seeding (ores_refdata_portfolios_tbl): the parent may live in another
// component, so its own component names the headers.
#include "ores.refdata.api/generators/portfolio_generator.hpp"
#include "ores.refdata.core/repository/portfolio_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <thread>

// Proves the "write an entity, observe its NATS entity-changed
// notification" pattern end to end for trade -- the
// production DB-write -> pg_notify -> postgres_event_source ->
// event_bus -> NATS publish chain, assembled directly here the same
// way the production event-registrar wires it.

namespace {

const std::string_view test_suite("trading.tests");
const std::string tags("[eventing][integration]");

// Trade writes are party-scoped: the session-level
// app.current_party_id GUC must be set before writing.
ores::database::context
write_test_party_and_scope_context(ores::testing::scoped_database_helper& h,
                                   ores::utility::generation::generation_context& ctx) {
    using ores::refdata::repository::party_repository;
    party_repository party_repo;
    auto party = ores::refdata::generators::generate_synthetic_party(ctx);
    party.change_reason_code = "system.test";
    auto existing = party_repo.read_latest(h.context());
    for (const auto& e : existing) {
        if (e.tenant_id == party.tenant_id) {
            party.parent_party_id = e.id;
            break;
        }
    }
    party_repo.write(h.context(), party);
    return h.context().with_party(h.tenant_id(), party.id, {party.id}, h.db_user());
}

}

using namespace ores::trading::generators;
using ores::trading::domain::trade;
using ores::trading::repository::trade_repository;
using ores::refdata::repository::currency_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_trade_publishes_nats_changed_event", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto party_ctx = write_test_party_and_scope_context(h, ctx);

    // 1. Wire the same DB-notify -> event_bus -> NATS-publish chain the
    // production event-registrar wires in the live service, assembled
    // directly in the test instead of via a running process.
    namespace ev = ores::eventing;
    ev::service::event_bus bus;
    ev::service::postgres_event_source event_source(party_ctx, bus);

    ores::nats::service::client nats(ores::testing::make_nats_options());
    nats.connect();
    REQUIRE(nats.is_connected());

    auto sub = bus.subscribe<ores::trading::eventing::trade_changed_event>(
        [&nats](const ores::trading::eventing::trade_changed_event& e) {
            ev::service::publish_entity_event(
                nats,
                std::string(
                    ev::domain::event_traits<ores::trading::eventing::trade_changed_event>::name),
                ev::domain::entity_change_event{.entity = "ores.trading.trade",
                                                .timestamp = e.timestamp,
                                                .entity_ids = e.trade_ids,
                                                .tenant_id = e.tenant_id});
        });

    event_source.register_mapping<ores::trading::eventing::trade_changed_event>(
        "ores.trading.trade", "ores_trading_trades");

    // 2. Subscribe as an external observer would, on the relative subject --
    // client::subscribe() prepends the subject_prefix itself.
    auto observer = nats.subscribe_buffered(
        std::string(ev::domain::event_traits<ores::trading::eventing::trade_changed_event>::name),
        10);

    // The listener thread issues LISTEN asynchronously on its own
    // dedicated connection. Block until it has actually done so before
    // writing -- Postgres does not queue NOTIFYs sent before a matching
    // LISTEN is registered.
    event_source.start();
    REQUIRE(event_source.wait_until_ready());

    // 3. Write -- triggers the entity's notify trigger -> pg_notify ->
    // the chain wired above -> NATS.
    auto v = generate_synthetic_trade(ctx);
    v.audit.change_reason_code = "system.test";
    v.identity.party_id = *party_ctx.party_id();
    // Seed the active book row ores_refdata_books_tbl references:
    // the insert trigger's existence check rejects a synthetic key that
    // matches no active row, so the parent must be written first.
    auto book_id_parent = ores::refdata::generators::generate_synthetic_book(ctx);
    book_id_parent.change_reason_code = "system.test";
    // The trade derives its own party from this
    // book, so the book carries the
    // session party rather than a fresh one. Give it any other party and the
    // written row is owned by a party the session cannot see, which makes it
    // invisible to the very session that wrote it.
    book_id_parent.party_id = *party_ctx.party_id();
    auto functional_currency_parent = ores::refdata::generators::generate_synthetic_currency(ctx);
    functional_currency_parent.change_reason_code = "system.test";
    auto parent_portfolio_id_parent = ores::refdata::generators::generate_synthetic_portfolio(ctx);
    parent_portfolio_id_parent.change_reason_code = "system.test";
    // Seed the active currency row ores_refdata_currencies_tbl references:
    // the referencing row's insert trigger rejects a synthetic key that
    // matches no active row, so it must be written first.
    ores::refdata::repository::currency_repository functional_currency_parent_repo;
    functional_currency_parent_repo.write(party_ctx, functional_currency_parent);
    book_id_parent.functional_currency = functional_currency_parent.iso_code;
    // portfolio carries a mandatory party_id FK of its own
    // (session-set in production), so seed a party for it before its write,
    // exactly as the direct-parent branch does.
    auto parent_portfolio_id_parent_party =
        ores::refdata::generators::generate_synthetic_party(ctx);
    parent_portfolio_id_parent_party.change_reason_code = "system.test";
    auto parent_portfolio_id_parent_party_existing =
        ores::refdata::repository::party_repository().read_latest(party_ctx);
    for (const auto& e : parent_portfolio_id_parent_party_existing) {
        if (e.tenant_id == parent_portfolio_id_parent_party.tenant_id) {
            parent_portfolio_id_parent_party.parent_party_id = e.id;
            break;
        }
    }
    ores::refdata::repository::party_repository parent_portfolio_id_parent_party_repo;
    parent_portfolio_id_parent_party_repo.write(party_ctx, parent_portfolio_id_parent_party);
    parent_portfolio_id_parent.party_id = parent_portfolio_id_parent_party.id;
    // Seed the active portfolio row ores_refdata_portfolios_tbl references:
    // the referencing row's insert trigger rejects a synthetic key that
    // matches no active row, so it must be written first.
    ores::refdata::repository::portfolio_repository parent_portfolio_id_parent_repo;
    parent_portfolio_id_parent_repo.write(party_ctx, parent_portfolio_id_parent);
    book_id_parent.parent_portfolio_id = parent_portfolio_id_parent.id;
    ores::refdata::repository::book_repository book_id_repo;
    book_id_repo.write(party_ctx, book_id_parent);
    v.parties.book_id = book_id_parent.id;
    // Seed the active portfolio row ores_refdata_portfolios_tbl references:
    // the insert trigger's existence check rejects a synthetic key that
    // matches no active row, so the parent must be written first.
    auto portfolio_id_parent = ores::refdata::generators::generate_synthetic_portfolio(ctx);
    portfolio_id_parent.change_reason_code = "system.test";
    // portfolio's own mandatory party_id FK (session-set in
    // production) needs an active party too: seed one, attached under the
    // tenant's root party like the direct-party branch below.
    auto portfolio_id_party = ores::refdata::generators::generate_synthetic_party(ctx);
    portfolio_id_party.change_reason_code = "system.test";
    auto portfolio_id_party_existing =
        ores::refdata::repository::party_repository().read_latest(party_ctx);
    for (const auto& e : portfolio_id_party_existing) {
        if (e.tenant_id == portfolio_id_party.tenant_id) {
            portfolio_id_party.parent_party_id = e.id;
            break;
        }
    }
    ores::refdata::repository::party_repository portfolio_id_party_repo;
    portfolio_id_party_repo.write(party_ctx, portfolio_id_party);
    portfolio_id_parent.party_id = portfolio_id_party.id;
    // The parent portfolio's insert trigger validates aggregation_ccy
    // against the currencies table for the write tenant, and the
    // synthetic portfolio generator always emits the X-0 sentinel --
    // seed it before the parent write or the parent insert is rejected.
    // Distinct name from the entity-level currency seed block: both are
    // in scope when the entity also carries the seed_currency flag.
    auto parent_ccy = ores::refdata::generators::generate_synthetic_currency(ctx);
    parent_ccy.iso_code = "X-0";
    currency_repository parent_ccy_repo;
    parent_ccy_repo.write(party_ctx, {parent_ccy});
    ores::refdata::repository::portfolio_repository portfolio_id_repo;
    portfolio_id_repo.write(party_ctx, portfolio_id_parent);
    v.parties.portfolio_id = portfolio_id_parent.id;
    const auto id_str = boost::uuids::to_string(v.identity.id);
    BOOST_LOG_SEV(lg, debug) << "Trade: " << v;

    trade_repository repo;
    repo.write(party_ctx, v);

    // 4. Poll the observer's buffer for the notification. The chain --
    // trigger -> pg_notify -> 100ms listener poll -> event_bus -> NATS
    // round trip -- is real, no mocks. Under CI load the listener or
    // NATS connection can hiccup once (reconnect backoff 1-5s) and the
    // notification in flight is lost forever; a lost notification never
    // arrives, so re-drive the write -- a new version row re-fires the
    // notify trigger. Bounded: 4 attempts, each polling ~2.5s.
    constexpr int max_attempts = 4;
    constexpr int polls_per_attempt = 25;
    std::vector<ores::nats::message> received;
    for (int attempt = 1; attempt <= max_attempts && received.empty(); ++attempt) {
        if (attempt > 1) {
            BOOST_LOG_SEV(lg, warn) << "No matching notification yet; re-driving write"
                                    << " (attempt " << attempt << " of " << max_attempts << ")";
            repo.write(party_ctx, v);
        }
        for (int i = 0; i < polls_per_attempt && received.empty(); ++i) {
            std::this_thread::sleep_for(std::chrono::milliseconds(100));
            auto snap = observer.snapshot();
            for (const auto& msg : snap) {
                auto decoded =
                    ores::nats::default_wire_codec().decode<ev::domain::entity_change_event>(
                        msg.data);
                if (decoded && decoded->entity == "ores.trading.trade") {
                    for (const auto& changed_id : decoded->entity_ids) {
                        if (changed_id == id_str)
                            received.push_back(msg);
                    }
                }
            }
        }
    }

    event_source.stop();

    if (received.empty()) {
        // Exhausted the budget: report what the observer did see so a
        // genuinely broken chain is diagnosable, not a bare empty check.
        const auto final_snapshot = observer.snapshot();
        BOOST_LOG_SEV(lg, error) << "No notification for trade " << id_str << " after "
                                 << max_attempts << " writes; observer received "
                                 << final_snapshot.size() << " message(s) in total";
        for (const auto& msg : final_snapshot)
            BOOST_LOG_SEV(lg, error) << "  unexpected message on subject '" << msg.subject << "', "
                                     << msg.data.size() << " bytes";
    }
    REQUIRE_FALSE(received.empty());
    BOOST_LOG_SEV(lg, info) << "Received " << received.size()
                            << " matching NATS notification(s) for trade " << id_str;

    // 5. CRUD round trip on the same row: update through the
    // repository, read the version history through the service, and
    // delete. Reads and writes go through party_ctx: for party-scoped
    // entities it already carries the visible-party GUC the RLS
    // policies filter every service read by; otherwise it is the
    // plain test context. The version history grows by one per write
    // (the notify re-drive above may have written more than once), so
    // only growth is asserted, not an exact count.
    {
        // party_ctx already carries the visible-party set: v's own
        // party is the session party the RLS policies filter by.
        const auto& crud_ctx = party_ctx;
        ores::trading::service::trade_service svc(crud_ctx);
        v.audit.change_commentary = "updated-by-crud-round-trip";
        // Rewriting the row is an amendment, not a booking: the booking
        // activity names a transition that starts the state machine and is
        // rejected on a row that already has a state.
        v.classification.activity_type_code = "amendment";
        repo.write(crud_ctx, v);

        auto versions = svc.get_trade_history(id_str);
        REQUIRE(versions.size() >= 2);
        REQUIRE(versions.front().audit.change_commentary == "updated-by-crud-round-trip");

        svc.delete_trade(id_str);
        // Delete soft-closes the active row (the instead-of delete
        // rule sets valid_to): the row disappears from latest reads,
        // and the version history keeps every version.
        REQUIRE_FALSE(svc.get_trade(id_str).has_value());
        REQUIRE(svc.get_trade_history(id_str).size() == versions.size());
    }
}
