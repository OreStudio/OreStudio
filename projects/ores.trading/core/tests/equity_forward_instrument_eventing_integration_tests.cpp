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
// A seeded parent is system-tenant reference data (its soft FK carries
// :use_system_tenant:), so its row is forced to the system tenant and
// written under a system-scoped context, and the tenant_id helpers are
// needed.
#include "ores.eventing.api/domain/entity_event.hpp"
#include "ores.eventing.api/domain/entity_event_traits.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.eventing.api/service/event_bus.hpp"
#include "ores.eventing.core/service/entity_event_publisher.hpp"
#include "ores.eventing.core/service/postgres_event_source.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.trading.api/domain/equity_forward_instrument.hpp"
#include "ores.trading.api/domain/equity_forward_instrument_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/eventing/equity_forward_instrument_event.hpp"
#include "ores.trading.api/generators/equity_forward_instrument_generator.hpp"
#include "ores.trading.api/messaging/equity_forward_instrument_protocol.hpp"
#include "ores.trading.core/repository/equity_forward_instrument_repository.hpp"
#include "ores.trading.core/service/equity_forward_instrument_service.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
// Party seeds (mandatory party_id soft FKs, direct or via a parent's own
// mandatory party_id FK): the party generator and repository are used
// regardless of the child's generator facet, hence the fully-qualified
// refdata paths.
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
// Soft-FK parent seeding (ores_trading_trades_tbl): the parent may live in another
// component, so its own component names the headers.
#include "ores.trading.api/generators/trade_generator.hpp"
#include "ores.trading.core/repository/trade_repository.hpp"
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
// Grand-parent seeding (ores_refdata_books_tbl): the parent's own mandatory soft FKs
// reference rows the test seeds before the parent, so their generator
// and repository headers are needed too.
#include "ores.refdata.api/generators/book_generator.hpp"
#include "ores.refdata.core/repository/book_repository.hpp"
// Grand-parent seeding (ores_refdata_portfolios_tbl): the parent's own mandatory soft FKs
// reference rows the test seeds before the parent, so their generator
// and repository headers are needed too.
#include "ores.refdata.api/generators/portfolio_generator.hpp"
#include "ores.refdata.core/repository/portfolio_repository.hpp"
// Grand-parent seeding (ores_dq_fsm_states_tbl): the parent's own mandatory soft FKs
// reference rows the test seeds before the parent, so their generator
// and repository headers are needed too.
#include "ores.dq.api/generators/fsm_state_generator.hpp"
#include "ores.dq.core/repository/fsm_state_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <thread>

// Proves the "write an entity, observe its NATS entity-changed
// notification" pattern end to end for equity_forward_instrument -- the
// production DB-write -> pg_notify -> postgres_event_source ->
// event_bus -> NATS publish chain, assembled directly here the same
// way the production event-registrar wires it.

namespace {

const std::string_view test_suite("trading.tests");
const std::string tags("[eventing][integration]");

// Equity Forward Instrument writes are party-scoped: the session-level
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
using ores::trading::domain::equity_forward_instrument;
using ores::trading::repository::equity_forward_instrument_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_equity_forward_instrument_publishes_an_event", tags) {
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

    using event_type = ores::trading::messaging::equity_forward_instrument_event;
    auto sub = bus.subscribe<event_type>([&nats](const event_type& e) {
        // One payload is addressed by three subjects, so the subject is the
        // collection's prefix and the action the event reports.
        ev::service::publish_entity_event(nats, ev::domain::event_subject<event_type>(e.action), e);
    });

    event_source.register_entity_event_mapping<event_type>(
        "ores_trading_equity_forward_instruments");

    // 2. Subscribe as an external observer would, on the relative subject --
    // client::subscribe() prepends the subject_prefix itself. The wildcard
    // takes every action: the first write creates the row and a re-drive
    // updates it, and the chain is what is under test rather than which of
    // the three subjects carried it.
    auto observer = nats.subscribe_buffered(
        std::string(ev::domain::entity_event_traits<event_type>::subject_prefix) + ".>", 10);

    // The listener thread issues LISTEN asynchronously on its own
    // dedicated connection. Block until it has actually done so before
    // writing -- Postgres does not queue NOTIFYs sent before a matching
    // LISTEN is registered.
    event_source.start();
    REQUIRE(event_source.wait_until_ready());

    // 3. Write -- triggers the entity's notify trigger -> pg_notify ->
    // the chain wired above -> NATS.
    auto v = generate_synthetic_equity_forward_instrument(ctx);
    v.audit.change_reason_code = "system.test";
    v.identity.party_id = *party_ctx.party_id();
    // Seed the active trade row ores_trading_trades_tbl references:
    // the insert trigger's existence check rejects a synthetic key that
    // matches no active row, so the parent must be written first.
    auto trade_id_parent = ores::trading::generators::generate_synthetic_trade(ctx);
    trade_id_parent.audit.change_reason_code = "system.test";
    auto trade_id_parent_book_parent_currency_parent =
        ores::refdata::generators::generate_synthetic_currency(ctx);
    trade_id_parent_book_parent_currency_parent.change_reason_code = "system.test";
    auto trade_id_parent_book_parent_portfolio_parent =
        ores::refdata::generators::generate_synthetic_portfolio(ctx);
    trade_id_parent_book_parent_portfolio_parent.change_reason_code = "system.test";
    auto trade_id_parent_book_parent = ores::refdata::generators::generate_synthetic_book(ctx);
    trade_id_parent_book_parent.change_reason_code = "system.test";
    auto trade_id_parent_portfolio_parent =
        ores::refdata::generators::generate_synthetic_portfolio(ctx);
    trade_id_parent_portfolio_parent.change_reason_code = "system.test";
    // Seed the active currency row ores_refdata_currencies_tbl references:
    // the referencing row's insert trigger rejects a synthetic key that
    // matches no active row, so it must be written first.
    ores::refdata::repository::currency_repository trade_id_parent_book_parent_currency_parent_repo;
    trade_id_parent_book_parent_currency_parent_repo.write(
        party_ctx, trade_id_parent_book_parent_currency_parent);
    trade_id_parent_book_parent.functional_currency =
        trade_id_parent_book_parent_currency_parent.iso_code;
    // portfolio carries a mandatory party_id FK of its own
    // (session-set in production), so seed a party for it before its write,
    // exactly as the direct-parent branch does.
    auto trade_id_parent_book_parent_portfolio_parent_party =
        ores::refdata::generators::generate_synthetic_party(ctx);
    trade_id_parent_book_parent_portfolio_parent_party.change_reason_code = "system.test";
    auto trade_id_parent_book_parent_portfolio_parent_party_existing =
        ores::refdata::repository::party_repository().read_latest(party_ctx);
    for (const auto& e : trade_id_parent_book_parent_portfolio_parent_party_existing) {
        if (e.tenant_id == trade_id_parent_book_parent_portfolio_parent_party.tenant_id) {
            trade_id_parent_book_parent_portfolio_parent_party.parent_party_id = e.id;
            break;
        }
    }
    ores::refdata::repository::party_repository
        trade_id_parent_book_parent_portfolio_parent_party_repo;
    trade_id_parent_book_parent_portfolio_parent_party_repo.write(
        party_ctx, trade_id_parent_book_parent_portfolio_parent_party);
    trade_id_parent_book_parent_portfolio_parent.party_id =
        trade_id_parent_book_parent_portfolio_parent_party.id;
    // Seed the active portfolio row ores_refdata_portfolios_tbl references:
    // the referencing row's insert trigger rejects a synthetic key that
    // matches no active row, so it must be written first.
    ores::refdata::repository::portfolio_repository
        trade_id_parent_book_parent_portfolio_parent_repo;
    trade_id_parent_book_parent_portfolio_parent_repo.write(
        party_ctx, trade_id_parent_book_parent_portfolio_parent);
    trade_id_parent_book_parent.parent_portfolio_id =
        trade_id_parent_book_parent_portfolio_parent.id;
    // book carries a mandatory party_id FK of its own
    // (session-set in production), so seed a party for it before its write,
    // exactly as the direct-parent branch does.
    auto trade_id_parent_book_parent_party =
        ores::refdata::generators::generate_synthetic_party(ctx);
    trade_id_parent_book_parent_party.change_reason_code = "system.test";
    auto trade_id_parent_book_parent_party_existing =
        ores::refdata::repository::party_repository().read_latest(party_ctx);
    for (const auto& e : trade_id_parent_book_parent_party_existing) {
        if (e.tenant_id == trade_id_parent_book_parent_party.tenant_id) {
            trade_id_parent_book_parent_party.parent_party_id = e.id;
            break;
        }
    }
    ores::refdata::repository::party_repository trade_id_parent_book_parent_party_repo;
    trade_id_parent_book_parent_party_repo.write(party_ctx, trade_id_parent_book_parent_party);
    trade_id_parent_book_parent.party_id = trade_id_parent_book_parent_party.id;
    // Seed the active book row ores_refdata_books_tbl references:
    // the referencing row's insert trigger rejects a synthetic key that
    // matches no active row, so it must be written first.
    ores::refdata::repository::book_repository trade_id_parent_book_parent_repo;
    trade_id_parent_book_parent_repo.write(party_ctx, trade_id_parent_book_parent);
    trade_id_parent.parties.book_id = trade_id_parent_book_parent.id;
    // portfolio carries a mandatory party_id FK of its own
    // (session-set in production), so seed a party for it before its write,
    // exactly as the direct-parent branch does.
    auto trade_id_parent_portfolio_parent_party =
        ores::refdata::generators::generate_synthetic_party(ctx);
    trade_id_parent_portfolio_parent_party.change_reason_code = "system.test";
    auto trade_id_parent_portfolio_parent_party_existing =
        ores::refdata::repository::party_repository().read_latest(party_ctx);
    for (const auto& e : trade_id_parent_portfolio_parent_party_existing) {
        if (e.tenant_id == trade_id_parent_portfolio_parent_party.tenant_id) {
            trade_id_parent_portfolio_parent_party.parent_party_id = e.id;
            break;
        }
    }
    ores::refdata::repository::party_repository trade_id_parent_portfolio_parent_party_repo;
    trade_id_parent_portfolio_parent_party_repo.write(party_ctx,
                                                      trade_id_parent_portfolio_parent_party);
    trade_id_parent_portfolio_parent.party_id = trade_id_parent_portfolio_parent_party.id;
    // Seed the active portfolio row ores_refdata_portfolios_tbl references:
    // the referencing row's insert trigger rejects a synthetic key that
    // matches no active row, so it must be written first.
    ores::refdata::repository::portfolio_repository trade_id_parent_portfolio_parent_repo;
    trade_id_parent_portfolio_parent_repo.write(party_ctx, trade_id_parent_portfolio_parent);
    trade_id_parent.parties.portfolio_id = trade_id_parent_portfolio_parent.id;
    // fsm_state is system-tenant reference data: reference a
    // seeded catalogue row instead of creating one, as the direct-parent
    // system-tenant branch does.
    {
        ores::dq::repository::fsm_state_repository trade_id_parent_fsm_state_parent_repo;
        const auto trade_id_parent_fsm_state_parent_catalogue =
            trade_id_parent_fsm_state_parent_repo.read_latest(
                party_ctx.with_tenant(ores::utility::uuid::tenant_id::system(), h.db_user()));
        REQUIRE_FALSE(trade_id_parent_fsm_state_parent_catalogue.empty());
        trade_id_parent.classification.status_id =
            trade_id_parent_fsm_state_parent_catalogue.front().id;
    }
    ores::trading::repository::trade_repository trade_id_repo;
    trade_id_repo.write(party_ctx, trade_id_parent);
    v.identity.trade_id = trade_id_parent.identity.id;
    const auto id_str = boost::uuids::to_string(v.identity.trade_id);
    BOOST_LOG_SEV(lg, debug) << "Equity Forward Instrument: " << v;

    equity_forward_instrument_repository repo;
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
                auto decoded = ores::nats::default_wire_codec().decode<event_type>(msg.data);
                // The event carries the row's own key record, so the row under
                // test is recognised by comparing it with the row written.
                if (decoded && decoded->key.trade_id == v.identity.trade_id)
                    received.push_back(msg);
            }
        }
    }

    event_source.stop();

    if (received.empty()) {
        // Exhausted the budget: report what the observer did see so a
        // genuinely broken chain is diagnosable, not a bare empty check.
        const auto final_snapshot = observer.snapshot();
        BOOST_LOG_SEV(lg, error) << "No notification for equity_forward_instrument " << id_str
                                 << " after " << max_attempts << " writes; observer received "
                                 << final_snapshot.size() << " message(s) in total";
        for (const auto& msg : final_snapshot)
            BOOST_LOG_SEV(lg, error) << "  unexpected message on subject '" << msg.subject << "', "
                                     << msg.data.size() << " bytes";
    }
    REQUIRE_FALSE(received.empty());
    BOOST_LOG_SEV(lg, info) << "Received " << received.size()
                            << " matching NATS notification(s) for equity_forward_instrument "
                            << id_str;

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
        ores::trading::service::equity_forward_instrument_service svc(crud_ctx);
        v.audit.change_commentary = "updated-by-crud-round-trip";
        repo.write(crud_ctx, v);

        auto versions = svc.get_equity_forward_instrument_history(id_str);
        REQUIRE(versions.size() >= 2);
        REQUIRE(versions.front().audit.change_commentary == "updated-by-crud-round-trip");

        svc.delete_equity_forward_instrument(v.identity.trade_id);
        // Delete soft-closes the active row (the instead-of delete
        // rule sets valid_to): the row disappears from latest reads,
        // and the version history keeps every version.
        REQUIRE_FALSE(svc.get_equity_forward_instrument(v.identity.trade_id).has_value());
        REQUIRE(svc.get_equity_forward_instrument_history(id_str).size() == versions.size());
    }
}
