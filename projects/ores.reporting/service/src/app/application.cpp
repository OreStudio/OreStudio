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
#include "ores.reporting.service/app/application.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.eventing.api/service/event_bus.hpp"
#include "ores.eventing.core/service/postgres_event_source.hpp"
#include "ores.iam.client/client/service_token_provider.hpp"
#include "ores.inbox.api/eventing/approval_request_event.hpp"
#include "ores.inbox.api/messaging/approval_request_protocol.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.reporting.api/eventing/report_definition_event.hpp"
#include "ores.reporting.core/messaging/registrar.hpp"
#include "ores.reporting.core/service/cut_authorisation_applier.hpp"
#include "ores.reporting.core/service/report_scheduling_service.hpp"
#include "ores.reporting.service/app/cut_authorisation_reconciler.hpp"
#include "ores.reporting.service/messaging/concurrency_policy_event_registrar.hpp"
#include "ores.reporting.service/messaging/cut_event_registrar.hpp"
#include "ores.reporting.service/messaging/cut_family_event_registrar.hpp"
#include "ores.reporting.service/messaging/cut_gap_event_registrar.hpp"
#include "ores.reporting.service/messaging/cut_kind_event_registrar.hpp"
#include "ores.reporting.service/messaging/cut_resolution_rule_event_registrar.hpp"
#include "ores.reporting.service/messaging/cut_take_registrar.hpp"
#include "ores.reporting.service/messaging/engine_settings_proposal_registrar.hpp"
#include "ores.reporting.service/messaging/report_definition_event_registrar.hpp"
#include "ores.reporting.service/messaging/report_instance_event_registrar.hpp"
#include "ores.reporting.service/messaging/report_type_event_registrar.hpp"
#include "ores.service/service/domain_service_runner.hpp"
#include "ores.service/service/heartbeat_publisher.hpp"
#include "ores.utility/version/version.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/post.hpp>
#include <cstdint>
#include <memory>

namespace ores::reporting::service::app {

using namespace ores::logging;
namespace ev = ores::eventing;
namespace rm = ores::reporting::messaging;

namespace {
constexpr std::string_view service_name = "ores.reporting.service";
constexpr std::string_view service_version = ORES_VERSION;
}

ores::database::context application::make_context(const ores::database::database_options& db_opts) {
    using ores::database::context_factory;

    context_factory::configuration cfg{.database_options = db_opts,
                                       .pool_size = static_cast<std::size_t>(db_opts.pool_size),
                                       .num_attempts = 10,
                                       .wait_time_in_seconds = 1,
                                       .service_account = db_opts.user};

    return context_factory::make_context(cfg);
}

application::application() = default;

boost::asio::awaitable<void> application::run(boost::asio::io_context& io_ctx,
                                              const config::options& cfg) const {

    BOOST_LOG_SEV(lg(), info) << ores::utility::version::format_startup_message(
        "ores.reporting.service", 0, 1);

    ores::nats::service::client nats(cfg.nats);
    nats.connect();

    ores::nats::service::nats_client svc_nats(nats,
                                              ores::iam::client::make_service_token_provider(
                                                  nats, cfg.database.user, cfg.database.password));

    auto db_ctx = make_context(cfg.database);

    // Listen for report definition changes so that definitions created or
    // activated after service startup are scheduled without a service restart.
    ev::service::event_bus event_bus;
    ev::service::postgres_event_source event_source(make_context(cfg.database), event_bus);
    // The generated registrar below maps this table's channel to the canonical
    // event, so the scheduler is a second subscriber on it.
    auto def_changed_sub = event_bus.subscribe<rm::report_definition_event>(
        [&io_ctx, db_ctx, &svc_nats](const rm::report_definition_event&) {
            // Post a reconcile task to the io_context so it runs in the
            // coroutine executor rather than the postgres listener thread.
            auto reconciler = std::make_shared<report_scheduling_service>(db_ctx, svc_nats);
            boost::asio::post(io_ctx, [reconciler, &io_ctx]() {
                boost::asio::co_spawn(
                    io_ctx,
                    [reconciler]() { return reconciler->reconcile(); },
                    boost::asio::detached);
            });
        });

    // Publish report_definition changed events to NATS so the Qt client
    // can auto-refresh the list. Runs alongside the scheduling subscriber
    // above (event_bus supports multiple subscribers per event type).
    auto rd_nats_sub =
        ores::reporting::service::messaging::register_report_definition_event_mapping(
            event_source, event_bus, nats);

    // Wire eventing for the remaining entities — publishes changed events
    // to NATS so the Qt client can refresh lists without manual reload.
    auto cp_changed_sub =
        ores::reporting::service::messaging::register_concurrency_policy_event_mapping(
            event_source, event_bus, nats);
    auto ri_changed_sub =
        ores::reporting::service::messaging::register_report_instance_event_mapping(
            event_source, event_bus, nats);
    auto rt_changed_sub = ores::reporting::service::messaging::register_report_type_event_mapping(
        event_source, event_bus, nats);
    auto cf_changed_sub = ores::reporting::service::messaging::register_cut_family_event_mapping(
        event_source, event_bus, nats);
    auto crr_changed_sub =
        ores::reporting::service::messaging::register_cut_resolution_rule_event_mapping(
            event_source, event_bus, nats);
    auto ck_changed_sub = ores::reporting::service::messaging::register_cut_kind_event_mapping(
        event_source, event_bus, nats);
    auto c_changed_sub = ores::reporting::service::messaging::register_cut_event_mapping(
        event_source, event_bus, nats);
    auto cg_changed_sub = ores::reporting::service::messaging::register_cut_gap_event_mapping(
        event_source, event_bus, nats);

    // A decided cut authorisation is stamped here, by reporting, which owns the
    // cuts. The inbox's request channel is only the nudge: each change runs the
    // reconciliation, which stamps every decided request the cut does not carry
    // yet, so a missed or repeated event changes nothing. The sweep below catches
    // what was decided while this service was down.
    using approval_request_event = ores::inbox::messaging::approval_request_event;
    auto cut_authorisations = std::make_shared<service::cut_authorisation_applier>(
        make_context(cfg.database));
    event_source.register_entity_event_mapping<approval_request_event>(
        "ores_inbox_approval_requests");
    auto cut_authorisation_sub = event_bus.subscribe<approval_request_event>(
        [cut_authorisations](const approval_request_event&) { cut_authorisations->apply(); });

    event_source.start();
    cut_authorisations->apply();

    co_await ores::service::service::run(
        io_ctx,
        nats,
        db_ctx,
        "ores.reporting.service",
        [&svc_nats, &cfg](auto& n, auto c, auto v) {
            auto subs = ores::reporting::messaging::registrar::register_handlers(
                n, c, v, svc_nats, cfg.http_base_url);
            // The take raises the approval request in the same act that
            // freezes the body, so it is registered here and not with the
            // entity handlers, and before them, because it reads the context
            // the proposal registrar takes.
            auto takes = ores::reporting::messaging::cut_take_registrar::register_handlers(n, c, v);
            for (auto& sub : takes)
                subs.push_back(std::move(sub));
            // The proposal operations raise approval requests, so they are
            // registered here and not with the entity handlers.
            auto proposals =
                ores::reporting::messaging::engine_settings_proposal_registrar::register_handlers(
                    n, std::move(c), std::move(v));
            for (auto& sub : proposals)
                subs.push_back(std::move(sub));
            return subs;
        },
        [&nats, &svc_nats, db_ctx, cut_authorisations](boost::asio::io_context& ioc) {
            // Reconcile scheduler jobs for all unscheduled report definitions.
            auto reconciler = std::make_shared<report_scheduling_service>(db_ctx, svc_nats);
            boost::asio::co_spawn(
                ioc, [reconciler]() { return reconciler->reconcile(); }, boost::asio::detached);

            // A decision the request's event never announced is applied within
            // the interval, because the sweep reads the store rather than
            // listening to it.
            constexpr std::uint32_t reconcile_interval_seconds = 60;
            auto cut_authorisation_sweep = std::make_shared<app::cut_authorisation_reconciler>(
                reconcile_interval_seconds, cut_authorisations);
            boost::asio::co_spawn(
                ioc,
                [cut_authorisation_sweep]() { return cut_authorisation_sweep->run(); },
                boost::asio::detached);

            auto hb = std::make_shared<ores::service::service::heartbeat_publisher>(
                std::string(service_name), std::string(service_version), nats);
            boost::asio::co_spawn(ioc, [hb]() { return hb->run(); }, boost::asio::detached);
        });

    event_source.stop();
    co_return;
}

}
