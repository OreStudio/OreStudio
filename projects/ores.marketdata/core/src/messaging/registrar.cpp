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
#include "ores.marketdata.core/messaging/registrar.hpp"
#include "ores.history.core/messaging/registrar.hpp"
#include "ores.history.core/service/dispatch_registry.hpp"
#include "ores.marketdata.api/messaging/market_series_export_protocol.hpp"
#include "ores.marketdata.core/messaging/curve_snapshot_handler.hpp"
#include "ores.marketdata.core/messaging/feed_binding_history_provider_registrar.hpp"
#include "ores.marketdata.core/messaging/feed_binding_registrar.hpp"
#include "ores.marketdata.core/messaging/import_handler.hpp"
#include "ores.marketdata.core/messaging/ore_export_handler.hpp"
#include "ores.marketdata.core/messaging/market_fixing_registrar.hpp"
#include "ores.marketdata.core/messaging/market_observation_registrar.hpp"
#include "ores.marketdata.core/messaging/market_series_handler.hpp"
#include "ores.marketdata.core/messaging/market_series_history_provider_registrar.hpp"
#include "ores.marketdata.core/messaging/market_series_registrar.hpp"
#include "ores.marketdata.core/messaging/observation_lineage_history_provider_registrar.hpp"
#include "ores.marketdata.core/messaging/observation_lineage_registrar.hpp"
#include "ores.marketdata.core/messaging/publish_from_dq_handler.hpp"
#include "ores.marketdata.core/messaging/series_classification_rule_history_provider_registrar.hpp"
#include "ores.marketdata.core/messaging/series_classification_rule_registrar.hpp"
#include "ores.nats/domain/message.hpp"
#include <functional>
#include <memory>
#include <utility>

namespace ores::marketdata::messaging {

namespace {
// Function-local static: must outlive the history.v1.get subscription (see
// ores::history::messaging::register_history_handlers's doc), and
// register_handlers is only ever called once per service process.
ores::history::service::dispatch_registry& history_registry() {
    static ores::history::service::dispatch_registry instance;
    return instance;
}
} // namespace

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
                             ores::database::context ctx,
                             ores::nats::service::nats_client& auth_nats,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier,
                             std::string http_base_url) {
    std::vector<ores::nats::service::subscription> subs;
    constexpr auto queue = "ores.marketdata.service";

    // Generated per-entity registrars (market series, fixings, observations,
    // feed bindings, observation lineages, classification rules). Each wires
    // the canonical CRUD set -- list/get/get-many/put/put-many/delete/
    // delete-many and the version reads -- to the generated handler.
    // subscription is move-only, so fold each returned vector in with move
    // iterators.
    const auto fold = [&subs](std::vector<ores::nats::service::subscription> s) {
        subs.insert(
            subs.end(), std::make_move_iterator(s.begin()), std::make_move_iterator(s.end()));
    };
    fold(register_market_series_handlers(nats, ctx, verifier));
    fold(register_market_fixing_handlers(nats, ctx, verifier));
    fold(register_market_observation_handlers(nats, ctx, verifier));
    fold(register_feed_binding_handlers(nats, ctx, verifier));
    fold(register_observation_lineage_handlers(nats, ctx, verifier));
    fold(register_series_classification_rule_handlers(nats, ctx, verifier));

    // Market series export: the one market-series verb with no generated
    // protocol model, because it is a report-feed read rather than CRUD. It
    // lives in its own header and is registered here by hand.
    subs.push_back(nats.queue_subscribe(
        std::string(export_market_data_to_storage_request::nats_subject),
        queue,
        [&nats, ctx, verifier, http_base_url](ores::nats::message msg) mutable {
            market_series_handler h(nats, ctx, verifier);
            h.export_to_storage(std::move(msg), http_base_url);
        }));

    // Curve snapshots (as-of / as-of-buckets, for curve/grid viewers)
    subs.push_back(nats.queue_subscribe(std::string(get_curve_snapshot_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            curve_snapshot_handler h(nats, ctx, verifier);
                                            h.get_snapshot(std::move(msg));
                                        }));

    subs.push_back(
        nats.queue_subscribe(std::string(get_curve_snapshot_buckets_request::nats_subject),
                             queue,
                             [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                 curve_snapshot_handler h(nats, ctx, verifier);
                                 h.get_snapshot_buckets(std::move(msg));
                             }));

    // Import
    subs.push_back(
        nats.queue_subscribe(std::string(import_market_data_request::nats_subject),
                             queue,
                             [&nats, ctx, verifier, &auth_nats](ores::nats::message msg) mutable {
                                 import_handler h(nats, ctx, verifier, auth_nats);
                                 h.import(std::move(msg));
                             }));

    // Export
    subs.push_back(
        nats.queue_subscribe(std::string(export_market_data_request::nats_subject),
                             queue,
                             [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                 ore_export_handler h(nats, ctx, verifier);
                                 h.write_all(std::move(msg));
                             }));

    // Publish-from-DQ workflow step handler
    {
        auto pdq = std::make_shared<publish_from_dq_handler>(nats, ctx);
        subs.push_back(
            nats.queue_subscribe("marketdata.v1.market-data-observations.publish-from-dq",
                                 queue,
                                 [pdq](ores::nats::message msg) { pdq->handle(std::move(msg)); }));
    }

    // ----------------------------------------------------------------
    // Generic history.v1.get subject. The registrar resolves each request
    // into a scoped context exactly like every other subject
    // (make_request_context), so a provider sees the same
    // tenant/party/roles/workspace visibility any other handler in this
    // file would. The two no_audit_columns entities have no provider: the
    // facet is withheld for them, because a version type has to carry an
    // actor as well as the timestamp and they carry neither.
    // ----------------------------------------------------------------
    {
        auto& hist_registry = history_registry();
        register_market_series_history_provider(hist_registry);
        register_feed_binding_history_provider(hist_registry);
        register_observation_lineage_history_provider(hist_registry);
        register_series_classification_rule_history_provider(hist_registry);

        subs.push_back(ores::history::messaging::register_history_handlers(
            nats, hist_registry, "marketdata", queue, ctx, verifier));
    }

    return subs;
}

}
