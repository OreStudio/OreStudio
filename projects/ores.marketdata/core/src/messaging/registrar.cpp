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
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/messaging/curve_snapshot_handler.hpp"
#include "ores.marketdata.core/messaging/feed_binding_activity_handler.hpp"
#include "ores.marketdata.core/messaging/feed_binding_history_provider_registrar.hpp"
#include "ores.marketdata.core/messaging/feed_binding_registrar.hpp"
#include "ores.marketdata.core/messaging/import_handler.hpp"
#include "ores.marketdata.core/messaging/manual_point_handler.hpp"
#include "ores.marketdata.core/messaging/market_fixing_registrar.hpp"
#include "ores.marketdata.core/messaging/market_observation_registrar.hpp"
#include "ores.marketdata.core/messaging/market_series_asset_class_history_provider_registrar.hpp"
#include "ores.marketdata.core/messaging/market_series_asset_class_registrar.hpp"
#include "ores.marketdata.core/messaging/market_series_handler.hpp"
#include "ores.marketdata.core/messaging/market_series_history_provider_registrar.hpp"
#include "ores.marketdata.core/messaging/market_series_registrar.hpp"
#include "ores.marketdata.core/messaging/observation_lineage_history_provider_registrar.hpp"
#include "ores.marketdata.core/messaging/observation_lineage_registrar.hpp"
#include "ores.marketdata.core/messaging/ore_export_handler.hpp"
#include "ores.marketdata.core/messaging/oresmd_schema_handler.hpp"
#include "ores.marketdata.core/messaging/oresmd_uri_handler.hpp"
#include "ores.marketdata.core/messaging/publish_from_dq_handler.hpp"
#include "ores.marketdata.core/messaging/series_classification_rule_history_provider_registrar.hpp"
#include "ores.marketdata.core/messaging/series_classification_rule_registrar.hpp"
#include "ores.marketdata.core/messaging/series_evolution_handler.hpp"
#include "ores.marketdata.core/messaging/series_identity_handler.hpp"
#include "ores.marketdata.core/messaging/series_slice_handler.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.service/messaging/step_command_stream.hpp"
#include <array>
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
}

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
                             ores::database::context ctx,
                             ores::nats::service::nats_client& auth_nats,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier,
                             std::string http_base_url) {
    std::vector<ores::nats::service::subscription> subs;
    constexpr auto queue = "ores.marketdata.service";

    // Generated per-entity registrars (market series, fixings, observations,
    // feed bindings, observation lineages, classification rules and the
    // series asset-class junction). Each wires
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
    fold(register_market_series_asset_class_handlers(nats, ctx, verifier));

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

    // The identity backfill: repairs the projection for series the write path
    // never saw, such as rows restored from a dump or written before the
    // projection existed.
    subs.push_back(nats.queue_subscribe(std::string(backfill_series_identity_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            market_series_handler h(nats, ctx, verifier);
                                            h.backfill_identity(std::move(msg));
                                        }));

    // The single-instant snapshot of a curve or grid, for curve/grid viewers.
    subs.push_back(nats.queue_subscribe(std::string(get_curve_snapshot_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            curve_snapshot_handler h(nats, ctx, verifier);
                                            h.get_snapshot(std::move(msg));
                                        }));

    // A whole composite object as instants by nodes, with the change each node
    // shows against the shape.
    subs.push_back(nats.queue_subscribe(std::string(get_series_evolution_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            series_evolution_handler h(nats, ctx, verifier);
                                            h.get_evolution(std::move(msg));
                                        }));

    // One component of a composite object as a term structure at each instant
    // in a range.
    subs.push_back(nats.queue_subscribe(std::string(get_series_slice_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            series_slice_handler h(nats, ctx, verifier);
                                            h.get_slice(std::move(msg));
                                        }));

    // Typed series identity: resolves a series from the identity fields a
    // caller states, so no caller builds an oresmd URI or matches on one.
    subs.push_back(nats.queue_subscribe(std::string(resolve_series_identity_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            series_identity_handler h(nats, ctx, verifier);
                                            h.resolve(std::move(msg));
                                        }));

    // The series a party can see, each as the typed identity the curve reads take.
    subs.push_back(nats.queue_subscribe(std::string(list_series_identities_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            series_identity_handler h(nats, ctx, verifier);
                                            h.list(std::move(msg));
                                        }));

    // A person's manual point on one node of a composite object: keyed so that it
    // owns the node, and cleared so that the feed value shows again.
    subs.push_back(nats.queue_subscribe(std::string(key_manual_point_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            manual_point_handler h(nats, ctx, verifier);
                                            h.key(std::move(msg));
                                        }));
    subs.push_back(nats.queue_subscribe(std::string(clear_manual_point_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            manual_point_handler h(nats, ctx, verifier);
                                            h.clear(std::move(msg));
                                        }));

    // What each feed binding has written: the series of its source's newest
    // observation and when the source last ticked.
    subs.push_back(
        nats.queue_subscribe(std::string(list_feed_binding_activity_request::nats_subject),
                             queue,
                             [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                 feed_binding_activity_handler h(nats, ctx, verifier);
                                 h.list(std::move(msg));
                             }));

    // The oresmd schema: what a URI may hold, for a client that edits one.
    subs.push_back(nats.queue_subscribe(std::string(describe_oresmd_schema_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            oresmd_schema_handler h(nats, ctx, verifier);
                                            h.describe(std::move(msg));
                                        }));

    // Read an oresmd URI into its parts, and write one from its parts, through
    // the codec.
    subs.push_back(nats.queue_subscribe(std::string(parse_oresmd_uri_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            oresmd_uri_handler h(nats, ctx, verifier);
                                            h.parse(std::move(msg));
                                        }));
    subs.push_back(nats.queue_subscribe(std::string(compose_oresmd_uri_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            oresmd_uri_handler h(nats, ctx, verifier);
                                            h.compose(std::move(msg));
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
    subs.push_back(nats.queue_subscribe(std::string(export_market_data_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            ore_export_handler h(nats, ctx, verifier);
                                            h.write_all(std::move(msg));
                                        }));

    // Publish-from-DQ workflow step handler
    {
        auto pdq = std::make_shared<publish_from_dq_handler>(nats, ctx);
        static constexpr std::array publish_subjects{
            "marketdata.v1.ops.publish_market_data_observations_from_dq"};
        const auto step_stream =
            ores::service::messaging::ensure_step_stream(nats, queue, publish_subjects);
        for (const auto subject : publish_subjects) {
            subs.push_back(ores::service::messaging::subscribe_step_command(
                nats, subject, queue, step_stream, [pdq](ores::nats::message msg) {
                    pdq->handle(std::move(msg));
                }));
        }
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
        register_market_series_asset_class_history_provider(hist_registry);
        register_feed_binding_history_provider(hist_registry);
        register_observation_lineage_history_provider(hist_registry);
        register_series_classification_rule_history_provider(hist_registry);

        subs.push_back(ores::history::messaging::register_history_handlers(
            nats, hist_registry, "marketdata", queue, ctx, verifier));
    }

    return subs;
}

}
