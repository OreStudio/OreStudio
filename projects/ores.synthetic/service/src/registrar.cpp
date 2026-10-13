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
#include "registrar.hpp"
#include "feed_config_handler.hpp"
#include "feed_overview_handler.hpp"
#include "feed_selection_handler.hpp"
#include "folder_feed_control_handler.hpp"
#include "fx_mixture_conversion_handler.hpp"
#include "fx_simple_conversion_handler.hpp"
#include "ir_curve_preview_handler.hpp"
#include "ir_parameter_conversion_handler.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.synthetic.api/messaging/feed_config_protocol.hpp"
#include "ores.synthetic.api/messaging/feed_selection_protocol.hpp"
#include "ores.synthetic.api/messaging/ir_curve_operations_protocol.hpp"
#include "simulate_handler.hpp"
#include "vintage_validity_handler.hpp"

namespace ores::synthetic::service {

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
                             ores::nats::service::nats_client& auth_nats,
                             std::shared_ptr<feed_controller> ctrl,
                             ores::database::context ctx,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier,
                             const feed_kind_registry& registry) {
    std::vector<ores::nats::service::subscription> subs;
    constexpr auto queue = "ores.synthetic.service";

    using namespace ores::marketdata::messaging;
    using namespace ores::synthetic::messaging;

    subs.push_back(nats.queue_subscribe(
        std::string(start_feed_request::nats_subject),
        queue,
        [&nats, &auth_nats, ctrl, ctx, verifier, &registry](ores::nats::message msg) {
            feed_config_handler h(nats, auth_nats, ctrl, ctx, verifier, registry);
            h.start(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(stop_feed_request::nats_subject),
        queue,
        [&nats, &auth_nats, ctrl, ctx, verifier, &registry](ores::nats::message msg) {
            feed_config_handler h(nats, auth_nats, ctrl, ctx, verifier, registry);
            h.stop(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(list_feeds_request::nats_subject),
        queue,
        [&nats, &auth_nats, ctrl, ctx, verifier, &registry](ores::nats::message msg) {
            feed_config_handler h(nats, auth_nats, ctrl, ctx, verifier, registry);
            h.list(std::move(msg));
        }));

    // One simulate subscription per registered kind: the subject and the
    // kind string both come from the registration, so a new kind needs no
    // edit here.
    for (const auto* k : registry.all()) {
        const std::string subject = k->simulate_subject;
        const std::string kind = k->kind;
        subs.push_back(nats.queue_subscribe(
            subject, queue, [&nats, ctx, verifier, &registry, kind](ores::nats::message msg) {
                simulate_handler h(nats, ctx, verifier, registry);
                h.simulate(std::move(msg), kind);
            }));
    }

    subs.push_back(nats.queue_subscribe(
        std::string(start_feeds_under_folder_request::nats_subject),
        queue,
        [&nats, &auth_nats, ctrl, ctx, verifier, &registry](ores::nats::message msg) {
            folder_feed_control_handler h(nats, ctrl, auth_nats, ctx, verifier, registry);
            h.start(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(stop_feeds_under_folder_request::nats_subject),
        queue,
        [&nats, &auth_nats, ctrl, ctx, verifier, &registry](ores::nats::message msg) {
            folder_feed_control_handler h(nats, ctrl, auth_nats, ctx, verifier, registry);
            h.stop(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_vintage_validity_request::nats_subject),
        queue,
        [&nats, &auth_nats, ctx, verifier, &registry](ores::nats::message msg) {
            vintage_validity_handler h(nats, auth_nats, ctx, verifier, registry);
            h.list(std::move(msg));
        }));

    // preview_shape is IR-only: there is no FX analogue, and the verb takes
    // its kind and its permission from that one registration.
    subs.push_back(nats.queue_subscribe(std::string(preview_ir_curve_shape_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier, &registry](ores::nats::message msg) {
                                            ir_curve_preview_handler h(
                                                nats, ctx, verifier, registry);
                                            h.preview_shape(std::move(msg), "ir_curve");
                                        }));

    // The mixture an engine change leaves: FX only, gated by the FX kind.
    subs.push_back(nats.queue_subscribe(
        std::string(ores::synthetic::messaging::convert_fx_mixture_request::nats_subject),
        queue,
        [&nats, ctx, verifier, &registry](ores::nats::message msg) {
            fx_mixture_conversion_handler h(nats, ctx, verifier, registry);
            h.convert(std::move(msg));
        }));

    // The children of a folder with the feeds under each and what runs, for the
    // feed overview and the feed tree. The read of any kind's feed configs opens it.
    subs.push_back(nats.queue_subscribe(
        std::string(ores::synthetic::messaging::list_feed_folders_request::nats_subject),
        queue,
        [&nats, ctrl, ctx, verifier, &registry](ores::nats::message msg) {
            feed_overview_handler h(nats, ctrl, ctx, verifier, registry);
            h.list(std::move(msg));
        }));

    // A page of the feeds in one folder, or of the feeds that match some words, for
    // the feed tree. The read of any kind's feed configs opens it.
    subs.push_back(nats.queue_subscribe(
        std::string(ores::synthetic::messaging::list_folder_feeds_request::nats_subject),
        queue,
        [&nats, ctrl, ctx, verifier, &registry](ores::nats::message msg) {
            feed_overview_handler h(nats, ctrl, ctx, verifier, registry);
            h.list_feeds(std::move(msg));
        }));

    // Start and stop of the feeds a person picked in the feed table, or the feeds
    // its filter matches. The read of every kind's feed configs opens them, as it
    // opens the folder verbs.
    subs.push_back(nats.queue_subscribe(
        std::string(ores::synthetic::messaging::start_selected_feeds_request::nats_subject),
        queue,
        [&nats, &auth_nats, ctrl, ctx, verifier, &registry](ores::nats::message msg) {
            feed_selection_handler h(nats, ctrl, auth_nats, ctx, verifier, registry);
            h.start(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(ores::synthetic::messaging::stop_selected_feeds_request::nats_subject),
        queue,
        [&nats, &auth_nats, ctrl, ctx, verifier, &registry](ores::nats::message msg) {
            feed_selection_handler h(nats, ctrl, auth_nats, ctx, verifier, registry);
            h.stop(std::move(msg));
        }));

    // The simple and the advanced view of an FX feed's mixture: FX only,
    // gated by the FX kind.
    subs.push_back(nats.queue_subscribe(
        std::string(ores::synthetic::messaging::convert_fx_simple_request::nats_subject),
        queue,
        [&nats, ctx, verifier, &registry](ores::nats::message msg) {
            fx_simple_conversion_handler h(nats, ctx, verifier, registry);
            h.convert(std::move(msg));
        }));

    // The simple and the advanced view of an IR feed's parameters: IR only,
    // gated by the IR kind.
    subs.push_back(nats.queue_subscribe(
        std::string(ores::synthetic::messaging::convert_ir_parameters_request::nats_subject),
        queue,
        [&nats, ctx, verifier, &registry](ores::nats::message msg) {
            ir_parameter_conversion_handler h(nats, ctx, verifier, registry);
            h.convert(std::move(msg));
        }));

    return subs;
}

}
