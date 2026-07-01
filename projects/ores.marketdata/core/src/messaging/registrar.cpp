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
#include "ores.marketdata.core/messaging/feed_binding_handler.hpp"
#include "ores.marketdata.core/messaging/import_handler.hpp"
#include "ores.nats/domain/message.hpp"
#include <functional>
#include "ores.marketdata.core/messaging/market_fixing_handler.hpp"
#include "ores.marketdata.core/messaging/market_observation_handler.hpp"
#include "ores.marketdata.core/messaging/market_series_handler.hpp"
#include "ores.marketdata.api/messaging/market_series_export_protocol.hpp"

namespace ores::marketdata::messaging {

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
                             ores::database::context ctx,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier,
                             std::string http_base_url,
                             std::function<void()> on_feed_bindings_changed) {
    std::vector<ores::nats::service::subscription> subs;
    constexpr auto queue = "ores.marketdata.service";

    // Market series
    subs.push_back(nats.queue_subscribe(std::string(get_market_series_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            market_series_handler h(nats, ctx, verifier);
                                            h.list(std::move(msg));
                                        }));

    subs.push_back(nats.queue_subscribe(std::string(save_market_series_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            market_series_handler h(nats, ctx, verifier);
                                            h.save(std::move(msg));
                                        }));

    subs.push_back(nats.queue_subscribe(std::string(delete_market_series_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            market_series_handler h(nats, ctx, verifier);
                                            h.remove(std::move(msg));
                                        }));

    subs.push_back(nats.queue_subscribe(
        std::string(export_market_data_to_storage_request::nats_subject),
        queue,
        [&nats, ctx, verifier, http_base_url](ores::nats::message msg) mutable {
            market_series_handler h(nats, ctx, verifier);
            h.export_to_storage(std::move(msg), http_base_url);
        }));

    // Market observations
    subs.push_back(nats.queue_subscribe(std::string(get_market_observations_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            market_observation_handler h(nats, ctx, verifier);
                                            h.list(std::move(msg));
                                        }));

    subs.push_back(nats.queue_subscribe(std::string(save_market_observation_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            market_observation_handler h(nats, ctx, verifier);
                                            h.save(std::move(msg));
                                        }));

    subs.push_back(
        nats.queue_subscribe(std::string(delete_market_observation_request::nats_subject),
                             queue,
                             [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                 market_observation_handler h(nats, ctx, verifier);
                                 h.remove(std::move(msg));
                             }));

    // Market fixings
    subs.push_back(nats.queue_subscribe(std::string(get_market_fixings_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            market_fixing_handler h(nats, ctx, verifier);
                                            h.list(std::move(msg));
                                        }));

    subs.push_back(nats.queue_subscribe(std::string(save_market_fixing_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            market_fixing_handler h(nats, ctx, verifier);
                                            h.save(std::move(msg));
                                        }));

    subs.push_back(nats.queue_subscribe(std::string(delete_market_fixing_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            market_fixing_handler h(nats, ctx, verifier);
                                            h.remove(std::move(msg));
                                        }));

    // Feed bindings
    subs.push_back(nats.queue_subscribe(std::string(get_feed_bindings_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            feed_binding_handler h(nats, ctx, verifier);
                                            h.list(std::move(msg));
                                        }));

    subs.push_back(nats.queue_subscribe(std::string(save_feed_binding_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            feed_binding_handler h(nats, ctx, verifier);
                                            h.save(std::move(msg));
                                        }));

    subs.push_back(nats.queue_subscribe(std::string(delete_feed_binding_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            feed_binding_handler h(nats, ctx, verifier);
                                            h.remove(std::move(msg));
                                        }));

    subs.push_back(nats.queue_subscribe(std::string(get_feed_binding_history_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            feed_binding_handler h(nats, ctx, verifier);
                                            h.history(std::move(msg));
                                        }));

    // Feed binding change notifications → refresh ingest loop subscriptions
    if (on_feed_bindings_changed) {
        subs.push_back(nats.subscribe(
            "ores.marketdata.feed_bindings",
            [on_feed_bindings_changed](ores::nats::message /*msg*/) mutable {
                on_feed_bindings_changed();
            }));
    }

    // Import
    subs.push_back(nats.queue_subscribe(std::string(import_market_data_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            import_handler h(nats, ctx, verifier);
                                            h.import(std::move(msg));
                                        }));

    return subs;
}

}
