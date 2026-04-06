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
#include "ores.marketdata.core/messaging/market_series_handler.hpp"
#include "ores.marketdata.core/messaging/market_observation_handler.hpp"
#include "ores.marketdata.core/messaging/market_fixing_handler.hpp"
#include "ores.marketdata.core/messaging/import_handler.hpp"

namespace ores::marketdata::messaging {

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier,
    std::string http_base_url) {
    std::vector<ores::nats::service::subscription> subs;
    constexpr auto queue = "ores.marketdata.service";

    // Market series
    subs.push_back(nats.queue_subscribe(
        std::string(get_market_series_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            market_series_handler h(nats, ctx, verifier);
            h.list(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_market_series_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            market_series_handler h(nats, ctx, verifier);
            h.save(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(delete_market_series_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            market_series_handler h(nats, ctx, verifier);
            h.del(std::move(msg));
        }));

    // Market observations
    subs.push_back(nats.queue_subscribe(
        std::string(get_market_observations_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            market_observation_handler h(nats, ctx, verifier);
            h.list(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_market_observations_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            market_observation_handler h(nats, ctx, verifier);
            h.save(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(delete_market_observations_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            market_observation_handler h(nats, ctx, verifier);
            h.del(std::move(msg));
        }));

    // Market fixings
    subs.push_back(nats.queue_subscribe(
        std::string(get_market_fixings_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            market_fixing_handler h(nats, ctx, verifier);
            h.list(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_market_fixings_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            market_fixing_handler h(nats, ctx, verifier);
            h.save(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(delete_market_fixings_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            market_fixing_handler h(nats, ctx, verifier);
            h.del(std::move(msg));
        }));

    // Import
    subs.push_back(nats.queue_subscribe(
        std::string(import_market_data_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            import_handler h(nats, ctx, verifier);
            h.import(std::move(msg));
        }));

    // Export to storage (report execution workflow)
    subs.push_back(nats.queue_subscribe(
        std::string(export_market_data_to_storage_request::nats_subject), queue,
        [&nats, ctx, verifier, http_base_url](ores::nats::message msg) mutable {
            market_series_handler h(nats, ctx, verifier, http_base_url);
            h.export_to_storage(std::move(msg));
        }));

    return subs;
}

}
