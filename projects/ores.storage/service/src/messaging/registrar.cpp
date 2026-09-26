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
#include "ores.storage.service/messaging/registrar.hpp"
#include "ores.storage.api/messaging/objects_protocol.hpp"
#include "ores.storage.core/messaging/objects_handler.hpp"
#include <string>
#include <utility>

namespace ores::storage::service::messaging {

using namespace ores::storage::messaging;

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
                             ores::database::context ctx,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier,
                             ores::storage::filesystem::local_store store) {
    std::vector<ores::nats::service::subscription> subs;
    const std::string queue = "ores.storage.service";

    subs.push_back(nats.queue_subscribe(
        std::string(put_objects_request::nats_subject), queue,
        [&nats, ctx, verifier, store](ores::nats::message msg) mutable {
            objects_handler h(nats, ctx, store, verifier);
            h.put(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_objects_request::nats_subject), queue,
        [&nats, ctx, verifier, store](ores::nats::message msg) mutable {
            objects_handler h(nats, ctx, store, verifier);
            h.get(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(delete_objects_request::nats_subject), queue,
        [&nats, ctx, verifier, store](ores::nats::message msg) mutable {
            objects_handler h(nats, ctx, store, verifier);
            h.remove(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(list_objects_request::nats_subject), queue,
        [&nats, ctx, verifier, store](ores::nats::message msg) mutable {
            objects_handler h(nats, ctx, store, verifier);
            h.list(std::move(msg));
        }));

    return subs;
}

}
