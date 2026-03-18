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
#include <memory>
#include <optional>
#include <vector>
#include "ores.assets/messaging/assets_protocol.hpp"
#include "ores.assets/messaging/image_handler.hpp"
#include "ores.assets/messaging/registrar.hpp"

namespace ores::assets::messaging {

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;

    auto h = std::make_shared<image_handler>(nats, std::move(ctx),
        std::move(verifier));

    subs.push_back(nats.queue_subscribe(
        get_images_request::nats_subject, "ores.assets.service",
        [h](ores::nats::message msg) { h->get(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        list_images_request::nats_subject, "ores.assets.service",
        [h](ores::nats::message msg) { h->list(std::move(msg)); }));

    subs.push_back(nats.queue_subscribe(
        save_image_request::nats_subject, "ores.assets.service",
        [h](ores::nats::message msg) { h->save(std::move(msg)); }));

    return subs;
}

}
