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
#include "ores.assets.core/messaging/registrar.hpp"
#include "ores.assets.core/messaging/image_registrar.hpp"
#include "ores.assets.core/messaging/publish_from_dq_handler.hpp"
#include "ores.assets.core/messaging/tag_registrar.hpp"
#include <iterator>
#include <memory>
#include <utility>

namespace ores::assets::messaging {

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
                             ores::database::context ctx,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;

    // Generated per-entity registrars: images and tags each wire the standard
    // CRUD surface. The image_tag junction has no handler of its own, because
    // its protocol is generic. subscription is move-only, so each returned
    // vector folds in with move iterators. This aggregator is the only caller
    // of the generated registrars, which is what keeps application.cpp a
    // single call as the component gains entities.
    const auto fold = [&subs](std::vector<ores::nats::service::subscription> s) {
        subs.insert(
            subs.end(), std::make_move_iterator(s.begin()), std::make_move_iterator(s.end()));
    };
    fold(register_image_handlers(nats, ctx, verifier));
    fold(register_tag_handlers(nats, ctx, verifier));

    // ----------------------------------------------------------------
    // Publish-from-DQ workflow step handler
    // ----------------------------------------------------------------
    {
        auto pdq = std::make_shared<publish_from_dq_handler>(nats, std::move(ctx));
        subs.push_back(
            nats.queue_subscribe("assets.v1.images.publish-from-dq",
                                 "ores.assets.service",
                                 [pdq](ores::nats::message msg) { pdq->handle(std::move(msg)); }));
    }

    return subs;
}

}
