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
#include "ores.variability.core/messaging/registrar.hpp"
#include "ores.variability.core/messaging/system_setting_handler.hpp"

namespace ores::variability::messaging {

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;
    constexpr auto queue = "ores.variability.service";
    subs.push_back(nats.queue_subscribe(
        std::string(list_settings_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            system_setting_handler h(nats, ctx, verifier);
            h.list(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_setting_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            system_setting_handler h(nats, ctx, verifier);
            h.save(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(delete_setting_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            system_setting_handler h(nats, ctx, verifier);
            h.remove(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_setting_history_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            system_setting_handler h(nats, ctx, verifier);
            h.history(std::move(msg));
        }));

    return subs;
}

}
