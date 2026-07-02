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
#include "ores.refdata.core/messaging/zero_convention_registrar.hpp"
#include "ores.refdata.api/messaging/zero_convention_protocol.hpp"
#include "ores.refdata.core/messaging/zero_convention_handler.hpp"
#include <memory>

namespace ores::refdata::messaging {

namespace {
static constexpr std::string_view queue_group = "ores.refdata.service";
} // namespace

std::vector<ores::nats::service::subscription>
register_zero_convention_handlers(ores::nats::service::client& nats,
                                  ores::database::context ctx,
                                  std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;
    auto h = std::make_shared<zero_convention_handler>(nats, std::move(ctx), std::move(verifier));
    subs.push_back(nats.queue_subscribe(get_zero_conventions_request::nats_subject,
                                        queue_group,
                                        [h](ores::nats::message msg) { h->list(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(save_zero_convention_request::nats_subject,
                                        queue_group,
                                        [h](ores::nats::message msg) { h->save(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        delete_zero_convention_request::nats_subject, queue_group, [h](ores::nats::message msg) {
            h->remove(std::move(msg));
        }));
    subs.push_back(
        nats.queue_subscribe(get_zero_convention_history_request::nats_subject,
                             queue_group,
                             [h](ores::nats::message msg) { h->history(std::move(msg)); }));
    return subs;
}

} // namespace ores::refdata::messaging
