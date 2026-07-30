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
#include "ores.dq.core/messaging/badge_mapping_registrar.hpp"
#include "ores.dq.api/messaging/badge_mapping_protocol.hpp"
#include "ores.dq.core/messaging/badge_mapping_handler.hpp"
#include <memory>

namespace ores::dq::messaging {

namespace {
static constexpr std::string_view queue_group = "ores.dq.service";
} // namespace

std::vector<ores::nats::service::subscription>
register_badge_mapping_handlers(ores::nats::service::client& nats,
                                ores::database::context ctx,
                                std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;
    auto h = std::make_shared<badge_mapping_handler>(nats, std::move(ctx), std::move(verifier));
    return subs;
}

} // namespace ores::dq::messaging
