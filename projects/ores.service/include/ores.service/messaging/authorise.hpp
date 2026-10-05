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
#ifndef ORES_SERVICE_MESSAGING_AUTHORISE_HPP
#define ORES_SERVICE_MESSAGING_AUTHORISE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>
#include <string_view>

namespace ores::service::messaging {

/**
 * @brief The caller's context when the caller holds @p permission; otherwise
 * replies with the error and returns nothing.
 */
inline std::optional<ores::database::context>
authorise(ores::nats::service::client& nats,
          const ores::database::context& base,
          const ores::nats::message& msg,
          const std::optional<ores::security::jwt::jwt_authenticator>& verifier,
          std::string_view permission) {
    auto ctx = ores::service::service::make_request_context(base, msg, verifier);
    if (!ctx) {
        error_reply(nats, msg, ctx.error());
        return std::nullopt;
    }
    if (!has_permission(*ctx, permission)) {
        error_reply(nats, msg, ores::service::error_code::forbidden);
        return std::nullopt;
    }
    return *ctx;
}

}

#endif
