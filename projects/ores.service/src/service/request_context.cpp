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
#include "ores.service/service/request_context.hpp"

#include <vector>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/string_generator.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.security/jwt/jwt_error.hpp"

namespace ores::service::service {

namespace {

using namespace ores::logging;
inline static std::string_view logger_name = "ores.service.service.request_context";
static auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

} // namespace

std::expected<ores::database::context, ores::service::error_code>
make_context_from_jwt(
    const ores::database::context& base_ctx,
    const std::string& token,
    const ores::security::jwt::jwt_authenticator& verifier) {

    auto claims = verifier.validate(token);
    if (!claims) {
        if (claims.error() == ores::security::jwt::jwt_error::expired_token)
            return std::unexpected(ores::service::error_code::token_expired);
        return std::unexpected(ores::service::error_code::unauthorized);
    }

    const auto tenant_id_str = claims->tenant_id.value_or("");
    if (tenant_id_str.empty())
        return std::unexpected(ores::service::error_code::unauthorized);

    auto tid_result = ores::utility::uuid::tenant_id::from_string(tenant_id_str);
    if (!tid_result)
        return std::unexpected(ores::service::error_code::unauthorized);

    if (!claims->party_id || claims->party_id->empty())
        return base_ctx.with_tenant(*tid_result, claims->username.value_or(""))
                       .with_roles(claims->roles);

    try {
        boost::uuids::string_generator sg;
        boost::uuids::uuid party_id = sg(*claims->party_id);
        std::vector<boost::uuids::uuid> visible_ids;
        for (const auto& pid_str : claims->visible_party_ids)
            visible_ids.push_back(sg(pid_str));
        return base_ctx.with_party(*tid_result, party_id,
            std::move(visible_ids), claims->username.value_or(""))
                       .with_roles(claims->roles);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to parse party UUIDs from JWT: " << e.what();
        return base_ctx.with_tenant(*tid_result, claims->username.value_or(""))
                       .with_roles(claims->roles);
    }
}

std::expected<ores::database::context, ores::service::error_code>
make_request_context(
    const ores::database::context& base_ctx,
    const ores::nats::message& msg,
    const std::optional<ores::security::jwt::jwt_authenticator>& verifier) {

    if (!verifier)
        return base_ctx;

    // Check X-Delegated-Authorization first: a downstream service forwarded
    // the original end-user JWT.  Validate it and build the user's full context
    // (tenant, party, actor, roles).  An expired delegated token is a hard
    // reject — the original session has ended.
    const auto del_it = msg.headers.find(
        std::string(ores::nats::headers::delegated_authorization));
    if (del_it != msg.headers.end()) {
        const auto& val = del_it->second;
        if (!val.starts_with(ores::nats::headers::bearer_prefix))
            return std::unexpected(ores::service::error_code::unauthorized);
        return make_context_from_jwt(
            base_ctx, val.substr(ores::nats::headers::bearer_prefix.size()),
            *verifier);
    }

    // Fall through to the standard Authorization header.
    const auto it = msg.headers.find(std::string(ores::nats::headers::authorization));
    if (it == msg.headers.end())
        return std::unexpected(ores::service::error_code::unauthorized);

    const auto& val = it->second;
    if (!val.starts_with(ores::nats::headers::bearer_prefix))
        return std::unexpected(ores::service::error_code::unauthorized);

    return make_context_from_jwt(
        base_ctx, val.substr(ores::nats::headers::bearer_prefix.size()), *verifier);
}

} // namespace ores::service::service
