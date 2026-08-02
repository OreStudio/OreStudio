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
#include "ores.iam.core/service/internal_impersonation_service.hpp"
#include "ores.iam.core/service/authorization_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.security/jwt/jwt_claims.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::iam::service {

namespace {
using namespace ores::logging;
inline static std::string_view logger_name = "ores.iam.service.internal_impersonation_service";
auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}
} // namespace

internal_impersonation_service::internal_impersonation_service(
    ores::security::jwt::jwt_authenticator signer, visible_party_ids_fn visible_party_ids)
    : signer_(std::move(signer))
    , visible_party_ids_(std::move(visible_party_ids)) {}

std::string internal_impersonation_service::mint_token(const context& ctx,
                                                       const std::string& tenant_id,
                                                       const boost::uuids::uuid& account_id,
                                                       const boost::uuids::uuid& party_id,
                                                       const std::string& username,
                                                       std::chrono::seconds ttl) {
    ores::security::jwt::jwt_claims claims;
    claims.subject = boost::uuids::to_string(account_id);
    claims.username = username;
    claims.tenant_id = tenant_id;
    claims.party_id = boost::uuids::to_string(party_id);

    for (const auto& visible_id : visible_party_ids_(tenant_id, party_id))
        claims.visible_party_ids.push_back(boost::uuids::to_string(visible_id));

    authorization_service auth_svc(ctx);
    claims.roles = auth_svc.get_effective_permissions(account_id);

    // issued_at/expires_at are stamped last, immediately before signing --
    // not up front -- so the DB round-trip above (get_effective_permissions)
    // never eats into the caller-requested ttl. With a short ttl (this
    // service also backs short-lived step-up tokens), stamping the clock
    // before that round-trip let it expire before the caller could even use
    // it under load.
    const auto ttl_claims = ores::security::jwt::jwt_claims::with_ttl(ttl);
    claims.issued_at = ttl_claims.issued_at;
    claims.expires_at = ttl_claims.expires_at;

    auto token = signer_.create_token(claims);
    if (!token) {
        BOOST_LOG_SEV(lg(), error) << "Failed to mint internal impersonation token for account "
                                   << claims.subject << " (party " << *claims.party_id << ")";
        return {};
    }

    BOOST_LOG_SEV(lg(), info) << "Minted internal impersonation token: account " << claims.subject
                              << ", party " << *claims.party_id << ", ttl " << ttl.count() << "s";
    return *token;
}

}
