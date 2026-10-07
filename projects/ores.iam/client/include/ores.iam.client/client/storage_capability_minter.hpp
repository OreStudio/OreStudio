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
#ifndef ORES_IAM_CLIENT_CLIENT_STORAGE_CAPABILITY_MINTER_HPP
#define ORES_IAM_CLIENT_CLIENT_STORAGE_CAPABILITY_MINTER_HPP

#include "ores.iam.client/export.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.security/jwt/jwt_claims.hpp"
#include <functional>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::iam::client {

/**
 * @brief Asks IAM for a storage capability naming a tenant's objects.
 *
 * The client must carry the calling service's identity, so build it with
 * @ref make_service_token_provider. The returned minter calls
 * =iam.v1.ops.mint_storage_capability= and answers the signed token, or nothing
 * when IAM refuses or does not answer.
 */
using storage_capability_minter = std::function<std::optional<std::string>(
    std::string_view tenant_id, const std::vector<ores::security::jwt::storage_grant>& grants)>;

/**
 * @brief Builds a minter over the given NATS client.
 *
 * @param nats The service's own client; it must outlive the minter.
 */
ORES_IAM_CLIENT_EXPORT storage_capability_minter
make_storage_capability_minter(ores::nats::service::nats_client& nats);

}

#endif
