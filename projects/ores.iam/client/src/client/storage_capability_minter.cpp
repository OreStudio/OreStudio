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
#include "ores.iam.client/client/storage_capability_minter.hpp"
#include "ores.iam.api/messaging/storage_capability_operations_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include <exception>

namespace ores::iam::client {

using namespace ores::logging;
using ores::security::jwt::storage_grant;

namespace {

auto& lg() {
    static auto instance =
        ores::logging::make_logger("ores.iam.client.storage_capability_minter");
    return instance;
}

}

storage_capability_minter make_storage_capability_minter(ores::nats::service::nats_client& nats) {
    return [&nats](std::string_view tenant_id,
                   const std::vector<storage_grant>& grants) -> std::optional<std::string> {
        messaging::mint_storage_capability_request request;
        request.tenant_id = std::string(tenant_id);
        request.grants = grants;
        const auto& codec = ores::nats::default_wire_codec();
        try {
            const auto reply = nats.authenticated_request(
                messaging::mint_storage_capability_request::nats_subject, codec.encode(request));
            auto response = codec.decode<messaging::mint_storage_capability_response>(reply.data);
            if (!response || !response->success || response->token.empty()) {
                BOOST_LOG_SEV(lg(), warn)
                    << "No storage capability for tenant " << tenant_id << ": "
                    << (response ? response->message : "the answer could not be read");
                return std::nullopt;
            }
            return response->token;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn) << "No storage capability for tenant " << tenant_id
                                      << ": the mint failed: " << e.what();
            return std::nullopt;
        }
    };
}

}
