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
#include "ores.refdata.core/service/tenant_party_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include "ores.refdata.core/service/party_service.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <algorithm>
#include <exception>

namespace ores::refdata::service {

using namespace ores::logging;

namespace {
inline auto& lg() {
    static auto instance = make_logger("ores.refdata.service.tenant_party_service");
    return instance;
}

messaging::list_tenant_parties_response refused(std::string message) {
    return messaging::list_tenant_parties_response{.success = false, .message = std::move(message)};
}
} // namespace

tenant_party_service::tenant_party_service(context caller)
    : caller_(std::move(caller)) {}

messaging::list_tenant_parties_response
tenant_party_service::list_tenant_parties(const messaging::list_tenant_parties_request& request) {
    if (!caller_.tenant_id().is_system())
        return refused("The parties of another tenant are read from the system tenant.");

    const auto target = utility::uuid::tenant_id::from_string(request.tenant_id);
    if (!target)
        return refused("The tenant id could not be read.");
    if (target->is_system())
        return refused("The system tenant's parties are not read through this request.");

    BOOST_LOG_SEV(lg(), debug) << "Reading the parties of tenant " << request.tenant_id;
    try {
        party_service parties(caller_.with_tenant(*target, caller_.actor()));
        const auto page = parties.list_parties(messaging::list_parties_request{
            .offset = request.offset,
            .limit = std::clamp(request.limit, std::uint32_t{1}, max_limit)});
        if (page.result.outcome != utility::domain::outcome::ok)
            return refused(page.result.message);
        return messaging::list_tenant_parties_response{
            .success = true, .parties = page.parties, .total = page.total};
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Reading the parties of tenant " << request.tenant_id
                                   << " failed: " << e.what();
        return refused(e.what());
    }
}

}
