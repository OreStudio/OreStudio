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
#include "ores.iam.core/service/tenant_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::iam::service {

using namespace ores::logging;

tenant_service::tenant_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::tenant> tenant_service::list_tenants(std::uint32_t offset,
                                                         std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all tenants";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t tenant_service::count_tenants() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total tenants count";
    return repo_.get_total_tenant_count(ctx_);
}


std::optional<domain::tenant> tenant_service::get_tenant_at_version(const std::string& id,
                                                                    std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting tenant at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::tenant> tenant_service::get_tenant(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting tenant. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void tenant_service::save_tenant(const domain::tenant& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Tenant id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving tenant. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved tenant. " << "id: " << v.id;
}

void tenant_service::save_tenants(const std::vector<domain::tenant>& tenants) {
    for (const auto& e : tenants) {
        if (e.id.is_nil())
            throw std::invalid_argument("Tenant id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << tenants.size() << " tenants";
    auto ts = tenants;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void tenant_service::delete_tenant(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing tenant. " << "id: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed tenant. " << "id: " << id;
}

void tenant_service::delete_tenants(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::tenant> tenant_service::get_tenant_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for tenant. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
