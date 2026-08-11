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
#include "ores.iam.core/service/tenant_status_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::iam::service {

using namespace ores::logging;

tenant_status_service::tenant_status_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::tenant_status> tenant_status_service::list_statuses(std::uint32_t offset,
                                                                        std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all tenant statuses";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t tenant_status_service::count_statuses() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total tenant statuses count";
    return repo_.get_total_status_count(ctx_);
}


std::optional<domain::tenant_status>
tenant_status_service::get_status_at_version(const std::string& status, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting tenant status at version. " << "status: " << status
                               << " version: " << version;
    return repo_.read_at_version(ctx_, status, version);
}

std::optional<domain::tenant_status> tenant_status_service::find_status(const std::string& status) {
    BOOST_LOG_SEV(lg(), debug) << "Finding tenant status. " << "status: " << status;
    auto results = repo_.read_latest(ctx_, status);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void tenant_status_service::save_status(const domain::tenant_status& v) {
    if (v.status.empty())
        throw std::invalid_argument("Tenant Status status cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving tenant status. " << "status: " << v.status;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved tenant status. " << "status: " << v.status;
}

void tenant_status_service::save_statuses(const std::vector<domain::tenant_status>& statuses) {
    for (const auto& e : statuses) {
        if (e.status.empty())
            throw std::invalid_argument("Tenant Status status cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << statuses.size() << " tenant statuses";
    auto ts = statuses;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void tenant_status_service::delete_status(const std::string& status) {
    BOOST_LOG_SEV(lg(), debug) << "Removing tenant status. " << "status: " << status;
    repo_.remove(ctx_, status);
    BOOST_LOG_SEV(lg(), info) << "Removed tenant status. " << "status: " << status;
}

void tenant_status_service::delete_statuses(const std::vector<std::string>& statuss) {
    repo_.remove(ctx_, statuss);
}

std::vector<domain::tenant_status>
tenant_status_service::get_status_history(const std::string& status) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for tenant status. " << "status: " << status;
    return repo_.read_all(ctx_, status);
}

}
