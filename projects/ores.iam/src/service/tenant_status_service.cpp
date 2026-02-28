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
#include "ores.iam/service/tenant_status_service.hpp"

#include <stdexcept>

namespace ores::iam::service {

using namespace ores::logging;

tenant_status_service::tenant_status_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::tenant_status> tenant_status_service::list_statuses() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all tenant statuses";
    return repo_.read_latest(ctx_);
}

std::optional<domain::tenant_status>
tenant_status_service::find_status(const std::string& status) {
    BOOST_LOG_SEV(lg(), debug) << "Finding tenant status: " << status;
    auto results = repo_.read_latest(ctx_, status);
    if (results.empty()) {
        return std::nullopt;
    }
    return results.front();
}

void tenant_status_service::save_status(const domain::tenant_status& status) {
    if (status.status.empty()) {
        throw std::invalid_argument("Tenant status code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving tenant status: " << status.status;
    repo_.write(ctx_, status);
    BOOST_LOG_SEV(lg(), info) << "Saved tenant status: " << status.status;
}

void tenant_status_service::remove_status(const std::string& status) {
    BOOST_LOG_SEV(lg(), debug) << "Removing tenant status: " << status;
    repo_.remove(ctx_, status);
    BOOST_LOG_SEV(lg(), info) << "Removed tenant status: " << status;
}

void tenant_status_service::remove_statuses(const std::vector<std::string>& statuses) {
    repo_.remove(ctx_, statuses);
}

std::vector<domain::tenant_status>
tenant_status_service::get_status_history(const std::string& status) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for tenant status: "
                               << status;
    return repo_.read_all(ctx_, status);
}

}
