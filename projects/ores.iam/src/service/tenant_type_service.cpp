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
#include "ores.iam/service/tenant_type_service.hpp"

#include <stdexcept>

namespace ores::iam::service {

using namespace ores::logging;

tenant_type_service::tenant_type_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::tenant_type> tenant_type_service::list_types() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all tenant types";
    return repo_.read_latest(ctx_);
}

std::optional<domain::tenant_type>
tenant_type_service::find_type(const std::string& type) {
    BOOST_LOG_SEV(lg(), debug) << "Finding tenant type: " << type;
    auto results = repo_.read_latest(ctx_, type);
    if (results.empty()) {
        return std::nullopt;
    }
    return results.front();
}

void tenant_type_service::save_type(const domain::tenant_type& type) {
    if (type.type.empty()) {
        throw std::invalid_argument("Tenant type code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving tenant type: " << type.type;
    repo_.write(ctx_, type);
    BOOST_LOG_SEV(lg(), info) << "Saved tenant type: " << type.type;
}

void tenant_type_service::remove_type(const std::string& type) {
    BOOST_LOG_SEV(lg(), debug) << "Removing tenant type: " << type;
    repo_.remove(ctx_, type);
    BOOST_LOG_SEV(lg(), info) << "Removed tenant type: " << type;
}

void tenant_type_service::remove_types(const std::vector<std::string>& types) {
    repo_.remove(ctx_, types);
}

std::vector<domain::tenant_type>
tenant_type_service::get_type_history(const std::string& type) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for tenant type: " << type;
    return repo_.read_all(ctx_, type);
}

}
