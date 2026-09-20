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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_service.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.iam.core/service/permission_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::iam::service {

using namespace ores::logging;

permission_service::permission_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::permission> permission_service::list_permissions(std::uint32_t offset,
                                                                     std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all permissions";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t permission_service::count_permissions() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total permissions count";
    return repo_.get_total_permission_count(ctx_);
}


std::optional<domain::permission> permission_service::get_permission(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting permission. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::optional<domain::permission>
permission_service::find_permission_by_code(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Finding permission by code: " << code;
    auto results = repo_.read_latest_by_code(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void permission_service::save_permission(const domain::permission& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Permission id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving permission. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved permission. " << "id: " << v.id;
}

void permission_service::save_permissions(const std::vector<domain::permission>& permissions) {
    for (const auto& e : permissions) {
        if (e.id.is_nil())
            throw std::invalid_argument("Permission id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << permissions.size() << " permissions";
    auto ts = permissions;
    for (auto& e : ts) {
        stamp(e, ctx_);
    }
    repo_.write(ctx_, ts);
}

void permission_service::delete_permission(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing permission. " << "id: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed permission. " << "id: " << id;
}

void permission_service::delete_permissions(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::permission> permission_service::get_permission_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for permission. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
