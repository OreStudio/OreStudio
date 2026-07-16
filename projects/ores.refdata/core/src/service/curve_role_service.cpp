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
#include "ores.refdata.core/service/curve_role_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

curve_role_service::curve_role_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::curve_role> curve_role_service::list_roles(std::uint32_t offset,
                                                               std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all curve roles";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t curve_role_service::count_roles() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total curve roles count";
    return repo_.get_total_role_count(ctx_);
}

std::optional<domain::curve_role> curve_role_service::get_role_at_version(const std::string& code,
                                                                          std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting curve role at version: " << code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, code, version);
}

std::optional<domain::curve_role> curve_role_service::get_role(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting curve role: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void curve_role_service::save_role(const domain::curve_role& v) {
    if (v.code.empty())
        throw std::invalid_argument("Curve Role code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving curve role: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved curve role: " << v.code;
}

void curve_role_service::save_roles(const std::vector<domain::curve_role>& roles) {
    for (const auto& e : roles)
        if (e.code.empty())
            throw std::invalid_argument("Curve Role code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << roles.size() << " curve roles";
    auto ts = roles;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void curve_role_service::delete_role(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing curve role: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed curve role: " << code;
}

void curve_role_service::delete_roles(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::curve_role> curve_role_service::get_role_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for curve role: " << code;
    return repo_.read_all(ctx_, code);
}

}
