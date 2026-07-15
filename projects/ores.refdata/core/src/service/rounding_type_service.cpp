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
#include "ores.refdata.core/service/rounding_type_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

rounding_type_service::rounding_type_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::rounding_type> rounding_type_service::list_types(std::uint32_t offset,
                                                                     std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all rounding types";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t rounding_type_service::count_types() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total rounding types count";
    return repo_.get_total_type_count(ctx_);
}

std::optional<domain::rounding_type>
rounding_type_service::get_type_at_version(const std::string& code, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting rounding type at version: " << code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, code, version);
}

std::optional<domain::rounding_type> rounding_type_service::get_type(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting rounding type: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void rounding_type_service::save_type(const domain::rounding_type& v) {
    if (v.code.empty())
        throw std::invalid_argument("Rounding Type code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving rounding type: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved rounding type: " << v.code;
}

void rounding_type_service::save_types(const std::vector<domain::rounding_type>& types) {
    for (const auto& e : types)
        if (e.code.empty())
            throw std::invalid_argument("Rounding Type code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << types.size() << " rounding types";
    auto ts = types;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void rounding_type_service::delete_type(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing rounding type: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed rounding type: " << code;
}

void rounding_type_service::delete_types(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::rounding_type>
rounding_type_service::get_type_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for rounding type: " << code;
    return repo_.read_all(ctx_, code);
}

}
