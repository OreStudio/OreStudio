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
#include "ores.refdata.core/service/tenor_kind_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

tenor_kind_service::tenor_kind_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::tenor_kind> tenor_kind_service::list_kinds(std::uint32_t offset,
                                                               std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all tenor kinds";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t tenor_kind_service::count_kinds() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total tenor kinds count";
    return repo_.get_total_kind_count(ctx_);
}

std::optional<domain::tenor_kind> tenor_kind_service::get_kind_at_version(const std::string& code,
                                                                          std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting tenor kind at version: " << code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, code, version);
}

std::optional<domain::tenor_kind> tenor_kind_service::get_kind(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting tenor kind: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void tenor_kind_service::save_kind(const domain::tenor_kind& v) {
    if (v.code.empty())
        throw std::invalid_argument("Tenor Kind code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving tenor kind: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved tenor kind: " << v.code;
}

void tenor_kind_service::save_kinds(const std::vector<domain::tenor_kind>& kinds) {
    for (const auto& e : kinds)
        if (e.code.empty())
            throw std::invalid_argument("Tenor Kind code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << kinds.size() << " tenor kinds";
    auto ts = kinds;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void tenor_kind_service::delete_kind(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing tenor kind: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed tenor kind: " << code;
}

void tenor_kind_service::delete_kinds(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::tenor_kind> tenor_kind_service::get_kind_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for tenor kind: " << code;
    return repo_.read_all(ctx_, code);
}

}
