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
#include "ores.refdata.core/service/overnight_index_convention_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

overnight_index_convention_service::overnight_index_convention_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::overnight_index_convention>
overnight_index_convention_service::list_overnight_index_conventions(std::uint32_t offset,
                                                                     std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all overnight index conventions";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t overnight_index_convention_service::count_overnight_index_conventions() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total overnight index conventions count";
    return repo_.get_total_overnight_index_convention_count(ctx_);
}


std::optional<domain::overnight_index_convention>
overnight_index_convention_service::get_overnight_index_convention_at_version(
    const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting overnight index convention at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::overnight_index_convention>
overnight_index_convention_service::get_overnight_index_convention(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting overnight index convention. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void overnight_index_convention_service::save_overnight_index_convention(
    const domain::overnight_index_convention& v) {
    if (v.id.empty())
        throw std::invalid_argument("Overnight Index Convention id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving overnight index convention. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved overnight index convention. " << "id: " << v.id;
}

void overnight_index_convention_service::save_overnight_index_conventions(
    const std::vector<domain::overnight_index_convention>& overnight_index_conventions) {
    for (const auto& e : overnight_index_conventions) {
        if (e.id.empty())
            throw std::invalid_argument("Overnight Index Convention id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << overnight_index_conventions.size()
                               << " overnight index conventions";
    auto ts = overnight_index_conventions;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void overnight_index_convention_service::delete_overnight_index_convention(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing overnight index convention. " << "id: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed overnight index convention. " << "id: " << id;
}

void overnight_index_convention_service::delete_overnight_index_conventions(
    const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::overnight_index_convention>
overnight_index_convention_service::get_overnight_index_convention_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for overnight index convention. "
                               << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
