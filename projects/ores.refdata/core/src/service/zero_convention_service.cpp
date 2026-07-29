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
#include "ores.refdata.core/service/zero_convention_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

zero_convention_service::zero_convention_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::zero_convention>
zero_convention_service::list_zero_conventions(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all zero conventions";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t zero_convention_service::count_zero_conventions() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total zero conventions count";
    return repo_.get_total_zero_convention_count(ctx_);
}


std::optional<domain::zero_convention>
zero_convention_service::get_zero_convention_at_version(const std::string& id,
                                                        std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting zero convention at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::zero_convention>
zero_convention_service::get_zero_convention(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting zero convention. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void zero_convention_service::save_zero_convention(const domain::zero_convention& v) {
    if (v.id.empty())
        throw std::invalid_argument("Zero Convention id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving zero convention. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved zero convention. " << "id: " << v.id;
}

void zero_convention_service::save_zero_conventions(
    const std::vector<domain::zero_convention>& zero_conventions) {
    for (const auto& e : zero_conventions) {
        if (e.id.empty())
            throw std::invalid_argument("Zero Convention id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << zero_conventions.size() << " zero conventions";
    auto ts = zero_conventions;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void zero_convention_service::delete_zero_convention(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing zero convention. " << "id: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed zero convention. " << "id: " << id;
}

void zero_convention_service::delete_zero_conventions(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::zero_convention>
zero_convention_service::get_zero_convention_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for zero convention. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
