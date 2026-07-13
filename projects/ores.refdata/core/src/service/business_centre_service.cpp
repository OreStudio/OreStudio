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
#include "ores.refdata.core/service/business_centre_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

business_centre_service::business_centre_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::business_centre> business_centre_service::list_centres(std::uint32_t offset,
                                                                           std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all business centres";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t business_centre_service::count_centres() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total business centres count";
    return repo_.get_total_centre_count(ctx_);
}

std::optional<domain::business_centre>
business_centre_service::get_centre_at_version(const std::string& code, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting business centre at version: " << code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, code, version);
}

std::optional<domain::business_centre>
business_centre_service::get_centre(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting business centre: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void business_centre_service::save_centre(const domain::business_centre& v) {
    if (v.code.empty())
        throw std::invalid_argument("Business Centre code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving business centre: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved business centre: " << v.code;
}

void business_centre_service::save_centres(const std::vector<domain::business_centre>& centres) {
    for (const auto& e : centres)
        if (e.code.empty())
            throw std::invalid_argument("Business Centre code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << centres.size() << " business centres";
    auto ts = centres;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void business_centre_service::delete_centre(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing business centre: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed business centre: " << code;
}

void business_centre_service::delete_centres(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::business_centre>
business_centre_service::get_centre_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for business centre: " << code;
    return repo_.read_all(ctx_, code);
}

}
