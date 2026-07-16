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
#include "ores.dq.core/service/badge_severity_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::dq::service {

using namespace ores::logging;

badge_severity_service::badge_severity_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::badge_severity> badge_severity_service::list_severities(std::uint32_t offset,
                                                                            std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all badge severities";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t badge_severity_service::count_severities() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total badge severities count";
    return repo_.get_total_severity_count(ctx_);
}

std::optional<domain::badge_severity>
badge_severity_service::get_severity_at_version(const std::string& code, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting badge severity at version: " << code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, code, version);
}

std::optional<domain::badge_severity>
badge_severity_service::get_severity(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting badge severity: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void badge_severity_service::save_severity(const domain::badge_severity& v) {
    if (v.code.empty())
        throw std::invalid_argument("Badge Severity code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving badge severity: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved badge severity: " << v.code;
}

void badge_severity_service::save_severities(
    const std::vector<domain::badge_severity>& severities) {
    for (const auto& e : severities)
        if (e.code.empty())
            throw std::invalid_argument("Badge Severity code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << severities.size() << " badge severities";
    auto ts = severities;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void badge_severity_service::delete_severity(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing badge severity: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed badge severity: " << code;
}

void badge_severity_service::delete_severities(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::badge_severity>
badge_severity_service::get_severity_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for badge severity: " << code;
    return repo_.read_all(ctx_, code);
}

}
