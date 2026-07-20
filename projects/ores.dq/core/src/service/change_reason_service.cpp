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
#include "ores.dq.core/service/change_reason_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::dq::service {

using namespace ores::logging;

change_reason_service::change_reason_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::change_reason> change_reason_service::list_reasons(std::uint32_t offset,
                                                                       std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all change reasons";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t change_reason_service::count_reasons() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total change reasons count";
    return repo_.get_total_reason_count(ctx_);
}

std::optional<domain::change_reason>
change_reason_service::get_reason_at_version(const std::string& code, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting change reason at version: " << code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, code, version);
}

std::optional<domain::change_reason> change_reason_service::get_reason(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting change reason: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void change_reason_service::save_reason(const domain::change_reason& v) {
    if (v.code.empty())
        throw std::invalid_argument("Change Reason code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving change reason: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved change reason: " << v.code;
}

void change_reason_service::save_reasons(const std::vector<domain::change_reason>& reasons) {
    for (const auto& e : reasons)
        if (e.code.empty())
            throw std::invalid_argument("Change Reason code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << reasons.size() << " change reasons";
    auto ts = reasons;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void change_reason_service::delete_reason(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing change reason: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed change reason: " << code;
}

void change_reason_service::delete_reasons(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::change_reason>
change_reason_service::get_reason_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for change reason: " << code;
    return repo_.read_all(ctx_, code);
}

}
