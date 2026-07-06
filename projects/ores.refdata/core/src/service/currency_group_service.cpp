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
#include "ores.refdata.core/service/currency_group_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

currency_group_service::currency_group_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::currency_group> currency_group_service::list_groups(std::uint32_t offset,
                                                                        std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all currency groups";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t currency_group_service::count_groups() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total currency groups count";
    return repo_.get_total_group_count(ctx_);
}


std::optional<domain::currency_group> currency_group_service::get_group(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting currency group: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void currency_group_service::save_group(const domain::currency_group& v) {
    if (v.code.empty())
        throw std::invalid_argument("Currency Group code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving currency group: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved currency group: " << v.code;
}

void currency_group_service::save_groups(const std::vector<domain::currency_group>& groups) {
    for (const auto& e : groups)
        if (e.code.empty())
            throw std::invalid_argument("Currency Group code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << groups.size() << " currency groups";
    auto ts = groups;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void currency_group_service::delete_group(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing currency group: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed currency group: " << code;
}

void currency_group_service::delete_groups(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::currency_group>
currency_group_service::get_group_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for currency group: " << code;
    return repo_.read_all(ctx_, code);
}

}
