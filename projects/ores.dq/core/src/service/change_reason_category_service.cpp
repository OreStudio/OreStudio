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
#include "ores.dq.core/service/change_reason_category_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::dq::service {

using namespace ores::logging;

change_reason_category_service::change_reason_category_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::change_reason_category>
change_reason_category_service::list_categories(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all change reason categories";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t change_reason_category_service::count_categories() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total change reason categories count";
    return repo_.get_total_category_count(ctx_);
}

std::optional<domain::change_reason_category>
change_reason_category_service::get_category_at_version(const std::string& code,
                                                        std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting change reason category at version: " << code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, code, version);
}

std::optional<domain::change_reason_category>
change_reason_category_service::get_category(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting change reason category: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void change_reason_category_service::save_category(const domain::change_reason_category& v) {
    if (v.code.empty())
        throw std::invalid_argument("Change Reason Category code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving change reason category: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved change reason category: " << v.code;
}

void change_reason_category_service::save_categories(
    const std::vector<domain::change_reason_category>& categories) {
    for (const auto& e : categories)
        if (e.code.empty())
            throw std::invalid_argument("Change Reason Category code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << categories.size() << " change reason categories";
    auto ts = categories;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void change_reason_category_service::delete_category(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing change reason category: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed change reason category: " << code;
}

void change_reason_category_service::delete_categories(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::change_reason_category>
change_reason_category_service::get_category_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for change reason category: " << code;
    return repo_.read_all(ctx_, code);
}

}
