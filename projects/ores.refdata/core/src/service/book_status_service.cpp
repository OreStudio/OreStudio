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
#include "ores.refdata.core/service/book_status_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

book_status_service::book_status_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::book_status> book_status_service::list_statuses(std::uint32_t offset,
                                                                    std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all book statuses";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t book_status_service::count_statuses() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total book statuses count";
    return repo_.get_total_status_count(ctx_);
}

std::optional<domain::book_status>
book_status_service::get_status_at_version(const std::string& code, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting book status at version: " << code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, code, version);
}

std::optional<domain::book_status> book_status_service::get_status(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting book status: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void book_status_service::save_status(const domain::book_status& v) {
    if (v.code.empty())
        throw std::invalid_argument("Book Status code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving book status: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved book status: " << v.code;
}

void book_status_service::save_statuses(const std::vector<domain::book_status>& statuses) {
    for (const auto& e : statuses)
        if (e.code.empty())
            throw std::invalid_argument("Book Status code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << statuses.size() << " book statuses";
    auto ts = statuses;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void book_status_service::delete_status(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing book status: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed book status: " << code;
}

void book_status_service::delete_statuses(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::book_status> book_status_service::get_status_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for book status: " << code;
    return repo_.read_all(ctx_, code);
}

}
