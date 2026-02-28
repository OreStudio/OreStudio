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
#include "ores.refdata/service/book_status_service.hpp"

#include <stdexcept>

namespace ores::refdata::service {

using namespace ores::logging;

book_status_service::book_status_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::book_status> book_status_service::list_statuses() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all book statuses";
    return repo_.read_latest(ctx_);
}

std::optional<domain::book_status>
book_status_service::find_status(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Finding book status: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty()) {
        return std::nullopt;
    }
    return results.front();
}

void book_status_service::save_status(const domain::book_status& status) {
    if (status.code.empty()) {
        throw std::invalid_argument("Party status code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving book status: " << status.code;
    repo_.write(ctx_, status);
    BOOST_LOG_SEV(lg(), info) << "Saved book status: " << status.code;
}

void book_status_service::save_statuses(
    const std::vector<domain::book_status>& statuses) {
    for (const auto& s : statuses) {
        if (s.code.empty())
            throw std::invalid_argument("Book status code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << statuses.size() << " book statuses";
    repo_.write(ctx_, statuses);
}

void book_status_service::remove_status(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing book status: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed book status: " << code;
}

std::vector<domain::book_status>
book_status_service::get_status_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for book status: " << code;
    return repo_.read_all(ctx_, code);
}

}
