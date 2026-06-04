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
#include "ores.refdata.core/service/book_service.hpp"
#include <stdexcept>

namespace ores::refdata::service {

using namespace ores::logging;

book_service::book_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::book> book_service::list_books() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all books";
    return repo_.read_latest(ctx_);
}

std::optional<domain::book> book_service::get_book(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting book: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void book_service::save_book(const domain::book& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Book id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving book: " << v.id;
    repo_.write(ctx_, v);
    BOOST_LOG_SEV(lg(), info) << "Saved book: " << v.id;
}

void book_service::remove_book(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing book: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed book: " << id;
}

std::vector<domain::book> book_service::get_book_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for book: " << id;
    return repo_.read_all(ctx_, id);
}

}
