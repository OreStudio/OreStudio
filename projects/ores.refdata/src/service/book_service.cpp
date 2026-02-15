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
#include "ores.refdata/service/book_service.hpp"

#include <stdexcept>
#include <boost/uuid/uuid_io.hpp>

namespace ores::refdata::service {

using namespace ores::logging;

book_service::book_service(context ctx)
    : repo_(ctx) {}

std::vector<domain::book> book_service::list_books() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all books";
    return repo_.read_latest();
}

std::optional<domain::book>
book_service::find_book(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding book: " << id;
    auto results = repo_.read_latest(id);
    if (results.empty()) {
        return std::nullopt;
    }
    return results.front();
}

std::optional<domain::book>
book_service::find_book_by_code(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Finding book by code: " << code;
    auto results = repo_.read_latest_by_code(code);
    if (results.empty()) {
        return std::nullopt;
    }
    return results.front();
}

void book_service::save_book(const domain::book& book) {
    if (book.id.is_nil()) {
        throw std::invalid_argument("Book ID cannot be nil.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving book: " << book.id;
    repo_.write(book);
    BOOST_LOG_SEV(lg(), info) << "Saved book: " << book.id;
}

void book_service::remove_book(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing book: " << id;
    repo_.remove(id);
    BOOST_LOG_SEV(lg(), info) << "Removed book: " << id;
}

std::vector<domain::book>
book_service::get_book_history(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for book: " << id;
    return repo_.read_all(id);
}

}
