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
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

book_service::book_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::book> book_service::list_books(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all books";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t book_service::count_books() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total books count";
    return repo_.get_total_book_count(ctx_);
}

std::vector<domain::book> book_service::list_books_by_parent_portfolio_id(
    const std::string& parent_portfolio_id, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing books by parent_portfolio_id: " << parent_portfolio_id;
    return repo_.read_latest_by_parent_portfolio_id(ctx_, parent_portfolio_id, offset, limit);
}

std::uint32_t
book_service::count_books_by_parent_portfolio_id(const std::string& parent_portfolio_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting total books count by parent_portfolio_id: "
                               << parent_portfolio_id;
    return repo_.get_total_book_count_by_parent_portfolio_id(ctx_, parent_portfolio_id);
}

std::vector<domain::book> book_service::list_books_by_parent_portfolio_id_as_of(
    const std::string& parent_portfolio_id,
    std::chrono::system_clock::time_point valid_from_bound,
    std::chrono::system_clock::time_point valid_to_bound) {
    BOOST_LOG_SEV(lg(), debug) << "Listing books by parent_portfolio_id as of window: "
                               << parent_portfolio_id;
    return repo_.read_by_parent_portfolio_id_as_of(
        ctx_, parent_portfolio_id, valid_from_bound, valid_to_bound);
}

std::optional<domain::book> book_service::get_book_at_version(const std::string& id,
                                                              std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting book at version: " << id << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
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
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved book: " << v.id;
}

void book_service::save_books(const std::vector<domain::book>& books) {
    for (const auto& e : books)
        if (e.id.is_nil())
            throw std::invalid_argument("Book id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << books.size() << " books";
    auto ts = books;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void book_service::delete_book(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing book: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed book: " << id;
}

void book_service::delete_books(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::book> book_service::get_book_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for book: " << id;
    return repo_.read_all(ctx_, id);
}

}
