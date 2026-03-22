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
#ifndef ORES_REFDATA_API_MESSAGING_BOOK_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_BOOK_PROTOCOL_HPP

#include <string>
#include <vector>
#include "ores.refdata.api/domain/book.hpp"

namespace ores::refdata::messaging {

struct get_books_request {
    using response_type = struct get_books_response;
    static constexpr std::string_view nats_subject = "refdata.v1.books.list";
    int offset = 0;
    int limit = 100;
};

struct get_books_response {
    std::vector<ores::refdata::domain::book> books;
    int total_available_count = 0;
};

struct save_book_request {
    using response_type = struct save_book_response;
    static constexpr std::string_view nats_subject = "refdata.v1.books.save";
    ores::refdata::domain::book data;
};

struct save_book_response {
    bool success = false;
    std::string message;
};

struct delete_book_request {
    using response_type = struct delete_book_response;
    static constexpr std::string_view nats_subject = "refdata.v1.books.delete";
    std::vector<std::string> ids;
};

struct delete_book_response {
    bool success = false;
    std::string message;
};

struct get_book_history_request {
    using response_type = struct get_book_history_response;
    static constexpr std::string_view nats_subject = "refdata.v1.books.history";
    std::string id;
};

struct get_book_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::refdata::domain::book> versions;
};

}

#endif
