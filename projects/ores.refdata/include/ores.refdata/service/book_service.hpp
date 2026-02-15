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
#ifndef ORES_REFDATA_SERVICE_BOOK_SERVICE_HPP
#define ORES_REFDATA_SERVICE_BOOK_SERVICE_HPP

#include <string>
#include <vector>
#include <optional>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.refdata/domain/book.hpp"
#include "ores.refdata/repository/book_repository.hpp"

namespace ores::refdata::service {

/**
 * @brief Service for managing books.
 *
 * This service provides functionality for:
 * - Managing books (CRUD operations)
 */
class book_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.book_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a book_service with required repositories.
     *
     * @param ctx The database context.
     */
    explicit book_service(context ctx);

    /**
     * @brief Lists all books.
     */
    std::vector<domain::book> list_books();

    /**
     * @brief Finds a book by its ID.
     */
    std::optional<domain::book>
    find_book(const boost::uuids::uuid& id);

    /**
     * @brief Finds a book by its code.
     */
    std::optional<domain::book>
    find_book_by_code(const std::string& code);

    /**
     * @brief Saves a book (creates or updates).
     *
     * @param book The book to save
     */
    void save_book(const domain::book& book);

    /**
     * @brief Removes a book.
     *
     * @param id The ID of the book to remove
     */
    void remove_book(const boost::uuids::uuid& id);

    /**
     * @brief Gets the version history for a book.
     *
     * @param id The book ID
     * @return Vector of all versions, newest first
     */
    std::vector<domain::book>
    get_book_history(const boost::uuids::uuid& id);

private:
    repository::book_repository repo_;
};

}

#endif
