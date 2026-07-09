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
#ifndef ORES_REFDATA_CORE_SERVICE_BOOK_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_BOOK_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/book.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/book_repository.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing books.
 *
 * Provides a higher-level interface for book operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT book_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.book_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a book_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit book_service(context ctx);

    /**
     * @brief Lists books with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of books for the requested page.
     */
    std::vector<domain::book> list_books(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active books.
     *
     * @return Total number of active books.
     */
    std::uint32_t count_books();


    /**
     * @brief Retrieves a single book by its id.
     *
     * @param id The id of the book.
     * @return The book if found, std::nullopt otherwise.
     */
    std::optional<domain::book> get_book(const std::string& id);

    /**
     * @brief Saves a book (creates or updates).
     *
     * @param book The book to save.
     * @throws std::exception on failure.
     */
    void save_book(const domain::book& book);

    /**
     * @brief Saves a batch of books.
     *
     * @param books The books to save.
     * @throws std::exception on failure.
     */
    void save_books(const std::vector<domain::book>& books);

    /**
     * @brief Deletes a book by its id.
     *
     * @param id The id of the book to delete.
     * @throws std::exception on failure.
     */
    void delete_book(const std::string& id);

    /**
     * @brief Deletes books by their ids.
     */
    void delete_books(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a book.
     */
    std::vector<domain::book> get_book_history(const std::string& id);

private:
    context ctx_;
    repository::book_repository repo_;
};

}

#endif
