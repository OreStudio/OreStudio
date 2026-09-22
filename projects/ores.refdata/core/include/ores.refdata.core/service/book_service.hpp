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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_service.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REFDATA_CORE_SERVICE_BOOK_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_BOOK_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/book.hpp"
#include "ores.refdata.api/messaging/book_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/book_repository.hpp"
#include <chrono>
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
     * @brief The protocol operations, one method per subject.
     *
     * A method takes the canonical request and answers its response, so the
     * handler that serves the subject decodes, calls and replies without
     * deciding anything. The result a caller reads -- missing, conflicting,
     * denied -- is filled here, where the storage call that decided it is
     * made, rather than being inferred from an exception.
     */
    /**@{*/
    messaging::list_books_response list_books(const messaging::list_books_request& request);
    messaging::get_book_response get_book(const messaging::get_book_request& request);
    messaging::get_many_books_response
    get_many_books(const messaging::get_many_books_request& request);
    messaging::put_book_response put_book(const messaging::put_book_request& request);
    messaging::put_many_books_response
    put_many_books(const messaging::put_many_books_request& request);
    messaging::delete_book_response delete_book(const messaging::delete_book_request& request);
    messaging::delete_many_books_response
    delete_many_books(const messaging::delete_many_books_request& request);
    messaging::list_by_parent_portfolio_id_books_response list_by_parent_portfolio_id_books(
        const messaging::list_by_parent_portfolio_id_books_request& request);
    messaging::list_book_versions_response
    list_book_versions(const messaging::list_book_versions_request& request);
    messaging::get_book_version_response
    get_book_version(const messaging::get_book_version_request& request);
    /**@}*/

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
     * @brief Lists books filtered by parent_portfolio_id, with pagination.
     *
     * @param parent_portfolio_id The parent_portfolio_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching books for the requested page.
     */
    std::vector<domain::book> list_books_by_parent_portfolio_id(
        const std::string& parent_portfolio_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active books filtered by parent_portfolio_id.
     *
     * @param parent_portfolio_id The parent_portfolio_id to filter by.
     * @return Total number of matching books.
     */
    std::uint32_t count_books_by_parent_portfolio_id(const std::string& parent_portfolio_id);


    /**
     * @brief Lists books filtered by parent_portfolio_id that were live at
     * any point during a parent version's own [valid_from, valid_to) window.
     * See the "Temporal composite entity versioning" architecture doc.
     *
     * @param parent_portfolio_id The parent_portfolio_id to filter by.
     * @param valid_from_bound The parent version's own valid_from.
     * @param valid_to_bound The parent version's own valid_to.
     * @return Vector of matching books.
     */
    std::vector<domain::book>
    list_books_by_parent_portfolio_id_as_of(const std::string& parent_portfolio_id,
                                            std::chrono::system_clock::time_point valid_from_bound,
                                            std::chrono::system_clock::time_point valid_to_bound);


    /**
     * @brief Retrieves a single book as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The book at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::book> get_book_at_version(const std::string& id, std::uint32_t version);

    /**
     * @brief Retrieves a single book by its primary key.
     *
     * @return The book if found, std::nullopt otherwise.
     */
    std::optional<domain::book> get_book(const std::string& id);

    /**
     * @brief Retrieves a batch of books by primary key.
     */
    std::vector<domain::book> get_books(const std::vector<std::string>& ids);

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
     * @brief Deletes a book by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_book(const std::string& id);

    /**
     * @brief Deletes books by their primary keys.
     */
    void delete_books(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a book.
     */
    std::vector<domain::book> get_book_history(const std::string& id);

private:
    context ctx_;
    repository::book_repository repo_;

    /**
     * @brief Checks one change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check, the
     * server-derived provenance and the version the store must match are one
     * decision made in one place. A batch that made the decision per element
     * would eventually make it differently from the single write.
     *
     * @param change The change as the caller stated it.
     * @param intent The reason and commentary the caller gave.
     * @param out The stamped domain object, written only when the result is ok.
     * @return ok, or why the change was refused.
     */
    ores::utility::domain::result prepare_change(const messaging::book_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::book& out);
};

}

#endif
