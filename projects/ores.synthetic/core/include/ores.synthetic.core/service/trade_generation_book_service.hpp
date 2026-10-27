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
#ifndef ORES_SYNTHETIC_CORE_SERVICE_TRADE_GENERATION_BOOK_SERVICE_HPP
#define ORES_SYNTHETIC_CORE_SERVICE_TRADE_GENERATION_BOOK_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.synthetic.api/domain/trade_generation_book.hpp"
#include "ores.synthetic.api/messaging/trade_generation_book_protocol.hpp"
#include "ores.synthetic.core/export.hpp"
#include "ores.synthetic.core/repository/trade_generation_book_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Service for managing trade generation books.
 *
 * Provides a higher-level interface for trade generation book operations,
 * wrapping the underlying repository.
 */
class ORES_SYNTHETIC_CORE_EXPORT trade_generation_book_service {
private:
    inline static std::string_view logger_name =
        "ores.synthetic.service.trade_generation_book_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a trade_generation_book_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit trade_generation_book_service(context ctx);

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
    ::ores::synthetic::messaging::list_trade_generation_books_response list_trade_generation_books(
        const ::ores::synthetic::messaging::list_trade_generation_books_request& request);
    ::ores::synthetic::messaging::get_trade_generation_book_response get_trade_generation_book(
        const ::ores::synthetic::messaging::get_trade_generation_book_request& request);
    ::ores::synthetic::messaging::get_many_trade_generation_books_response
    get_many_trade_generation_books(
        const ::ores::synthetic::messaging::get_many_trade_generation_books_request& request);
    ::ores::synthetic::messaging::put_trade_generation_book_response put_trade_generation_book(
        const ::ores::synthetic::messaging::put_trade_generation_book_request& request);
    ::ores::synthetic::messaging::put_many_trade_generation_books_response
    put_many_trade_generation_books(
        const ::ores::synthetic::messaging::put_many_trade_generation_books_request& request);
    ::ores::synthetic::messaging::delete_trade_generation_book_response
    delete_trade_generation_book(
        const ::ores::synthetic::messaging::delete_trade_generation_book_request& request);
    ::ores::synthetic::messaging::delete_many_trade_generation_books_response
    delete_many_trade_generation_books(
        const ::ores::synthetic::messaging::delete_many_trade_generation_books_request& request);
    ::ores::synthetic::messaging::list_trade_generation_book_versions_response
    list_trade_generation_book_versions(
        const ::ores::synthetic::messaging::list_trade_generation_book_versions_request& request);
    ::ores::synthetic::messaging::get_trade_generation_book_version_response
    get_trade_generation_book_version(
        const ::ores::synthetic::messaging::get_trade_generation_book_version_request& request);
    /**@}*/

    /**
     * @brief Lists trade generation books with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of trade generation books for the requested page.
     */
    std::vector<domain::trade_generation_book> list_trade_generation_books(std::uint32_t offset,
                                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active trade generation books.
     *
     * @return Total number of active trade generation books.
     */
    std::uint32_t count_trade_generation_books();


    /**
     * @brief Retrieves a single trade generation book as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The trade generation book at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::trade_generation_book>
    get_trade_generation_book_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single trade generation book by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The trade generation book if found, std::nullopt otherwise.
     */
    std::optional<domain::trade_generation_book>
    get_trade_generation_book(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of trade generation books by primary key.
     */
    std::vector<domain::trade_generation_book>
    get_trade_generation_books(const std::vector<std::string>& ids);

    /**
     * @brief Saves a trade generation book (creates or updates).
     *
     * @param trade_generation_book The trade generation book to save.
     * @throws std::exception on failure.
     */
    void save_trade_generation_book(const domain::trade_generation_book& trade_generation_book);

    /**
     * @brief Saves a batch of trade generation books.
     *
     * @param trade_generation_books The trade generation books to save.
     * @throws std::exception on failure.
     */
    void save_trade_generation_books(
        const std::vector<domain::trade_generation_book>& trade_generation_books);

    /**
     * @brief Deletes a trade generation book by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_trade_generation_book(const boost::uuids::uuid& id);

    /**
     * @brief Deletes trade generation books by their primary keys.
     */
    void delete_trade_generation_books(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a trade generation book.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::trade_generation_book>
    get_trade_generation_book_history(const std::string& id);

private:
    context ctx_;
    repository::trade_generation_book_repository repo_;

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
    ores::utility::domain::result
    prepare_change(const ::ores::synthetic::messaging::trade_generation_book_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::trade_generation_book& out);
};

}

#endif
