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
#ifndef ORES_REFDATA_CORE_SERVICE_BOOK_STATUS_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_BOOK_STATUS_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/book_status.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/book_status_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing book statuses.
 *
 * Provides a higher-level interface for book status operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT book_status_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.book_status_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a book_status_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit book_status_service(context ctx);

    /**
     * @brief Lists book statuses with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of book statuses for the requested page.
     */
    std::vector<domain::book_status> list_statuses(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active book statuses.
     *
     * @return Total number of active book statuses.
     */
    std::uint32_t count_statuses();

    /**
     * @brief Retrieves a single book status as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param code The code of the book status.
     * @param version The version to fetch.
     * @return The book status at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::book_status> get_status_at_version(const std::string& code,
                                                             std::uint32_t version);

    /**
     * @brief Retrieves a single book status by its code.
     *
     * @param code The code of the book status.
     * @return The book status if found, std::nullopt otherwise.
     */
    std::optional<domain::book_status> get_status(const std::string& code);

    /**
     * @brief Saves a book status (creates or updates).
     *
     * @param status The book status to save.
     * @throws std::exception on failure.
     */
    void save_status(const domain::book_status& status);

    /**
     * @brief Saves a batch of book statuses.
     *
     * @param statuses The book statuses to save.
     * @throws std::exception on failure.
     */
    void save_statuses(const std::vector<domain::book_status>& statuses);

    /**
     * @brief Deletes a book status by its code.
     *
     * @param code The code of the book status to delete.
     * @throws std::exception on failure.
     */
    void delete_status(const std::string& code);

    /**
     * @brief Deletes book statuses by their codes.
     */
    void delete_statuses(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a book status.
     */
    std::vector<domain::book_status> get_status_history(const std::string& code);

private:
    context ctx_;
    repository::book_status_repository repo_;
};

}

#endif
