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
#ifndef ORES_DQ_CORE_SERVICE_CHANGE_REASON_CATEGORY_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_CHANGE_REASON_CATEGORY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/change_reason_category.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/change_reason_category_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing change reason categories.
 *
 * Provides a higher-level interface for change reason category operations,
 * wrapping the underlying repository.
 */
class ORES_DQ_CORE_EXPORT change_reason_category_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.change_reason_category_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a change_reason_category_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit change_reason_category_service(context ctx);

    /**
     * @brief Lists change reason categories with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of change reason categories for the requested page.
     */
    std::vector<domain::change_reason_category> list_categories(std::uint32_t offset,
                                                                std::uint32_t limit);

    /**
     * @brief Gets the total count of active change reason categories.
     *
     * @return Total number of active change reason categories.
     */
    std::uint32_t count_categories();

    /**
     * @brief Retrieves a single change reason category as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param code The code of the change reason category.
     * @param version The version to fetch.
     * @return The change reason category at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::change_reason_category> get_category_at_version(const std::string& code,
                                                                          std::uint32_t version);

    /**
     * @brief Retrieves a single change reason category by its code.
     *
     * @param code The code of the change reason category.
     * @return The change reason category if found, std::nullopt otherwise.
     */
    std::optional<domain::change_reason_category> get_category(const std::string& code);

    /**
     * @brief Saves a change reason category (creates or updates).
     *
     * @param category The change reason category to save.
     * @throws std::exception on failure.
     */
    void save_category(const domain::change_reason_category& category);

    /**
     * @brief Saves a batch of change reason categories.
     *
     * @param categories The change reason categories to save.
     * @throws std::exception on failure.
     */
    void save_categories(const std::vector<domain::change_reason_category>& categories);

    /**
     * @brief Deletes a change reason category by its code.
     *
     * @param code The code of the change reason category to delete.
     * @throws std::exception on failure.
     */
    void delete_category(const std::string& code);

    /**
     * @brief Deletes change reason categories by their codes.
     */
    void delete_categories(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a change reason category.
     */
    std::vector<domain::change_reason_category> get_category_history(const std::string& code);

private:
    context ctx_;
    repository::change_reason_category_repository repo_;
};

}

#endif
