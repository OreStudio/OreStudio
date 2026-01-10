/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_IAM_SERVICE_CHANGE_MANAGEMENT_SERVICE_HPP
#define ORES_IAM_SERVICE_CHANGE_MANAGEMENT_SERVICE_HPP

#include <string>
#include <vector>
#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.iam/domain/change_reason_category.hpp"
#include "ores.iam/domain/change_reason.hpp"
#include "ores.iam/repository/change_reason_category_repository.hpp"
#include "ores.iam/repository/change_reason_repository.hpp"

namespace ores::iam::service {

/**
 * @brief Service for managing change reason categories and change reasons.
 *
 * This service provides functionality for:
 * - Managing change reason categories (CRUD operations)
 * - Managing change reasons and their associated categories
 * - Validating change reason codes against the catalog
 */
class change_management_service {
private:
    inline static std::string_view logger_name =
        "ores.iam.service.change_management_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a change_management_service with required repositories.
     *
     * @param ctx The database context.
     */
    explicit change_management_service(context ctx);

    // ========================================================================
    // Category Management
    // ========================================================================

    /**
     * @brief Lists all change reason categories.
     */
    std::vector<domain::change_reason_category> list_categories();

    /**
     * @brief Lists change reason categories with pagination.
     */
    std::vector<domain::change_reason_category>
    list_categories(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active categories.
     */
    std::uint32_t get_category_count();

    /**
     * @brief Finds a category by its code.
     */
    std::optional<domain::change_reason_category>
    find_category(const std::string& code);

    /**
     * @brief Creates a new change reason category.
     *
     * @param category The category to create
     * @return The created category
     * @throws std::invalid_argument if code is empty
     * @throws std::runtime_error if category already exists
     */
    domain::change_reason_category create_category(
        const domain::change_reason_category& category);

    /**
     * @brief Updates an existing change reason category.
     *
     * @param category The category with updated values
     */
    void update_category(const domain::change_reason_category& category);

    /**
     * @brief Removes a change reason category.
     *
     * @param code The code of the category to remove
     */
    void remove_category(const std::string& code);

    /**
     * @brief Gets the version history for a category.
     *
     * @param code The category code
     * @return Vector of all versions, newest first
     */
    std::vector<domain::change_reason_category>
    get_category_history(const std::string& code);

    // ========================================================================
    // Reason Management
    // ========================================================================

    /**
     * @brief Lists all change reasons.
     */
    std::vector<domain::change_reason> list_reasons();

    /**
     * @brief Lists change reasons with pagination.
     */
    std::vector<domain::change_reason>
    list_reasons(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Lists change reasons for a specific category.
     */
    std::vector<domain::change_reason>
    list_reasons_by_category(const std::string& category_code);

    /**
     * @brief Gets the total count of active reasons.
     */
    std::uint32_t get_reason_count();

    /**
     * @brief Finds a reason by its code.
     */
    std::optional<domain::change_reason> find_reason(const std::string& code);

    /**
     * @brief Creates a new change reason.
     *
     * @param reason The change reason to create
     * @return The created reason
     * @throws std::runtime_error if the category_code doesn't exist
     */
    domain::change_reason create_reason(const domain::change_reason& reason);

    /**
     * @brief Updates an existing change reason.
     *
     * @param reason The reason with updated values
     */
    void update_reason(const domain::change_reason& reason);

    /**
     * @brief Removes a change reason.
     *
     * @param code The code of the reason to remove
     */
    void remove_reason(const std::string& code);

    /**
     * @brief Gets the version history for a reason.
     *
     * @param code The reason code
     * @return Vector of all versions, newest first
     */
    std::vector<domain::change_reason> get_reason_history(const std::string& code);

    // ========================================================================
    // Validation
    // ========================================================================

    /**
     * @brief Validates that a change reason code exists.
     *
     * @param code The change reason code to validate
     * @return true if the code is valid, false otherwise
     */
    bool is_valid_reason_code(const std::string& code);

    /**
     * @brief Validates that a category code exists.
     *
     * @param code The category code to validate
     * @return true if the code is valid, false otherwise
     */
    bool is_valid_category_code(const std::string& code);

private:
    repository::change_reason_category_repository category_repo_;
    repository::change_reason_repository reason_repo_;
};

}

#endif
