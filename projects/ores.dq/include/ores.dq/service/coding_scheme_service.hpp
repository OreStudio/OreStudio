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
#ifndef ORES_DQ_SERVICE_CODING_SCHEME_SERVICE_HPP
#define ORES_DQ_SERVICE_CODING_SCHEME_SERVICE_HPP

#include <string>
#include <vector>
#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.dq/domain/coding_scheme.hpp"
#include "ores.dq/domain/coding_scheme_authority_type.hpp"
#include "ores.dq/repository/coding_scheme_repository.hpp"
#include "ores.dq/repository/coding_scheme_authority_type_repository.hpp"

namespace ores::dq::service {

/**
 * @brief Service for managing coding schemes.
 *
 * This service provides functionality for:
 * - Managing coding schemes (CRUD operations)
 * - Filtering coding schemes by authority type
 */
class coding_scheme_service {
private:
    inline static std::string_view logger_name =
        "ores.dq.service.coding_scheme_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a coding_scheme_service with required repositories.
     *
     * @param ctx The database context.
     */
    explicit coding_scheme_service(context ctx);

    // ========================================================================
    // Coding Scheme Management
    // ========================================================================

    /**
     * @brief Lists all coding schemes.
     */
    std::vector<domain::coding_scheme> list_coding_schemes();

    /**
     * @brief Lists coding schemes with pagination.
     */
    std::vector<domain::coding_scheme>
    list_coding_schemes(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Lists coding schemes for a specific authority type.
     */
    std::vector<domain::coding_scheme>
    list_coding_schemes_by_authority_type(const std::string& authority_type);

    /**
     * @brief Gets the total count of active coding schemes.
     */
    std::uint32_t get_coding_scheme_count();

    /**
     * @brief Finds a coding scheme by its code.
     */
    std::optional<domain::coding_scheme>
    find_coding_scheme(const std::string& code);

    /**
     * @brief Saves a coding scheme (creates or updates).
     *
     * @param scheme The coding scheme to save
     */
    void save_coding_scheme(const domain::coding_scheme& scheme);

    /**
     * @brief Removes a coding scheme.
     *
     * @param code The code of the coding scheme to remove
     */
    void remove_coding_scheme(const std::string& code);

    /**
     * @brief Gets the version history for a coding scheme.
     *
     * @param code The coding scheme code
     * @return Vector of all versions, newest first
     */
    std::vector<domain::coding_scheme>
    get_coding_scheme_history(const std::string& code);

    // ========================================================================
    // Coding Scheme Authority Type Management
    // ========================================================================

    /**
     * @brief Lists all coding scheme authority types.
     */
    std::vector<domain::coding_scheme_authority_type> list_authority_types();

    /**
     * @brief Finds a coding scheme authority type by its code.
     */
    std::optional<domain::coding_scheme_authority_type>
    find_authority_type(const std::string& code);

    /**
     * @brief Saves a coding scheme authority type (creates or updates).
     *
     * @param authority_type The authority type to save
     */
    void save_authority_type(
        const domain::coding_scheme_authority_type& authority_type);

    /**
     * @brief Removes a coding scheme authority type.
     *
     * @param code The code of the authority type to remove
     */
    void remove_authority_type(const std::string& code);

    /**
     * @brief Gets the version history for a coding scheme authority type.
     *
     * @param code The authority type code
     * @return Vector of all versions, newest first
     */
    std::vector<domain::coding_scheme_authority_type>
    get_authority_type_history(const std::string& code);

private:
    repository::coding_scheme_repository coding_scheme_repo_;
    repository::coding_scheme_authority_type_repository authority_type_repo_;
};

}

#endif
