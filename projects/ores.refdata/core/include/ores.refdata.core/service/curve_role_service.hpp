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
#ifndef ORES_REFDATA_CORE_SERVICE_CURVE_ROLE_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_CURVE_ROLE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/curve_role.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/curve_role_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing curve roles.
 *
 * Provides a higher-level interface for curve role operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT curve_role_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.curve_role_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a curve_role_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit curve_role_service(context ctx);

    /**
     * @brief Lists curve roles with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of curve roles for the requested page.
     */
    std::vector<domain::curve_role> list_roles(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active curve roles.
     *
     * @return Total number of active curve roles.
     */
    std::uint32_t count_roles();

    /**
     * @brief Retrieves a single curve role as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param code The code of the curve role.
     * @param version The version to fetch.
     * @return The curve role at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::curve_role> get_role_at_version(const std::string& code,
                                                          std::uint32_t version);

    /**
     * @brief Retrieves a single curve role by its code.
     *
     * @param code The code of the curve role.
     * @return The curve role if found, std::nullopt otherwise.
     */
    std::optional<domain::curve_role> get_role(const std::string& code);

    /**
     * @brief Saves a curve role (creates or updates).
     *
     * @param role The curve role to save.
     * @throws std::exception on failure.
     */
    void save_role(const domain::curve_role& role);

    /**
     * @brief Saves a batch of curve roles.
     *
     * @param roles The curve roles to save.
     * @throws std::exception on failure.
     */
    void save_roles(const std::vector<domain::curve_role>& roles);

    /**
     * @brief Deletes a curve role by its code.
     *
     * @param code The code of the curve role to delete.
     * @throws std::exception on failure.
     */
    void delete_role(const std::string& code);

    /**
     * @brief Deletes curve roles by their codes.
     */
    void delete_roles(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a curve role.
     */
    std::vector<domain::curve_role> get_role_history(const std::string& code);

private:
    context ctx_;
    repository::curve_role_repository repo_;
};

}

#endif
