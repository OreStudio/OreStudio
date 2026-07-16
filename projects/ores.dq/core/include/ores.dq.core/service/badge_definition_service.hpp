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
#ifndef ORES_DQ_CORE_SERVICE_BADGE_DEFINITION_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_BADGE_DEFINITION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/badge_definition.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/badge_definition_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing badge definitions.
 *
 * Provides a higher-level interface for badge definition operations,
 * wrapping the underlying repository.
 */
class ORES_DQ_CORE_EXPORT badge_definition_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.badge_definition_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a badge_definition_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit badge_definition_service(context ctx);

    /**
     * @brief Lists badge definitions with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of badge definitions for the requested page.
     */
    std::vector<domain::badge_definition> list_definitions(std::uint32_t offset,
                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active badge definitions.
     *
     * @return Total number of active badge definitions.
     */
    std::uint32_t count_definitions();

    /**
     * @brief Retrieves a single badge definition as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param code The code of the badge definition.
     * @param version The version to fetch.
     * @return The badge definition at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::badge_definition> get_definition_at_version(const std::string& code,
                                                                      std::uint32_t version);

    /**
     * @brief Retrieves a single badge definition by its code.
     *
     * @param code The code of the badge definition.
     * @return The badge definition if found, std::nullopt otherwise.
     */
    std::optional<domain::badge_definition> get_definition(const std::string& code);

    /**
     * @brief Saves a badge definition (creates or updates).
     *
     * @param definition The badge definition to save.
     * @throws std::exception on failure.
     */
    void save_definition(const domain::badge_definition& definition);

    /**
     * @brief Saves a batch of badge definitions.
     *
     * @param definitions The badge definitions to save.
     * @throws std::exception on failure.
     */
    void save_definitions(const std::vector<domain::badge_definition>& definitions);

    /**
     * @brief Deletes a badge definition by its code.
     *
     * @param code The code of the badge definition to delete.
     * @throws std::exception on failure.
     */
    void delete_definition(const std::string& code);

    /**
     * @brief Deletes badge definitions by their codes.
     */
    void delete_definitions(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a badge definition.
     */
    std::vector<domain::badge_definition> get_definition_history(const std::string& code);

private:
    context ctx_;
    repository::badge_definition_repository repo_;
};

}

#endif
