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
#ifndef ORES_DQ_CORE_SERVICE_ARTEFACT_TYPE_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_ARTEFACT_TYPE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/artefact_type.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/artefact_type_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing artefact types.
 *
 * Provides a higher-level interface for artefact type operations,
 * wrapping the underlying repository.
 */
class ORES_DQ_CORE_EXPORT artefact_type_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.artefact_type_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a artefact_type_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit artefact_type_service(context ctx);

    /**
     * @brief Lists artefact types with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of artefact types for the requested page.
     */
    std::vector<domain::artefact_type> list_types(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active artefact types.
     *
     * @return Total number of active artefact types.
     */
    std::uint32_t count_types();


    /**
     * @brief Retrieves a single artefact type as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The artefact type at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::artefact_type> get_type_at_version(const std::string& code,
                                                             std::uint32_t version);

    /**
     * @brief Retrieves a single artefact type by its primary key.
     *
     * @return The artefact type if found, std::nullopt otherwise.
     */
    std::optional<domain::artefact_type> get_type(const std::string& code);

    /**
     * @brief Saves a artefact type (creates or updates).
     *
     * @param type The artefact type to save.
     * @throws std::exception on failure.
     */
    void save_type(const domain::artefact_type& type);

    /**
     * @brief Saves a batch of artefact types.
     *
     * @param types The artefact types to save.
     * @throws std::exception on failure.
     */
    void save_types(const std::vector<domain::artefact_type>& types);

    /**
     * @brief Deletes a artefact type by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_type(const std::string& code);

    /**
     * @brief Deletes artefact types by their primary keys.
     */
    void delete_types(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a artefact type.
     */
    std::vector<domain::artefact_type> get_type_history(const std::string& code);

private:
    context ctx_;
    repository::artefact_type_repository repo_;
};

}

#endif
