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
#ifndef ORES_REFDATA_CORE_SERVICE_LEG_TYPE_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_LEG_TYPE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/leg_type.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/leg_type_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing leg types.
 *
 * Provides a higher-level interface for leg type operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT leg_type_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.leg_type_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a leg_type_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit leg_type_service(context ctx);

    /**
     * @brief Lists leg types with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of leg types for the requested page.
     */
    std::vector<domain::leg_type> list_types(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active leg types.
     *
     * @return Total number of active leg types.
     */
    std::uint32_t count_types();

    /**
     * @brief Retrieves a single leg type as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param code The code of the leg type.
     * @param version The version to fetch.
     * @return The leg type at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::leg_type> get_type_at_version(const std::string& code,
                                                        std::uint32_t version);

    /**
     * @brief Retrieves a single leg type by its code.
     *
     * @param code The code of the leg type.
     * @return The leg type if found, std::nullopt otherwise.
     */
    std::optional<domain::leg_type> get_type(const std::string& code);

    /**
     * @brief Saves a leg type (creates or updates).
     *
     * @param type The leg type to save.
     * @throws std::exception on failure.
     */
    void save_type(const domain::leg_type& type);

    /**
     * @brief Saves a batch of leg types.
     *
     * @param types The leg types to save.
     * @throws std::exception on failure.
     */
    void save_types(const std::vector<domain::leg_type>& types);

    /**
     * @brief Deletes a leg type by its code.
     *
     * @param code The code of the leg type to delete.
     * @throws std::exception on failure.
     */
    void delete_type(const std::string& code);

    /**
     * @brief Deletes leg types by their codes.
     */
    void delete_types(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a leg type.
     */
    std::vector<domain::leg_type> get_type_history(const std::string& code);

private:
    context ctx_;
    repository::leg_type_repository repo_;
};

}

#endif
