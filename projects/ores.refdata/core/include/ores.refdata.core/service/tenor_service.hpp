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
#ifndef ORES_REFDATA_CORE_SERVICE_TENOR_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_TENOR_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/tenor.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/tenor_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing tenors.
 *
 * Provides a higher-level interface for tenor operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT tenor_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.tenor_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a tenor_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit tenor_service(context ctx);

    /**
     * @brief Lists tenors with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of tenors for the requested page.
     */
    std::vector<domain::tenor> list_tenors(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active tenors.
     *
     * @return Total number of active tenors.
     */
    std::uint32_t count_tenors();

    /**
     * @brief Retrieves a single tenor as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param code The code of the tenor.
     * @param version The version to fetch.
     * @return The tenor at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::tenor> get_tenor_at_version(const std::string& code,
                                                      std::uint32_t version);

    /**
     * @brief Retrieves a single tenor by its code.
     *
     * @param code The code of the tenor.
     * @return The tenor if found, std::nullopt otherwise.
     */
    std::optional<domain::tenor> get_tenor(const std::string& code);

    /**
     * @brief Saves a tenor (creates or updates).
     *
     * @param tenor The tenor to save.
     * @throws std::exception on failure.
     */
    void save_tenor(const domain::tenor& tenor);

    /**
     * @brief Saves a batch of tenors.
     *
     * @param tenors The tenors to save.
     * @throws std::exception on failure.
     */
    void save_tenors(const std::vector<domain::tenor>& tenors);

    /**
     * @brief Deletes a tenor by its code.
     *
     * @param code The code of the tenor to delete.
     * @throws std::exception on failure.
     */
    void delete_tenor(const std::string& code);

    /**
     * @brief Deletes tenors by their codes.
     */
    void delete_tenors(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a tenor.
     */
    std::vector<domain::tenor> get_tenor_history(const std::string& code);

private:
    context ctx_;
    repository::tenor_repository repo_;
};

}

#endif
