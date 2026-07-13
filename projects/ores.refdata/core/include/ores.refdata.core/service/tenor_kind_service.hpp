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
#ifndef ORES_REFDATA_CORE_SERVICE_TENOR_KIND_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_TENOR_KIND_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/tenor_kind.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/tenor_kind_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing tenor kinds.
 *
 * Provides a higher-level interface for tenor kind operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT tenor_kind_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.tenor_kind_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a tenor_kind_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit tenor_kind_service(context ctx);

    /**
     * @brief Lists tenor kinds with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of tenor kinds for the requested page.
     */
    std::vector<domain::tenor_kind> list_kinds(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active tenor kinds.
     *
     * @return Total number of active tenor kinds.
     */
    std::uint32_t count_kinds();

    /**
     * @brief Retrieves a single tenor kind as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param code The code of the tenor kind.
     * @param version The version to fetch.
     * @return The tenor kind at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::tenor_kind> get_kind_at_version(const std::string& code,
                                                          std::uint32_t version);

    /**
     * @brief Retrieves a single tenor kind by its code.
     *
     * @param code The code of the tenor kind.
     * @return The tenor kind if found, std::nullopt otherwise.
     */
    std::optional<domain::tenor_kind> get_kind(const std::string& code);

    /**
     * @brief Saves a tenor kind (creates or updates).
     *
     * @param kind The tenor kind to save.
     * @throws std::exception on failure.
     */
    void save_kind(const domain::tenor_kind& kind);

    /**
     * @brief Saves a batch of tenor kinds.
     *
     * @param kinds The tenor kinds to save.
     * @throws std::exception on failure.
     */
    void save_kinds(const std::vector<domain::tenor_kind>& kinds);

    /**
     * @brief Deletes a tenor kind by its code.
     *
     * @param code The code of the tenor kind to delete.
     * @throws std::exception on failure.
     */
    void delete_kind(const std::string& code);

    /**
     * @brief Deletes tenor kinds by their codes.
     */
    void delete_kinds(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a tenor kind.
     */
    std::vector<domain::tenor_kind> get_kind_history(const std::string& code);

private:
    context ctx_;
    repository::tenor_kind_repository repo_;
};

}

#endif
