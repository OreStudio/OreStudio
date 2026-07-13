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
#ifndef ORES_REFDATA_CORE_SERVICE_TENOR_ANCHOR_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_TENOR_ANCHOR_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/tenor_anchor.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/tenor_anchor_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing tenor anchors.
 *
 * Provides a higher-level interface for tenor anchor operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT tenor_anchor_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.tenor_anchor_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a tenor_anchor_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit tenor_anchor_service(context ctx);

    /**
     * @brief Lists tenor anchors with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of tenor anchors for the requested page.
     */
    std::vector<domain::tenor_anchor> list_anchors(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active tenor anchors.
     *
     * @return Total number of active tenor anchors.
     */
    std::uint32_t count_anchors();

    /**
     * @brief Retrieves a single tenor anchor as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param code The code of the tenor anchor.
     * @param version The version to fetch.
     * @return The tenor anchor at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::tenor_anchor> get_anchor_at_version(const std::string& code,
                                                              std::uint32_t version);

    /**
     * @brief Retrieves a single tenor anchor by its code.
     *
     * @param code The code of the tenor anchor.
     * @return The tenor anchor if found, std::nullopt otherwise.
     */
    std::optional<domain::tenor_anchor> get_anchor(const std::string& code);

    /**
     * @brief Saves a tenor anchor (creates or updates).
     *
     * @param anchor The tenor anchor to save.
     * @throws std::exception on failure.
     */
    void save_anchor(const domain::tenor_anchor& anchor);

    /**
     * @brief Saves a batch of tenor anchors.
     *
     * @param anchors The tenor anchors to save.
     * @throws std::exception on failure.
     */
    void save_anchors(const std::vector<domain::tenor_anchor>& anchors);

    /**
     * @brief Deletes a tenor anchor by its code.
     *
     * @param code The code of the tenor anchor to delete.
     * @throws std::exception on failure.
     */
    void delete_anchor(const std::string& code);

    /**
     * @brief Deletes tenor anchors by their codes.
     */
    void delete_anchors(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a tenor anchor.
     */
    std::vector<domain::tenor_anchor> get_anchor_history(const std::string& code);

private:
    context ctx_;
    repository::tenor_anchor_repository repo_;
};

}

#endif
