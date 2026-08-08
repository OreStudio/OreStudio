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
#ifndef ORES_DQ_CORE_SERVICE_LEI_RELATIONSHIP_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_LEI_RELATIONSHIP_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/lei_relationship.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/lei_relationship_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing LEI relationships.
 *
 * Provides a higher-level interface for LEI relationship operations,
 * wrapping the underlying repository.
 */
class ORES_DQ_CORE_EXPORT lei_relationship_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.lei_relationship_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a lei_relationship_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit lei_relationship_service(context ctx);

    /**
     * @brief Lists LEI relationships with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of LEI relationships for the requested page.
     */
    std::vector<domain::lei_relationship> list_relationships(std::uint32_t offset,
                                                             std::uint32_t limit);

    /**
     * @brief Gets the total count of active LEI relationships.
     *
     * @return Total number of active LEI relationships.
     */
    std::uint32_t count_relationships();


    /**
     * @brief Retrieves a single LEI relationship as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The LEI relationship at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::lei_relationship>
    get_relationship_at_version(const std::string& relationship_start_node_node_id,
                                std::uint32_t version);

    /**
     * @brief Retrieves a single LEI relationship by its primary key.
     *
     * @return The LEI relationship if found, std::nullopt otherwise.
     */
    std::optional<domain::lei_relationship>
    get_relationship(const std::string& relationship_start_node_node_id);

    /**
     * @brief Saves a LEI relationship (creates or updates).
     *
     * @param relationship The LEI relationship to save.
     * @throws std::exception on failure.
     */
    void save_relationship(const domain::lei_relationship& relationship);

    /**
     * @brief Saves a batch of LEI relationships.
     *
     * @param relationships The LEI relationships to save.
     * @throws std::exception on failure.
     */
    void save_relationships(const std::vector<domain::lei_relationship>& relationships);

    /**
     * @brief Deletes a LEI relationship by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_relationship(const std::string& relationship_start_node_node_id);

    /**
     * @brief Deletes LEI relationships by their primary keys.
     */
    void delete_relationships(const std::vector<std::string>& relationship_start_node_node_ids);

    /**
     * @brief Retrieves all historical versions of a LEI relationship.
     */
    std::vector<domain::lei_relationship>
    get_relationship_history(const std::string& relationship_start_node_node_id);

private:
    context ctx_;
    repository::lei_relationship_repository repo_;
};

}

#endif
