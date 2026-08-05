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
#ifndef ORES_DQ_CORE_SERVICE_LEI_ENTITY_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_LEI_ENTITY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/lei_entity.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/lei_entity_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing LEI entities.
 *
 * Provides a higher-level interface for LEI entity operations,
 * wrapping the underlying repository.
 */
class ORES_DQ_CORE_EXPORT lei_entity_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.lei_entity_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a lei_entity_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit lei_entity_service(context ctx);

    /**
     * @brief Lists LEI entities with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of LEI entities for the requested page.
     */
    std::vector<domain::lei_entity> list_entities(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active LEI entities.
     *
     * @return Total number of active LEI entities.
     */
    std::uint32_t count_entities();


    /**
     * @brief Retrieves a single LEI entity as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The LEI entity at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::lei_entity> get_entity_at_version(const std::string& lei,
                                                            std::uint32_t version);

    /**
     * @brief Retrieves a single LEI entity by its primary key.
     *
     * @return The LEI entity if found, std::nullopt otherwise.
     */
    std::optional<domain::lei_entity> get_entity(const std::string& lei);

    /**
     * @brief Saves a LEI entity (creates or updates).
     *
     * @param entity The LEI entity to save.
     * @throws std::exception on failure.
     */
    void save_entity(const domain::lei_entity& entity);

    /**
     * @brief Saves a batch of LEI entities.
     *
     * @param entities The LEI entities to save.
     * @throws std::exception on failure.
     */
    void save_entities(const std::vector<domain::lei_entity>& entities);

    /**
     * @brief Deletes a LEI entity by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_entity(const std::string& lei);

    /**
     * @brief Deletes LEI entities by their primary keys.
     */
    void delete_entities(const std::vector<std::string>& leis);

    /**
     * @brief Retrieves all historical versions of a LEI entity.
     */
    std::vector<domain::lei_entity> get_entity_history(const std::string& lei);

private:
    context ctx_;
    repository::lei_entity_repository repo_;
};

}

#endif
