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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_service.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_DQ_CORE_SERVICE_LEI_ENTITY_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_LEI_ENTITY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/lei_entity.hpp"
#include "ores.dq.api/messaging/lei_entity_protocol.hpp"
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
     * @brief The protocol operations, one method per subject.
     *
     * A method takes the canonical request and answers its response, so the
     * handler that serves the subject decodes, calls and replies without
     * deciding anything. The result a caller reads -- missing, conflicting,
     * denied -- is filled here, where the storage call that decided it is
     * made, rather than being inferred from an exception.
     */
    /**@{*/
    messaging::list_lei_entities_response
    list_lei_entities(const messaging::list_lei_entities_request& request);
    messaging::get_lei_entity_response
    get_lei_entity(const messaging::get_lei_entity_request& request);
    messaging::get_many_lei_entities_response
    get_many_lei_entities(const messaging::get_many_lei_entities_request& request);
    messaging::put_lei_entity_response
    put_lei_entity(const messaging::put_lei_entity_request& request);
    messaging::put_many_lei_entities_response
    put_many_lei_entities(const messaging::put_many_lei_entities_request& request);
    messaging::delete_lei_entity_response
    delete_lei_entity(const messaging::delete_lei_entity_request& request);
    messaging::delete_many_lei_entities_response
    delete_many_lei_entities(const messaging::delete_many_lei_entities_request& request);
    messaging::list_lei_entity_versions_response
    list_lei_entity_versions(const messaging::list_lei_entity_versions_request& request);
    messaging::get_lei_entity_version_response
    get_lei_entity_version(const messaging::get_lei_entity_version_request& request);
    /**@}*/

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
     * @brief Retrieves a batch of LEI entities by primary key.
     */
    std::vector<domain::lei_entity> get_entities(const std::vector<std::string>& leis);

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
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::lei_entity> get_entity_history(const std::string& lei);

private:
    context ctx_;
    repository::lei_entity_repository repo_;

    /**
     * @brief Checks one change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check, the
     * server-derived provenance and the version the store must match are one
     * decision made in one place. A batch that made the decision per element
     * would eventually make it differently from the single write.
     *
     * @param change The change as the caller stated it.
     * @param intent The reason and commentary the caller gave.
     * @param out The stamped domain object, written only when the result is ok.
     * @return ok, or why the change was refused.
     */
    ores::utility::domain::result prepare_change(const messaging::lei_entity_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::lei_entity& out);
};

}

#endif
