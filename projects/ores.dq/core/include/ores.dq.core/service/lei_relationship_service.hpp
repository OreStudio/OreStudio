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
#ifndef ORES_DQ_CORE_SERVICE_LEI_RELATIONSHIP_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_LEI_RELATIONSHIP_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/lei_relationship.hpp"
#include "ores.dq.api/messaging/lei_relationship_protocol.hpp"
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
     * @brief The protocol operations, one method per subject.
     *
     * A method takes the canonical request and answers its response, so the
     * handler that serves the subject decodes, calls and replies without
     * deciding anything. The result a caller reads -- missing, conflicting,
     * denied -- is filled here, where the storage call that decided it is
     * made, rather than being inferred from an exception.
     */
    /**@{*/
    messaging::list_lei_relationships_response
    list_lei_relationships(const messaging::list_lei_relationships_request& request);
    messaging::get_lei_relationship_response
    get_lei_relationship(const messaging::get_lei_relationship_request& request);
    messaging::get_many_lei_relationships_response
    get_many_lei_relationships(const messaging::get_many_lei_relationships_request& request);
    messaging::put_lei_relationship_response
    put_lei_relationship(const messaging::put_lei_relationship_request& request);
    messaging::put_many_lei_relationships_response
    put_many_lei_relationships(const messaging::put_many_lei_relationships_request& request);
    messaging::delete_lei_relationship_response
    delete_lei_relationship(const messaging::delete_lei_relationship_request& request);
    messaging::delete_many_lei_relationships_response
    delete_many_lei_relationships(const messaging::delete_many_lei_relationships_request& request);
    messaging::list_lei_relationship_versions_response list_lei_relationship_versions(
        const messaging::list_lei_relationship_versions_request& request);
    messaging::get_lei_relationship_version_response
    get_lei_relationship_version(const messaging::get_lei_relationship_version_request& request);
    /**@}*/

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
     * @brief Retrieves a batch of LEI relationships by primary key.
     */
    std::vector<domain::lei_relationship>
    get_relationships(const std::vector<std::string>& relationship_start_node_node_ids);

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
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::lei_relationship>
    get_relationship_history(const std::string& relationship_start_node_node_id);

private:
    context ctx_;
    repository::lei_relationship_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::lei_relationship_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::lei_relationship& out);
};

}

#endif
