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
#ifndef ORES_REFDATA_CORE_SERVICE_LEGAL_ENTITY_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_LEGAL_ENTITY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/legal_entity.hpp"
#include "ores.refdata.api/messaging/legal_entity_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/legal_entity_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing legal entities.
 *
 * Provides a higher-level interface for legal entity operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT legal_entity_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.legal_entity_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a legal_entity_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit legal_entity_service(context ctx);

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
    ::ores::refdata::messaging::list_legal_entities_response
    list_legal_entities(const ::ores::refdata::messaging::list_legal_entities_request& request);
    ::ores::refdata::messaging::get_legal_entity_response
    get_legal_entity(const ::ores::refdata::messaging::get_legal_entity_request& request);
    ::ores::refdata::messaging::get_many_legal_entities_response get_many_legal_entities(
        const ::ores::refdata::messaging::get_many_legal_entities_request& request);
    ::ores::refdata::messaging::put_legal_entity_response
    put_legal_entity(const ::ores::refdata::messaging::put_legal_entity_request& request);
    ::ores::refdata::messaging::put_many_legal_entities_response put_many_legal_entities(
        const ::ores::refdata::messaging::put_many_legal_entities_request& request);
    ::ores::refdata::messaging::delete_legal_entity_response
    delete_legal_entity(const ::ores::refdata::messaging::delete_legal_entity_request& request);
    ::ores::refdata::messaging::delete_many_legal_entities_response delete_many_legal_entities(
        const ::ores::refdata::messaging::delete_many_legal_entities_request& request);
    ::ores::refdata::messaging::list_legal_entity_versions_response list_legal_entity_versions(
        const ::ores::refdata::messaging::list_legal_entity_versions_request& request);
    ::ores::refdata::messaging::get_legal_entity_version_response get_legal_entity_version(
        const ::ores::refdata::messaging::get_legal_entity_version_request& request);
    /**@}*/

    /**
     * @brief Lists legal entities with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of legal entities for the requested page.
     */
    std::vector<domain::legal_entity> list_legal_entities(std::uint32_t offset,
                                                          std::uint32_t limit);

    /**
     * @brief Gets the total count of active legal entities.
     *
     * @return Total number of active legal entities.
     */
    std::uint32_t count_legal_entities();


    /**
     * @brief Retrieves a single legal entity as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The legal entity at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::legal_entity> get_legal_entity_at_version(const std::string& lei,
                                                                    std::uint32_t version);

    /**
     * @brief Retrieves a single legal entity by its primary key.
     *
     * @return The legal entity if found, std::nullopt otherwise.
     */
    std::optional<domain::legal_entity> get_legal_entity(const std::string& lei);

    /**
     * @brief Retrieves a batch of legal entities by primary key.
     */
    std::vector<domain::legal_entity> get_legal_entities(const std::vector<std::string>& leis);

    /**
     * @brief Saves a legal entity (creates or updates).
     *
     * @param legal_entity The legal entity to save.
     * @throws std::exception on failure.
     */
    void save_legal_entity(const domain::legal_entity& legal_entity);

    /**
     * @brief Saves a batch of legal entities.
     *
     * @param legal_entities The legal entities to save.
     * @throws std::exception on failure.
     */
    void save_legal_entities(const std::vector<domain::legal_entity>& legal_entities);

    /**
     * @brief Deletes a legal entity by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_legal_entity(const std::string& lei);

    /**
     * @brief Deletes legal entities by their primary keys.
     */
    void delete_legal_entities(const std::vector<std::string>& leis);

    /**
     * @brief Retrieves all historical versions of a legal entity.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::legal_entity> get_legal_entity_history(const std::string& lei);

private:
    context ctx_;
    repository::legal_entity_repository repo_;

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
    ores::utility::domain::result
    prepare_change(const ::ores::refdata::messaging::legal_entity_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::legal_entity& out);
};

}

#endif
