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
#ifndef ORES_IAM_CORE_SERVICE_AGENT_KEY_SERVICE_HPP
#define ORES_IAM_CORE_SERVICE_AGENT_KEY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/agent_key.hpp"
#include "ores.iam.api/messaging/agent_key_protocol.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.iam.core/repository/agent_key_repository.hpp"
#include "ores.logging/logger.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::service {

/**
 * @brief Service for managing agent keys.
 *
 * Provides a higher-level interface for agent key operations,
 * wrapping the underlying repository.
 */
class ORES_IAM_CORE_EXPORT agent_key_service {
private:
    inline static std::string_view logger_name = "ores.iam.service.agent_key_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a agent_key_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit agent_key_service(const context& ctx);

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
    ::ores::iam::messaging::list_agent_keys_response
    list_agent_keys(const ::ores::iam::messaging::list_agent_keys_request& request);
    ::ores::iam::messaging::get_agent_key_response
    get_agent_key(const ::ores::iam::messaging::get_agent_key_request& request);
    ::ores::iam::messaging::get_many_agent_keys_response
    get_many_agent_keys(const ::ores::iam::messaging::get_many_agent_keys_request& request);
    ::ores::iam::messaging::list_by_grant_id_agent_keys_response list_by_grant_id_agent_keys(
        const ::ores::iam::messaging::list_by_grant_id_agent_keys_request& request);
    ::ores::iam::messaging::list_agent_key_versions_response
    list_agent_key_versions(const ::ores::iam::messaging::list_agent_key_versions_request& request);
    ::ores::iam::messaging::get_agent_key_version_response
    get_agent_key_version(const ::ores::iam::messaging::get_agent_key_version_request& request);
    /**@}*/

    /**
     * @brief Lists agent keys with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of agent keys for the requested page.
     */
    std::vector<domain::agent_key> list_keys(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active agent keys.
     *
     * @return Total number of active agent keys.
     */
    std::uint32_t count_keys();


    /**
     * @brief Lists agent keys filtered by grant_id, with pagination.
     *
     * @param grant_id The grant_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching agent keys for the requested page.
     */
    std::vector<domain::agent_key>
    list_keys_by_grant_id(const std::string& grant_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active agent keys filtered by grant_id.
     *
     * @param grant_id The grant_id to filter by.
     * @return Total number of matching agent keys.
     */
    std::uint32_t count_keys_by_grant_id(const std::string& grant_id);


    /**
     * @brief Retrieves a single agent key as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The agent key at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::agent_key> get_key_at_version(const boost::uuids::uuid& id,
                                                        std::uint32_t version);

    /**
     * @brief Retrieves a single agent key by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The agent key if found, std::nullopt otherwise.
     */
    std::optional<domain::agent_key> get_key(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single agent key by its uuid primary key.
     *
     * @return The agent key if found, std::nullopt otherwise.
     */
    std::optional<domain::agent_key> find_key(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of agent keys by primary key.
     */
    std::vector<domain::agent_key> get_keys(const std::vector<std::string>& ids);

    /**
     * @brief Saves a agent key (creates or updates).
     *
     * @param key The agent key to save.
     * @throws std::exception on failure.
     */
    void save_key(const domain::agent_key& key);

    /**
     * @brief Saves a batch of agent keys.
     *
     * @param keys The agent keys to save.
     * @throws std::exception on failure.
     */
    void save_keys(const std::vector<domain::agent_key>& keys);

    /**
     * @brief Deletes a agent key by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_key(const boost::uuids::uuid& id);

    /**
     * @brief Removes a agent key by its uuid primary key.
     *
     * @throws std::exception on failure.
     */
    void remove_key(const boost::uuids::uuid& id);

    /**
     * @brief Deletes agent keys by their primary keys.
     */
    void delete_keys(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a agent key.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::agent_key> get_key_history(const std::string& id);

    /**
     * @brief Retrieves all historical versions of a agent key
     * by its uuid primary key.
     */
    std::vector<domain::agent_key> get_key_history(const boost::uuids::uuid& id);

private:
    context ctx_;
    repository::agent_key_repository repo_;
};

}

#endif
