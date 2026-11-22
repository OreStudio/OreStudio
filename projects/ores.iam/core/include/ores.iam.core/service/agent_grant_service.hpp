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
#ifndef ORES_IAM_CORE_SERVICE_AGENT_GRANT_SERVICE_HPP
#define ORES_IAM_CORE_SERVICE_AGENT_GRANT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/agent_grant.hpp"
#include "ores.iam.api/messaging/agent_grant_protocol.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.iam.core/repository/agent_grant_repository.hpp"
#include "ores.logging/logger.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::service {

/**
 * @brief Service for managing agent grants.
 *
 * Provides a higher-level interface for agent grant operations,
 * wrapping the underlying repository.
 */
class ORES_IAM_CORE_EXPORT agent_grant_service {
private:
    inline static std::string_view logger_name = "ores.iam.service.agent_grant_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a agent_grant_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit agent_grant_service(const context& ctx);

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
    ::ores::iam::messaging::list_agent_grants_response
    list_agent_grants(const ::ores::iam::messaging::list_agent_grants_request& request);
    ::ores::iam::messaging::get_agent_grant_response
    get_agent_grant(const ::ores::iam::messaging::get_agent_grant_request& request);
    ::ores::iam::messaging::get_many_agent_grants_response
    get_many_agent_grants(const ::ores::iam::messaging::get_many_agent_grants_request& request);
    ::ores::iam::messaging::list_by_grantor_account_id_agent_grants_response
    list_by_grantor_account_id_agent_grants(
        const ::ores::iam::messaging::list_by_grantor_account_id_agent_grants_request& request);
    ::ores::iam::messaging::list_agent_grant_versions_response list_agent_grant_versions(
        const ::ores::iam::messaging::list_agent_grant_versions_request& request);
    ::ores::iam::messaging::get_agent_grant_version_response
    get_agent_grant_version(const ::ores::iam::messaging::get_agent_grant_version_request& request);
    /**@}*/

    /**
     * @brief Lists agent grants with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of agent grants for the requested page.
     */
    std::vector<domain::agent_grant> list_grants(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active agent grants.
     *
     * @return Total number of active agent grants.
     */
    std::uint32_t count_grants();


    /**
     * @brief Lists agent grants filtered by grantor_account_id, with pagination.
     *
     * @param grantor_account_id The grantor_account_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching agent grants for the requested page.
     */
    std::vector<domain::agent_grant> list_grants_by_grantor_account_id(
        const std::string& grantor_account_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active agent grants filtered by grantor_account_id.
     *
     * @param grantor_account_id The grantor_account_id to filter by.
     * @return Total number of matching agent grants.
     */
    std::uint32_t count_grants_by_grantor_account_id(const std::string& grantor_account_id);


    /**
     * @brief Retrieves a single agent grant as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The agent grant at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::agent_grant> get_grant_at_version(const boost::uuids::uuid& id,
                                                            std::uint32_t version);

    /**
     * @brief Retrieves a single agent grant by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The agent grant if found, std::nullopt otherwise.
     */
    std::optional<domain::agent_grant> get_grant(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single agent grant by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The agent grant if found, std::nullopt otherwise.
     */
    std::optional<domain::agent_grant> get_grant_by_agent_name(const std::string& agent_name);

    /**
     * @brief Retrieves a single agent grant by its uuid primary key.
     *
     * @return The agent grant if found, std::nullopt otherwise.
     */
    std::optional<domain::agent_grant> find_grant(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of agent grants by primary key.
     */
    std::vector<domain::agent_grant> get_grants(const std::vector<std::string>& ids);

    /**
     * @brief Saves a agent grant (creates or updates).
     *
     * @param grant The agent grant to save.
     * @throws std::exception on failure.
     */
    void save_grant(const domain::agent_grant& grant);

    /**
     * @brief Saves a batch of agent grants.
     *
     * @param grants The agent grants to save.
     * @throws std::exception on failure.
     */
    void save_grants(const std::vector<domain::agent_grant>& grants);

    /**
     * @brief Deletes a agent grant by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_grant(const boost::uuids::uuid& id);

    /**
     * @brief Removes a agent grant by its uuid primary key.
     *
     * @throws std::exception on failure.
     */
    void remove_grant(const boost::uuids::uuid& id);

    /**
     * @brief Deletes agent grants by their primary keys.
     */
    void delete_grants(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a agent grant.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::agent_grant> get_grant_history(const std::string& key);

    /**
     * @brief Retrieves all historical versions of a agent grant
     * by its uuid primary key.
     */
    std::vector<domain::agent_grant> get_grant_history(const boost::uuids::uuid& id);

private:
    context ctx_;
    repository::agent_grant_repository repo_;
};

}

#endif
