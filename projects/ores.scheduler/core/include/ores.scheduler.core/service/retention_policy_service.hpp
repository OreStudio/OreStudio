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
#ifndef ORES_SCHEDULER_CORE_SERVICE_RETENTION_POLICY_SERVICE_HPP
#define ORES_SCHEDULER_CORE_SERVICE_RETENTION_POLICY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.scheduler.api/domain/retention_policy.hpp"
#include "ores.scheduler.api/messaging/retention_policy_protocol.hpp"
#include "ores.scheduler.core/export.hpp"
#include "ores.scheduler.core/repository/retention_policy_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::scheduler::service {

/**
 * @brief Service for managing retention policies.
 *
 * Provides a higher-level interface for retention policy operations,
 * wrapping the underlying repository.
 */
class ORES_SCHEDULER_CORE_EXPORT retention_policy_service {
private:
    inline static std::string_view logger_name = "ores.scheduler.service.retention_policy_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a retention_policy_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit retention_policy_service(const context& ctx);

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
    ::ores::scheduler::messaging::list_retention_policies_response list_retention_policies(
        const ::ores::scheduler::messaging::list_retention_policies_request& request);
    ::ores::scheduler::messaging::get_retention_policy_response
    get_retention_policy(const ::ores::scheduler::messaging::get_retention_policy_request& request);
    ::ores::scheduler::messaging::get_many_retention_policies_response get_many_retention_policies(
        const ::ores::scheduler::messaging::get_many_retention_policies_request& request);
    ::ores::scheduler::messaging::put_retention_policy_response
    put_retention_policy(const ::ores::scheduler::messaging::put_retention_policy_request& request);
    ::ores::scheduler::messaging::put_many_retention_policies_response put_many_retention_policies(
        const ::ores::scheduler::messaging::put_many_retention_policies_request& request);
    ::ores::scheduler::messaging::delete_retention_policy_response delete_retention_policy(
        const ::ores::scheduler::messaging::delete_retention_policy_request& request);
    ::ores::scheduler::messaging::delete_many_retention_policies_response
    delete_many_retention_policies(
        const ::ores::scheduler::messaging::delete_many_retention_policies_request& request);
    ::ores::scheduler::messaging::list_retention_policy_versions_response
    list_retention_policy_versions(
        const ::ores::scheduler::messaging::list_retention_policy_versions_request& request);
    ::ores::scheduler::messaging::get_retention_policy_version_response
    get_retention_policy_version(
        const ::ores::scheduler::messaging::get_retention_policy_version_request& request);
    /**@}*/

    /**
     * @brief Lists retention policies with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of retention policies for the requested page.
     */
    std::vector<domain::retention_policy> list_policies(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active retention policies.
     *
     * @return Total number of active retention policies.
     */
    std::uint32_t count_policies();


    /**
     * @brief Retrieves a single retention policy as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The retention policy at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::retention_policy> get_policy_at_version(const std::string& code,
                                                                  std::uint32_t version);

    /**
     * @brief Retrieves a single retention policy by its primary key.
     *
     * @return The retention policy if found, std::nullopt otherwise.
     */
    std::optional<domain::retention_policy> get_policy(const std::string& code);

    /**
     * @brief Retrieves a batch of retention policies by primary key.
     */
    std::vector<domain::retention_policy> get_policies(const std::vector<std::string>& codes);

    /**
     * @brief Saves a retention policy (creates or updates).
     *
     * @param policy The retention policy to save.
     * @throws std::exception on failure.
     */
    void save_policy(const domain::retention_policy& policy);

    /**
     * @brief Saves a batch of retention policies.
     *
     * @param policies The retention policies to save.
     * @throws std::exception on failure.
     */
    void save_policies(const std::vector<domain::retention_policy>& policies);

    /**
     * @brief Deletes a retention policy by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_policy(const std::string& code);

    /**
     * @brief Deletes retention policies by their primary keys.
     */
    void delete_policies(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a retention policy.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::retention_policy> get_policy_history(const std::string& code);

private:
    context ctx_;
    repository::retention_policy_repository repo_;

    /**
     * @brief Checks one change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check, the
     * server-derived provenance and the version the store must match are one
     * decision made in one place. A batch that made the decision per element
     * would eventually make it differently from the single write.
     *
     * The caller reads the current row, so a batch reads every row it names
     * in one query rather than one query per change.
     *
     * @param change The change as the caller stated it.
     * @param intent The reason and commentary the caller gave.
     * @param current The row the change names as it is now, or empty.
     * @param out The stamped domain object, written only when the result is ok.
     * @return ok, or why the change was refused.
     */
    ores::utility::domain::result
    prepare_change(const ::ores::scheduler::messaging::retention_policy_change& change,
                   const ores::utility::domain::change_intent& intent,
                   const std::vector<domain::retention_policy>& current,
                   domain::retention_policy& out);
};

}

#endif
