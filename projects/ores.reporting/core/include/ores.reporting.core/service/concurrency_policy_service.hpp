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
#ifndef ORES_REPORTING_CORE_SERVICE_CONCURRENCY_POLICY_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_CONCURRENCY_POLICY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/concurrency_policy.hpp"
#include "ores.reporting.api/messaging/concurrency_policy_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/repository/concurrency_policy_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Service for managing concurrency policies.
 *
 * Provides a higher-level interface for concurrency policy operations,
 * wrapping the underlying repository.
 */
class ORES_REPORTING_CORE_EXPORT concurrency_policy_service {
private:
    inline static std::string_view logger_name =
        "ores.reporting.service.concurrency_policy_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a concurrency_policy_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit concurrency_policy_service(context ctx);

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
    messaging::list_concurrency_policies_response
    list_concurrency_policies(const messaging::list_concurrency_policies_request& request);
    messaging::get_concurrency_policy_response
    get_concurrency_policy(const messaging::get_concurrency_policy_request& request);
    messaging::get_many_concurrency_policies_response
    get_many_concurrency_policies(const messaging::get_many_concurrency_policies_request& request);
    messaging::put_concurrency_policy_response
    put_concurrency_policy(const messaging::put_concurrency_policy_request& request);
    messaging::put_many_concurrency_policies_response
    put_many_concurrency_policies(const messaging::put_many_concurrency_policies_request& request);
    messaging::delete_concurrency_policy_response
    delete_concurrency_policy(const messaging::delete_concurrency_policy_request& request);
    messaging::delete_many_concurrency_policies_response delete_many_concurrency_policies(
        const messaging::delete_many_concurrency_policies_request& request);
    messaging::list_concurrency_policy_versions_response list_concurrency_policy_versions(
        const messaging::list_concurrency_policy_versions_request& request);
    messaging::get_concurrency_policy_version_response get_concurrency_policy_version(
        const messaging::get_concurrency_policy_version_request& request);
    /**@}*/

    /**
     * @brief Lists concurrency policies with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of concurrency policies for the requested page.
     */
    std::vector<domain::concurrency_policy> list_policies(std::uint32_t offset,
                                                          std::uint32_t limit);

    /**
     * @brief Gets the total count of active concurrency policies.
     *
     * @return Total number of active concurrency policies.
     */
    std::uint32_t count_policies();


    /**
     * @brief Retrieves a single concurrency policy as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The concurrency policy at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::concurrency_policy> get_policy_at_version(const std::string& code,
                                                                    std::uint32_t version);

    /**
     * @brief Retrieves a single concurrency policy by its primary key.
     *
     * @return The concurrency policy if found, std::nullopt otherwise.
     */
    std::optional<domain::concurrency_policy> get_policy(const std::string& code);

    /**
     * @brief Retrieves a batch of concurrency policies by primary key.
     */
    std::vector<domain::concurrency_policy> get_policies(const std::vector<std::string>& codes);

    /**
     * @brief Saves a concurrency policy (creates or updates).
     *
     * @param policy The concurrency policy to save.
     * @throws std::exception on failure.
     */
    void save_policy(const domain::concurrency_policy& policy);

    /**
     * @brief Saves a batch of concurrency policies.
     *
     * @param policies The concurrency policies to save.
     * @throws std::exception on failure.
     */
    void save_policies(const std::vector<domain::concurrency_policy>& policies);

    /**
     * @brief Deletes a concurrency policy by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_policy(const std::string& code);

    /**
     * @brief Deletes concurrency policies by their primary keys.
     */
    void delete_policies(const std::vector<std::string>& codes);

    /**
     * @brief Retrieves all historical versions of a concurrency policy.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::concurrency_policy> get_policy_history(const std::string& code);

private:
    context ctx_;
    repository::concurrency_policy_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::concurrency_policy_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::concurrency_policy& out);
};

}

#endif
