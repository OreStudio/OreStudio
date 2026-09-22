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
#ifndef ORES_IAM_CORE_SERVICE_TENANT_STATUS_SERVICE_HPP
#define ORES_IAM_CORE_SERVICE_TENANT_STATUS_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/tenant_status.hpp"
#include "ores.iam.api/messaging/tenant_status_protocol.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.iam.core/repository/tenant_status_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::service {

/**
 * @brief Service for managing tenant statuses.
 *
 * Provides a higher-level interface for tenant status operations,
 * wrapping the underlying repository.
 */
class ORES_IAM_CORE_EXPORT tenant_status_service {
private:
    inline static std::string_view logger_name = "ores.iam.service.tenant_status_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a tenant_status_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit tenant_status_service(context ctx);

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
    messaging::list_tenant_statuses_response
    list_tenant_statuses(const messaging::list_tenant_statuses_request& request);
    messaging::get_tenant_status_response
    get_tenant_status(const messaging::get_tenant_status_request& request);
    messaging::get_many_tenant_statuses_response
    get_many_tenant_statuses(const messaging::get_many_tenant_statuses_request& request);
    messaging::put_tenant_status_response
    put_tenant_status(const messaging::put_tenant_status_request& request);
    messaging::put_many_tenant_statuses_response
    put_many_tenant_statuses(const messaging::put_many_tenant_statuses_request& request);
    messaging::delete_tenant_status_response
    delete_tenant_status(const messaging::delete_tenant_status_request& request);
    messaging::delete_many_tenant_statuses_response
    delete_many_tenant_statuses(const messaging::delete_many_tenant_statuses_request& request);
    messaging::list_tenant_status_versions_response
    list_tenant_status_versions(const messaging::list_tenant_status_versions_request& request);
    messaging::get_tenant_status_version_response
    get_tenant_status_version(const messaging::get_tenant_status_version_request& request);
    /**@}*/

    /**
     * @brief Lists tenant statuses with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of tenant statuses for the requested page.
     */
    std::vector<domain::tenant_status> list_statuses(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active tenant statuses.
     *
     * @return Total number of active tenant statuses.
     */
    std::uint32_t count_statuses();


    /**
     * @brief Retrieves a single tenant status as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The tenant status at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::tenant_status> get_status_at_version(const std::string& status,
                                                               std::uint32_t version);

    /**
     * @brief Retrieves a single tenant status by its primary key.
     *
     * @return The tenant status if found, std::nullopt otherwise.
     */
    std::optional<domain::tenant_status> find_status(const std::string& status);

    /**
     * @brief Retrieves a batch of tenant statuses by primary key.
     */
    std::vector<domain::tenant_status> get_statuses(const std::vector<std::string>& statuss);

    /**
     * @brief Saves a tenant status (creates or updates).
     *
     * @param status The tenant status to save.
     * @throws std::exception on failure.
     */
    void save_status(const domain::tenant_status& status);

    /**
     * @brief Saves a batch of tenant statuses.
     *
     * @param statuses The tenant statuses to save.
     * @throws std::exception on failure.
     */
    void save_statuses(const std::vector<domain::tenant_status>& statuses);

    /**
     * @brief Deletes a tenant status by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_status(const std::string& status);

    /**
     * @brief Deletes tenant statuses by their primary keys.
     */
    void delete_statuses(const std::vector<std::string>& statuss);

    /**
     * @brief Retrieves all historical versions of a tenant status.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::tenant_status> get_status_history(const std::string& status);

private:
    context ctx_;
    repository::tenant_status_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::tenant_status_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::tenant_status& out);
};

}

#endif
