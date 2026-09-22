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
#ifndef ORES_IAM_CORE_SERVICE_TENANT_TYPE_SERVICE_HPP
#define ORES_IAM_CORE_SERVICE_TENANT_TYPE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/tenant_type.hpp"
#include "ores.iam.api/messaging/tenant_type_protocol.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.iam.core/repository/tenant_type_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::service {

/**
 * @brief Service for managing tenant types.
 *
 * Provides a higher-level interface for tenant type operations,
 * wrapping the underlying repository.
 */
class ORES_IAM_CORE_EXPORT tenant_type_service {
private:
    inline static std::string_view logger_name = "ores.iam.service.tenant_type_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a tenant_type_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit tenant_type_service(context ctx);

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
    messaging::list_tenant_types_response
    list_tenant_types(const messaging::list_tenant_types_request& request);
    messaging::get_tenant_type_response
    get_tenant_type(const messaging::get_tenant_type_request& request);
    messaging::get_many_tenant_types_response
    get_many_tenant_types(const messaging::get_many_tenant_types_request& request);
    messaging::put_tenant_type_response
    put_tenant_type(const messaging::put_tenant_type_request& request);
    messaging::put_many_tenant_types_response
    put_many_tenant_types(const messaging::put_many_tenant_types_request& request);
    messaging::delete_tenant_type_response
    delete_tenant_type(const messaging::delete_tenant_type_request& request);
    messaging::delete_many_tenant_types_response
    delete_many_tenant_types(const messaging::delete_many_tenant_types_request& request);
    messaging::list_tenant_type_versions_response
    list_tenant_type_versions(const messaging::list_tenant_type_versions_request& request);
    messaging::get_tenant_type_version_response
    get_tenant_type_version(const messaging::get_tenant_type_version_request& request);
    /**@}*/

    /**
     * @brief Lists tenant types with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of tenant types for the requested page.
     */
    std::vector<domain::tenant_type> list_types(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active tenant types.
     *
     * @return Total number of active tenant types.
     */
    std::uint32_t count_types();


    /**
     * @brief Retrieves a single tenant type as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The tenant type at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::tenant_type> get_type_at_version(const std::string& type,
                                                           std::uint32_t version);

    /**
     * @brief Retrieves a single tenant type by its primary key.
     *
     * @return The tenant type if found, std::nullopt otherwise.
     */
    std::optional<domain::tenant_type> find_type(const std::string& type);

    /**
     * @brief Retrieves a batch of tenant types by primary key.
     */
    std::vector<domain::tenant_type> get_types(const std::vector<std::string>& types);

    /**
     * @brief Saves a tenant type (creates or updates).
     *
     * @param type The tenant type to save.
     * @throws std::exception on failure.
     */
    void save_type(const domain::tenant_type& type);

    /**
     * @brief Saves a batch of tenant types.
     *
     * @param types The tenant types to save.
     * @throws std::exception on failure.
     */
    void save_types(const std::vector<domain::tenant_type>& types);

    /**
     * @brief Deletes a tenant type by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_type(const std::string& type);

    /**
     * @brief Deletes tenant types by their primary keys.
     */
    void delete_types(const std::vector<std::string>& types);

    /**
     * @brief Retrieves all historical versions of a tenant type.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::tenant_type> get_type_history(const std::string& type);

private:
    context ctx_;
    repository::tenant_type_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::tenant_type_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::tenant_type& out);
};

}

#endif
