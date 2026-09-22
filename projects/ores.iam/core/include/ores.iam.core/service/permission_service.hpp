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
#ifndef ORES_IAM_CORE_SERVICE_PERMISSION_SERVICE_HPP
#define ORES_IAM_CORE_SERVICE_PERMISSION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/permission.hpp"
#include "ores.iam.api/messaging/permission_protocol.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.iam.core/repository/permission_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::service {

/**
 * @brief Service for managing permissions.
 *
 * Provides a higher-level interface for permission operations,
 * wrapping the underlying repository.
 */
class ORES_IAM_CORE_EXPORT permission_service {
private:
    inline static std::string_view logger_name = "ores.iam.service.permission_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a permission_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit permission_service(context ctx);

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
    messaging::list_permissions_response
    list_permissions(const messaging::list_permissions_request& request);
    messaging::get_permission_response
    get_permission(const messaging::get_permission_request& request);
    messaging::get_many_permissions_response
    get_many_permissions(const messaging::get_many_permissions_request& request);
    messaging::put_permission_response
    put_permission(const messaging::put_permission_request& request);
    messaging::put_many_permissions_response
    put_many_permissions(const messaging::put_many_permissions_request& request);
    messaging::delete_permission_response
    delete_permission(const messaging::delete_permission_request& request);
    messaging::delete_many_permissions_response
    delete_many_permissions(const messaging::delete_many_permissions_request& request);
    /**@}*/

    /**
     * @brief Lists permissions with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of permissions for the requested page.
     */
    std::vector<domain::permission> list_permissions(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active permissions.
     *
     * @return Total number of active permissions.
     */
    std::uint32_t count_permissions();


    /**
     * @brief Retrieves a single permission by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The permission if found, std::nullopt otherwise.
     */
    std::optional<domain::permission> get_permission(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single permission by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The permission if found, std::nullopt otherwise.
     */
    std::optional<domain::permission> get_permission_by_code(const std::string& code);

    /**
     * @brief Retrieves a single permission by its code.
     *
     * @return The permission if found, std::nullopt otherwise.
     */
    std::optional<domain::permission> find_permission_by_code(const std::string& code);

    /**
     * @brief Retrieves a batch of permissions by primary key.
     */
    std::vector<domain::permission> get_permissions(const std::vector<std::string>& ids);

    /**
     * @brief Saves a permission (creates or updates).
     *
     * @param permission The permission to save.
     * @throws std::exception on failure.
     */
    void save_permission(const domain::permission& permission);

    /**
     * @brief Saves a batch of permissions.
     *
     * @param permissions The permissions to save.
     * @throws std::exception on failure.
     */
    void save_permissions(const std::vector<domain::permission>& permissions);

    /**
     * @brief Deletes a permission by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_permission(const boost::uuids::uuid& id);

    /**
     * @brief Deletes permissions by their primary keys.
     */
    void delete_permissions(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a permission.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::permission> get_permission_history(const std::string& key);

private:
    context ctx_;
    repository::permission_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::permission_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::permission& out);
};

}

#endif
