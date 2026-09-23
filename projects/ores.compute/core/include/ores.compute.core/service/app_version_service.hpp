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
#ifndef ORES_COMPUTE_CORE_SERVICE_APP_VERSION_SERVICE_HPP
#define ORES_COMPUTE_CORE_SERVICE_APP_VERSION_SERVICE_HPP

#include "ores.compute.api/domain/app_version.hpp"
#include "ores.compute.api/messaging/app_version_protocol.hpp"
#include "ores.compute.core/export.hpp"
#include "ores.compute.core/repository/app_version_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::compute::service {

/**
 * @brief Service for managing app versions.
 *
 * Provides a higher-level interface for app version operations,
 * wrapping the underlying repository.
 */
class ORES_COMPUTE_CORE_EXPORT app_version_service {
private:
    inline static std::string_view logger_name = "ores.compute.service.app_version_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a app_version_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit app_version_service(context ctx);

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
    messaging::list_app_versions_response
    list_app_versions(const messaging::list_app_versions_request& request);
    messaging::get_app_version_response
    get_app_version(const messaging::get_app_version_request& request);
    messaging::get_many_app_versions_response
    get_many_app_versions(const messaging::get_many_app_versions_request& request);
    messaging::put_app_version_response
    put_app_version(const messaging::put_app_version_request& request);
    messaging::put_many_app_versions_response
    put_many_app_versions(const messaging::put_many_app_versions_request& request);
    messaging::delete_app_version_response
    delete_app_version(const messaging::delete_app_version_request& request);
    messaging::delete_many_app_versions_response
    delete_many_app_versions(const messaging::delete_many_app_versions_request& request);
    messaging::list_app_version_versions_response
    list_app_version_versions(const messaging::list_app_version_versions_request& request);
    messaging::get_app_version_version_response
    get_app_version_version(const messaging::get_app_version_version_request& request);
    /**@}*/

    /**
     * @brief Lists app versions with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of app versions for the requested page.
     */
    std::vector<domain::app_version> list_app_versions(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active app versions.
     *
     * @return Total number of active app versions.
     */
    std::uint32_t count_app_versions();


    /**
     * @brief Retrieves a single app version as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The app version at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::app_version> get_app_version_at_version(const boost::uuids::uuid& id,
                                                                  std::uint32_t version);

    /**
     * @brief Retrieves a single app version by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The app version if found, std::nullopt otherwise.
     */
    std::optional<domain::app_version> get_app_version(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single app version by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The app version if found, std::nullopt otherwise.
     */
    std::optional<domain::app_version>
    get_app_version_by_wrapper_version(const std::string& wrapper_version);

    /**
     * @brief Retrieves a batch of app versions by primary key.
     */
    std::vector<domain::app_version> get_app_versions(const std::vector<std::string>& ids);

    /**
     * @brief Saves a app version (creates or updates).
     *
     * @param app_version The app version to save.
     * @throws std::exception on failure.
     */
    void save_app_version(const domain::app_version& app_version);

    /**
     * @brief Saves a batch of app versions.
     *
     * @param app_versions The app versions to save.
     * @throws std::exception on failure.
     */
    void save_app_versions(const std::vector<domain::app_version>& app_versions);

    /**
     * @brief Deletes a app version by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_app_version(const boost::uuids::uuid& id);

    /**
     * @brief Deletes app versions by their primary keys.
     */
    void delete_app_versions(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a app version.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::app_version> get_app_version_history(const std::string& key);

private:
    context ctx_;
    repository::app_version_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::app_version_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::app_version& out);
};

}

#endif
