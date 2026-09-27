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
#ifndef ORES_REPORTING_CORE_SERVICE_CONFIGURATION_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_CONFIGURATION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/configuration.hpp"
#include "ores.reporting.api/messaging/configuration_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/repository/configuration_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Service for managing configurations.
 *
 * Provides a higher-level interface for configuration operations,
 * wrapping the underlying repository.
 */
class ORES_REPORTING_CORE_EXPORT configuration_service {
private:
    inline static std::string_view logger_name = "ores.reporting.service.configuration_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a configuration_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit configuration_service(context ctx);

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
    messaging::list_configurations_response
    list_configurations(const messaging::list_configurations_request& request);
    messaging::get_configuration_response
    get_configuration(const messaging::get_configuration_request& request);
    messaging::get_many_configurations_response
    get_many_configurations(const messaging::get_many_configurations_request& request);
    messaging::put_configuration_response
    put_configuration(const messaging::put_configuration_request& request);
    messaging::put_many_configurations_response
    put_many_configurations(const messaging::put_many_configurations_request& request);
    messaging::delete_configuration_response
    delete_configuration(const messaging::delete_configuration_request& request);
    messaging::delete_many_configurations_response
    delete_many_configurations(const messaging::delete_many_configurations_request& request);
    messaging::list_by_configuration_type_code_configurations_response
    list_by_configuration_type_code_configurations(
        const messaging::list_by_configuration_type_code_configurations_request& request);
    messaging::list_configuration_versions_response
    list_configuration_versions(const messaging::list_configuration_versions_request& request);
    messaging::get_configuration_version_response
    get_configuration_version(const messaging::get_configuration_version_request& request);
    /**@}*/

    /**
     * @brief Lists configurations with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of configurations for the requested page.
     */
    std::vector<domain::configuration> list_configurations(std::uint32_t offset,
                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active configurations.
     *
     * @return Total number of active configurations.
     */
    std::uint32_t count_configurations();


    /**
     * @brief Lists configurations filtered by configuration_type_code, with pagination.
     *
     * @param configuration_type_code The configuration_type_code to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching configurations for the requested page.
     */
    std::vector<domain::configuration> list_configurations_by_configuration_type_code(
        const std::string& configuration_type_code, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active configurations filtered by configuration_type_code.
     *
     * @param configuration_type_code The configuration_type_code to filter by.
     * @return Total number of matching configurations.
     */
    std::uint32_t
    count_configurations_by_configuration_type_code(const std::string& configuration_type_code);


    /**
     * @brief Retrieves a single configuration as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The configuration at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::configuration> get_configuration_at_version(const boost::uuids::uuid& id,
                                                                      std::uint32_t version);

    /**
     * @brief Retrieves a single configuration by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The configuration if found, std::nullopt otherwise.
     */
    std::optional<domain::configuration> get_configuration(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single configuration by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The configuration if found, std::nullopt otherwise.
     */
    std::optional<domain::configuration> get_configuration_by_name(const std::string& name);

    /**
     * @brief Retrieves a batch of configurations by primary key.
     */
    std::vector<domain::configuration> get_configurations(const std::vector<std::string>& ids);

    /**
     * @brief Saves a configuration (creates or updates).
     *
     * @param configuration The configuration to save.
     * @throws std::exception on failure.
     */
    void save_configuration(const domain::configuration& configuration);

    /**
     * @brief Saves a batch of configurations.
     *
     * @param configurations The configurations to save.
     * @throws std::exception on failure.
     */
    void save_configurations(const std::vector<domain::configuration>& configurations);

    /**
     * @brief Deletes a configuration by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_configuration(const boost::uuids::uuid& id);

    /**
     * @brief Deletes configurations by their primary keys.
     */
    void delete_configurations(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a configuration.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::configuration> get_configuration_history(const std::string& key);

private:
    context ctx_;
    repository::configuration_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::configuration_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::configuration& out);
};

}

#endif
