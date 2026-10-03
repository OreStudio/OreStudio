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
#ifndef ORES_REFDATA_CORE_SERVICE_DEFAULT_CURVE_CONFIGURATION_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_DEFAULT_CURVE_CONFIGURATION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/default_curve_configuration.hpp"
#include "ores.refdata.api/messaging/default_curve_configuration_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/default_curve_configuration_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing default curve configurations.
 *
 * Provides a higher-level interface for default curve configuration operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT default_curve_configuration_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.default_curve_configuration_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a default_curve_configuration_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit default_curve_configuration_service(context ctx);

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
    messaging::list_default_curve_configurations_response list_default_curve_configurations(
        const messaging::list_default_curve_configurations_request& request);
    messaging::get_default_curve_configuration_response get_default_curve_configuration(
        const messaging::get_default_curve_configuration_request& request);
    messaging::get_many_default_curve_configurations_response get_many_default_curve_configurations(
        const messaging::get_many_default_curve_configurations_request& request);
    messaging::put_default_curve_configuration_response put_default_curve_configuration(
        const messaging::put_default_curve_configuration_request& request);
    messaging::put_many_default_curve_configurations_response put_many_default_curve_configurations(
        const messaging::put_many_default_curve_configurations_request& request);
    messaging::delete_default_curve_configuration_response delete_default_curve_configuration(
        const messaging::delete_default_curve_configuration_request& request);
    messaging::delete_many_default_curve_configurations_response
    delete_many_default_curve_configurations(
        const messaging::delete_many_default_curve_configurations_request& request);
    messaging::list_default_curve_configuration_versions_response
    list_default_curve_configuration_versions(
        const messaging::list_default_curve_configuration_versions_request& request);
    messaging::get_default_curve_configuration_version_response
    get_default_curve_configuration_version(
        const messaging::get_default_curve_configuration_version_request& request);
    /**@}*/

    /**
     * @brief Lists default curve configurations with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of default curve configurations for the requested page.
     */
    std::vector<domain::default_curve_configuration> list_configurations(std::uint32_t offset,
                                                                         std::uint32_t limit);

    /**
     * @brief Gets the total count of active default curve configurations.
     *
     * @return Total number of active default curve configurations.
     */
    std::uint32_t count_configurations();


    /**
     * @brief Retrieves a single default curve configuration as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The default curve configuration at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::default_curve_configuration>
    get_configuration_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single default curve configuration by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The default curve configuration if found, std::nullopt otherwise.
     */
    std::optional<domain::default_curve_configuration>
    get_configuration(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of default curve configurations by primary key.
     */
    std::vector<domain::default_curve_configuration>
    get_configurations(const std::vector<std::string>& ids);

    /**
     * @brief Saves a default curve configuration (creates or updates).
     *
     * @param configuration The default curve configuration to save.
     * @throws std::exception on failure.
     */
    void save_configuration(const domain::default_curve_configuration& configuration);

    /**
     * @brief Saves a batch of default curve configurations.
     *
     * @param configurations The default curve configurations to save.
     * @throws std::exception on failure.
     */
    void
    save_configurations(const std::vector<domain::default_curve_configuration>& configurations);

    /**
     * @brief Deletes a default curve configuration by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_configuration(const boost::uuids::uuid& id);

    /**
     * @brief Deletes default curve configurations by their primary keys.
     */
    void delete_configurations(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a default curve configuration.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::default_curve_configuration>
    get_configuration_history(const std::string& id);

private:
    context ctx_;
    repository::default_curve_configuration_repository repo_;

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
    prepare_change(const messaging::default_curve_configuration_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::default_curve_configuration& out);
};

}

#endif
