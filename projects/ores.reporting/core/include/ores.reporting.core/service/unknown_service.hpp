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
#ifndef ORES_REPORTING_CORE_SERVICE__SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE__SERVICE_HPP

#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.reporting.api/domain/.hpp"
#include "ores.reporting.api/messaging/_protocol.hpp"
#include "ores.reporting.core/repository/_repository.hpp"
#include "ores.reporting.core/export.hpp"

namespace ores::reporting::service {

/**
 * @brief Service for managing configuration parameters.
 *
 * Provides a higher-level interface for configuration parameter operations,
 * wrapping the underlying repository.
 */
class ORES_REPORTING_CORE_EXPORT _service {
private:
    inline static std::string_view logger_name =
        "ores.reporting.service._service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a _service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit _service(context ctx);

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
    messaging::list_s_response list_s(const messaging::list_s_request& request);
    messaging::get__response get_(const messaging::get__request& request);
    messaging::get_many_s_response get_many_s(const messaging::get_many_s_request& request);
    messaging::put__response put_(const messaging::put__request& request);
    messaging::put_many_s_response put_many_s(const messaging::put_many_s_request& request);
    messaging::delete__response delete_(const messaging::delete__request& request);
    messaging::delete_many_s_response delete_many_s(const messaging::delete_many_s_request& request);
    messaging::list_by_configuration_id_s_response list_by_configuration_id_s(const messaging::list_by_configuration_id_s_request& request);
    messaging::list_by_parameter_definition_id_s_response list_by_parameter_definition_id_s(const messaging::list_by_parameter_definition_id_s_request& request);
    messaging::list__versions_response list__versions(const messaging::list__versions_request& request);
    messaging::get__version_response get__version(const messaging::get__version_request& request);
    /**@}*/

    /**
     * @brief Lists configuration parameters with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of configuration parameters for the requested page.
     */
    std::vector<domain::>
    list_parameter_values(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active configuration parameters.
     *
     * @return Total number of active configuration parameters.
     */
    std::uint32_t count_parameter_values();



    /**
     * @brief Lists configuration parameters filtered by configuration_id, with pagination.
     *
     * @param configuration_id The configuration_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching configuration parameters for the requested page.
     */
    std::vector<domain::>
    list_parameter_values_by_configuration_id(const std::string& configuration_id,
                                              std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active configuration parameters filtered by configuration_id.
     *
     * @param configuration_id The configuration_id to filter by.
     * @return Total number of matching configuration parameters.
     */
    std::uint32_t count_parameter_values_by_configuration_id(const std::string& configuration_id);



    /**
     * @brief Lists configuration parameters filtered by parameter_definition_id, with pagination.
     *
     * @param parameter_definition_id The parameter_definition_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching configuration parameters for the requested page.
     */
    std::vector<domain::>
    list_parameter_values_by_parameter_definition_id(const std::string& parameter_definition_id,
                                              std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active configuration parameters filtered by parameter_definition_id.
     *
     * @param parameter_definition_id The parameter_definition_id to filter by.
     * @return Total number of matching configuration parameters.
     */
    std::uint32_t count_parameter_values_by_parameter_definition_id(const std::string& parameter_definition_id);



    /**
     * @brief Retrieves a single configuration parameter as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The configuration parameter at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::>
    get_parameter_value_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single configuration parameter by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The configuration parameter if found, std::nullopt otherwise.
     */
    std::optional<domain::>
    get_parameter_value(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single configuration parameter by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The configuration parameter if found, std::nullopt otherwise.
     */
    std::optional<domain::>
    get_parameter_value_by_value(const std::string& value);

    /**
     * @brief Retrieves a batch of configuration parameters by primary key.
     */
    std::vector<domain::>
    get_parameter_values(const std::vector<std::string>& ids);

    /**
     * @brief Saves a configuration parameter (creates or updates).
     *
     * @param parameter_value The configuration parameter to save.
     * @throws std::exception on failure.
     */
    void save_parameter_value(const domain::& parameter_value);

    /**
     * @brief Saves a batch of configuration parameters.
     *
     * @param parameter_values The configuration parameters to save.
     * @throws std::exception on failure.
     */
    void save_parameter_values(
        const std::vector<domain::>& parameter_values);

    /**
     * @brief Deletes a configuration parameter by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_parameter_value(const boost::uuids::uuid& id);

    /**
     * @brief Deletes configuration parameters by their primary keys.
     */
    void delete_parameter_values(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a configuration parameter.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::>
    get_parameter_value_history(const std::string& key);

private:
    context ctx_;
    repository::_repository repo_;

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
    ores::utility::domain::result prepare_change(
        const messaging::_change& change,
        const ores::utility::domain::change_intent& intent,
        domain::& out);
};

}

#endif
