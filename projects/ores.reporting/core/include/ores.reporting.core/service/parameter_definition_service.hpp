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
#ifndef ORES_REPORTING_CORE_SERVICE_PARAMETER_DEFINITION_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_PARAMETER_DEFINITION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/parameter_definition.hpp"
#include "ores.reporting.api/messaging/parameter_definition_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/repository/parameter_definition_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Service for managing parameter definitions.
 *
 * Provides a higher-level interface for parameter definition operations,
 * wrapping the underlying repository.
 */
class ORES_REPORTING_CORE_EXPORT parameter_definition_service {
private:
    inline static std::string_view logger_name =
        "ores.reporting.service.parameter_definition_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a parameter_definition_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit parameter_definition_service(context ctx);

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
    messaging::list_parameter_definitions_response
    list_parameter_definitions(const messaging::list_parameter_definitions_request& request);
    messaging::get_parameter_definition_response
    get_parameter_definition(const messaging::get_parameter_definition_request& request);
    messaging::get_many_parameter_definitions_response get_many_parameter_definitions(
        const messaging::get_many_parameter_definitions_request& request);
    messaging::put_parameter_definition_response
    put_parameter_definition(const messaging::put_parameter_definition_request& request);
    messaging::put_many_parameter_definitions_response put_many_parameter_definitions(
        const messaging::put_many_parameter_definitions_request& request);
    messaging::delete_parameter_definition_response
    delete_parameter_definition(const messaging::delete_parameter_definition_request& request);
    messaging::delete_many_parameter_definitions_response delete_many_parameter_definitions(
        const messaging::delete_many_parameter_definitions_request& request);
    messaging::list_by_parameter_value_domain_code_parameter_definitions_response
    list_by_parameter_value_domain_code_parameter_definitions(
        const messaging::list_by_parameter_value_domain_code_parameter_definitions_request&
            request);
    messaging::list_parameter_definition_versions_response list_parameter_definition_versions(
        const messaging::list_parameter_definition_versions_request& request);
    messaging::get_parameter_definition_version_response get_parameter_definition_version(
        const messaging::get_parameter_definition_version_request& request);
    /**@}*/

    /**
     * @brief Lists parameter definitions with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of parameter definitions for the requested page.
     */
    std::vector<domain::parameter_definition> list_parameters(std::uint32_t offset,
                                                              std::uint32_t limit);

    /**
     * @brief Gets the total count of active parameter definitions.
     *
     * @return Total number of active parameter definitions.
     */
    std::uint32_t count_parameters();


    /**
     * @brief Lists parameter definitions filtered by parameter_value_domain_code, with pagination.
     *
     * @param parameter_value_domain_code The parameter_value_domain_code to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching parameter definitions for the requested page.
     */
    std::vector<domain::parameter_definition> list_parameters_by_parameter_value_domain_code(
        const std::string& parameter_value_domain_code, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active parameter definitions filtered by
     * parameter_value_domain_code.
     *
     * @param parameter_value_domain_code The parameter_value_domain_code to filter by.
     * @return Total number of matching parameter definitions.
     */
    std::uint32_t
    count_parameters_by_parameter_value_domain_code(const std::string& parameter_value_domain_code);


    /**
     * @brief Retrieves a single parameter definition as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The parameter definition at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::parameter_definition>
    get_parameter_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single parameter definition by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The parameter definition if found, std::nullopt otherwise.
     */
    std::optional<domain::parameter_definition> get_parameter(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single parameter definition by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The parameter definition if found, std::nullopt otherwise.
     */
    std::optional<domain::parameter_definition> get_parameter_by_name(const std::string& name);

    /**
     * @brief Retrieves a batch of parameter definitions by primary key.
     */
    std::vector<domain::parameter_definition> get_parameters(const std::vector<std::string>& ids);

    /**
     * @brief Saves a parameter definition (creates or updates).
     *
     * @param parameter The parameter definition to save.
     * @throws std::exception on failure.
     */
    void save_parameter(const domain::parameter_definition& parameter);

    /**
     * @brief Saves a batch of parameter definitions.
     *
     * @param parameters The parameter definitions to save.
     * @throws std::exception on failure.
     */
    void save_parameters(const std::vector<domain::parameter_definition>& parameters);

    /**
     * @brief Deletes a parameter definition by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_parameter(const boost::uuids::uuid& id);

    /**
     * @brief Deletes parameter definitions by their primary keys.
     */
    void delete_parameters(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a parameter definition.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::parameter_definition> get_parameter_history(const std::string& key);

private:
    context ctx_;
    repository::parameter_definition_repository repo_;

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
    prepare_change(const messaging::parameter_definition_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::parameter_definition& out);
};

}

#endif
