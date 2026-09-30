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
#ifndef ORES_REPORTING_CORE_SERVICE_REPORT_ANALYTIC_PARAMETER_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_REPORT_ANALYTIC_PARAMETER_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/report_analytic_parameter.hpp"
#include "ores.reporting.api/messaging/report_analytic_parameter_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/repository/report_analytic_parameter_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Service for managing analytic parameters.
 *
 * Provides a higher-level interface for analytic parameter operations,
 * wrapping the underlying repository.
 */
class ORES_REPORTING_CORE_EXPORT report_analytic_parameter_service {
private:
    inline static std::string_view logger_name =
        "ores.reporting.service.report_analytic_parameter_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a report_analytic_parameter_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit report_analytic_parameter_service(context ctx);

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
    messaging::list_report_analytic_parameters_response list_report_analytic_parameters(
        const messaging::list_report_analytic_parameters_request& request);
    messaging::get_report_analytic_parameter_response
    get_report_analytic_parameter(const messaging::get_report_analytic_parameter_request& request);
    messaging::get_many_report_analytic_parameters_response get_many_report_analytic_parameters(
        const messaging::get_many_report_analytic_parameters_request& request);
    messaging::put_report_analytic_parameter_response
    put_report_analytic_parameter(const messaging::put_report_analytic_parameter_request& request);
    messaging::put_many_report_analytic_parameters_response put_many_report_analytic_parameters(
        const messaging::put_many_report_analytic_parameters_request& request);
    messaging::delete_report_analytic_parameter_response delete_report_analytic_parameter(
        const messaging::delete_report_analytic_parameter_request& request);
    messaging::delete_many_report_analytic_parameters_response
    delete_many_report_analytic_parameters(
        const messaging::delete_many_report_analytic_parameters_request& request);
    messaging::list_by_report_analytic_id_report_analytic_parameters_response
    list_by_report_analytic_id_report_analytic_parameters(
        const messaging::list_by_report_analytic_id_report_analytic_parameters_request& request);
    messaging::list_by_parameter_definition_id_report_analytic_parameters_response
    list_by_parameter_definition_id_report_analytic_parameters(
        const messaging::list_by_parameter_definition_id_report_analytic_parameters_request&
            request);
    messaging::list_report_analytic_parameter_versions_response
    list_report_analytic_parameter_versions(
        const messaging::list_report_analytic_parameter_versions_request& request);
    messaging::get_report_analytic_parameter_version_response get_report_analytic_parameter_version(
        const messaging::get_report_analytic_parameter_version_request& request);
    /**@}*/

    /**
     * @brief Lists analytic parameters with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of analytic parameters for the requested page.
     */
    std::vector<domain::report_analytic_parameter> list_parameter_values(std::uint32_t offset,
                                                                         std::uint32_t limit);

    /**
     * @brief Gets the total count of active analytic parameters.
     *
     * @return Total number of active analytic parameters.
     */
    std::uint32_t count_parameter_values();


    /**
     * @brief Lists analytic parameters filtered by report_analytic_id, with pagination.
     *
     * @param report_analytic_id The report_analytic_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching analytic parameters for the requested page.
     */
    std::vector<domain::report_analytic_parameter> list_parameter_values_by_report_analytic_id(
        const std::string& report_analytic_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active analytic parameters filtered by report_analytic_id.
     *
     * @param report_analytic_id The report_analytic_id to filter by.
     * @return Total number of matching analytic parameters.
     */
    std::uint32_t
    count_parameter_values_by_report_analytic_id(const std::string& report_analytic_id);


    /**
     * @brief Lists analytic parameters filtered by parameter_definition_id, with pagination.
     *
     * @param parameter_definition_id The parameter_definition_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching analytic parameters for the requested page.
     */
    std::vector<domain::report_analytic_parameter> list_parameter_values_by_parameter_definition_id(
        const std::string& parameter_definition_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active analytic parameters filtered by
     * parameter_definition_id.
     *
     * @param parameter_definition_id The parameter_definition_id to filter by.
     * @return Total number of matching analytic parameters.
     */
    std::uint32_t
    count_parameter_values_by_parameter_definition_id(const std::string& parameter_definition_id);


    /**
     * @brief Retrieves a single analytic parameter as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The analytic parameter at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::report_analytic_parameter>
    get_parameter_value_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single analytic parameter by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The analytic parameter if found, std::nullopt otherwise.
     */
    std::optional<domain::report_analytic_parameter>
    get_parameter_value(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of analytic parameters by primary key.
     */
    std::vector<domain::report_analytic_parameter>
    get_parameter_values(const std::vector<std::string>& ids);

    /**
     * @brief Saves a analytic parameter (creates or updates).
     *
     * @param parameter_value The analytic parameter to save.
     * @throws std::exception on failure.
     */
    void save_parameter_value(const domain::report_analytic_parameter& parameter_value);

    /**
     * @brief Saves a batch of analytic parameters.
     *
     * @param parameter_values The analytic parameters to save.
     * @throws std::exception on failure.
     */
    void
    save_parameter_values(const std::vector<domain::report_analytic_parameter>& parameter_values);

    /**
     * @brief Deletes a analytic parameter by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_parameter_value(const boost::uuids::uuid& id);

    /**
     * @brief Deletes analytic parameters by their primary keys.
     */
    void delete_parameter_values(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a analytic parameter.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::report_analytic_parameter>
    get_parameter_value_history(const std::string& id);

private:
    context ctx_;
    repository::report_analytic_parameter_repository repo_;

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
    prepare_change(const messaging::report_analytic_parameter_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::report_analytic_parameter& out);
};

}

#endif
