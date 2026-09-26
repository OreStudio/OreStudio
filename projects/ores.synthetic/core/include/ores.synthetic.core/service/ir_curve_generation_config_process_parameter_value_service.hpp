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
#ifndef ORES_SYNTHETIC_CORE_SERVICE_IR_CURVE_GENERATION_CONFIG_PROCESS_PARAMETER_VALUE_SERVICE_HPP
#define ORES_SYNTHETIC_CORE_SERVICE_IR_CURVE_GENERATION_CONFIG_PROCESS_PARAMETER_VALUE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config_process_parameter_value.hpp"
#include "ores.synthetic.api/messaging/ir_curve_generation_config_process_parameter_value_protocol.hpp"
#include "ores.synthetic.core/export.hpp"
#include "ores.synthetic.core/repository/ir_curve_generation_config_process_parameter_value_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Service for managing IR curve generation config process parameter values.
 *
 * Provides a higher-level interface for IR curve generation config process parameter value
 * operations, wrapping the underlying repository.
 */
class ORES_SYNTHETIC_CORE_EXPORT ir_curve_generation_config_process_parameter_value_service {
private:
    inline static std::string_view logger_name =
        "ores.synthetic.service.ir_curve_generation_config_process_parameter_value_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a ir_curve_generation_config_process_parameter_value_service with a
     * database context.
     *
     * @param ctx The database context for operations.
     */
    explicit ir_curve_generation_config_process_parameter_value_service(context ctx);

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
    messaging::list_ir_curve_generation_config_process_parameter_values_response
    list_ir_curve_generation_config_process_parameter_values(
        const messaging::list_ir_curve_generation_config_process_parameter_values_request& request);
    messaging::get_ir_curve_generation_config_process_parameter_value_response
    get_ir_curve_generation_config_process_parameter_value(
        const messaging::get_ir_curve_generation_config_process_parameter_value_request& request);
    messaging::get_many_ir_curve_generation_config_process_parameter_values_response
    get_many_ir_curve_generation_config_process_parameter_values(
        const messaging::get_many_ir_curve_generation_config_process_parameter_values_request&
            request);
    messaging::put_ir_curve_generation_config_process_parameter_value_response
    put_ir_curve_generation_config_process_parameter_value(
        const messaging::put_ir_curve_generation_config_process_parameter_value_request& request);
    messaging::put_many_ir_curve_generation_config_process_parameter_values_response
    put_many_ir_curve_generation_config_process_parameter_values(
        const messaging::put_many_ir_curve_generation_config_process_parameter_values_request&
            request);
    messaging::delete_ir_curve_generation_config_process_parameter_value_response
    delete_ir_curve_generation_config_process_parameter_value(
        const messaging::delete_ir_curve_generation_config_process_parameter_value_request&
            request);
    messaging::delete_many_ir_curve_generation_config_process_parameter_values_response
    delete_many_ir_curve_generation_config_process_parameter_values(
        const messaging::delete_many_ir_curve_generation_config_process_parameter_values_request&
            request);
    messaging::list_ir_curve_generation_config_process_parameter_value_versions_response
    list_ir_curve_generation_config_process_parameter_value_versions(
        const messaging::list_ir_curve_generation_config_process_parameter_value_versions_request&
            request);
    messaging::get_ir_curve_generation_config_process_parameter_value_version_response
    get_ir_curve_generation_config_process_parameter_value_version(
        const messaging::get_ir_curve_generation_config_process_parameter_value_version_request&
            request);
    /**@}*/

    /**
     * @brief Lists IR curve generation config process parameter values with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of IR curve generation config process parameter values for the requested page.
     */
    std::vector<domain::ir_curve_generation_config_process_parameter_value>
    list_process_parameter_values(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active IR curve generation config process parameter values.
     *
     * @return Total number of active IR curve generation config process parameter values.
     */
    std::uint32_t count_process_parameter_values();


    /**
     * @brief Retrieves a single IR curve generation config process parameter value as it stood at a
     * specific version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The IR curve generation config process parameter value at that version if found,
     * std::nullopt otherwise.
     */
    std::optional<domain::ir_curve_generation_config_process_parameter_value>
    get_process_parameter_value_at_version(const boost::uuids::uuid& id, std::uint32_t version);

    /**
     * @brief Retrieves a single IR curve generation config process parameter value by its primary
     * key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The IR curve generation config process parameter value if found, std::nullopt
     * otherwise.
     */
    std::optional<domain::ir_curve_generation_config_process_parameter_value>
    get_process_parameter_value(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of IR curve generation config process parameter values by primary
     * key.
     */
    std::vector<domain::ir_curve_generation_config_process_parameter_value>
    get_process_parameter_values(const std::vector<std::string>& ids);

    /**
     * @brief Saves a IR curve generation config process parameter value (creates or updates).
     *
     * @param process_parameter_value The IR curve generation config process parameter value to
     * save.
     * @throws std::exception on failure.
     */
    void save_process_parameter_value(
        const domain::ir_curve_generation_config_process_parameter_value& process_parameter_value);

    /**
     * @brief Saves a batch of IR curve generation config process parameter values.
     *
     * @param process_parameter_values The IR curve generation config process parameter values to
     * save.
     * @throws std::exception on failure.
     */
    void save_process_parameter_values(
        const std::vector<domain::ir_curve_generation_config_process_parameter_value>&
            process_parameter_values);

    /**
     * @brief Deletes a IR curve generation config process parameter value by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_process_parameter_value(const boost::uuids::uuid& id);

    /**
     * @brief Deletes IR curve generation config process parameter values by their primary keys.
     */
    void delete_process_parameter_values(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a IR curve generation config process parameter
     * value.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::ir_curve_generation_config_process_parameter_value>
    get_process_parameter_value_history(const std::string& id);

private:
    context ctx_;
    repository::ir_curve_generation_config_process_parameter_value_repository repo_;

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
        const messaging::ir_curve_generation_config_process_parameter_value_change& change,
        const ores::utility::domain::change_intent& intent,
        domain::ir_curve_generation_config_process_parameter_value& out);
};

}

#endif
