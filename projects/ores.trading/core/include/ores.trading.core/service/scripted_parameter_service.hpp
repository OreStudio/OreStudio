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
#ifndef ORES_TRADING_CORE_SERVICE_SCRIPTED_PARAMETER_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_SCRIPTED_PARAMETER_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/scripted_parameter.hpp"
#include "ores.trading.api/messaging/scripted_parameter_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/scripted_parameter_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing scripted parameters.
 *
 * Provides a higher-level interface for scripted parameter operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT scripted_parameter_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.scripted_parameter_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a scripted_parameter_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit scripted_parameter_service(const context& ctx);

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
    ::ores::trading::messaging::list_scripted_parameters_response list_scripted_parameters(
        const ::ores::trading::messaging::list_scripted_parameters_request& request);
    ::ores::trading::messaging::get_scripted_parameter_response get_scripted_parameter(
        const ::ores::trading::messaging::get_scripted_parameter_request& request);
    ::ores::trading::messaging::get_many_scripted_parameters_response get_many_scripted_parameters(
        const ::ores::trading::messaging::get_many_scripted_parameters_request& request);
    ::ores::trading::messaging::put_scripted_parameter_response put_scripted_parameter(
        const ::ores::trading::messaging::put_scripted_parameter_request& request);
    ::ores::trading::messaging::put_many_scripted_parameters_response put_many_scripted_parameters(
        const ::ores::trading::messaging::put_many_scripted_parameters_request& request);
    ::ores::trading::messaging::delete_scripted_parameter_response delete_scripted_parameter(
        const ::ores::trading::messaging::delete_scripted_parameter_request& request);
    ::ores::trading::messaging::delete_many_scripted_parameters_response
    delete_many_scripted_parameters(
        const ::ores::trading::messaging::delete_many_scripted_parameters_request& request);
    ::ores::trading::messaging::list_by_trade_id_scripted_parameters_response
    list_by_trade_id_scripted_parameters(
        const ::ores::trading::messaging::list_by_trade_id_scripted_parameters_request& request);
    ::ores::trading::messaging::list_scripted_parameter_versions_response
    list_scripted_parameter_versions(
        const ::ores::trading::messaging::list_scripted_parameter_versions_request& request);
    ::ores::trading::messaging::get_scripted_parameter_version_response
    get_scripted_parameter_version(
        const ::ores::trading::messaging::get_scripted_parameter_version_request& request);
    /**@}*/

    /**
     * @brief Lists scripted parameters with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of scripted parameters for the requested page.
     */
    std::vector<domain::scripted_parameter> list_scripted_parameters(std::uint32_t offset,
                                                                     std::uint32_t limit);

    /**
     * @brief Gets the total count of active scripted parameters.
     *
     * @return Total number of active scripted parameters.
     */
    std::uint32_t count_scripted_parameters();


    /**
     * @brief Lists scripted parameters filtered by trade_id, with pagination.
     *
     * @param trade_id The trade_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching scripted parameters for the requested page.
     */
    std::vector<domain::scripted_parameter> list_scripted_parameters_by_trade_id(
        const std::string& trade_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active scripted parameters filtered by trade_id.
     *
     * @param trade_id The trade_id to filter by.
     * @return Total number of matching scripted parameters.
     */
    std::uint32_t count_scripted_parameters_by_trade_id(const std::string& trade_id);


    /**
     * @brief Retrieves a single scripted parameter as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The scripted parameter at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::scripted_parameter> get_scripted_parameter_at_version(
        const std::string& trade_id, const std::string& sequence_number, std::uint32_t version);

    /**
     * @brief Retrieves a single scripted parameter by its primary key.
     *
     * @return The scripted parameter if found, std::nullopt otherwise.
     */
    std::optional<domain::scripted_parameter>
    get_scripted_parameter(const std::string& trade_id, const std::string& sequence_number);

    /**
     * @brief Retrieves a batch of scripted parameters by primary key.
     */
    std::vector<domain::scripted_parameter>
    get_scripted_parameters(const std::vector<std::string>& trade_ids,
                            const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Saves a scripted parameter (creates or updates).
     *
     * @param scripted_parameter The scripted parameter to save.
     * @throws std::exception on failure.
     */
    void save_scripted_parameter(const domain::scripted_parameter& scripted_parameter);

    /**
     * @brief Saves a batch of scripted parameters.
     *
     * @param scripted_parameters The scripted parameters to save.
     * @throws std::exception on failure.
     */
    void
    save_scripted_parameters(const std::vector<domain::scripted_parameter>& scripted_parameters);

    /**
     * @brief Deletes a scripted parameter by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_scripted_parameter(const std::string& trade_id, const std::string& sequence_number);

    /**
     * @brief Deletes scripted parameters by their primary keys.
     */
    void delete_scripted_parameters(const std::vector<std::string>& trade_ids,
                                    const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Retrieves all historical versions of a scripted parameter.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::scripted_parameter>
    get_scripted_parameter_history(const std::string& trade_id, const std::string& sequence_number);

private:
    context ctx_;
    repository::scripted_parameter_repository repo_;

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
    prepare_change(const ::ores::trading::messaging::scripted_parameter_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::scripted_parameter& out);
};

}

#endif
