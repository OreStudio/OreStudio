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
#ifndef ORES_TRADING_CORE_SERVICE_SCRIPT_INPUT_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_SCRIPT_INPUT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.trading.api/domain/script_input.hpp"
#include "ores.trading.api/messaging/script_input_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/script_input_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing script inputs.
 *
 * Provides a higher-level interface for script input operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT script_input_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.script_input_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a script_input_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit script_input_service(const context& ctx);

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
    ::ores::trading::messaging::list_script_inputs_response
    list_script_inputs(const ::ores::trading::messaging::list_script_inputs_request& request);
    ::ores::trading::messaging::get_script_input_response
    get_script_input(const ::ores::trading::messaging::get_script_input_request& request);
    ::ores::trading::messaging::get_many_script_inputs_response get_many_script_inputs(
        const ::ores::trading::messaging::get_many_script_inputs_request& request);
    ::ores::trading::messaging::put_script_input_response
    put_script_input(const ::ores::trading::messaging::put_script_input_request& request);
    ::ores::trading::messaging::put_many_script_inputs_response put_many_script_inputs(
        const ::ores::trading::messaging::put_many_script_inputs_request& request);
    ::ores::trading::messaging::delete_script_input_response
    delete_script_input(const ::ores::trading::messaging::delete_script_input_request& request);
    ::ores::trading::messaging::delete_many_script_inputs_response delete_many_script_inputs(
        const ::ores::trading::messaging::delete_many_script_inputs_request& request);
    ::ores::trading::messaging::list_script_input_versions_response list_script_input_versions(
        const ::ores::trading::messaging::list_script_input_versions_request& request);
    ::ores::trading::messaging::get_script_input_version_response get_script_input_version(
        const ::ores::trading::messaging::get_script_input_version_request& request);
    /**@}*/

    /**
     * @brief Lists script inputs with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of script inputs for the requested page.
     */
    std::vector<domain::script_input> list_inputs(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active script inputs.
     *
     * @return Total number of active script inputs.
     */
    std::uint32_t count_inputs();


    /**
     * @brief Retrieves a single script input as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The script input at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::script_input> get_input_at_version(const std::string& script_name,
                                                             const std::string& sequence_number,
                                                             std::uint32_t version);

    /**
     * @brief Retrieves a single script input by its primary key.
     *
     * @return The script input if found, std::nullopt otherwise.
     */
    std::optional<domain::script_input> get_input(const std::string& script_name,
                                                  const std::string& sequence_number);

    /**
     * @brief Retrieves a batch of script inputs by primary key.
     */
    std::vector<domain::script_input> get_inputs(const std::vector<std::string>& script_names,
                                                 const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Saves a script input (creates or updates).
     *
     * @param input The script input to save.
     * @throws std::exception on failure.
     */
    void save_input(const domain::script_input& input);

    /**
     * @brief Saves a batch of script inputs.
     *
     * @param inputs The script inputs to save.
     * @throws std::exception on failure.
     */
    void save_inputs(const std::vector<domain::script_input>& inputs);

    /**
     * @brief Deletes a script input by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_input(const std::string& script_name, const std::string& sequence_number);

    /**
     * @brief Deletes script inputs by their primary keys.
     */
    void delete_inputs(const std::vector<std::string>& script_names,
                       const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Retrieves all historical versions of a script input.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::script_input> get_input_history(const std::string& script_name,
                                                        const std::string& sequence_number);

private:
    context ctx_;
    repository::script_input_repository repo_;

    /**
     * @brief Checks one change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check, the
     * server-derived provenance and the version the store must match are one
     * decision made in one place. A batch that made the decision per element
     * would eventually make it differently from the single write.
     *
     * The caller reads the current row, so a batch reads every row it names
     * in one query rather than one query per change.
     *
     * @param change The change as the caller stated it.
     * @param intent The reason and commentary the caller gave.
     * @param current The row the change names as it is now, or empty.
     * @param out The stamped domain object, written only when the result is ok.
     * @return ok, or why the change was refused.
     */
    ores::utility::domain::result
    prepare_change(const ::ores::trading::messaging::script_input_change& change,
                   const ores::utility::domain::change_intent& intent,
                   const std::vector<domain::script_input>& current,
                   domain::script_input& out);
};

}

#endif
