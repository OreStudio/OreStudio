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
#ifndef ORES_TRADING_CORE_SERVICE_SCRIPT_RESULT_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_SCRIPT_RESULT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.trading.api/domain/script_result.hpp"
#include "ores.trading.api/messaging/script_result_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/script_result_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing script results.
 *
 * Provides a higher-level interface for script result operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT script_result_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.script_result_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a script_result_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit script_result_service(const context& ctx);

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
    ::ores::trading::messaging::list_script_results_response
    list_script_results(const ::ores::trading::messaging::list_script_results_request& request);
    ::ores::trading::messaging::get_script_result_response
    get_script_result(const ::ores::trading::messaging::get_script_result_request& request);
    ::ores::trading::messaging::get_many_script_results_response get_many_script_results(
        const ::ores::trading::messaging::get_many_script_results_request& request);
    ::ores::trading::messaging::put_script_result_response
    put_script_result(const ::ores::trading::messaging::put_script_result_request& request);
    ::ores::trading::messaging::put_many_script_results_response put_many_script_results(
        const ::ores::trading::messaging::put_many_script_results_request& request);
    ::ores::trading::messaging::delete_script_result_response
    delete_script_result(const ::ores::trading::messaging::delete_script_result_request& request);
    ::ores::trading::messaging::delete_many_script_results_response delete_many_script_results(
        const ::ores::trading::messaging::delete_many_script_results_request& request);
    ::ores::trading::messaging::list_script_result_versions_response list_script_result_versions(
        const ::ores::trading::messaging::list_script_result_versions_request& request);
    ::ores::trading::messaging::get_script_result_version_response get_script_result_version(
        const ::ores::trading::messaging::get_script_result_version_request& request);
    /**@}*/

    /**
     * @brief Lists script results with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of script results for the requested page.
     */
    std::vector<domain::script_result> list_results(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active script results.
     *
     * @return Total number of active script results.
     */
    std::uint32_t count_results();


    /**
     * @brief Retrieves a single script result as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The script result at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::script_result> get_result_at_version(const std::string& script_name,
                                                               const std::string& purpose,
                                                               const std::string& sequence_number,
                                                               std::uint32_t version);

    /**
     * @brief Retrieves a single script result by its primary key.
     *
     * @return The script result if found, std::nullopt otherwise.
     */
    std::optional<domain::script_result> get_result(const std::string& script_name,
                                                    const std::string& purpose,
                                                    const std::string& sequence_number);

    /**
     * @brief Retrieves a batch of script results by primary key.
     */
    std::vector<domain::script_result>
    get_results(const std::vector<std::string>& script_names,
                const std::vector<std::string>& purposes,
                const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Saves a script result (creates or updates).
     *
     * @param result The script result to save.
     * @throws std::exception on failure.
     */
    void save_result(const domain::script_result& result);

    /**
     * @brief Saves a batch of script results.
     *
     * @param results The script results to save.
     * @throws std::exception on failure.
     */
    void save_results(const std::vector<domain::script_result>& results);

    /**
     * @brief Deletes a script result by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_result(const std::string& script_name,
                       const std::string& purpose,
                       const std::string& sequence_number);

    /**
     * @brief Deletes script results by their primary keys.
     */
    void delete_results(const std::vector<std::string>& script_names,
                        const std::vector<std::string>& purposes,
                        const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Retrieves all historical versions of a script result.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::script_result> get_result_history(const std::string& script_name,
                                                          const std::string& purpose,
                                                          const std::string& sequence_number);

private:
    context ctx_;
    repository::script_result_repository repo_;

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
    prepare_change(const ::ores::trading::messaging::script_result_change& change,
                   const ores::utility::domain::change_intent& intent,
                   const std::vector<domain::script_result>& current,
                   domain::script_result& out);
};

}

#endif
