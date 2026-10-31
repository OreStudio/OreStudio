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
#ifndef ORES_REPORTING_CORE_SERVICE_RUN_RESULT_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_RUN_RESULT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/domain/run_result.hpp"
#include "ores.reporting.api/messaging/run_result_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/repository/run_result_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Service for managing run results.
 *
 * Provides a higher-level interface for run result operations,
 * wrapping the underlying repository.
 */
class ORES_REPORTING_CORE_EXPORT run_result_service {
private:
    inline static std::string_view logger_name = "ores.reporting.service.run_result_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a run_result_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit run_result_service(context ctx);

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
    ::ores::reporting::messaging::list_run_results_response
    list_run_results(const ::ores::reporting::messaging::list_run_results_request& request);
    ::ores::reporting::messaging::get_run_result_response
    get_run_result(const ::ores::reporting::messaging::get_run_result_request& request);
    ::ores::reporting::messaging::get_many_run_results_response
    get_many_run_results(const ::ores::reporting::messaging::get_many_run_results_request& request);
    /**@}*/

    /**
     * @brief Lists run results with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of run results for the requested page.
     */
    std::vector<domain::run_result> list_results(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active run results.
     *
     * @return Total number of active run results.
     */
    std::uint32_t count_results();


    /**
     * @brief Retrieves a single run result by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The run result if found, std::nullopt otherwise.
     */
    std::optional<domain::run_result> get_result(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of run results by primary key.
     */
    std::vector<domain::run_result> get_results(const std::vector<std::string>& ids);

    /**
     * @brief Saves a run result (creates or updates).
     *
     * @param result The run result to save.
     * @throws std::exception on failure.
     */
    void save_result(const domain::run_result& result);

    /**
     * @brief Saves a batch of run results.
     *
     * @param results The run results to save.
     * @throws std::exception on failure.
     */
    void save_results(const std::vector<domain::run_result>& results);

    /**
     * @brief Deletes a run result by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_result(const boost::uuids::uuid& id);

    /**
     * @brief Deletes run results by their primary keys.
     */
    void delete_results(const std::vector<std::string>& ids);


private:
    context ctx_;
    repository::run_result_repository repo_;
};

}

#endif
