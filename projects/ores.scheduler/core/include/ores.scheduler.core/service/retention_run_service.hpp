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
#ifndef ORES_SCHEDULER_CORE_SERVICE_RETENTION_RUN_SERVICE_HPP
#define ORES_SCHEDULER_CORE_SERVICE_RETENTION_RUN_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.scheduler.api/domain/retention_run.hpp"
#include "ores.scheduler.api/messaging/retention_run_protocol.hpp"
#include "ores.scheduler.core/export.hpp"
#include "ores.scheduler.core/repository/retention_run_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::scheduler::service {

/**
 * @brief Service for managing retention runs.
 *
 * Provides a higher-level interface for retention run operations,
 * wrapping the underlying repository.
 */
class ORES_SCHEDULER_CORE_EXPORT retention_run_service {
private:
    inline static std::string_view logger_name = "ores.scheduler.service.retention_run_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a retention_run_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit retention_run_service(const context& ctx);

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
    ::ores::scheduler::messaging::list_retention_runs_response
    list_retention_runs(const ::ores::scheduler::messaging::list_retention_runs_request& request);
    ::ores::scheduler::messaging::get_retention_run_response
    get_retention_run(const ::ores::scheduler::messaging::get_retention_run_request& request);
    ::ores::scheduler::messaging::get_many_retention_runs_response get_many_retention_runs(
        const ::ores::scheduler::messaging::get_many_retention_runs_request& request);
    /**@}*/

    /**
     * @brief Lists retention runs with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of retention runs for the requested page.
     */
    std::vector<domain::retention_run> list_runs(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active retention runs.
     *
     * @return Total number of active retention runs.
     */
    std::uint32_t count_runs();


    /**
     * @brief Retrieves a single retention run by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The retention run if found, std::nullopt otherwise.
     */
    std::optional<domain::retention_run> get_run(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of retention runs by primary key.
     */
    std::vector<domain::retention_run> get_runs(const std::vector<std::string>& ids);

    /**
     * @brief Saves a retention run (creates or updates).
     *
     * @param run The retention run to save.
     * @throws std::exception on failure.
     */
    void save_run(const domain::retention_run& run);

    /**
     * @brief Saves a batch of retention runs.
     *
     * @param runs The retention runs to save.
     * @throws std::exception on failure.
     */
    void save_runs(const std::vector<domain::retention_run>& runs);

    /**
     * @brief Deletes a retention run by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_run(const boost::uuids::uuid& id);

    /**
     * @brief Deletes retention runs by their primary keys.
     */
    void delete_runs(const std::vector<std::string>& ids);


private:
    context ctx_;
    repository::retention_run_repository repo_;
};

}

#endif
