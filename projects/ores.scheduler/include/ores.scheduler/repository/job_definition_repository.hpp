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
#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.scheduler/domain/job_definition.hpp"
#include "ores.scheduler/domain/job_instance.hpp"

namespace ores::scheduler::repository {

/**
 * @brief Persistence layer for job_definition and job_instance.
 *
 * job_definition rows are stored in ores_scheduler_job_definitions_tbl.
 * job_instance data is read from cron.job_run_details (pg_cron system table)
 * joined with job definitions to enforce tenant and party isolation.
 */
class job_definition_repository final {
private:
    inline static std::string_view logger_name =
        "ores.scheduler.repository.job_definition_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit job_definition_repository(context ctx);

    // -------------------------------------------------------------------------
    // job_definition persistence
    // -------------------------------------------------------------------------

    /**
     * @brief Insert or update a job_definition.
     *
     * On first write: inserts a new row (version=0, trigger sets version=1).
     * On update: inserts a new temporal row closing the previous one.
     */
    void save(const domain::job_definition& def,
              const std::string& change_reason_code = "created",
              const std::string& change_commentary = "");

    /**
     * @brief Retrieve a single job_definition by our UUID.
     *
     * Returns nullopt if not found or not visible under current RLS context.
     */
    [[nodiscard]] std::optional<domain::job_definition>
    find_by_id(const boost::uuids::uuid& id) const;

    /**
     * @brief Retrieve a job_definition by its pg_cron job ID.
     *
     * Used to re-link a definition after calling cron.schedule().
     */
    [[nodiscard]] std::optional<domain::job_definition>
    find_by_cron_job_id(std::int64_t cron_job_id) const;

    /**
     * @brief All active job_definitions visible to the current tenant+party.
     */
    [[nodiscard]] std::vector<domain::job_definition> get_all() const;

    /**
     * @brief Update the cron_job_id after pg_cron assigns a job ID.
     *
     * Called by cron_scheduler immediately after cron.schedule() succeeds.
     */
    void update_cron_job_id(const boost::uuids::uuid& id,
                            std::int64_t cron_job_id,
                            const std::string& modified_by);

    /**
     * @brief Clear the cron_job_id when a job is paused (unscheduled).
     */
    void clear_cron_job_id(const boost::uuids::uuid& id,
                           const std::string& modified_by);

    // -------------------------------------------------------------------------
    // job_instance queries (read-only, from cron.job_run_details)
    // -------------------------------------------------------------------------

    /**
     * @brief Recent execution history for a specific job_definition.
     *
     * Queries cron.job_run_details filtered to the cron_job_id of the given
     * job_definition UUID. Results are ordered newest-first.
     *
     * @param limit Maximum number of instances to return (default: 100).
     */
    [[nodiscard]] std::vector<domain::job_instance>
    get_job_history(const boost::uuids::uuid& job_definition_id,
                    std::size_t limit = 100) const;

private:
    context ctx_;
};

} // namespace ores::scheduler::repository
