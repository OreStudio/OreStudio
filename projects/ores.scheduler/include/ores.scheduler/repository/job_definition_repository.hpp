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
#ifndef ORES_SCHEDULER_REPOSITORY_JOB_DEFINITION_REPOSITORY_HPP
#define ORES_SCHEDULER_REPOSITORY_JOB_DEFINITION_REPOSITORY_HPP

#include <cstdint>
#include <optional>
#include <string>
#include <vector>
#include <sqlgen/postgres.hpp>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.scheduler/domain/job_definition.hpp"
#include "ores.scheduler/domain/job_instance.hpp"

namespace ores::scheduler::repository {

/**
 * @brief Reads and writes job definitions to data storage.
 */
class job_definition_repository {
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

    std::string sql();

    void write(context ctx, const domain::job_definition& v);
    void write(context ctx, const std::vector<domain::job_definition>& v);

    std::vector<domain::job_definition> read_latest(context ctx);
    std::vector<domain::job_definition>
    read_latest(context ctx, const std::string& id);
    std::vector<domain::job_definition>
    read_all(context ctx, const std::string& id);

    std::optional<domain::job_definition>
    find_by_id(context ctx, const boost::uuids::uuid& id);

    void remove(context ctx, const std::string& id);

    /**
     * @brief Sets the cron_job_id on the current active record (raw UPDATE,
     *        no new bitemporal version).
     */
    void update_cron_job_id(context ctx, const boost::uuids::uuid& id,
                            std::int64_t cron_job_id);

    /**
     * @brief Clears cron_job_id and sets is_active=0 on the current active
     *        record (raw UPDATE, no new bitemporal version).
     */
    void clear_cron_job_id(context ctx, const boost::uuids::uuid& id);

    /**
     * @brief Returns pg_cron execution history for the given job definition.
     *
     * Queries cron.job_run_details in the postgres database via the
     * cron_job_id stored on the job definition.
     */
    std::vector<domain::job_instance>
    get_job_history(context ctx, const boost::uuids::uuid& id, std::size_t limit);
};

}

#endif
