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
#ifndef ORES_REPORTING_CORE_SERVICE_REPORT_RUN_ADMISSION_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_REPORT_RUN_ADMISSION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.reporting.core/service/report_run_grant_service.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

namespace ores::reporting::service {

/**
 * @brief Records the trigger that admitted a report run, and admits the run.
 *
 * A run is admitted one way whatever asked for it: the trigger row is written
 * first, and the instance then names it. The database holds
 * report_instance.trigger_id as a real foreign key, so an instance without its
 * trigger is refused, and the two are written here together rather than at
 * every caller.
 *
 * This is the one entry point both paths call. The scheduler's trigger handler
 * and the on-demand command of the report operation each build a plan and
 * admit; the plan says what asked for the run and nothing about which path
 * built it.
 *
 * The service does not check permissions, read the definition or apply the
 * definition's concurrency policy. A caller does that before it admits, because
 * the refusal is the caller's to report.
 *
 * The run's consent to act is the definition's, and the caller settles it
 * before it builds the plan: see @ref report_run_grant_service, which both
 * acts that need a consent call.
 *
 * The table is =:read_only:=, which suppresses the generated write verb, so
 * this service writes the trigger through the generated repository by hand, as
 * =ores.iam.auth_event= does.
 */
class ORES_REPORTING_CORE_EXPORT report_run_admission_service {
private:
    inline static std::string_view logger_name =
        "ores.reporting.service.report_run_admission_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Everything the admission copies onto the instance it creates.
     *
     * The caller states the definition's own values, because only the caller
     * has read the definition. The admission states the trigger and the time,
     * so both are one reading of the clock.
     */
    struct plan {
        boost::uuids::uuid definition_id;
        // A tenant id has no default constructor, so the member states one and
        // the caller overwrites it with the definition's.
        utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
        boost::uuids::uuid party_id;
        std::string name;
        std::string description;

        /**
         * @brief What asked for the run: =on_demand= or =scheduled=.
         */
        std::string trigger_type;

        std::optional<boost::uuids::uuid> scheduler_job_id;
        std::optional<std::int64_t> schedule_job_run_id;

        std::string kind;
        std::string output_message = std::string();
        std::optional<boost::uuids::uuid> run_grant_id;
        std::optional<boost::uuids::uuid> fsm_state_id;

        /**
         * @brief When the run began, stated by the caller because only the
         * caller knows whether the instance will run at all.
         */
        std::optional<std::chrono::system_clock::time_point> started_at;
    };

    /**
     * @brief What the admission wrote, so a caller replies with the ids rather
     * than re-reading the rows it just made.
     */
    struct receipt {
        boost::uuids::uuid trigger_id;
        boost::uuids::uuid instance_id;
    };

    explicit report_run_admission_service(context ctx);

    /**
     * @brief Writes the trigger row and the instance that names it.
     *
     * The trigger is the report's own event: it records the definition it runs,
     * the party the run acts in, the person or service the caller acts as, and
     * the instant. A scheduled plan names the scheduler job and the firing that
     * produced it; an on-demand plan names neither, and the trigger type carries
     * that difference.
     *
     * @throws std::invalid_argument when the plan states no trigger type, or
     * states a scheduled trigger without the scheduler job that fired, which
     * the trigger table's own check refuses.
     */
    receipt admit(const plan& p);

private:
    context ctx_;
};

}

#endif
