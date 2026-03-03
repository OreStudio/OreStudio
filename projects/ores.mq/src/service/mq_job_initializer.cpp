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
#include "ores.mq/service/mq_job_initializer.hpp"

#include <algorithm>
#include <stdexcept>
#include "ores.logging/make_logger.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.database/service/service_accounts.hpp"
#include "ores.scheduler/builder/job_definition_builder.hpp"
#include "ores.scheduler/service/cron_scheduler.hpp"

namespace ores::mq::service {

using namespace ores::logging;

namespace {

auto& lg() {
    static auto instance = make_logger("ores.mq.service.mq_job_initializer");
    return instance;
}

constexpr std::string_view job_name        = "ores.mq.metrics_scrape";
constexpr std::string_view job_description = "Scrape pgmq queue metrics into ores_mq_metrics_samples_tbl";
constexpr std::string_view job_command     = "SELECT ores_mq_scrape_metrics_fn()";
constexpr std::string_view job_schedule    = "* * * * *";
const std::string_view job_modified_by = database::service::service_accounts::comms;

} // anonymous namespace

void mq_job_initializer::initialise(const context& ctx,
                                    const boost::uuids::uuid& system_party_id) {
    BOOST_LOG_SEV(lg(), info) << "Initialising MQ metrics scrape job.";

    const auto system_tenant = utility::uuid::tenant_id::system();
    const auto result = scheduler::builder::job_definition_builder{}
        .with_name(job_name)
        .with_description(job_description)
        .with_command(job_command)
        .with_cron_schedule(job_schedule)
        .with_database(ctx.credentials().dbname)
        .with_tenant(system_tenant)
        .with_party(system_party_id)
        .with_modified_by(job_modified_by)
        .build();

    if (!result) {
        throw std::runtime_error(
            "mq_job_initializer: failed to build job definition: " + result.error());
    }

    scheduler::service::cron_scheduler cron(ctx);

    const auto existing = cron.get_all_definitions();
    const bool already_exists = std::ranges::any_of(existing,
        [](const auto& d) { return d.job_name == job_name; });

    if (already_exists) {
        BOOST_LOG_SEV(lg(), info) << "MQ metrics scrape job already registered, skipping.";
        return;
    }

    cron.schedule(*result, "SYSTEM_INIT", "MQ metrics scrape job registered at service startup");
    BOOST_LOG_SEV(lg(), info) << "MQ metrics scrape job registered successfully.";
}

} // namespace ores::mq::service
