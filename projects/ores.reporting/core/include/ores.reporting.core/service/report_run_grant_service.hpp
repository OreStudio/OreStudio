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
#ifndef ORES_REPORTING_CORE_SERVICE_REPORT_RUN_GRANT_SERVICE_HPP
#define ORES_REPORTING_CORE_SERVICE_REPORT_RUN_GRANT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.reporting.api/domain/report_definition.hpp"
#include "ores.reporting.core/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <expected>
#include <optional>
#include <string>

namespace ores::reporting::service {

/**
 * @brief The role a report run acts with, and the services that exchange its
 * grant.
 *
 * IAM's exchange reads the caller's service name from that role, so the
 * audience carries role names and not host names. Both acts that consent to a
 * run -- scheduling it and asking for one by hand -- state this pair, so it is
 * stated once, here.
 */
inline constexpr std::string_view run_role = "ReportRun";
inline constexpr std::string_view run_audience = "ReportingService,OreService,ComputeService";

/**
 * @brief The report's consent to run, created from the person who asks for a
 * run and reused by every later trigger.
 *
 * A run acts under a grant IAM issues to the person who asked for it, and the
 * definition holds that grant so the report keeps one consent rather than
 * minting a new one per trigger. Both acts that need it -- a person scheduling
 * the report and a person triggering a run by hand -- reach IAM the same way:
 * on the person's own token, because a grant is a person's consent and a
 * service account cannot give it.
 *
 * @ref ensure therefore refuses when there is no person behind the call. The
 * scheduled path is covered because scheduling created the grant first; a
 * firing that finds no grant is a definition that was never scheduled by a
 * person, and no authority exists at the firing to create one.
 */
class ORES_REPORTING_CORE_EXPORT report_run_grant_service {
private:
    inline static std::string_view logger_name = "ores.reporting.service.report_run_grant_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @param ctx         The definition's tenant and party. The write that
     *                    records the grant is made here, not in the person's
     *                    session, because the reporting service owns the
     *                    definition row.
     * @param person_nats The client that relays the person's token. The client
     *                    carries it in X-Delegated-Authorization, so IAM reads
     *                    the person as the grantor. A deployment that builds no
     *                    such client cannot create a grant at all.
     */
    report_run_grant_service(ores::database::context ctx,
                             ores::nats::service::nats_client person_nats);

    /**
     * @brief The grant the definition's runs act under, creating it if the
     * definition has none.
     *
     * A definition that already records a grant returns it unread and IAM is
     * not called, so every trigger after the first is a read. A definition
     * with none asks IAM for one, records its id on the definition, and
     * returns it.
     *
     * Two admissions racing on one definition both ask IAM, and IAM answers
     * both with one grant: the natural key it enforces is the trio of party,
     * resource and grantor, and it answers a repeated create with the grant
     * that already exists. Both writes therefore record the same id, which is
     * why no lock is needed here.
     *
     * @param def The definition the run belongs to. Its id names the resource
     *            IAM records the consent against, and its party must be the
     *            party the person acts in.
     * @return The grant's id, or why there is none.
     */
    std::expected<boost::uuids::uuid, std::string> ensure(const domain::report_definition& def);

private:
    /**
     * @brief Asks IAM, on the person's token, for the grant the definition's
     * runs act under, and returns its id.
     */
    std::expected<boost::uuids::uuid, std::string> create(const domain::report_definition& def);

    ores::database::context ctx_;
    ores::nats::service::nats_client person_nats_;
};

}

#endif
