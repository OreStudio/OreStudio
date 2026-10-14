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
#include "ores.reporting.core/service/report_run_grant_service.hpp"
#include "ores.iam.api/messaging/run_grant_operations_protocol.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.reporting.core/service/report_definition_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <stdexcept>
#include <utility>

namespace ores::reporting::service {

using namespace ores::logging;

namespace {

/**
 * @brief The resource IAM records a report's consent against.
 *
 * The definition's own id, so one definition holds one consent however many
 * runs it produces.
 */
std::string resource_of(const domain::report_definition& def) {
    return "reporting.report_definition/" + boost::uuids::to_string(def.id);
}

}

report_run_grant_service::report_run_grant_service(ores::database::context ctx,
                                                   ores::nats::service::nats_client person_nats)
    : ctx_(std::move(ctx))
    , person_nats_(std::move(person_nats)) {}

std::expected<boost::uuids::uuid, std::string>
report_run_grant_service::create(const domain::report_definition& def) {
    ores::iam::messaging::create_run_grant_request req;
    req.resource = resource_of(def);
    req.role = std::string(run_role);
    req.audience = std::string(run_audience);

    const auto& codec = ores::nats::default_wire_codec();
    try {
        const auto reply = person_nats_.authenticated_request(
            ores::iam::messaging::create_run_grant_request::nats_subject, codec.encode(req));
        if (const auto it = reply.headers.find(std::string(ores::nats::headers::x_error));
            it != reply.headers.end())
            return std::unexpected("IAM refused the run grant for definition " +
                                   boost::uuids::to_string(def.id) + ": " + it->second);
        const auto granted =
            codec.decode<ores::iam::messaging::create_run_grant_response>(reply.data);
        if (!granted)
            return std::unexpected("IAM answered the run grant for definition " +
                                   boost::uuids::to_string(def.id) + " unreadably");
        if (!granted->success)
            return std::unexpected(granted->message);
        return boost::uuids::string_generator()(granted->grant_id);
    } catch (const std::exception& e) {
        return std::unexpected(std::string("IAM run grant call failed: ") + e.what());
    }
}

std::expected<boost::uuids::uuid, std::string>
report_run_grant_service::ensure(const domain::report_definition& def) {
    if (def.run_grant_id)
        return *def.run_grant_id;

    const auto party = ctx_.party_id();
    if (!party || *party != def.party_id)
        return std::unexpected("The definition's runs act in its own party, and this session is "
                               "in another: act for that party to consent to its runs.");

    const auto grant_id = create(def);
    if (!grant_id)
        return std::unexpected(grant_id.error());

    // The definition records the consent, not the trigger: every later run
    // reads it here rather than asking IAM again, and the record is what makes
    // the reuse visible to any process that reads the definition. The write is
    // made in the definition's tenant, because the request context carries the
    // tenant the person asked from and a trigger may name a definition in
    // another one.
    auto updated = def;
    updated.run_grant_id = *grant_id;
    updated.change_reason_code = std::string(ores::service::messaging::change_reasons::update);
    updated.change_commentary = "Recorded the consent a run needs";
    report_definition_service svc(ctx_.with_tenant(
        def.tenant_id, ctx_.actor().empty() ? ctx_.service_account() : ctx_.actor()));
    svc.save_definition(updated);

    BOOST_LOG_SEV(lg(), info) << "Recorded run grant " << boost::uuids::to_string(*grant_id)
                              << " on definition " << boost::uuids::to_string(def.id);
    return grant_id;
}

}
