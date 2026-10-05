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
#include "ores.ore.service/messaging/run_configuration_handler.hpp"
#include "ores.nats/domain/correlation.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.ore.api/messaging/run_configuration_protocol.hpp"
#include "ores.ore.service/messaging/run_configuration_operations.hpp"
#include "ores.service/error_code.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.workflow.api/messaging/workflow_protocol.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <format>
#include <rfl/json.hpp>

namespace ores::ore::service::messaging {

using namespace ores::logging;
using namespace ores::service::messaging;
using namespace ores::ore::messaging;

namespace {

input_files files_of(const std::vector<run_input_file>& files) {
    input_files out;
    for (const auto& f : files)
        out[f.name] = f.content;
    return out;
}

}

run_configuration_handler::run_configuration_handler(ores::nats::service::client& nats,
                                                     ores::database::context ctx,
                                                     ores::security::jwt::jwt_authenticator signer,
                                                     ores::nats::service::nats_client outbound_nats)
    : nats_(nats)
    , ctx_(std::move(ctx))
    , signer_(std::move(signer))
    , outbound_nats_(std::move(outbound_nats)) {}

void run_configuration_handler::import_run_configuration(ores::nats::message msg) {
    using ores::workflow::messaging::start_workflow_message;
    const auto correlation_id = ores::nats::extract_or_generate_correlation_id(msg);
    auto ctx = ores::service::service::make_request_context(
        ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>(signer_));
    if (!ctx) {
        error_reply(nats_, msg, ctx.error());
        return;
    }
    if (!has_permission(*ctx, "reporting::report_run_setups:write")) {
        error_reply(nats_, msg, ores::service::error_code::forbidden);
        return;
    }
    const auto req = decode<import_run_configuration_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }
    if (req->report_definition_id.empty()) {
        reply(nats_,
              msg,
              import_run_configuration_response{.message = "report_definition_id is required.",
                                                .correlation_id = correlation_id});
        return;
    }

    run_configuration_import_execute_request execute{
        .report_definition_id = req->report_definition_id,
        .name = req->name,
        .files = req->files,
        .correlation_id = correlation_id,
        .bearer_token = ores::nats::service::extract_bearer(msg)};

    const auto instance_id = boost::uuids::to_string(boost::uuids::random_generator()());
    start_workflow_message swm;
    swm.type = "run_configuration_import_workflow";
    swm.tenant_id = boost::uuids::to_string(ctx->tenant_id().to_uuid());
    swm.request_json = rfl::json::write(execute);
    swm.correlation_id = correlation_id;
    swm.instance_id = instance_id;
    nats_.js_publish(start_workflow_message::nats_subject,
                     ores::nats::default_wire_codec().encode(swm),
                     ores::nats::service::forwarded_caller_headers(msg));

    BOOST_LOG_SEV(lg(), info) << "run configuration import dispatched | corr=" << correlation_id
                              << " instance=" << instance_id;
    reply(nats_,
          msg,
          import_run_configuration_response{.success = true,
                                            .message = "Run configuration import submitted.",
                                            .correlation_id = correlation_id,
                                            .workflow_instance_id = instance_id});
}

void run_configuration_handler::export_run_configuration(ores::nats::message msg) {
    const auto correlation_id = ores::nats::extract_or_generate_correlation_id(msg);
    auto ctx = ores::service::service::make_request_context(
        ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>(signer_));
    if (!ctx) {
        error_reply(nats_, msg, ctx.error());
        return;
    }
    if (!has_permission(*ctx, "reporting::report_run_setups:read")) {
        error_reply(nats_, msg, ores::service::error_code::forbidden);
        return;
    }
    const auto req = decode<export_run_configuration_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }
    try {
        auto owners = outbound_nats_.with_delegation(ores::nats::service::extract_bearer(msg))
                          .with_correlation_id(correlation_id);
        export_run_configuration_response response;
        for (const auto& [name, content] : export_run(owners, req->report_definition_id))
            response.files.push_back({name, content});
        response.success = true;
        response.message = std::format("Exported {} files.", response.files.size());
        reply(nats_, msg, response);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "run configuration export refused | corr=" << correlation_id
                                  << " " << e.what();
        reply(nats_, msg, export_run_configuration_response{.message = e.what()});
    }
}

run_configuration_import_handler::run_configuration_import_handler(
    ores::nats::service::client& nats, ores::nats::service::nats_client outbound_nats)
    : nats_(nats)
    , outbound_nats_(std::move(outbound_nats)) {}

void run_configuration_import_handler::execute(ores::nats::message msg) {
    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf)
        return;
    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    const auto req = rfl::json::read<run_configuration_import_execute_request>(sv);
    if (!req) {
        wf->fail("Failed to decode run_configuration_import_execute_request");
        return;
    }
    try {
        auto owners = outbound_nats_.with_delegation(req->bearer_token)
                          .with_correlation_id(req->correlation_id);
        const auto result =
            import_run(owners, req->report_definition_id, req->name, files_of(req->files));
        BOOST_LOG_SEV(lg(), info) << "run configuration imported | corr=" << req->correlation_id
                                  << " stored=" << result.stored.size();
        wf->complete(rfl::json::write(result));
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "run configuration import failed | corr="
                                  << req->correlation_id << " " << e.what();
        wf->fail(e.what());
    }
}

void run_configuration_import_handler::rollback(ores::nats::message msg) {
    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf)
        return;
    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    const auto req = rfl::json::read<run_configuration_import_rollback_request>(sv);
    if (!req) {
        wf->fail("Failed to decode run_configuration_import_rollback_request");
        return;
    }
    try {
        auto owners = outbound_nats_.with_delegation(req->bearer_token)
                          .with_correlation_id(req->correlation_id);
        undo_import(
            owners, req->report_definition_id, req->run_document_saved, req->saved_documents);
        wf->complete("{}");
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error)
            << "run configuration rollback failed | corr=" << req->correlation_id << " "
            << e.what();
        wf->fail(e.what());
    }
}

}
