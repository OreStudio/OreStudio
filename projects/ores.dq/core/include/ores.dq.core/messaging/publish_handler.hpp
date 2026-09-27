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
#ifndef ORES_DQ_CORE_MESSAGING_PUBLISH_HANDLER_HPP
#define ORES_DQ_CORE_MESSAGING_PUBLISH_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/messaging/publish_bundle_protocol.hpp"
#include "ores.dq.api/messaging/publish_datasets_protocol.hpp"
#include "ores.dq.api/messaging/publish_params.hpp"
#include "ores.dq.api/workflow/bundle_publish_workflow.hpp"
#include "ores.dq.core/service/publish_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/correlation.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.workflow.api/messaging/workflow_events.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <optional>
#include <rfl/json.hpp>
#include <stdexcept>
#include <string>
#include <vector>

namespace ores::dq::messaging {

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

namespace {
inline auto& publish_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.dq.messaging.publish_handler");
    return instance;
}
} // namespace

/**
 * @brief The two publication verbs: a named set of datasets, and a bundle.
 *
 * Both verbs resolve their datasets to the same publishable entries and then
 * dispatch the same bundle-publish workflow, so they share one class and one
 * dispatch helper. Neither is a CRUD verb over an entity, so neither can be
 * derived from an entity model; the handler stays hand-written beside the
 * =publish_bundle= and =publish_datasets= operation models that declare its
 * messages.
 */
class publish_handler {
public:
    publish_handler(ores::nats::service::client& nats,
                    ores::database::context ctx,
                    std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void publish_datasets(ores::nats::message msg) {
        BOOST_LOG_SEV(publish_handler_lg(), debug) << "Handling " << msg.subject;
        const auto correlation_id = ores::nats::extract_or_generate_correlation_id(msg);

        auto req = decode<publish_datasets_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(publish_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "dq::datasets:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        try {
            boost::uuids::string_generator str_gen;
            std::vector<boost::uuids::uuid> uuids;
            uuids.reserve(req->dataset_ids.size());
            for (const auto& id : req->dataset_ids)
                uuids.push_back(str_gen(id));

            service::publish_service svc(ctx);
            const auto entries = svc.list_publishable_datasets(uuids, req->resolve_dependencies);

            if (entries.empty()) {
                publish_datasets_response resp;
                resp.success = false;
                resp.message = "No publishable datasets found";
                reply(nats_, msg, resp);
                return;
            }

            dataset_publish_params params;
            if (ctx.party_id())
                params.party_id = boost::uuids::to_string(*ctx.party_id());

            const auto instance_id = start_publish_workflow(entries,
                                                            std::string{},
                                                            req->published_by,
                                                            req->mode,
                                                            build_params_json(params),
                                                            correlation_id,
                                                            ctx);

            publish_datasets_response resp;
            resp.success = true;
            resp.instance_id = instance_id;
            resp.datasets_dispatched = static_cast<int>(entries.size());
            BOOST_LOG_SEV(publish_handler_lg(), info)
                << "Datasets publish workflow started: instance=" << instance_id
                << " datasets=" << entries.size();
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(publish_handler_lg(), error) << msg.subject << " failed: " << e.what();
            publish_datasets_response resp;
            resp.success = false;
            resp.message = e.what();
            reply(nats_, msg, resp);
        }
    }

    void publish_bundle(ores::nats::message msg) {
        BOOST_LOG_SEV(publish_handler_lg(), debug) << "Handling " << msg.subject;
        const auto correlation_id = ores::nats::extract_or_generate_correlation_id(msg);

        auto req = decode<publish_bundle_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(publish_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "dq::datasets:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        try {
            service::publish_service svc(ctx);
            auto entries = svc.list_bundle_publishable_datasets(req->bundle_code);

            // A bundle's optional members never publish by default -- only its
            // always-included (non-optional) members do. opted_in_datasets names
            // which optional members to additively pull in on top of those; it is
            // not a whitelist over the whole bundle, so a caller opting in to one
            // large GLEIF dataset still gets every non-optional member (badges,
            // fpml.* codes, countries, calendars, ...) alongside it.
            std::vector<std::string> allowed;
            if (!req->params_json.empty()) {
                auto parsed = rfl::json::read<publish_bundle_params>(req->params_json);
                if (parsed)
                    allowed = parsed->opted_in_datasets;
            }
            entries.erase(std::remove_if(entries.begin(),
                                         entries.end(),
                                         [&](const auto& e) {
                                             return e.optional &&
                                                    std::find(allowed.begin(),
                                                              allowed.end(),
                                                              e.dataset_code) == allowed.end();
                                         }),
                          entries.end());
            BOOST_LOG_SEV(publish_handler_lg(), debug)
                << "optional-member filter applied: " << entries.size() << " datasets retained";

            if (entries.empty()) {
                publish_bundle_response resp;
                resp.success = false;
                resp.error_message = "No publishable datasets in bundle: " + req->bundle_code;
                reply(nats_, msg, resp);
                return;
            }

            const std::string params = req->params_json.empty() ? "{}" : req->params_json;
            const auto instance_id = start_publish_workflow(entries,
                                                            req->bundle_code,
                                                            req->published_by,
                                                            req->mode,
                                                            params,
                                                            correlation_id,
                                                            ctx);

            publish_bundle_response resp;
            resp.success = true;
            resp.instance_id = instance_id;
            resp.datasets_dispatched = static_cast<int>(entries.size());
            BOOST_LOG_SEV(publish_handler_lg(), info)
                << "Bundle publish workflow started: bundle=" << req->bundle_code
                << " instance=" << instance_id << " datasets=" << entries.size();
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(publish_handler_lg(), error) << msg.subject << " failed: " << e.what();
            publish_bundle_response resp;
            resp.success = false;
            resp.error_message = e.what();
            reply(nats_, msg, resp);
        }
    }

private:
    /**
     * @brief Starts the bundle-publish workflow for resolved datasets.
     *
     * The workflow is shared by both verbs: each resolves its own dataset set,
     * and both dispatch the same per-dataset publish_from_dq_command. A direct
     * publication leaves =bundle_code= empty, because it names no bundle.
     *
     * @return The workflow instance id the caller reports back.
     */
    std::string
    start_publish_workflow(const std::vector<service::bundle_publishable_dataset>& entries,
                           const std::string& bundle_code,
                           const std::string& published_by,
                           const std::string& mode,
                           const std::string& params_json,
                           const std::string& correlation_id,
                           const ores::database::context& ctx) {
        const auto tenant_id = boost::uuids::to_string(ctx.tenant_id().to_uuid());

        ores::dq::workflow::bundle_publish_workflow_request wf_req;
        wf_req.bundle_code = bundle_code;
        wf_req.tenant_id = tenant_id;
        wf_req.published_by = published_by;
        for (const auto& entry : entries) {
            ores::dq::workflow::bundle_publish_workflow_dataset ds;
            ds.dataset_id = entry.dataset_id;
            ds.dataset_code = entry.dataset_code;
            ds.target_subject = entry.target_subject;
            ds.mode = mode;
            ds.params_json = params_json;
            wf_req.datasets.push_back(std::move(ds));
        }

        boost::uuids::random_generator rng;
        const auto instance_id = boost::uuids::to_string(rng());

        ores::workflow::messaging::start_workflow_message start_msg;
        start_msg.type = "bundle_publish_workflow";
        start_msg.tenant_id = tenant_id;
        start_msg.request_json = rfl::json::write(wf_req);
        start_msg.correlation_id = correlation_id;
        start_msg.instance_id = instance_id;

        nats_.js_publish(ores::workflow::messaging::start_workflow_message::nats_subject,
                         ores::nats::default_wire_codec().encode(start_msg));
        return instance_id;
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::dq::messaging

#endif
