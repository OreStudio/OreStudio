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
#ifndef ORES_DQ_CORE_MESSAGING_DATASET_HANDLER_HPP
#define ORES_DQ_CORE_MESSAGING_DATASET_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/messaging/dataset_protocol.hpp"
#include "ores.dq.api/workflow/bundle_publish_workflow.hpp"
#include "ores.dq.core/service/dataset_service.hpp"
#include "ores.dq.core/service/publication_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/correlation.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.workflow.api/messaging/workflow_events.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <format>
#include <optional>
#include <rfl/json.hpp>
#include <span>
#include <stdexcept>
#include <vector>

namespace ores::dq::messaging {

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::stamp;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

namespace {
inline auto& dataset_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.dq.messaging.dataset_handler");
    return instance;
}
} // namespace

class dataset_handler {
public:
    dataset_handler(ores::nats::service::client& nats,
                    ores::database::context ctx,
                    std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(dataset_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_datasets_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(dataset_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::dataset_service svc(ctx);
        try {
            const auto items = svc.list_datasets(static_cast<std::uint32_t>(req->offset),
                                                 static_cast<std::uint32_t>(req->limit));
            const auto count = svc.get_dataset_count();
            get_datasets_response resp;
            resp.datasets = items;
            resp.total_available_count = static_cast<int>(count);
            BOOST_LOG_SEV(dataset_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(dataset_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_datasets_response resp;
            resp.total_available_count = 0;
            reply(nats_, msg, resp);
        }
    }

    void save(ores::nats::message msg) {
        BOOST_LOG_SEV(dataset_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<save_dataset_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(dataset_handler_lg(), warn) << "Failed to decode: " << msg.subject;
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
        service::dataset_service svc(ctx);
        try {
            for (auto& ds : req->datasets)
                stamp(ds, ctx);
            svc.save_datasets(req->datasets);
            BOOST_LOG_SEV(dataset_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, save_dataset_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(dataset_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, save_dataset_response{false, e.what()});
        }
    }

    void remove(ores::nats::message msg) {
        BOOST_LOG_SEV(dataset_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<delete_dataset_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(dataset_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "dq::datasets:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::dataset_service svc(ctx);
        try {
            boost::uuids::string_generator gen;
            for (const auto& id : req->ids)
                svc.remove_dataset(gen(id));
            BOOST_LOG_SEV(dataset_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, delete_dataset_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(dataset_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, delete_dataset_response{false, e.what()});
        }
    }

    void history(ores::nats::message msg) {
        BOOST_LOG_SEV(dataset_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_dataset_history_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(dataset_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::dataset_service svc(ctx);
        try {
            const auto hist = svc.get_dataset_history(boost::uuids::string_generator{}(req->id));
            get_dataset_history_response resp;
            resp.success = true;
            resp.history = hist;
            BOOST_LOG_SEV(dataset_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(dataset_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_dataset_history_response resp;
            resp.success = false;
            resp.message = e.what();
            reply(nats_, msg, resp);
        }
    }

    void publish(ores::nats::message msg) {
        BOOST_LOG_SEV(dataset_handler_lg(), debug) << "Handling " << msg.subject;
        const auto correlation_id = ores::nats::extract_or_generate_correlation_id(msg);

        auto req = decode<publish_datasets_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(dataset_handler_lg(), warn) << "Failed to decode: " << msg.subject;
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

            service::publication_service svc(ctx);
            const auto entries = svc.list_publishable_datasets(uuids, req->resolve_dependencies);

            if (entries.empty()) {
                publish_datasets_response resp;
                resp.success = false;
                resp.message = "No publishable datasets found";
                reply(nats_, msg, resp);
                return;
            }

            const auto tenant_id_str = boost::uuids::to_string(ctx.tenant_id().to_uuid());
            const std::string mode_str = to_string(req->mode);
            const std::string params_json = ctx.party_id()
                ? std::format(R"({{"party_id":"{}"}})", boost::uuids::to_string(*ctx.party_id()))
                : "{}";

            ores::dq::workflow::bundle_publish_workflow_request wf_req;
            wf_req.tenant_id = tenant_id_str;
            wf_req.published_by = req->published_by;
            for (const auto& entry : entries) {
                ores::dq::workflow::bundle_publish_workflow_dataset ds;
                ds.dataset_id = entry.dataset_id;
                ds.dataset_code = entry.dataset_code;
                ds.target_subject = entry.target_subject;
                ds.mode = mode_str;
                ds.params_json = params_json;
                wf_req.datasets.push_back(std::move(ds));
            }

            boost::uuids::random_generator rng;
            const auto instance_id = boost::uuids::to_string(rng());

            ores::workflow::messaging::start_workflow_message start_msg;
            start_msg.type = "bundle_publish_workflow";
            start_msg.tenant_id = tenant_id_str;
            start_msg.request_json = rfl::json::write(wf_req);
            start_msg.correlation_id = correlation_id;
            start_msg.instance_id = instance_id;

            const auto json = rfl::json::write(start_msg);
            const auto data = std::as_bytes(std::span{json.data(), json.size()});
            nats_.js_publish(ores::workflow::messaging::start_workflow_message::nats_subject, data);

            publish_datasets_response resp;
            resp.success = true;
            resp.instance_id = instance_id;
            resp.datasets_dispatched = static_cast<int>(entries.size());
            BOOST_LOG_SEV(dataset_handler_lg(), info)
                << "Datasets publish workflow started: instance=" << instance_id
                << " datasets=" << entries.size();
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(dataset_handler_lg(), error) << msg.subject << " failed: " << e.what();
            publish_datasets_response resp;
            resp.success = false;
            resp.message = e.what();
            reply(nats_, msg, resp);
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::dq::messaging

#endif
