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
#include "ores.compute.service/app/workunit_dispatcher.hpp"
#include "ores.compute.api/domain/result.hpp"
#include "ores.compute.api/messaging/work_protocol.hpp"
#include "ores.compute.api/net/compute_storage.hpp"
#include "ores.compute.core/repository/app_version_platform_repository.hpp"
#include "ores.compute.core/repository/result_repository.hpp"
#include "ores.compute.core/service/result_service.hpp"
#include "ores.compute.core/service/workunit_service.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.dq.api/domain/change_reason_codes.hpp"
#include "ores.iam.client/client/storage_capability_minter.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.security/jwt/jwt_claims.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.storage.api/net/storage_paths.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <optional>
#include <vector>

namespace ores::compute::service::app {

using namespace ores::logging;
using ores::security::jwt::storage_grant;
using ores::service::messaging::stamp;

namespace {

/**
 * @brief The grant that admits one operation on the object a path names.
 *
 * The key is the whole prefix, so the capability admits exactly the object the
 * assignment names and nothing beside it.
 */
std::optional<storage_grant> grant_for_path(const std::string& path, std::string op) {
    std::string bucket;
    std::string key;
    if (!ores::storage::net::storage_paths::split_object_path(path, bucket, key))
        return std::nullopt;
    return storage_grant{.bucket = std::move(bucket), .key_prefix = std::move(key), .op = std::move(op)};
}

}

workunit_dispatcher::workunit_dispatcher(ores::nats::service::client& nats,
                                         ores::database::context ctx,
                                         ores::nats::service::nats_client& service_nats)
    : nats_(nats)
    , ctx_(std::move(ctx))
    , minter_(ores::iam::client::make_storage_capability_minter(service_nats)) {}

void workunit_dispatcher::dispatch(const ores::compute::eventing::workunit_changed_event& evt) {
    if (evt.workunit_ids.empty())
        return;

    try {
        const auto tenant_ctx =
            ores::database::service::tenant_context::with_tenant(ctx_, evt.tenant_id);
        BOOST_LOG_SEV(lg(), debug) << "Dispatching " << evt.workunit_ids.size()
                                   << " workunit(s) for tenant " << evt.tenant_id;
        for (const auto& workunit_id : evt.workunit_ids) {
            try {
                dispatch_one(tenant_ctx, workunit_id);
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), error)
                    << "Dispatch failed for workunit " << workunit_id << ": " << e.what();
            }
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error)
            << "Cannot dispatch workunits for tenant " << evt.tenant_id << ": " << e.what();
    }
}

void workunit_dispatcher::dispatch_one(const ores::database::context& tenant_ctx,
                                       const std::string& workunit_id) {
    ores::compute::service::workunit_service wu_svc(tenant_ctx);
    const auto wu = wu_svc.get_workunit(boost::lexical_cast<boost::uuids::uuid>(workunit_id));
    if (!wu) {
        BOOST_LOG_SEV(lg(), warn) << "Workunit not found for dispatch: " << workunit_id;
        return;
    }

    // Workunit events fire on every row version (insert, update, close).
    // Dispatch converges on the redundancy target: a workunit short of its
    // target tops up the missing assignments on the next event, so a
    // partially failed publish is retried instead of stranding the workunit.
    repository::result_repository result_repo;
    const auto existing = static_cast<int>(
        result_repo.get_total_result_count_by_workunit_id(tenant_ctx, workunit_id));
    const auto missing = wu->target_redundancy - existing;
    if (missing <= 0) {
        BOOST_LOG_SEV(lg(), debug)
            << "Workunit " << workunit_id << " at redundancy target; skipping";
        return;
    }

    if (wu->app_version_id.is_nil()) {
        BOOST_LOG_SEV(lg(), warn) << "Workunit " << workunit_id
                                  << " has no app_version_id; cannot dispatch";
        return;
    }

    repository::app_version_platform_repository avp_repo(tenant_ctx);
    const auto avps = avp_repo.read_latest_by_app_version(wu->app_version_id);
    if (avps.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "No platform packages for app_version "
                                  << boost::uuids::to_string(wu->app_version_id)
                                  << "; cannot dispatch workunit " << workunit_id;
        return;
    }

    const auto app_version_id = boost::uuids::to_string(wu->app_version_id);
    const auto tenant_uuid = tenant_ctx.tenant_id().to_string();
    ores::compute::service::result_service result_svc(tenant_ctx);

    for (int i = 0; i < missing; ++i) {
        const auto& avp = avps[i % avps.size()];
        const auto result_id = boost::uuids::random_generator()();
        ores::compute::domain::result r;
        r.id = result_id;
        r.workunit_id = wu->id;
        // Unsent.
        r.server_state = 2;
        r.change_reason_code = ores::dq::domain::change_reasons::system_new_record;
        r.change_commentary = "Created on workunit dispatch";
        stamp(r, tenant_ctx);
        result_svc.save_result(r);

        const auto result_id_str = boost::uuids::to_string(result_id);
        const auto output_path =
            ores::compute::net::compute_storage::output_path(result_id_str);

        // The node holds no standing storage credential, so the assignment
        // carries one for exactly the package, the input and the output. A
        // grant that cannot be built is a dispatch that cannot run, so it is
        // skipped and the next workunit event tops the workunit up.
        std::vector<storage_grant> grants;
        const struct {
            const std::string& path;
            const char* op;
        } sources[] = {{avp.package_uri, "get"}, {wu->input_uri, "get"}, {output_path, "put"}};
        for (const auto& source : sources) {
            auto grant = grant_for_path(source.path, source.op);
            if (!grant) {
                BOOST_LOG_SEV(lg(), error)
                    << "Cannot build a storage grant for " << source.path
                    << "; cannot dispatch workunit " << workunit_id;
                return;
            }
            grants.push_back(std::move(*grant));
        }
        const auto storage_token = minter_(tenant_uuid, grants);
        if (!storage_token) {
            BOOST_LOG_SEV(lg(), error)
                << "No storage capability for result " << result_id_str
                << "; cannot dispatch workunit " << workunit_id;
            return;
        }

        const auto event = ores::compute::messaging::work_assignment_event{
            .result_id = result_id_str,
            .workunit_id = workunit_id,
            .app_version_id = app_version_id,
            .package_uri = avp.package_uri,
            .package_sha256 = avp.sha256,
            .input_uri = wu->input_uri,
            .config_uri = wu->config_uri,
            .output_uri = output_path,
            .storage_token = *storage_token};
        const std::string subject =
            std::string(ores::compute::messaging::work_assignment_event::nats_subject) + "." +
            tenant_uuid + "." + avp.platform_code;
        nats_.js_publish(subject, ores::nats::default_wire_codec().encode(event));
        BOOST_LOG_SEV(lg(), info) << "Dispatched result " << result_id_str << " for workunit "
                                  << workunit_id << " to platform " << avp.platform_code;
    }
}

}
