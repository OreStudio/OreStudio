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
#ifndef ORES_COMPUTE_CORE_MESSAGING_CLAIM_HANDLER_HPP
#define ORES_COMPUTE_CORE_MESSAGING_CLAIM_HANDLER_HPP

#include "ores.compute.api/messaging/work_protocol.hpp"
#include "ores.compute.api/net/compute_storage.hpp"
#include "ores.compute.core/export.hpp"
#include "ores.compute.core/messaging/assignment_grants.hpp"
#include "ores.compute.core/messaging/submit_decision.hpp"
#include "ores.compute.core/repository/app_version_platform_repository.hpp"
#include "ores.compute.core/repository/work_ledger_repository.hpp"
#include "ores.compute.core/service/result_service.hpp"
#include "ores.compute.core/service/workunit_service.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.dq.api/domain/change_reason_codes.hpp"
#include "ores.iam.client/client/storage_capability_minter.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>

namespace ores::compute::messaging {

namespace {
inline auto& claim_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.compute.messaging.claim_handler");
    return instance;
}
} // namespace

/**
 * @brief Serves a free node its next job from the work ledger.
 *
 * The queue of the grid is the ledger, which the installation owns. A claim
 * takes the next waiting entry as the service's own session, which is the system
 * tenant, then enters the tenant that owns the job to mark its result in
 * progress. It reads no tenant's rows across tenants and relies on no policy that
 * does. The storage token is minted here, for the one job, so it lives from the
 * claim and not from the moment the job was queued.
 *
 * Like the heartbeat and the submit, the channel is trusted at the transport
 * layer, because a node holds no user JWT.
 */
class ORES_COMPUTE_CORE_EXPORT claim_handler {
public:
    /// The time a claim holds a job before the lease must be extended or the job goes back.
    static constexpr std::chrono::seconds lease{300};

    claim_handler(ores::nats::service::client& nats,
                  ores::database::context ctx,
                  ores::nats::service::nats_client service_nats)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , service_nats_(std::move(service_nats))
        , minter_(ores::iam::client::make_storage_capability_minter(service_nats_)) {}

    void claim(ores::nats::message msg) {
        BOOST_LOG_SEV(claim_handler_lg(), ores::logging::debug) << "Handling " << msg.subject;
        claim_work_response resp;
        if (auto req = ores::service::messaging::decode<claim_work_request>(msg)) {
            try {
                resp = serve(*req);
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(claim_handler_lg(), ores::logging::warn)
                    << "claim_work failed: " << e.what();
                resp = claim_work_response{};
                resp.message = e.what();
            }
        } else {
            resp.message = "Failed to decode request";
        }
        ores::service::messaging::reply(nats_, msg, resp);
    }

private:
    /// How many entries one claim may drop as no longer runnable before it answers with no work.
    static constexpr int max_dropped = 50;

    claim_work_response serve(const claim_work_request& req) {
        boost::uuids::uuid host;
        try {
            host = boost::lexical_cast<boost::uuids::uuid>(req.host_id);
        } catch (const boost::bad_lexical_cast&) {
            throw std::invalid_argument("The host id is not a UUID: " + req.host_id);
        }
        repository::work_ledger_repository ledger;
        // A host runs one job at a time, so a host that asks for work is free. An entry it
        // still holds was claimed and never reported, for example when the reply to that
        // claim was lost, and nobody is working on it. Return it to the queue now: its
        // heartbeats would otherwise keep it leased for as long as the host lives.
        if (const auto abandoned = ledger.requeue_held(ctx_, host); abandoned > 0) {
            BOOST_LOG_SEV(claim_handler_lg(), ores::logging::warn)
                << abandoned << " job(s) held by host " << req.host_id
                << " were never reported and went back to the queue";
        }
        for (int dropped = 0; dropped <= max_dropped; ++dropped) {
            const auto entry = ledger.claim(ctx_, host, req.platform, lease);
            if (!entry) {
                claim_work_response none;
                none.success = true;
                return none;
            }
            std::optional<claim_work_response> assignment;
            try {
                assignment = complete(ledger, *entry, host, req.platform);
            } catch (...) {
                // The entry was taken and the claim failed for a reason not handled below, such as
                // a database error. Give the entry back, so it is not left claimed with no node
                // working on it, and let the failure end this claim.
                try {
                    ledger.release(ctx_, entry->result_id);
                } catch (const std::exception& e) {
                    BOOST_LOG_SEV(claim_handler_lg(), ores::logging::error)
                        << "Could not release result " << boost::uuids::to_string(entry->result_id)
                        << " after a failed claim: " << e.what();
                }
                throw;
            }
            if (assignment)
                return *assignment;
            // The entry was dropped because its result can no longer run. Take the next one.
        }
        claim_work_response none;
        none.success = true;
        none.message = "Many entries were no longer runnable; ask again.";
        return none;
    }

    /**
     * @brief Finish a claim: enter the tenant, mint the token and mark the result.
     *
     * @return The assignment, or nothing when the entry was dropped because its
     * result can no longer run.
     */
    std::optional<claim_work_response> complete(repository::work_ledger_repository& ledger,
                                                const repository::claimed_entry& entry,
                                                const boost::uuids::uuid& host,
                                                const std::string& platform) {
        const auto tenant = boost::uuids::to_string(entry.work_tenant_id);
        const auto tenant_ctx = ores::database::service::tenant_context::with_tenant(ctx_, tenant);

        service::result_service result_svc(tenant_ctx);
        auto result = result_svc.get_result(entry.result_id);
        // Unsent, or in progress when an earlier claim of the entry lost its lease: the reaper
        // returns the entry to the queue and leaves the result as that node marked it.
        if (!result || !claimable(result->server_state, entry.attempts)) {
            // Retired, done or gone while it waited. Nothing can run it.
            BOOST_LOG_SEV(claim_handler_lg(), ores::logging::info)
                << "Dropping ledger entry for result " << boost::uuids::to_string(entry.result_id)
                << ": it is no longer unsent";
            ledger.remove(ctx_, entry.result_id);
            return std::nullopt;
        }

        service::workunit_service wu_svc(tenant_ctx);
        const auto workunit = wu_svc.get_workunit(entry.workunit_id);
        if (!workunit) {
            BOOST_LOG_SEV(claim_handler_lg(), ores::logging::warn)
                << "Dropping ledger entry for result " << boost::uuids::to_string(entry.result_id)
                << ": its workunit is gone";
            ledger.remove(ctx_, entry.result_id);
            return std::nullopt;
        }

        repository::app_version_platform_repository avp_repo(tenant_ctx);
        std::optional<domain::app_version_platform> package;
        for (auto& candidate : avp_repo.read_latest_by_app_version(workunit->app_version_id)) {
            if (candidate.platform_code == platform) {
                package = std::move(candidate);
                break;
            }
        }
        const auto result_id = boost::uuids::to_string(entry.result_id);
        const auto output_path = ores::compute::net::compute_storage::output_path(result_id);
        const auto results_path = ores::compute::net::compute_storage::results_path(result_id);
        const auto grants =
            package ?
                storage_grants_for(
                    package->package_uri, workunit->input_uri, output_path, results_path) :
                std::nullopt;
        if (!grants) {
            // No node of this platform can ever run it. End it as a failure that a person
            // sees, and take it out of the queue, so it cannot hold the head of the line.
            BOOST_LOG_SEV(claim_handler_lg(), ores::logging::error)
                << "Result " << result_id << " cannot be run on platform " << platform
                << ": no package or no storage path";
            auto ended = *result;
            ended.server_state = 5;
            ended.outcome = 3;
            ended.host_id = host;
            ended.received_at = std::chrono::system_clock::now();
            ended.error_message =
                "The job has no package for the platform of the node that took it.";
            ended.change_reason_code = ores::dq::domain::change_reasons::system_new_record;
            ended.change_commentary = ended.error_message;
            ores::service::messaging::stamp(ended, tenant_ctx);
            result_svc.save_result(ended);
            ledger.remove(ctx_, entry.result_id);
            return std::nullopt;
        }

        const auto token = minter_(tenant, *grants);
        if (!token) {
            // IAM refused or did not answer. The entry goes back and the claim stops, because
            // the next entry would meet the same refusal.
            ledger.release(ctx_, entry.result_id);
            throw std::runtime_error("No storage capability could be minted for result " +
                                     result_id);
        }

        try {
            auto taken = *result;
            // InProgress.
            taken.server_state = 4;
            taken.host_id = host;
            taken.started_at = std::chrono::system_clock::now();
            taken.change_reason_code = ores::dq::domain::change_reasons::system_new_record;
            taken.change_commentary = "Claimed by a node";
            ores::service::messaging::stamp(taken, tenant_ctx);
            result_svc.save_result(taken);
        } catch (const std::exception& e) {
            // The result could not be marked. Either it changed since it was read, because it was
            // retired or finished, or the database failed. The entry goes back and the claim
            // stops. If the result was retired, the next claim finds it no longer unsent and
            // drops the entry. If the database failed, the next claim tries again. Deleting the
            // entry here would lose a job that the database failure left unsent.
            BOOST_LOG_SEV(claim_handler_lg(), ores::logging::warn)
                << "Giving back ledger entry for result " << result_id
                << ": the result could not be marked in progress (" << e.what() << ")";
            ledger.release(ctx_, entry.result_id);
            throw;
        }

        claim_work_response assignment;
        assignment.success = true;
        assignment.has_work = true;
        assignment.tenant_id = tenant;
        assignment.result_id = result_id;
        assignment.workunit_id = boost::uuids::to_string(entry.workunit_id);
        assignment.app_version_id = boost::uuids::to_string(workunit->app_version_id);
        assignment.package_uri = package->package_uri;
        assignment.package_sha256 = package->sha256;
        assignment.input_uri = workunit->input_uri;
        assignment.config_uri = workunit->config_uri;
        assignment.output_uri = output_path;
        assignment.results_uri = results_path;
        assignment.storage_token = *token;
        assignment.attempt = entry.attempts;
        BOOST_LOG_SEV(claim_handler_lg(), ores::logging::info)
            << "Claimed result " << result_id << " for host " << boost::uuids::to_string(host)
            << " (attempt " << entry.attempts << ")";
        return assignment;
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    ores::nats::service::nats_client service_nats_;
    ores::iam::client::storage_capability_minter minter_;
};

} // namespace ores::compute::messaging

#endif
