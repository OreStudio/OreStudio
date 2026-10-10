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
#ifndef ORES_IAM_MESSAGING_SESSION_HANDLER_HPP
#define ORES_IAM_MESSAGING_SESSION_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.iam.api/messaging/session_operations_protocol.hpp"
#include "ores.iam.api/messaging/session_samples_protocol.hpp"
#include "ores.iam.api/messaging/session_statistics_operations_protocol.hpp"
#include "ores.iam.core/repository/session_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <boost/lexical_cast.hpp>
#include <chrono>

namespace ores::iam::messaging {

namespace {

inline auto& session_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.iam.messaging.session_operations_handler");
    return instance;
}

} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::log_handler_entry;
using namespace ores::logging;

class session_operations_handler {
public:
    session_operations_handler(ores::nats::service::client& nats,
                               ores::database::context ctx,
                               ores::security::jwt::jwt_authenticator signer)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , signer_(std::move(signer)) {}

    /**
     * @brief Serves iam.v1.ops.get_active_sessions.
     *
     * The rows are the sessions whose end time is empty, which is what makes a
     * session active. The caller must hold the permission a session read needs,
     * as the derived CRUD reads of this resource do.
     */
    void active(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(session_handler_lg(), msg);
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, signer_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "iam::sessions:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        try {
            /*
             * The repository and the service that wraps it are generated, so
             * neither carries a read for this verb and a hand-written method in
             * either would be erased by the next regeneration. The read is
             * therefore the generated listing with the ended sessions dropped.
             * That costs a scan of the tenant's sessions; the fix is a read
             * declared in the model, which is its own task.
             */
            repository::session_repository repo;
            auto rows = repo.read_latest(req_ctx);
            std::vector<domain::session> open;
            for (auto& row : rows) {
                if (row.end_time.empty())
                    open.push_back(std::move(row));
            }
            BOOST_LOG_SEV(session_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_,
                  msg,
                  get_active_sessions_response{.sessions = std::move(open), .success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(session_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_active_sessions_response{.success = false, .message = e.what()});
        }
    }

    void samples(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(session_handler_lg(), msg);
        BOOST_LOG_SEV(session_handler_lg(), debug) << "Completed " << msg.subject;
        reply(nats_, msg, get_session_samples_response{.success = true});
    }

    /**
     * @brief Serves iam.v1.sessions.end.
     *
     * The caller names a session and the service closes it. The tenant comes
     * from the caller's own session, and the sessions table carries row-level
     * security, so another tenant's session is not there to be read, let
     * alone ended. The byte counters are carried through: ending a session
     * records when it stopped, it does not forget what it moved.
     */
    void end(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(session_handler_lg(), msg);
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, signer_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "iam::sessions:end")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto request = decode<end_session_request>(msg);
        if (!request) {
            BOOST_LOG_SEV(session_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        try {
            repository::session_repository repo;
            const auto session = repo.read(req_ctx, request->session_id);
            if (!session) {
                reply(nats_,
                      msg,
                      end_session_response{.success = false,
                                           .message = "No such session in this tenant"});
                return;
            }
            if (!session->end_time.empty()) {
                reply(nats_,
                      msg,
                      end_session_response{.success = false, .message = "Session already ended"});
                return;
            }

            repo.end_session(req_ctx,
                             request->session_id,
                             session->start_time,
                             std::chrono::system_clock::now(),
                             session->bytes_sent,
                             session->bytes_received);

            BOOST_LOG_SEV(session_handler_lg(), info)
                << "Ended session " << boost::lexical_cast<std::string>(request->session_id);
            reply(nats_, msg, end_session_response{.success = true, .message = "Session ended"});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(session_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, end_session_response{.success = false, .message = e.what()});
        }
    }


    /**
     * @brief Serves iam.v1.ops.get_session_statistics.
     *
     * The rows are one per day and account, aggregated from the sessions
     * table so the read answers on either Timescale licence. The tenant
     * comes from the caller's own session.
     */
    void statistics(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(session_handler_lg(), msg);
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, signer_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "iam::sessions:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto request = decode<get_session_statistics_request>(msg);
        if (!request) {
            BOOST_LOG_SEV(session_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        try {
            const auto rows = ores::database::repository::execute_parameterized_multi_column_query(
                req_ctx,
                "select day::text, account_id, session_count, avg_duration_seconds, "
                "total_bytes_sent, total_bytes_received, avg_bytes_sent, avg_bytes_received, "
                "unique_countries "
                "from ores_iam_session_stats_for_tenant_fn("
                "$1::uuid, $2::text, nullif($3, '')::timestamp with time zone, "
                "nullif($4, '')::timestamp with time zone, $5::integer, $6::integer)",
                {req_ctx.tenant_id().to_string(),
                 request->account_id,
                 request->from_time,
                 request->to_time,
                 std::to_string(request->limit),
                 std::to_string(request->offset)},
                session_handler_lg(),
                "Reading session statistics");

            std::vector<session_statistics_row> stats;
            stats.reserve(rows.size());
            for (const auto& row : rows) {
                if (row.size() < 9)
                    continue;
                session_statistics_row r;
                r.day = row[0].value_or("");
                r.account_id = row[1].value_or("");
                r.session_count = row[2] ? std::stoull(*row[2]) : 0;
                r.avg_duration_seconds = row[3] ? std::stod(*row[3]) : 0.0;
                r.total_bytes_sent = row[4] ? std::stoull(*row[4]) : 0;
                r.total_bytes_received = row[5] ? std::stoull(*row[5]) : 0;
                r.avg_bytes_sent = row[6] ? std::stod(*row[6]) : 0.0;
                r.avg_bytes_received = row[7] ? std::stod(*row[7]) : 0.0;
                r.unique_countries = row[8] ? std::stoull(*row[8]) : 0;
                stats.push_back(std::move(r));
            }

            BOOST_LOG_SEV(session_handler_lg(), debug)
                << "Completed " << msg.subject << " with " << stats.size() << " row(s)";
            reply(nats_,
                  msg,
                  get_session_statistics_response{.rows = std::move(stats), .success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(session_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(
                nats_, msg, get_session_statistics_response{.success = false, .message = e.what()});
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    ores::security::jwt::jwt_authenticator signer_;
};

} // namespace ores::iam::messaging
#endif
