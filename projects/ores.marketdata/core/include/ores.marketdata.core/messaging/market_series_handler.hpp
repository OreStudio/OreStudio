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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_nats_handler.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_MARKETDATA_CORE_MESSAGING_MARKET_SERIES_HANDLER_HPP
#define ORES_MARKETDATA_CORE_MESSAGING_MARKET_SERIES_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/market_series_protocol.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/repository/market_series_identity_projector.hpp"
#include "ores.marketdata.core/repository/market_series_identity_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/service/market_series_service.hpp"
#include "ores.marketdata.core/service/ore_export_service.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.storage.core/net/storage_transfer.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <optional>
#include <set>
#include <string>
#include <vector>

namespace ores::marketdata::messaging {

namespace {
inline auto& market_series_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.messaging.market_series_handler");
    return instance;
}
}

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

/**
 * @brief NATS message handler for market series operations.
 */
class market_series_handler {
public:
    market_series_handler(ores::nats::service::client& nats,
                          ores::database::context ctx,
                          std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    /**
     * @brief Serves marketdata.v1.market_series.list.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void list_market_series(ores::nats::message msg) {
        BOOST_LOG_SEV(market_series_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "marketdata::market_series:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<list_market_series_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(market_series_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::market_series_service svc(req_ctx);
        try {
            auto response = svc.list_market_series(*req);
            BOOST_LOG_SEV(market_series_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(market_series_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            list_market_series_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves marketdata.v1.market_series.get.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_market_series(ores::nats::message msg) {
        BOOST_LOG_SEV(market_series_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "marketdata::market_series:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_market_series_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(market_series_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::market_series_service svc(req_ctx);
        try {
            auto response = svc.get_market_series(*req);
            BOOST_LOG_SEV(market_series_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(market_series_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_market_series_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves marketdata.v1.market_series.get_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_many_market_series(ores::nats::message msg) {
        BOOST_LOG_SEV(market_series_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "marketdata::market_series:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_many_market_series_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(market_series_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::market_series_service svc(req_ctx);
        try {
            auto response = svc.get_many_market_series(*req);
            BOOST_LOG_SEV(market_series_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(market_series_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_many_market_series_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves marketdata.v1.market_series.put.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void put_market_series(ores::nats::message msg) {
        BOOST_LOG_SEV(market_series_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "marketdata::market_series:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<put_market_series_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(market_series_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::market_series_service svc(req_ctx);
        try {
            auto response = svc.put_market_series(*req);
            BOOST_LOG_SEV(market_series_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(market_series_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            put_market_series_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves marketdata.v1.market_series.put_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void put_many_market_series(ores::nats::message msg) {
        BOOST_LOG_SEV(market_series_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "marketdata::market_series:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<put_many_market_series_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(market_series_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::market_series_service svc(req_ctx);
        try {
            auto response = svc.put_many_market_series(*req);
            BOOST_LOG_SEV(market_series_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(market_series_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            put_many_market_series_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves marketdata.v1.market_series.delete.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void delete_market_series(ores::nats::message msg) {
        BOOST_LOG_SEV(market_series_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "marketdata::market_series:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<delete_market_series_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(market_series_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::market_series_service svc(req_ctx);
        try {
            auto response = svc.delete_market_series(*req);
            BOOST_LOG_SEV(market_series_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(market_series_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            delete_market_series_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves marketdata.v1.market_series.delete_many.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void delete_many_market_series(ores::nats::message msg) {
        BOOST_LOG_SEV(market_series_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "marketdata::market_series:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<delete_many_market_series_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(market_series_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::market_series_service svc(req_ctx);
        try {
            auto response = svc.delete_many_market_series(*req);
            BOOST_LOG_SEV(market_series_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(market_series_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            delete_many_market_series_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves marketdata.v1.market_series_versions.list.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void list_market_series_versions(ores::nats::message msg) {
        BOOST_LOG_SEV(market_series_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "marketdata::market_series:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<list_market_series_versions_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(market_series_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::market_series_service svc(req_ctx);
        try {
            auto response = svc.list_market_series_versions(*req);
            BOOST_LOG_SEV(market_series_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(market_series_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            list_market_series_versions_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    /**
     * @brief Serves marketdata.v1.market_series_versions.get.
     *
     * The adapter decides nothing: it proves the request, checks the
     * permission a write needs, decodes the canonical request, calls the
     * service and replies with the response the service filled. The outcome
     * a caller reads -- missing, conflicting, denied -- is the service's
     * answer, so the two cannot disagree about what happened.
     */
    void get_market_series_version(ores::nats::message msg) {
        BOOST_LOG_SEV(market_series_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "marketdata::market_series:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<get_market_series_version_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(market_series_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        service::market_series_service svc(req_ctx);
        try {
            auto response = svc.get_market_series_version(*req);
            BOOST_LOG_SEV(market_series_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, response);
        } catch (const std::exception& e) {
            // The service reports what it decided in the response; an
            // exception here is the store failing, which is a different
            // thing and is reported as such.
            BOOST_LOG_SEV(market_series_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            get_market_series_version_response failure;
            failure.result.outcome = ores::utility::domain::outcome::failed;
            failure.result.code = "internal_error";
            failure.result.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    void backfill_identity(ores::nats::message msg) {
        BOOST_LOG_SEV(market_series_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "marketdata::market_series:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<backfill_series_identity_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(market_series_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        backfill_series_identity_response resp;
        try {
            auto series = repository::market_series_repository{}.read_latest(req_ctx);
            if (!req->party_id.empty())
                std::erase_if(series, [&](const ores::marketdata::domain::market_series& s) {
                    return boost::uuids::to_string(s.party_id) != req->party_id;
                });

            // The projection keeps one row per series, so what is already
            // there is what a writer has seen. Only the rest are projected.
            std::set<std::string> projected;
            for (const auto& row :
                 repository::market_series_identity_repository{}.read_latest(req_ctx))
                projected.insert(boost::uuids::to_string(row.series_id));

            std::vector<ores::marketdata::domain::market_series> missing;
            for (const auto& s : series) {
                if (!projected.contains(boost::uuids::to_string(s.id)))
                    missing.push_back(s);
            }
            repository::market_series_identity_projector::project(req_ctx, missing);

            resp.projected_count = static_cast<int>(missing.size());
            resp.already_projected_count = static_cast<int>(series.size() - missing.size());
            resp.success = true;
            resp.message = "Projected " + std::to_string(missing.size()) + " of " +
                           std::to_string(series.size()) + " series.";
            BOOST_LOG_SEV(market_series_handler_lg(), info) << msg.subject << ": " << resp.message;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(market_series_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.message = e.what();
        }
        reply(nats_, msg, resp);
    }

    void export_to_storage(ores::nats::message msg, const std::string& http_base_url) {
        [[maybe_unused]] const auto correlation_id =
            ores::service::messaging::log_handler_entry(market_series_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        export_market_data_to_storage_response resp;
        try {
            auto req = decode<export_market_data_to_storage_request>(msg);
            if (!req) {
                resp.message = "Failed to decode request.";
                reply(nats_, msg, resp);
                return;
            }

            service::ore_export_service svc(ctx);
            const auto written = svc.write_all();

            ores::storage::net::storage_transfer transfer(http_base_url,
                                                          ores::nats::service::extract_bearer(msg));
            transfer.upload_blob(req->storage_bucket, req->storage_key, written.market_data);
            transfer.upload_blob(req->storage_bucket, req->fixings_storage_key, written.fixings);

            resp.success = true;
            resp.series_count = written.series_count;
            resp.storage_key = req->storage_key;
            resp.fixings_storage_key = req->fixings_storage_key;
            resp.message = "Exported " + std::to_string(written.series_count) +
                           " series as ORE market data to storage.";

            BOOST_LOG_SEV(market_series_handler_lg(), info)
                << "export_to_storage: wrote " << written.market_data.size()
                << " bytes of ORE market data for " << written.series_count << " series ("
                << written.observation_count << " observations, " << written.fixing_count
                << " fixings) to " << req->storage_bucket << "/" << req->storage_key;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(market_series_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.message = e.what();
        }
        reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
