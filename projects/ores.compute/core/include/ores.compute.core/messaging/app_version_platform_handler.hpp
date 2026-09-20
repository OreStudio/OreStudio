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
#ifndef ORES_COMPUTE_CORE_MESSAGING_APP_VERSION_PLATFORM_HANDLER_HPP
#define ORES_COMPUTE_CORE_MESSAGING_APP_VERSION_PLATFORM_HANDLER_HPP

#include "ores.compute.api/messaging/app_version_platform_protocol.hpp"
#include "ores.compute.core/service/app_version_platform_service.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cstddef>
#include <optional>

namespace ores::compute::messaging {

namespace {
inline auto& app_version_platform_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.compute.messaging.app_version_platform_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

/**
 * @brief NATS message handler for app version platforms operations.
 */
class app_version_platform_handler {
public:
    app_version_platform_handler(ores::nats::service::client& nats,
                                 ores::database::context ctx,
                                 std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::app_version_platform_service svc(req_ctx);
        if (auto req = decode<get_app_version_platforms_request>(msg)) {
            get_app_version_platforms_response resp;
            try {
                resp.app_version_platforms =
                    svc.list_app_version_platforms(req->offset, req->limit);
                resp.total_available_count =
                    static_cast<int>(svc.get_total_app_version_platform_count());
                resp.success = true;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(app_version_platform_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.success = false;
                resp.message = e.what();
            }
            BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } else {
            BOOST_LOG_SEV(app_version_platform_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void list_by_app_version(ores::nats::message msg) {
        BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::app_version_platform_service svc(req_ctx);
        if (auto req = decode<get_app_version_platforms_by_app_version_request>(msg)) {
            get_app_version_platforms_by_app_version_response resp;
            try {
                auto rows = svc.list_app_version_platforms_by_app_version(
                    boost::lexical_cast<boost::uuids::uuid>(req->app_version_id),
                    req->offset,
                    req->limit);
                resp.total_available_count =
                    static_cast<int>(svc.get_total_app_version_platform_count_by_app_version(
                        boost::lexical_cast<boost::uuids::uuid>(req->app_version_id)));
                resp.app_version_platforms.reserve(rows.size());
                for (auto& row : rows) {
                    app_version_platform_view view;
                    view.app_version_platform = std::move(row);
                    view.platform_code = view.app_version_platform.platform_code;
                    resp.app_version_platforms.push_back(std::move(view));
                }
                resp.success = true;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(app_version_platform_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.success = false;
                resp.message = e.what();
            }
            BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } else {
            BOOST_LOG_SEV(app_version_platform_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void save(ores::nats::message msg) {
        BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "compute::app_version_platforms:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::app_version_platform_service svc(req_ctx);
        if (auto req = decode<save_app_version_platform_request>(msg)) {
            save_app_version_platform_response resp;
            try {
                svc.save_app_version_platforms(req->app_version_platforms);
                resp.success = true;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(app_version_platform_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.success = false;
                resp.message = e.what();
            }
            BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } else {
            BOOST_LOG_SEV(app_version_platform_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void remove(ores::nats::message msg) {
        BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "compute::app_version_platforms:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::app_version_platform_service svc(req_ctx);
        if (auto req = decode<delete_app_version_platform_request>(msg)) {
            if (req->app_version_ids.size() != req->platform_ids.size()) {
                BOOST_LOG_SEV(app_version_platform_handler_lg(), warn)
                    << msg.subject << " rejected: key vectors differ in length, "
                    << req->app_version_ids.size() << " and " << req->platform_ids.size();
                error_reply(nats_, msg, ores::service::error_code::bad_request);
                return;
            }
            delete_app_version_platform_response resp;
            try {
                for (std::size_t i = 0; i < req->app_version_ids.size(); ++i) {
                    const auto app_version_id_key =
                        boost::lexical_cast<boost::uuids::uuid>(req->app_version_ids[i]);
                    const auto platform_id_key =
                        boost::lexical_cast<boost::uuids::uuid>(req->platform_ids[i]);
                    svc.remove_app_version_platform(app_version_id_key, platform_id_key);
                }
                resp.success = true;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(app_version_platform_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.success = false;
                resp.message = e.what();
            }
            BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } else {
            BOOST_LOG_SEV(app_version_platform_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void count_by_app_version(ores::nats::message msg) {
        BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::app_version_platform_service svc(req_ctx);
        if (auto req = decode<count_app_version_platforms_by_app_version_request>(msg)) {
            count_app_version_platforms_by_app_version_response resp;
            try {
                resp.total_available_count =
                    static_cast<int>(svc.get_total_app_version_platform_count_by_app_version(
                        boost::lexical_cast<boost::uuids::uuid>(req->app_version_id)));
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(app_version_platform_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.total_available_count = 0;
            }
            BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } else {
            BOOST_LOG_SEV(app_version_platform_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void count_by_platform(ores::nats::message msg) {
        BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::app_version_platform_service svc(req_ctx);
        if (auto req = decode<count_app_version_platforms_by_platform_request>(msg)) {
            count_app_version_platforms_by_platform_response resp;
            try {
                resp.total_available_count =
                    static_cast<int>(svc.get_total_app_version_platform_count_by_platform(
                        boost::lexical_cast<boost::uuids::uuid>(req->platform_id)));
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(app_version_platform_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.total_available_count = 0;
            }
            BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } else {
            BOOST_LOG_SEV(app_version_platform_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void replace_by_app_version(ores::nats::message msg) {
        BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "compute::app_version_platforms:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::app_version_platform_service svc(req_ctx);
        if (auto req = decode<replace_app_version_platforms_by_app_version_request>(msg)) {
            replace_app_version_platforms_by_app_version_response resp;
            try {
                svc.replace_app_version_platforms_by_app_version(
                    boost::lexical_cast<boost::uuids::uuid>(req->app_version_id),
                    req->app_version_platforms,
                    req->modified_by,
                    req->performed_by,
                    req->change_reason_code,
                    req->change_commentary);
                resp.success = true;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(app_version_platform_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.success = false;
                resp.message = e.what();
            }
            BOOST_LOG_SEV(app_version_platform_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } else {
            BOOST_LOG_SEV(app_version_platform_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::compute::messaging

#endif
