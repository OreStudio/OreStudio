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
#ifndef ORES_DQ_CORE_MESSAGING_DATA_ORGANIZATION_HANDLER_HPP
#define ORES_DQ_CORE_MESSAGING_DATA_ORGANIZATION_HANDLER_HPP

#include <optional>
#include <stdexcept>
#include <boost/uuid/string_generator.hpp>
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.dq.api/messaging/data_organization_protocol.hpp"
#include "ores.dq.core/service/data_organization_service.hpp"
#include "ores.dq.core/service/dataset_service.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::dq::messaging {

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::stamp;
using ores::service::messaging::error_reply;
using namespace ores::logging;

namespace {
inline auto& data_organization_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.dq.messaging.data_organization_handler");
    return instance;
}
} // namespace

class data_organization_handler {
public:
    data_organization_handler(
        ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    // =========================================================================
    // Catalogs
    // =========================================================================

    void list_catalogs(ores::nats::message msg) {
        BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_catalogs_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(data_organization_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::data_organization_service svc(ctx);
        try {
            const auto items = svc.list_catalogs(
                static_cast<std::uint32_t>(req->offset),
                static_cast<std::uint32_t>(req->limit));
            const auto count = svc.get_catalog_count();
            get_catalogs_response resp;
            resp.catalogs = items;
            resp.total_available_count = static_cast<int>(count);
            BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(data_organization_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_catalogs_response resp;
            resp.total_available_count = 0;
            reply(nats_, msg, resp);
        }
    }

    void save_catalog(ores::nats::message msg) {
        BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<save_catalog_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(data_organization_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::data_organization_service svc(ctx);
        try {
            stamp(req->data, ctx);
            svc.save_catalog(req->data);
            BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, save_catalog_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(data_organization_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, save_catalog_response{false, e.what()});
        }
    }

    void delete_catalogs(ores::nats::message msg) {
        BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<delete_catalog_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(data_organization_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::data_organization_service svc(ctx);
        try {
            for (const auto& code : req->codes)
                svc.remove_catalog(code);
            BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, delete_catalog_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(data_organization_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, delete_catalog_response{false, e.what()});
        }
    }

    void catalog_history(ores::nats::message msg) {
        BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_catalog_history_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(data_organization_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::data_organization_service svc(ctx);
        try {
            const auto history = svc.get_catalog_history(req->code);
            get_catalog_history_response resp;
            resp.success = true;
            resp.history = history;
            BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(data_organization_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_catalog_history_response resp;
            resp.success = false;
            resp.message = e.what();
            reply(nats_, msg, resp);
        }
    }

    // =========================================================================
    // Data Domains
    // =========================================================================

    void list_data_domains(ores::nats::message msg) {
        BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_data_domains_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(data_organization_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::data_organization_service svc(ctx);
        try {
            const auto items = svc.list_data_domains();
            get_data_domains_response resp;
            resp.domains = items;
            resp.total_available_count = static_cast<int>(items.size());
            BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(data_organization_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_data_domains_response resp;
            resp.total_available_count = 0;
            reply(nats_, msg, resp);
        }
    }

    void save_data_domain(ores::nats::message msg) {
        BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<save_data_domain_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(data_organization_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::data_organization_service svc(ctx);
        try {
            stamp(req->data, ctx);
            svc.save_data_domain(req->data);
            BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, save_data_domain_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(data_organization_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, save_data_domain_response{false, e.what()});
        }
    }

    void delete_data_domains(ores::nats::message msg) {
        BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<delete_data_domain_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(data_organization_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::data_organization_service svc(ctx);
        try {
            for (const auto& name : req->names)
                svc.remove_data_domain(name);
            BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, delete_data_domain_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(data_organization_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, delete_data_domain_response{false, e.what()});
        }
    }

    void data_domain_history(ores::nats::message msg) {
        BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_data_domain_history_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(data_organization_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::data_organization_service svc(ctx);
        try {
            const auto history = svc.get_data_domain_history(req->name);
            get_data_domain_history_response resp;
            resp.success = true;
            resp.history = history;
            BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(data_organization_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_data_domain_history_response resp;
            resp.success = false;
            resp.message = e.what();
            reply(nats_, msg, resp);
        }
    }

    // =========================================================================
    // Methodologies
    // =========================================================================

    void list_methodologies(ores::nats::message msg) {
        BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_methodologies_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(data_organization_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::dataset_service svc(ctx);
        try {
            const auto items = svc.list_methodologies(
                static_cast<std::uint32_t>(req->offset),
                static_cast<std::uint32_t>(req->limit));
            const auto count = svc.get_methodology_count();
            get_methodologies_response resp;
            resp.methodologies = items;
            resp.total_available_count = static_cast<int>(count);
            BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(data_organization_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_methodologies_response resp;
            resp.total_available_count = 0;
            reply(nats_, msg, resp);
        }
    }

    void save_methodology(ores::nats::message msg) {
        BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<save_methodology_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(data_organization_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::dataset_service svc(ctx);
        try {
            stamp(req->data, ctx);
            svc.save_methodology(req->data);
            BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, save_methodology_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(data_organization_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, save_methodology_response{false, e.what()});
        }
    }

    void delete_methodologies(ores::nats::message msg) {
        BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<delete_methodology_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(data_organization_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::dataset_service svc(ctx);
        try {
            for (const auto& code : req->codes)
                svc.remove_methodology(boost::uuids::string_generator{}(code));
            BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, delete_methodology_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(data_organization_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, delete_methodology_response{false, e.what()});
        }
    }

    void methodology_history(ores::nats::message msg) {
        BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_methodology_history_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(data_organization_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::dataset_service svc(ctx);
        try {
            const auto history = svc.get_methodology_history(
                boost::uuids::string_generator{}(req->code));
            get_methodology_history_response resp;
            resp.success = true;
            resp.history = history;
            BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(data_organization_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_methodology_history_response resp;
            resp.success = false;
            resp.message = e.what();
            reply(nats_, msg, resp);
        }
    }

    // =========================================================================
    // Subject Areas
    // =========================================================================

    void list_subject_areas(ores::nats::message msg) {
        BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_subject_areas_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(data_organization_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::data_organization_service svc(ctx);
        try {
            const auto items = svc.list_subject_areas(
                static_cast<std::uint32_t>(req->offset),
                static_cast<std::uint32_t>(req->limit));
            const auto count = svc.get_subject_area_count();
            get_subject_areas_response resp;
            resp.subject_areas = items;
            resp.total_available_count = static_cast<int>(count);
            BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(data_organization_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_subject_areas_response resp;
            resp.total_available_count = 0;
            reply(nats_, msg, resp);
        }
    }

    void save_subject_area(ores::nats::message msg) {
        BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<save_subject_area_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(data_organization_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::data_organization_service svc(ctx);
        try {
            stamp(req->data, ctx);
            svc.save_subject_area(req->data);
            BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, save_subject_area_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(data_organization_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, save_subject_area_response{false, e.what()});
        }
    }

    void delete_subject_areas(ores::nats::message msg) {
        BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<delete_subject_area_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(data_organization_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::data_organization_service svc(ctx);
        try {
            for (const auto& key : req->keys)
                svc.remove_subject_area(key.name, key.domain_name);
            BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, delete_subject_area_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(data_organization_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, delete_subject_area_response{false, e.what()});
        }
    }

    void subject_area_history(ores::nats::message msg) {
        BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_subject_area_history_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(data_organization_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::data_organization_service svc(ctx);
        try {
            const auto history = svc.get_subject_area_history(
                req->key.name, req->key.domain_name);
            get_subject_area_history_response resp;
            resp.success = true;
            resp.history = history;
            BOOST_LOG_SEV(data_organization_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(data_organization_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_subject_area_history_response resp;
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
