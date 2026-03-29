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
#ifndef ORES_ASSETS_MESSAGING_IMAGE_HANDLER_HPP
#define ORES_ASSETS_MESSAGING_IMAGE_HANDLER_HPP

#include <optional>
#include <vector>
#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.assets.api/messaging/assets_protocol.hpp"
#include "ores.assets.core/repository/image_repository.hpp"
#include "ores.assets.core/service/assets_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"

namespace ores::assets::messaging {

namespace {
inline auto& image_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.assets.messaging.image_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::stamp;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

class image_handler {
public:
    image_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void get(ores::nats::message msg) {
        BOOST_LOG_SEV(image_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_images_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(image_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        try {
            service::assets_service svc(ctx);
            const auto images = svc.get_images(req->image_ids);
            BOOST_LOG_SEV(image_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, get_images_response{true, {}, images});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(image_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_images_response{false, e.what()});
        }
    }

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(image_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<list_images_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(image_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        try {
            repository::image_repository repo;
            std::vector<domain::image> images;
            if (req->modified_since.has_value())
                images = repo.read_latest_since(ctx, *req->modified_since);
            else
                images = repo.read_latest(ctx);
            list_images_response resp;
            resp.success = true;
            resp.images.reserve(images.size());
            for (const auto& img : images) {
                image_info info;
                info.image_id = boost::uuids::to_string(img.image_id);
                info.key = img.key;
                info.description = img.description;
                resp.images.push_back(std::move(info));
            }
            BOOST_LOG_SEV(image_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(image_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, list_images_response{false, e.what()});
        }
    }

    void save(ores::nats::message msg) {
        BOOST_LOG_SEV(image_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<save_image_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(image_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "assets::images:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        try {
            repository::image_repository repo;
            stamp(req->data, ctx);
            repo.write(ctx, req->data);
            BOOST_LOG_SEV(image_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, save_image_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(image_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, save_image_response{false, e.what()});
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::assets::messaging

#endif
