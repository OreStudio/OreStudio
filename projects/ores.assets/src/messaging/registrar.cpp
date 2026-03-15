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
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.assets/messaging/assets_protocol.hpp"
#include "ores.assets/repository/image_repository.hpp"
#include "ores.assets/service/assets_service.hpp"
#include "ores.assets/messaging/registrar.hpp"

namespace ores::assets::messaging {

using namespace ores::logging;

namespace {

inline static std::string_view logger_name = "ores.assets.messaging.registrar";
static auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

template<typename Resp>
void reply(ores::nats::service::client& nats,
    const ores::nats::message& msg, const Resp& resp) {
    if (msg.reply_subject.empty()) return;
    const auto json = rfl::json::write(resp);
    const auto* p = reinterpret_cast<const std::byte*>(json.data());
    nats.publish(msg.reply_subject, std::span<const std::byte>(p, json.size()));
}

template<typename Req>
std::optional<Req> decode(const ores::nats::message& msg) {
    const std::string_view sv(
        reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto r = rfl::json::read<Req>(sv);
    if (!r) return std::nullopt;
    return *r;
}

void handle_get_images(ores::nats::service::client& nats,
    const ores::nats::message& msg,
    ores::database::context ctx) {
    auto req = decode<get_images_request>(msg);
    if (!req) {
        reply(nats, msg, get_images_response{false, "Failed to decode request"});
        return;
    }
    try {
        service::assets_service svc(std::move(ctx));
        const auto images = svc.get_images(req->image_ids);
        reply(nats, msg, get_images_response{true, {}, images});
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Error handling get_images: " << e.what();
        reply(nats, msg, get_images_response{false, e.what()});
    }
}

void handle_list_images(ores::nats::service::client& nats,
    const ores::nats::message& msg,
    ores::database::context ctx) {
    auto req = decode<list_images_request>(msg);
    if (!req) {
        reply(nats, msg, list_images_response{false, "Failed to decode request"});
        return;
    }
    try {
        repository::image_repository repo;
        std::vector<domain::image> images;
        if (req->modified_since.has_value()) {
            images = repo.read_latest_since(ctx, *req->modified_since);
        } else {
            images = repo.read_latest(ctx);
        }
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
        reply(nats, msg, resp);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Error handling list_images: " << e.what();
        reply(nats, msg, list_images_response{false, e.what()});
    }
}

void handle_save_image(ores::nats::service::client& nats,
    const ores::nats::message& msg,
    ores::database::context ctx) {
    auto req = decode<save_image_request>(msg);
    if (!req) {
        reply(nats, msg,
            save_image_response{false, "Failed to decode request"});
        return;
    }
    try {
        repository::image_repository repo;
        repo.write(ctx, req->data);
        reply(nats, msg, save_image_response{true, "Image saved successfully"});
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Error handling save_image: " << e.what();
        reply(nats, msg, save_image_response{false, e.what()});
    }
}

} // namespace

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
    ores::database::context ctx) {
    std::vector<ores::nats::service::subscription> subs;
    subs.push_back(nats.queue_subscribe(
        "assets.v1.>", "ores.assets.service",
        [&nats, ctx](ores::nats::message msg) {
            if (msg.subject.ends_with("images.get")) {
                handle_get_images(nats, msg, ctx);
            } else if (msg.subject.ends_with("images.list")) {
                handle_list_images(nats, msg, ctx);
            } else if (msg.subject.ends_with("images.save")) {
                handle_save_image(nats, msg, ctx);
            }
        }));
    return subs;
}

}
