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
#include "ores.storage.core/messaging/objects_handler.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.storage.api/messaging/objects_protocol.hpp"
#include <algorithm>
#include <utility>

namespace ores::storage::messaging {

using namespace ores::logging;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::reply;
using ores::service::service::make_request_context;
using storage::filesystem::object_not_found;

namespace {

/**
 * @brief The largest page a listing will return, whatever the caller asks for.
 */
constexpr std::uint32_t k_max_list_limit = 1000;

}

objects_handler::objects_handler(ores::nats::service::client& nats,
                                 ores::database::context base_ctx,
                                 filesystem::local_store store,
                                 std::optional<ores::security::jwt::jwt_authenticator> verifier)
    : nats_(nats)
    , base_ctx_(std::move(base_ctx))
    , store_(std::move(store))
    , verifier_(std::move(verifier)) {}

std::optional<ores::database::context>
objects_handler::authorise(const ores::nats::message& msg,
                           std::string_view required_permission,
                           std::string_view operation_name) {
    const auto ctx = make_request_context(base_ctx_, msg, verifier_);
    if (!ctx) {
        BOOST_LOG_SEV(lg(), warn) << operation_name << " denied: the request context failed";
        error_reply(nats_, msg, ctx.error());
        return std::nullopt;
    }

    if (!has_permission(*ctx, required_permission)) {
        BOOST_LOG_SEV(lg(), warn) << operation_name << " denied: lacks " << required_permission;
        error_reply(nats_, msg, ores::service::error_code::forbidden);
        return std::nullopt;
    }

    return *ctx;
}

void objects_handler::put(ores::nats::message msg) {
    BOOST_LOG_SEV(lg(), debug) << "Handling " << msg.subject;

    const auto req = decode<put_objects_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }

    if (!authorise(msg, "storage::objects:write", "put"))
        return;

    put_objects_response resp;
    try {
        store_.put(req->bucket, req->key, req->content);
        resp.size_bytes = req->content.size();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "put failed: " << e.what();
        resp.success = false;
        resp.message = e.what();
    }

    reply(nats_, msg, resp);
}

void objects_handler::get(ores::nats::message msg) {
    BOOST_LOG_SEV(lg(), debug) << "Handling " << msg.subject;

    const auto req = decode<get_objects_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }

    if (!authorise(msg, "storage::objects:read", "get"))
        return;

    get_objects_response resp;
    const auto bytes = store_.size(req->bucket, req->key);
    if (!bytes) {
        // Absence is an answer, not a failure: the caller learns the object is
        // not there from found=false, the way a HEAD learns it from a 404.
        resp.found = false;
        reply(nats_, msg, resp);
        return;
    }

    resp.found = true;
    resp.size_bytes = *bytes;
    if (req->include_content) {
        try {
            resp.content = store_.get(req->bucket, req->key);
        } catch (const object_not_found&) {
            resp.found = false;
            resp.size_bytes = 0;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "get failed: " << e.what();
            resp.success = false;
            resp.message = e.what();
        }
    }

    reply(nats_, msg, resp);
}

void objects_handler::remove(ores::nats::message msg) {
    BOOST_LOG_SEV(lg(), debug) << "Handling " << msg.subject;

    const auto req = decode<delete_objects_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }

    if (!authorise(msg, "storage::objects:delete", "delete"))
        return;

    delete_objects_response resp;
    try {
        resp.removed = store_.remove(req->bucket, req->key);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "delete failed: " << e.what();
        resp.success = false;
        resp.message = e.what();
    }

    reply(nats_, msg, resp);
}

void objects_handler::list(ores::nats::message msg) {
    BOOST_LOG_SEV(lg(), debug) << "Handling " << msg.subject;

    const auto req = decode<list_objects_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }

    if (!authorise(msg, "storage::objects:read", "list"))
        return;

    list_objects_response resp;
    try {
        const auto entries = store_.list(req->bucket, req->prefix);
        resp.total_available_count = entries.size();

        const auto limit = std::min(req->limit, k_max_list_limit);
        const auto first = std::min<std::size_t>(req->offset, entries.size());
        const auto last = std::min<std::size_t>(first + limit, entries.size());
        for (std::size_t i = first; i < last; ++i)
            resp.objects.push_back(object_summary{entries[i].key, entries[i].size_bytes});
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "list failed: " << e.what();
        resp.success = false;
        resp.message = e.what();
    }

    reply(nats_, msg, resp);
}

}
