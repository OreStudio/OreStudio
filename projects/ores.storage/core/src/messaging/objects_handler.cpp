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
#include "ores.storage.core/filesystem/archive_reader.hpp"
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

/**
 * @brief The most objects one expiry removes, whatever the caller asks for.
 *
 * An expiry is one request, and a prefix can hold more work than one request
 * should hold the store for. The reply states what the cutoff left, so a caller
 * repeats until nothing is left rather than asking for it all at once.
 */
constexpr std::uint32_t k_max_expire_limit = 1000;

}

objects_handler::objects_handler(ores::nats::service::client& nats,
                                 const ores::database::context& base_ctx,
                                 filesystem::local_store store,
                                 std::optional<ores::security::jwt::jwt_authenticator> verifier)
    : nats_(nats)
    , base_ctx_(base_ctx)
    , store_(std::move(store))
    , verifier_(std::move(verifier)) {}

std::optional<ores::database::context>
objects_handler::authorise(const ores::nats::message& msg,
                           std::string_view required_permission,
                           std::string_view operation_name) {
    const auto ctx = make_request_context(base_ctx_, msg, verifier_);
    if (!ctx) {
        ORES_LOG_SEV(lg(), warn) << operation_name << " denied: the request context failed";
        error_reply(nats_, msg, ctx.error());
        return std::nullopt;
    }

    if (!has_permission(*ctx, required_permission)) {
        ORES_LOG_SEV(lg(), warn) << operation_name << " denied: lacks " << required_permission;
        error_reply(nats_, msg, ores::service::error_code::forbidden);
        return std::nullopt;
    }

    return *ctx;
}

void objects_handler::put(ores::nats::message msg) {
    ORES_LOG_SEV(lg(), debug) << "Handling " << msg.subject;

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
        ORES_LOG_SEV(lg(), error) << "put failed: " << e.what();
        resp.success = false;
        resp.message = e.what();
    }

    reply(nats_, msg, resp);
}

void objects_handler::get(ores::nats::message msg) {
    ORES_LOG_SEV(lg(), debug) << "Handling " << msg.subject;

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
            ORES_LOG_SEV(lg(), error) << "get failed: " << e.what();
            resp.success = false;
            resp.message = e.what();
        }
    }

    reply(nats_, msg, resp);
}

void objects_handler::remove(ores::nats::message msg) {
    ORES_LOG_SEV(lg(), debug) << "Handling " << msg.subject;

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
        ORES_LOG_SEV(lg(), error) << "delete failed: " << e.what();
        resp.success = false;
        resp.message = e.what();
    }

    reply(nats_, msg, resp);
}

void objects_handler::list(ores::nats::message msg) {
    ORES_LOG_SEV(lg(), debug) << "Handling " << msg.subject;

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
            resp.objects.push_back(
                object_summary{entries[i].key, entries[i].size_bytes, entries[i].last_modified});
    } catch (const std::exception& e) {
        ORES_LOG_SEV(lg(), error) << "list failed: " << e.what();
        resp.success = false;
        resp.message = e.what();
    }

    reply(nats_, msg, resp);
}

void objects_handler::expire(ores::nats::message msg) {
    ORES_LOG_SEV(lg(), debug) << "Handling " << msg.subject;

    const auto req = decode<expire_objects_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }

    // An expiry is the deletion path, so it is admitted by the code the delete
    // is admitted by, whatever the call states it will do.
    if (!authorise(msg, "storage::objects:delete", "expire"))
        return;

    expire_objects_response resp;
    try {
        const auto limit = std::min(req->limit, k_max_expire_limit);
        const auto done =
            store_.expire(req->bucket, req->prefix, req->older_than, limit, req->dry_run);
        resp.would_remove = done.selected;
        resp.would_bytes = done.selected_bytes;
        resp.removed = done.removed;
        resp.removed_bytes = done.removed_bytes;
        resp.remaining = done.remaining;

        ORES_LOG_SEV(lg(), debug) << "Expiry of " << req->prefix << ": would remove "
                                  << resp.would_remove << ", removed " << resp.removed << ", left "
                                  << resp.remaining;
    } catch (const std::exception& e) {
        ORES_LOG_SEV(lg(), error) << "expire failed: " << e.what();
        resp.success = false;
        resp.message = e.what();
    }

    reply(nats_, msg, resp);
}

void objects_handler::list_archive_entries(ores::nats::message msg) {
    ORES_LOG_SEV(lg(), debug) << "Handling " << msg.subject;

    const auto req = decode<list_archive_entries_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }

    if (!authorise(msg, "storage::objects:read", "list_archive_entries"))
        return;

    list_archive_entries_response resp;
    for (const auto& key : req->keys) {
        archive_listing listing;
        listing.key = key;
        /*
         * One package the store cannot read is stated in its own listing, so a
         * run whose output never arrived still draws the files it did write.
         */
        const auto path = store_.resolve(req->bucket, key);
        listing.size_bytes = store_.size(req->bucket, key).value_or(0);
        if (!path) {
            listing.success = false;
            listing.message = "No object at this key.";
            ORES_LOG_SEV(lg(), warn)
                << "list_archive_entries: no object at " << req->bucket << "/" << key;
            resp.archives.push_back(std::move(listing));
            continue;
        }
        try {
            for (const auto& entry : filesystem::archive_reader::index_file(*path))
                listing.entries.push_back(
                    archive_entry{entry.name, static_cast<std::uint64_t>(entry.size)});
        } catch (const std::exception& e) {
            listing.success = false;
            listing.message = e.what();
            ORES_LOG_SEV(lg(), warn) << "list_archive_entries: " << req->bucket << "/" << key
                                     << " is not a package: " << e.what();
        }
        resp.archives.push_back(std::move(listing));
    }

    reply(nats_, msg, resp);
}

}
