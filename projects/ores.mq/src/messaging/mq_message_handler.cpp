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
#include "ores.mq/messaging/mq_message_handler.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.mq/messaging/mq_protocol.hpp"
#include "ores.mq/service/mq_service.hpp"
#include "ores.mq/domain/queue_definition.hpp"
#include "ores.platform/time/datetime.hpp"

namespace ores::mq::messaging {

using namespace ores::logging;
using comms::messaging::message_type;

mq_message_handler::mq_message_handler(
    database::context ctx,
    std::shared_ptr<comms::service::auth_session_service> sessions)
    : tenant_aware_handler(std::move(ctx), std::move(sessions)) {}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
mq_message_handler::handle_message(message_type type,
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Handling MQ message type " << type;

    switch (type) {
    case message_type::get_queues_request:
        co_return co_await handle_get_queues_request(payload, remote_address);
    case message_type::get_queue_stats_request:
        co_return co_await handle_get_queue_stats_request(payload, remote_address);
    case message_type::get_queue_stats_samples_request:
        co_return co_await handle_get_queue_stats_samples_request(payload, remote_address);
    case message_type::create_queue_request:
        co_return co_await handle_create_queue_request(payload, remote_address);
    case message_type::drop_queue_request:
        co_return co_await handle_drop_queue_request(payload, remote_address);
    case message_type::purge_queue_request:
        co_return co_await handle_purge_queue_request(payload, remote_address);
    case message_type::send_message_request:
        co_return co_await handle_send_message_request(payload, remote_address);
    case message_type::read_messages_request:
        co_return co_await handle_read_messages_request(payload, remote_address);
    case message_type::pop_messages_request:
        co_return co_await handle_pop_messages_request(payload, remote_address);
    case message_type::ack_messages_request:
        co_return co_await handle_ack_messages_request(payload, remote_address);
    case message_type::nack_message_request:
        co_return co_await handle_nack_message_request(payload, remote_address);
    case message_type::delete_messages_request:
        co_return co_await handle_delete_messages_request(payload, remote_address);
    default:
        BOOST_LOG_SEV(lg(), error) << "Unknown MQ message type " << std::hex
                                   << static_cast<std::uint16_t>(type);
        co_return std::unexpected(ores::utility::serialization::error_code::invalid_message_type);
    }
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
mq_message_handler::handle_get_queues_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing get_queues_request.";

    auto auth = require_authentication(remote_address, "Get queues");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = get_queues_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_queues_request";
        co_return std::unexpected(request_result.error());
    }

    auto ctx = make_request_context(*auth);
    service::mq_service svc(ctx);

    get_queues_response response;
    try {
        response.queues = svc.list_queues();
        response.success = true;
        BOOST_LOG_SEV(lg(), info) << "Retrieved " << response.queues.size()
                                  << " queues";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to list queues: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
mq_message_handler::handle_get_queue_stats_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing get_queue_stats_request.";

    auto auth = require_authentication(remote_address, "Get queue stats");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = get_queue_stats_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_queue_stats_request";
        co_return std::unexpected(request_result.error());
    }

    auto ctx = make_request_context(*auth);
    service::mq_service svc(ctx);

    get_queue_stats_response response;
    try {
        response.stats = svc.get_stats();
        response.success = true;
        BOOST_LOG_SEV(lg(), info) << "Retrieved stats for " << response.stats.size()
                                  << " queues";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to retrieve queue stats: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
mq_message_handler::handle_get_queue_stats_samples_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing get_queue_stats_samples_request.";

    auto auth = require_authentication(remote_address, "Get queue stats samples");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = get_queue_stats_samples_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to deserialize get_queue_stats_samples_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    auto ctx = make_request_context(*auth);
    service::mq_service svc(ctx);

    get_queue_stats_samples_response response;
    response.queue_id = request.queue_id;
    try {
        boost::uuids::uuid queue_uuid;
        try {
            queue_uuid = boost::lexical_cast<boost::uuids::uuid>(request.queue_id);
        } catch (...) {
            response.success = false;
            response.message = "Invalid queue_id UUID: " + request.queue_id;
            co_return response.serialize();
        }
        response.samples = svc.get_stats_samples(queue_uuid, request.from, request.to);
        response.success = true;
        BOOST_LOG_SEV(lg(), info) << "Retrieved " << response.samples.size()
                                  << " stats samples for queue '"
                                  << request.queue_id << "'";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to retrieve stats samples: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
mq_message_handler::handle_create_queue_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing create_queue_request.";

    auto auth = require_authentication(remote_address, "Create queue");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = create_queue_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize create_queue_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    auto ctx = make_request_context(*auth);
    service::mq_service svc(ctx);

    create_queue_response response;
    try {
        domain::queue_definition def;
        def.name = request.queue_name;
        def.description = request.description;

        if (request.scope_type == "tenant")
            def.scope_type = domain::queue_scope_type::tenant;
        else if (request.scope_type == "system")
            def.scope_type = domain::queue_scope_type::system;
        else
            def.scope_type = domain::queue_scope_type::party;

        def.queue_type = (request.queue_type == "channel")
            ? domain::queue_type::channel : domain::queue_type::task;

        // Populate tenant/party from the session context.
        if (!auth->tenant_id.is_system())
            def.tenant_id = auth->tenant_id.to_uuid();
        if (auth->party_id != boost::uuids::uuid{})
            def.party_id = auth->party_id;

        const auto new_id = svc.create_queue(def, boost::uuids::to_string(auth->account_id));
        response.queue_id = boost::uuids::to_string(new_id);
        response.success = true;
        BOOST_LOG_SEV(lg(), info) << "Created queue '" << request.queue_name
                                  << "' id=" << response.queue_id;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to create queue: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
mq_message_handler::handle_drop_queue_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing drop_queue_request.";

    auto auth = require_authentication(remote_address, "Drop queue");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = drop_queue_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize drop_queue_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    auto ctx = make_request_context(*auth);
    service::mq_service svc(ctx);

    drop_queue_response response;
    try {
        // Look up the queue by name, then deactivate it.
        std::optional<boost::uuids::uuid> tenant_id;
        if (!auth->tenant_id.is_system())
            tenant_id = auth->tenant_id.to_uuid();
        std::optional<boost::uuids::uuid> party_id;
        if (auth->party_id != boost::uuids::uuid{})
            party_id = auth->party_id;

        auto queue = svc.find_queue(request.queue_name, tenant_id, party_id);
        if (!queue) {
            response.success = true;
            response.message = "Queue did not exist";
        } else {
            svc.deactivate(queue->id);
            response.success = true;
        }
        BOOST_LOG_SEV(lg(), info) << "Dropped queue '" << request.queue_name << "'";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to drop queue: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
mq_message_handler::handle_purge_queue_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing purge_queue_request.";

    auto auth = require_authentication(remote_address, "Purge queue");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = purge_queue_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize purge_queue_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    auto ctx = make_request_context(*auth);
    service::mq_service svc(ctx);

    purge_queue_response response;
    try {
        std::optional<boost::uuids::uuid> tenant_id;
        if (!auth->tenant_id.is_system())
            tenant_id = auth->tenant_id.to_uuid();
        std::optional<boost::uuids::uuid> party_id;
        if (auth->party_id != boost::uuids::uuid{})
            party_id = auth->party_id;

        auto queue = svc.find_queue(request.queue_name, tenant_id, party_id);
        if (!queue) {
            response.success = true;
            response.purged_count = 0;
            response.message = "Queue not found";
        } else {
            response.purged_count = svc.purge(queue->id);
            response.success = true;
        }
        BOOST_LOG_SEV(lg(), info) << "Purged " << response.purged_count
                                  << " messages from queue '"
                                  << request.queue_name << "'";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to purge queue: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
mq_message_handler::handle_send_message_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing send_message_request.";

    auto auth = require_authentication(remote_address, "Send message");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = send_message_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize send_message_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    auto ctx = make_request_context(*auth);
    service::mq_service svc(ctx);

    send_message_response response;
    try {
        boost::uuids::uuid queue_uuid;
        try {
            queue_uuid = boost::lexical_cast<boost::uuids::uuid>(request.queue_id);
        } catch (...) {
            response.success = false;
            response.message = "Invalid queue_id UUID: " + request.queue_id;
            co_return response.serialize();
        }

        response.msg_id = svc.send(queue_uuid, request.message_type,
            request.payload, request.delay_seconds);
        response.success = true;
        BOOST_LOG_SEV(lg(), info) << "Sent message " << response.msg_id
                                  << " to queue '" << request.queue_id << "'";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to send message: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

namespace {

/**
 * @brief Converts a domain::mq_message to the wire queue_message type.
 */
queue_message to_wire_message(const domain::mq_message& m) {
    queue_message wm;
    wm.msg_id = m.id;
    wm.queue_id = boost::uuids::to_string(m.queue_id);
    wm.message_type = m.message_type;
    wm.payload_type = m.payload_type;
    wm.read_count = m.read_count;
    wm.created_at = ores::platform::time::datetime::format_time_point_utc(m.created_at);
    wm.visible_after = ores::platform::time::datetime::format_time_point_utc(m.visible_after);
    wm.payload = m.payload.value_or("");

    switch (m.status) {
    case domain::mq_message_status::processing: wm.status = "processing"; break;
    case domain::mq_message_status::done:       wm.status = "done";       break;
    case domain::mq_message_status::failed:     wm.status = "failed";     break;
    default:                                    wm.status = "pending";    break;
    }

    return wm;
}

} // anonymous namespace

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
mq_message_handler::handle_read_messages_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing read_messages_request.";

    auto auth = require_authentication(remote_address, "Read messages");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = read_messages_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize read_messages_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    auto ctx = make_request_context(*auth);
    service::mq_service svc(ctx);

    read_messages_response response;
    try {
        boost::uuids::uuid queue_uuid;
        try {
            queue_uuid = boost::lexical_cast<boost::uuids::uuid>(request.queue_id);
        } catch (...) {
            response.success = false;
            response.message = "Invalid queue_id UUID: " + request.queue_id;
            co_return response.serialize();
        }

        auto msgs = svc.read(queue_uuid, request.count, request.vt_seconds);
        response.messages.reserve(msgs.size());
        for (const auto& m : msgs)
            response.messages.push_back(to_wire_message(m));
        response.success = true;
        BOOST_LOG_SEV(lg(), info) << "Read " << response.messages.size()
                                  << " messages from queue '" << request.queue_id << "'";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to read messages: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
mq_message_handler::handle_pop_messages_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing pop_messages_request.";

    auto auth = require_authentication(remote_address, "Pop messages");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = pop_messages_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize pop_messages_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    auto ctx = make_request_context(*auth);
    service::mq_service svc(ctx);

    pop_messages_response response;
    try {
        boost::uuids::uuid queue_uuid;
        try {
            queue_uuid = boost::lexical_cast<boost::uuids::uuid>(request.queue_id);
        } catch (...) {
            response.success = false;
            response.message = "Invalid queue_id UUID: " + request.queue_id;
            co_return response.serialize();
        }

        // Read messages with zero visibility timeout so they are immediately
        // visible again, then ack them all (read + delete = pop semantics).
        auto msgs = svc.read(queue_uuid, request.count, 0);
        if (!msgs.empty()) {
            std::vector<std::int64_t> ids;
            ids.reserve(msgs.size());
            for (const auto& m : msgs) {
                ids.push_back(m.id);
                response.messages.push_back(to_wire_message(m));
            }
            svc.ack(ids);
        }
        response.success = true;
        BOOST_LOG_SEV(lg(), info) << "Popped " << response.messages.size()
                                  << " messages from queue '" << request.queue_id << "'";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to pop messages: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
mq_message_handler::handle_ack_messages_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing ack_messages_request.";

    auto auth = require_authentication(remote_address, "Ack messages");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = ack_messages_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize ack_messages_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    auto ctx = make_request_context(*auth);
    service::mq_service svc(ctx);

    ack_messages_response response;
    try {
        svc.ack(request.message_ids);
        response.success = true;
        BOOST_LOG_SEV(lg(), info) << "Acknowledged " << request.message_ids.size()
                                  << " message(s)";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to ack messages: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
mq_message_handler::handle_nack_message_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing nack_message_request.";

    auto auth = require_authentication(remote_address, "Nack message");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = nack_message_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize nack_message_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    auto ctx = make_request_context(*auth);
    service::mq_service svc(ctx);

    nack_message_response response;
    try {
        svc.nack(request.message_id, request.error);
        response.success = true;
        BOOST_LOG_SEV(lg(), info) << "Nacked message " << request.message_id;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to nack message: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
mq_message_handler::handle_delete_messages_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing delete_messages_request.";

    auto auth = require_authentication(remote_address, "Delete messages");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = delete_messages_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize delete_messages_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    auto ctx = make_request_context(*auth);
    service::mq_service svc(ctx);

    delete_messages_response response;
    try {
        // delete_messages_request is the legacy path — delegate to ack.
        svc.ack(request.msg_ids);
        response.deleted_count = static_cast<std::int32_t>(request.msg_ids.size());
        response.success = true;
        BOOST_LOG_SEV(lg(), info) << "Deleted " << response.deleted_count
                                  << " messages";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to delete messages: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

}
