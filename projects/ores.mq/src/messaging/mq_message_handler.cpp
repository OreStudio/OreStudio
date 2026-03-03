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
#include "ores.utility/rfl/reflectors.hpp" // Must be before rfl/json.hpp
#include "ores.mq/messaging/mq_message_handler.hpp"

#include "ores.mq/messaging/mq_protocol.hpp"
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
    case message_type::get_queue_metrics_request:
        co_return co_await handle_get_queue_metrics_request(payload, remote_address);
    case message_type::get_queue_metric_samples_request:
        co_return co_await handle_get_queue_metric_samples_request(payload, remote_address);
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

    get_queues_response response;
    try {
        response.queues = client_.list_queues(ctx);
        response.success = true;
        BOOST_LOG_SEV(lg(), info) << "Retrieved " << response.queues.size() << " queues";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to list queues: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
mq_message_handler::handle_get_queue_metrics_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing get_queue_metrics_request.";

    auto auth = require_authentication(remote_address, "Get queue metrics");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = get_queue_metrics_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_queue_metrics_request";
        co_return std::unexpected(request_result.error());
    }

    auto ctx = make_request_context(*auth);

    get_queue_metrics_response response;
    try {
        response.metrics = client_.metrics_all(ctx);
        response.success = true;
        BOOST_LOG_SEV(lg(), info) << "Retrieved metrics for " << response.metrics.size() << " queues";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to retrieve queue metrics: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
mq_message_handler::handle_get_queue_metric_samples_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing get_queue_metric_samples_request.";

    auto auth = require_authentication(remote_address, "Get queue metric samples");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = get_queue_metric_samples_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to deserialize get_queue_metric_samples_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    auto ctx = make_request_context(*auth);

    get_queue_metric_samples_response response;
    response.queue_name = request.queue_name;
    try {
        response.samples = client_.metric_samples(
            ctx, request.queue_name, request.from, request.to);
        response.success = true;
        BOOST_LOG_SEV(lg(), info) << "Retrieved " << response.samples.size()
                                  << " metric samples for queue '"
                                  << request.queue_name << "'";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to retrieve metric samples: " << e.what();
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

    create_queue_response response;
    try {
        if (request.is_unlogged)
            client_.create_unlogged(ctx, request.queue_name);
        else
            client_.create(ctx, request.queue_name);
        response.success = true;
        BOOST_LOG_SEV(lg(), info) << "Created queue '" << request.queue_name << "'";
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

    drop_queue_response response;
    try {
        const bool dropped = client_.drop(ctx, request.queue_name);
        response.success = true;
        if (!dropped)
            response.message = "Queue did not exist";
        BOOST_LOG_SEV(lg(), info) << "Dropped queue '" << request.queue_name
                                  << "' (existed=" << dropped << ")";
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

    purge_queue_response response;
    try {
        response.purged_count = client_.purge(ctx, request.queue_name);
        response.success = true;
        BOOST_LOG_SEV(lg(), info) << "Purged " << response.purged_count
                                  << " messages from queue '" << request.queue_name << "'";
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

    send_message_response response;
    try {
        response.msg_id = client_.send<std::string>(
            ctx, request.queue_name, request.payload,
            std::chrono::seconds(request.delay_seconds));
        response.success = true;
        BOOST_LOG_SEV(lg(), info) << "Sent message " << response.msg_id
                                  << " to queue '" << request.queue_name << "'";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to send message: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

namespace {

queue_message to_wire_message(const pgmq::message<std::string>& m) {
    return queue_message{
        .msg_id = m.msg_id,
        .read_ct = m.read_ct,
        .enqueued_at = ores::platform::time::datetime::format_time_point_utc(m.enqueued_at),
        .vt = ores::platform::time::datetime::format_time_point_utc(m.vt),
        .payload = m.body
    };
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

    read_messages_response response;
    try {
        auto msgs = client_.read<std::string>(ctx, request.queue_name,
            std::chrono::seconds(request.vt_seconds), request.count);
        for (const auto& m : msgs)
            response.messages.push_back(to_wire_message(m));
        response.success = true;
        BOOST_LOG_SEV(lg(), info) << "Read " << response.messages.size()
                                  << " messages from queue '" << request.queue_name << "'";
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

    pop_messages_response response;
    try {
        auto msgs = client_.pop_batch<std::string>(ctx, request.queue_name, request.count);
        response.messages.reserve(msgs.size());
        for (const auto& m : msgs) response.messages.push_back(to_wire_message(m));
        response.success = true;
        BOOST_LOG_SEV(lg(), info) << "Popped " << response.messages.size()
                                  << " messages from queue '" << request.queue_name << "'";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to pop messages: " << e.what();
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

    delete_messages_response response;
    try {
        auto deleted = client_.erase(ctx, request.queue_name, request.msg_ids);
        response.deleted_count = static_cast<std::int32_t>(deleted.size());
        response.success = true;
        BOOST_LOG_SEV(lg(), info) << "Deleted " << response.deleted_count
                                  << " messages from queue '" << request.queue_name << "'";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to delete messages: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

}
